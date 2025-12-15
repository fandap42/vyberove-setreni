import numpy as np
import os
from concurrent.futures import ProcessPoolExecutor, as_completed
import pandas as pd
import matplotlib.pyplot as plt
from scipy import stats
from scipy.stats import norm, lognorm
from tqdm import tqdm
import warnings
import seaborn as sns

warnings.filterwarnings('ignore')

np.random.seed(42)

try:
    plt.style.use('seaborn-v0_8-whitegrid')
except:
    sns.set_style("whitegrid")

plt.rcParams['figure.figsize'] = (15, 10)
plt.rcParams['font.size'] = 11
plt.rcParams['axes.titlesize'] = 13
plt.rcParams['axes.labelsize'] = 12

def lognormal_pdf(x, mu, sigma):
    scale = np.exp(mu)
    return lognorm.pdf(x, s=sigma, scale=scale)


def lognormal_quantile(p, mu, sigma):
    scale = np.exp(mu)
    return lognorm.ppf(p, s=sigma, scale=scale)


def theoretical_variance_taylor(p, mu, sigma, n):
    q_p = lognormal_quantile(p, mu, sigma)
    f_q = lognormal_pdf(q_p, mu, sigma)
    return (p * (1 - p)) / (n * f_q**2)

def estimate_variance_golden(p, mu, sigma, n):
    return theoretical_variance_taylor(p, mu, sigma, n)


def estimate_variance_plugin(sample, p):
    n = len(sample)
    q_hat = np.quantile(sample, p)
    
    try:
        kde = stats.gaussian_kde(sample, bw_method='scott')
        f_hat = kde(q_hat)[0]
        if f_hat < 1e-10:
            return np.nan
        return (p * (1 - p)) / (n * f_hat**2)
    except:
        return np.nan


def estimate_variance_bootstrap(sample, p, B=1000):
    n = len(sample)
    bootstrap_quantiles = np.zeros(B)
    for b in range(B):
        resample = np.random.choice(sample, size=n, replace=True)
        bootstrap_quantiles[b] = np.quantile(resample, p)
    return np.var(bootstrap_quantiles, ddof=1)

SAMPLE_SIZES = [30, 100, 500, 1000]
SIGMA_VALUES = [0.5, 1.0, 1.5]
QUANTILE_LEVELS = [0.5, 0.95, 0.99]
MU = 0
M = 500
B = 100

def run_simulation(n, sigma, p, M, B, mu=0):
    true_quantile = lognormal_quantile(p, mu, sigma)
    var_golden = estimate_variance_golden(p, mu, sigma, n)

    conf_level = 0.95
    alpha = 1 - conf_level
    z = norm.ppf(1 - alpha/2)
        
    sample_quantiles = np.zeros(M)
    var_plugin_estimates = np.zeros(M)
    var_bootstrap_estimates = np.zeros(M)
    
    for m in range(M):
        sample = np.random.lognormal(mean=mu, sigma=sigma, size=n)
        sample_quantiles[m] = np.quantile(sample, p)
        var_plugin_estimates[m] = estimate_variance_plugin(sample, p)
        var_bootstrap_estimates[m] = estimate_variance_bootstrap(sample, p, B)
    
    empirical_variance = np.var(sample_quantiles, ddof=1)
    
    results = {
        'n': n, 'sigma': sigma, 'p': p,
        'true_quantile': true_quantile,
        'empirical_variance': empirical_variance,
        'theoretical_variance': var_golden,
        'conf_level': conf_level 
    }
    
    # 1. Oracle
    results['golden_mean_var'] = var_golden
    results['golden_bias'] = var_golden - empirical_variance
    results['golden_rel_bias'] = (var_golden - empirical_variance) / empirical_variance
    
    se_golden = np.sqrt(var_golden)
    lower_golden = sample_quantiles - z * se_golden
    upper_golden = sample_quantiles + z * se_golden
    results['golden_coverage'] = np.mean((lower_golden <= true_quantile) & (true_quantile <= upper_golden))
    
    # 2. Plug-in
    results['plugin_mean_var'] = np.nanmean(var_plugin_estimates)
    results['plugin_bias'] = np.nanmean(var_plugin_estimates) - empirical_variance
    results['plugin_rel_bias'] = (np.nanmean(var_plugin_estimates) - empirical_variance) / empirical_variance
    results['plugin_mse'] = np.nanmean((var_plugin_estimates - empirical_variance)**2)
    
    se_plugin = np.sqrt(var_plugin_estimates)
    lower_plugin = sample_quantiles - z * se_plugin
    upper_plugin = sample_quantiles + z * se_plugin
    coverage_plugin = (lower_plugin <= true_quantile) & (true_quantile <= upper_plugin)
    results['plugin_coverage'] = np.nanmean(coverage_plugin)
    
    # 3. Bootstrap
    results['bootstrap_mean_var'] = np.mean(var_bootstrap_estimates)
    results['bootstrap_bias'] = np.mean(var_bootstrap_estimates) - empirical_variance
    results['bootstrap_rel_bias'] = (np.mean(var_bootstrap_estimates) - empirical_variance) / empirical_variance
    results['bootstrap_mse'] = np.mean((var_bootstrap_estimates - empirical_variance)**2)
    
    se_bootstrap = np.sqrt(var_bootstrap_estimates)
    lower_bootstrap = sample_quantiles - z * se_bootstrap
    upper_bootstrap = sample_quantiles + z * se_bootstrap
    results['bootstrap_coverage'] = np.mean((lower_bootstrap <= true_quantile) & (true_quantile <= upper_bootstrap))
    
    return results

def main():
    param_combinations = []
    for sigma in SIGMA_VALUES:
        for p in QUANTILE_LEVELS:
            for n in SAMPLE_SIZES:
                param_combinations.append((n, sigma, p))

    workers = os.cpu_count()
    print(f"Starting simulation with {len(param_combinations)} tasks using ProcessPoolExecutor on {workers} cores...")

    results_list = []
    with ProcessPoolExecutor(max_workers=workers) as executor:
        future_to_params = {
            executor.submit(run_simulation, n, sigma, p, M, B, MU): (n, sigma, p)
            for n, sigma, p in param_combinations
        }
        
        for future in tqdm(as_completed(future_to_params), total=len(param_combinations), desc="Simulating"):
            try:
                result = future.result()
                results_list.append(result)
            except Exception as exc:
                n, sigma, p = future_to_params[future]
                print(f'Task (n={n}, sigma={sigma}, p={p}) generated an exception: {exc}')

    results_df = pd.DataFrame(results_list)

    # Graf 1: Coverage Probability
    fig, axes = plt.subplots(3, 3, figsize=(15, 15), sharey=True)
    colors = {'golden': 'green', 'plugin': 'blue', 'bootstrap': 'red'}
    labels = {'golden': 'Taylor (Oracle)', 'plugin': 'Plug-in (KDE)', 'bootstrap': 'Bootstrap'}
    markers = {'golden': 'o', 'plugin': 's', 'bootstrap': '^'}

    for row, p in enumerate(QUANTILE_LEVELS):
        target_cov = 0.95
        
        for col, sigma in enumerate(SIGMA_VALUES):
            ax = axes[row, col]
            subset = results_df[(results_df['p'] == p) & (results_df['sigma'] == sigma)]
            
            for method in ['golden', 'plugin', 'bootstrap']:
                ax.plot(subset['n'], subset[f'{method}_coverage'], 
                       color=colors[method], marker=markers[method], 
                       label=labels[method], linewidth=2, markersize=8)
            
            ax.axhline(y=target_cov, color='gray', linestyle='--', alpha=0.7, label=f'Nominální {target_cov*100:.0f}%')
            ax.set_xlabel('Velikost výběru (n)')
            ax.set_ylabel('Coverage Probability')
            ax.set_title(f'σ = {sigma}, p = {p} (Target {target_cov})')
            ax.set_ylim(0.5, 1.01) 
            if (row == 2 and col == 2):
                ax.legend(loc='lower right', fontsize=9)
            ax.grid(True, alpha=0.3)

    plt.suptitle('Coverage Probability (95% CI)', fontsize=14, fontweight='bold')
    plt.tight_layout()
    plt.savefig('coverage_probability.png', dpi=150, bbox_inches='tight')

    # Graf 2: Relativní Bias
    fig, axes = plt.subplots(3, 3, figsize=(15, 15))

    for row, p in enumerate(QUANTILE_LEVELS):
        for col, sigma in enumerate(SIGMA_VALUES):
            ax = axes[row, col]
            subset = results_df[(results_df['p'] == p) & (results_df['sigma'] == sigma)]
            
            for method in ['golden', 'plugin', 'bootstrap']:
                ax.plot(subset['n'], subset[f'{method}_rel_bias'] * 100, 
                       color=colors[method], marker=markers[method], 
                       label=labels[method], linewidth=2, markersize=8)
            
            ax.axhline(y=0, color='gray', linestyle='--', alpha=0.7)
            ax.set_xlabel('Velikost výběru (n)')
            ax.set_ylabel('Relativní Bias (%)')
            ax.set_title(f'σ = {sigma}, p = {p}')
            ax.set_ylim(-100, 100) 
            if (row == 0 and col == 0):
                ax.legend(loc='best', fontsize=9)
            ax.grid(True, alpha=0.3)

    plt.suptitle('Relativní Bias odhadu rozptylu', fontsize=14, fontweight='bold')
    plt.tight_layout()
    plt.savefig('relative_bias.png', dpi=150, bbox_inches='tight')

    # Graf 3: MSE Log-Log
    fig, axes = plt.subplots(3, 3, figsize=(15, 15))

    for row, p in enumerate(QUANTILE_LEVELS):
        for col, sigma in enumerate(SIGMA_VALUES):
            ax = axes[row, col]
            subset = results_df[(results_df['p'] == p) & (results_df['sigma'] == sigma)]
            
            for method in ['plugin', 'bootstrap']:
                ax.loglog(subset['n'], subset[f'{method}_mse'], 
                         color=colors[method], marker=markers[method], 
                         label=labels[method], linewidth=2, markersize=8)
            
            ax.set_xlabel('Velikost výběru (n)')
            ax.set_ylabel('MSE')
            ax.set_title(f'σ = {sigma}, p = {p}')
            if (row == 0 and col == 0):
                ax.legend(loc='best', fontsize=9)
            ax.grid(True, alpha=0.3, which='both')

    plt.suptitle('MSE odhadu rozptylu (log-log škála)', fontsize=14, fontweight='bold')
    plt.tight_layout()
    plt.savefig('mse_loglog.png', dpi=150, bbox_inches='tight')

    results_df.to_csv('simulation_results.csv', index=False)

if __name__ == '__main__':
    main()
