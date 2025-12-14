import numpy as np
import pandas as pd
import scipy.stats as stats
import matplotlib.pyplot as plt
import seaborn as sns
import os

# Set style for better aesthetics (wow factor)
sns.set_theme(style="whitegrid")
plt.rcParams.update({'figure.figsize': (10, 6), 'figure.dpi': 100})

print("Script started.")

# Configuration
np.random.seed(42)
Ns = [30, 100, 1000]
Ps = [0.50, 0.95, 0.99]
n_mc = 1000 
n_boot = 200
mu, sigma = 0, 1.0 

# True values
def get_true_quantile(p, mu, sigma):
    return stats.lognorm.ppf(p, s=sigma, scale=np.exp(mu))

def get_true_pdf(x, mu, sigma):
    return stats.lognorm.pdf(x, s=sigma, scale=np.exp(mu))

true_quatiles = {p: get_true_quantile(p, mu, sigma) for p in Ps}
true_densities = {p: get_true_pdf(true_quatiles[p], mu, sigma) for p in Ps}

results = []

print("Starting simulation...")

for n in Ns:
    print(f"  Running for n={n}...")
    
    mc_storage = {p: {'q_hat': [], 'var_oracle': [], 'var_plugin': [], 'var_boot': []} for p in Ps}
    
    for _ in range(n_mc):
        data = stats.lognorm.rvs(s=sigma, scale=np.exp(mu), size=n)
        
        try:
            kde = stats.gaussian_kde(data)
        except:
            kde = None
            
        for p in Ps:
            q_hat = np.quantile(data, p)
            mc_storage[p]['q_hat'].append(q_hat)
            
            # Oracle
            f_true = true_densities[p]
            var_oracle = (p * (1 - p)) / (n * f_true**2)
            mc_storage[p]['var_oracle'].append(var_oracle)
            
            # Plug-in
            if kde is not None:
                try:
                    vals = kde(q_hat)
                    f_hat = vals[0]
                except:
                    f_hat = 1e-10
            else:
                f_hat = 1e-10
            if f_hat < 1e-10: f_hat = 1e-10
            
            var_plugin = (p * (1 - p)) / (n * f_hat**2)
            mc_storage[p]['var_plugin'].append(var_plugin)
            
            # Bootstrap
            boot_qs = []
            for _ in range(n_boot):
                sample_b = np.random.choice(data, size=n, replace=True)
                boot_qs.append(np.quantile(sample_b, p))
            var_boot = np.var(boot_qs, ddof=1)
            mc_storage[p]['var_boot'].append(var_boot)
            
    for p in Ps:
        true_q = true_quatiles[p]
        q_hats = np.array(mc_storage[p]['q_hat'])
        
        var_mc = np.var(q_hats, ddof=1)
        se_mc = np.sqrt(var_mc)
        
        # Mean estimated SE
        mean_se_oracle = np.mean(np.sqrt(mc_storage[p]['var_oracle']))
        mean_se_plugin = np.mean(np.sqrt(mc_storage[p]['var_plugin']))
        mean_se_boot = np.mean(np.sqrt(mc_storage[p]['var_boot']))

        # Coverage
        cov_oracle = 0
        cov_plugin = 0
        cov_boot = 0
        z = 1.96
        
        for i in range(n_mc):
            qh = mc_storage[p]['q_hat'][i]
            
            se_o = np.sqrt(mc_storage[p]['var_oracle'][i])
            if (qh - z*se_o <= true_q <= qh + z*se_o): cov_oracle += 1
            
            se_p = np.sqrt(mc_storage[p]['var_plugin'][i])
            if (qh - z*se_p <= true_q <= qh + z*se_p): cov_plugin += 1
            
            se_b = np.sqrt(mc_storage[p]['var_boot'][i])
            if (qh - z*se_b <= true_q <= qh + z*se_b): cov_boot += 1
            
        results.append({
            'n': n,
            'p': p,
            'SE_MC': se_mc,
            'RB_Oracle': (mean_se_oracle - se_mc) / se_mc,
            'RB_Plugin': (mean_se_plugin - se_mc) / se_mc,
            'RB_Boot': (mean_se_boot - se_mc) / se_mc,
            'Cov_Oracle': cov_oracle / n_mc,
            'Cov_Plugin': cov_plugin / n_mc,
            'Cov_Boot': cov_boot / n_mc
        })

df = pd.DataFrame(results)

# --- Plot 1: Relative Bias of SE ---
# Reshape for seaborn
df_melt_bias = df.melt(id_vars=['n', 'p'], value_vars=['RB_Oracle', 'RB_Plugin', 'RB_Boot'], 
                       var_name='Method', value_name='Relative Bias')
df_melt_bias['Method'] = df_melt_bias['Method'].str.replace('RB_', '')

g = sns.FacetGrid(df_melt_bias, col="p", sharey=False, height=5, aspect=0.8)
g.map_dataframe(sns.lineplot, x="n", y="Relative Bias", hue="Method", style="Method", markers=True, dashes=False)
g.add_legend()
g.set(xscale="log")
g.fig.suptitle("Relativní vychýlení odhadu SE (Relative Bias)", y=1.02)
plt.savefig('relative_bias.png', bbox_inches='tight')
plt.close()

# --- Plot 2: Coverage Probability ---
df_melt_cov = df.melt(id_vars=['n', 'p'], value_vars=['Cov_Oracle', 'Cov_Plugin', 'Cov_Boot'], 
                      var_name='Method', value_name='Coverage')
df_melt_cov['Method'] = df_melt_cov['Method'].str.replace('Cov_', '')

g = sns.FacetGrid(df_melt_cov, col="p", sharey=True, height=5, aspect=0.8)
g.map_dataframe(sns.lineplot, x="n", y="Coverage", hue="Method", style="Method", markers=True, dashes=False)
# Add reference line
for ax in g.axes.flat:
    ax.axhline(0.95, ls='--', color='gray', alpha=0.5)
    ax.set_ylim(0.80, 1.00)

g.add_legend()
g.set(xscale="log")
g.fig.suptitle("Coverage Probability (Target = 0.95)", y=1.02)
plt.savefig('coverage_probability.png', bbox_inches='tight')
plt.close()

print("Graphs generated successfully!")
