import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from scipy.stats import lognorm

def plot_lognormal_densities():
    try:
        plt.style.use('seaborn-v0_8-whitegrid')
    except:
        sns.set_style("whitegrid")

    plt.rcParams['font.size'] = 11
    plt.rcParams['axes.titlesize'] = 13
    plt.rcParams['axes.labelsize'] = 12
    
    mu = 0
    sigmas = [0.5, 1.0, 1.5]
    x = np.linspace(0, 5, 1000)

    plt.figure(figsize=(10, 6))
    
    # Use a color palette for better aesthetics
    colors = sns.color_palette("deep", len(sigmas))
    
    for i, sigma in enumerate(sigmas):
        # scipy lognorm parameters: s=sigma, scale=exp(mu)
        y = lognorm.pdf(x, s=sigma, scale=np.exp(mu))
        plt.plot(x, y, label=rf'$\sigma={sigma}$', color=colors[i], linewidth=2.5)

    plt.title(r'Hustoty Log-normálního rozdělení ($LN(\mu=0, \sigma)$)')
    plt.xlabel('x')
    plt.ylabel('Hustota pravděpodobnosti f(x)')
    plt.legend(frameon=True)
    plt.grid(True, alpha=0.3)
    
    # Save as PDF for LaTeX
    output_path = 'simulace/lognormal_densities.pdf'
    plt.savefig(output_path, bbox_inches='tight')
    print(f"Plot saved to {output_path}")

if __name__ == "__main__":
    plot_lognormal_densities()
