
import pandas as pd
from math import *
import os
import re
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import bootstrap

parent_dir = 'SIMSbTY0_ordered'

data_lst = []
for f in os.listdir(parent_dir):
    if "SIMS" in f:
        seed = f.split(".")[0].split('-')[-1]
        nsim = f.split('.')[0].split('-')[-2]
        if "c." in f:
            data = pd.read_csv(os.path.join(parent_dir, f))
            data['seed'] = seed
            data['crossfit'] = True
            data['nsim'] = nsim
            data_lst.append(data)

        else:
            data = pd.read_csv(os.path.join(parent_dir, f))
            data['seed'] = seed
            data['crossfit'] = False
            data['nsim'] = nsim
            data_lst.append(data)



SIMS = pd.concat(data_lst)
df = SIMS.copy()
df = df.loc[:, ~df.isna().all()]
df = df.dropna(axis = 0)

print(df.shape)
df.head()


# %%


# %%
alphas = np.arange(0, 1, 0.01)

reject_naive = [(df['ATENaivepval'] < alpha).mean() for alpha in alphas]
reject_boot = [(df['ATEBoot00pval'] < alpha).mean() for alpha in alphas]
reject_boot12 = [(df['ATEBoot12pval'] < alpha).mean() for alpha in alphas]
reject_oracle = [(df['ground_truthpval'] < alpha).mean() for alpha in alphas]

# %%
fig, axes = plt.subplots(1,len(df['nsim'].unique()), figsize=(16, 8), sharex=True)
if len(df['nsim'].unique()) == 1:
    axes = [axes]

for idx, nsim in enumerate(df['nsim'].unique()):
    df_nsim = df[df['nsim'] == nsim]
    reject_naive_nsim = [(df_nsim['ATENaivepval'] < alpha).mean() for alpha in alphas]
    reject_boot00_nsim = [(df_nsim['ATEBoot00pval'] < alpha).mean() for alpha in alphas]
    reject_boot12_nsim = [(df_nsim['ATEBoot12pval'] < alpha).mean() for alpha in alphas]
    reject_oracle_nsim = [(df_nsim['ground_truthpval'] < alpha).mean() for alpha in alphas]
    
    axes[idx].plot(alphas, reject_naive_nsim, label='Naive')
    axes[idx].plot(alphas, reject_boot00_nsim, label='Boot')
    axes[idx].plot(alphas, reject_boot12_nsim, label='Boot12')
    axes[idx].plot(alphas, reject_oracle_nsim, label='Oracle')

    axes[idx].set_title(f'nsim = {nsim}', fontsize=20)
    axes[idx].set_ylabel('Type I Error Rate', fontsize=20)
    axes[idx].legend(fontsize=20)

plt.xlabel('Significance Level')
fig.suptitle('Type I Error Rate for Ordinal Regression', fontsize=24)
plt.tight_layout()
plt.savefig('figs/type1error_naive_boot.png')


# %%
fig, axes = plt.subplots(1,len(df['nsim'].unique()), figsize=(16, 8), sharex=True)
if len(df['nsim'].unique()) == 1:
    axes = [axes]

# Number of bootstrap samples
n_bootstrap = 1000

for idx, nsim in enumerate(["100","1000"]):
    df_nsim = df[df['nsim'] == nsim]
    
    # Initialize arrays to store bootstrap results
    reject_naive_boot = np.zeros((n_bootstrap, len(alphas)))
    reject_boot00_boot = np.zeros((n_bootstrap, len(alphas)))
    reject_boot12_boot = np.zeros((n_bootstrap, len(alphas)))
    reject_oracle_boot = np.zeros((n_bootstrap, len(alphas)))
    
    # Perform bootstrap
    for b in range(n_bootstrap):
        boot_idx = np.random.choice(len(df_nsim), size=len(df_nsim), replace=True)
        df_boot = df_nsim.iloc[boot_idx]
        
        reject_naive_boot[b] = [(df_boot['ATENaivepval'] < alpha).mean() for alpha in alphas]
        reject_boot00_boot[b] = [(df_boot['ATEBoot00pval'] < alpha).mean() for alpha in alphas]
        reject_oracle_boot[b] = [(df_boot['ground_truthpval'] < alpha).mean() for alpha in alphas]
    
    # Calculate means and confidence intervals
    reject_naive_mean = reject_naive_boot.mean(axis=0)
    reject_boot00_mean = reject_boot00_boot.mean(axis=0)
    reject_boot12_mean = reject_boot12_boot.mean(axis=0)
    reject_oracle_mean = reject_oracle_boot.mean(axis=0)
    
    reject_naive_ci = np.percentile(reject_naive_boot, [2.5, 97.5], axis=0)
    reject_boot00_ci = np.percentile(reject_boot00_boot, [2.5, 97.5], axis=0)
    reject_boot12_ci = np.percentile(reject_boot12_boot, [2.5, 97.5], axis=0)
    reject_oracle_ci = np.percentile(reject_oracle_boot, [2.5, 97.5], axis=0)
    
    # Plot means and confidence intervals
    axes[idx].plot(alphas, reject_naive_mean, label='Naive')
    axes[idx].fill_between(alphas, reject_naive_ci[0], reject_naive_ci[1], alpha=0.2)
    
    axes[idx].plot(alphas, reject_boot00_mean, label='Boot')
    axes[idx].fill_between(alphas, reject_boot00_ci[0], reject_boot00_ci[1], alpha=0.2)
    
    axes[idx].plot(alphas, reject_oracle_mean, label='Oracle')
    axes[idx].fill_between(alphas, reject_oracle_ci[0], reject_oracle_ci[1], alpha=0.2)

    axes[idx].set_title(f'nsim = {nsim}', fontsize=20)
    axes[idx].set_ylabel('Type I Error Rate', fontsize=20)
    axes[idx].set_xlabel('Significance Level', fontsize = 20)
    axes[idx].legend(fontsize=20)
    axes[idx].tick_params(axis='both', labelsize=16)

fig.suptitle('Type I Error Rate for Naïve, Oracle, and Bayesian Bootstrap', fontsize=24)
plt.tight_layout()
plt.savefig('figs/type1error_naive_boot_binary.png')


# %%
# alphas = np.arange(0, 1, 0.01)

# reject_naive = [(df['ATEBoot12pval'] < alpha).mean() for alpha in alphas]
# reject_boot = [(df['ATEBoot11pval'] < alpha).mean() for alpha in alphas]

# plt.plot(alphas, reject_naive, label = 'Naive')
# plt.plot(alphas, reject_boot, label = 'Boot')
# plt.plot(alphas, alphas, label = 'Nominal', linestyle='-')
# plt.title('Type I Error Rate for Ordinal Regression')
# plt.xlabel('Significance Level')
# plt.ylabel('Type I Error Rate')
# plt.suptitle(f'n = {len(alphas)}')
# plt.legend()
# plt.show()



