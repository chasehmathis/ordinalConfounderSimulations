
import pandas as pd
from math import *
import os
import re
import matplotlib.pyplot as plt
import numpy as np


parent_dir = 'SIMS'
data_lst = []
for f in os.listdir(parent_dir):
    if "SIMS" in f:
        seed = f.split(".")[0].split('-')[-1]
        nsim = f.split(".")[0].split('-')[-2]
        data = pd.read_csv(os.path.join(parent_dir, f))
        data['crossfit'] = False
        data['nsim'] = nsim
        data['seed'] = seed
        data_lst.append(data)



df = pd.concat(data_lst)
df = df.loc[:, ~df.isna().all()]
df = df.dropna(axis = 0)

print(df.shape)
df.head()


def error(x, y, squared = True):
    if squared:
        return (x - y)**2
    else:
        return abs(x - y)


df['errorBB'] = error(df['ATEBayesBootstrap'], df['bTY'])
df['errorNaive'] = error(df['ATENaive'], df['bTY'])


df['oracle'] = error(df['RegressionTrue'], df['bTY'])




grouped_df = df.groupby(['nsim'])


# Create summary table
summary_table = pd.DataFrame({
    'errorBB_mean': grouped_df['errorBB'].mean(),
    'errorNaive_mean': grouped_df['errorNaive'].mean(), 
    'errorBB_median': grouped_df['errorBB'].median(),
    'errorNaive_median': grouped_df['errorNaive'].median(),
    'oracle_mean': grouped_df['oracle'].mean(),
    'oracle_median': grouped_df['oracle'].median()
})

# Write to CSV
summary_table.to_csv('figs/error_summary_stats_binary.csv')

from scipy.stats import bootstrap
df['corCategorries'] = pd.cut(df['PartialCorrTYU'].abs(), 
                             bins=[-float('inf'), 0.25, 0.75, float('inf')],
                             labels=['low', 'medium', 'high'])
df['diff_error'] = df['errorNaive'] - df['errorBB']

# Calculate medians and confidence intervals
medians = df.groupby(['nsim','corCategorries'])['diff_error'].median()

import pdb; pdb.set_trace()
cis = df.groupby(['nsim','corCategorries']).apply(lambda x: bootstrap((x['diff_error'],), np.median, confidence_level=0.95))

# Create dataframe with median and CI bounds
results_df = pd.DataFrame({
    'median': medians,
    'ci_lower': [ci.confidence_interval[0] for ci in cis],
    'ci_upper': [ci.confidence_interval[1] for ci in cis]
})

# %%
# Create figure with two subplots side by side
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(12, 5))

# Plot for nsim = 100
results_100 = results_df.xs("100", level='nsim')
ax1.bar(range(len(results_100)), results_100['median'])
ax1.errorbar(range(len(results_100)), results_100['median'],
            yerr=[results_100['median'] - results_100['ci_lower'],
                  results_100['ci_upper'] - results_100['median']],
            fmt='none', capsize=5, color='black')
ax1.set_xticks(range(len(results_100)))
ax1.set_xticklabels(results_100.index, fontsize=20)
ax1.set_ylabel('Median Difference in Error', fontsize=20)
ax1.set_xlabel('Correlation Categories', fontsize=20)
ax1.set_title('nsim = 100', fontsize=20)
ax1.tick_params(axis='y', labelsize=16)

# Plot for nsim = 1000
results_1000 = results_df.xs("1000", level='nsim')
ax2.bar(range(len(results_1000)), results_1000['median'])
ax2.errorbar(range(len(results_1000)), results_1000['median'],
            yerr=[results_1000['median'] - results_1000['ci_lower'],
                  results_1000['ci_upper'] - results_1000['median']],
            fmt='none', capsize=5, color='black')
ax2.set_xticks(range(len(results_1000)))
ax2.set_xticklabels(results_1000.index, fontsize=20)
ax2.set_ylabel('Median Difference in Error', fontsize=20)
ax2.set_xlabel('Correlation Categories', fontsize=20)
ax2.set_title('nsim = 1000', fontsize=20)
ax2.tick_params(axis='y', labelsize=16)

plt.tight_layout()
plt.subplots_adjust(top=0.85)  # Make room for suptitle
plt.suptitle("Median Squared Error Stratified By Partial Correlation", fontsize=20)
plt.savefig('figs/median_error_by_correlation_binary.png')

plt.close()




