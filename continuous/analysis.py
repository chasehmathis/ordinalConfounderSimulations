
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

# %%
df['error00'] = error(df['ATEBoot00'], df['bTY'])
df['errorNaive'] = error(df['ATENaive'], df['bTY'])
df['error10'] = error(df['ATEBoot10'], df['bTY'])
df['error11'] = error(df['ATEBoot11'], df['bTY'])
df['error12'] = error(df['ATEBoot12'], df['bTY'])


df['error3'] = error(df['P2SLS'], df['bTY'])
df['oracle'] = error(df['RegressionTrue'], df['bTY'])



# %%
grouped_df = df.groupby(['nsim'])

# %%
grouped_df['error00'].mean(), grouped_df['errorNaive'].mean(), grouped_df['error00'].median(), grouped_df['errorNaive'].median()

# %%
grouped_df['error10'].mean(), grouped_df['error11'].mean(), grouped_df['error12'].mean(), grouped_df['error10'].median(), grouped_df['error11'].median(), grouped_df['error12'].median()

# %%
grouped_df['error3'].mean(), grouped_df['error3'].median()  

# %%
grouped_df['oracle'].mean(), grouped_df['oracle'].median()

# %%
# plt.scatter(df['PartialCorrTYU'], df['error11'])
# plt.scatter(df['PartialCorrTYU'], df['error12'])
plt.scatter(df['PartialCorrTYU'], df['error00'] - df['errorNaive'])
plt.ylim(-0.15, 0.15)


# %%
from scipy.stats import bootstrap
df['corCategorries'] = pd.cut(df['PartialCorrTYU'].abs(), 
                             bins=[-float('inf'), 0.25, 0.75, float('inf')],
                             labels=['low', 'medium', 'high'])
df['diff_error'] = df['errorNaive'] - df['error00']

# Calculate medians and confidence intervals
medians = df.groupby(['nsim','corCategorries'])['diff_error'].median()


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

# %%
# Create bar plot with confidence intervals
ax = plt.bar(range(len(results_df)), results_df['median'])

# Add error bars using confidence intervals
plt.errorbar(range(len(results_df)), results_df['median'],
            yerr=[results_df['median'] - results_df['ci_lower'], 
                  results_df['ci_upper'] - results_df['median']],
            fmt='none', capsize=5, color='black')

# Customize plot
plt.xticks(range(len(results_df)), results_df.index)
plt.ylabel('Median Difference in Error')
plt.xlabel('Correlation Categories')
plt.title('Median Error Difference by Correlation Category\nwith 95% Confidence Intervals')

plt.savefig('figs/error_by_correlation.png')



