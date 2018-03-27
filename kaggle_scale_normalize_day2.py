#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Mar 27 10:52:42 2018

@author: rohithbharatha
"""

# modules we'll use
import pandas as pd
import numpy as np

# for Box-Cox Transformation
from scipy import stats

# for min_max scaling
from mlxtend.preprocessing import minmax_scaling

# plotting modules
import seaborn as sns
import matplotlib.pyplot as plt

# read in all our data
kickstarters_2017 = pd.read_csv("ks-projects-201801.csv")

# set seed for reproducibility
np.random.seed(0)

# generate 1000 data points randomly drawn from an exponential distribution
original_data = np.random.exponential(size = 1000)

# mix-max scale the data between 0 and 1
scaled_data = minmax_scaling(original_data, columns = [0])

# plot both together to compare
fig, ax=plt.subplots(1,2)
sns.distplot(original_data, ax=ax[0])
ax[0].set_title("Original Data")
sns.distplot(scaled_data, ax=ax[1])
ax[1].set_title("Scaled data")

# normalize the exponential data with boxcox
normalized_data = stats.boxcox(original_data)

# plot both together to compare
fig, ax=plt.subplots(1,2)
sns.distplot(original_data, ax=ax[0])
ax[0].set_title("Original Data")
sns.distplot(normalized_data[0], ax=ax[1])
ax[1].set_title("Normalized data")


usd_goal = kickstarters_2017.usd_goal_real

scaled_data = minmax_scaling(usd_goal, columns = [0])

#plotting

fig, ax = plt.subplots(1,2)
sns.distplot(usd_goal, ax=ax[0])
ax[0].set_title('USD Goal')
sns.distplot(scaled_data,ax=ax[1])
ax[1].set_title('USD Goal Scaled data')

goal = kickstarters_2017.goal

scaled_data_goal = minmax_scaling(goal,columns = [0])

fig, ax = plt.subplots(1,2)
sns.distplot(goal,ax=ax[0])
ax[0].set_title('Original Goal Data')
sns.distplot(scaled_data_goal, ax=ax[1])
ax[1].set_title('Scaled Goal Data')

indexes_usd_pledged_real = kickstarters_2017.usd_pledged_real > 0 

positive_usd_pledged_real = kickstarters_2017.usd_pledged_real.loc[indexes_usd_pledged_real]

normalized_data_positive_usd = stats.boxcox(positive_usd_pledged_real)[0]

fig, ax = plt.subplots(1,2)
sns.distplot(positive_usd_pledged_real,ax=ax[0])
ax[0].set_title('Original Pledged Data')
sns.distplot(normalized_data_positive_usd, ax=ax[1])
ax[1].set_title('Normalized Pledged Data ')


indexes_pledged = kickstarters_2017.pledged > 0 

positive_pledged = kickstarters_2017.pledged[indexes_pledged]

normalized_pledged = stats.boxcox(positive_pledged)[0]

fig, ax = plt.subplots(1,2)
sns.distplot(positive_pledged,ax=ax[0])
ax[0].set_title('Original Pledged Data')
sns.distplot(normalized_pledged,ax=ax[1])
ax[1].set_title('Normalized Pledged Data')