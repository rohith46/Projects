#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Mar 26 11:42:52 2018

@author: rohithbharatha
"""

import pandas as pd
import numpy as np
import os

os.getcwd()
os.chdir('Desktop')

nfl_data = pd.read_csv('NFL Play by Play 2009-2017 (v4).csv')
sf_permits = pd.read_csv('Building_Permits.csv')

np.random.seed(0)

nfl_data.sample(5)

sf_permits.sample(5)

missing_values_count = nfl_data.isnull().sum()

missing_values_count[1:10]

total_cells = np.product(nfl_data.shape)

total_missing_values  = missing_values_count.sum()

(total_missing_values/total_cells)*100

# 25% of data is missing 


missing_values_count_sf = sf_permits.isnull().sum()
missing_values_count_sf[1:12]

total_cells_sf = np.product(sf_permits.shape)
total_missing_values_sf = missing_values_count_sf.sum()

(total_missing_values_sf/total_cells_sf) * 100


#removing rows with na  
nfl_data.dropna()


columns_with_na_dropped = nfl_data.dropna(axis = 1)

columns_with_na_dropped.head()

#How much data we lost 

print('columns with original data set %d \n' % nfl_data.shape[1])
print('columns with na dropped %d \n' % columns_with_na_dropped.shape[1])


# removing rows in sf data set

rows_with_na_dropped = sf_permits.dropna()
rows_with_na_dropped
print("Rows in original sf_permits data set : %d " % sf_permits.shape[1])
print("Rows with na's dropped : %d " % rows_with_na_dropped.shape[1])


# columns in sf data set

columns_with_na_dropped_sf = sf_permits.dropna(axis = 1)
print('columns with original data set %d \n' % sf_permits.shape[1])
print('columns with na dropped %d \n' % columns_with_na_dropped_sf.shape[1])



