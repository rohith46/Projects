#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Mar 30 10:33:43 2018

@author: rohithbharatha
"""

import pandas as pd
import numpy as np
import fuzzywuzzy
from fuzzywuzzy import process
import chardet
np.random.seed(0)

#suicide_attacks = pd.read_csv('PakistanSuicideAttacks Ver 11 (30-November-2017).csv')


with open('PakistanSuicideAttacks Ver 11 (30-November-2017).csv', 'rb') as rawdata:
    result = chardet.detect(rawdata.read(100000))
print(result)

suicide_attacks = pd.read_csv('PakistanSuicideAttacks Ver 11 (30-November-2017).csv',
                              encoding = 'Windows-1252')

cities = suicide_attacks['City'].unique()

cities.sort()

cities

suicide_attacks['City'] = suicide_attacks['City'].str.lower()
suicide_attacks['City'] = suicide_attacks['City'].str.strip()

province = suicide_attacks['Province'].unique()
province.sort()
province

suicide_attacks['Province'] = suicide_attacks['Province'].str.lower()
suicide_attacks['Province']= suicide_attacks['Province'].str.strip()

province = suicide_attacks['Province'].unique()
province.sort()
province

cities = suicide_attacks['City'].unique()

# sort them alphabetically and then take a closer look
cities.sort()
cities


matches = fuzzywuzzy.process.extract('d.i khan', cities, limit = 10,
                                     scorer = fuzzywuzzy.fuzz.token_sort_ratio)

matches

matches = fuzzywuzzy.process.extract('kuram agency', cities, limit = 10, 
                                     scorer = fuzzywuzzy.fuzz.token_sort_ratio)
matches


def replace_matches_in_column(df, column, string_to_match, min_ratio = 90):
    strings = df[column].unique()
    matches = fuzzywuzzy.process.extract(string_to_match, strings, limit = 10, 
                                         scorer = fuzzywuzzy.fuzz.token_sort_ratio)
    close_matches = [matches[0] for matches in matches if matches[1] >= min_ratio]
    rows_with_matches = df[column].isin(close_matches)
    df.loc[rows_with_matches,column] = string_to_match
    print('All Done!')
    
replace_matches_in_column(df = suicide_attacks, column = 'City', string_to_match = 'd.i khan')

cities = suicide_attacks['City'].unique()

# sort them alphabetically and then take a closer look
cities.sort()
cities

replace_matches_in_column(df = suicide_attacks, column = 'City', string_to_match = 'kuram agency')

# version 6 suicide attacks 

with open('PakistanSuicideAttacks Ver 6 (10-October-2017).csv','rb') as rawdata:
    result = chardet.detect(rawdata.read(100000))
result

version6_attacks = pd.read_csv('PakistanSuicideAttacks Ver 6 (10-October-2017).csv',
                               encoding = 'Windows-1252')


version6_attacks.head(5)


cities = version6_attacks['City'].unique()
cities.sort()
cities

version6_attacks['City'] = version6_attacks['City'].str.lower()
version6_attacks['City'] = version6_attacks['City'].str.strip()


replace_matches_in_column(df = version6_attacks, column = 'City', string_to_match = 'd.i khan')

replace_matches_in_column(df = version6_attacks, column = 'City', string_to_match = 'kuram agency')

cities = version6_attacks['City'].unique()
cities.sort()
cities

list(version6_attacks.columns.values)
islamic = version6_attacks['Islamic'].unique()


targeted = version6_attacks['Targeted Sect if any'].unique()

targeted.sort()
targeted

version6_attacks['Islamic Date'].sample(10)
