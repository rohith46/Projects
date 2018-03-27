#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Mar 14 21:49:35 2018

@author: rohithbharatha
"""

import os
import pandas as pd
import seaborn as sns

#import datetime


os.getcwd()

#changing directory 
os.chdir('Desktop')

#loading data 
uberdata = pd.read_csv('Uber Request Data1.csv')

# checking the data type 
type(uberdata['Request timestamp'])
uberdata.head()
uberdata['Request timestamp']

uberdata.describe()

#missing values
uberdata.isnull().any(axis = 0)

# making consistent phrase in request and drop columns
uberdata['Request timestamp'] = uberdata['Request timestamp'].str.replace('/','-')

uberdata['Drop timestamp'] = uberdata['Drop timestamp'].str.replace('/','-')

uberdata.head()
#converting to date data type

uberdata['Request timestamp'] = uberdata['Request timestamp'].apply(lambda x: pd.to_datetime(x).strftime('%d-%m-%Y'))
uberdata['Request timestamp'] = pd.to_datetime(uberdata['Request timestamp'])
uberdata['Drop timestamp'] = pd.to_datetime(uberdata['Drop timestamp'])
uberdata['Request timestamp']
uberdata['Drop timestamp']
#pd.to_datetime(uberdata['Drop timestamp'])


#extract hour 
uberdata['hour'] = uberdata['Request timestamp'].dt.hour
uberdata['day'] = uberdata['Request timestamp'].dt.day
uberdata['day1'] = uberdata['Request timestamp'].dt.month
uberdata.head()

# plotting 

#sns.barplot(x= 'Status', y = 'hour', data = uberdata)

sns.countplot(x = 'hour' , hue = 'Status', data = uberdata)

sns.factorplot(x = 'hour' , hue = 'Status', col = 'day', kind = 'count', data = uberdata )

uberdata['day']
