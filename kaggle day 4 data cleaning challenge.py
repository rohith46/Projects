#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Mar 29 09:58:25 2018

@author: rohithbharatha
"""

import pandas as pd
import numpy as np
import chardet
import os

np.random.seed(0)

before = "This is the euro symbol: €"

type(before)

after = before.encode('utf-8',errors = 'replace' )

type(after)

before = 'This is a hindi word : नमस्ते'
type(before)

after = before.encode('utf-8', errors = 'replace')

type(after)
after
print(after.decode('utf-8'))

os.chdir('datasets')

kickstarter_2016 = pd.read_csv("ks-projects-201612.csv")
with open("ks-projects-201801.csv", 'rb') as rawdata:
    result = chardet.detect(rawdata.read(10000))

# check what the character encoding might be
print(result)


kickstarter_2016 = pd.read_csv("ks-projects-201612.csv", encoding = 'Windows-1252')

kickstarter_2016.head()

policekillings = pd.read_csv('PoliceKillingsUS.csv')

with open('PoliceKillingsUS.csv','rb') as rawdata:
    result = chardet.detect(rawdata.read(100000))
print(result)

policekillings = pd.read_csv('PoliceKillingsUS.csv', encoding = 'Windows-1252')


policekillings.to_csv('PoliceKillingsUS-utf8.csv')