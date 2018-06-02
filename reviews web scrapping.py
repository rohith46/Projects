#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Feb 27 23:21:54 2018

@author: rohithbharatha
"""
"""
import requests,bs4,pprint

url="https://www.amazon.in/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords=sport+shoes"

req=requests.get(url)
soup=bs4.BeautifulSoup(req.text,"html5lib")


shoe_prices=soup.select('<span class="currencyINR">&nbsp;&nbsp;</span>')

print(shoe_prices[2])
pprint.pprint(shoe_prices)


shoe_data = soup.select('.a-size-medium')
shoe_data = soup.select('.a-size-small.a-link-normal.a-text-normal')
print(len(shoe_data))
print(pprint.pprint(shoe_data))"""

import pandas as pd
import numpy as np

import json,requests,pprint

add="UpGrad, Nishuvi building, Anne Besant Road, Worli, Mumbai"
split_address=add.split(" ")
address="+".join(split_address)
print(address)

api_key = "AIzaSyBXrK8md7uaOcpRpaluEGZAtdXS4pcI5xo"

url = "https://maps.googleapis.com/maps/api/geocode/json?address={0}&key={1}".format(address, api_key)

r= requests.get(url)

print(type(r.text))
#print(r.text)

r_dict=json.loads(r.text)

pprint.pprint(r_dict['results'])


lat = r_dict['results'][0]['geometry']['location']['lat']
log=r_dict['results'][0]['geometry']['location']['lng']

print(lat,log)
