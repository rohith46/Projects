#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue May 29 17:25:44 2018

@author: rohithbharatha
"""
import numpy as np
import pandas as pd
from keras.layers import Dense
from keras.models import Sequential
from keras.utils import to_categorical
from keras.callbacks import EarlyStopping

digits = pd.read_csv('train_digit.csv')

digits.head()

predictors = digits.drop('label', axis =1)

predictors = predictors.values
#target= digits['label']

target = to_categorical(digits['label'])
model = Sequential()

model.add(Dense(100, activation='relu', input_shape=(784,)))
model.add(Dense(100, activation='relu'))
model.add(Dense(100, activation='relu'))
model.add(Dense(100, activation='relu'))
model.add(Dense(100, activation='relu'))
model.add(Dense(10, activation='softmax'))

early_stopping_monitor = EarlyStopping(patience=3)
model.compile(optimizer='adam', loss='categorical_crossentropy', metrics=['accuracy'])

model.fit(predictors, target, validation_split=0.1, epochs=30, callbacks=[early_stopping_monitor])

test = pd.read_csv('test_digit.csv')

test.head()

pred = model.predict_classes(test.values)

results = pd.Series(pred,name="Label")

submission = pd.concat([pd.Series(range(1,28001),name = "ImageId"),results],axis = 1)

submission.to_csv("dnn_mnist_datagen.csv",index=False)
