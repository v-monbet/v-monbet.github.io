#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Jul 27 11:51:27 2018

@author: valerie
"""

import numpy as np
import matplotlib.pyplot as plt
from urllib.request import urlopen
from sklearn.ensemble import AdaBoostRegressor
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.tree import DecisionTreeRegressor
from sklearn.model_selection import train_test_split
import xgboost as xgb


url="https://perso.univ-rennes1.fr/valerie.monbet/doc/cours/Biscuits.csv"
biscuits=np.loadtxt(urlopen(url),skiprows=1,delimiter=";")


# Extraction de la colonne fat
fat=biscuits[:,0]
# Extraction des variables explicatives
X=biscuits[:,1:]

# In the following plot, color is varying according to the fat percent
fatn=(fat-np.min(fat))/(np.max(fat)-np.min(fat))
colors= plt.cm.inferno(fatn)
plt.figure(2)
for i in range(len(fat)):
   plt.plot(X[i,:],color=colors[i])
plt.title("Spectres NIR")
plt.ylabel("Absorbances")
plt.show()


y = fat
# Adaboost ============================================================
 
B = 100
n_test = 4
mse_AdaBoost = np.zeros(B) 



# Gradient Boosting ============================================================




# eXtreme Gradient Boosting ============================================================





