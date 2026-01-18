#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Jul 27 11:51:27 2018

@author: valerie
"""

import numpy as np
import matplotlib.pyplot as plt
from urllib.request import urlopen
url="https://perso.univ-rennes1.fr/valerie.monbet/doc/cours/Biscuits.csv"
biscuits=np.loadtxt(urlopen(url),skiprows=1,delimiter=";")
# Extraction de la colonne fat
fat=biscuits[:,0]
# Extraction des variables explicatives
X=biscuits[:,1:]
# Trace d'un spectre
plt.figure(1)
plt.plot(X[1,:])
plt.title("Un exemple de spectre")
# TIn the following plot, color is varying according to the fat percent
fatn=(fat-np.min(fat))/(np.max(fat)-np.min(fat))
colors= plt.cm.inferno(fatn)
plt.figure(2)
for i in range(len(fat)):
   plt.plot(X[i,:],color=colors[i])
plt.title("Spectres NIR")
plt.ylabel("Absorbances")
plt.show()


# Find the waves legth with highest  correlation with fat percent =============
p = X.shape[1]
rho = np.zeros(p)
for j in range(p):
    rho[j] = np.corrcoef(X[:,j],fat)[0,1]

# Overfitting 
rho28 = np.sort(rho)[p-28]
keep = np.where(rho>=rho28)[0]

# Fit a linear model ==========================================================
from sklearn import linear_model
from sklearn.model_selection import train_test_split


X_train, X_test, y_train, y_test = train_test_split(X[:,keep],fat,test_size=4)




# Fit Ridge and Lasso regression ==============================================
from sklearn.linear_model import Ridge, Lasso

alpha_values = [1e-6,1e-5,1e-4,1e-3,1e-2,1e-1,1,5]


