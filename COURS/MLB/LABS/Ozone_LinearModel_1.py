#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Apr 11 13:48:37 2019

@author: valerie
"""

import matplotlib.pyplot as plt
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn import linear_model
from urllib.request import urlopen


# Data importation
# On lit le tableau comme un tableau de chaînes de caractères,
# on extrait les colonnes numériques, 
# on fabrique artisanalement les dummies avec les colonnes catégorielles,
# on reconstitue le tout.
url="https://perso.univ-rennes1.fr/bernard.delyon/tp/ozone.txt"
Ozone = np.loadtxt(urlopen(url),skiprows=1,dtype=np.ndarray)
nomvar=np.genfromtxt(urlopen(url),max_rows=1,dtype='str')
y=Ozone[:,[1]].astype(float)
X=Ozone[:,2:-2]
vent=Ozone[:,[-2]]
VO=(vent=='Ouest')*1   # categorical variables have to be recoded
VS=(vent=='Sud')*1
VE=(vent=='Est')*1
P=(Ozone[:,[-1]]=='Pluie')*1
X=np.concatenate((X,VO,VE,VS,P),axis=1).astype(float)
# Noms des variables explicatives
nomvar=np.concatenate((nomvar[1:-2],["Ouest"],["Sud"],["Est"],["Pluie"]))



lr = linear_model.LinearRegression(fit_intercept=True)

B = 30 # number of samples for the cross validation
mse_lr, mse_pca = np.zeros(B),np.zeros(B)
for b in range(0,B):
    # sampling for train, test sets
    X_train, X_test, y_train, y_test = train_test_split(X,y,test_size=0.25)

    # standardization
    numvar = np.arange(10) # list of quantitative variables
    mk=np.mean(X_train[:,numvar],axis=0)
    sk=np.maximum(np.std(X_train[:,numvar],axis=0),10*np.finfo(float).eps)
    X_train[:,numvar], X_test[:,numvar] = np.add(X_train[:,numvar],-mk), np.add(X_test[:,numvar],-mk)
    X_train[:,numvar], X_test[:,numvar] = np.multiply(X_train[:,numvar],1/sk),np.multiply(X_test[:,numvar],1/sk)
    
    # learning linear model
    lr.fit(X_train,y_train) # fit the linear model
    y_pred = lr.predict(X_test) # prediction for the test set
    mse_lr[b] = np.mean((y_test-y_pred)**2) # compute MSE
    
    y_pred_naive = np.mean(y_train)
    mse_naive = np.mean((y_test-y_pred_naive)**2)
    
    

print("Error of linear regression: ", str(np.round(np.sqrt(np.mean(mse_lr)),2)))
print("Error of naive prediction: ", str(np.round(np.sqrt(np.mean(mse_naive)),2)))
