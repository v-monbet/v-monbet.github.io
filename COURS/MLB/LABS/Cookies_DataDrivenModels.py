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


fichier = "/Users/valerie/ENSEIGNEMENT/RADO/Biscuits.csv"
biscuits=np.loadtxt(fichier,skiprows=1,delimiter=";")

# Extraction de la colonne fat
fat=biscuits[:,0]
# Extraction des variables explicatives
X=biscuits[:,1:]
# Trace d'un spectre
plt.figure(1)
plt.plot(X[1,:])
plt.title("Un exemple de spectre")
# TIn the following plot, color is varying according to the fat percent
fatn=fatn/max(fatn)
colors= plt.cm.inferno(fatn)
plt.figure(2)
for i in range(len(fat)):
   plt.plot(X[i,:],color=colors[i])
plt.title("Spectres NIR")
plt.ylabel("Absorbances")
plt.show()

# knn ==========================================================
from sklearn import neighbors

nb_neighbors = [1,5,10,20]


B = 30 # number of samples for the cross validation
mse_knn= np.zeros((B,len(nb_neighbors)))
for b in range(0,B):
    X_train, X_test, y_train, y_test = train_test_split(X,fat,test_size=9)
    mk=np.mean(X_train,axis=0)
    sk=np.maximum(np.std(X_train,axis=0),10*np.finfo(float).eps)
    X_train, X_test = np.add(X_train,-mk), np.add(X_test,-mk)
    X_train, X_test = np.multiply(X_train,1/sk),np.multiply(X_test,1/sk)
    for i,k in enumerate(nb_neighbors):
        knn = neighbors.KNeighborsRegressor(k)
        knn.fit(X_train,y_train)
        y_pred = knn.predict(X_test)
        mse_knn[b,i] = np.mean((y_pred-y_test)**2)
    
plt.boxplot(err_knn,labels=nb_neighbors)
plt.ylabel('Mean classification error')
plt.xlabel('Nb of neighbors')

print("Error of knn method: ",np.sqrt(np.min(np.mean(mse_knn,axis=0))))

# This result could be improved by running knn in space with smaller dimension. 

# Decision Tree ============================================================
from sklearn import tree

B = 30
n_test = 9
mse_tree = np.zeros(B)
for b in range(B): 
    X_train, X_test, y_train, y_test = train_test_split(X,fat,test_size=n_test)
    mk=np.mean(X_train,axis=0)
    sk=np.maximum(np.std(X_train,axis=0),10*np.finfo(float).eps)
    X_train, X_test = np.add(X_train,-mk), np.add(X_test,-mk)
    X_train, X_test = np.multiply(X_train,1/sk),np.multiply(X_test,1/sk)
    clf = tree.DecisionTreeRegressor()
    clf.fit(X_train,y_train)
    y_pred = clf.predict(X_test)
    mse_tree[b] = np.mean((y_pred-y_test)**2)
    
 print("Error of tree regression: ",np.sqrt(np.mean(mse_tree)))
