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
# In the following plot, color is varying according to the fat percent
fatn=(fat-np.min(fat))/(np.max(fat)-np.min(fat))
colors= plt.cm.inferno(fatn)
plt.figure(2)
for i in range(len(fat)):
   plt.plot(X[i,:],color=colors[i])
plt.title("Spectres NIR")
plt.ylabel("Absorbances")
plt.show()


# Find the waves length with highest  correlation with fat percent =============
p = X.shape[1]
rho = np.zeros(p)
for j in range(p):
    rho[j] = np.corrcoef(X[:,j],fat)[0,1]

rho_10 = np.sort(rho)[p-10]
keep = np.where(rho>=rho_10)[0]

plt.figure(2)
for i in range(len(fat)):
   plt.plot(X[i,:],"c")
for i in range(len(fat)):
   plt.plot(keep,X[i,keep],".",color=colors[i])
plt.title("Spectres NIR")
plt.ylabel("Absorbances")
plt.show()
# Compute PCA components ======================================================
# Standardisation
from sklearn.decomposition import PCA

def stdise(X):
  (n,m) = np.shape(X)
  Xs = np.zeros((n,m))
  for k in range(m):
    mk = np.mean(X[:,k])
    sk = np.std(X[:,k])
    Xs[:,k]=(X[:,k]-mk)/sk
  return Xs

# Plot the eigenvalues
Xs=stdise(X)/np.sqrt(np.shape(X)[0])
pca = PCA(n_components=10)
Xpca = pca.fit_transform(Xs)
plt.figure() 
plt.title('Pourcentage of explained variance')
plt.bar(np.arange(len(pca.explained_variance_ratio_)) ,pca.explained_variance_ratio_)

plt.figure()
plt.scatter(Xpca[:,0],Xpca[:,1],color=colors)

# Fit a linear model ==========================================================
# And use Cross validation