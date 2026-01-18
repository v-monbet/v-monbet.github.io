#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Apr 18 13:18:33 2019

@author: valerie
"""


import numpy as np
from sklearn.decomposition import PCA
from sklearn import manifold
import matplotlib.pyplot as plt
import matplotlib.cm as cmx
from urllib.request import urlopen

# Chargement des données ======================================================
url="https://perso.univ-rennes1.fr/valerie.monbet/doc/cours/digits_extrait_images.csv"
digits=np.loadtxt(urlopen(url), delimiter=',',skiprows=1,usecols=range(1,785)) 

url="https://perso.univ-rennes1.fr/valerie.monbet/doc/cours/digits_extrait_labels.csv"
labels=np.loadtxt(urlopen(url), delimiter=',',skiprows=1,usecols=range(1,2))


K = int(np.max(labels)+1)
print("Number of different labels :", K)

X = np.copy(digits)
n = digits.shape[0]
d = digits.shape[1]

# Routine de standardisation ==================================================
def stdise(X):
  mk=np.mean(X,axis=0)
  # Calcul de l'écart-type avec max pour éviter une division par 0
  sk=np.maximum(np.std(X,axis=0),10*np.finfo(float).eps)
  Xs=np.add(X,-mk)
  Xs=np.multiply(Xs,1/sk)
  return Xs


# Après standardisation les colonnes sont de norme "nb de ligne" et non 1,
# on corrige cela.
#Xs = stdise(X)/np.sqrt(n)
Xs = digits/255.

# PCA =========================================================================
from sklearn.decomposition import PCA


pca = PCA(n_components=2)
D = pca.fit_transform(Xs)

# Plot of the individuals with a color by label



# Reconstruction
pca = PCA(n_components=5)
D = pca.fit_transform(Xs)
Xs_pca = pca.inverse_transform(D)
img_list = [0,3,1,7,15]
plt.figure()
for i,j in enumerate(img_list):
    plt.subplot(2,5,i+1)
    plt.imshow(Xs[j,:].reshape(28,28),cmap="gray")
    plt.xticks([])
    plt.yticks([])
    plt.subplot(2,5,i+5+1)
    plt.imshow(Xs_pca[j,:].reshape(28,28),cmap="gray")
    plt.xticks([])
    plt.yticks([])
plt.show()
    
# Loadings (latent variables)
pca = PCA(n_components=5)
D = pca.fit_transform(Xs)
loadings = pca.components_.T * np.sqrt(pca.explained_variance_)

plt.figure()
for k in range(5):
    plt.subplot(1,5,k+1)
    plt.imshow(loadings[:,k].reshape((28,28)),cmap="gray")
    plt.title(k+1)
    plt.xticks([])
    plt.yticks([])
plt.show()
        
    

# MDS =========================================================================


# t-SNE =======================================================================


