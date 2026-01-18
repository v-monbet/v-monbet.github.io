#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Jul 24 09:50:18 2018

@author: valerie
"""

import numpy as np
import matplotlib.pyplot as plt
from urllib.request import urlopen
url="https://perso.univ-rennes1.fr/bernard.delyon/data/poterie.dat"
pot=np.loadtxt(urlopen(url),skiprows=1,usecols=range(0,10))
# Extraction de la colonne four
Four=pot[:,9]
# Elimination de cette colonne
pot=pot[:,np.arange(0,9)]
# Recuperation des noms de variable
potn=np.genfromtxt(urlopen(url),max_rows=1,dtype='str',usecols=range(0,9))

# Routine de standardisation
def stdise(X):
  mk=np.mean(X,axis=0)
  # Calcul de l'écart-type avec max pour éviter une division par 0
  sk=np.maximum(np.std(X,axis=0),10*np.finfo(float).eps)
  Xs=np.add(X,-mk)
  Xs=np.multiply(Xs,1/sk)
  return Xs

# SVD. Axes Composantes
# Après standardisation les colonnes sont de norme "nb de ligne" et non 1,
# on corrige cela.
Ps=stdise(pot)/np.sqrt(np.shape(pot)[0])
(U,D,VT) = np.linalg.svd(Ps,full_matrices=False)
V=VT.T

## Details of PCA (math formulation)
# Deux premieres composantes principales
C1 = D[0]*U[:,0]
C2 = D[1]*U[:,1]
# Axes principaux modifiés pour le cercle des corrélations
A1 = D[0]*V[:,0]
A2 = D[1]*V[:,1]

# Tracés
mp=['inferno','nipy_spectral','rainbow']
plt.close('all')
plt.title('Représentation des individus dans le plan\
 (C1,C2)\n La couleur correspond au four')
plt.scatter(C1,C2,c=-Four,cmap=plt.cm.get_cmap(mp[1]))
plt.xlabel('C1')
plt.ylabel('C2')
plt.show()

plt.figure() 
plt.title('Cercle des corrélations')
X = np.linspace(-np.pi, np.pi, 256,endpoint=True)
C,S = np.cos(X), np.sin(X)
plt.plot(C,S,c='black',lw=.7)
plt.axvline(c='black',ls='dashed',lw=1)
plt.axhline(c='black',ls='dashed',lw=1)
for i, txt in enumerate(potn):
  plt.arrow(0,0,A1[i],A2[i], length_includes_head=True,head_width=0.025, 
            head_length=.05)
  plt.annotate(txt, (A1[i]+.01,A2[i]+.01),fontsize=12)
plt.xlabel('C1')
plt.ylabel('C2')
plt.show()

plt.figure() 
plt.title('Valeurs propres')
plt.bar(np.arange(np.shape(D)[0])+1,D)
plt.show()

"""
Compute PCA using python functions
"""
import pandas as pd
import seaborn as sns
from sklearn.decomposition import PCA


pca = PCA(n_components=2)
D = pca.fit_transform(Ps)

df = pd.DataFrame({ "PC1":D[:,0],"PC2":D[:,1],"Four":Four })
sns.scatterplot(data=df, x="PC1", y="PC2", hue="Four",palette="tab10")
plt.show()



# Plot of the variables 
A1 = pca.components_[0,:]
A2 = pca.components_[1,:]
plt.figure()
plt.scatter(A1,A2,c='black',marker="+")
for i, txt in enumerate(potn):
    plt.annotate(txt, (A1[i]+.01,A2[i]+.01))
plt.xlabel('PC1')
plt.ylabel('PC2')
plt.show()

# Plot the eigenvalues
pca = PCA(n_components=Ps.shape[1])
D = pca.fit_transform(Ps)
plt.figure() 
plt.title('Pourcentage of explained variance')
plt.bar(np.arange(Ps.shape[1])+1,pca.explained_variance_ratio_)
plt.show()

"""
Compute MDS 
"""
from sklearn import manifold
from time import time


t0 = time()
mds = manifold.MDS(n_components=2, n_init=5)
Xmds = mds.fit_transform(Ps)
t1 = time()
print("MDS: %.2g sec" % (t1 - t0))

df = pd.DataFrame({ "Axis 1":Xmds[:,0],"Axis 2":Xmds[:,1],"Four":Four })
sns.scatterplot(data=df, x="Axis 1", y="Axis 2", hue="Four",palette="tab10")
plt.show()


"""
Isomap
"""
from sklearn import manifold
n_neighbors = 10
Xiso = manifold.Isomap(n_neighbors=n_neighbors, n_components=2).fit_transform(Ps)

df = pd.DataFrame({ "Axis 1":Xiso[:,0],"Axis 2":Xiso[:,1],"Four":Four })
sns.scatterplot(data=df, x="Axis 1", y="Axis 2", hue="Four",palette="tab10")
plt.show()



"""
tSNE
"""

t0 = time()
#tsne = manifold.TSNE(n_components=2, init='pca', random_state=0)
perp = 10
tsne = manifold.TSNE(n_components=2, init='pca',random_state=0,verbose=1,
                     perplexity=perp,n_iter=1000)

Xtsne = tsne.fit_transform(Ps)
t1 = time()
print("t-SNE: %.2g sec" % (t1 - t0))
df = pd.DataFrame({ "Axis 1":Xtsne[:,0],"Axis 2":Xtsne[:,1],"Four":Four })
sns.scatterplot(data=df, x="Axis 1", y="Axis 2", hue="Four",palette="tab10")

plt.title("tSNE, perplexity = "+str(perp))
plt.show()

