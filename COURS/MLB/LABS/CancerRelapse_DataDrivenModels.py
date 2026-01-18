#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Jul 29 13:13:23 2018

@author: valerie
"""

import matplotlib.pyplot as plt
import numpy as np
from urllib.request import urlopen

from sklearn.model_selection import train_test_split
from sklearn import neighbors, tree

# ===================================================
#      Prediction of a categorical variable
# ===================================================

# Cancer Relaps dataset
url = "https://perso.univ-rennes1.fr/valerie.monbet/MachineLearning/Ex2_breast_cancer_ER_train_set_for_other_methods.gct"
X = np.loadtxt(urlopen(url),skiprows=3,usecols=range(2,99))
X = X.T
url = "https://perso.univ-rennes1.fr/valerie.monbet/MachineLearning/Ex2_breast_cancer_ER_train_set_class_labels.cls"
label = np.loadtxt(urlopen(url),skiprows=2)

y = label

# Plot of ROC curves ==========================================================
from sklearn.metrics import roc_curve, auc


def ROC(y_test,y_score,methodName=" ",plot=True):

    ntest = np.size(y_test,0)
    if len(y_test.shape)>1: B = np.size(y_test,1)
    else : B=1
    fpr, tpr, _ = roc_curve(np.reshape(y_test,B*ntest), np.reshape(y_score,B*ntest))
#    if len(fpr)<3:
#        print("Problem: len(fpr) is lower than 3")
#        return
    roc_auc = auc(fpr, tpr)

    if plot:
        lw = 2
        plt.plot(fpr, tpr, color='darkorange',
            lw=lw, label='ROC curve (area = %0.2f)' % roc_auc)
        plt.plot([0, 1], [0, 1], color='navy', lw=lw, linestyle='--')
        plt.xlim([0.0, 1.0])
        plt.ylim([0.0, 1.05])
        plt.xlabel('False Positive Rate')
        plt.ylabel('True Positive Rate')
        plt.title(methodName)
        plt.legend(loc="lower right")
        plt.show()
    return(roc_auc)

# For genes swith best SNR selection ===========================================

def genes_SNR(X,y):
  # - Inputs
  # X = dataset
  # y = labels (a boolean or factor variable)
  # - Output
  # SNR
  labels = np.unique(y)
  n,p = X.shape
  K = len(labels)
  
  means = np.zeros((K,p))
  sd = np.std(X,axis=0)
  
  for k in range(K):
      means[k,] = np.mean(X[y==labels[k],:],axis=0)
  SNR = np.max(np.reshape(np.abs(np.diff(means,axis=0)),(K-1,p)),axis=0)/sd
  return(SNR)
  
# Nearest neighbor algorithm ==================================================
nb_neighbors = [1,3,5,10,20]


B = 30 # number of samples for the cross validation
err_knn= np.zeros((B,len(nb_neighbors)))
prob_knn = []
ytest_knn = []
for b in range(0,B):
    X_train, X_test, y_train, y_test = train_test_split(X,y,test_size=.25)
    ytest_knn = np.concatenate((ytest_knn,np.ravel(y_test)))
    SNR = genes_SNR(X_train,y_train)
    keep = np.argsort(SNR)[-50:] 
    mk=np.mean(X_train,axis=0)
    sk=np.maximum(np.std(X_train,axis=0),10*np.finfo(float).eps)
    X_train, X_test = np.add(X_train,-mk), np.add(X_test,-mk)
    X_train, X_test = np.multiply(X_train,1/sk),np.multiply(X_test,1/sk)
    prob_tmp = []
    for i,k in enumerate(nb_neighbors):
        knn = neighbors.KNeighborsClassifier(k)
        knn.fit(X_train[:,keep],np.ravel(y_train))
        y_pred = knn.predict(X_test[:,keep])
        err_knn[b,i] = np.mean(y_pred!=y_test)
        prob_tmp = np.concatenate((prob_tmp,knn.predict_proba(X_test[:,keep])[:,1]))
    if b>0:
        prob_knn = np.concatenate((prob_knn,np.reshape(prob_tmp,(len(nb_neighbors),len(y_test))).T),axis=0)
    else : prob_knn = np.reshape(prob_tmp,(len(y_test),len(nb_neighbors)))
    
err_knn_all = np.mean(err_knn,axis=0)

plt.figure(1)
plt.subplot(1,2,1)
plt.plot(err_knn_all,'ko')
plt.ylabel('Mean classification error')
plt.xlabel('Nb of neighbors')
plt.grid()
plt.subplot(1,2,2)
k0 = np.argmin(err_knn_all)
ROC(ytest_knn,prob_knn[:,k0],methodName="knn",plot=True)


# Classification tree algorithm ==================================================

B = 30 # number of samples for the cross validation
err_tree= []
prob_tree = []
ytest_tree = []
for b in range(0,B):
    X_train, X_test, y_train, y_test = train_test_split(X,y,test_size=.25)
    ytest_tree = np.concatenate((ytest_tree,np.ravel(y_test)))
    SNR = genes_SNR(X_train,y_train)
    keep = np.argsort(SNR)[-50:] 
    mk=np.mean(X_train,axis=0)
    sk=np.maximum(np.std(X_train,axis=0),10*np.finfo(float).eps)
    X_train, X_test = np.add(X_train,-mk), np.add(X_test,-mk)
    X_train, X_test = np.multiply(X_train,1/sk),np.multiply(X_test,1/sk)
    clf = tree.DecisionTreeClassifier()
    clf.fit(X_train[:,keep],y_train)
    y_pred = clf.predict(X_test[:,keep])
    err_tree.append(np.mean(y_pred!=y_test))
    prob_tree = np.concatenate((prob_tree,clf.predict_proba(X_test[:,keep])[:,1]))

print("Mean classification error, tree: ",round(np.mean(err_tree),2))     
plt.figure(2)         
ROC(ytest_tree,prob_tree,"Tree")
