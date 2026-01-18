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
from sklearn.ensemble import bagging, RandomForestClassifier


# ===================================================
#      Prediction of a categorical variable
# ===================================================

# Cancer Relaps dataset
url = "https://perso.univ-rennes1.fr/valerie.monbet/MachineLearning/Ex2_breast_cancer_ER_train_set_for_other_methods.gct"
X = np.loadtxt(urlopen(url),skiprows=3,usecols=range(2,99))
X = X.T
url = "https://perso.univ-rennes1.fr/valerie.monbet/MachineLearning/Ex2_breast_cancer_ER_train_set_class_labels.cls"
label = np.loadtxt(urlopen(url),skiprows=2)

url = "https://perso.univ-rennes1.fr/valerie.monbet/MachineLearning/Ex2_breast_cancer_ER_train_set_for_other_methods.gct"
names=np.genfromtxt(urlopen(url),usecols=0,skip_header=3,dtype='str')

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
  
# Find to genes with best SNR =================================================
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


# Bagging ============================================================





# Random Forest ============================================================
