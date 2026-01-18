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

n_est_list = [20,50,100,500] # number of trees

B = 30 # number of samples for the cross validation
err_bag = np.zeros((B,len(n_est_list)))
prob_bag = []
ytest_bag = []
for b in range(0,B):
    X_train, X_test, y_train, y_test = train_test_split(X,y,test_size=.25)
    ytest_bag = np.concatenate((ytest_bag,np.ravel(y_test)))
    #SNR = genes_SNR(X_train,y_train)
    #keep = np.argsort(SNR)[-50:] 
    mk=np.mean(X_train,axis=0)
    sk=np.maximum(np.std(X_train,axis=0),10*np.finfo(float).eps)
    X_train, X_test = np.add(X_train,-mk), np.add(X_test,-mk)
    X_train, X_test = np.multiply(X_train,1/sk),np.multiply(X_test,1/sk)
    prob_tmp = []
    for i,n_est in enumerate(n_est_list):
        clf = bagging.BaggingClassifier(n_estimators=n_est,n_jobs=-1,max_features=20,oob_score=True) # n_job : ofr parallel computing
        clf.fit(X_train,y_train)
        y_pred = clf.predict(X_test)
        err_bag[b,i] = np.mean(y_pred!=y_test)
        prob_tmp = np.concatenate((prob_tmp,clf.predict_proba(X_test)[:,1]))
    if b>0:
        prob_bag = np.concatenate((prob_bag,np.reshape(prob_tmp,(len(n_est_list),len(y_test))).T),axis=0)
    else : prob_bag = np.reshape(prob_tmp,(len(y_test),len(n_est_list)))
    

err_bag_all = np.mean(err_bag,axis=0)

plt.figure(1)
plt.subplot(1,2,1)
plt.plot(err_bag_all,'ko')
plt.ylabel('Mean classification error')
plt.xlabel('Nb of trees')
plt.grid()
plt.subplot(1,2,2)
k0 = np.argmin(err_bag_all)
ROC(ytest_bag,prob_bag[:,k0],methodName="Bagging",plot=True)

# Random Forest ============================================================
from sklearn.ensemble import RandomForestClassifier

n_est_list = [20,50,100,500] # number of trees

B = 30 # number of samples for the cross validation
err_rf = np.zeros((B,len(n_est_list)))
prob_rf = []
ytest_rf = []
for b in range(0,B):
    X_train, X_test, y_train, y_test = train_test_split(X,y,test_size=.25)
    ytest_rf = np.concatenate((ytest_rf,np.ravel(y_test)))
    mk=np.mean(X_train,axis=0)
    sk=np.maximum(np.std(X_train,axis=0),10*np.finfo(float).eps)
    X_train, X_test = np.add(X_train,-mk), np.add(X_test,-mk)
    X_train, X_test = np.multiply(X_train,1/sk),np.multiply(X_test,1/sk)
    prob_tmp = []
    for i,n_est in enumerate(n_est_list):
        clf = RandomForestClassifier(n_estimators=n_est,n_jobs=-1,max_features=20) # n_job : for parallel computing
        clf.fit(X_train,y_train)
        y_pred = clf.predict(X_test)
        err_rf[b,i] = np.mean(y_pred!=y_test)
        prob_tmp = np.concatenate((prob_tmp,clf.predict_proba(X_test)[:,1]))
    if b>0:
        prob_rf = np.concatenate((prob_rf,np.reshape(prob_tmp,(len(n_est_list),len(y_test))).T),axis=0)
    else : prob_rf = np.reshape(prob_tmp,(len(y_test),len(n_est_list)))
    

err_rf_all = np.mean(err_rf,axis=0)

plt.figure(1)
plt.subplot(1,2,1)
plt.plot(err_rf_all,'ko')
plt.ylabel('Mean classification error')
plt.xlabel('Nb of trees')
plt.grid()
plt.subplot(1,2,2)
k0 = np.argmin(err_rf_all)
ROC(ytest_rf,prob_rf[:,k0],methodName="Random Forest",plot=True)

# Variable importance (last forest)
importances = clf.feature_importances_
indices = np.argsort(importances)[::-1]
print("Feature ranking (20 first:")
for f in range(20):
    print("%d. feature %d (%f)" % (f + 1, indices[f], importances[indices[f]]))

# Plot the feature importances of the last forest
plt.figure(2)
plt.clf()
plt.title("Feature importances (20 first)")
plt.barh(range(20), importances[indices][:20],
       color="r", align="center")
plt.yticks(range(20), [names[i] for i in indices[:20]])
plt.show()



# =============================================================================
# =============================================================================
# Another way to do the selection of the hyper parameters
# - more efficient from a computation time point of vue
# =============================================================================
from sklearn.model_selection import ShuffleSplit
from sklearn.model_selection import GridSearchCV, cross_val_score
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import BaggingClassifier

params = {'n_estimators': [40, 42], 
          'base_estimator__max_leaf_nodes':[10, 15], 
          'base_estimator__max_depth':[4, 5, 6]}

clf = GridSearchCV(bagging.BaggingClassifier(DecisionTreeClassifier(),
                        n_estimators = 100, max_features = 20),
                   param_grid=params, 
                   cv=ShuffleSplit(test_size=.25,
                                   n_splits=30, random_state=1), 
                   scoring = "roc_auc", 
                   refit=False,n_jobs=-1)
clf.fit(X, y)

print("Best parameters :",clf.best_params_)
print("Best score :",np.round(clf.best_score_,2))
