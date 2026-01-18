#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Jul 29 13:13:23 2018

@author: valerie
"""

# ===================================================
#      Prediction of a categorical variable
# ===================================================

# Cancer Relaps dataset
url = "https://perso.univ-rennes1.fr/valerie.monbet/MachineLearning/Ex2_breast_cancer_ER_train_set_for_other_methods.gct"
X = np.loadtxt(urlopen(url),skiprows=3,usecols=range(2,99))
X = X.T
url = "https://perso.univ-rennes1.fr/valerie.monbet/MachineLearning/Ex2_breast_cancer_ER_train_set_class_labels.cls"
label = np.loadtxt(urlopen(url),skiprows=2)


#fichier = "/Users/valerie/Documents/ENSEIGNEMENT/MACHINE LEARNING/Master BMC/PROJETS/Ex_datasets/Ex2/Ex2_breast_cancer_ER_train_set_for_other_methods.gct"
#X = np.loadtxt(fichier,skiprows=3,usecols=range(2,99))
#fichier = "/Users/valerie/Documents/ENSEIGNEMENT/MACHINE LEARNING/Master BMC/PROJETS/Ex_datasets/Ex2/Ex2_breast_cancer_ER_train_set_class_labels.cls"
#label = np.loadtxt(fichier,skiprows=2)


# Rigde and Lasso regression ============================================================
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import train_test_split


alpha_values = [1e-6,1e-5,1e-4,1e-3,1e-2,1e-1,1,5]


B = 30 # number of samples for the cross validation
err_ridge= np.zeros((B,len(alpha_values)))
for b in range(0,B):
    X_train, X_test, y_train, y_test = train_test_split(X,label,test_size=9)
    mk=np.mean(X_train,axis=0)
    sk=np.maximum(np.std(X_train,axis=0),10*np.finfo(float).eps)
    X_train, X_test = np.add(X_train,-mk), np.add(X_test,-mk)
    X_train, X_test = np.multiply(X_train,1/sk),np.multiply(X_test,1/sk)
    for i,alpha in enumerate(alpha_values):
        ridge = LogisticRegression(penalty="l2",C=1/alpha)
        ridge.fit(X_train,y_train)
        y_pred = ridge.predict(X_test)
        err_ridge[b,i] = np.mean(y_pred!=y_test)
    

print("Error of Ridge regression: ", np.round(np.min(np.mean(err_ridge,axis=0))*100)/100)

imin = np.argmin(np.mean(err_ridge,axis=0))

# Plot of ROC curves ======================================================================
from sklearn.metrics import roc_curve, auc


def ROC(y_test,y_score,methodName=" ",plot=True):

    ntest = np.size(y_test,0)
    B = np.size(y_test,1)
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


# for the ROC curves plot we need to store y_test and y_prob
y = 2*label-1
B = 30 # number of samples for the cross validation
y_score = np.zeros((9,B))
y_test_all = np.zeros((9,B))
for b in range(0,B):
    X_train, X_test, y_train, y_test = train_test_split(X,y,test_size=9)
    mk=np.mean(X_train,axis=0)
    sk=np.maximum(np.std(X_train,axis=0),10*np.finfo(float).eps)
    X_train, X_test = np.add(X_train,-mk), np.add(X_test,-mk)
    X_train, X_test = np.multiply(X_train,1/sk),np.multiply(X_test,1/sk)

    ridge = LogisticRegression(penalty="l2",C=1/alpha_values[imin])
    ridge.fit(X_train,y_train)
    y_score[:,b] = ridge.predict_proba(X_test)[:,1]
    y_test_all[:,b] = y_test

plt.clf()
ROC(y_test_all, y_score, "Ridge")
