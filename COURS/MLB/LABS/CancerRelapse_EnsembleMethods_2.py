#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Jul 29 13:13:23 2018

@author: valerie
"""

# ===================================================
#      Prediction of a categorical variable
# ===================================================
import numpy as np
import matplotlib.pyplot as plt
from urllib.request import urlopen

# Cancer Relaps dataset
url = "https://perso.univ-rennes1.fr/valerie.monbet/MachineLearning/Ex2_breast_cancer_ER_train_set_for_other_methods.gct"
X = np.loadtxt(urlopen(url),skiprows=3,usecols=range(2,99))
X = X.T
names = np.genfromtxt(urlopen(url),dtype='str',usecols=1,skip_header=3)

url = "https://perso.univ-rennes1.fr/valerie.monbet/MachineLearning/Ex2_breast_cancer_ER_train_set_class_labels.cls"
label = np.loadtxt(urlopen(url),skiprows=2)

y = label

## ============================================================================
## Import utilities
from sklearn.model_selection import train_test_split
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

# Adaboost ============================================================
 
from sklearn.ensemble import AdaBoostClassifier
from sklearn.tree import DecisionTreeClassifier
from sklearn.model_selection import train_test_split


B = 30
n_test = 9
y_score = np.zeros([n_test,B]) 
y_test_all = np.zeros([n_test,B]) 
for b in range(B): 
    X_train, X_test, y_train, y_test = train_test_split(X,label,test_size=n_test)
    mk=np.mean(X_train,axis=0)
    sk=np.maximum(np.std(X_train,axis=0),10*np.finfo(float).eps)
    X_train, X_test = np.add(X_train,-mk), np.add(X_test,-mk)
    X_train, X_test = np.multiply(X_train,1/sk),np.multiply(X_test,1/sk)
    clf = AdaBoostClassifier(DecisionTreeClassifier(max_depth=2),n_estimators=20)
    clf.fit(X_train,y_train)
    y_score[:,b] = clf.predict_proba(X_test)[:,1]
    y_test_all[:,b] = y_test
              
ROC(y_test_all,y_score,"AdaBoost")

importances = clf.feature_importances_
indices = np.argsort(importances)[::-1]


print("Feature ranking (20 first:")

for f in range(20):
    print("%d. feature %d (%f)" % (f + 1, indices[f], importances[indices[f]]))

# Plot the feature importances of the last forest
plt.figure(1)
plt.clf()
plt.title("Feature importances (20 first)")
plt.barh(range(20), importances[indices][:20],
       color="r", align="center")
plt.yticks(range(20), [names[i] for i in indices[:20]])
plt.show()


# Gradient Boosting ============================================================
from sklearn.ensemble import GradientBoostingClassifier

B = 30
n_test = 9
y_score = np.zeros([n_test,B]) 
y_test_all = np.zeros([n_test,B]) 
for b in range(B): 
    X_train, X_test, y_train, y_test = train_test_split(X,label,test_size=n_test)
    mk=np.mean(X_train,axis=0)
    sk=np.maximum(np.std(X_train,axis=0),10*np.finfo(float).eps)
    X_train, X_test = np.add(X_train,-mk), np.add(X_test,-mk)
    X_train, X_test = np.multiply(X_train,1/sk),np.multiply(X_test,1/sk)
    clf = GradientBoostingClassifier(loss='exponential',n_estimators=200,max_features=5)
    clf.fit(X_train,y_train)
    y_score[:,b] = clf.predict_proba(X_test)[:,1]
    y_test_all[:,b] = y_test
              
ROC(y_test_all,y_score,"Gradient Boosting")

importances = clf.feature_importances_
indices = np.argsort(importances)[::-1]


print("Feature ranking (20 first:")

for f in range(20):
    print("%d. feature %d (%f)" % (f + 1, indices[f], importances[indices[f]]))

# Plot the feature importances of the last forest
plt.figure(1)
plt.clf()
plt.title("Feature importances (20 first)")
plt.barh(range(20), importances[indices][:20],
       color="r", align="center")
plt.yticks(range(20), [names[i] for i in indices[:20]])
plt.show()


# eXtreme Gradient Boosting ============================================================
import xgboost as xgb

B = 30
n_test = 9
y_pred_proba = []
ytest_xgb = [] 
ncol = X.shape[1]
for b in range(B): 
    X_train, X_test, y_train, y_test = train_test_split(X,label,test_size=n_test)
    ytest_xgb.append(y_test)
    mk=np.mean(X_train,axis=0)
    sk=np.maximum(np.std(X_train,axis=0),10*np.finfo(float).eps)
    X_train, X_test = np.add(X_train,-mk), np.add(X_test,-mk)
    X_train, X_test = np.multiply(X_train,1/sk),np.multiply(X_test,1/sk)
    dtrain = xgb.DMatrix(data=X_train,label=y_train)
    dtest = xgb.DMatrix(data=X_test,label=y_test)
    params = {'n_estimators': 100, 'max_depth': 2, 'min_samples_split': 5,
               'eta': 1,'objective':'reg:logistic','alpha': .1,'silent': 1,
               'early_stopping_rounds': 3}
    watchlist = [(dtest, 'eval')]
    num_round=50
    bst = xgb.train(params,dtrain,num_round,watchlist)
    y_pred_proba.append(bst.predict(dtest))

              
ROC(ytest_xgb,y_pred_proba,"XGB")

##
print('XGBoost with grid search')
# play with these params
params={
    'max_depth': [2,5,9], #[3,4,5,6,7,8,9], # 5 is good but takes too long in kaggle env
    'subsample': [.05,.1,.5], 
    'colsample_bytree': [.5,.7], 
    'n_estimators': [10,100,1000], #[1000,2000,3000]
    'reg_alpha': [0.03,.1,.5,1] #[0.01, 0.02, 0.03, 0.04]
}

xgb_clf = xgb.XGBClassifier()
clf = GridSearchCV(xgb_clf,
                  params,
                  cv=ShuffleSplit(test_size=.25,
                                   n_splits=30, random_state=1), 
                  scoring = "accuracy", 
                  refit=False,n_jobs=-1)
clf.fit(X,y)
print("Best parameters :",rs.best_params_)
print("Best accuracy :",np.round(np.sqrt(-rs.best_score_),2))

