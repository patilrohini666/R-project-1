# -*- coding: utf-8 -*-
"""
Created on Fri Nov 30 14:17:55 2018

@author: Sanket
"""

# import the libraries
# --------------------
import pandas as pd
import numpy as np
import math
import pylab    #for graphs

# uses Ordinary Least Squares (OLS) method
# -------------------------------------------
import statsmodels.api as sm
from sklearn import cross_validation
from sklearn.cross_validation import train_test_split
import scipy.stats as stats

import seaborn as sns   #used for multicolinearity check

# VIF
# ---
from statsmodels.stats.outliers_influence import variance_inflation_factor

# Feature selection
# -----------------
from sklearn.feature_selection import f_regression as fs

# </import libraries>


# read the input file
# --------------------
path="D:/imarticus/Python class Project/salesdata.csv"
sales = pd.read_csv(path)

sales = pd.read_csv(path, na_values=' ')
sales.head(20)

# count of Rows and Columns
# -------------------------
sales.shape

# describe the dataset (R,C)
# --------------------------
sales.dtypes

# summarize the dataset
# clearer view. removed the 1st row as it contains same info (total records)
# ------------------------------------------------------------
desc = sales.describe()
desc = desc.drop(desc.index[0])  #to remove the count row from desc as not req.
desc

cols = list(sales.columns)
type(cols)
cols.remove("Item_Outlet_Sales")


for c in cols:
    if (len(sales[c][sales[c].isnull()])) > 0:
        print("WARNING: Column '{}' has NULL values".format(c))

    if (len(sales[c][sales[c] == 0])) > 0:
        print("WARNING: Column '{}' has value = 0".format(c))
    
    # since numerics have strings, convert num to str and check
    if ( len(sales[c][sales[c].astype(str).str.isnumeric()]) < len(sales) ):
        print("WARNING: Column '{}' has Non-numeric values".format(c))

   
sales.info()     
# check for outliers in dataset
# -------------------------------
sales.boxplot(column='Item_Weight')
sales.boxplot(column='Item_MRP')
sales.boxplot(column='Item_Visibility')
sales.boxplot(column='Item_Type')

# or to check all in one loop
for i in cols:
    sales.boxplot(column=i)

    
# convert NULLS variable 'Outlet_Size' with 'Unknown' And
    # variable 'Item_Weight' with mode value = 17.6
        
sales.Outlet_Size = sales[['Outlet_Size']].convert_objects(convert_numeric=True).fillna('Unkown')
sales.head(20)

mode = sales['Item_Weight'].mode()
mode

sales.Item_Weight = sales[['Item_Weight']].convert_objects(convert_numeric=True).fillna(sales['Item_Weight'].mode())
# OR
sales.Item_Weight = sales[['Item_Weight']].convert_objects(convert_numeric=True).fillna(17.6)
sales.head(20)


# to check for number of '0' in Item_Visibility col
rows=len(sales)
print(sales['Item_Visibility'].value_counts())

# finding correlation
sales.corr()
cor = sales.iloc[:,0:10].corr()

# correlation using visualization
# -------------------------------
# cor --> defined above as the correlation amongst the x-variables
sns.heatmap(cor, xticklabels=cor.columns, yticklabels=cor.columns)  
       
#Drop columns 
del sales['Item_Identifier']
del sales['Outlet_Identifier']
del sales['Outlet_Establishment_Year']

#sales_dt = pd.DataFrame(sales)
#columns=sales_dt.columns[sales_dt.any()]

# convert columns into numeric data type

sales.dtypes

print(sales['Item_Fat_Content'].unique())
#Reducing levels of column Item_Fat_Content
sales.Item_Fat_Content[sales.Item_Fat_Content == "low fat"] = "Low Fat"
sales.Item_Fat_Content[sales.Item_Fat_Content == "LF"] = "Low Fat"
sales.Item_Fat_Content[sales.Item_Fat_Content == "reg"] = "Regular"

#Replace "Low Fat" = 1 And "Regular" = 2
sales.Item_Fat_Content[sales.Item_Fat_Content == 'Low Fat'] = 1
sales.Item_Fat_Content[sales.Item_Fat_Content == 'Regular'] = 2

sales['Item_Fat_Content']=pd.to_numeric(sales.Item_Fat_Content)
sales.dtypes

len(sales['Item_Type'].unique())
sales.Item_Type[sales.Item_Type == 'Dairy'] = 1
sales.Item_Type[sales.Item_Type == 'Soft Drinks'] = 2
sales.Item_Type[sales.Item_Type == 'Meat'] = 3
sales.Item_Type[sales.Item_Type == 'Fruits and Vegetables'] = 4
sales.Item_Type[sales.Item_Type == 'Household'] = 5
sales.Item_Type[sales.Item_Type == 'Baking Goods'] = 6
sales.Item_Type[sales.Item_Type == 'Snack Foods'] = 7
sales.Item_Type[sales.Item_Type == 'Frozen Foods'] = 8
sales.Item_Type[sales.Item_Type == 'Breakfast'] = 9
sales.Item_Type[sales.Item_Type == 'Health and Hygiene'] = 10
sales.Item_Type[sales.Item_Type == 'Hard Drinks'] = 11
sales.Item_Type[sales.Item_Type == 'Canned'] = 12
sales.Item_Type[sales.Item_Type == 'Breads'] = 13
sales.Item_Type[sales.Item_Type == 'Starchy Foods'] = 14
sales.Item_Type[sales.Item_Type == 'Others'] = 15
sales.Item_Type[sales.Item_Type == 'Seafood'] = 16

sales['Item_Type']=pd.to_numeric(sales.Item_Type)
sales.dtypes

print(sales['Outlet_Size'].unique())
sales.Outlet_Size[sales.Outlet_Size == 'Medium'] = 1
sales.Outlet_Size[sales.Outlet_Size == 'Unkown'] = 2
sales.Outlet_Size[sales.Outlet_Size == 'High'] = 3
sales.Outlet_Size[sales.Outlet_Size == 'Small'] = 4

sales['Outlet_Size']=pd.to_numeric(sales.Outlet_Size)
sales.dtypes

print(sales['Outlet_Location_Type'].unique())
sales.Outlet_Location_Type[sales.Outlet_Location_Type == 'Tier 1'] = 1
sales.Outlet_Location_Type[sales.Outlet_Location_Type == 'Tier 2'] = 2
sales.Outlet_Location_Type[sales.Outlet_Location_Type == 'Tier 3'] = 3

sales['Outlet_Location_Type']=pd.to_numeric(sales.Outlet_Location_Type)
sales.dtypes

print(sales['Outlet_Type'].unique())
sales.Outlet_Type[sales.Outlet_Type == 'Supermarket Type1'] = 1
sales.Outlet_Type[sales.Outlet_Type == 'Supermarket Type2'] = 2
sales.Outlet_Type[sales.Outlet_Type == 'Supermarket Type3'] = 3
sales.Outlet_Type[sales.Outlet_Type == 'Grocery Store'] = 4

sales['Outlet_Type']=pd.to_numeric(sales.Outlet_Type)
sales.dtypes


#Divide the dataset in train and test
cols = list(sales.columns)
print(sales)
columns=sales.columns[sales.any()]

test_set = sales[sales["Item_Outlet_Sales"].isnull()][columns]
len(test_set)
test_set.shape

train_set = sales[sales["Item_Outlet_Sales"].notnull()][columns]
len(train_set)
train_set.shape

# Replace blanks in y var with '0'

test_set.Item_Outlet_Sales = test_set[['Item_Outlet_Sales']].convert_objects(convert_numeric=True).fillna(0)   
  
sales.info()

# split the train and test into X and Y variables
# ------------------------------------------------
train_x = train_set.iloc[:,0:8]; train_y = train_set.iloc[:,8]
test_x  = test_set.iloc[:,0:8];  test_y = test_set.iloc[:,8]

print(train_x.shape)
print(train_y.shape)
print(test_x.shape)
print(test_y.shape)


# MODEL BUILDING (LINEAR REGRESSION)

# function -> getresiduals()
# -------------------------------
def getresiduals(lm,train_x,train_y):
    predicted = lm.predict(train_x)
    actual = train_y
    residual = actual-predicted
    
    return(residual)

# To add the constant term A (Y = A + B1X1 + B2X2 + ... + BnXn)
# Xn = ccomp,slag,flyash.....
# ----------------------------------------------------------
train_x = sm.add_constant(train_x) #to get values of intercept and slope
test_x = sm.add_constant(test_x)
# model building
lm1 = sm.OLS(train_y, train_x).fit()

# interpret the result
# =====================
# 1) significant variables: having high |t| or low P values
# 2) coefficients = average(coeff(0.025,0.975))
lm1.summary()
# coefficients
lm1.params

### validating the assumptions
# ----------------------------
residuals = getresiduals(lm1,train_x,train_y)
print(residuals)

# 1) Residual mean is 0
# ----------------------------
print(residuals.mean())

# 2) Residuals have constant variance
# ------------------------------------
y = lm1.predict(train_x)
sns.set(style="whitegrid")
sns.residplot(residuals,y,lowess=True,color="g")

# 3) Residuals are normally distributed
# --------------------------------------
stats.probplot(residuals,dist="norm",plot=pylab)
pylab.show()

# 4) rows > columns
# ------------------
sales.shape

# VIF (Variance Inflation Factor)
# -------------------------------
vif = pd.DataFrame()
vif["VIF Factor"] = [variance_inflation_factor(train_x.values, i) for i in range(train_x.shape[1])]
vif["features"] = train_x.columns
print(vif)

# cross-validation
import math as m

sno = range(1,51)
df1 = pd.DataFrame({"data" : list(sno)})
folds=8
df1.head()

'''
df_train = df1.iloc[0:train].values.flatten()
type(df_train)
print(df_train)

df_train = df1.iloc[0:train]
type(df_train)
print(df_train.values.flatten())
'''

# test=m.floor(len(df1)/folds); print(test)
test=m.ceil(len(df1)/folds); print(test)
train=len(df1)-test; print(train)

# df_train=df1.copy(deep=True); df_train
# df_test=[]

for i in range(folds):
    df_train = df1[0:train]
    df_test = df1.tail(test)
    
    print("Iteration " + str(i) + "\n")
    print("Train ...")
    print(df_train.values.flatten())
    print("Length = {0}".format(len(df_train)))
    
    print("Test ...")
    print(df_test.values.flatten())
    print("Length = {0}".format(len(df_test)))
    
    print("\n")
    
    # add the datasets in the order "test + train"
    df1=pd.merge(df_test,df_train,how='outer')
    


##################################################
    
import random as r

# r.sample(range(1,100),2)

df_train = r.sample(list(df1.iloc[0:len(df1)].values.flatten()),train)
len(df_train)
df_test = set(list(df1.iloc[0:len(df1)].values.flatten())).difference(set(df_train))
len(df_test)

print(list(df_train))
print(list(df_test))  


# feature selection
# ------------------
X=train_x.iloc[:,1:8]
features = fs(X,train_y,center=True)
list(features[0])
# pd.DataFrame({'column':cols[1:9], 'coefficieint':coefficients})

df_features = pd.DataFrame({"columns":train_x.columns[1:8], 
                            "score":features[0],
                            "p-val":features[1]
                            })
print(df_features)

# predict
# -----------------
pdct1 = lm1.predict(test_x)
print(pdct1)

# mean square error
# -----------------
mse = np.mean((pdct1 - test_y)**2)
print("MSE = {0}, RMSE = {1}".format(mse,math.sqrt(mse)))

# store the actual and predicted values in a dataframe for comparison
# -------------------------------------------------------------------
actual = list(test_y.head(50))
predicted = np.round(np.array(list(pdct1.head(50))),2)
print(predicted)

diff=actual-predicted

df_results = pd.DataFrame({'actual':actual, 'predicted':predicted, 'difference':diff})
print(df_results)

print(df_features)

# MODEL 2 (significant variables)

df_sales = sales.loc[:,["Item_MRP","Outlet_Size","Item_Visibility","Outlet_Location_Type","Item_Outlet_Sales"]]
df_sales.shape

#del sales['Item_Fat_Content']
#del sales['Item_Type']
#del sales['Item_Weight']


columns2=df_sales.columns[df_sales.any()]

df_sales.dtypes

test_set2 = df_sales[df_sales["Item_Outlet_Sales"].isnull()][columns2]
len(test_set2)
test_set2.shape

train_set2 = df_sales[df_sales["Item_Outlet_Sales"].notnull()][columns2]
len(train_set2)
train_set2.shape


test_set2.Item_Outlet_Sales = test_set2[['Item_Outlet_Sales']].convert_objects(convert_numeric=True).fillna(0)   


# split the train and test into X and Y variables
# ------------------------------------------------
train_x2 = train_set2.iloc[:,0:4]; train_y2 = train_set2.iloc[:,4]
test_x2  = test_set2.iloc[:,0:4];  test_y2 = test_set2.iloc[:,4]

print(train_x2.shape)
print(train_y2.shape)
print(test_x2.shape)
print(test_y2.shape)

# MODEL BUILDING (LINEAR REGRESSION)

def getresiduals(lm,train_x2,train_y2):
    predicted2 = lm.predict(train_x2)
    actual2 = train_y2
    residual2 = actual2-predicted2
    
    return(residual2)

# To add the constant term A (Y = A + B1X1 + B2X2 + ... + BnXn)
train_x2 = sm.add_constant(train_x2) 
test_x2 = sm.add_constant(test_x2)

#df_sales=sales.dropna(inplace=True)

# model building
lm2 = sm.OLS(train_y2, train_x2).fit()

# interpret the result
# =====================
# 1) significant variables: having high |t| or low P values
# 2) coefficients = average(coeff(0.025,0.975))
lm2.summary()
# coefficients
lm2.params

# VIF (Variance Inflation Factor)
# -------------------------------
vif2 = pd.DataFrame()
vif2["VIF Factor"] = [variance_inflation_factor(train_x2.values, i) for i in range(train_x2.shape[1])]
vif2["features"] = train_x2.columns
print(vif2)

# cross-validation





sno2 = len(train_x2)
df2 = pd.DataFrame({"data" : list(sno2)})
folds=6
df2.head()

test2=m.ceil(len(df2)/folds); print(test2)
train2=len(df2)-test2; print(train2)


for i in range(folds):
    df_train2 = df2[0:train2]
    df_test2 = df2.tail(test2)
    
    print("Iteration " + str(i) + "\n")
    print("Train ...")
    print(df_train2.values.flatten())
    print("Length = {0}".format(len(df_train2)))
    
    print("Test ...")
    print(df_test2.values.flatten())
    print("Length = {0}".format(len(df_test2)))
    
    print("\n")
    
    # add the datasets in the order "test + train"
    df2=pd.merge(df_test2,df_train2,how='outer')
    


##################################################
    
import random as r

# r.sample(range(1,100),2)

df_train2 = r.sample(list(df2.iloc[0:len(df2)].values.flatten()),train2)
len(df_train2)
df_test2 = set(list(df2.iloc[0:len(df2)].values.flatten())).difference(set(df_train2))
len(df_test2)

print(list(df_train))
print(list(df_test))  


# feature selection
# ------------------
X2=train_x2.iloc[:,1:5]
features2 = fs(X2,train_y2,center=True)
list(features2[0])
# pd.DataFrame({'column':cols[1:9], 'coefficieint':coefficients})

df_features2 = pd.DataFrame({"columns":train_x2.columns[1:5], 
                            "score":features2[0],
                            "p-val":features2[1]
                            })
print(df_features2)

# predict
# -----------------
pdct2 = lm2.predict(test_x2)
print(pdct2)

# mean square error
# -----------------
mse2 = np.mean((pdct2 - test_y2)**2)
print("MSE = {0}, RMSE = {1}".format(mse2,math.sqrt(mse2)))

# store the actual and predicted values in a dataframe for comparison
# -------------------------------------------------------------------
actual2 = list(test_y2.head(50))
predicted2 = np.round(np.array(list(pdct2.head(50))),2)
print(predicted2)

diff2=actual2-predicted2

df_results2 = pd.DataFrame({'actual':actual2, 'predicted':predicted2, 'difference':diff2})
print(df_results2)

print(df_features2)







