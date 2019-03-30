#R_PROJECT (GROUP - ROHINI & SUKRITI)

# random forest algorithm - CLASSIFICATION

install.packages("randomForest")

library(ggplot2)
library(randomForest)
library(caret) # for confusion matrix

# Diabetes dataset to predict the patient is diabetic or not
path = "D:/imarticus/R class PROJECT/dataset_diabetes/diabetic_data.csv"
# diab = read.csv(d1, header=T, stringsAsFactors = T)
diab = read.csv(path, header=T)


head(diab,5)
View(diab)
nrow(diab)
ncol(diab)

# check datatypes
str(diab)

# check for Nulls, Zeroes for all columns
col_name = colnames(diab) [apply(diab, 2, function(n) any(is.na(n)))]
#any is used for any value of nullin any position
if(length(col_name) > 0) print("NULLs present") else print("No NULLs")

col_name = colnames(diab) [apply(diab, 2, function(n) any(n == ""))]
if(length(col_name) > 0) print("Blanks present") else print("No Blanks")

col_name = colnames(diab) [apply(diab, 2, function(n) any(n==0))]
print(col_name)
if(length(col_name) > 0)
{
  print("Zeroes present in columns : ")
  print(col_name)
} else 
  print("No Zeroes")

# check for columns having '?'

col_name = colnames(diab) [apply(diab, 2, function(n) any(n=='?'))]
print(col_name)

100*prop.table(table(diab$weight=='?'))

100*prop.table(table(diab$race=='?'))

# EDA 
#1) Race column

library(ggplot2)
ggplot(diab,aes(x=diab$race,fill=diab$gender))+geom_bar()
levels(diab$race)
count(diab,'race')

#Replace race with different levels

c =0; i=1
while(i <= 101766)
{
  if(diab$race[i] == '?'){
    c = c + 1
    if(c <= 50010){
      diab$race[i] = 'AfricanAmerican'
    }else if(c > 50010 & c <= 101766){
      diab$race[i] = 'Caucasian'
    }
  }
  i = i+1
}

print(c)
View(diab)
table(diab$race)

nrow(diab[diab$race=="?",])

# 2) payer_code column
table(diab$payer_code)

#Check count of '?'

c =0; i=1
while(i < 101766)
{
  if(diab$payer_code[i] == '?'){
    c = c + 1
    if(c <= 6710){
      diab$payer_code[i] = 'BC'
    }else if(c > 6710 & c <= 18418){
      diab$payer_code[i] = 'HM'
    }else if(c > 18418 & c <= 40418){
      diab$payer_code[i] = 'CP'
    }else if(c > 40418 & c <= 75000){
      diab$payer_code[i] = 'SP'
    }else if(c > 75000 & c <= 101766){
      diab$payer_code[i] = 'MC'
    }
  }
  i = i+1
}
table(diab$payer_code)

p = 1
while(p<= 101766)
{
  if(diab$payer_code[p] == '?'){
    diab$payer_code[p] = 'MD'
  }
  p = p+1
}

table(diab$payer_code)
View(diab)
print(c)

# column  medical_specialty

levels(diab$medical_specialty)
100*prop.table(table(diab$medical_specialty))


c =0; i=1
while(i < 101766)
{
  if(diab$medical_specialty[i] == '?'){
    c = c + 1
    if(c <= 6710){
      diab$medical_specialty[i] = 'Cardiology'
    }else if(c > 6710 & c <= 18418){
      diab$medical_specialty[i] = 'Family/GeneralPractice'
    }else if(c > 18418 & c <= 40418){
      diab$medical_specialty[i] = 'Emergency/Trauma'
    }else if(c > 40418 & c <= 75000){
      diab$medical_specialty[i] = 'InternalMedicine'
    }else if(c > 75000 & c <= 101766){
      diab$medical_specialty[i] = 'Surgery-General'
    }
  }
  i = i+1
}

table(diab$medical_specialty)
View(diab)
rows = nrow(diab)
rows
p = 1
while(p<= 101766)
  
{
  if(diab$medical_specialty[p] == '?'){
    diab$medical_specialty[p] = 'Nephrology'
  }
  p = p+1
}

# 3) drop columns
diab$weight= NULL
diab$encounter_id = NULL
diab$patient_nbr = NULL
diab$admission_type_id = NULL
diab$admission_source_id = NULL
diab$discharge_disposition_id = NULL
colnames(diab)

col_name = colnames(diab) [apply(diab, 2, function(n) any(n=='?'))]
print(col_name)

# 4) columns diag_1, diag_2, diag_3

#Replace diag_1 with different levels

diab$diag_1 = as.character(diab$diag_1)

#for decimals
i=1
for(i in 1:length(diab$diag_1))
{
  if(nchar(diab$diag_1[i]) >= 5){
    sub=diab$diag_1[i]
    sub1 = substr(sub,1,3)
    diab$diag_1[i] = sub1
  }
}  

table(diab$diag_1)

#for "V"
i=1
for(i in 1:length(diab$diag_1))
{
  sub=diab$diag_1[i]
  rep = "V"
  #str = gsub("V","",sub)
  if(grepl(rep,sub)){
    diab$diag_1[i] = gsub(rep,"",sub)
  }
}  

#for "E"
i=1
for(i in 1:length(diab$diag_1))
{
  sub=diab$diag_1[i]
  rep = "E"
  #str = gsub("E","",sub)
  if(grepl(rep,sub)){
    diab$diag_1[i] = gsub(rep,"",sub)
  }
} 

str(diab)

#Replace "?" in  diag_1 with different levels
diab$diag_1 = as.factor(diab$diag_1)

i=1
while(i <= 101766)
{
  if(diab$diag_1[i] == '?'){
    diab$diag_1[i] = 272
  }
  i = i+1
}

table(diab$diag_1)


#Replace diag_2 with different levels

100*prop.table(table(diab$diag_2=='?'))
diab$diag_2 = as.character(diab$diag_2)

#for decimals
i=1
for(i in 1:length(diab$diag_2))
{
  if(nchar(diab$diag_2[i]) >= 5){
    sub=diab$diag_2[i]
    sub1= substr(sub,1,3)
    diab$diag_2[i] = sub1
  }
}  

table(diab$diag_2)

#for "V"
i=1
for(i in 1:length(diab$diag_2))
{
  sub=diab$diag_2[i]
  rep = "V"
  #str = gsub("V","",sub)
  if(grepl(rep,sub)){
    diab$diag_2[i] = gsub(rep,"",sub)
  }
}  

#for "E"
i=1
for(i in 1:length(diab$diag_2))
{
  sub=diab$diag_2[i]
  rep = "E"
  #str = gsub("E","",sub)
  if(grepl(rep,sub)){
    diab$diag_2[i] = gsub(rep,"",sub)
  }
} 

str(diab)

#Replace "?" in  diag_1 with different levels
diab$diag_2 = as.factor(diab$diag_2)

i=1
while(i <= 101766)
{
  if(diab$diag_2[i] == '?'){
    diab$diag_2[i] = 250
  }
  i = i+1
}

table(diab$diag_2)


#Replace diag_3 with different levels

100*prop.table(table(diab$diag_3=='?'))
diab$diag_3 = as.character(diab$diag_3)

#for decimals
i=1
for(i in 1:length(diab$diag_3))
{
  if(nchar(diab$diag_3[i]) >= 5){
    sub=diab$diag_3[i]
    sub1= substr(sub,1,3)
    diab$diag_3[i] = sub1
  }
}  

table(diab$diag_3)

#for "V"
i=1
for(i in 1:length(diab$diag_3))
{
  sub=diab$diag_3[i]
  rep = "V"
  #str = gsub("V","",sub)
  if(grepl(rep,sub)){
    diab$diag_3[i] = gsub(rep,"",sub)
  }
}  

#for "E"
i=1
for(i in 1:length(diab$diag_3))
{
  sub=diab$diag_3[i]
  rep = "E"
  #str = gsub("E","",sub)
  if(grepl(rep,sub)){
    diab$diag_3[i] = gsub(rep,"",sub)
  }
} 

str(diab)

#Replace "?" in  diag_1 with different levels
diab$diag_3 = as.factor(diab$diag_3)

i=1
while(i <= 101766)
{
  if(diab$diag_3[i] == '?'){
    diab$diag_3[i] = 401
  }
  i = i+1
}

table(diab$diag_3)

col_name = colnames(diab) [apply(diab, 2, function(n) any(n=='?'))]
print(col_name)

#Drop weight column--> more than 50% '?'
diab$weight = NULL
diab$encounter_id = NULL
diab$patient_nbr = NULL
diab$admission_type_id = NULL
diab$admission_source_id = NULL
diab$discharge_disposition_id = NULL

#convert to numeric -- > more than 50 levels
str(diab)
diab$medical_specialty = as.numeric(diab$medical_specialty)
diab$diag_1 = as.numeric(diab$diag_1)
diab$diag_2 = as.numeric(diab$diag_2)
diab$diag_3 = as.numeric(diab$diag_3)

###############################################################
# RANDOM FOREST MODEL
#--------------------------------------------------------------

# randomly shuffle the dataset
grp = runif(nrow(diab))
diab = diab[order(grp),]

# Model - 1 -----> Accuracy (58.08%)

# split the dataset into Training and Testing
# sample = sample(2, nrow(diab), replace = T, prob = c(0.7,0.3))

ind = sample(seq_len(nrow(diab)), floor(nrow(diab)*0.7))
train = diab[ind,]
test = diab[-ind,]

nrow(train)
nrow(test)
ncol(diab)
str(diab)  

# to check the count of each value of a factor variable against the Y-variable
# ----------------------------------------------------------------------------
100*prop.table(table(train$readmitted))

100*prop.table(table(test$readmitted))

100*prop.table(table(diab$readmitted)) #actual % of <30, >30 and NO

colnames(train);colnames(test)

#  call the randomforest() for Classification
# ---------------------------------------------------------------------
train_x = train[,1:43]
train_y = train[,44]
rf1 = randomForest(train_x, factor(train_y)) #Build model 
rf1

importance(rf1)
summary(rf1)
# ntree=500 is the default. More the better.

# predict the Classification for the Testing data
# ------------------------------------------------
pdct_rf1 = predict(rf1, test)
pdct_rf1

table(predicted = pdct_rf1 , actual = test$readmitted)
confusionMatrix(pdct_rf1 ,test$readmitted)

# variable importance - for feature selection
# ----------------------------------------------
varImpPlot(rf1)


library("rpart")
library("rpart.plot")

#MOdel-2 -------> Accuracy(54.98%)

diab_M2 = data.frame(diab$race,diab$gender,diab$number_outpatient,diab$number_emergency,
                     diab$A1Cresult,diab$metformin,diab$nateglinide,diab$glimepiride.pioglitazone,
                     diab$glyburide.metformin,diab$readmitted)

View(diab_M2)
# split the dataset into Training and Testing
# sample = sample(2, nrow(diab), replace = T, prob = c(0.7,0.3))

ind2 = sample(seq_len(nrow(diab_M2)), floor(nrow(diab_M2)*0.7))
train2 = diab_M2[ind2,]
test2 = diab_M2[-ind2,]

nrow(train2)
nrow(test2)
ncol(diab_M2)
str(diab_M2) 

length(test2$diab.readmitted)
100*prop.table(table(test2$diab.readmitted))

#  call the randomforest() for Classification
# ---------------------------------------------------------------------
train_x2 = train2[,1:9]
train_y2 = train2[,10]
rf2 = randomForest(train_x2, factor(train_y2)) #Build model 
rf2

importance(rf2)
summary(rf2)

# predict the Classification for the Testing data
# ------------------------------------------------
pdct_rf2 = predict(rf2, test2)
pdct_rf2

table(predicted = pdct_rf2 , actual = test2$diab.readmitted)
confusionMatrix(pdct_rf2 ,test2$diab.readmitted)

#MOdel-3 ------> Accuracy(55.13%)

diab_M3 = data.frame(diab$race,diab$gender,diab$number_outpatient,diab$number_emergency,
                     diab$A1Cresult,diab$metformin,diab$glipizide,diab$glyburide,
                     diab$tolazamide,diab$readmitted)

View(diab_M3)
# split the dataset into Training and Testing
# sample = sample(2, nrow(diab), replace = T, prob = c(0.7,0.3))

ind3 = sample(seq_len(nrow(diab_M3)), floor(nrow(diab_M3)*0.7))
train3 = diab_M3[ind3,]
test3 = diab_M3[-ind3,]

nrow(train3)
nrow(test3)
ncol(diab_M3)
str(diab_M3) 

length(test3$diab.readmitted)
100*prop.table(table(test3$diab.readmitted))

#  call the randomforest() for Classification
# ---------------------------------------------------------------------
train_x3 = train3[,1:9]
train_y3 = train3[,10]
rf3 = randomForest(train_x3, factor(train_y3)) #Build model 
rf3

importance(rf3)
summary(rf3)

# predict the Classification for the Testing data
# ------------------------------------------------
pdct_rf3 = predict(rf3, test3)
pdct_rf3

table(predicted = pdct_rf3 , actual = test3$diab.readmitted)
confusionMatrix(pdct_rf3 ,test3$diab.readmitted)

#Model-4 -----> Accuracy(55.51%)

diab_M4 = data.frame(diab$race,diab$gender,diab$number_outpatient,diab$number_emergency,
                     diab$A1Cresult,diab$metformin,diab$glipizide,diab$glyburide,
                     diab$readmitted)

# split the dataset into Training and Testing
# sample = sample(2, nrow(diab), replace = T, prob = c(0.7,0.3))

ind4 = sample(seq_len(nrow(diab_M4)), floor(nrow(diab_M4)*0.7))
train4 = diab_M4[ind4,]
test4 = diab_M4[-ind4,]

nrow(train4)
nrow(test4)
ncol(diab_M4)
str(diab_M4) 

length(test4$diab.readmitted)
100*prop.table(table(test4$diab.readmitted))

#  call the randomforest() for Classification
# ---------------------------------------------------------------------
train_x4 = train4[,1:8]
train_y4 = train4[,9]
rf4 = randomForest(train_x4, factor(train_y4)) #Build model 
rf4

importance(rf4)
summary(rf4)

# predict the Classification for the Testing data
# ------------------------------------------------
pdct_rf4 = predict(rf4, test4)
pdct_rf4

table(predicted = pdct_rf4, actual = test4$diab.readmitted)
confusionMatrix(pdct_rf4 ,test4$diab.readmitted)



#MOdel-5 ---- > Accurcay(55.27%)

diab_M6 = data.frame(diab$race,diab$gender,diab$number_outpatient,diab$number_emergency,
                     diab$A1Cresult,diab$metformin,diab$nateglinide,diab$glimepiride.pioglitazone,
                     diab$glyburide.metformin,diab$readmitted)

View(diab_M6)
# split the dataset into Training and Testing
# sample = sample(2, nrow(diab), replace = T, prob = c(0.7,0.3))

ind6 = sample(seq_len(nrow(diab_M6)), floor(nrow(diab_M6)*0.7))
train6 = diab_M6[ind6,]
test6 = diab_M6[-ind6]

nrow(train2)
nrow(test2)
ncol(diab_M2)
str(diab_M2) 

length(test2$diab.readmitted)
100*prop.table(table(test2$diab.readmitted))

#  call the randomforest() for Classification
# ---------------------------------------------------------------------
train_x6 = train6[,1:9]
train_y6 = train6[,10]
rf6 = randomForest(train_x6, factor(train_y6)) #Build model 
rf6

importance(rf6)
summary(rf6)

# predict the Classification for the Testing data
# ------------------------------------------------
pdct_rf6 = predict(rf6, test6)
pdct_rf6

table(predicted = pdct_rf6 , actual = test6$diab.readmitted)
confusionMatrix(pdct_rf6 ,test6$diab.readmitted)


####################################################################
#END
#-------------------------------------------------------------------
















