
#################Part-1 : install and load all the required packages#################
#Creat vector with all required packages
required_packages <- c("caret", "Metrics","dplyr","tidyr","ROSE", "caTools", "stringr","rpart","ipred","randomForest","gbm","e1071", "dummies", "Boruta")

#Create a vector which contains packag names, which are not yet installed in the machine
not_installed_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]

#Check wether all required packages installed, if not, install them
if(length(not_installed_packages) == 0){
  
  print("All required packages are installed")
  
} else {
  
  for(i in 1:length(not_installed_packages)){
    
    install.packages(not_installed_packages[i])
  }
}

#Load all the required packages for this project
for(i in 1:length(required_packages )){
  
  library(required_packages[i], character.only = T)
}

#################Part-2 : Load the data, clean, tidy and transform the data#################

setwd("E:\\1 Data Science\\Competitions\\Simplifai\\bank-additional")

#Read in dataset
bank_additional <- read.csv("bank-additional.csv", sep = ";")

#Exclude zero variance variables from the dataset
bank_additional <- bank_additional[,-c(nearZeroVar(bank_additional), 11)]

#Structure of the dataset
glimpse(bank_additional)

#Summary of dataset
summary(bank_additional)

#Create data partition of 80:20 train and test data

set.seed(123) #For reproducibility

bank_additional <- bank_additional[sample(1:nrow(bank_additional)),]

split <- round(nrow(bank_additional) * 0.8)

train <- bank_additional[1:split,]

test <- bank_additional[(split + 1) : nrow(bank_additional),]

