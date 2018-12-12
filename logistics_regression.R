# logistics regression 
# information value
# http://r-statistics.co/Logistic-Regression-With-R.html
library(tidyverse)

# load data
inputData <- read.csv("http://rstatistics.net/wp-content/uploads/2015/09/adult.csv")

# check class distribution
# there is obvious class bias because 1's = 24.1%, 0's = 75.9%
table(inputData$ABOVE50K)

# preparation
# create training and test data samples
# Create Training Data
# because class bias, training set will be sampled by class separatly
input_ones <- inputData %>% filter(ABOVE50K == 1) # all 1's
input_zeros <- inputData %>% filter(ABOVE50K == 0)  # all 0's

set.seed(100)  # for repeatability of samples
# One way to address the problem of class bias is to draw the 0’s and 1’s for the trainingData (test data) in equal proportions
input_ones_training_rows <- sample(1:nrow(input_ones), 0.7*nrow(input_ones))  # 1's for training
input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7*nrow(input_ones))  # 0's for training. Pick as many 0's as 1's
training_ones <- input_ones[input_ones_training_rows, ]  
training_zeros <- input_zeros[input_zeros_training_rows, ]
trainingData <- rbind(training_ones, training_zeros)  # row bind the 1's and 0's 

# Create Test Data
test_ones <- input_ones[-input_ones_training_rows, ]
test_zeros <- input_zeros[-input_zeros_training_rows, ]
testData <- rbind(test_ones, test_zeros)  # row bind the 1's and 0's 

# Compute Information Values
library(smbinning)
# segregate continuous and factor variables
factor_vars <- c ("WORKCLASS", "EDUCATION", "MARITALSTATUS", "OCCUPATION", "RELATIONSHIP", "RACE", "SEX", "NATIVECOUNTRY")
continuous_vars <- c("AGE", "FNLWGT","EDUCATIONNUM", "HOURSPERWEEK", "CAPITALGAIN", "CAPITALLOSS")

# initialize for IV results
iv_df <- data.frame(VARS=c(factor_vars, continuous_vars), IV=numeric(14))  

# compute IV for categoricals
for(factor_var in factor_vars){
    smb <- smbinning.factor(trainingData, y="ABOVE50K", x=factor_var)  # WOE table
    if(class(smb) != "character"){ # heck if some error occured
        iv_df[iv_df$VARS == factor_var, "IV"] <- smb$iv
    }
}

# compute IV for continuous vars
for(continuous_var in continuous_vars){
    smb <- smbinning(trainingData, y="ABOVE50K", x=continuous_var)  # WOE table
    if(class(smb) != "character"){  # any error while calculating scores.
        iv_df[iv_df$VARS == continuous_var, "IV"] <- smb$iv
    }
}

iv_df <- iv_df[order(-iv_df$IV), ]  # sort

# alternative solution to calculate IV by using package Information
# library(Information)
# IV <- create_infotables(data=trainingData, y="ABOVE50K", bins=10, parallel=FALSE)

# build logit modeles and predict
logitMod <- glm(ABOVE50K ~ RELATIONSHIP + MARITALSTATUS + AGE + CAPITALGAIN + OCCUPATION + EDUCATIONNUM, data=trainingData, family=binomial(link="logit"))
# predicted scores # predicted <- predict(logitMod, testData, type="response")  # predicted scores
predicted <- plogis(predict(logitMod, testData))  

library(InformationValue)
optCutOff <- optimalCutoff(testData$ABOVE50K, predicted)[1] 

# model Diagnostics
library(car)
vif(logitMod) # vif function is built in car package
misClassError(testData$ABOVE50K, predicted, threshold = optCutOff)
plotROC(testData$ABOVE50K, predicted)
sensitivity(testData$ABOVE50K, predicted, threshold = optCutOff)
specificity(testData$ABOVE50K, predicted, threshold = optCutOff)
