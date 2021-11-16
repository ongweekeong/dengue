library(ggplot2)
library(dplyr)
library(class)
library(caret)

setwd("C:/Users/junqi/OneDrive - National University of Singapore/Desktop/4308 PROJECT DATASETS") #set working directory

#deal with training set first
df <- read.csv("./dengue_features_train_cleaned.csv")

str(df) #check structure

#preprocessing
#convert city to binary. sj = 1, iq = 0
df$city <- ifelse(df$city == "sj", 1, 0)

t(colnames(df)) #get column index
#create function for conversion of K to C
tempconvert <- function(x){
  return(x - 273.15)
}
df_convert <- as.data.frame(lapply(df[,c(10:14)], tempconvert))
df[,colnames(df) %in% colnames(df_convert)] <- df_convert #replace new col into df
head(df) #check

#rename col 10-14 for consistency
names(df)[10] <- "reanalysis_air_temp_c"
names(df)[11] <- "reanalysis_avg_temp_c"
names(df)[12] <- "reanalysis_dew_point_temp_c"
names(df)[13] <- "reanalysis_max_air_temp_c"
names(df)[14] <- "reanalysis_min_air_temp_c"

#create function to normalize
normalize <- function(y){
  return((y - min(y)) / (max(y) - min(y)))}
df_normalize <- as.data.frame(lapply(df[,c(5:24)], normalize))
df[,colnames(df) %in% colnames(df_normalize)] <- df_normalize #replace normalized col into df
summary(df) #check if normalized

#split dataset
lastsj <- sum(df$city == 1) #last row of sj is 936, first row of iq is 937
firstiq <- lastsj + 1
sj80 <- round(0.8*sum(df$city == 1)) #last row of 80% sj is 749
sj81 <- sj80 + 1
iq80 <- sum(df$city == 1) + 0.8*sum(df$city == 0) #last row of 80 % iq 
iq81 <- iq80 + 1

df_train <- rbind(df[c(1:sj80),],df[c(firstiq: iq80),])
df_test <- rbind(df[c(sj81:lastsj),],df[c(iq81:1456),])

##df_train <- rbind(df[c(1:749),],df[c(937: 1352),])
##df_test <- rbind(df[c(750:936),],df(c(1353:1456),])

#training and testing data with 3 features
df3_train <- df_train[,c(1,14,19,23)]
df3_test <- df_test[,c(1,14,19,23)]

#read label 
dengue_labels <- read.csv("./dengue_labels_train.csv")
dengue_labels$total_cases <- as.numeric(dengue_labels$total_cases)
dengue_labels[is.na(dengue_labels)] <- 0

#separate into sg and iq
label_sj <- dengue_labels[1:936,]
label_iq <- dengue_labels[937:1456,]

#classify risk levels sj
label_sj$total_cases <- cut(label_sj$total_cases, breaks = c(-1, 9, 37, Inf), labels = c("Low Risk", "Medium Risk", "High risk"))
#classify risk levels iq
label_iq$total_cases <- cut(label_iq$total_cases, breaks = c(-1, 1, 9, Inf), labels = c("Low Risk", "Medium Risk", "High risk"))
#merge sj and iq
dengue_labels_train <- full_join(label_sj, label_iq)
#split train-test
df_train_filter <- full_join(dengue_labels_train[c(1:sj80),],dengue_labels_train[c(firstiq: iq80),])
df_test_filter <- full_join(dengue_labels_train[c(sj81:lastsj),],dengue_labels_train[c(iq81:1456),])

df_train_target <- df_train_filter[,1] 
df_test_target <- df_test_filter[,1]

#build model
set.seed(42)
i=1
k.optm = 1
for (i in 1:40){
  knn.mod <- knn(train=df3_train, test=df3_test, cl=df_train_target, k=i)
  k.optm[i] <- 100*sum(df_test_target == knn.mod)/NROW(df_test_target)
  k=i
  cat(k,"=",k.optm[i],"\n")
}
plot(k.optm, type="b", xlab="K-Value", ylab="Accuracy level")