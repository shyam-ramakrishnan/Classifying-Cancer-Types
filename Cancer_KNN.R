#Read the csv file and store in data frame
#setwd("~/Desktop/ALDA PROJECT")
prc <- read.csv("Prostate_Cancer.csv",stringsAsFactors = FALSE)
str(prc) #check if structured
prc <- prc[-1] #remove inconsequential column - 1st id column

table(prc$diagnosis_result)

prc$diagnosis_result <- factor(prc$diagnosis_result, levels = c("B", "M"), labels = c("Benign", "Malignant"))
round(prop.table(table(prc$diagnosis_result))*100, digits = 1)

#Normalize the data - all columns except diagnosys
normalize <- function(x){
  return( (x - min(x)) / ( max(x) - min(x)) )
}
prc_n <- as.data.frame(lapply(prc[2:9], normalize))
summary(prc_n$radius)

#Setting Training and Testing data
prc_train <- prc_n[1:65,]
prc_test <- prc_n[66:100,]

#Including diagnosys column which was left out in normalized data
prc_train_labels <- prc[1:65, 1]
prc_test_labels <- prc[66:100, 1]

#Install and use package 'class' for knn
install.packages("class")
library(class)

#Apply knn using k=10 and store in prc_test_pred
prc_test_pred <- knn(train = prc_train, test = prc_test, cl = prc_train_labels, k=10)

#Check if values in prc_test_pred matches with prc_test_labels
install.packages("gmodels")
library(gmodels)
CrossTable(x = prc_test_labels, y = prc_test_pred, prop.chisq = FALSE)

#Total Observations = 35
#True Positive - Malignant = 15
#True Negative - Benign = 6
#Accuracy = (TP+TN)/35 = 21/35 = 0.6