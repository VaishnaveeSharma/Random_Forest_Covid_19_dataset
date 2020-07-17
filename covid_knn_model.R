library(caret)
library(e1071)
#library(quanteda)
#library(RColorBrewer)
library(tidyverse)
library(tm)
#library(lattice)
#library(ggplot2)
library(dplyr)
library(NLP)
#library(plyr)
#library(rpart)
#library(rpart.plot)

data_original<-read.csv(file="C:/Users/sharm/Documents/RET/SPAM project/Team A-Jin/covid_19_data.csv",header=TRUE,sep = ",", stringsAsFactors = FALSE)

data_original$LABEL[data_original$LABEL=='FACTS']<-'FACT'
Fact_index<-data_original$LABEL[data_original$LABEL=='FACT']
MYTH_index <-data_original$LABEL[data_original$LABEL=='MYTH']
data <- data_original

#for sampling data twice
for (i in 1:2)
{
  rows <- sample(nrow(data))
  data <- data[rows, ]
}

table(data$LABEL)

docs <- Corpus(VectorSource(as.vector(data$MESSAGE)))
docs<-tm_map(docs,content_transformer(tolower)) # converting word to lowercase
docs<-tm_map(docs,removeNumbers) # remove number from the documents
docs<-tm_map(docs,removeWords,stopwords("english"))# remove stopwords from the documents
docs<-tm_map(docs,removePunctuation)# remove punctuation from the documents
docs<-tm_map(docs,stripWhitespace)# remove whitespace from the documents
docs <- tm_map(docs, stemDocument, language = "english") #stemming the document in english
tdm<-TermDocumentMatrix(docs)
sparse.mt<-as.matrix(tdm)
sparse.mt2<-t(sparse.mt)
sparse.df<-as.data.frame(sparse.mt2)
sparse.df<-cbind(sparse.df,data$LABEL)
names(sparse.df)[names(sparse.df)=="data$LABEL"] <- "Target"

set.seed(120) 
train_index<-createDataPartition(sparse.df$Target, p = .70,list = F)
trainset<-sparse.df[train_index,]
testset<-sparse.df[-train_index,]
table(trainset$Target)
table(testset$Target)


grid <- expand.grid(k = c(1:20))
trctrl <- trainControl(method = "repeatedcv", number = 30, repeats = 5) 
knn_fit<-train(Target ~.,data=trainset,method = "knn", tuneGrid =grid, trControl=trctrl, preProcess = c("center"))
plot(knn_fit)
test_pred <- predict(knn_fit, newdata = testset)
plot(test_pred)
testset$Target <- as.factor(testset$Target)
Knn_confmat <- confusionMatrix(test_pred, testset$Target)
Knn_confmat
Knn_confmat$byClass["Precision"]
Knn_confmat$byClass["Recall"]
Knn_confmat$byClass["F1"]

