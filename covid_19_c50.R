getwd()
setwd("F://Text Mining Project 2020")

library(tm)
library(NLP)
library(e1071)
library(C50)
library(caret)
library(dplyr)
library(caTools)

df<-read.csv(file="covid_19_data_new.csv",header=TRUE,sep = ",", stringsAsFactors = FALSE)
data = df

data$LABEL[data$LABEL=='FACTS']<-'FACT'
View(data)

data$LABEL<-factor(data$LABEL)

rows<-sample(nrow(data))
data<-data[rows,]

corpus<-iconv(data$MESSAGE)
corpus<-VectorSource(corpus)
corpus<-Corpus(corpus)
corpus<-tm_map(corpus,tolower)
corpus<-tm_map(corpus,removePunctuation)
corpus<-tm_map(corpus,removeNumbers)
inspect(corpus)

cleandata<-tm_map(corpus,removeWords, stopwords('english'))
cleandata<-tm_map(cleandata,stripWhitespace)
inspect(cleandata)

tdm<-TermDocumentMatrix(cleandata)
tdm 

removeSparseTerms(tdm, 0.995) 
sparse<-removeSparseTerms(tdm, 0.995)
sparse.mt<-as.matrix(sparse)
sparse.mt2<-t(sparse.mt)
sparse.df<-as.data.frame(sparse.mt2)
sparse.df$LABEL<-data$LABEL

set.seed(975)

split<-sample.split(sparse.df$LABEL, SplitRatio = 0.70)
trainc50<-subset(sparse.df,  split==TRUE)
testc50<-subset(sparse.df, split==FALSE)

table(testc50$LABEL)
table(trainc50$LABEL)

c5tree.model <- C5.0(as.formula(LABEL~.), data=trainc50, rules=T)
print(c5tree.model)
summary(c5tree.model)

#run the model on the data, print a confusion matrix, and show the accuracy
c5tree.prediction <- predict(c5tree.model, newdata=testc50)
c5confusion.matrix <- table(testc50$LABEL, c5tree.prediction)
print(c5confusion.matrix)
accuracy.percent <- 100*sum(diag(c5confusion.matrix))/sum(c5confusion.matrix)
print(paste("accuracy:",accuracy.percent,"%"))

#plot the tree (have to rerun the model with rules=F)
c5tree.model <- C5.0(as.formula(LABEL~.), data=trainc50, rules=F)
plot(c5tree.model)
