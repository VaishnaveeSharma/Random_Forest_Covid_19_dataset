#running random forest on covid 19 dataset having myths and facts

library(tm)
setwd("C:/Users/Lee/Documents/R")
data <- read.csv("covid_19_data.csv", sep =",")

data <- data[order(data$LABEL),]
row.names(data) <- 1:nrow(data)

#task 1
facts_index<-data$LABEL=="FACTS"
# Create positive and negative review set
fact_set<-data[facts_index,]
myth_set<-data[!facts_index,]


fact_set <- fact_set$MESSAGE
myth_set <- myth_set$MESSAGE
library(dplyr)
fact_set_tibble <- tibble(line = 1:length(fact_set), text = fact_set)
myth_set_tibble <- tibble(line = 1:length(myth_set), text = myth_set)




library(tidytext)

#stemming
library(SnowballC)


#lemmitisation
library(textstem)


#removing numbers
library(stringr)


#everything in one go
pos_set_processed <- fact_set_tibble %>%
  unnest_tokens(word, text) %>%
  filter(!str_detect(word, "[^[:alpha:]]")) %>%
  anti_join(stop_words, by = "word") %>%
  distinct() %>%
  mutate(word_stem = SnowballC::wordStem(word)) %>%
  mutate(word_lemma = textstem::lemmatize_words(word)) 

#everything in one go
neg_set_processed <- myth_set_tibble %>%
  unnest_tokens(word, text) %>%
  filter(!str_detect(word, "[^[:alpha:]]")) %>%
  anti_join(stop_words, by = "word") %>%
  distinct() %>%
  mutate(word_stem = SnowballC::wordStem(word)) %>%
  mutate(word_lemma = textstem::lemmatize_words(word)) 


pos_set_processed <- as.data.frame(cbind(pos_set_processed$line, pos_set_processed$word_lemma))
names(pos_set_processed) <- c("line","word")

neg_set_processed <- as.data.frame(cbind(neg_set_processed$line, neg_set_processed$word_lemma))
names(neg_set_processed) <- c("line","word")

#task 3

#reading the positive dictionary
pdict <- read.csv("positive-dictionary.txt",header=T, sep='\t', stringsAsFactors = FALSE)
pdict <- as.data.frame(pdict[29:2034,])
names(pdict)<-  c("word")

#reading the negative dictionary
ndict <- read.csv("negative-dictionary.txt",header=T, sep='\t', stringsAsFactors = FALSE)
ndict <- as.data.frame(ndict[30:4812,])
names(ndict)<-  c("word")

# Counting how many positive/negative words in Fact dataset
pos_pdict <- count(inner_join(pos_set_processed,pdict),word,sort=TRUE)
pos_ndict <- count(inner_join(pos_set_processed,ndict),word,sort=TRUE)

sum(pos_pdict$n)
sum(pos_ndict$n)


# Counting how many positive/negative words in myth dataset
neg_pdict <- count(inner_join(neg_set_processed,pdict),word,sort=TRUE)
neg_ndict <- count(inner_join(neg_set_processed,ndict),word,sort=TRUE)

sum(neg_pdict$n)
sum(neg_ndict$n)



library(ggplot2)
pos_words_facts <- pos_pdict %>%
  top_n(10) %>%
  ggplot(aes(reorder(word, n), n)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  labs(y = "Positive words contribution to Facts", x = NULL, title = "Positive Word Count") +
  coord_flip()
plot(pos_words_facts)

neg_words_facts <- pos_ndict %>%
  top_n(10) %>%
  ggplot(aes(reorder(word, n), n)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  labs(y = "Negative words contribution to Facts", x = NULL, title = "Negative Word Count") +
  coord_flip()
plot(neg_words_facts)

#word cloud
#library(wordcloud)
#library(RColorBrewer)
#wordcloud(pos_pdict$word, freq = pos_pdict$n, scale = c(3, 0.5), max.words = 20)
#wordcloud(pos_ndict$word, freq = pos_ndict$n, scale = c(3, 0.5), max.words = 20)

library(wordcloud)
library(RColorBrewer)

set.seed(1234)
pal <- brewer.pal(8,"Dark2")
wordcloud(words = pos_pdict$word, freq = pos_pdict$n, min.freq = 1,
          max.words=20, random.order=FALSE, rot.per=0.35, 
          colors=pal, scale = c(3,0.5))


# Task 4

####### nrc ####################################################
nrc_sents <- get_sentiments("nrc")

pos_nrc <- count(inner_join(pos_set_processed,nrc_sents),word,sentiment,sort=TRUE)
neg_nrc <- count(inner_join(neg_set_processed,nrc_sents),word,sentiment,sort=TRUE)

pos_nrc_sum <- aggregate(pos_nrc$n, by=list(sentiment=pos_nrc$sentiment), FUN=sum)
neg_nrc_sum <- aggregate(neg_nrc$n, by=list(sentiment=neg_nrc$sentiment), FUN=sum)
barplot(pos_nrc_sum$x, col = c("red","black", "darkblue", "green", "orange","yellow"), main = "Sentiment Proportion in Facts Set",
         names.arg = pos_nrc_sum$sentiment, ylab = "Number", ylim = c(0,200))
barplot(neg_nrc_sum$x, col = c("red","black", "darkblue", "green", "orange","yellow"), main = "Sentiment Proportion in Myths Set",
        names.arg = neg_nrc_sum$sentiment, ylab = "Number", ylim = c(0,200))
################################################################

####### loughran ###############################################

lough_sents <- get_sentiments("loughran")
table(lough_sents$sentiment)

pos_lough <- count(inner_join(pos_set_processed,lough_sents),word,sentiment,sort=TRUE)
neg_lough <- count(inner_join(neg_set_processed,lough_sents),word,sentiment,sort=TRUE)

pos_lough_sum <- aggregate(pos_lough$n, by=list(sentiment=pos_lough$sentiment), FUN=sum)
neg_lough_sum <- aggregate(neg_lough$n, by=list(sentiment=neg_lough$sentiment), FUN=sum)

barplot(pos_lough_sum$x, col = c("red","black", "darkblue", "green", "orange","yellow"), main = "Sentiment Proportion in Facts Set",
        names.arg = pos_lough_sum$sentiment, ylab = "Number", ylim = c(0,40))
barplot(neg_lough_sum$x, col = c("red","black", "darkblue", "green", "orange","yellow"), main = "Sentiment Proportion in Myths Set",
        names.arg = neg_lough_sum$sentiment, ylab = "Number", ylim = c(0,40))
################################################################




# Random Forest - trying to predict if a statement is fact or myth

library(randomForest)

# Setting up data
rf_fact_pos <- cbind(inner_join(pos_set_processed,pdict),rep(as.numeric(1)))#rep("Positive"))
names(rf_fact_pos) <- c("line","word","sentiment")
rf_fact_neg <- cbind(inner_join(pos_set_processed,ndict),rep(as.numeric(-1)))#,rep("Negative"))
names(rf_fact_neg) <- c("line","word","sentiment")
rf_fact <- rbind(rf_fact_pos,rf_fact_neg)
rf_fact_neu <- cbind(subset(pos_set_processed, !(word %in% rf_fact$word)),rep(as.numeric(0)))#rep("Neutral"))
names(rf_fact_neu) <- c("line","word","sentiment")
rf_fact <- rbind(rf_fact,rf_fact_neu)

rf_fact$line <- as.numeric(rf_fact$line)
rf_fact <- rf_fact[order(rf_fact$line),]

rf_myth_pos <- cbind(inner_join(neg_set_processed,pdict),rep(as.numeric(1)))#rep("Positive"))
names(rf_myth_pos) <- c("line","word","sentiment")
rf_myth_neg <- cbind(inner_join(neg_set_processed,ndict),rep(as.numeric(-1)))#,rep("Negative"))
names(rf_myth_neg) <- c("line","word","sentiment")
rf_myth <- rbind(rf_myth_pos,rf_myth_neg)
rf_myth_neu <- cbind(subset(neg_set_processed, !(word %in% rf_myth$word)),rep(as.numeric(0)))#rep("Neutral"))
names(rf_myth_neu) <- c("line","word","sentiment")
rf_myth <- rbind(rf_myth,rf_myth_neu)

rf_myth$line <- as.numeric(rf_myth$line)
rf_myth <- rf_myth[order(rf_myth$line),]


# This means sum sentiments, grouping by line numbers
rf_fact_sums <- aggregate(rf_fact$sentiment, by=list(line=rf_fact$line), FUN=sum)
rf_myth_sums <- aggregate(rf_myth$sentiment, by=list(line=rf_myth$line), FUN=sum)
rf_fact_sums <- cbind(rf_fact_sums,rep("Fact"))
names(rf_fact_sums) <- c("line","sentiment","label")
rf_myth_sums <- cbind(rf_myth_sums,rep("Myth"))
names(rf_myth_sums) <- c("line","sentiment","label")
rf_data <- rbind(rf_fact_sums, rf_myth_sums)
# "rf_data" rows (and row names) match "data" rows (and row names)

# Add more data columns
rf_data <- cbind(rf_data,length=lengths(strsplit(data$MESSAGE," "))) # Number of words
rf_data <- cbind(rf_data,numChars=nchar(data$MESSAGE)) # Number of characters

rf_data <- rf_data[,2:ncol(rf_data)]
rf_data$label <- as.factor(rf_data$label)

# Get train/test sets
train_ind <- runif(nrow(rf_data)) < 0.7
train_set <- rf_data[train_ind,]
test_set <- rf_data[!train_ind,]

# Train RF
#set.seed(10)
rf <- randomForest(formula = label~., data = train_set, ntree = 1000, type = "classification")
rf
plot(rf) #plots trees versus error
rf.prediction <- predict(rf,newdata=test_set)
rfconfusion.matrix <- table(test_set$label, rf.prediction)
print(rfconfusion.matrix)
accuracy.percent <- 100*sum(diag(rfconfusion.matrix))/sum(rfconfusion.matrix)
print(paste("accuracy:",accuracy.percent,"%"))
