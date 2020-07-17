library(tm)
library(dplyr)
library(tidytext)
library(SnowballC) #stemming
library(textstem) #lemmitisation
library(stringr) #removing numbers
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(C50)

### Task 1: Read file, separate the two categories. 
data <- read.csv("C:/Users/Wendy/Desktop/All Files/Team A-Jin/Team A-Jin/covid_19_data.csv",header=T, sep=',', stringsAsFactors = FALSE)
View(data)

#organize data by label (myth or fact)  
data <- data[order(data$LABEL),]
View(data)

#after organizing data by label the row names get moved around
#this puts the row names back in order 1,2,3...nrow(data) 
row.names(data) <- 1:nrow(data)
View(data)

#create a column that shows the label of the rumors
facts_index<-data$LABEL=="FACTS"
View(facts_index)

#creates a table showing fact/myth label and message
fact_set<-data[facts_index,] #facts only w/ message
View(fact_set)
myth_set<-data[!facts_index,] #myths only w/message
View(myth_set)

#creates a table showing only the fact/myth messages
fact_set <- fact_set$MESSAGE
View(fact_set)
myth_set <- myth_set$MESSAGE
View(myth_set)

#creates a table showing fact/myth messages including a numbered column
fact_set_tibble <- tibble(line = 1:length(fact_set), text = fact_set)
View(fact_set_tibble)
myth_set_tibble <- tibble(line = 1:length(myth_set), text = myth_set)
View(myth_set_tibble)

### Task 2: Preprocess the text messages, such as 
###         tokenization, stem, remove stopwords, etc. 
#positive sentiments from the numbered fact messages table
pos_set_processed <- fact_set_tibble %>%
  unnest_tokens(word, text) %>%
  filter(!str_detect(word, "[^[:alpha:]]")) %>%
  anti_join(stop_words, by = "word") %>%
  distinct() %>%
  mutate(word_stem = SnowballC::wordStem(word)) %>%
  mutate(word_lemma = textstem::lemmatize_words(word)) 
View(pos_set_processed)

#negative sentiments from the numbered myth messages table
neg_set_processed <- myth_set_tibble %>%
  unnest_tokens(word, text) %>%
  filter(!str_detect(word, "[^[:alpha:]]")) %>%
  anti_join(stop_words, by = "word") %>%
  distinct() %>%
  mutate(word_stem = SnowballC::wordStem(word)) %>%
  mutate(word_lemma = textstem::lemmatize_words(word)) 
View(neg_set_processed)

#trying to get word lemma with label
neg_sent <- merge(neg_set_processed$word_lemma, data$LABEL)
View(neg_sent)
pos_sent <- merge(pos_set_processed$word_lemma, data$LABEL)
View(pos_sent)

#this will create a list of the lemmatized words from the fact set
pos_set_processed <- as.data.frame(cbind(pos_set_processed$line, pos_set_processed$word_lemma))
View(pos_set_processed)
#this just renames the columns 
names(pos_set_processed) <- c("line","word")
View(pos_set_processed)

#this will create a list of the lemmatized words from the myth set
neg_set_processed <- as.data.frame(cbind(neg_set_processed$line, neg_set_processed$word_lemma))
View(neg_set_processed)
#this just renames the columns
names(neg_set_processed) <- c("line","word")
View(neg_set_processed)

### Task 3: Do sentiment analysis for the two categories using 
###         the positive and negative dictionaries. 
###         Compare two category's sentiment difference. 

#reading the positive dictionary
pdict <- read.csv("C:/Users/Wendy/Desktop/All Files/Team A-Jin/Team A-Jin/positive-dictionary.txt",header=T, sep='\t', stringsAsFactors = FALSE)
pdict <- as.data.frame(pdict[29:2034,]) #ignores leading lines by only reading in lines 29-2034
names(pdict)<-  c("word") #changes column name
View(pdict)

#reading the negative dictionary
ndict <- read.csv("C:/Users/Wendy/Desktop/All Files/Team A-Jin/Team A-Jin/negative-dictionary.txt",header=T, sep='\t', stringsAsFactors = FALSE)
ndict <- as.data.frame(ndict[30:4812,]) #ignores leading lines by only reading in lines 30-4812
names(ndict)<-  c("word")
View(ndict)

# Counting how many positive/negative words in Fact dataset
pos_pdict <- count(inner_join(pos_set_processed,pdict),word,sort=TRUE)
View(pos_pdict)
pos_ndict <- count(inner_join(pos_set_processed,ndict),word,sort=TRUE)
View(pos_ndict)
#displays word count of positive words from fact sets
sum(pos_pdict$n) #37
sum(pos_ndict$n) #128

# Counting how many positive/negative words in myth dataset
neg_pdict <- count(inner_join(neg_set_processed,pdict),word,sort=TRUE)
View(neg_pdict)
neg_ndict <- count(inner_join(neg_set_processed,ndict),word,sort=TRUE)
View(neg_ndict)
#displays word count of negative words from fact sets
sum(neg_pdict$n) #33
sum(neg_ndict$n) #78

#this will create a graph showing a list of positive words that
#contribute to the facts
pos_words_facts <- pos_pdict %>%
  top_n(10) %>%
  ggplot(aes(reorder(word, n), n)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  labs(y = "Positive Words Contribution to Facts", x = NULL, title = "Positive Word Count") +
  coord_flip()
plot(pos_words_facts)

#this will create a graph showing a list of negative words that
#contribute to the facts 
neg_words_facts <- pos_ndict %>%
  top_n(10) %>%
  ggplot(aes(reorder(word, n), n)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  labs(y = "Negative Words Contribution to Facts", x = NULL, title = "Negative Word Count") +
  coord_flip()
plot(neg_words_facts)

### Task 4: Create more dictionaries, such as "exciting", 
###         "calm", "reliable", etc. Does rumors/spam look 
###         more exciting? Is the non-spam/facts dataset more 
###         calm?

#creating dictionary manually
calm_dict<-data.frame(c("tranqil","composed","peaceful","collected","serene","soothe","detached","placid","unruffled","levelheaded",
                        "cool","unconcerened","coolheaded","unshaken","inoffensive","civil","quite","patient","still","amiable", "moderate", "dispassionate", "eventempered", "mild", 
                        "smooth", "in-order"))
dim(calm_dict)
calm_dict <- cbind(calm_dict, rep("calm", nrow(calm_dict)))
names(calm_dict)<-  c("word", "sentiment")
View(calm_dict)

pos_calm <- pos_set_processed %>%
  inner_join(calm_dict) %>%
  count(word, sentiment, sort = TRUE)
View(pos_calm)

#getting all the sentiments
sentiments <-get_sentiments("nrc")
View(sentiments)
str(sentiments)

#grouping by the different sentiments
Grouped_sentiments <- sentiments %>%
  group_by(sentiment) %>%
  summarise(noOfWords = n())
View(Grouped_sentiments)

#filtering words on the basis of sentiments
sentiments %>% 
  filter(sentiment %in% c("anger", "anticipation", "disgust", "joy", "fear", "sadness", "trust", "surprise")) %>%
  group_by(sentiment) %>%
  summarise(noOfWords = n())

#selecting words with joy sentiment
nrc_joy <- sentiments %>%
  filter(sentiment == "joy")
View(nrc_joy)

pos_joy <- pos_set_processed %>%
  inner_join(nrc_joy) %>%
  count(word, sentiment, sort = TRUE)
View(pos_joy)

#sadness sentiment
nrc_sad <- sentiments %>%
  filter(sentiment == "sadness")
View(nrc_sad)

pos_sad <- pos_set_processed %>%
  inner_join(nrc_sad) %>%
  count(word, sentiment, sort = TRUE)
View(pos_sad)

#plotting bar plot for pos, neg, joy, sad sentiment in facts_set
#creating character vector of sentiments
freq <- c(pos_pdict$sentiment, pos_ndict$sentiment, pos_joy$sentiment, pos_sad$sentiment, pos_calm$sentiment)
View(freq)
summary(freq)
rawbardata <- table(freq)
rawbp <- barplot(rawbardata, col = c("red","black", "darkblue", "green", "orange"), main = "Sentiment Proportion in Facts Set ",
                 ylim = c(0, 50), ylab = "Number")
text(rawbp, 0, round(rawbardata, 1), col = "white", cex = 1, pos = 3)

#storing a dataframe to txt file or csv file
setwd("C:/Users/Wendy/Desktop/All Files/Team A-Jin/Team A-Jin")
write.table(nrc_joy, file = "nrc_joy.txt", sep = "\t",
            row.names = TRUE, col.names = NA)
data <- read.csv("C:/Users/Wendy/Desktop/All Files/Team A-Jin/Team A-Jin/nrc_joy.txt",header=T, sep='\t', stringsAsFactors = FALSE)
View(data)

write.csv(nrc_joy, file = "nrc_joy.csv")
joy_csv <- read.csv("C:/Users/Wendy/Desktop/All Files/Team A-Jin/Team A-Jin/nrc_joy.csv",header=T, sep=',', stringsAsFactors = FALSE)
View(joy_csv)

### Task 5: Visualize your sentiment analysis results using 
###         word cloud.

#word cloud for positive sentiments
set.seed(1234)
pal <- brewer.pal(8, "Dark2")
wordcloud(words = pos_pdict$word, freq = pos_pdict$n, min.freq = 1, 
          max.words = 20, random.order = FALSE, rot.per = 0.35,
          colors = pal, scale = c(3,0.5))
mtext("Covid-19 Data: Positive Sentiments", side = 3, padj = -2)
#more words
wordcloud(words = pos_pdict$word, freq = pos_pdict$n, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,            
          colors=brewer.pal(8, "Dark2"))

#word cloud for negative sentiments
set.seed(1234)
pal <- brewer.pal(8, "Dark2")
wordcloud(words = neg_ndict$word, freq = neg_ndict$n, min.freq = 1, 
          max.words = 20, random.order = FALSE, rot.per = 0.35,
          colors = pal, scale = c(3,0.5))
mtext("Covid-19 Data: Negative Sentiments", side = 3, padj = -2)
#more words
wordcloud(words = neg_ndict$word, freq = neg_ndict$n, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,            
          colors=brewer.pal(8, "Dark2"))

#word cloud showing both positive and negative sentiments
library(reshape2)
library(SentimentAnalysis)
pos_pdict %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)

### A different looking word cloud
n = nrow(pos_pdict)
colors = rep("grey", n)
colors[pos_pdict$n < -1.96] = "Red"
colors[pos_pdict$n > 1.96] =  "Green"
# Creating the word cloud
library(wordcloud2)
wordcloud2(data = pos_pdict[, -3], color = colors, size = 0.5)
wordcloud2(data = neg_ndict[, -3], color = colors, size = 0.9)

### A different looking word cloud
wordcloud2(data=pos_pdict, size=0.8, color='random-dark')
wordcloud2(data=neg_ndict, size=1.6, color='random-dark')

### Pentagon shaped word cloud
wordcloud2(data=pos_pdict, size = 0.7, shape = 'pentagon')
wordcloud2(data=neg_ndict, size = 0.7, shape = 'pentagon')

### Other shapes 
wordcloud2(data=pos_pdict, size = 0.44, shape = 'star')
wordcloud2(data=neg_ndict, size = 0.7, shape='triangle', backgroundColor='black')