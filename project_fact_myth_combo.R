library(tm)
library(NLP)
library(dplyr)
library(tidytext)
library(SnowballC)
library(textstem)
library(ggplot2)
library(stringr)
library(wordcloud)
library(RColorBrewer)
library(gridExtra)

df<-read.csv(file="covid_19_data.csv",header=TRUE,sep = ",", stringsAsFactors = FALSE)
data = df

#FACT STUFF FIRST AND LOOK AT IT

data$LABEL[data$LABEL=='FACTS']<-'FACT'

positive_check <-data$LABEL == "FACT"
positive_message <- data[positive_check,]
negative_message <- data[!positive_check,]

positive_message <- positive_message$MESSAGE
View(positive_message)

positive_message_tibble <- tibble(line = 1:length(positive_message), text = positive_message)
View(positive_message_tibble)

positive_message_processed <- positive_message_tibble %>%
  unnest_tokens(word, text) %>%
  filter(!str_detect(word, "[^[:alpha:]]")) %>%
  anti_join(stop_words, by = "word") %>%
  distinct() %>%
  mutate(word_stem = SnowballC::wordStem(word)) %>%
  mutate(word_lemma = textstem::lemmatize_words(word)) 

View(positive_message_processed)
#dim(body_words)

sentiments <- get_sentiments("bing")
positive_dictionary <- text <- read.delim(file.choose(), stringsAsFactor = FALSE)
negative_dictionary <- text <- read.delim(file.choose(), stringsAsFactor = FALSE)

View(positive_dictionary)
View(negative_dictionary)

dim(positive_dictionary)
dim(negative_dictionary)

positive_dictionary <- as.data.frame(positive_dictionary[29:2034,])
names(positive_dictionary)[1] <- "words"
dim(positive_dictionary)
View(positive_dictionary)

positive_Value <- rep('positive',nrow(positive_dictionary))
positive_dictionary <- cbind(positive_dictionary, positive_Value)
names(positive_dictionary)<-  c("word", "sentiment")
View(positive_dictionary)

negative_dictionary <- as.data.frame(negative_dictionary[30:4812,])
negative_value <- rep("negative", nrow(negative_dictionary))
negative_dictionary <- cbind(negative_dictionary, negative_value)
names(negative_dictionary)<-  c("word", "sentiment")
View(negative_dictionary)

pos_positive_dictionary <- positive_message_processed %>%
  inner_join(positive_dictionary) %>%
  count(word, sentiment, sort = TRUE)
View(pos_positive_dictionary)

pos_negative_dictionary <- positive_message_processed %>%
  inner_join(negative_dictionary) %>%
  count(word, sentiment, sort = TRUE)
View(pos_negative_dictionary)

pos_words_facts <- pos_positive_dictionary %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ggplot(aes(reorder(word, n), n, fill = sentiment)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Positive Words Contribution to COVID-19 Facts", x = NULL) +
  coord_flip()
plot(pos_words_facts)

neg_words_facts <- pos_negative_dictionary %>%
  group_by(sentiment) %>%
  top_n(10) %>% 
  ggplot(aes(reorder(word, n), n, fill = sentiment)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Negative Words Contribution to COVID-19 Facts", x = NULL) +
  coord_flip()
plot(neg_words_facts)

set.seed(100)

wordcloud(words = pos_positive_dictionary$word, freq = pos_positive_dictionary$n, min.freq = 1,
          max.words = 200, random.order = FALSE, 
          rot.per = 0.35,colors = brewer.pal(8, "Dark2"),
          scale = c(2.5, 0.75))
mtext("Positive Words from COVID-19 Facts", side = 3, padj = -2)

wordcloud(words = pos_negative_dictionary$word, freq = pos_negative_dictionary$n, min.freq = 1,
          max.words = 200, random.order = FALSE, 
          rot.per = 0.35,colors = brewer.pal(8, "Dark2"),
          scale = c(2.5, 0.75))
mtext("Negative Words from COVID-19 Facts", side = 3, padj = -2)

#NOW THE MYTH BREAKDOWN......JUST LIKE THE FACTS!!! :-) 

negative_check <-data$LABEL == "MYTH"
m_positive_message <- data[negative_check,]
m_negative_message <- data[!negative_check,]

m_positive_message <- m_positive_message$MESSAGE
View(m_positive_message)

m_positive_message_tibble <- tibble(line = 1:length(m_positive_message), text = m_positive_message)
View(m_positive_message_tibble)

m_positive_message_processed <- m_positive_message_tibble %>%
  unnest_tokens(word, text) %>%
  filter(!str_detect(word, "[^[:alpha:]]")) %>%
  anti_join(stop_words, by = "word") %>%
  distinct() %>%
  mutate(word_stem = SnowballC::wordStem(word)) %>%
  mutate(word_lemma = textstem::lemmatize_words(word)) 

View(m_positive_message_processed)
#dim(body_words)

sentiments <- get_sentiments("bing")
m_positive_dictionary <- text <- read.delim(file.choose(), stringsAsFactor = FALSE)
m_negative_dictionary <- text <- read.delim(file.choose(), stringsAsFactor = FALSE)

View(m_positive_dictionary)
View(m_negative_dictionary)

dim(m_positive_dictionary)
dim(m_negative_dictionary)

m_positive_dictionary <- as.data.frame(m_positive_dictionary[29:2034,])
names(m_positive_dictionary)[1] <- "words"
dim(m_positive_dictionary)
View(m_positive_dictionary)

m_positive_Value <- rep('positive',nrow(m_positive_dictionary))
m_positive_dictionary <- cbind(m_positive_dictionary, m_positive_Value)
names(m_positive_dictionary)<-  c("word", "sentiment")
View(m_positive_dictionary)

m_negative_dictionary <- as.data.frame(m_negative_dictionary[30:4812,])
m_negative_value <- rep("negative", nrow(m_negative_dictionary))
m_negative_dictionary <- cbind(m_negative_dictionary, m_negative_value)
names(m_negative_dictionary)<-  c("word", "sentiment")
View(m_negative_dictionary)

neg_positive_dictionary <- m_positive_message_processed %>%
  inner_join(m_positive_dictionary) %>%
  count(word, sentiment, sort = TRUE)
View(neg_positive_dictionary)

neg_negative_dictionary <- m_positive_message_processed %>%
  inner_join(m_negative_dictionary) %>%
  count(word, sentiment, sort = TRUE)
View(neg_negative_dictionary)

pos_words_myths <- neg_positive_dictionary %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ggplot(aes(reorder(word, n), n, fill = sentiment)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Positive Words Contribution to COVID-19 Myths", x = NULL) +
  coord_flip()
plot(pos_words_myths)

neg_words_myths <- neg_negative_dictionary %>%
  group_by(sentiment) %>%
  top_n(10) %>% 
  ggplot(aes(reorder(word, n), n, fill = sentiment)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Negative Words Contribution to COVID-19 Myths", x = NULL) +
  coord_flip()
plot(neg_words_myths)

set.seed(100)

wordcloud(words = neg_positive_dictionary$word, freq = neg_positive_dictionary$n, min.freq = 1,
          max.words = 200, random.order = FALSE, 
          rot.per = 0.35,colors = brewer.pal(8, "Dark2"),
          scale = c(2.5, 0.75))
mtext("Positive Words from COVID-19 Myths", side = 3, padj = -2)

wordcloud(words = neg_negative_dictionary$word, freq = neg_negative_dictionary$n, min.freq = 1,
          max.words = 200, random.order = FALSE, 
          rot.per = 0.35,colors = brewer.pal(8, "Dark2"),
          scale = c(2.5, 0.75))
mtext("Negative Words from COVID-19 Myths", side = 3, padj = -2)
