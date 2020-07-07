#upload library 
library(readr)
library(dplyr)
library(ggplot2)
library(tidytext)
library(tibble)
library(gutenbergr)
library(qdap)
library(tidyverse)
library(wordcloud)
library(reshape2)
library(tidyr)

#load the data
this_side <- gutenberg_download(805)
beau_dam <- gutenberg_download(9830)

#binding the data together
data <- rbind(this_side, beau_dam) 

#tokenization and stop words
words <- data %>%
  unnest_tokens(word, text)

word_counts <- words %>%
  anti_join(stop_words, by = 'word') %>%
  count(word,sort=TRUE)

#most frequent words
freq <-head(word_counts,10) 

freqplot <- ggplot(freq, aes(x=reorder(word,-n), y=n, fill=word)) +
  geom_bar(stat="identity") +
  theme_bw() +
  coord_flip()

#negative and positive word counts
book_sentiments <- word_counts %>%
  inner_join(get_sentiments("bing"))

top_words <- book_sentiments %>%
  group_by(sentiment ) %>%
  ungroup() %>%
  top_n(15, n) %>%
  ungroup() %>%
  mutate(word=reorder(word,n))

sentimentsplot <- ggplot(top_words, aes(word,n,fill=sentiment)) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~sentiment, scales = "free")+
  coord_flip()

#wordcloud
wordcloud <- words%>%
  inner_join(get_sentiments('bing')) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~ sentiment, value.var='n', fill=0) %>%
  comparison.cloud(colors=c('purple','pink'),
                   max.words=150)

#getting bigrams
novels_bigrams <- data %>%
  unnest_tokens(bigram,text,token='ngrams',n=2) %>%
  count(bigram,sort=TRUE)

bigrams_separated <- novels_bigrams %>%
  separate(bigram, c('word1','word2'),sep=' ')

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_united <- bigrams_filtered %>%
  unite(bigram,word1,word2,sep=' ')


#top bigrams and plotting them
top_bigrams <- bigram_united %>%
  ungroup() %>%
  top_n(15, n) %>%
  ungroup() %>%
  mutate(word=reorder(bigram,n))

bigramsplot <- ggplot(top_bigrams, aes(x=reorder(word,-n), y=n, fill=n)) +
  geom_bar(stat="identity") +
  theme_bw() +
  coord_flip()
  