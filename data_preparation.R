library(plyr)
library(magrittr)
library(stringr)
library(tm)
library(RWeka)
library(SnowballC)
library(ggplot2)

# download the dataset if it is not already there
if(!file.exists("./data")){
  dir.create("./data")
  url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
  download.file(Url, destfile="./data/Coursera-SwiftKey.zip", mode = "wb")
  unzip(zipfile="./data/Coursera-SwiftKey.zip", exdir="./data")
}

# Read the datasets
dataBlogs <- readLines("./data/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE)
dataNews <- readLines("./data/en_US/en_US.news.txt", encoding = "UTF-8", skipNul = TRUE)
dataTwitter <- readLines("./data/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE)

# Sample the data and create the corpus
subdataBlogs <- sample(dataBlogs, size = 1000)
subdataNews <- sample(dataNews, size = 1000)
subdataTwitter <- sample(dataTwitter, size = 1000)
sampledData <- c(subdataBlogs, subdataNews, subdataTwitter)
corpus <- VCorpus(VectorSource(sampledData))

# Remove stopwords, punctuation, whitespaces, numbers etc. from the corpus
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus <- tm_map(corpus, toSpace, "/|@|//|$|:|:)|*|&|!|?|_|-|#|")
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
#corpus <- tm_map(corpus, removeWords, stopwords())
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, stripWhitespace)

# Create the DocumentTermMatrizes
dtm1 <- TermDocumentMatrix(corpus)
bigram <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
dtm2 <- TermDocumentMatrix(corpus, control = list(tokenize = bigram))
trigram <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
dtm3 <- TermDocumentMatrix(corpus, control = list(tokenize = trigram))
quadgram <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
dtm4 <- TermDocumentMatrix(corpus, control = list(tokenize = quadgram))

# 1-Gram Frequency
freq1 <- rowSums(as.matrix(dtm1))
freq1 <- sort(freq1, decreasing = TRUE)
dfFreq1 <- data.frame(word = names(freq1), freq=freq1)

# 2-Gram Frequency
freq2 <- rowSums(as.matrix(dtm2))
freq2 <- sort(freq2, decreasing = TRUE)
dfFreq2 <- data.frame(word = names(freq2), freq=freq2)

# 3-Gram Frequency
freq3 <- rowSums(as.matrix(dtm3))
freq3 <- sort(freq3, decreasing = TRUE)
dfFreq3 <- data.frame(word = names(freq3), freq=freq3)

# 4-Gram Frequency
freq4 <- rowSums(as.matrix(dtm4))
freq4 <- sort(freq4, decreasing = TRUE)
dfFreq4 <- data.frame(word = names(freq4), freq=freq4)



