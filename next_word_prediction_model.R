library(dplyr)
library(magrittr)
library(stringr)
library(tm)
library(RWeka)
library(SnowballC)
library(ggplot2)

# Download the dataset if it is not already there
if(!file.exists("./data")){
  dir.create("./data")
  url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
  download.file(Url, destfile="./data/Coursera-SwiftKey.zip", mode = "wb")
  unzip(zipfile="./data/Coursera-SwiftKey.zip", exdir="./data")
}

# Read the datasets
blogs <- readLines("./data/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE)
news <- readLines("./data/en_US/en_US.news.txt", encoding = "UTF-8", skipNul = TRUE)
tweets <- readLines("./data/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE)

# Sample the datasets
set.seed(8)
samplelines <- c(sample(tweets, length(tweets) * 0.1),
                 sample(news, length(news) * 1),
                 sample(blogs, length(blogs) * 0.1))

# Create train, test and validation set
set.seed(8)
samplelines <- sample(samplelines)
validationIndex <- floor(length(samplelines) * 0.8)
testingIndex <- floor(length(samplelines) * 0.9)

training <- samplelines[1:validationIndex]
validation <- samplelines[(validationIndex+1):testingIndex]
testing <- samplelines[(testingIndex+1):length(samplelines)]

# Tidy the data
tokenizer <- function(lines) {
  lines <- tolower(lines)
  lines <- gsub("'", "'", lines)
  lines <- gsub("[.!?]$|[.!?] |$", " ''split'' ", lines)
  tokens <- unlist(strsplit(lines, "[^a-z']"))
  tokens <- tokens[tokens != ""]
  return(tokens)
}
tokens <- tokenizer(training)
vtokens <- tokenizer(validation)
ttokens <- tokenizer(testing)

# Generate n-grams counts
tokens2 <- c(tokens[-1], ".")
tokens3 <- c(tokens2[-1], ".")
tokens4 <- c(tokens3[-1], ".")
#tokens5 <- c(tokens4[-1], ".")
#tokens6 <- c(tokens5[-1], ".")

unigrams <- tokens
bigrams <- paste(tokens, tokens2)
trigrams <- paste(tokens, tokens2, tokens3)
quadgrams <- paste(tokens, tokens2, tokens3, tokens4)
#fivegrams <- paste(tokens, tokens2, tokens3, tokens4, tokens5)
#sixgrams <- paste(tokens, tokens2, tokens3, tokens4, tokens5, tokens6)

unigrams <- unigrams[!grepl("''split''", unigrams)]
bigrams <- bigrams[!grepl("''split''", bigrams)]
trigrams <- trigrams[!grepl("''split''", trigrams)]
quadgrams <- quadgrams[!grepl("''split''", quadgrams)]
#fivegrams <- fivegrams[!grepl("''split''", fivegrams)]
#sixgrams <- sixgrams[!grepl("''split''", sixgrams)]

unigrams <- sort(table(unigrams), decreasing=T)
bigrams <- sort(table(bigrams), decreasing=T)
trigrams <- sort(table(trigrams), decreasing=T)
quadgrams <- sort(table(quadgrams), decreasing=T)
#fivegrams <- sort(table(fivegrams), decreasing=T)
#sixgrams <- sort(table(sixgrams), decreasing=T)

# Generating n-grams probabilities
getLastWords <- function(string, words) {
  pattern <- paste("[a-z']+( [a-z']+){", words - 1, "}$", sep="")
  return(substring(string, str_locate(string, pattern)[,1]))
}

removeLastWord <- function(string) {
  sub(" [a-z']+$", "", string)
}

# Kneser-Ney Smoothing
kneserNay <- function(ngrams, d) {
  n <- length(strsplit(names(ngrams[1]), " ")[[1]])
  
  # Special case for unigrams
  if(n==1) {
    noFirst <- unigrams[getLastWords(names(bigrams), 1)]
    pContinuation <- table(names(noFirst))[names(unigrams)] / length(bigrams)
    return(pContinuation)
  }
  
  # Get needed counts
  nMinusOne <- list(unigrams, bigrams, trigrams, quadgrams)[[n-1]] #, fivegrams, sixgrams
  noLast <- nMinusOne[removeLastWord(names(ngrams))]
  noFirst <- nMinusOne[getLastWords(names(ngrams), n-1)]
  
  # Calculate discounts, lambda and pContinuation
  discounts <- ngrams - d
  discounts[discounts < 0] <- 0
  lambda <- d * table(names(noLast))[names(noLast)] / noLast
  if(n == 2) pContinuation <- table(names(noFirst))[names(noFirst)] / length(ngrams)
  else pContinuation <- kneserNay(noFirst, d)
  
  # Put it all together
  probabilities <- discounts / noLast + lambda * pContinuation / length(ngrams)
  return(probabilities)
}

# Calulate probabilities
unigramProbs <- kneserNay(unigrams, 0.75)
bigramProbs <- kneserNay(bigrams, 0.75)
trigramProbs <- kneserNay(trigrams, 0.75)
quadgramProbs <- kneserNay(quadgrams, 0.75)
#fivegramProbs <- kneserNay(fivegrams, 0.75)
#sixgramProbs <- kneserNay(sixgrams, 0.75)

# Build a model
vtokens2 <- c(vtokens[-1], ".")
vtokens3 <- c(vtokens2[-1], ".")
vtokens4 <- c(vtokens3[-1], ".")
#vtokens5 <- c(vtokens4[-1], ".")
#vtokens6 <- c(vtokens5[-1], ".")
vfourgrams <- paste(vtokens, vtokens2, vtokens3, vtokens4) #, vtokens5, vtokens6

createModel <- function(n, threshold) {
  ngrams <- list(bigramProbs, trigramProbs, quadgramProbs)[[n-1]] #, fivegramProbs, sixgramProbs
  
  model <- ngrams[getLastWords(vfourgrams[1:10000], n)]
  names(model) <- vfourgrams[1:10000]
  
  #if(n > 5) model[is.na(model) | model < threshold] <- 
  #  fivegramProbs[getLastWords(names(model[is.na(model) | model < threshold]), 5)]
  if(n > 4) model[is.na(model) | model < threshold] <- 
    quadgramProbs[getLastWords(names(model[is.na(model) | model < threshold]), 4)]
  if(n > 3) model[is.na(model) | model < threshold] <- 
    trigramProbs[getLastWords(names(model[is.na(model) | model < threshold]), 3)]
  if(n > 2) model[is.na(model) | model < threshold] <- 
    bigramProbs[getLastWords(names(model[is.na(model) | model < threshold]), 2)]
  if(n > 1) model[is.na(model) | model < threshold] <- 
    unigramProbs[getLastWords(names(model[is.na(model) | model < threshold]), 1)]
  return(model)
}

model <- createModel(4, 0.005)

# Predict next words
unigramDF <- data.frame("Words" = (names(unigrams)), "Probability" = unigramProbs, stringsAsFactors=F)
bigramsDF <- data.frame("FirstWords" = removeLastWord(names(bigrams)), 
                        "LastWord" = getLastWords(names(bigrams), 1), 
                        "Probability" = bigramProbs, stringsAsFactors=F)
trigramsDF <- data.frame("FirstWords" = removeLastWord(names(trigrams)), 
                         "LastWord" = getLastWords(names(trigrams), 1), 
                         "Probability" = trigramProbs, stringsAsFactors=F)
quadgramsDF <- data.frame("FirstWords" = removeLastWord(names(quadgrams)), 
                          "LastWord" = getLastWords(names(quadgrams), 1), 
                          "Probability" = quadgramProbs, stringsAsFactors=F)

unigramDF <- (unigramDF %>% arrange(desc(unigramProbs)))
bigramsDF <- bigramsDF %>% arrange(desc(bigramProbs)) %>% filter(bigramProbs > 0.0001)
trigramsDF <- trigramsDF %>% arrange(desc(trigramProbs)) %>% filter(trigramProbs > 0.0001)
quadgramsDF <- quadgramsDF %>% arrange(desc(quadgramProbs)) %>% filter(quadgramProbs > 0.0001)

predictor <- function(input) {
  n <- length(strsplit(input, " ")[[1]])
  prediction <- c()
  if(n >= 3 && length(prediction)<3) 
    prediction <- c(prediction, filter(quadgramsDF, getLastWords(input, 3) == FirstWords)$LastWord)
  if(n >= 2 && length(prediction)<3) 
    prediction <- c(prediction, filter(trigramsDF, getLastWords(input, 2) == FirstWords)$LastWord)
  if(n >= 1 && length(prediction)<3) 
    prediction <- c(prediction, filter(bigramsDF, getLastWords(input, 1) == FirstWords)$LastWord)
  if(length(prediction)<3 ) prediction <- c(prediction, unigramDF$Words)
  
  return(unique(prediction)[1:3])
}

