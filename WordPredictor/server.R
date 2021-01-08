library(shiny)
library(stringr)
library(dplyr)
if(!exists("unigramDF"))
  unigramDF <- readRDS("unigram.rds")
if(!exists("bigramsDF"))
  bigramsDF <- readRDS("bigram.rds")
if(!exists("trigramDF"))
  trigramsDF <- readRDS("trigram.rds")

getLastWords <- function(string, words) {
  pattern <- paste("[a-z']+( [a-z']+){", words - 1, "}$", sep="")
  return(substring(string, str_locate(string, pattern)[,1]))
}

removeLastWord <- function(string) {
  sub(" [a-z']+$", "", string)
}

predictor <- function(input) {
  n <- length(strsplit(input, " ")[[1]])
  prediction <- c()
  if(n >= 2 && length(prediction)<3) 
    prediction <- c(prediction, filter(trigramsDF, getLastWords(input, 2) == trigramsDF["FirstWords"])["LastWord"])
  if(n >= 1 && length(prediction)<3)
    prediction <- c(prediction, filter(bigramsDF, getLastWords(input, 1) == bigramsDF["FirstWords"])["LastWord"])
  if(length(prediction)<3 )
    prediction <- c(prediction, unigramDF["Words"])

  print(getLastWords(input, 1))
  print(head(trigramsDF["FirstWords"]))
  #print(unique(prediction))
  return(unique(prediction[[2]])[1:3])
}

shinyServer(function(input, output) {
  
  out <- reactiveValues(sentence = "this is", prediction = "a the not")
  
  observeEvent(input$clickPredict, {
    text <- input$txtInput
    out$sentence <- text
    out$prediction <- predictor(text)
  })
  
  output$wordPrediction <- renderText({
    if (is.null(out$prediction)) return()
    out$prediction
  })
  
  output$inputSentence <- renderText({
    if (is.null(out$sentence)) return()
    out$sentence
  })
  
})
