library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Next Word Predictor - Data Science Capstone Project"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("txtInput", "Input", "this is"),
      helpText("Type your sentence in the textbox above and press the button below 
         to predict the three most likely words to follow your input."),
      actionButton("clickPredict", "Predict Next Word", width = 200, style="color:red")
    ),
    
    mainPanel(
      fluidRow(
        align="center",
        h5("Your sentence"),
        h3(textOutput("inputSentence")), br(), br(),
        h5("Top 3 next word predictions for your sentence"),
        h1(textOutput("wordPrediction"), style="color:red")
      )
    )
  )
))
