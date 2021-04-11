library(shiny)
shinyUI(fluidPage(
  titlePanel("'What's next?' word prediction app"),
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Prediction", br(), 
                         textInput("textInput", label = h3("Text input"), value = ""),
                         h3("Next world"),
                         textOutput("predWord"),
                ),
                tabPanel("Info", br(), 
                         "This app predicts the next word using Stupid Backoff approach.", br(),br(),
                         "Database and coding for this app can bi found on GitHub: nilomartin/data-science-capstone-project"
                )
    )
  )
))