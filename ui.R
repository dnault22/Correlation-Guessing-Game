library(shiny)
               
shinyUI(fluidPage(

  # Application title
  titlePanel("Test Yourself – Guess The Correlation"),

  # Sidebar with input for correlation and number of observations
  sidebarPanel(
#    "INPUTS",

    "Slide the bar so it matches your best guess for the Pearson product-moment correlation coefficient represented in the scatterplot to the right.",
    br(),
    sliderInput("rho", "", min = -1, max = 1, value = 0.0, step=0.01),
    uiOutput("correctness1"),
    actionButton("checkAnswer", "Check Answer \n"),
    actionButton("newPlot", "Get New Plot"),
    actionButton("resetScore", "Reset History \n")
    ),

  mainPanel(plotOutput("scatterplot",width = "700px", height="500px")),

fluidRow(
  column(width = 4,
         plotOutput("secondResults")
  ),
  column(width = 7,
         plotOutput("results")
  )
)


))
