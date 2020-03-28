# Reporting Protocols and the Reliability of Scientific Findings
# << UI >>
# by Aydin Mohseni

# Load packages
library(shiny)
library(ggplot2)
library(pwr)

# Set encoding for special characters
Sys.setlocale("LC_ALL", "fr_FR.UTF-8")

# Define UI for application
shinyUI(fluidPage(
  # CSS for visual
  includeCSS("www/style.css"),
  
  # Title
  titlePanel("Reporting Protocols and the Reliability of Scientific Findings"),
  
  # Load MathJax
  withMathJax(),
  
  fluidRow(style = "background-color:#F2F2F2; margin-top: 30px; margin-bottom: 30px; padding: 10px",
           column(width = 12,
                  # Introduction text:
                  div(
                    HTML(
                      "<strong>Description:</strong> This model explores the effect of various methods of testing and reporting hypotheses on the reliability of scientific findings."
                    )
                  ))),
  
  # Sidebar for Parameter Input
  sidebarLayout(
    sidebarPanel(
      # Base rate of hypotheses selected via prediction
      sliderInput(
        "baseRateOfPrediction",
        "Base rate of hypotheses selected via prediction \\(\\pi_p\\):",
        min = 0,
        max = 1,
        value = 0.9,
        step = 0.05
      ),
      
      # Base rate of hypotheses selected via HARKing
      sliderInput(
        "baseRateOfHarking",
        "Base rate of hypotheses selected via HARKing \\(\\pi_h\\):",
        min = 0,
        max = 1,
        value = 0.1,
        step = 0.05
      ),
      
      # Number of hypotheses
      sliderInput(
        "numberOfHypotheses",
        "Number of candidate hypotheses \\(L\\):",
        min = 1,
        max = 100,
        value = 10,
        step = 1
      ),
      
      # Average study effect size
      sliderInput(
        "effectSize",
        "Average study effect size \\(d\\):",
        min = 0,
        max = 2,
        value = .5,
        step = 0.05
      ),
      
      # Average study sample size
      sliderInput(
        "sampleSize",
        "Average study sample size \\(n\\):",
        "Individual confirmation bias \\(b\\):",
        min = 5,
        max = 150,
        value = 30,
        step = 5
      )
      
    ),
    
    # Main Panel with Stationary Distribution + Simulation & Stats Panels
    mainPanel(
      fluidRow(
        style = "padding-left: 20px; text-align: center;",
        plotOutput("reliabilityPlotOutput", height = "600px")
      )
    )
  )
))

### EOD ###