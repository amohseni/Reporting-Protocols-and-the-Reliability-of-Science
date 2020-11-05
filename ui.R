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
  # CSS for visuals
  includeCSS("www/style.css"),
  
  # Title
  titlePanel("The Reliability of Scientific Findings and Reporting Protocols"),
  
  # Load MathJax
  withMathJax(),
  
  fluidRow(style = "background-color:#F2F2F2; margin-top: 30px; margin-bottom: 30px; padding: 10px",
           column(width = 4,
                  # Description & The Model
                  div(
                    HTML(
                      "<strong>Description:</strong> This model explores the effect of various methods of testing and reporting hypotheses on the reliability of scientific findings. </br></br> <strong>The model:</strong> Consider a large population of scientific studies where a study consists of hypothesis tests between up to \\(L \\in \\mathbb{N} \\) probabilistically indepedent pairs of null \\(H_0\\) and alternative \\(H_1\\) hypotheses with a given significance threshold \\(\\alpha \\in (0,1)\\), average statistical power \\(1-\\beta \\in (0,1)\\) (which is itself determined by the average sample size \\(n \\in \\mathbb{N}\\) and effect size \\(d \\in \\mathbb{R}\\) for studies), and underlying prevalence of true hypotheses \\(\\pi \\in (0,1)\\). </br></br></br></br> <strong>Reference:</strong> For the full details of the model and findings see Mohseni (2020) <em>'HARKing: From Misdiagnosis to Misprescription'</em> at <a href='http://www.aydinmohseni.com/research'>www.aydinmohseni.com</a>.</br></br> <strong>Open-Source Code:</strong> All the <a href='https://shiny.rstudio.com/'>R</a> and <a href='https://www.r-project.org/'>Shiny</a> code for this project is available at <a href='https://github.com/amohseni/Reporting-Protocols-and-the-Reliability-of-Science'>www.github.com/amohseni</a>."
                   ))),
           column(width = 4,
                  # Reporting Methods
                  div(
                    HTML(
                      "<strong>False Discovery Rate (FDR):</strong> A false positive occurs when a study yields a significant outcome when the null hypothesis, \\(H_0\\), is true. The false discovery rate is equal to the expected number of false positive outcomes over the number of all significant outcomes. \\[FDR=\\cfrac{Pr(H_0|\\text{significant})}{Pr(\\text{significant})}\\] <strong>False Omission Rate (FOR):</strong> A false negative occurs when a study yields a non-significant outcome when the alternative hypothesis, \\(H_1\\), is true. The false omission rate is equal to the expected number of false negative outcomes over the number of all non-significant outcomes. \\[FOR=\\cfrac{Pr(H_1|\\text{non-significant})}{Pr(\\text{non-significant})}\\] <strong>Magnitude Exaggeration Ratio  (MER):</strong> A reported effect size, \\(\\hat{d}\\), is exaggerated when it is larger than the true effect size, \\(d\\). The magnitude exaggeration ratio is equal to the expected ratio of this exaggeration. \\[MER=\\mathbb{E}[ \\hat{d}/d]\\]"
                    ))),
            column(width = 4,
                   # False Discovery and False Omission Rate text:
                   div(
                     HTML(
                       "<strong>Reporting Protocols:</strong> Given a parameterization of a population of studies and the preceding measures of the (un)reliability of findings, we can consider the effects of alternative protocols for selecting and submitting hypotheses to test for the reliability of scientific findings. Call these <em>reporting protocols</em>: 
<ul> </br>
  <li><strong>Prediction:</strong> Prior to observing the data, choose a hypothesis and report it if it is statistically significant. </li> </br>
  <li><strong>Pure HARKing:</strong> After obseving the data, select a hypothesis to report from among those that are statistically signficant.</li> </br>
  <li><strong>Fallback HARKing:</strong> Prior to observing the data, choose a hypothesis and report it if it is statistically significant; otherwise select a hypothesis to report from among the auxialliary hypotheses that are signficant. </li>
</ul>"                       
                     )))),  
  
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
        style = "padding-left: 20px; padding-bottom: 40px; text-align: center;",
        tabsetPanel(type = "tabs",
                    tabPanel("False Discovery Rate (FDR)", plotOutput("FDRPlotOutput", height = "600px")),
                    tabPanel("False Omission Rate (FOR)", plotOutput("FORPlotOutput", height = "600px")),
                    tabPanel("Magnitude Exaggeration Ratio (MER)", plotOutput("MERPlotOutput", height = "600px"))
        )
      )
    )
  )
))

### EOD ###