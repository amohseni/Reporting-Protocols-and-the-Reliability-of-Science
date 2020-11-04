# Reporting Protocols and the Reliability of Scientific Findings
# << SERVER >>
# by Aydin Mohseni

# Load packages
library(shiny)
library(shinyEventLogger)
library(ggplot2)
library(ggthemes)
library(pwr)

shinyServer(function(input, output, session) {
  computeReliability <- reactive({
    # Global Variables
    N <- as.numeric(input$numberOfHypotheses) # number of possible hypotheses
    brPrediction <-
      as.numeric(input$baseRateOfPrediction) # the base rate for true hypotheses selected via prediction
    brHARKing <- as.numeric(input$baseRateOfHarking) # the base rate for true hypotheses selected via HARKing
    #alpha <- 0.05 # the type I error rate of statistical tests
    d <- as.numeric(input$effectSize) # the average effect size for difference of means in terms of Cohen's d
    n <- as.numeric(input$sampleSize) # the average sample size for studies
    
    # We model the effect of changing the conventional threshold
    # for statistical signficance on the false discovery rate of a literature.
    
    # A literature consists in a unit mass of studies.
    # A study consists of gathering data against which a set of N logically independent hypotheses may be tested
    
    # One descriptive protocol researchers may follow is: `fallback HARKing'
    # In `fallback HARKING', prior to observing her data a researcher select a hypothesis for testing that she judges is most likely true H_1. Upon observing her data, if the researcher finds that her hypothesis H_1 is statistically significant then she reports it. If, however, she finds that her hypothesis H_1 is not statistically significant, she turns to her N-1 other hypotheses and reports one that is in fact significant, if one exists.
    
    # Define a function to compute the false discovery rate (FDR) for each protocol
    funFDR <- function(alpha, br) {
      # First, compute the expected Type II error for the test
      beta <- 1 - pwr.t.test(n = n,
                             d = d,
                             sig.level = alpha)$power
      # If alpha = 0, there are no discoveries, true or false, so we add this conditional to avoid dividing by zero
      if (alpha != 0) {
        # The false discovery rate of a protocol is given by the fraction of false discoveries over all discoveries.
        x <-
          alpha * (1 - br) # false discoveries = Pr(significant | H_0) * Pr(H_0)
        y <-
          (1 - beta) * br # true discoveries = Pr(significant | H_1) * Pr(H_1)
        z <-
          x / (x + y) # false discovery rate = false discoveries / all discoveries
        return(z)
      } else {
        # If alpha = 0, there are no discoveries, true or false
        return(0) #
      }
    }
    
    # Define a function to compute the false omission rate (FOR) for each protocol
    funFOR <- function(alpha, br) {
      # First, compute the expected Type II error for the test
      beta <- 1 - pwr.t.test(n = n,
                             d = d,
                             sig.level = alpha)$power
      # If alpha = 1, there are no omission, true or false, so we add this conditional to avoid dividing by zero
      if (alpha != 1) {
        # The false omission rate of a protocol is given by the fraction of false discoveries over all discoveries.
        u <-
          beta * br # false omissions = Pr(non-significant | H_1) * Pr(H_1)
        v <-
          (1 - alpha) * (1 - br) # true omissions = Pr(non-significant | H_0) * Pr(H_0)
        w <-
          u / (u + v) # false discovery rate = false discoveries / all discoveries
        return(w)
      } else {
        # If alpha = 1, there are no omissions, true or false
        return(0) #
      }
    }
    
    # Define a function to compute the _net_ false discovery rate (FDR) for the literature
    funFDRn <-
      function(alpha) {
        # First, compute the expected Type II error for the test
        beta <- 1 - pwr.t.test(n = n,
                               d = d,
                               sig.level = alpha)$power
        # The net false discovery rate of the protocol is a weighted mixture of the false discovery rate for each prediction and for HARKing.
        # Specifically, with probability q corresponding to the discovery rate of prediction the researcher will report that her predicted hypothesis is significanct. Such discoveries will, of course, exhibit the false discovery rate of predicton.
        q <-
          (1 - beta) * brPrediction + alpha * (1 - brPrediction) # discovery rate of prediction = true discoveries + false discoveries
        # And with probability 1-q her result will not be significant and so she will turn to select another hypothesis from the set of significant hypotheses (which will exhibit a lower base rate). There will be at least one such hypothesis with probability r = 1 - (discovery rate) ^ N. These hypotheses will exhibit the false discovery of HARKing.
        r <-
          1 - ((1 - alpha) * (1 - brHARKing) + beta * brHARKing) ^ N # discovery rate of HARKing = 1 - (true omissions + false omissions) ^ N
        # Now, we compute the approporate weighting s of the discovery rates for the prediction protocol.
        # (The weight of the the discovery rate of the HARKing protocol is simply the complement of that or prediction 1 - s.)
        if (alpha != 0) {
          # If alpha = 0, there are no discoveries by either protocol, so we add this conitional to avoid dividing by zero
          s <- q / (q + r * (1 - q))
        } else {
          s <- 0
        }
        # Finally, we compute the false discovery rate of `fallback HARKing' as the appropriately weighted mixture of the false discovery rates of each prediction with plausible hypotheses and HARKing with implausible ones.
        t <-
          s * funFDR(alpha, brPrediction) + (1 - s) * funFDR(alpha, brHARKing)
        return(c(t, s, r)) # return the aggreatate FDR, the weight of prediction, and the discovery rate for HARKing
      }
    
    # Define a function to compute the magnitude exaggeration ratio (MER) for the literature
    funMER <- function(alpha, FDR) {
      if (alpha == 0) {
        return(1)
      } else {
        dTrue <-
          FDR * 0 + (1 - FDR) * d # The true mean effect size of findings
        dCrit <- qnorm(
          p = alpha,
          mean = 0,
          sd = 1,
          lower.tail = FALSE
        ) # The mean critical value of tests
        dReportedTrue <-
          integrate(function(x)
            x * dnorm(x, mean = d), dCrit, Inf)[[1]] / (1 - pnorm(dCrit, mean = d)) # mean effect size of true effects that cross significance threshold
        dReportedFalse <-
          integrate(function(x)
            x * dnorm(x, mean = 0), dCrit, Inf)[[1]] /  (1 - pnorm(dCrit, mean = 0)) # mean effect size of false effects that cross significance threshold
        dReported <-
          FDR * dReportedTrue + (1 - FDR) * dReportedFalse # weighted mean of true and false effect sizes that cross the signficance treshold
        return(dReported / dTrue) # Ratio of mean reported to true mean effects sizes
      }
    }
    
    # Compute the net false discovery rate for various values of alpha
    alphaVec <- seq(from = 0, to = 1, by = 0.01)
    FDRp <- sapply(alphaVec, funFDR, br = brPrediction)
    FDRh <- (1 - 1 / N) * sapply(alphaVec, funFDR, br = brHARKing) + (1 / N) * FDRp
    FDRn <- sapply(alphaVec, funFDRn)[1,]
    
    # Compute the net false omiision rate for various values of alpha
    FORp <- sapply(alphaVec, funFOR, br = brPrediction)
    FORh <- sapply(alphaVec, funFOR, br = brHARKing)
    FORn <- (1 - 1 / N) * sapply(alphaVec, funFOR, br = brHARKing) + (1 / N) * FORp
    
    # Compute the magnitude exaggeration ratios for various values of alpha
    MERp <- mapply(funMER, alphaVec, FDRp)
    MERh <- mapply(funMER, alphaVec, FDRh)
    MERn <- mapply(funMER, alphaVec, FDRn)
    
    # Produce the data frame of the false discovery rates to use in graphing
    dataLabelsFDR <- c("FDR(h)", "FDR(fh)", "FDR(p)")
    dataFDR <- c(FDRh, FDRn, FDRp)
    group <- rep(dataLabelsFDR, each = length(alphaVec))
    FDRdf <-
      data.frame(data = matrix(c(
        rep(alphaVec, times = length(dataLabelsFDR)), dataFDR, c(group)
      ), ncol = 3))
    colnames(FDRdf) <- c("Alpha", "FDR", "Group")
    FDRdf$Alpha <- as.numeric(as.character(FDRdf$Alpha))
    FDRdf$FDR <- as.numeric(as.character(FDRdf$FDR))
    FDRdf$Group <- factor(FDRdf$Group, dataLabelsFDR)
    
    # Produce the data frame of the false omission rates to use in graphing
    dataLabelsFOR <- c("FOR(h)", "FOR(fh)", "FOR(p)")
    dataFOR <- c(FORh, FORn, FORp)
    group <- rep(dataLabelsFOR, each = length(alphaVec))
    FORdf <-
      data.frame(data = matrix(c(
        rep(alphaVec, times = length(dataLabelsFOR)), dataFOR, c(group)
      ), ncol = 3))
    colnames(FORdf) <- c("Alpha", "FOR", "Group")
    FORdf$Alpha <- as.numeric(as.character(FORdf$Alpha))
    FORdf$FOR <- as.numeric(as.character(FORdf$FOR))
    FORdf$Group <- factor(FORdf$Group, dataLabelsFOR)
    
    # Produce the data frame of the magnitude exaggeration ratios to use in graphing
    dataLabelsMER <- c("MER(h)", "MER(fh)", "MER(p)")
    dataMER <- c(MERh, MERn, MERp)
    group <- rep(dataLabelsMER, each = length(alphaVec))
    MERdf <-
      data.frame(data = matrix(c(
        rep(alphaVec, times = length(dataLabelsMER)), dataMER, c(group)
      ), ncol = 3))
    colnames(MERdf) <- c("Alpha", "MER", "Group")
    MERdf$Alpha <- as.numeric(as.character(MERdf$Alpha))
    MERdf$MER <- as.numeric(as.character(MERdf$MER))
    MERdf$Group <- factor(MERdf$Group, dataLabelsMER)
    
    # OUTPUT the data for the plots
    return(list(FDRdf,FORdf,MERdf))
    
  })
  
  # Output a plot of the false disocovery rates of findings for each protocol
  output$FDRPlotOutput <- renderPlot({
    # Import computed distributions
    FDRdf <- computeReliability()[[1]]
    
    # Make a graph of the false discovery rates as a function of the signfinance threshold
    G <- ggplot(FDRdf) +
      geom_line(
        data = FDRdf,
        size = 2,
        aes(
          x = Alpha,
          y = FDR,
          color = Group
        ),
        alpha = 1
      ) +
      theme_minimal() +
      ggtitle("") +
      labs(x = expression(paste("Significance Threshold ", alpha)), y = "False Discovery Rate") +
      scale_color_manual(
        values = c("orangered2", "#3475BC", "black"),
        labels = c("Pure \nHARKing", "Fallback \nHARKing", "Prediction")
      ) +
      scale_x_continuous(limits = c(0, 1)) +
      theme(
        legend.title = element_blank(),
        legend.position = "right",
        legend.spacing.x = unit(10, 'pt'),
        legend.spacing.y = unit(30, 'pt'),
        legend.text = element_text(size = 16, margin = margin(
          t = 5, b = 5, unit = "pt"
        )),
        plot.title = element_text(
          hjust = 0.5,
          margin = margin(b = 10, unit = "pt"),
          lineheight = 1.15
        ),
        axis.title.x =  element_text(margin = margin(t = 10, unit = "pt")),
        axis.title.y =  element_text(margin = margin(r = 20, unit = "pt")),
        text = element_text(size = 16)
      )
    # Plot the final graph
    print(G)
  })
  
  # Output a plot of the false omission rates of findings for each protocol
  output$FORPlotOutput <- renderPlot({
    # Import computed distributions
    FORdf <- computeReliability()[[2]]

    # Make a graph of the false omission rates as a function of the signfinance threshold
    H <- ggplot(FORdf) +
      geom_line(
        data = FORdf,
        size = 2,
        aes(
          x = Alpha,
          y = FOR,
          color = Group
        ),
        alpha = 1
      ) +
      theme_minimal() +
      ggtitle("") +
      labs(x = expression(paste("Significance Threshold ", alpha)), y = "False Omission Rate") +
      scale_color_manual(
        values = c("orangered2", "#3475BC", "black"),
        labels = c("Pure \nHARKing", "Fallback \nHARKing", "Prediction")
      ) +
      scale_x_continuous(limits = c(0, 1)) +
      theme(
        legend.title = element_blank(),
        legend.position = "right",
        legend.spacing.x = unit(10, 'pt'),
        legend.spacing.y = unit(30, 'pt'),
        legend.text = element_text(size = 16, margin = margin(
          t = 5, b = 5, unit = "pt"
        )),
        plot.title = element_text(
          hjust = 0.5,
          margin = margin(b = 10, unit = "pt"),
          lineheight = 1.15
        ),
        axis.title.x =  element_text(margin = margin(t = 10, unit = "pt")),
        axis.title.y =  element_text(margin = margin(r = 20, unit = "pt")),
        text = element_text(size = 16)
      )
    # Plot the final graph
    print(H)
  })
  
  # Output a plot of the magnitude exaggeration ratio of findings for each protocol
  output$MERPlotOutput <- renderPlot({
    # Import computed distributions
    MERdf <- computeReliability()[[3]]
    
    # Make a graph of the magnitude exaggeration ratios as a function of the signfinance threshold
    I <- ggplot(MERdf) +
      geom_line(
        data = MERdf,
        size = 2,
        aes(
          x = Alpha,
          y = MER,
          color = Group
        ),
        alpha = 1
      ) +
      theme_minimal() +
      ggtitle("") +
      labs(x = expression(paste("Significance Threshold ", alpha)), y = "Magnitude Exaggeration Ratio") +
      scale_color_manual(
        values = c("orangered2", "#3475BC", "black"),
        labels = c("Pure \nHARKing", "Fallback \nHARKing", "Prediction")
      ) +
      scale_x_continuous(limits = c(0, 1)) +
      theme(
        legend.title = element_blank(),
        legend.position = "right",
        legend.spacing.x = unit(10, 'pt'),
        legend.spacing.y = unit(30, 'pt'),
        legend.text = element_text(size = 16, margin = margin(
          t = 5, b = 5, unit = "pt"
        )),
        plot.title = element_text(
          hjust = 0.5,
          margin = margin(b = 10, unit = "pt"),
          lineheight = 1.15
        ),
        axis.title.x =  element_text(margin = margin(t = 10, unit = "pt")),
        axis.title.y =  element_text(margin = margin(r = 20, unit = "pt")),
        text = element_text(size = 16)
      )
    # Plot the final graph
    print(I)
  })
  
})

### EOD ###