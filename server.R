# FALSE DISCOVERY, REPORTING PROTOCOLS, AND REDEFINING STATISTICAL SIGNIFICANCE
# << SERVER >>
# by Aydin Mohseni

# Load packages
library(ggplot2)
library(pwr)

# Global Variables
N <- 100 # number of possible hypotheses
brPrediction <-
  0.9 # the base rate for true hypotheses selected via prediction
brHARKing <-
  0.1 # the base rate for true hypotheses selected via HARKing
alpha <- 0.05 # the type I error rate of statistical tests
d <-
  0.5 # the average effect size for difference of means in terms of Cohen's d
n <- 30 # the average sample size for studies

# We model the effect of changing the conventional threshold 
# for statistical signficance on the false discovery rate of a literature.

# A literature consists in a unite mass of studies.
# A study consists of gathering data against which a set of N logically independent hypotheses may be tested

# One descriptive protocol researchers may follow is: `fallback HARKing'
# In `fallback HARKING', prior to observing her data a researcher select a hypothesis for testing that she judges is most likely true H_1. Upon observing her data, if the researcher finds that her hypothesis H_1 is statistically significant then she reports it. If, however, she finds that her hypothesis H_1 is not statistically significant, she turns to her N-1 other hypotheses and reports one that is in fact significant, if one exists.

# Define a function to compute the false discovery rate (FDR) for each protocol
funFDR <- function(alpha, br) {
  # First, compute the expected Type II error for the test
  beta <- 1 - pwr.t.test(n = 30, d = d, sig.level = alpha)$power
  # If alpha = 0, there are no discoveries, true or false, so we add this conditional to avoid dividing by zero
  if (alpha != 0) {
    # The false discovery rate of a protocol is given by the fraction of false discoveries over all discoveries.
    x <-
      alpha * (1 - br) # false discoveries = Pr(significant | H_0) * Pr(H_0)
    y <-
      (1 - beta) * br # true discoveries = Pr(significant | H_1) * Pr(H_1)
    z <-
      x / (x + y) # false discovery rate = false discoveries / (false discoveries + true discoveries)
    return(z)
  } else {
    # If alpha = 0, there are no discoveries, true or false
    return(0) #
  }
}

# Define a function to compute the _net_ false discovery rate (FDR) for the literature
funFDRn <-
  function(alpha) {
    # First, compute the expected Type II error for the test
    beta <- 1 - pwr.t.test(n = 30, d = d, sig.level = alpha)$power
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

# Compute the net false discovery rate for various values of alpha
alphaVec <- seq(from = 0, to = 1, by = 0.01)
FDRp <- sapply(alphaVec, funFDR, br = brPrediction)
FDRh <- sapply(alphaVec, funFDR, br = brHARKing)
FDRn <- sapply(alphaVec, funFDRn)[1, ]
# Here we check if all of the outputs are behaving as expected
sVec <- sapply(alphaVec, funFDRn)[2, ]
print(sVec) # the fraction of findings contributed via prediction
rVec <- sapply(alphaVec, funFDRn)[3, ]
print(rVec) # the findings rate of HARking

# Produce the data frame of the false discovery rates to use in graphing
# dataLabels <- c("FDR(p)", "FDR(h)", "FDR(fh)")
dataLabels <- c("FDR(fh)", "FDR(p)")
# data <- c(FDRn, FDRh, FDRp)
data <- c(FDRn, FDRp)
group <- rep(dataLabels, each = length(alphaVec))
df <-
  data.frame(data = matrix(c(
    rep(alphaVec, times = length(dataLabels)), data, c(group)
  ), ncol = 3))
colnames(df) <- c("Alpha", "FDR", "Group")
df$Alpha <- as.numeric(as.character(df$Alpha))
df$FDR <- as.numeric(as.character(df$FDR))
df$Group <- factor(df$Group, dataLabels)

# Make a graph of the false discovery rates as a function of the signfinance threshold
G <- ggplot(df) +
  geom_line(
    data = df,
    size = 1,
    aes(
      x = Alpha,
      y = FDR,
      linetype = group,
      color = group
    ),
    alpha = 1
  ) +
  theme_minimal() +
  ggtitle("") +
  labs(x = expression(paste("Significance Threshold ", alpha)), y = "False Discovery Rate") +
  scale_linetype_manual(
    values = c("solid", "solid"),
    labels = c("Fallback \nHARKing", "Prediction")
  ) +
  scale_color_manual(
    values = c("orangered2", "black"),
    labels = c("Fallback \nHARKing", "Prediction")
  ) +
  scale_x_continuous(limits = c(0, 1)) +
  # scale_y_continuous(limits = c(0, 0.3)) +
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