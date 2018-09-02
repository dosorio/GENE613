# GENE613 - Homework 1
# Daniel Osorio - dcosorioh@tamu.edu
# Department of Veterinary Medicine and Biomedical Sciences
# Texas A&M University

# Read the data
hw1Data <- read.delim("../data/hw1data18.txt",stringsAsFactors = FALSE)

# 1. Give the minimum, the maximum, the mean, the variance, the standard deviation, 
# and the coefficient of variation for the ADG and DDMI.
hw1pt1 <- function(data) {
  return(c(
    min = min(data),
    max = max(data),
    mean = mean(data),
    var = var(data),
    stdDev = sd(data),
    CV = (sd(data) / mean(data))
  ))
}
apply(hw1Data[, 3:4], 2, hw1pt1)

# 2. Calculate the covariance of ADG and DDMI, the correlation of ADG and DDMI,
# the regression of ADG on DDMI, and the regression of DDMI on ADG, and explain
# the differences between these values
hw1pt2 <- function(data) {
  return(c(
    covariance = with(data, cov(ADG, DDMI)),
    correlation = with(data, cor(ADG, DDMI)),
    b1_ADGonDDMI = (with(data, lm(ADG ~ DDMI))[[1]][[2]]),
    b1_DDMIonADG = (with(data, lm(DDMI ~ ADG))[[1]][[2]])
  ))
}
hw1pt2(hw1Data)

# Execute the following code (you may need to modify the trait names to the right
# of the <- depending upon how you input your data) in R
no3.adg.ddmi <-  with(hw1Data, lm(ADG~DDMI))
no3.ddmi.adg <-  with(hw1Data, lm(DDMI~ADG))
summary(no3.adg.ddmi)
summary(no3.ddmi.adg)

# Calculate means and SD for sires. If you wanted to increase ADG, which sire would
# you choose to be a parent in the next generation? If you wanted to decrease DDMI,
# which sire should you choose?
sireInformation <- function(sireID){
  selectedSire <- hw1Data[with(hw1Data, sire %in% sireID),]
  c(MEAN=apply(selectedSire[,3:4],2,mean),
  SD=apply(selectedSire[,3:4],2,sd))
}
sapply(unique(hw1Data$sire),sireInformation)
