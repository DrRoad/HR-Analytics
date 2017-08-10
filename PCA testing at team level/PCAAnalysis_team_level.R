library(psych)
library("factoextra")
setwd("/Users/Teresa/Documents/R/PCA testing at team level")
# read data
survey.data <- read.csv("Survey Result Team Level.csv", 
                        header = T, sep=",")
head(survey.data)

# Run a PCA (Principal Component Analysis) to determine the number of the factors.
# Those eigenvalues above 1 can be the factors
survey.pca <- prcomp(survey.data, scale = TRUE)
eig.val <- get_eigenvalue(survey.pca)
eig.val


#eigenvalue variance.percent cumulative.variance.percent
#Dim.1 4.5066490048     50.073877831                    50.07388
#Dim.2 1.6167586979     17.963985532                    68.03786
#Dim.3 1.3701905868     15.224339853                    83.26220
#Dim.4 0.6199287958      6.888097731                    90.15030
#Dim.5 0.4247478977      4.719421086                    94.86972
#Dim.6 0.2090177040      2.322418934                    97.19214
#Dim.7 0.1680957428      1.867730476                    99.05987
#Dim.8 0.0841990287      0.935544763                    99.99542
#Dim.9 0.0004125415      0.004583794                   100.00000

##############################################
# Based on the engenvalues, three components exist
# factor = 3
# rotation = varimax <-- default setting
##############################################
fit <- principal(survey.data, nfactors=3, rotate="varimax")
fit

# Reliability test
psych::alpha(survey.data)

