library(psych)
library("factoextra")
setwd("/Users/Teresa/Documents/R/Factor testing at group level")
# read data
survey.data <- read.csv("Survey Result Team Level.csv", 
                        header = T, sep=",")
head(survey.data)

# Run a PCA (Primary Component Analysis) to determine the number of the factors.
# Those eigenvalues above 1 can be factors
survey.pca <- prcomp(survey.data, scale = TRUE)
eig.val <- get_eigenvalue(survey.pca)
eig.val


##############################################
# Based on the engenvalues, three components exist
# factanal requires the dataset: survey.data
# factor = 3
# rotation = varimax <-- default setting
##############################################

survey.fa <- factanal(survey.data, factors = 3, rotation = "varimax", 
                      scores = "Bartlett")
survey.fa

# Reliability test
ocb.survey <- cbind(survey.data$ocb1, survey.data$ocb2,survey.data$ocb3,
                    survey.data$ocb4)
colnames(ocb.survey)<- c('Ocb1', 'Ocb2', 'Ocb3', 'Ocb4')

alpha(ocb.survey)

install.packages("factoextra")
