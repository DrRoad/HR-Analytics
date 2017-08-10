library(psych)
setwd("/Users/Teresa/Documents/R/Employee attitude surveys")
# read data
survey.data <- read.csv("Employee attitude survey result.csv", 
                        header = T, sep=",")
head(survey.data)

# get the questions we want to test: 
# employee engagement and perceived organizational support (POS)

selected.survey <- cbind(survey.data$Eng1, survey.data$Eng2, survey.data$Eng3, 
                         survey.data$Eng4, survey.data$pos1, survey.data$pos2,
                         survey.data$pos3)
colnames(selected.survey) <- c('Eng1', 'Eng2', 'Eng3', 'Eng4', 'Pos1', 'Pos2',
                               'Pos3')

# Run a PCA (Principal Component Analysis) to determine the number of the factors.
selected.survey.pca <- princomp(na.omit(selected.survey))
summary(selected.survey.pca)
plot(selected.survey.pca)

##############################################
# Based on the summary, two components exist
# factor = 2
# rotation = varimax <-- default setting
##############################################

selected.survey.pr <- principal(na.omit(selected.survey), nfactors = 2, 
                               rotate = "varimax")
selected.survey.pr

# Reliability test
ocb.survey <- cbind(survey.data$ocb1, survey.data$ocb2,survey.data$ocb3,
                    survey.data$ocb4)
colnames(ocb.survey)<- c('Ocb1', 'Ocb2', 'Ocb3', 'Ocb4')

psych::alpha(ocb.survey)


