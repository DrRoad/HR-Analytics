setwd("/Users/Teresa/Documents/R/Engagement levels and prediction")
empSurvey <- read.csv("Engagement and team level data.csv", 
                     header = T, sep=",",
                     stringsAsFactors = F)

###########################################################
# Independent samples T-test
# Independent variable(categorical)- department functions: 
# sales (function 1) or professional (function 2)
# dependent variable (continuous) - BAME percentage
###########################################################

# independent t-test for LondonorNot vs engegement
ldn <- empSurvey$LondonorNot == 1
ldn.eng <- empSurvey[ldn,]$EMPsurvEngagement
noLdn.eng <- empSurvey[!ldn,]$EMPsurvEngagement
t.test(ldn.eng, noLdn.eng)

# independent t-test for Function vs engegement
s <- empSurvey$Function == 1
sales.eng <- empSurvey[s,]$EMPsurvEngagement
prof.eng <- empSurvey[!s,]$EMPsurvEngagement
t.test(sales.eng, prof.eng)

# using multiple regression to predict team-level engagement









