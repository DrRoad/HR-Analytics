library("QuantPsyc")
setwd("/Users/Teresa/Documents/R/Engagement levels and prediction")
empSurvey <- read.csv("Engagement and team level data.csv", 
                     header = T, sep=",",
                     stringsAsFactors = F)

###########################################################
# Independent samples T-test
# Independent variable(categorical)- function and LondonorNot
# dependent variable (continuous) - EMPsurvEngagement
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
eng.lm <- lm(EMPsurvEngagement ~ Function + GroupSize + PercentMale + BAME +
             EmpSurvOrgIntegrity + EmpSurvSupervisor, data=empSurvey)

summary(eng.lm)

# standardized beta coefficient - to know which variable has bigger impact
# The higher the absolute value of the beta coefficient, the stronger the effect.
lm.beta(eng.lm)









