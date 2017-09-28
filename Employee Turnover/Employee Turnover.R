library("QuantPsyc")
library("car")
library("multcomp")
library("userfriendlyscience")
library("BaylorEdPsych")
library("metafor")
setwd("/Users/Teresa/Documents/R/Employee Turnover")

# Turnover at the individual level
# Turnover value is a catagorical data

emp <- read.csv("Individual Turnover.csv", header = T, sep=",",
                stringsAsFactors = F)

# Create a frequency table and explore
emp.table <- table(emp$Country, emp$LeaverStatus)

rownames(emp.table) <- c('Belgium','Sweden','Italy','France','Poland',
                         'Mexico', 'Spain','UK','United States','Australia')
colnames(emp.table)<- c('Stayer','Leaver')

# overall turnover percentage
turnover.pcnt <- round(sum(emp.table[,2]) / sum((emp.table[,1] + 
                                                   emp.table[,2])),3)
turnover.pcnt

# Chi square to explore regional differences in individual staff turnover
chisq.test(emp.table, simulate.p.value = TRUE)

# There is no significant difference between what we would expect in each region
# and what was observed. 

#######################################################
# Turnover rate (TeamSeparation) is a continuous data. 
# Use Levene's Test and one-way Anova to test

teamTurnOver <- read.csv("Team Turnover.csv", header = T, sep=",")
teamTurnOver <- within(teamTurnOver, {
      Country <- factor(Country, 
                        labels=c('UK','United States','Canada','Spain')) 
      })

# Lavene's Test for turnover rate
leveneTest(TeamSeparation ~ Country, data=teamTurnOver, center="median")

# Lavene's Test for Engagement rate
leveneTest(Engagement ~ Country, data=teamTurnOver, center="median")

# Because both results are significant, it means that the variances are not
# equally distributed across the countries. 

# Next step is to use one way Anova with Welch F-test not assuming equal 
# variance.
# Turnover rate
turnover.oneway <- oneway.test(TeamSeparation ~ Country, data=teamTurnOver) 
#turnover.aov <- aov(TeamSeparation ~ Country, data=teamTurnOver)
turnover.oneway

# Employee Engagement rate
oneway.test(Engagement ~ Country, data=teamTurnOver) # Welch test
#eng.aov <- aov(Engagement ~ Country, data=teamTurnOver)

# To identify which countries differ, we need to look at the post-hoc tests.
# method 1: userfriendly package - posthocTGH. It allows Games-Howell method
# which is used when the variance is not spreaded equally.

posthocTGH(teamTurnOver$TeamSeparation, teamTurnOver$Country,
           method = "games-howell", # or Tukey
           #conf.level = 0.95, 
           digits=3, 
           formatPvalue = TRUE)
posthocTGH(teamTurnOver$Engagement, teamTurnOver$Country,
           method = "games-howell",
           digits=3, 
           formatPvalue = TRUE)

# method 2 - Tukey using glht()'s Tukey use it when there is an equal variance
#Turnover.postHocs <-glht( turnover.aov, linfct = mcp( Country = "Tukey")) 
#summary( Turnover.postHocs) 
#confint( Turnover.postHocs)

#Eng.postHocs <-glht( eng.aov, linfct = mcp( Country = "Tukey")) 
#summary( Eng.postHocs) 
#confint( Eng.postHocs)

##########################################################################
##########################################################################

# Logistic regression to predict individual turnover. Use emp dataset.
# Make Country a factor
emp <- within(emp, {
  Country <- factor(Country, 
                    labels=c('Belgium','Sweden','Italy','France', 'Poland',
                             'Mexico', 'Spain', 'UK', 'US','Australia')) 
})
logit.ind.tnvr <- glm(LeaverStatus ~.,family=binomial(link='logit'),data=emp)
summary(logit.ind.tnvr)

# Pseudo R-square values
PseudoR2(logit.ind.tnvr)

# Odds ratio
exp(logit.ind.tnvr$coefficients)






