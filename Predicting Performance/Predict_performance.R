library("plyr")
library("BaylorEdPsych")
library("QuantPsyc")
setwd("/Users/Teresa/Documents/R/Predicting Performance")

perf <- read.csv("performance_combination.csv", header = T, sep=",",
                 stringsAsFactors = F)

# Frequencies statistics
apply(perf,2, table)

# Any missing values?
apply(is.na(perf),2,sum)

# var_prop <- perf %>% 
#   count(Gender) %>%  # group_by() & summarise(n = n()) are implicit
#   mutate(prnt = prop.table(n))    # prop = n/sum(n) works too
# as.data.frame(var_prop) 

perf.lm <- with(perf, lm(Year1performanceRating ~ Gender + EducationHighest + BAMEYN +
              WorkExperience + ACPersonalityO + ACPersonalityC + 
              ACPersonalityE + ACPersonalityA + ACPersonalityN + 
              ACRatingINTCOMPA + ACRatingINTCOMPB + ACRatingINTCOMPC +
              ACRatingINTCOMPD + ACRatingINTCOMPE + ACRatingAPTnumerical +
              ACRatingAPTverbal + InductionDay + InductionWeek + 
              OnBoardingBuddy + FinanceDummyV + MarketingDummyV +
              SalesDummyV + RiskDummyV + LegalDummyV + OperationsDummyV))

summary(perf.lm)

lm.beta(perf.lm)




