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

# Get BAME samples
S <- emp.data$Function == 1
sales.bame <- emp.data[S,]$BAME
prof.bame <- emp.data[!S,]$BAME

# t-test of BAME percentage between sales and prof functions
t.test(sales.bame, prof.bame)

# Get percentMale samples
sales.male <- emp.data[S,]$PercentMale
prof.male <- emp.data[!S,]$PercentMale

# t-test of male percentage between sales and prof functions
t.test(sales.male, prof.male)








