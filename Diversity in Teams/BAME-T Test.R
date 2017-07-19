setwd("/Users/Teresa/Documents/R/Diversity in Teams")
emp.data <- read.csv("emp_diversity_data.csv", 
                     header = T, sep=",",
                     stringsAsFactors = F)

# Descriptive data for the percentage of male workers  
# The column is percentage but the data hasn't been divided by 100 yet.
summary(emp.data$PercentMale/100)
# Min.    1st Qu. Median  Mean    3rd Qu.  Max.    NA's 
# 0.0100  0.4500  0.6500  0.5895  0.6700  1.0000       1

# Get PercentMale std deviation - 0.222
sd(emp.data$PercentMale/100, na.rm=T)

# Descriptive data for BAME (Black, Asian, or minority ethnic)  
# The data is already in percetage form.
summary(emp.data$BAME)
# Min.    1st Qu. Median  Mean    3rd Qu.  Max.    NA's 
# 0.0000  0.0300  0.0900  0.1184  0.2000  0.4500     182

# Get BAME std deviation - 0.113
sd(emp.data$BAME, na.rm=T)

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








