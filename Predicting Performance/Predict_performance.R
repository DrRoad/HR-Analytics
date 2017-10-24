library("plyr")
library("BaylorEdPsych")
setwd("/Users/Teresa/Documents/R/Predicting Performance")

perf <- read.csv("performance_combination.csv", header = T, sep=",",
                 stringsAsFactors = F)

# Predicting year one performance 









# Create frequency tables and explore the variables
gender <- count(apps, 'Gender')
gender
prop.table(table(apps$Gender))

BAMEyn <- count(apps, 'BAMEyn')
BAMEyn
prop.table(table(apps$BAMEyn))

ShortlistedNY <- count(apps, 'ShortlistedNY')
ShortlistedNY
prop.table(table(apps$ShortlistedNY))

Interviewed <- count(apps, 'Interviewed')
Interviewed
prop.table(table(apps$Interviewed))

FemaleONpanel <- count(apps, 'FemaleONpanel')
FemaleONpanel
prop.table(table(apps$FemaleONpanel))

OfferNY <- count(apps, 'OfferNY')
OfferNY
prop.table(table(apps$OfferNY))

JoinYN <- count(apps, 'JoinYN')
JoinYN
prop.table(table(apps$JoinYN))

AcceptNY <- count(apps, 'AcceptNY')
AcceptNY
prop.table(table(apps$AcceptNY))

######################################################
## Gender and BAME with shortlisting
######################################################

# Chi square to explore gender on shortlisting 
gen.short.tab <- table(apps$Gender, apps$ShortlistedNY)
rownames(gen.short.tab)<-c('Male','Female')
colnames(gen.short.tab)<-c('Shortlisted No','Shortlisted Yes')
gen.short.tab
chisq.test(gen.short.tab)

# Chi square to explore BAME on shortlisting 
bame.short.tab <- table(apps$BAMEyn, apps$ShortlistedNY)
rownames(bame.short.tab)<-c('BAME-Yes','BAME-No')
colnames(bame.short.tab)<-c('Shortlisted-No','Shortlisted-Yes')
bame.short.tab
chisq.test(bame.short.tab)

# Combine gender and bame to predict shortlisting
logit.short <- glm(ShortlistedNY ~ BAMEyn + Gender,
                   family=binomial(link='logit'),data=apps)
summary(logit.short)

# Pseudo R-square values
PseudoR2(logit.short)

# Odds ratio
exp(logit.short$coefficients)

######################################################
## Gender and BAME with Offers made
######################################################
# Chi square to explore gender with offers made 
gen.offer.tab <- table(apps$Gender, apps$OfferNY)
rownames(gen.offer.tab)<-c('Male','Female')
colnames(gen.offer.tab)<-c('No','Yes')
gen.offer.tab
chisq.test(gen.offer.tab)

# Chi square to explore BAME with offers made 
bame.offer.tab <- table(apps$BAMEyn, apps$OfferNY)
rownames(bame.offer.tab)<-c('Male','Female')
colnames(bame.offer.tab)<-c('No','Yes')
bame.offer.tab
chisq.test(bame.offer.tab)




