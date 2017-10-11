library("plyr")
setwd("/Users/Teresa/Documents/R/Recruitment")

apps <- read.csv("Applicants.csv", header = T, sep=",",
                stringsAsFactors = F)

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

# Chi square to explore gender on shortlisting 
gen.short.tab <- table(apps$Gender, apps$ShortlistedNY)
rownames(gen.short.tab)<-c('Male','Female')
colnames(gen.short.tab)<-c('No','Yes')
gen.short.tab
chisq.test(gen.short.tab)

# Chi square to explore BAME on shortlisting 
bame.short.tab <- table(apps$BAMEyn, apps$ShortlistedNY)
rownames(bame.short.tab)<-c('Male','Female')
colnames(bame.short.tab)<-c('No','Yes')
bame.short.tab
chisq.test(bame.short.tab)

# Combine gender and bame to predict shortlisting





