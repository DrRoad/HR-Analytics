
library(ggplot2)
setwd("/Users/Teresa/Documents/R/Chi Square Testing")
emp.data <- read.csv("diversity.csv", 
                        header = T, sep=",",
                        stringsAsFactors = F)
# Create a summary table
emp.table <- table(emp.data)

colnames(emp.table) <- c("Clerical / Officer","Administrator / Assistant",
                         "Graduate / Trainee Consultant","Consultant",
                         "Senior Consultant","Managing Consultant",
                         "Principal Consultant","Partner")
rownames(emp.table) <- c("Female","Male")

# explore 
mosaicplot(emp.table, main=" ", xlab = "Job Role", ylab = "Gender", las = 2,
           color="skyblue2", border = "chocolate", off = 30, dir = c("h","v"),
           sort = c(2,1))

# use chi square to verify the association between gender and job grade
chisq.test(emp.table)
