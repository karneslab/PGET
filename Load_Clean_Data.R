# load packages
library(tidyverse)
library(ggbeeswarm)
library(janitor)
library(readxl)
library(epitools)
library(pastecs)
library(reshape2)
library(rmarkdown)
library(olsrr)
library(reshape2)
library(ggsci)

#load data
student_join <- read.csv("C:\\Users\\Chloe\\Desktop\\StudentData.csv")

# Clean and Factor Data --------

cleanstudentdata <- clean_names(student_join) 

factorvars=c("id",
             "case_x",
             "year_x",
             "sex",
             "personalexp",
             "campus",
             "actionable_genotypes_highly")

cleanstudentdata[factorvars] = lapply(cleanstudentdata[factorvars],factor)

#identify column numbers 
match("q2post",names(cleanstudentdata))

#find total pre and post scores on knowledge quizzes 
cleanstudentdata$preknowsum <- rowSums(cleanstudentdata[ , c(157,158,159,160,161,162,163,164,165,166,174)])
cleanstudentdata$postknowsum <- rowSums(cleanstudentdata[ , c(138,139,140,141,142,143,144,145,146,147,175)])

#difference in score on knowledge quizzes   
cleanstudentdata$know_diff <- cleanstudentdata$postknowsum - cleanstudentdata$preknowsum


#### seperate by case and control ---------

casedata <- cleanstudentdata %>%
  filter(case_x==1)

controldata <- cleanstudentdata %>%
  filter(case_x==0)

