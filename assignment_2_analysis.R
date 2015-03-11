# Assignment 2 dataset #
# set working directory #
rm(list=ls())


setwd("/Users/Chris/Desktop/PSU_Class_Documents/2014_Fall_Semester_Classes/STAT_557_Data_Mining/assignment_2")
uciPort <- read.table("Wholesale_data.csv", sep=",", header=TRUE)
head(uciPort)
summary(uciPort)
# check if region is factor
names(uciPort)




