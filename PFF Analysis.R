###Setup

library("readxl")
library("dplyr")
library("plyr")
library("xlsx")
library("rlist")


###Read offensive files
setwd("C:/Users/Patrick Holbrook/Documents/PFF Data")
o1 = read.csv("receiving_summary-2010.csv")
o2 = read.csv("receiving_summary-2011.csv")
o3 = read.csv("receiving_summary-2012.csv")
o4 = read.csv("receiving_summary-2013.csv")
o5 = read.csv("receiving_summary-2014.csv")
o6 = read.csv("receiving_summary-2015.csv")
o7 = read.csv("receiving_summary-2016.csv")
o8 = read.csv("receiving_summary-2017.csv")
o9 = read.csv("receiving_summary-2018.csv")
o10 = read.csv("receiving_summary-2019.csv")

###Add datasets vertically into one file
All_DATA <- rbind(o1,o2,o3,o4,o5,o6,o7,o8,o9,o10)

###PFF Score on TDs 

TDS <- lm("grades_offense ~ touchdowns", data = subset(All_DATA, position=="WR"))
summary(TDS)

### PFF score on targets

targets <- lm("grades_offense ~ targets", data = subset(All_DATA, position=="WR"))
summary(targets)

###PFF Score on receptions

receptions <- lm("grades_offense ~ receptions", data = subset(All_DATA, position=="WR"))
summary(receptions)

###PFF Score on Yards_per_reception

yards_per <- lm("grades_offense ~ yards_per_reception", data = subset(All_DATA, position=="WR"))
summary(yards_per)

###PFF Score on caught_percent

caught_percent <- lm("grades_offense ~ caught_percent", data = subset(All_DATA, position=="WR"))
summary(caught_percent)

###PFF score on total receiving yards
total_yards <- lm("grades_offense ~ yards", data = subset(All_DATA, position=="WR"))
summary(total_yards)

###PFF score on all the above

All_variables <- lm("grades_offense ~ touchdowns + targets + receptions + yards_per_reception + caught_percent + yards", data = subset(All_DATA, position=="WR"))
summary(All_variables)

##For fun - PFF score on QB rating

QB_Rating <- lm("grades_offense ~ targeted_qb_rating", data = subset(All_DATA, position =="WR"))
summary(QB_Rating)

###Big regression with QB rating

All_variables_qb <- lm("grades_offense ~ touchdowns + targets + receptions + yards_per_reception + caught_percent + yards + targeted_qb_rating", data = subset(All_DATA, position=="WR"))
summary(All_variables_qb)


###PFF backwards aka does PFF predict something?

PFF_touchdowns <- lm("touchdowns ~ grades_offense", data = subset(All_DATA, position =="WR"))
summary(PFF_touchdowns)

PFF_targets <- lm("targets ~ grades_offense", data = subset(All_DATA, position =="WR"))
summary(PFF_targets)

PFF_receptions <- lm("receptions ~ grades_offense", data = subset(All_DATA, position =="WR"))
summary(PFF_receptions)

PFF_caught_percent <- lm("caught_percent ~ grades_offense", data = subset(All_DATA, position =="WR"))
summary(PFF_caught_percent)

PFF_yards <- lm("yards ~ grades_offense", data = subset(All_DATA, position =="WR"))
summary(PFF_yards)

###How well does PFF predict salary?
SalaryData <- read.csv("2019 Salary_vR.csv")

No_lag <- lm("Salary ~ X2019.PFF", data = SalaryData)
summary(No_lag)

lag_2018 <-lm("Salary ~ X2019.PFF + X2018.PFF", data = SalaryData)
summary(lag_2018)

lag_2017 <-lm("Salary ~ X2019.PFF + X2018.PFF + X2017.PFF", data = SalaryData)
summary(lag_2017)

lag_2016 <-lm("Salary ~ X2019.PFF + X2018.PFF + X2017.PFF + X2016.PFF", data = SalaryData)
summary(lag_2016)

lag_exclude_2019 <-lm("Salary ~ X2018.PFF + X2017.PFF + X2016.PFF", data = SalaryData)
summary(lag_exclude_2019)

#Plot PFF against salary

#PFF predicting next year's PFF
PFF_lag_1 <- lm("X2019.PFF ~ X2018.PFF", data = SalaryData)
summary(PFF_lag_1)

PFF_lag_2 <- lm("X2019.PFF ~ X2018.PFF + X2017.PFF", data = SalaryData)
summary(PFF_lag_2)

PFF_lag_3 <- lm("X2019.PFF ~ X2018.PFF + X2017.PFF + X2016.PFF", data = SalaryData)
summary(PFF_lag_3)

##Load Fernando PFF Data

by_year <- read.csv("PFF_byyear_2.csv")

### PFF by year in league for players that played all 4 years

by_year_ALL <- lm("PFF4 ~ PFF1 + PFF2 + PFF3", data = subset(by_year, "Played all four?" =1))
summary(by_year_ALL)

by_year_1 <- lm("PFF4 ~ PFF1", data = subset(by_year, "Played all four?" = 1))
summary(by_year_1)

year1on2 <- lm("PFF2 ~ PFF1", data = subset(by_year, "Played all four?" = 1))
summary(year1on2)

year2on3 <- lm("PFF3 ~ PFF2", data = subset(by_year, "Played all four?" = 1))
summary(year2on3)

year3on4 <- lm("PFF4 ~ PFF3", data = subset(by_year, "Played all four?" = 1))
summary(year3on4)

year2on4 <- lm("PFF4 ~ PFF2", data = subset(by_year, "Played all four?" = 1))
summary(year2on4)

### PFF by year in league for players that didn't play all 4 years

xby_year_ALL <- lm("PFF4 ~ PFF1 + PFF2 + PFF3", data = subset(by_year, "Played all four?" =0))
summary(xby_year_ALL)

xby_year_1 <- lm("PFF4 ~ PFF1", data = subset(by_year, "Played all four?" = 0))
summary(xby_year_1)

xyear1on2 <- lm("PFF2 ~ PFF1", data = subset(by_year, "Played all four?" = 0))
summary(xyear1on2)

xyear2on3 <- lm("PFF3 ~ PFF2", data = subset(by_year, "Played all four?" = 0))
summary(xyear2on3)

xyear3on4 <- lm("PFF4 ~ PFF3", data = subset(by_year, "Played all four?" = 0))
summary(xyear3on4)

xyear2on4 <- lm("PFF4 ~ PFF2", data = subset(by_year, "Played all four?" = 0))
summary(xyear2on4)

##Difference in year 1 PFF whether or not you made it 4 years

pff1_diff <- lm("PFF1 ~ Played.all.four.", data = by_year)
summary(pff1_diff)

pff2_diff <- lm("PFF2 ~ Played.all.four.", data = by_year)
summary(pff2_diff)

