###Setup

library("readxl")
library("dplyr")
library(ggplot2)
library("plyr")
library("xlsx")
library("rlist")
setwd("~/Booth/Sports Analytics/Project/Data/PFF Data-20200509T180454Z-001/PFF Data")
rm(list=ls())

###Read offensive files
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

#Add season column
o1$season <- 2010
o2$season <- 2011
o3$season <- 2012
o4$season <- 2013
o5$season <- 2014
o6$season <- 2015
o7$season <- 2016
o8$season <- 2017
o9$season <- 2018
o10$season <- 2019

###Read Salary Data
SalaryData = read.csv("Salary.csv")

### Filter Salary Data by Year
SalaryData_2010 <- SalaryData %>% filter(ï..season == 2010)
SalaryData_2011 <- SalaryData %>% filter(ï..season == 2011)
SalaryData_2012 <- SalaryData %>% filter(ï..season == 2012)
SalaryData_2013 <- SalaryData %>% filter(ï..season == 2013)
SalaryData_2014 <- SalaryData %>% filter(ï..season == 2014)
SalaryData_2015 <- SalaryData %>% filter(ï..season == 2015)
SalaryData_2016 <- SalaryData %>% filter(ï..season == 2016)
SalaryData_2017 <- SalaryData %>% filter(ï..season == 2017)
SalaryData_2018 <- SalaryData %>% filter(ï..season == 2018)
SalaryData_2019 <- SalaryData %>% filter(ï..season == 2019)
SalaryData_2020 <- SalaryData %>% filter(ï..season == 2020)

### Merge Salary Data with Performance Data
o1 <- merge(o1, SalaryData_2010, by="player", all.x = TRUE)
o2 <- merge(o2, SalaryData_2011, by="player", all.x = TRUE)
o3 <- merge(o3, SalaryData_2012, by="player", all.x = TRUE)
o4 <- merge(o4, SalaryData_2013, by="player", all.x = TRUE)
o5 <- merge(o5, SalaryData_2014, by="player", all.x = TRUE)
o6 <- merge(o6, SalaryData_2015, by="player", all.x = TRUE)
o7 <- merge(o7, SalaryData_2016, by="player", all.x = TRUE)
o8 <- merge(o8, SalaryData_2017, by="player", all.x = TRUE)
o9 <- merge(o9, SalaryData_2018, by="player", all.x = TRUE)
o10 <- merge(o10, SalaryData_2019, by="player", all.x = TRUE)

###Read allowed pressure files
a1 = read.csv("allowed_pressures-2010.csv")
a2 = read.csv("allowed_pressures-2011.csv")
a3 = read.csv("allowed_pressures-2012.csv")
a4 = read.csv("allowed_pressures-2013.csv")
a5 = read.csv("allowed_pressures-2014.csv")
a6 = read.csv("allowed_pressures-2015.csv")
a7 = read.csv("allowed_pressures-2016.csv")
a8 = read.csv("allowed_pressures-2017.csv")
a9 = read.csv("allowed_pressures-2018.csv")
a10 = read.csv("allowed_pressures-2019.csv")

o1 <- merge(o1, a1[,c(2,6)], by="player_id", all.x = TRUE)
o2 <- merge(o2, a2[,c(2,6)], by="player_id", all.x = TRUE)
o3 <- merge(o3, a3[,c(2,6)], by="player_id", all.x = TRUE)
o4 <- merge(o4, a4[,c(2,6)], by="player_id", all.x = TRUE)
o5 <- merge(o5, a5[,c(2,6)], by="player_id", all.x = TRUE)
o6 <- merge(o6, a6[,c(2,6)], by="player_id", all.x = TRUE)
o7 <- merge(o7, a7[,c(2,6)], by="player_id", all.x = TRUE)
o8 <- merge(o8, a8[,c(2,6)], by="player_id", all.x = TRUE)
o9 <- merge(o9, a9[,c(2,6)], by="player_id", all.x = TRUE)
o10 <- merge(o10, a10[,c(2,6)], by="player_id", all.x = TRUE)

###Add datasets vertically into one file
All_DATA <- rbind(o1,o2,o3,o4,o5,o6,o7,o8,o9,o10)
# All_DATA$Cap.Number <- as.numeric(as.character(All_DATA$Cap.Number))
# All_DATA$Cash.Spent <- as.numeric(as.character(All_DATA$Cash.Spent))

###Read Draft Data
Draft_Data = read.csv("Draft Data.csv")
colnames(Draft_Data)[5] <- "player"


library(plyr)
All_DATA <- merge(All_DATA, Draft_Data[,c(5,2)], by="player", all.x = TRUE)
All_DATA$experience <- All_DATA$season - All_DATA$Year + 1
WR_Data <- All_DATA %>% filter(position == "WR")
WR_Data <- All_DATA %>% filter(experience > -1)

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
Patrick_Salary <- read.csv("2019 Salary_vR.csv")

No_lag <- lm("Salary ~ X2019.PFF", data = Patrick_Salary)
summary(No_lag)

lag_2018 <-lm("Salary ~ X2019.PFF + X2018.PFF", data = Patrick_Salary)
summary(lag_2018)

lag_2017 <-lm("Salary ~ X2019.PFF + X2018.PFF + X2017.PFF", data = Patrick_Salary)
summary(lag_2017)

lag_2016 <-lm("Salary ~ X2019.PFF + X2018.PFF + X2017.PFF + X2016.PFF", data = Patrick_Salary)
summary(lag_2016)

lag_exclude_2019 <-lm("Salary ~ X2018.PFF + X2017.PFF + X2016.PFF", data = Patrick_Salary)
summary(lag_exclude_2019)

#Plot PFF distribution
WR_Data$season <- as.factor(WR_Data$season)
WR_Data$experience <- as.factor(WR_Data$experience)

write.csv(WR_Data, 'wr_data.csv')

mean_pff <- mean(WR_Data$grades_offense, na.rm=TRUE)
mean_snaps <- mean(WR_Data$snap_counts_offense, na.rm=TRUE)

#Snapcount Histogram
s <-ggplot(WR_Data, aes(x=snap_counts_offense)) + 
  geom_histogram(aes(y=..density..), binwidth=10, colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")

s + geom_vline(aes(xintercept=mean_snaps),
               color="blue", linetype="dashed", size=1)+
  ggtitle("Distribution of Snapcounts for WRs (2010 - 2019)") +
  xlab("Snap Count") + ylab("Density")

# Basic histogram
p<-ggplot(WR_Data, aes(x=grades_offense)) + 
  geom_histogram(aes(y=..density..), binwidth=1, colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")

p + geom_vline(aes(xintercept=mean(grades_offense)),
              color="blue", linetype="dashed", size=1) +
    geom_text(aes(label=round(mean_pff,1),y=0,x=mean_pff),
            vjust=1.1,col='black',size=2) +
    ggtitle("Distribution of PFF for WRs (2010 - 2019)") +
    xlab("PFF") + ylab("Density")

# Boxplot by years
pff_seasons <- ggplot(WR_Data, aes(x=experience, y=grades_offense, color=experience)) + 
  geom_boxplot() 

means <- aggregate(grades_offense ~  experience, WR_Data, mean)
counts <- aggregate(grades_offense ~  experience, WR_Data, length)


pff_seasons + geom_text(data = means, aes(label = round(grades_offense,1), y = grades_offense + 1.5), size =2)  +
  ggtitle("Mean PFF by years of experience") +
  xlab("Years of Experience") + ylab("Mean PFF")

# Salary Regression vs PFF
install.packages("stargazer")
library(stargazer)
fits_salary_all <- list()

WR_Data$experience <- as.numeric(WR_Data$experience)
fits_salary_all$pff <- lm("Cash.Spent ~ grades_offense", data = WR_Data)

salary_scatter <- ggplot(data = subset(WR_Data, snap_counts_offense > 100), aes(x = grades_offense, y = Cash.Spent))
salary_scatter + geom_point(aes(color = Cash.Spent), size = 3) +
  scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme(legend.position = "right") + 
  geom_smooth(aes(color = Cash.Spent), method = lm, se = FALSE, fullrange = TRUE) +
  ggtitle("Salary ~ PFF") +
  xlab("PFF") + ylab("Salary")

# Salary Regression vs Performance Metrics (all)
fits_salary_all$touchdowns <- lm("Cash.Spent ~ touchdowns", data = WR_Data)
fits_salary_all$receptions <- lm("Cash.Spent ~ receptions", data = WR_Data)
fits_salary_all$yac <- lm("Cash.Spent ~ yards_after_catch", data = WR_Data)
stargazer(fits_salary_all, type="text", dep.var.labels="Salary" )

# Salary Regression vs Performance Metrics (subset 50 snaps)
fits_salary_subset <- list()
snap_threshold = 50

fits_salary_subset$pff <- lm("Cash.Spent ~ grades_offense", data = subset(WR_Data, snap_counts_offense > snap_threshold))
fits_salary_subset$touchdowns <- lm("Cash.Spent ~ touchdowns", data = subset(WR_Data, snap_counts_offense > snap_threshold))
fits_salary_subset$receptions <- lm("Cash.Spent ~ receptions", data = subset(WR_Data, snap_counts_offense > snap_threshold))
fits_salary_subset$yac <- lm("Cash.Spent ~ yards_after_catch", data = subset(WR_Data, snap_counts_offense > snap_threshold))
stargazer(fits_salary_subset, type="text", dep.var.labels="Salary" )

# Snaps Regression vs Performance Metrics (all)
fits_snaps <- list()
fits_snaps$pff <- lm("snap_counts_offense ~ grades_offense", data = WR_Data)
fits_snaps$touchdowns <- lm("snap_counts_offense ~ touchdowns", data = WR_Data)
fits_snaps$receptions <- lm("snap_counts_offense ~ receptions", data = WR_Data)
fits_snaps$yac <- lm("snap_counts_offense ~ yards_after_catch", data = WR_Data)
stargazer(fits_snaps, type="text", dep.var.labels="Snap Count" )

receptions_scatter <- ggplot(data = subset(WR_Data, snap_counts_offense > snap_threshold), aes(x = receptions, y = Cash.Spent))
receptions_scatter + geom_point(aes(color = Cash.Spent), size = 3) +
  scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme(legend.position = "right") + 
  geom_smooth(aes(color = Cash.Spent), method = lm, se = FALSE, fullrange = TRUE) +
  ggtitle("Salary ~ Receptions") +
  xlab("Receptions") + ylab("Salary")

pff_td_scatter <- ggplot(WR_Data, aes(x = touchdowns, y = grades_offense))
pff_td_scatter + geom_point(aes(color = grades_offense), size = 3) +
  scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme(legend.position = "right") + 
  geom_smooth(aes(color = grades_offense), method = lm, se = FALSE, fullrange = TRUE) +
  ggtitle("PFF ~ Touchdowns") +
  xlab("Touchdowns") + ylab("PFF")

#Predicting yr 5 salary based off yr 4 data
WR_Wrangled = read.csv("wr_wrangling.csv")
fits_salary_y5 <- list()
fits_salary_y5$pff <- lm("y5_salary ~ y4_pff", data = WR_Wrangled)
fits_salary_y5$touchdowns <- lm("y5_salary ~ y4_touchdowns", data = WR_Wrangled)
fits_salary_y5$receptions <- lm("y5_salary ~ y4_receptions", data = WR_Wrangled)
fits_salary_y5$yac <- lm("y5_salary ~ y4_yac", data = WR_Wrangled)
stargazer(fits_salary_y5, type="text", dep.var.labels="Y5 Salary" )

#Predicting yr 5 snaps based off yr 4 data
WR_Wrangled = read.csv("wr_wrangling.csv")
fits_snaps_y5 <- list()
fits_snaps_y5$pff <- lm("y5_snaps ~ y4_pff", data = WR_Wrangled)
fits_snaps_y5$touchdowns <- lm("y5_snaps ~ y4_touchdowns", data = WR_Wrangled)
fits_snaps_y5$receptions <- lm("y5_snaps ~ y4_receptions", data = WR_Wrangled)
fits_snaps_y5$yac <- lm("y5_snaps ~ y4_yac", data = WR_Wrangled)
stargazer(fits_snaps_y5, type="text", dep.var.labels="Y5 Snaps" )

#Predicting yr 5 salary based off yr 3 data
fits_salary_y5 <- list()
fits_salary_y5$pff <- lm("y5_salary ~ y3_pff", data = WR_Wrangled)
fits_salary_y5$touchdowns <- lm("y5_salary ~ y3_touchdowns", data = WR_Wrangled)
fits_salary_y5$receptions <- lm("y5_salary ~ y3_receptions", data = WR_Wrangled)
fits_salary_y5$yac <- lm("y5_salary ~ y3_yac", data = WR_Wrangled)
stargazer(fits_salary_y5, type="text", dep.var.labels="Y5 Salary" )


#Predicting yr 5 salary based off yr 2 data
fits_salary_y5 <- list()
fits_salary_y5$pff <- lm("y5_salary ~ y2_pff", data = WR_Wrangled)
fits_salary_y5$touchdowns <- lm("y5_salary ~ y2_touchdowns", data = WR_Wrangled)
fits_salary_y5$receptions <- lm("y5_salary ~ y2_receptions", data = WR_Wrangled)
fits_salary_y5$yac <- lm("y5_salary ~ y2_yac", data = WR_Wrangled)
stargazer(fits_salary_y5, type="text", dep.var.labels="Y5 Salary" )

#Predicting yr 5 salary based off yr 1 data
fits_salary_y5 <- list()
fits_salary_y5$pff <- lm("y5_salary ~ y1_pff", data = WR_Wrangled)
fits_salary_y5$touchdowns <- lm("y5_salary ~ y1_touchdowns", data = WR_Wrangled)
fits_salary_y5$receptions <- lm("y5_salary ~ y1_receptions", data = WR_Wrangled)
fits_salary_y5$yac <- lm("y5_salary ~ y1_yac", data = WR_Wrangled)
stargazer(fits_salary_y5, type="text", dep.var.labels="Y5 Salary" )

#Predicting next year salary based off previous year metrics
WR_Wrangled_Lag = read.csv("wr_wrangling_lag.csv")
fits_salary_lag <- list()
fits_salary_lag$pff <- lm("y2_salary ~ y1_pff", data = WR_Wrangled_Lag)
fits_salary_lag$touchdowns <- lm("y2_salary ~ y1_touchdowns", data = WR_Wrangled_Lag)
fits_salary_lag$receptions <- lm("y2_salary ~ y1_receptions", data = WR_Wrangled_Lag)
fits_salary_lag$yac <- lm("y2_salary ~ y1_yac", data = WR_Wrangled_Lag)
stargazer(fits_salary_lag, type="text", dep.var.labels="Y5 Salary" )

#Predicting next year snaps based off previous year metrics
WR_Wrangled_Lag = read.csv("wr_wrangling_lag.csv")
fits_snaps_lag <- list()
fits_snaps_lag$pff <- lm("y2_snaps ~ y1_pff", data = WR_Wrangled_Lag)
fits_snaps_lag$touchdowns <- lm("y2_snaps ~ y1_touchdowns", data = WR_Wrangled_Lag)
fits_snaps_lag$receptions <- lm("y2_snaps ~ y1_receptions", data = WR_Wrangled_Lag)
fits_snaps_lag$yac <- lm("y2_snaps ~ y1_yac", data = WR_Wrangled_Lag)
stargazer(fits_snaps_lag, type="text", dep.var.labels="Next Year Snaps" )

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