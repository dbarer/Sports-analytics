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
WR_Data$a_pff <- WR_Data$grades_offense * WR_Data$snap_counts_offense

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

# A_PFF histogram
a_pff_histogram <-ggplot(WR_Data, aes(x=a_pff)) + 
  geom_histogram(aes(y=..density..), binwidth=1000, colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")

mean_a_pff <- mean(WR_Data$a_pff, na.rm=TRUE)

a_pff_histogram + geom_vline(aes(xintercept=mean(a_pff)),
               color="blue", linetype="dashed", size=1) +
  geom_text(aes(label=round(mean_a_pff,1),y=0,x=mean_a_pff),
            vjust=1.1,col='black',size=2) +
  ggtitle("Distribution of Adusted PFF for WRs (2010 - 2019)") +
  xlab("Adjusted PFF") + ylab("Density") +
  scale_x_continuous(labels=comma) 


# Boxplot by years
snap_threshold = 50

pff_seasons <- ggplot(data = subset(WR_Data, snap_counts_offense > snap_threshold), aes(x=experience, y=grades_offense, color=experience)) + 
  geom_boxplot() 

means <- aggregate(grades_offense ~  experience, data = subset(WR_Data, snap_counts_offense > snap_threshold), mean)
counts <- aggregate(grades_offense ~  experience, data = subset(WR_Data, snap_counts_offense > snap_threshold), length)

pff_seasons + geom_text(data = means, aes(label = round(grades_offense,1), y = grades_offense + 1.5), size =5)  +
  ggtitle("Mean PFF by years of experience (Snaps > 50)") +
  xlab("Years of Experience") + ylab("Mean PFF")

a_pff_seasons <- ggplot(data = subset(WR_Data, snap_counts_offense > snap_threshold), aes(x=experience, y=a_pff, color=experience)) + 
  geom_boxplot() 

means <- aggregate(a_pff ~  experience, data = subset(WR_Data, snap_counts_offense > snap_threshold), mean)
counts <- aggregate(a_pff ~  experience, data = subset(WR_Data, snap_counts_offense > snap_threshold), length)

a_pff_seasons + geom_text(data = means, aes(label = round(a_pff,1), y = a_pff + 1000), size =4)  +
  ggtitle("Mean Adjusted PFF by years of experience (Snaps > 50)") +
  xlab("Years of Experience") + ylab("Mean Adjusted PFF") +
  scale_y_continuous(labels=comma) 


# Salary Scatter vs PFF
#install.packages("stargazer")
library(stargazer)

WR_Data <- All_DATA %>% filter(position == "WR")
WR_Data <- All_DATA %>% filter(experience > -1)
WR_Data$a_pff <- WR_Data$grades_offense * WR_Data$snap_counts_offense

library(scales)
snap_threshold = 50

salary_scatter <- ggplot(data = subset(WR_Data, snap_counts_offense > snap_threshold), aes(x = grades_offense, y = Cash.Spent))
salary_scatter + geom_point(aes(color = Cash.Spent), size = 3) +
  scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme(legend.position = "none") + 
  geom_smooth(aes(color = Cash.Spent), method = lm, se = FALSE, fullrange = TRUE) +
  ggtitle("Salary ~ PFF (WRs 2010 - 2019 with atleast 50 snaps/season)") +
  xlab("PFF") + ylab("Salary") +
  scale_y_continuous(labels=dollar_format(prefix="$")) 

a_salary_scatter <- ggplot(data = subset(WR_Data, snap_counts_offense > snap_threshold), aes(x = a_pff, y = Cash.Spent))
a_salary_scatter + geom_point(aes(color = Cash.Spent), size = 3) +
  scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme(legend.position = "none") + 
  geom_smooth(aes(color = Cash.Spent), method = lm, se = FALSE, fullrange = TRUE) +
  ggtitle("Salary ~ Adjusted PFF (WRs 2010 - 2019 with atleast 50 snaps/season)") +
  xlab("Adjusted PFF") + ylab("Salary") +
  scale_y_continuous(labels=dollar_format(prefix="$")) +
  scale_x_continuous(labels=comma)

# Snaps Scatter vs PFF

snaps_scatter <- ggplot(data = subset(WR_Data, snap_counts_offense > snap_threshold), aes(x = grades_offense, y = snap_counts_offense))
snaps_scatter + geom_point(aes(color = snap_counts_offense), size = 3) +
  scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme(legend.position = "none") + 
  geom_smooth(aes(color = snap_counts_offense), method = lm, se = FALSE, fullrange = TRUE) +
  ggtitle("Snaps ~ PFF (Snaps > 50)") +
  xlab("PFF") + ylab("Snaps") +
  scale_y_continuous(labels=comma) 

a_snaps_scatter <- ggplot(data = subset(WR_Data, snap_counts_offense > snap_threshold), aes(x = a_pff, y = snap_counts_offense))
a_snaps_scatter + geom_point(aes(color = snap_counts_offense), size = 3) +
  scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme(legend.position = "none") + 
  geom_smooth(aes(color = snap_counts_offense), method = lm, se = FALSE, fullrange = TRUE) +
  ggtitle("Snaps ~ Adjusted PFF (Snaps > 50)") +
  xlab("Adjusted PFF") + ylab("Snaps") +
  scale_y_continuous(labels=comma) +
  scale_x_continuous(labels=comma) 

basic_fits <- list()
basic_fits$pff_reg <- lm("a_pff_y1 ~ y1_pff", data = subset(WR_Wrangled_Lag, snap_counts_offense > snap_threshold))
basic_fits$snaps_pff_reg <- lm("y1_snaps ~ y1_pff", data = subset(WR_Wrangled_Lag, snap_counts_offense > snap_threshold))
basic_fits$snaps_a_pff_reg <- lm("y1_snaps ~ a_pff_y1", data = subset(WR_Wrangled_Lag, snap_counts_offense > snap_threshold))
stargazer(basic_fits, type="text",
          column.labels = c("Same Season Adjusted PFF", "Same Season Snaps", "Same Season Snaps"), dep.var.labels="Players with snaps > 50", header = FALSE, font.size = "small",
          align = TRUE, digits=2, column.separate = c(1, 1, 1), covariate.labels = c("Current Season PFF", "Current Season Adjusted PFF"))


# receptions_scatter <- ggplot(data = subset(WR_Data, snap_counts_offense > snap_threshold), aes(x = receptions, y = Cash.Spent))
# receptions_scatter + geom_point(aes(color = Cash.Spent), size = 3) +
#   scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07")) +
#   theme(legend.position = "none") + 
#   geom_smooth(aes(color = Cash.Spent), method = lm, se = FALSE, fullrange = TRUE) +
#   ggtitle("Salary ~ Receptions") +
#   xlab("Receptions") + ylab("Salary")  +
#   scale_y_continuous(labels=dollar_format(prefix="$")) 
# 
# pff_td_scatter <- ggplot(WR_Data, aes(x = touchdowns, y = grades_offense))
# pff_td_scatter + geom_point(aes(color = grades_offense), size = 3) +
#   scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07")) +
#   theme(legend.position = "none") + 
#   geom_smooth(aes(color = grades_offense), method = lm, se = FALSE, fullrange = TRUE) +
#   ggtitle("PFF ~ Touchdowns") +
#   xlab("Touchdowns") + ylab("PFF")


WR_Wrangled_Lag = read.csv("wr_wrangling_lag.csv")
WR_Wrangled_Lag$a_pff_y1 <- WR_Wrangled_Lag$y1_pff * WR_Wrangled_Lag$y1_snaps
WR_Wrangled_Lag$a_pff_y2 <- WR_Wrangled_Lag$y2_pff * WR_Wrangled_Lag$y2_snaps

PFF_Smooth_Data = read.csv("PFF_correctcomb_V04.csv")

WR_Wrangled = read.csv("wr_wrangling.csv")
WR_Wrangled$a_pff_y1 <- WR_Wrangled$y1_pff * WR_Wrangled$y1_snaps
WR_Wrangled$a_pff_y2 <- WR_Wrangled$y2_pff * WR_Wrangled$y2_snaps
WR_Wrangled$a_pff_y3 <- WR_Wrangled$y3_pff * WR_Wrangled$y3_snaps
WR_Wrangled$a_pff_y4 <- WR_Wrangled$y4_pff * WR_Wrangled$y4_snaps
WR_Wrangled$a_pff_y5 <- WR_Wrangled$y5_pff * WR_Wrangled$y5_snaps

#Scatter Previous Year vs Year

pffy12_scatter <- ggplot(data = subset(WR_Wrangled_Lag, snap_counts_offense > snap_threshold), aes(x = y1_pff, y = y2_pff))
pffy12_scatter + geom_point(aes(color = y2_pff), size = 3) +
  scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme(legend.position = "none") + 
  geom_smooth(aes(color = y2_pff), method = lm, se = FALSE, fullrange = TRUE) +
  ggtitle("PFF ~ PFF Previous Year (WRs Y1-Y4 Snaps > 50)") +
  xlab("PFF Previous Year") + ylab("PFF") +
  scale_y_continuous(labels=comma) 

pffy1_snaps2_scatter <- ggplot(data = subset(WR_Wrangled_Lag, snap_counts_offense > snap_threshold), aes(x = y1_pff, y = y2_snaps))
pffy1_snaps2_scatter + geom_point(aes(color = y2_snaps), size = 3) +
  scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme(legend.position = "none") + 
  geom_smooth(aes(color = y2_snaps), method = lm, se = FALSE, fullrange = TRUE) +
  ggtitle("Snaps ~ PFF Previous Year (WRs Y1-Y4 Snaps > 50)") +
  xlab("PFF Previous Year") + ylab("Snaps") +
  scale_y_continuous(labels=comma) 

pffy1_comp2_scatter <- ggplot(data = subset(WR_Wrangled_Lag, snap_counts_offense > snap_threshold), aes(x = y1_pff, y = y2_salary))
pffy1_comp2_scatter + geom_point(aes(color = y2_salary), size = 3) +
  scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme(legend.position = "none") + 
  geom_smooth(aes(color = y2_salary), method = lm, se = FALSE, fullrange = TRUE) +
  ggtitle("Salary ~ PFF Previous Year (WRs Y1-Y4 Snaps > 50)") +
  xlab("PFF Previous Year") + ylab("Salary") +
  scale_y_continuous(labels=comma) 


a_pffy12_scatter <- ggplot(data = subset(WR_Wrangled_Lag, snap_counts_offense > snap_threshold), aes(x = a_pff_y1, y = a_pff_y2))
a_pffy12_scatter + geom_point(aes(color = a_pff_y2), size = 3) +
  scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme(legend.position = "none") + 
  geom_smooth(aes(color = a_pff_y2), method = lm, se = FALSE, fullrange = TRUE) +
  ggtitle("Adj PFF ~ Adj PFF Previous Year (WRs Y1-Y4 Snaps > 50)") +
  xlab("Adjusted PFF Previous Year") + ylab("Adjusted PFF") +
  scale_y_continuous(labels=comma) + 
  scale_x_continuous(labels=comma)

a_pffy1_snaps2_scatter <- ggplot(data = subset(WR_Wrangled_Lag, snap_counts_offense > snap_threshold), aes(x = a_pff_y1, y = y2_snaps))
a_pffy1_snaps2_scatter + geom_point(aes(color = y2_snaps), size = 3) +
  scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme(legend.position = "none") + 
  geom_smooth(aes(color = y2_snaps), method = lm, se = FALSE, fullrange = TRUE) +
  ggtitle("Snaps ~ Adj PFF PY (WRs Y1-Y4 Snaps > 50)") +
  xlab("Adjusted PFF Previous Year") + ylab("Snaps") +
  scale_y_continuous(labels=comma) 

a_pffy1_comp2_scatter <- ggplot(data = subset(WR_Wrangled_Lag, snap_counts_offense > snap_threshold), aes(x = a_pff_y1, y = y2_salary))
a_pffy1_comp2_scatter + geom_point(aes(color = y2_salary), size = 3) +
  scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme(legend.position = "none") + 
  geom_smooth(aes(color = y2_salary), method = lm, se = FALSE, fullrange = TRUE) +
  ggtitle("Salary ~ Adj PFF PY (WRs Y1-Y4 Snaps > 50)") +
  xlab("Adjusted PFF Previous Year") + ylab("Salary") +
  scale_y_continuous(labels=comma) 


lag_fits <- list()
lag_fits$pff_time_reg <- lm("y2_pff ~ y1_pff", data = subset(WR_Wrangled_Lag, snap_counts_offense > snap_threshold))
lag_fits$pff_snaps_reg <- lm("y2_snaps ~ y1_pff", data = subset(WR_Wrangled_Lag, snap_counts_offense > snap_threshold))
lag_fits$pff_salary_reg <- lm("y2_salary ~ y1_pff", data = subset(WR_Wrangled_Lag, snap_counts_offense > snap_threshold))
lag_fits$a_pff_time_reg <- lm("a_pff_y2 ~ a_pff_y1", data = subset(WR_Wrangled_Lag, snap_counts_offense > snap_threshold))
lag_fits$a_pff_snaps_reg <- lm("y2_snaps ~ a_pff_y1", data = subset(WR_Wrangled_Lag, snap_counts_offense > snap_threshold))
lag_fits$a_pff_salary_reg <- lm("y2_salary ~ a_pff_y1", data = subset(WR_Wrangled_Lag, snap_counts_offense > snap_threshold))
stargazer(lag_fits, type="text", title = "Regressing Previous Year PFF with Next Year Success Measures", dep.var.labels="Success Measures", 
          column.labels = c("Next Season PFF", "Next Season Snaps", "Next Season Salary", "Next Season Adjusted PFF", "Next Season Snaps", "Next Season Salary"), header = FALSE, font.size = "small",
          align = TRUE, digits=2, column.separate = c(1, 1, 1, 1, 1, 1),
          covariate.labels = c("Current Season PFF", "Current Season Adjusted PFF"))

#Bar plot DRank vs PFF_Smooth
PFF_Smooth_Data$Drank <- as.factor(PFF_Smooth_Data$Drank)

means <- aggregate(pff_smooth ~  Drank, data = subset(PFF_Smooth_Data, snaps> snap_threshold), mean)
counts <- aggregate(pff_smooth ~  Drank, data = subset(PFF_Smooth_Data, snaps> snap_threshold), length)

Drank_PFF <- ggplot(means, aes(x=Drank, y=pff_smooth, fill=Drank)) + 
  geom_bar(stat = "identity") 

Drank_PFF + geom_text(data = means, aes(label = round(pff_smooth,1), y = pff_smooth + 500), size =3)  +
  ggtitle("Average Career Performance Score by Draft Rank (WRs 2011 - 2018 Snaps > 50)") +
  xlab("Draft Rank") + ylab("Average Career Performance Score") +
  scale_y_continuous(labels=comma) +
  theme(legend.position = "none")

#Bar plot DRank vs Y4 A_PFF and Y5 Cash
WR_Wrangled = read.csv("wr_wrangling.csv")
WR_Wrangled$a_pff_y1 <- WR_Wrangled$y1_pff * WR_Wrangled$y1_snaps
WR_Wrangled$a_pff_y2 <- WR_Wrangled$y2_pff * WR_Wrangled$y2_snaps
WR_Wrangled$a_pff_y3 <- WR_Wrangled$y3_pff * WR_Wrangled$y3_snaps
WR_Wrangled$a_pff_y4 <- WR_Wrangled$y4_pff * WR_Wrangled$y4_snaps
WR_Wrangled$a_pff_y5 <- WR_Wrangled$y5_pff * WR_Wrangled$y5_snaps

WR_Wrangled$Draft.Group <- as.factor(WR_Wrangled$Draft.Group)
WR_Wrangled$Draft.Group <- factor(WR_Wrangled$Draft.Group,levels(WR_Wrangled$Draft.Group)[c(2,7,3:6)])
means <- aggregate(y4_pff ~  Draft.Group, data = subset(WR_Wrangled, snap_counts_offense > snap_threshold), mean)

Drank_PFF <- ggplot(means, aes(x=Draft.Group, y=y4_pff, fill=Draft.Group)) + 
  geom_bar(stat = "identity") 
Drank_PFF + geom_text(data = means, aes(label = round(y4_pff,1), y = y4_pff + 2), size =3)  +
  ggtitle("Y4 PFF by Draft Rank (Snaps >50)") +
  xlab("Draft Rank Group") + ylab("Y4 PFF") +
  scale_y_continuous(labels=comma) +
  theme(legend.position = "none")

means <- aggregate(a_pff_y4 ~  Draft.Group, data = subset(WR_Wrangled, snap_counts_offense > snap_threshold), mean)

Drank_A_PFF <- ggplot(means, aes(x=Draft.Group, y=a_pff_y4, fill=Draft.Group)) + 
  geom_bar(stat = "identity") 
Drank_A_PFF + geom_text(data = means, aes(label = round(a_pff_y4,1), y = a_pff_y4 + 200), size =3)  +
  ggtitle("Y4 Adjusted PFF by Draft Rank (Snaps > 50)") +
  xlab("Draft Rank Group") + ylab("Y4 Adjusted PFF") +
  scale_y_continuous(labels=comma) +
  theme(legend.position = "none")


means <- aggregate(y5_salary ~  Draft.Group, data = subset(WR_Wrangled, snap_counts_offense > snap_threshold), mean)
Drank_Cash <- ggplot(means, aes(x=Draft.Group, y=y5_salary, fill=Draft.Group)) + 
  geom_bar(stat = "identity") 
Drank_Cash + geom_text(data = means, aes(label = round(y5_salary,1), y = y5_salary + 500000), size =3)  +
  ggtitle("Y5 Salary by Draft Rank (Snaps > 50)") +
  xlab("Draft Rank Group") + ylab("Y5 Salary") +
  scale_y_continuous(labels=dollar_format(prefix="$")) #+ coord_cartesian(ylim=c(40,100))

# Salary Regression vs Performance Metrics (all)
fits_salary_all <- list()
fits_salary_all$pff <- lm("Cash.Spent ~ grades_offense", data = WR_Data)
fits_salary_all$adjusted_pff <- lm("Cash.Spent ~ a_pff", data = WR_Data)
fits_salary_all$touchdowns <- lm("Cash.Spent ~ touchdowns", data = WR_Data)
fits_salary_all$receptions <- lm("Cash.Spent ~ receptions", data = WR_Data)
#fits_salary_all$yac <- lm("Cash.Spent ~ yards_after_catch", data = WR_Data)
stargazer(fits_salary_all, type="text", title = "Regressing Salary with Performance Metrics", dep.var.labels="Salary", header = FALSE, 
          covariate.labels = c("PFF", "Adjusted PFF", "Touchdowns", "Receptions"))

# Salary Regression vs Performance Metrics (subset 50 snaps)
fits_salary_subset <- list()

fits_salary_subset$pff <- lm("Cash.Spent ~ grades_offense", data = subset(WR_Data, snap_counts_offense > snap_threshold))
fits_salary_subset$adjusted_pff <- lm("Cash.Spent ~ a_pff", data = subset(WR_Data, snap_counts_offense > snap_threshold))
fits_salary_subset$touchdowns <- lm("Cash.Spent ~ touchdowns", data = subset(WR_Data, snap_counts_offense > snap_threshold))
fits_salary_subset$receptions <- lm("Cash.Spent ~ receptions", data = subset(WR_Data, snap_counts_offense > snap_threshold))
#fits_salary_subset$yac <- lm("Cash.Spent ~ yards_after_catch", data = subset(WR_Data, snap_counts_offense > snap_threshold))
stargazer(fits_salary_all, fits_salary_subset, type="text", title = "Regressing Salary with Performance Metrics",
          column.labels = c("All WRs", "Players with snaps > 50"), dep.var.labels="Salary", header = FALSE, font.size = "small",
          align = TRUE, digits=2, column.separate = c(4, 4), covariate.labels = c("PFF", "Adjusted PFF", "Touchdowns", "Receptions"), column.sep.width = "-15pt")

# Snaps Regression vs Performance Metrics (all)
fits_snaps <- list()
fits_snaps$pff <- lm("snap_counts_offense ~ grades_offense", data = WR_Data)
fits_snaps$adjusted_pff <- lm("snap_counts_offense ~ a_pff", data = WR_Data)
fits_snaps$touchdowns <- lm("snap_counts_offense ~ touchdowns", data = WR_Data)
fits_snaps$receptions <- lm("snap_counts_offense ~ receptions", data = WR_Data)
#fits_snaps$yac <- lm("snap_counts_offense ~ yards_after_catch", data = WR_Data)
stargazer(fits_snaps, type="text", dep.var.labels="Snap Count", title = "Regressing Snaps with Performance Metrics",
          column.labels = c("All WRs"), header = FALSE, font.size = "small",
          align = TRUE, digits=2, column.separate = 5, covariate.labels = c("PFF", "Adjusted PFF", "Touchdowns", "Receptions", "Yards After Catch"), column.sep.width = "-15pt" )

fits_snaps_subset <- list()
fits_snaps_subset$pff <- lm("snap_counts_offense ~ grades_offense", data = subset(WR_Data, snap_counts_offense > snap_threshold))
fits_snaps_subset$adjusted_pff <- lm("snap_counts_offense ~ a_pff", data = subset(WR_Data, snap_counts_offense > snap_threshold))
fits_snaps_subset$touchdowns <- lm("snap_counts_offense ~ touchdowns", data = subset(WR_Data, snap_counts_offense > snap_threshold))
fits_snaps_subset$receptions <- lm("snap_counts_offense ~ receptions", ddata = subset(WR_Data, snap_counts_offense > snap_threshold))
#fits_snaps_subset$yac <- lm("snap_counts_offense ~ yards_after_catch", data = WR_Data)
stargazer(fits_snaps, fits_snaps_subset, type="text", dep.var.labels="Snap Count", title = "Regressing Snaps with Performance Metrics",
          column.labels = c("All WRs", "Players with snaps > 50"), header = FALSE, font.size = "small",
          align = TRUE, digits=2, column.separate = c(4,4), covariate.labels = c("PFF", "Adjusted PFF", "Touchdowns", "Receptions"), column.sep.width = "-15pt" )


#Predicting yr 5 salary based off yr 4 data
fits_salary_y5_4 <- list()
fits_salary_y5_4$pff <- lm("y5_salary ~ y4_pff", data = subset(WR_Wrangled, y4_snaps > snap_threshold))
fits_salary_y5_4$adjusted_pff <- lm("y5_salary ~ a_pff_y4", data = subset(WR_Wrangled, y4_snaps > snap_threshold))
fits_salary_y5_4$touchdowns <- lm("y5_salary ~ y4_touchdowns", data = subset(WR_Wrangled, y4_snaps > snap_threshold))
fits_salary_y5_4$receptions <- lm("y5_salary ~ y4_receptions", data = subset(WR_Wrangled, y4_snaps > snap_threshold))
#fits_salary_y5_4$yac <- lm("y5_salary ~ y4_yac", data = subset(WR_Wrangled, y4_snaps > snap_threshold))
stargazer(fits_salary_y5_4, type="text", title = "Regressing Y5 Salary with Past Performance Metrics",
          dep.var.labels="Y5 Salary", header = FALSE, font.size = "small",
          align = TRUE, digits=2, column.separate = c(4, 4), covariate.labels = c("Y4 PFF", "Y4 Adjusted PFF", "Y4 Touchdowns", "Y4 Receptions"), column.sep.width = "-15pt")

#Predicting yr 5 snaps based off yr 4 data
WR_Wrangled = read.csv("wr_wrangling.csv")
fits_snaps_y5 <- list()
fits_snaps_y5$pff <- lm("y5_snaps ~ y4_pff", data = WR_Wrangled)
fits_snaps_y5$touchdowns <- lm("y5_snaps ~ y4_touchdowns", data = WR_Wrangled)
fits_snaps_y5$receptions <- lm("y5_snaps ~ y4_receptions", data = WR_Wrangled)
fits_snaps_y5$yac <- lm("y5_snaps ~ y4_yac", data = WR_Wrangled)
stargazer(fits_snaps_y5, type="text", dep.var.labels="Y5 Snaps" )

#Predicting yr 5 salary based off yr 3 data
fits_salary_y5_3 <- list()
fits_salary_y5_3$pff <- lm("y5_salary ~ y3_pff", data = subset(WR_Wrangled, y3_snaps > snap_threshold))
fits_salary_y5_3$adjusted_pff <- lm("y5_salary ~ a_pff_y3", data = subset(WR_Wrangled, y3_snaps > snap_threshold))
fits_salary_y5_3$touchdowns <- lm("y5_salary ~ y3_touchdowns", data = subset(WR_Wrangled, y3_snaps > snap_threshold))
fits_salary_y5_3$receptions <- lm("y5_salary ~ y3_receptions", data = subset(WR_Wrangled, y3_snaps > snap_threshold))
#fits_salary_y5_3$yac <- lm("y5_salary ~ y3_yac", data = subset(WR_Wrangled, y3_snaps > snap_threshold))
stargazer(fits_salary_y5_3, type="text", title = "Regressing Y5 Salary with Past Performance Metrics",
          dep.var.labels="Y5 Salary", header = FALSE, font.size = "small",
          align = TRUE, digits=2, column.separate = c(4, 4), covariate.labels = c("Y3 PFF", "Y3 Adjusted PFF", "Y3 Touchdowns", "Y3 Receptions"), column.sep.width = "-15pt")

#Predicting yr 5 salary based off yr 2 data
fits_salary_y5_2 <- list()
fits_salary_y5_2$pff <- lm("y5_salary ~ y2_pff", data = subset(WR_Wrangled, y2_snaps > snap_threshold))
fits_salary_y5_2$adjusted_pff <- lm("y5_salary ~ a_pff_y2", data = subset(WR_Wrangled, y2_snaps > snap_threshold))
fits_salary_y5_2$touchdowns <- lm("y5_salary ~ y2_touchdowns", data = subset(WR_Wrangled, y2_snaps > snap_threshold))
fits_salary_y5_2$receptions <- lm("y5_salary ~ y2_receptions", data = subset(WR_Wrangled, y2_snaps > snap_threshold))
#fits_salary_y5_2$yac <- lm("y5_salary ~ y2_yac", data = subset(WR_Wrangled, y2_snaps > snap_threshold))
stargazer(fits_salary_y5_2, type="text", title = "Regressing Y5 Salary with Past Performance Metrics",
          dep.var.labels="Y5 Salary", header = FALSE, font.size = "small",
          align = TRUE, digits=2, column.separate = c(4, 4), covariate.labels = c("Y2 PFF", "Y2 Adjusted PFF", "Y2 Touchdowns", "Y2 Receptions"), column.sep.width = "-15pt")

#Predicting yr 5 salary based off yr 1 data
fits_salary_y5_1 <- list()
fits_salary_y5_1$pff <- lm("y5_salary ~ y1_pff", data = subset(WR_Wrangled, y1_snaps > snap_threshold))
fits_salary_y5_1$adjusted_pff <- lm("y5_salary ~ a_pff_y1", data = subset(WR_Wrangled, y1_snaps > snap_threshold))
fits_salary_y5_1$touchdowns <- lm("y5_salary ~ y1_touchdowns", data = subset(WR_Wrangled, y1_snaps > snap_threshold))
fits_salary_y5_1$receptions <- lm("y5_salary ~ y1_receptions", data = subset(WR_Wrangled, y2_snaps > snap_threshold))
#fits_salary_y5_1$yac <- lm("y5_salary ~ y1_yac", data = WR_Wrangled)
stargazer(fits_salary_y5_1, type="text", title = "Regressing Y5 Salary with Past Performance Metrics",
          dep.var.labels="Y5 Salary", header = FALSE, font.size = "small",
          align = TRUE, digits=2, column.separate = c(4, 4), covariate.labels = c("Y1 PFF", "Y1 Adjusted PFF", "Y1 Touchdowns", "Y1 Receptions"), column.sep.width = "-15pt")


# stargazer(fits_salary_y5_1, fits_salary_y5_2, type="text", title = "Regressing Y5 Salary with Past Performance Metrics ",
#           column.labels = c("Y1 Performance", "Y2 Performance"), dep.var.labels="Salary", header = FALSE, font.size = "small",
#           align = TRUE, digits=2, column.separate = c(5, 5), covariate.labels = c("Y1 PFF", "Y1 Adjusted PFF", "Y1 Touchdowns", "Y1 Receptions", "Y1 Yards After Catch",
#                                                                                   "Y2 PFF", "Y2 Adjusted PFF", "Y2 Touchdowns", "Y2 Receptions", "Y2 Yards After Catch"), column.sep.width = "-15pt")
# 
# stargazer(fits_salary_y5_3, fits_salary_y5_4, type="text", title = "Regressing Y5 Salary with Past Performance Metrics ",
#           column.labels = c("Y3 Performance", "Y4 Performance"), dep.var.labels="Salary", header = FALSE, font.size = "small",
#           align = TRUE, digits=2, column.separate = c(5, 5), covariate.labels = c("Y3 PFF", "Y3 Adjusted PFF", "Y3 Touchdowns", "Y3 Receptions", "Y3 Yards After Catch",
#                                                                                   "Y4 PFF", "Y4 Adjusted PFF", "Y4 Touchdowns", "Y4 Receptions", "Y4 Yards After Catch"), column.sep.width = "-15pt")


#Predicting next year salary based off previous year metrics
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