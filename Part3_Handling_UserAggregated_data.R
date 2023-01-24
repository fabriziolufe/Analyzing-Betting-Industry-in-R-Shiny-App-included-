#PREPARING THE TABLE FOR USER AGGREGATED DATA
# Read the RawData ----

setwd("~/MASTER BIG DATA ANALYTICS/7. BUSINESS ANALYTICS TOOLS - OPEN SOURCE/Group Assignment")
dir()

# Package installation
install.packages("dplyr")
library(dplyr)
install.packages("haven")
library(haven)
install.packages("lubridate")
library(lubridate)
install.packages("data.table")
library(data.table)

demo <- read_sas("RawDataIDemographics.sas7bdat")
user <- read_sas("RawDataIIUserDailyAggregation.sas7bdat")
poker <- read_sas("RawDataIIIPokerChipConversions.sas7bdat")

# Data cleaning ----

## RawDataI Demographics ----

summary(demo)
class(demo)

## RawDataI UserDailyAggregation ----

summary(user)
class(user)

user$Date_DailyAgg <- as.Date(user$Date, format = "%Y%m%d")
user$UserID <- as.character(user$UserID)

## RawDataI PokerChipConversions ----

summary(poker)
class(poker)

# DataSet Construction ----

## Grouping by user UserDailyAggregation ----

user_agg <- user

# Subsetting dates 
# You can exclude records in the raw dataset UserDailyAggregation that took place before the 
# first pay-in date (i.e., variable FirstPay in raw dataset Demographics) in the preparation of 
# the data mart.

demo_firstday <- demo[,c(1,5)]
demo_firstday$FirstPay <- as.Date(demo_firstday$FirstPay, format = "%Y%m%d")

user_agg <- merge(user_agg, demo_firstday, by = "UserID")
user_agg <- subset(user_agg, user_agg$Date_DailyAgg >= user_agg$FirstPay)

# Products ----

user_agg$Product1 <- ifelse(user_agg$ProductID == 1, 1, 0)
user_agg$Product2 <- ifelse(user_agg$ProductID == 2, 1, 0)
user_agg$Product3 <- ifelse(user_agg$ProductID == 3, 1, 0)
user_agg$Product4 <- ifelse(user_agg$ProductID == 4, 1, 0)
user_agg$Product5 <- ifelse(user_agg$ProductID == 5, 1, 0)
user_agg$Product6 <- ifelse(user_agg$ProductID == 6, 1, 0)
user_agg$Product7 <- ifelse(user_agg$ProductID == 7, 1, 0)
user_agg$Product8 <- ifelse(user_agg$ProductID == 8, 1, 0)

# Stakes, Winnings and Bets ----

user_agg$Stakes_P1 <- ifelse(user_agg$ProductID == 1, user_agg$Stakes, 0)
user_agg$Stakes_P2 <- ifelse(user_agg$ProductID == 2, user_agg$Stakes, 0)
user_agg$Stakes_P3 <- ifelse(user_agg$ProductID == 3, user_agg$Stakes, 0)
user_agg$Stakes_P4 <- ifelse(user_agg$ProductID == 4, user_agg$Stakes, 0)
user_agg$Stakes_P5 <- ifelse(user_agg$ProductID == 5, user_agg$Stakes, 0)
user_agg$Stakes_P6 <- ifelse(user_agg$ProductID == 6, user_agg$Stakes, 0)
user_agg$Stakes_P7 <- ifelse(user_agg$ProductID == 7, user_agg$Stakes, 0)
user_agg$Stakes_P8 <- ifelse(user_agg$ProductID == 8, user_agg$Stakes, 0)

user_agg$Wins_P1 <- ifelse(user_agg$ProductID == 1, user_agg$Winnings, 0)
user_agg$Wins_P2 <- ifelse(user_agg$ProductID == 2, user_agg$Winnings, 0)
user_agg$Wins_P3 <- ifelse(user_agg$ProductID == 3, user_agg$Winnings, 0)
user_agg$Wins_P4 <- ifelse(user_agg$ProductID == 4, user_agg$Winnings, 0)
user_agg$Wins_P5 <- ifelse(user_agg$ProductID == 5, user_agg$Winnings, 0)
user_agg$Wins_P6 <- ifelse(user_agg$ProductID == 6, user_agg$Winnings, 0)
user_agg$Wins_P7 <- ifelse(user_agg$ProductID == 7, user_agg$Winnings, 0)
user_agg$Wins_P8 <- ifelse(user_agg$ProductID == 8, user_agg$Winnings, 0)

user_agg$Bets_P1 <- ifelse(user_agg$ProductID == 1, user_agg$Bets, 0)
user_agg$Bets_P2 <- ifelse(user_agg$ProductID == 2, user_agg$Bets, 0)
user_agg$Bets_P3 <- ifelse(user_agg$ProductID == 3, user_agg$Bets, 0)
user_agg$Bets_P4 <- ifelse(user_agg$ProductID == 4, user_agg$Bets, 0)
user_agg$Bets_P5 <- ifelse(user_agg$ProductID == 5, user_agg$Bets, 0)
user_agg$Bets_P6 <- ifelse(user_agg$ProductID == 6, user_agg$Bets, 0)
user_agg$Bets_P7 <- ifelse(user_agg$ProductID == 7, user_agg$Bets, 0)
user_agg$Bets_P8 <- ifelse(user_agg$ProductID == 8, user_agg$Bets, 0)

# Dates ----

# user_agg$DateI_P1 <- ifelse(user_agg$ProductID == 1, user_agg$Date_DailyAgg, 0)
# user_agg$DateF_P1 <- ifelse(user_agg$ProductID == 1, user_agg$Date_DailyAgg, 0)
# 
# user_agg$DateI_P2 <- ifelse(user_agg$ProductID == 2, user_agg$Date_DailyAgg, 0)
# user_agg$DateF_P2 <- ifelse(user_agg$ProductID == 2, user_agg$Date_DailyAgg, 0)
# 
# user_agg$DateI_P3 <- ifelse(user_agg$ProductID == 3, user_agg$Date_DailyAgg, 0)
# user_agg$DateF_P3 <- ifelse(user_agg$ProductID == 3, user_agg$Date_DailyAgg, 0)
# 
# user_agg$DateI_P4 <- ifelse(user_agg$ProductID == 4, user_agg$Date_DailyAgg, 0)
# user_agg$DateF_P4 <- ifelse(user_agg$ProductID == 4, user_agg$Date_DailyAgg, 0)
# 
# user_agg$DateI_P5 <- ifelse(user_agg$ProductID == 5, user_agg$Date_DailyAgg, 0)
# user_agg$DateF_P5 <- ifelse(user_agg$ProductID == 5, user_agg$Date_DailyAgg, 0)
# 
# user_agg$DateI_P6 <- ifelse(user_agg$ProductID == 6, user_agg$Date_DailyAgg, 0)
# user_agg$DateF_P6 <- ifelse(user_agg$ProductID == 6, user_agg$Date_DailyAgg, 0)
# 
# user_agg$DateI_P7 <- ifelse(user_agg$ProductID == 7, user_agg$Date_DailyAgg, 0)
# user_agg$DateF_P7 <- ifelse(user_agg$ProductID == 7, user_agg$Date_DailyAgg, 0)
# 
# user_agg$DateI_P8 <- ifelse(user_agg$ProductID == 8, user_agg$Date_DailyAgg, 0)
# user_agg$DateF_P8 <- ifelse(user_agg$ProductID == 8, user_agg$Date_DailyAgg, 0)

user_agg$DateI <- user_agg$Date_DailyAgg
user_agg$DateF <- user_agg$Date_DailyAgg


# Data aggregation ----

class(user_agg)

user_agg2 <- data.table(user_agg)

user_agg3 <- user_agg2[,.(Stakes=sum(Stakes), Winnings=sum(Winnings), Bets=sum(Bets),
                        N_Product1 = sum(Product1),
                        N_Product2 = sum(Product2),
                        N_Product3 = sum(Product3),
                        N_Product4 = sum(Product4),
                        N_Product5 = sum(Product5),
                        N_Product6 = sum(Product6),
                        N_Product7 = sum(Product7),
                        N_Product8 = sum(Product8),
                        Stakes_P1 = sum(Stakes_P1),
                        Stakes_P2 = sum(Stakes_P2),
                        Stakes_P3 = sum(Stakes_P3),
                        Stakes_P4 = sum(Stakes_P4),
                        Stakes_P5 = sum(Stakes_P5),
                        Stakes_P6 = sum(Stakes_P6),
                        Stakes_P7 = sum(Stakes_P7),
                        Stakes_P8 = sum(Stakes_P8),
                        Wins_P1 = sum(Wins_P1),
                        Wins_P2 = sum(Wins_P2),
                        Wins_P3 = sum(Wins_P3),
                        Wins_P4 = sum(Wins_P4),
                        Wins_P5 = sum(Wins_P5),
                        Wins_P6 = sum(Wins_P6),
                        Wins_P7 = sum(Wins_P7),
                        Wins_P8 = sum(Wins_P8),
                        Bets_P1 = sum(Bets_P1),
                        Bets_P2 = sum(Bets_P2),
                        Bets_P3 = sum(Bets_P3),
                        Bets_P4 = sum(Bets_P4),
                        Bets_P5 = sum(Bets_P5),
                        Bets_P6 = sum(Bets_P6),
                        Bets_P7 = sum(Bets_P7),
                        Bets_P8 = sum(Bets_P8),
                        DateI = min(DateI),
                        DateF = max(DateF)), by = "UserID"]

n_distinct(user$UserID) == nrow(user_agg3)

summary(user_agg3$DateI)
summary(user_agg3$DateF)
# Round values

user_agg3 <- user_agg3 %>% mutate(across(where(is.numeric), round, 1))


# Variable creation ----

# user_agg3$Tot_Products <- sum(user_agg3$N_Product1,
#                               user_agg3$N_Product2,
#                               user_agg3$N_Product3,
#                               user_agg3$N_Product4,
#                               user_agg3$N_Product5,
#                               user_agg3$N_Product6,
#                               user_agg3$N_Product7,
#                               user_agg3$N_Product8)

# sum of products
user_agg3$Tot_Products <- user_agg3$N_Product1 +
                          user_agg3$N_Product2 +
                          user_agg3$N_Product3 +
                          user_agg3$N_Product4 +
                          user_agg3$N_Product5 +
                          user_agg3$N_Product6 +
                          user_agg3$N_Product7 +
                          user_agg3$N_Product8

# Weight of amount of products, stakes, winnings, bets
user_agg3$Weight_Products_Perc <- (user_agg3$Tot_Products / sum(user_agg3$Tot_Products))*100
user_agg3$Weight_Stakes_Perc <- (user_agg3$Stakes / sum(user_agg3$Stakes))*100
user_agg3$Weight_Winnings_Perc <- (user_agg3$Winnings / sum(user_agg3$Winnings))*100
user_agg3$Weight_Bets_Perc <- (user_agg3$Bets / sum(user_agg3$Bets))*100

# Index wins ($) / stakes ($)

user_agg3$Index_WinsOverStakes <- ifelse(is.na(user_agg3$Winnings/user_agg3$Stakes),0,user_agg3$Winnings/user_agg3$Stakes)
user_agg3$Index_WinsOverStakes <- as.numeric(user_agg3$Index_WinsOverStakes)
summary(user_agg3$index_WinsOverStakes)  
  
# Average stakes and wins per bet

user_agg3$Index_StakesPerBet <- user_agg3$Stakes / user_agg3$Bets
user_agg3$Index_WinsPerBet <- user_agg3$Winnings / user_agg3$Bets

sum(user_agg3$Weight_Bets_Perc)

# Tabla de fechas para merge con tabla final

colnames(user_agg3)

user_agg_dateF <- user_agg3[,c(1,37,38)]

write.csv(user_agg3,'User_aggregated.csv')