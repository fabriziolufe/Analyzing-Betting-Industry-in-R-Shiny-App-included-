
#PREPARING THE TABLE FOR POKER CHIP CONVERSIONS


# Intalation of "haven" package to read the data
#install.packages("haven")
#Intalation of "lubridate" to transform and manipulate de dates 
#install.packages("lubridate")

#LIBRARIES 
library(lubridate)
library(magrittr)
library(dplyr)
library(tidyr)
library("haven")


#1.Reading SAS files into R ----
setwd("C:/Users/flucerofernandez/Documents/IESEG S01/R/GP/...")
Chip<- read_sas("./RawDataIIIPokerChipConversions.sas7bdat")


#2. Summary of the data base to find Missing values ----
summary(Chip)


#3. Show the data type of the variables ----
str(Chip)


#4. Transform TransDateTime from character to posixct ----
Chip$Date<- as.Date(Chip$TransDateTime)
Chip$Weekday <- weekdays(Chip$Date)



#5. Delete the Date column ----
NChip <- subset(Chip, select = -c(2))
length(unique(NChip$UserID))

#6. Groupby in order to extract the Poker Last Date, we group info by UserId ----
extractlastDate<- NChip %>% group_by(UserID)  %>%
  summarise(LastDate= max(Date),
            .groups = 'drop'
  )

#7. Pivot the Table in order to have detailed columns by user. in this case the columns will show in poker
#the amount of buys and sells of poker chips every day (grouped also by weekday) ----

Grouped_Chip_Pivot <- NChip %>% group_by(UserID)  %>%
  summarise (CountBuy_Monday = sum(TransType == '124'& Weekday=='Monday'),
            CountSell_Monday= sum(TransType=='24'& Weekday=='Monday'),
            Sumbuy_Monday=sum(TransAmount[which(TransType=='124'& Weekday=='Monday')]) ,
            SumSell_Monday=sum(TransAmount[which(TransType=='24'& Weekday=='Monday')]),
            CountBuy_Tuesday = sum(TransType == '124'& Weekday=='Tuesday'),
            CountSell_Tuesday= sum(TransType=='24'& Weekday=='Tuesday'),
            Sumbuy_Tuesday=sum(TransAmount[which(TransType=='124'& Weekday=='Tuesday')]) ,
            SumSell_Tuesday=sum(TransAmount[which(TransType=='24'& Weekday=='Tuesday')]),
            CountBuy_Wednesday = sum(TransType == '124'& Weekday=='Wednesday'),
            CountSell_Wednesday= sum(TransType=='24'& Weekday=='Wednesday'),
            Sumbuy_Wednesday=sum(TransAmount[which(TransType=='124'& Weekday=='Wednesday')]) ,
            SumSell_Wednesday=sum(TransAmount[which(TransType=='24'& Weekday=='Wednesday')]),
            CountBuy_Thursday = sum(TransType == '124'& Weekday=='Thursday'),
            CountSell_Thursday= sum(TransType=='24'& Weekday=='Thursday'),
            Sumbuy_Thursday=sum(TransAmount[which(TransType=='124'& Weekday=='Thursday')]) ,
            SumSell_Thursday=sum(TransAmount[which(TransType=='24'& Weekday=='Thursday')]),
            CountBuy_Friday = sum(TransType == '124'& Weekday=='Friday'),
            CountSell_Friday= sum(TransType=='24'& Weekday=='Friday'),
            Sumbuy_Friday=sum(TransAmount[which(TransType=='124'& Weekday=='Friday')]) ,
            SumSell_Friday=sum(TransAmount[which(TransType=='24'& Weekday=='Friday')]),
            CountBuy_Saturday = sum(TransType == '124'& Weekday=='Saturday'),
            CountSell_Saturday= sum(TransType=='24'& Weekday=='Saturday'),
            Sumbuy_Saturday=sum(TransAmount[which(TransType=='124'& Weekday=='Saturday')]) ,
            SumSell_Saturday=sum(TransAmount[which(TransType=='24'& Weekday=='Saturday')]),
            CountBuy_Sunday = sum(TransType == '124'& Weekday=='Sunday'),
            CountSell_Sunday= sum(TransType=='24'& Weekday=='Sunday'),
            Sumbuy_Sunday=sum(TransAmount[which(TransType=='124'& Weekday=='Sunday')]) ,
            SumSell_Sunday=sum(TransAmount[which(TransType=='24'& Weekday=='Sunday')]),
            .groups = 'drop'
  )

#8. After pivoting we merge the dataset with the last date of poker transactions, to be used for calculating recency.----
Grouped_Chip_Pivot <- merge(Grouped_Chip_Pivot, extractlastDate, by= "UserID", 
                          all.x = TRUE, all.y = FALSE)

#9. Calculating the Return over Investment for each day accumulates ( Poker)----


Grouped_Chip_Pivot$ROI_Monday <- round(((Grouped_Chip_Pivot$SumSell_Monday-Grouped_Chip_Pivot$Sumbuy_Monday)
                                        /Grouped_Chip_Pivot$Sumbuy_Monday)*100,2)
Grouped_Chip_Pivot$ROI_Tuesday <- round(((Grouped_Chip_Pivot$SumSell_Tuesday-Grouped_Chip_Pivot$Sumbuy_Tuesday)
                                         /Grouped_Chip_Pivot$Sumbuy_Tuesday)*100,2)
Grouped_Chip_Pivot$ROI_Wednesday <- round(((Grouped_Chip_Pivot$SumSell_Wednesday-Grouped_Chip_Pivot$Sumbuy_Wednesday)
                                           /Grouped_Chip_Pivot$Sumbuy_Wednesday)*100,2)
Grouped_Chip_Pivot$ROI_Thursday <- round(((Grouped_Chip_Pivot$SumSell_Thursday-Grouped_Chip_Pivot$Sumbuy_Thursday)
                                          /Grouped_Chip_Pivot$Sumbuy_Thursday)*100,2)
Grouped_Chip_Pivot$ROI_Friday <- round(((Grouped_Chip_Pivot$SumSell_Friday-Grouped_Chip_Pivot$Sumbuy_Friday)
                                        /Grouped_Chip_Pivot$Sumbuy_Friday)*100,2)
Grouped_Chip_Pivot$ROI_Saturday <- round(((Grouped_Chip_Pivot$SumSell_Saturday-Grouped_Chip_Pivot$Sumbuy_Saturday)
                                          /Grouped_Chip_Pivot$Sumbuy_Saturday)*100,2)
Grouped_Chip_Pivot$ROI_Sunday <- round(((Grouped_Chip_Pivot$SumSell_Sunday-Grouped_Chip_Pivot$Sumbuy_Sunday)
                                        /Grouped_Chip_Pivot$Sumbuy_Sunday)*100,2)

#10. Visualize the summary of the dataset.----
summary(Grouped_Chip_Pivot)

#11. Save the file in csv (easier to handle when merging everything together.)----
write.csv(Grouped_Chip_Pivot,'Transactions_Conversions.csv')
  





