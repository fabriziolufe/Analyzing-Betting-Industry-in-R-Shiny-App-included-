#MERGING TOGETHER AND ADDITIONAL TOUCHES FOR DATAMART RAW

#LIBRARIES
library("lubridate")

#1. Reading Both csv SAS files into R ----
setwd("C:/Users/flucerofernandez/Documents/IESEG S01/R/GP/...")

Chip_Conversions <- read.csv("./Transactions_Conversions.csv")
Demographics <- read.csv("./Demographics_by_User.csv")
User_Transactions <- read.csv("./User_aggregated.csv")

#2. Merge the datasets together ----
merge_Dem_Chip <-merge(Demographics, Chip_Conversions, by= "UserID", 
                       all.x = TRUE, all.y = FALSE)

merge_Dem_Chip <- subset(merge_Dem_Chip, select = -c(2,18))

Merge_with_trans <- merge(merge_Dem_Chip, User_Transactions, by= "UserID", 
                          all.x = TRUE, all.y = FALSE)

#3.Remove index created by merges. (by def)----
Merge_with_trans <- subset(Merge_with_trans, select = -53)

#4. Create a variable of last day of analysis----
analysis_date <- as.Date("2005-09-30")

# 5. Double check if the dates are in the correct format.---- 
Merge_with_trans$LastDate<- as.Date(Merge_with_trans$LastDate)

Merge_with_trans$DateF <- as.Date(Merge_with_trans$DateF)

Merge_with_trans$DateI <- as.Date(Merge_with_trans$DateI)

Merge_with_trans$FirstPo <- as.Date(Merge_with_trans$FirstPo)




#6. Create Recency indicator----
Merge_with_trans$Index_Recency <- difftime(analysis_date,
                                     pmax(Merge_with_trans$LastDate, Merge_with_trans$DateF, na.rm = TRUE), units='days')

Merge_with_trans$Index_Recency <-ifelse(Merge_with_trans$Index_Recency<0, 0, Merge_with_trans$Index_Recency)

#7. Create Length of Relationship indicator----
Merge_with_trans$Index_LoR <- difftime(pmax(Merge_with_trans$LastDate, Merge_with_trans$DateF, na.rm = TRUE),
                                 pmin(Merge_with_trans$FirstPo, Merge_with_trans$DateI, na.rm = TRUE), units ='days')



#8. Rename some columns for better understanding. ----

colnames(Merge_with_trans)[colnames(Merge_with_trans)=="DateI"] <- "Date_Initial_AllGames"
colnames(Merge_with_trans)[colnames(Merge_with_trans)=="DateF"] <- "Date_Final_AllGames"
colnames(Merge_with_trans)[colnames(Merge_with_trans)=="LastDate"] <- "Date_Final_Poker"
colnames(Merge_with_trans)[colnames(Merge_with_trans)=="ROI_Monday"] <- "Index_ROI_Po_Monday"
colnames(Merge_with_trans)[colnames(Merge_with_trans)=="ROI_Tuesday"] <- "Index_ROI_Po_Tuesday"
colnames(Merge_with_trans)[colnames(Merge_with_trans)=="ROI_Wednesday"] <- "Index_ROI_Po_Wednesday"
colnames(Merge_with_trans)[colnames(Merge_with_trans)=="ROI_Thursday"] <- "Index_ROI_Po_Thursday"
colnames(Merge_with_trans)[colnames(Merge_with_trans)=="ROI_Friday"] <- "Index_ROI_Po_Friday"
colnames(Merge_with_trans)[colnames(Merge_with_trans)=="ROI_Saturday"] <- "Index_ROI_Po_Saturday"
colnames(Merge_with_trans)[colnames(Merge_with_trans)=="ROI_Sunday"] <- "Index_ROI_Po_Sunday"
colnames(Merge_with_trans)[colnames(Merge_with_trans)=="Index_WinsOverStakes"] <- "Index_ROI_WinsOverStakes"
colnames(Merge_with_trans)[colnames(Merge_with_trans)=="Application.Description"] <- "Application_Description"
colnames(Merge_with_trans)[colnames(Merge_with_trans)=="Language.Description"] <- "Language_Description"
colnames(Merge_with_trans)[colnames(Merge_with_trans)=="Country.Name"] <- "Country_Name"

#9. Save the file in csv (easier to handle when merging everything together.) ----
write.csv(Merge_with_trans,'Data_Mart_Raw.csv')