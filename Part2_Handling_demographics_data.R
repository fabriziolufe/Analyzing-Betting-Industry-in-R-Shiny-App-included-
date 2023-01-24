#PREPARING THE TABLE FOR DEMOGRAPHICS


#install.packages('haven')
#install.packages('lubridate')
#install.packages("readxl")

# LIBRARIES 

library("readxl")
library('haven')
library("dplyr")
library(tidyr)
library('lubridate')

#1. Read SAS dataset of demogrpahics----
setwd("C:/Users/flucerofernandez/Documents/IESEG S01/R/GP/...")
Demographics <- read_sas("./RawDataIDemographics.sas7bdat")

#2. Change RegDate and other important dates into a R Date format, represented as double in order to make Calcs.----
Demographics$RegDate <- as.Date(Demographics$RegDate, format = "%Y-%m-%d")

Demographics$FirstPay <- as.Date(Demographics$FirstPay, format = "%Y%m%d")

Demographics$FirstAct <- as.Date(Demographics$FirstAct, format = "%Y%m%d")

Demographics$FirstSp <- as.Date(Demographics$FirstSp, format = "%Y%m%d")

Demographics$FirstCa <- as.Date(Demographics$FirstCa, format = "%Y%m%d")

Demographics$FirstGa <- as.Date(Demographics$FirstGa, format = "%Y%m%d")

Demographics$FirstPo <- as.Date(Demographics$FirstPo, format = "%Y%m%d")


#in order to merge without issues, we will leave data types as they are, for now...----
#(changed just after merging everything together)



#3. Read appendices with readxl ----

appendix1 <- read_excel("./Appendices Group Assignment.xlsx", sheet = "Appendix 1")
appendix2 <- read_excel("./Appendices Group Assignment.xlsx", sheet = "Appendix 2")
appendix3 <- read_excel("./Appendices Group Assignment.xlsx", sheet = "Appendix 3")
appendix4 <- read_excel("./Appendices Group Assignment.xlsx", sheet = "Appendix 4")


#4. Merge dataframes of appendices----
First_merge <- merge(Demographics, appendix2, by.x = "Country", 
                      by.y = "Country", all.x = TRUE, all.y = FALSE)

Second_merge <-merge(First_merge, appendix3, by.x = "Language", 
                      by.y = "Language", all.x = TRUE, all.y = FALSE)

Demographics_With_Merges <- merge(Second_merge, appendix4, by.x = "ApplicationID", 
                      by.y = "ApplicationID", all.x = TRUE, all.y = FALSE)


#5. Create 'StartingAfter' refering to the days between registration and first bet of any kind----
Demographics_With_Merges$StartingAfter <- difftime(Demographics_With_Merges$FirstAct,
                                                        Demographics_With_Merges$RegDate, units='days')



#6. Filter by conditions: Opened an account between Feb 1, 2005 and Feb 27, 2005 ----


Demographics_filter1 <- Demographics_With_Merges[(Demographics_With_Merges$RegDate> "2005-01-31" & 
                                                   Demographics_With_Merges$RegDate < "2005-02-28"),]

#7. Filter by conditions: Not missing values in First Act ----
Demographics_filter2 <- Demographics_filter1 %>% filter(!is.na(FirstAct))

#8. replace NA in gender with mode of the category ----
# 8.1. Create the function for mode.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# 8.2. replace na with the mode function for gender
Demographics_filter2$Gender <- replace_na(Demographics_filter2$Gender, getmode(Demographics_filter2$Gender))


#8.3. check
print(sum(is.na(Demographics_filter2$FirstAct)))

#9. Re-order the columns (numeric to right, strings in the left)----
Demographics_arranged <- Demographics_filter2[,c(4,5,6,7,8,9,10,11,16,12,1,15,2,14,3,13)]

#10. Sort by Registration Date ----
Demographics_cleaned <- Demographics_arranged[order(Demographics_arranged$RegDate,Demographics_arranged$UserID ),]


#11. Final table (number of rows correspond to the total number of observations of the final basetable) ----
summary(Demographics_cleaned)

#12. Save the file in csv (easier to handle when merging everything together.) ----
write.csv(Demographics_cleaned,'Demographics_by_User.csv')