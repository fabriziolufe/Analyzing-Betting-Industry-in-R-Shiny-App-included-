library(dplyr)
library(readxl)
library(stringr)
library("tidyr")
library("ggplot2")
library(ggpubr)
#------------------------------------------------------------------------------------------------
setwd("C:/Users/flucerofernandez/Documents/IESEG S01/R/GP/...")
dir()
#Initial configuration --------------------------------------------------------------------------
draft_table <- read.csv("Data_Mart_Raw.csv")
#1.1. Poker Stakes and Winnings grouped together
draft_table$PokerStakes <- draft_table %>% dplyr:: select(starts_with("Sumbuy")) %>%
  rowSums(na.rm = TRUE)

draft_table$PokerWins <- draft_table %>% dplyr:: select(starts_with("Sumsell")) %>%
  rowSums(na.rm = TRUE)

## also calculating the total frequency for poker activity
draft_table$N_PokerStakes <- draft_table %>% dplyr:: select(starts_with("Countbuy")) %>%
  rowSums(na.rm = TRUE)

draft_table$N_PokerWins <- draft_table %>% dplyr:: select(starts_with("Countsell")) %>%
  rowSums(na.rm = TRUE)

#1.2. Calculate global user total stakes and winnings: Poker + + Sports + Other products
Benefits <- draft_table %>% select(UserID,starts_with("Stakes_P"), PokerStakes,N_PokerStakes, Bets, Bets_P1, Bets_P2) %>% 
  transmute( UserID=UserID,
             Bets=Bets,
             Bets_Poker=N_PokerStakes,
             Bets_Sports= Bets_P1+Bets_P2,
             Sports_Benefits= Stakes_P1 + Stakes_P2,
             Other_Benefits = rowSums(.[3:8]),
             Poker_Benefits= PokerStakes)

Expenses <- draft_table %>% select(UserID,starts_with("Wins_P"), PokerWins, Bets) %>% 
  transmute( UserID=UserID,
             Bets=Bets,
             Sports_Expenses= Wins_P1 + Wins_P2,
             Other_Expenses = rowSums(.[3:8]),
             Poker_Expenses= PokerWins)

Benefits$TOTAL_BENEFITS <- rowSums(Benefits[5:7],na.rm=TRUE)
Expenses$TOTAL_EXPENSES <- rowSums(Expenses[3:5],na.rm=TRUE)
#LEAVING THE CUSTOMER ID With insightful marketing metrics.
Marketing_Insights <- draft_table %>% select(UserID,StartingAfter, Index_Recency, Index_LoR)
#Rename it to a more "marketingly" name
Marketing_Insights <- Marketing_Insights %>% 
  rename("Beginner_Inactivity"="StartingAfter",
         "Recency"="Index_Recency",
         "LoR"="Index_LoR")
#ADITIONAL MEASURES. 
#1. CUSTOMER LIFETIME VALUE --------------------------------------------------------------------
#SOURCE: https://www.leadsquared.com/learn/sales/clv-customer-lifetime-value/
#1.3.Calculating average churn rate... taking as a churner someone who stopped betting in Sept 2005.
draft_table$Date_Final_AllGames<- ifelse(is.na(draft_table$Date_Final_AllGames),
                                         draft_table$Date_Final_Poker,draft_table$Date_Final_AllGames)
draft_table$Customer_continues <- ifelse(draft_table$Date_Final_AllGames<'2005-09-01',0,1)

CHURN_RATE <- sum(draft_table$Customer_continues, na.rm=TRUE)/ nrow(draft_table)

#1.4 Merging Benefits and expenses

Revenue_Customers <- merge(Benefits, Expenses, by= c('UserID','Bets'), 
      all.x = TRUE, all.y = FALSE)

Revenue_Customers$CLV <- format(round((Revenue_Customers$TOTAL_BENEFITS - Revenue_Customers$TOTAL_EXPENSES)/CHURN_RATE,2), 
                                scientific = FALSE)

#2. APPU -----------------------------------------------------------------------------------
#Average Profit per use ( for every customer, taking into account the frequency of use.)
Revenue_Customers$TOTAL_BETS<- ifelse(is.na(Revenue_Customers$Bets),0,Revenue_Customers$Bets)+Revenue_Customers$Bets_Poker
Revenue_Customers$APPU <- format(round((Revenue_Customers$TOTAL_BENEFITS - Revenue_Customers$TOTAL_EXPENSES)/(Revenue_Customers$TOTAL_BETS),2), 
                                scientific = FALSE)

#3. ENGAGEMENT_RATE----------------------------------------------------------------------------------
#Source: https://clevertap.com/blog/how-to-measure-customer-loyalty/
#Here we want to take the frequency of total bets and compare with the average. 
#let's plot to see the bets distribution:

bets<-ggplot(Revenue_Customers, aes(x=TOTAL_BETS)) + 
  geom_histogram(color="black", fill="white", bins=3)
bets# doesn't show a normal distribution, using the median is more secure.
median_engagement <- median(ifelse(is.na(Revenue_Customers$Bets),0,Revenue_Customers$Bets)+Revenue_Customers$Bets_Poker)
Revenue_Customers$Engagement_rate <- round(((ifelse(is.na(Revenue_Customers$Bets),0,Revenue_Customers$Bets)+Revenue_Customers$Bets_Poker))/median_engagement,2)

#4. SportsvRest_RATE---------------------------------------------------------------------------------
#Since Sports bets are the most popular, let's compare the users by this metric.
Revenue_Customers$SvR_rate <- round(ifelse(is.na(Revenue_Customers$Bets_Sports),0,Revenue_Customers$Bets_Sports)/Revenue_Customers$TOTAL_BETS,2)

#5. Operating_RATE ----------------------------------------------------------------------------------
#expenses/revenues
Revenue_Customers$Operating_rate <- round(ifelse(Revenue_Customers$TOTAL_BENEFITS==0,0,Revenue_Customers$TOTAL_EXPENSES/(Revenue_Customers$TOTAL_EXPENSES+Revenue_Customers$TOTAL_BENEFITS)),2)


#select only the final stats.
Revenue_Customers <- Revenue_Customers %>% select(UserID,TOTAL_BENEFITS,TOTAL_BETS,CLV,APPU, Operating_rate,Engagement_rate, SvR_rate)
#NA'S 
#Replace values with Na's in operating rate to 0 ( due to a multiplication of 0/0)
Revenue_Customers$Operating_rate <- ifelse(is.na(Revenue_Customers$Operating_rate),0,Revenue_Customers$Operating_rate)


#Merge into final statistics.-----------------------------------------------------------------------
Marketing_Insights <- merge(Marketing_Insights, Revenue_Customers, by= c('UserID'), 
                   all.x = TRUE, all.y = FALSE)

#Rename TOTAL_BENEFITS TO Monetary
Marketing_Insights <- Marketing_Insights %>% 
  rename("Monetary"="TOTAL_BENEFITS",
         "Frequency"="TOTAL_BETS")

#NA'S 
#Change LoR of the person who betted in the last day to 1
Marketing_Insights$LoR <- ifelse(Marketing_Insights$UserID ==1348430,1,Marketing_Insights$LoR  )
Marketing_Insights <- Marketing_Insights %>% drop_na()

#CLUSTER ------------------------------------------------------------------------------------------

#SOURCE: https://www.statology.org/elbow-method-in-r/#:~:text=One%20of%20the%20most%20common%20ways%20to%20choose%20a%20value,bend%20appears%20in%20the%20plot.
library(cluster)
library(factoextra)

#standardize
Marketing_Insights <- Marketing_Insights %>% mutate_if(is.character, as.numeric)
std_df <- scale(subset( Marketing_Insights, select = -UserID ))
#create plot of number of clusters vs total within sum of squares
wss <- sapply(2:10, function(k){kmeans(std_df, k, nstart=5 )$tot.withinss})
plot(2:10, wss, type="b", pch = 19,  xlab="Number of clusters K", ylab="Total within-clusters sum of squares")

#make this example reproducible
set.seed(1)

#perform k-means clustering with k = 4 clusters
km <- kmeans(std_df, centers = 4, nstart = 25)

Marketing_Insights <- cbind(Marketing_Insights, cluster = km$cluster)

#Plot characteristics per Cluster
CLUSTER_DF<- Marketing_Insights%>% 
  group_by(cluster) %>%
  summarise (Beginner_Inactivity=mean(Beginner_Inactivity, na.rm=TRUE),
             Recency=mean(Recency, na.rm=TRUE),
             LoR=mean(LoR, na.rm=TRUE),
             Monetary=mean(Monetary, na.rm=TRUE),
             CLV=mean(CLV, na.rm=TRUE),
             APPU=mean(APPU, na.rm=TRUE),
             Operating_rate=mean(Operating_rate, na.rm=TRUE),
             Engagement_rate=mean(Engagement_rate, na.rm=TRUE),
             SvR_rate=mean(SvR_rate, na.rm=TRUE),
             .groups = 'keep')

#Plot the averages between clusters
plot_BI<- ggplot(CLUSTER_DF, aes(x=cluster, y=Beginner_Inactivity, color=cluster)) + geom_point()+theme(legend.position='top')
plot_Recency<- ggplot(CLUSTER_DF, aes(x=cluster, y=Recency, color=cluster)) + geom_point()+theme(legend.position='top')
plot_LoR<- ggplot(CLUSTER_DF, aes(x=cluster, y=LoR, color=cluster)) + geom_point()+theme(legend.position='top')
plot_Monetary<- ggplot(CLUSTER_DF, aes(x=cluster, y=Monetary, color=cluster)) + geom_point()+theme(legend.position='top')
plot_CLV <- ggplot(CLUSTER_DF, aes(x=cluster, y=CLV, color=cluster)) + geom_point()+theme(legend.position='top')
plot_APPU<- ggplot(CLUSTER_DF, aes(x=cluster, y=APPU, color=cluster)) + geom_point()+theme(legend.position='top')
plot_Operating_rate<- ggplot(CLUSTER_DF, aes(x=cluster, y=Operating_rate, color=cluster)) + geom_point()+theme(legend.position='top')
plot_Engagement_rate<- ggplot(CLUSTER_DF, aes(x=cluster, y=Engagement_rate, color=cluster)) + geom_point()+theme(legend.position='top')
plot_SvR_rate<- ggplot(CLUSTER_DF, aes(x=cluster, y=SvR_rate, color=cluster)) + geom_point()+theme(legend.position='top')
#Plot them together
if(!require(devtools)) install.packages("devtools")

ggarrange(plot_BI, plot_Recency, plot_LoR, plot_Monetary, plot_CLV, plot_APPU, plot_Operating_rate, plot_Engagement_rate, plot_SvR_rate,
          labels = c("Beg_Inactivity", "Recency","Length of Relation", "Monetary","CLV","APPU","Op_rate","Eng_rate","SvR_rate"),
          ncol=3, nrow=3)
#Rename the cluster according to the plot results ( give names) ...
#CLUSTER 1<- Low Recency, High LoR, High Monetary, High CLV, Low SvR Rate <- "Big Fixed Bettors" 
#CLUSTER 2<- Low Recency, High LoR, Low Monetary, Low CLV, High SvR Rate <- "Sports Enthusiasts" 
#CLUSTER 3<- High Beg Innacitivty, medium APPU, high SvR rate, Low LoR <- "Uncertain gamblers" 
#CLUSTER 4<- Low Beg_Innac, High Recency, Low Operating Rate <- "Leaving winners"

Marketing_Insights$cluster <-  ifelse(Marketing_Insights$cluster==1, "Big Fixed Bettors", 
         ifelse(Marketing_Insights$cluster==2,"Sports Enthusiasts",
                ifelse(Marketing_Insights$cluster==3,"Uncertain gamblers" ,
                       "Leaving winners")))

##Save cleaned and transformed table - join with demographics-------------------------------------------------------
Draft_datamart <- read.csv("./Data_Mart_Raw.csv")
Draft_datamart <- Draft_datamart %>% select(UserID,RegDate,FirstPay,Gender,ApplicationID, Application_Description,Language, Language_Description,
                                            Country, Country_Name, Date_Initial_AllGames, Date_Final_AllGames)
Marketing_Insights <- merge( Draft_datamart, Marketing_Insights, by= "UserID", 
                          all.x = FALSE, all.y = TRUE)

#Result---------------------------------------------------------
write.csv(Marketing_Insights,'Marketing_Insights.csv')
