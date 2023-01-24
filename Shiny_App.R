#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

#------------------------------Loading libraries and Data-----------------------

library(readr)
#library(shiny)
library(plotly)
library(dplyr)
library(ggplot2)
library(readxl)
library(shinythemes)
library(shinydashboard)
library(lubridate)
library(bubbles)

#Reading the data mart

#setwd("C:/Users/lramirezsanchez/OneDrive - IESEG/Clases/S1/Business Analytical Tools Open Source/Group assignment/Sassy R/Data_Mart/App")
data<-read.csv("Marketing_Insights.csv")


#-----------------------------Preparing the data for graphics-------------------

#..........................Creating a custom palette............................

#source https://meghan.rbind.io/blog/2022-10-11-creating-custom-color-palettes-with-ggplot2/
shiny_colors <- function(...) {
  
  shiny_colors <- c(
    `bwin`     = "#ffcc00",
    `bwinlight` = "#ffed80",
    `orange`     = "#ffc66a",
    `green`    = "#eee19e",
    `yellow`   = "#ffea9d",
    `whiteish`="#fffecc",
    `grey`="#8e8685",
    `red`="#ff7363",
    `pink`="#ff8277",
    `salmon`="#ffd9b8"
    
  )
  
  cols <- c(...)
  
  if (is.null(cols))
    return (shiny_colors)
  
  shiny_colors[cols]
}

shiny_palette <- function(palette = "main", ...) {
  
  shiny_palette <- list(
    `main` = shiny_colors("bwin", "bwinlight", "orange", "grey"),
    `yellows`=shiny_colors("bwin","bwinlight","orange","green","yellow","whiteish"),
    `highlight` = shiny_colors("bwin", "grey","red"),
    `all`=shiny_colors("bwin","orange","bwinlight","grey","green","red","yellow","whiteish","pink","salmon")
  )
  
  shiny_palette[[palette]]
  
}


#.............................Transforming variables type ......................

#Transforming dates

#RegDate
data$RegDate<-ymd(data$RegDate)
data$RegDay<-weekdays(data$RegDate)

#FirstPay
data$FirstPay<-ymd(data$FirstPay)

#Date_Initial_AllGames
data$Date_Initial_AllGames<-ymd(data$Date_Initial_AllGames)

#Date_Final_AllGames
data$Date_Final_AllGames<-ymd(data$Date_Final_AllGames)


#Transforming categorical variables into factors

#Transforming Gender to a categorical variable
data$Gender<-factor(data$Gender)

#Transforming language variables into factors
data$Language_Description<-factor(data$Language_Description)

#Tranfomring Country names into factors
data$Country_Name<-factor(data$Country_Name)

#Transforming cluster into factors 
data$cluster<- factor(data$cluster)


#..........................Preparing data for map...............................
#get country codes
codes <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')
codes<-subset(codes,select = -c(GDP..BILLIONS.))

map<- data%>%count(Country_Name)
map$perc<-paste(round((map$n/sum(map$n)*100),2),"%")

#merges map with codes 
map<-merge(map,codes,all.x = TRUE,by.x = c("Country_Name"),by.y = c("COUNTRY"))
#find missing values
map[is.na(map$CODE),]

#Replace known countries with their codes
map$CODE<-replace(map$CODE,map$Country_Name=="FYR Macedonia","MKD")
map$CODE<-replace(map$CODE,map$Country_Name=="Holland","NLD")
map$CODE<-replace(map$CODE,map$Country_Name=="Moldavia","MDA")
map$CODE<-replace(map$CODE,map$Country_Name=="Russian Federation","RUS")
map$CODE<-replace(map$CODE,map$Country_Name=="Serbia and Montenegro","SRB")
map$CODE<-replace(map$CODE,map$Country_Name=="Tunesia","TUN")
map$CODE<-replace(map$CODE,map$Country_Name=="United States Virgin Islands","VGB")
map$CODE<-replace(map$CODE,map$Country_Name=="USA","USA")
map$Country_Name<-replace(map$Country_Name,map$Country_Name=="USA","United States")

#Put all the country codes
map<-merge(map,codes,by.x = c("CODE"),by.y = c("CODE"),all=TRUE)

#Replace NA with 0
map$n <- replace(map$n,is.na(map$n),0)
map$perc<-replace(map$perc,is.na(map$perc),"0%")

#........................Preparing data barchart Gender........................
gender<-data%>%count(Gender)
gender$perc<-gender$n/sum(gender$n)

#........................Preparing data for language chart......................
bubble<-data%>%count(Language_Description)
bubble$perc<-paste(round((bubble$n/sum(bubble$n)*100),2),"%")

#........................Preparing data barchart cluster........................
clusters<-data%>%count(cluster)
clusters$perc<-clusters$n/sum(clusters$n)

#........................Preparing data Treemap sites...........................

tree_data<-data%>%count(Application_Description)
tree_data$perc<-paste(round((tree_data$n/sum(tree_data$n)*100),2),"%")
#........................Preparing data First Payment Chart.....................
fp<-data%>%arrange(FirstPay)%>%group_by(FirstPay)%>%summarise(
  total_users=n(),
  .groups = "drop"
)

#.......................Preparing data registration activity ...................
data$RegDay<-weekdays(data$RegDate)
data$RegDay<-factor(data$RegDay,levels=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))

#......................Preparing data Inactivity vs first payment...............
beg_ina<-data%>%arrange(FirstPay)%>%group_by(month(FirstPay,label = TRUE))%>%summarise(
  avg_ina=round(mean(Beginner_Inactivity),2),
  count_fp=n(),
  .groups = "drop"
)
beg_ina <- beg_ina %>% rename(FirstPay="month(FirstPay, label = TRUE)")



#.....................Preparing data for CLV chart.............................
clv_or<-data%>%group_by(Application_Description)%>%summarise(
  avg_clv=round(mean(CLV),2),
  avg_or=round(mean(Operating_rate),2),
  count=n(),
  .groups = "drop"
)

#....................Preparing data for engagement chart........................
engt<-data%>%group_by(Application_Description)%>%summarise(
  avg_eng=round(mean(Engagement_rate),2),
  count=n())
engt<-engt%>%arrange(desc(avg_eng))

#....................Preparing data for Frequency by cluster chart .............
clu<-data%>%group_by(cluster,Gender)%>%summarise(
  count=n(),
  .groups = "drop"
)

clu<-clu %>% pivot_wider(names_from = Gender, values_from = count)

clu<-clu%>%rename(Female="0",Male="1")


freq<-data%>%group_by(cluster)%>%summarise(
  avg_freq=round(mean(Frequency),2),
  .groups = "drop"
)

#......................Preparing Data LoR and monetary..........................
data$Monetary<-round(data$Monetary,2)



#-------------------------Builidng the app--------------------------------------

#..............................UI...............................................
ui <- navbarPage("Bwin",theme = shinytheme("cosmo"),
 tabPanel("Our clients",
          fluidRow(
            valueBoxOutput("TotalUsers",width = 2),
            valueBoxOutput("AverageLoR",width = 2),
            valueBoxOutput("Avg.Spendbyuser",width = 2),
            valueBoxOutput("AverageAPPU",width = 2),
            valueBoxOutput("AverageEngagementRate",width=3)
          ),
          fluidRow(
            plotlyOutput("map")
          ),
          fluidRow(
            column(6, style='padding-bottom:10px;',h3("Languages of the user"),
                   bubblesOutput("Language",width = "400px", height = "400px")),
            column(6,style='padding-bottom:10px;',
            plotlyOutput("gender",width = "400px",
                         height = "400px")
            )
                    ),
          fluidRow(
            column(6,style='padding-bottom:10px;',h3("Access site of the user"),
                   plotlyOutput("tree_map")),
            column(6,
                   plotlyOutput("type_user")
            )),
          fluidRow(
            column(6,style='padding-bottom:10px;',
              plotlyOutput("firstpayment")),
              column(
                6,style='padding-bottom:10px;',
                plotlyOutput("regdays")
            )
           )),
 tabPanel("Insights",
          fluidRow(
            column(
              6,style='padding-bottom:10px;',
              plotlyOutput("begina")
            ),
            column(6,style='padding-bottom:10px;',
              plotlyOutput("APPU")
            )
            
          ),
          fluidRow(
            column(6,style='padding-bottom:10px;',
              plotlyOutput("CLV")
            ),
            column(6,style='padding-bottom:10px;',
              plotlyOutput("ENG")
                   )
          )),
    
  tabPanel("Type of user",
           fluidRow(
             column(6,style='padding-bottom:10px;',
                    plotlyOutput("fclu")
               
             ),
             column(6,style='padding-bottom:10px;',
                    plotlyOutput("LVM")
                    )
           ),
           
           fluidRow(
             fluidRow(
             hola<-selectInput("filterrfm","Select an option to visualize:",choices=c("LoR","Monetary","Recency")),
             
           ),
           fluidRow(plotlyOutput("RFM"))
           )
          
   
 )
 )
             
                  

#..................................Server.......................................
server <- function(input, output) {

#................................Our clients....................................    
  #KPI cards 
  output$TotalUsers<- renderValueBox({
    valueBox(format(length(unique(data$UserID)),nsmall=1, big.mark = ","),
      "Total Users")
  })
    
   output$AverageLoR<-renderValueBox({
     valueBox(round(mean(data$LoR),2),"Average LoR" )
   })
   
   output$Avg.Spendbyuser<-renderValueBox({
     valueBox(paste("$",round(mean(data$Monetary),2)),"Avg.Sped by user")
     })
   output$AverageAPPU<-renderValueBox({
     valueBox(paste("$",round(mean(data$APPU),2)),"Average APPU")
   })
   
   output$AverageEngagementRate<-renderValueBox({
     valueBox(round(mean(data$Engagement_rate),2),"Average engagement rate")
   })
  #map
   output$map<-renderPlotly({
     l <- list(color = toRGB("grey"), width = 0.5)
     
     g <- list(
       showframe = FALSE,
       showcoastlines = FALSE,
       projection = list(type = 'Mercator')
     )
     
     fig <- plot_geo(map)
     fig <- fig %>% add_trace(
       z = ~n, color = ~n, colors = "YlOrRd",
       text = ~COUNTRY, locations = ~CODE, marker = list(line = l)
     )
     
     fig <- fig %>% colorbar(title = 'Number of people')
     fig <- fig %>% layout(
       title = 'Users per Country',
       geo = g
     )
     
     fig
     
   })
  
  #Gender histogram
  output$gender<-renderPlotly({
    
    gender_chart<-ggplot(gender,aes(x=Gender,y=perc,fill=Gender))+geom_bar(stat="sum",show.legend = TRUE)+
      labs(x="Gender",y="Proportion")+
      ggtitle("Gender of the users")+
      scale_x_discrete(labels = c('Women','Men'))+
      scale_fill_manual(values = unname(c(shiny_colors("bwinlight","bwin"))))+
      geom_text(aes(label=scales::percent(perc)), position = position_dodge(0.9))+
      scale_y_continuous(labels = scales::percent)+
      theme_classic()
    
    
    ggplotly(gender_chart,tooltip ="Gender <br> perc")%>% plotly::hide_legend()
  })
  
  #Main language of the user
  output$Language<-renderBubbles({
    bubbles(sqrt(bubble$n), paste(bubble$Language_Description), tooltip = bubble$perc, color = get_palette(unname(c(shiny_palette("yellows"))),17),
            textColor = "#333333")
  })
  
  #Type of user
  output$type_user<-renderPlotly({
    
    type_user<-ggplot(clusters,aes(x=cluster,y=perc,fill=cluster))+geom_bar(stat="sum",show.legend = TRUE)+
      labs(x="Type of user",y="Proportion")+
      ggtitle("Type of users")+
      scale_fill_manual(values = unname(c(shiny_colors("bwin","bwinlight", "green", "orange", "grey"))))+
      geom_text(aes(label=scales::percent(perc)), position = position_dodge(0.9))+
      scale_y_continuous(labels = scales::percent)+
      theme_classic()
    
    
    
    ggplotly(type_user)%>% plotly::hide_legend()
  })
  
  #Origin site
  output$tree_map<-renderPlotly({
    tm <- plot_ly(
      tree_data,
      labels = ~paste(Application_Description,perc,sep="/n"),
      parents = NA,
      values = ~ n,
      type = 'treemap',
      hovertemplate = "Site: %{label}<br>Number of users: %{value}<extra></extra>",
      marker=list(colors=get_palette(unname(c(shiny_palette("all"))),25))
    )
   
    tm
  })
  
  #First payment behavior over a year
  output$firstpayment<-renderPlotly({
    fig <- plot_ly(fp, type = 'scatter', mode = 'lines',line=list(color="black"))%>%
      add_trace(x = ~FirstPay, y = ~total_users)%>%
      layout(title="First payment over a year",xaxis = list(title = "Date of first payment"),yaxis = list(title = "Number of users"),showlegend = F)
    fig
    
    
  })
  #Registration activity per day
  output$regdays<-renderPlotly({
    plot_ly(data, x = data$RegDay, type = "histogram",marker = list(color = get_palette(unname(c(shiny_palette("yellows"))),7)))%>%
      layout(title="Registration activity trend per day ",xaxis = list(title = "Day of the week"),yaxis = list(title = "Number of users"),showlegend = F)
  })
 
  
#...................................Insights....................................   
 #Inactivity vs first payment
  output$begina<-renderPlotly({
    ay <- list(
      overlaying = "y",
      side = "right",
      title = "Number of first payments"
    )
    ch_bina<-plot_ly(beg_ina, x = ~FirstPay, y = ~avg_ina, type = 'bar',name="Avg.Inactivity",
                     marker = list(color = get_palette(unname(c(shiny_palette("yellows"))),9)),
                     transforms=list(
                       list(
                         type='aggregate',
                         aggregations=list(
                           list(
                             target='y',func='avg',enabled=T
                           )
                         )
                       )
                     ))
    
    ch_bina<-ch_bina%>%add_lines(
      x = ~FirstPay,
      y = ~count_fp,
      name="Number of first payments",
      yaxis="y2",
      type='scatter',
      mode='lines+markers',
      line = list(color = 'black')
    )
    ch_bina <- ch_bina %>% layout(yaxis2 = ay,title = 'Average days of inactivity before first payment',
                                  xaxis = list(showgrid = FALSE, showticklabels=FALSE, title = "Month"),
                                  yaxis = list(showgrid = FALSE, title = "Average Inactivity (days)"),
                                  showlegend = TRUE)
    
    ch_bina
  })
  #Average APPU country name and application description
  output$APPU<-renderPlotly({
    
    #.....................Preparing data for APPU chart.............................
  
    
    ch_appu<-data%>%group_by(Country_Name,Application_Description)%>%summarise(
      avg_appu=round(mean(APPU),2),
      count=n())
    
    ch_appu<-ch_appu %>%arrange(desc(avg_appu),Country_Name,.by_group = TRUE)
    ch_appu <-head(ch_appu,20)
    
    
    
    #creating plot
    appu <- plot_ly(ch_appu, x = ~avg_appu, y = ~Country_Name, type = 'bar', orientation = 'h',
                    text=~paste(Application_Description  ),
                    marker = list(color = get_palette(unname(c(shiny_palette("yellows"))),20))) 
    appu <- appu %>% add_trace(x = ~avg_appu,marker = list(color = get_palette(unname(c(shiny_palette("yellows"))),20))) 
    appu <- appu %>% layout(title = "APPU by country and origin platform of the user ",
                            xaxis = list(title = "APPU",
                                         showgrid = TRUE,
                                         showline = TRUE,
                                         showticklabels = TRUE,
                                         zeroline = TRUE,
                                         domain = c(0.15, 1)),
                            yaxis = list(title = "Country",
                                         showgrid = TRUE,
                                         showline = TRUE,
                                         showticklabels = TRUE,
                                         zeroline = TRUE),
                            barmode = 'stack',
                            margin = list(l = 120, r = 10, t = 140, b = 80),
                        showlegend = FALSE) 
    appu
    
  })
  
  #Average of CLV and operating rate
  output$CLV<-renderPlotly({
    ay <- list(
      overlaying = "y",
      side = "right",
      title = "Number of first payments"
    )
    ch_clv_or<-plot_ly(clv_or, x = ~Application_Description, y = ~avg_clv, type = 'bar',name="Avg.Inactivity",
                       marker = list(color = get_palette(unname(c(shiny_palette("yellows"))),22)),
                       transforms=list(
                         list(
                           type='aggregate',
                           aggregations=list(
                             list(
                               target='y',func='avg',enabled=T
                             )
                           )
                         )
                       ))
    
    ch_clv_or<-ch_clv_or%>%add_lines(
      x = ~Application_Description,
      y = ~avg_or,
      name="Number of first payments",
      yaxis="y2",
      type='scatter',
      mode='lines+markers',
      line = list(color = 'black')
    )
    ch_clv_or <- ch_clv_or %>% layout(yaxis2 = ay,title = 'Average days of inactivity before first payment',
                                      xaxis = list(showgrid = FALSE, showticklabels=FALSE, title = "Month"),
                                      yaxis = list(showgrid = FALSE, title = "Average Inactivity (days)"),
                                      showlegend = TRUE)
    
    ch_clv_or
    
    
  })
  
  #Funnel engagement rate
  output$ENG<-renderPlotly({
    
    eng <- plot_ly(engt) 
    eng <- eng %>%
      add_trace(
        type = "funnel",
        y = ~Application_Description,
        x = ~avg_eng,
        marker = list(color = get_palette(unname(c(shiny_palette("yellows"))),22)))
    eng <- eng %>%
      layout(yaxis = list(categoryarray = c("Website visit", "Downloads", "Potential customers", "Requested price", "invoice sent")))
    
    eng
    
  })
  
  #.................................Type of user................................
  
  output$fclu<-renderPlotly({
    plotlyOutput("LVM")
    ay <- list(
      overlaying = "y",
      side = "right",
      title = "Number of first payments"
    )
    fqnc<-plot_ly(clu, x = ~cluster, y = ~Male, type = 'bar',name="Avg.Inactivity",
                  marker = list(color = get_palette(unname(c(shiny_palette("yellows"))),22)),
                  transforms=list(
                    list(
                      type='aggregate',
                      aggregations=list(
                        list(
                          target='y',func='avg',enabled=T
                        )
                      )
                    )
                  ))
    fqnc <- fqnc %>% add_trace(y = ~Female,marker = list(color = get_palette(unname(c(shiny_colors("green"))),22))) 
    fqnc <- fqnc %>% layout(xaxis = list(title = "",
                                         showgrid = TRUE,
                                         showline = TRUE,
                                         showticklabels = TRUE,
                                         zeroline = TRUE,
                                         domain = c(0.15, 1)),
                            yaxis = list(title = "",
                                         showgrid = TRUE,
                                         showline = TRUE,
                                         showticklabels = TRUE,
                                         zeroline = TRUE),
                            barmode = 'stack',
                            margin = list(l = 120, r = 10, t = 140, b = 80),
                            showlegend = TRUE) 
    
    fqnc<-fqnc%>%add_lines(data = freq,
                           x = ~cluster,
                           y = ~avg_freq,
                           name="Number of first payments",
                           yaxis="y2",
                           type='scatter',
                           mode='lines',
                           line = list(color = 'black')
    )
    fqnc <- fqnc %>% layout(yaxis2 = ay,title = 'Average days of inactivity before first payment',
                            xaxis = list(showgrid = FALSE, showticklabels=TRUE, title = "Cluster",
                                         categoryorder = "category ascending"),
                            yaxis = list(showgrid = FALSE, title = "Average Inactivity (days)"),
                            showlegend = FALSE)
    
    
    fqnc
    
  })
  output$LVM<-renderPlotly({
    plot_ly(data, x =~LoR , y = ~Monetary,color=~cluster, type="scatter",mode="markers")
    
    
  })
  
  output$RFM<-renderPlotly({
    if (input$filterrfm == "LoR") {
      op <- data$LoR
    } else if (input$filterrfm == "Monetary"){  
      op<-data$Monetary
    } else {
      op <- data$Recency
    }
    
    plot_ly(data, x = ~cluster, y = op, type = 'bar',
            marker = list(color = get_palette(unname(c(shiny_palette("yellows"))),4)),
            transforms=list(
      list(
        type='aggregate',
        groups=data$cluster,
        aggregations=list(
          list(
            target='y',func='avg',enabled=T
          )
        )
      )
    ))
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


