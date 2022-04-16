
library(shiny)
library(lubridate)
library(ggplot2)
library(plotly)
library(shinythemes)
library(shiny)
library(stringdist)
library(igraph)
library(ggmap)
library(revgeo)
library(googleway)
library(DT)
library(leaflet)
library(leaflet.providers)

shinyApp(
  ui <- navbarPage(title = "Osprey Wilds Charter Schools",theme = shinytheme("flatly"),
                   
                   
                   tabPanel("Home",titlePanel("Academic Performance Tracker"),fluidRow(), tags$img(src= "https://i0.wp.com/econometricus.wpcomstaging.com/wp-content/uploads/2022/03/school-kids-running-to-camera-in-elementary-school-2021-08-26-16-14-10-utc.jpg?resize=1200%2C800&ssl=1",height=800, width=1400)),
                   
                   tabPanel("Our 34 School",fluidRow(column(2,
                                                            
                                                            fileInput("HCP_File", "Upload csv file", accept = c(".csv")),
                                                            textInput("Org_ID", label = "What School do you want to learn about?"),
                                                            
                                                            
                                                            actionButton("go_3", "Search by School Name", class = "btn-warning")),
                                                     #downloadLink("downloadData", "Download"),
                                                     #textInput("HCP_First", label = "Who do you want to find affiliation for?"),
                                                     #numericInput("NPI", label = "What Organization NPI do you want to find affiliation for?", value = ""),
                                                     #actionButton("Buscar_NPI", "Search by Org NPI"),
                                                     #numericInput("NPI_1", label = "Which HCP NPI do you want to find affiliation for?", value = "")),
                                                     column(9,tabsetPanel(type = "pills",
                                                                          
                                                                          tabPanel("Schools",DTOutput("Names")),
                                                                          tabPanel("Affiliations",plotOutput("Plot_NPI_1")))))),
                   
                   tabPanel("Geo Location",fluidRow(column(2,fileInput("Geo_File", "Upload csv file", accept = c(".csv")),
                                                           selectInput("HCP_Speciality","School Specialty", choices = c("Experimental Learning",
                                                                                                                        "Arts and Music",
                                                                                                                        "Developmental - Behavioral Pediatrics",
                                                                                                                        "Child Development",
                                                                                                                        "Stem",
                                                                                                                        "Neonatal Program")),
                                                           actionButton("go_places","Show Locations", class = "btn-warning"),
                                                           
                                                           
                   ),
                   column(2,leafletOutput("mymap",height=1000,width = 1600), offset = .5))),
                   
                   
                   tabPanel("Academic Performance", fluidRow(column(2,
                                                                     fileInput("TOFU_File", "Upload csv file", accept = c(".csv")),
                                                                     
                                                                     selectInput("Operation", "Select your Preferred Statistic",c("Median", "Mean", "Mode")),
                                                                     selectInput("Ad_Set_Name", "Choose Subject Set", c("TOFU | LAL | Leads 0-5% | Lead",
                                                                                                                   "TOFU | Interest Stack | Fast Foods | Lead",
                                                                                                                   "TOFU | Broad | Lead",
                                                                                                                   "TOFU | Interest Stack | Competitors | Lead",
                                                                                                                   "TOFU | LAL | Accepted Offer 0-10% | Lead",
                                                                                                                   "TOFU | LAL | Site Traffic 1% | Lead",
                                                                                                                   "TOFU | Interest Stack | Car Repair & Maintenance",
                                                                                                                   "TOFU | Interest Stack | Old Cars",
                                                                                                                   "TOFU | Interest Stack | Hardware Stores",
                                                                                                                   "TOFU | Interest Stack | Retail Stores",
                                                                                                                   "TOFU | Interest Stack | Car Brands",
                                                                                                                   "TOFU | Interest Stack | Car Types (General)",
                                                                                                                   "TOFU | Interest Stack | Kids",
                                                                                                                   "TOFU | Interest Stack | DIY",
                                                                                                                   "TOFU | Interest Stack | Supermarkets",
                                                                                                                   "TOFU | Interest Stack | Clothing / Apparel",
                                                                                                                   "TOFU | Broad | Lead | Cost Cap $8",
                                                                                                                   "TOFU | Broad | Lead | Cost Cap $10",
                                                                                                                   "TOFU | Broad | Lead | Cost Cap $15",
                                                                                                                   "TOFU | Broad | Lead | Bid Cap $100",
                                                                                                                   "TOFU | Broad | Lead | Bid Cap $50",
                                                                                                                   "TOFU | Broad | Lead | Bid Cap $25",
                                                                                                                   "Peddle - Finished Quote But Didn't Sell - Story",
                                                                                                                   "Peddle - Started Quote But Didn't Finish/Sell 60d - Story")),
                                                                     checkboxGroupInput("Campaing_Name", "Choose Subject", c("HS | ToFu | Phase 2 Test + Learn | Sprint 4 | Audience Testing",
                                                                                                                              "HS | ToFu | Phase 2 Test + Learn | Sprint 4 | Audience Testing | Cost Cap",
                                                                                                                              "HS | ToFu | Phase 2 Test + Learn | Sprint 4 | Audience Testing | Bid Cap",
                                                                                                                              "HS | MoFu | Phase 2 Test + Learn | Sprint 1 | Creative Testing")),
                                                                     radioButtons(inputId = "Down_Type", label = "Select download type file", choices = list("png","pdf")),
                                                                     actionButton("go","Run my Analysis", class = "btn-warning")),
                                                              column(9,
                                                                     tabsetPanel(type = "pills",
                                                                                 tabPanel("Click Through Rate", plotlyOutput("Aggregate")),
                                                                                 tabPanel("Click Through Rate Histogram", plotlyOutput("CTR_Hist")),
                                                                                 tabPanel("Presented Offer", plotlyOutput("Presented_Offer")),
                                                                                 tabPanel("Presented Offer Histogram", plotlyOutput("Presented_Offer_Hist")),
                                                                                 tabPanel("Ads Performance", plotlyOutput("Ads_Performance")),
                                                                                 tabPanel("Campaing Performance", plotlyOutput("Campaing_Performance")),
                                                                                 tabPanel("Ads Performance by Campaing", plotlyOutput("Ads_Performance_Campaing")))),height=1000
                                                              
                                                              
                   )),
                   
                   tabPanel("Distric Analytics", fluidRow(column(2,
                                                               fileInput("file", "Upload CSV File", accept = c(".csv")),
                                                               #dateRangeInput("Date_Range", "Select your Analysis Period", min = "2000-01-01", max = "2022-05-01"),
                                                               textInput("Start_Date","Start Date in format YYYY-MM-DD"),
                                                               textInput("End_Date","End Date in format YYYY-MM-DD"),
                                                               #selectInput("Metrics", "Choose your Totals' Metric",c("Total Emails Sent"= "Total.Emails.Sent",
                                                               #                                                     "Emails Delivered"="Emails.Delivered",
                                                               #                                                     "Total Email Opens"="Total.Email.Opens")),
                                                               selectInput("Brand", "Choose your Metric", c("Metric Do"="Square Strike Wedge",
                                                                                                            "Metric Re"="C3i Golf",
                                                                                                            "Metric Mi"="GX7 Golf",
                                                                                                            "Metric Fa"="iRT Golf",
                                                                                                            "Metric So"="Square to Square",
                                                                                                            "Metric La"="Blue Strike",
                                                                                                            "Metric Ti"="Super Sonic Golf",
                                                                                                            "Metric A"="Autopilot",
                                                                                                            "Metric B"="Moon Wood",
                                                                                                            "Metric C"="Forward Press",
                                                                                                            "Metric D"="LC1",
                                                                                                            "Metric E"="Vixa",
                                                                                                            "Metric F"="S7K",
                                                                                                            "Metric G"="x59 Golf",
                                                                                                            "Metric 1"="Power Manifesto",
                                                                                                            "Metric 2"="xE1 Golf",
                                                                                                            "Metric 3"="Ruckus Golf",
                                                                                                            "Metric 4"="Mike Bender",
                                                                                                            "Metric 5"="Speed Stik")),
                                                               actionButton("go_2","Run my Analysis", class = "btn-warning")),
                                                        
                                                        column(9,
                                                               
                                                               tabsetPanel(type = "pills",
                                                                           
                                                                           tabPanel("Overall Trend",plotOutput("Total_Sent")),
                                                                           #tabPanel("Comparative Ranking", plotOutput("Non_Smoothed")),
                                                                           tabPanel("State Trends", plotOutput("Delivered_Smoothed")),
                                                                           #tabPanel("Email Delivered Non-Smothed Series", plotOutput("Delivered_Non_Smoothed")),
                                                                           tabPanel("National Trend", plotOutput("Open_Smoothed")),
                                                                           tabPanel("OCDE Trend", plotOutput("Open_Non_Smoothed")))),height=1000
                                                        
                   ))
                   
  )      
  
  ,
  
  server <- function(input, output, session){
    
    ###
    ###
    
    
    observeEvent(input$go_3, { 
      
      output$Plot_NPI_1 <- 
        
        
        renderPlot({
          
          
          inFile <- input$HCP_File
          Clusters <- read.csv(inFile$datapath, header= TRUE, stringsAsFactors = FALSE)
          colnames(Clusters)[1]<- c("HCP NPI")
          colnames(Clusters)[2]<- c("Organization NPI")
          colnames(Clusters)[3]<- c("Provider First Name")
          colnames(Clusters)[4]<- c("Provider Last Name")
          colnames(Clusters)[5]<- c("Organization Name")
          colnames(Clusters)[6]<- c("Organization Address")
          colnames(Clusters)[7]<- c("HCP Address")
          colnames(Clusters)[8]<- c("Taxonomy")
          colnames(Clusters)[9]<- c("Speciality")
          colnames(Clusters)[10]<- c("Provider Phone Number")
          colnames(Clusters)[11]<- c("Authorized Phone Number")
          colnames(Clusters)[12]<- c("HCP Fax Number")
          colnames(Clusters)[13]<- c("State")
          colnames(Clusters)[14]<- c("Full Name")
          colnames(Clusters)[15]<- c("Email")
          colnames(Clusters)[16]<- c("Secondary Email")
          
          #VV <- Clusters[which(Clusters$`Organization Name` == input$Org_ID),]
          
          Clusters$Distance <- stringdist(toupper(input$Org_ID), Clusters$`Organization Name`,method = "osa")
          Clusters <- Clusters[with(Clusters, order(Clusters$Distance, decreasing = FALSE)),]
          CC <- subset(Clusters, Clusters$`Organization Name` == as.character(Clusters$`Organization Name`)[1:30])
          
          #subset(Clusters, Clusters$`Organization Name` == agrep(input$Org_ID, Clusters$`Organization Name`,max.distance = input$precise,ignore.case = TRUE, value = TRUE))
          #CC <- as.data.frame(amatch(input$Org_ID, Clusters$`Organization Name`,method = "osa", maxDist = 5))
          #colnames(CC)[1] <- c("match")
          #CC <- Clusters[CC$match,]
          
          Cluster_Graph <- graph_from_data_frame(CC)
          
          g <- simplify(Cluster_Graph, remove.multiple = TRUE, remove.loops = TRUE)
          plot(Cluster_Graph, layout= layout_nicely)
          
        })
      
      output$Names <- DT::renderDataTable({
        inFile <- input$HCP_File
        Clusters <- read.csv(inFile$datapath, header= TRUE, stringsAsFactors = FALSE)
        colnames(Clusters)[1]<- c("HCP NPI")
        colnames(Clusters)[2]<- c("Organization NPI")
        colnames(Clusters)[3]<- c("Provider First Name")
        colnames(Clusters)[4]<- c("Provider Last Name")
        colnames(Clusters)[5]<- c("Organization Name")
        colnames(Clusters)[6]<- c("Organization Address")
        colnames(Clusters)[7]<- c("HCP Address")
        colnames(Clusters)[8]<- c("Taxonomy")
        colnames(Clusters)[9]<- c("Speciality")
        colnames(Clusters)[10]<- c("Provider Phone Number")
        colnames(Clusters)[11]<- c("Authorized Phone Number")
        colnames(Clusters)[12]<- c("HCP Fax Number")
        colnames(Clusters)[13]<- c("State")
        colnames(Clusters)[14]<- c("Full Name")
        colnames(Clusters)[15]<- c("Email")
        colnames(Clusters)[16]<- c("Secondary Email")
        Clusters$Distances <- stringdist(toupper(input$Org_ID), Clusters$`Organization Name`, method = "osa")
        Clusters <- Clusters[with(Clusters, order(Clusters$Distances, decreasing = FALSE)),]
        
        DD <- subset(Clusters, Clusters$`Organization Name` == as.character(Clusters$`Organization Name`)[1:200])
        NN <- DD[,c("HCP NPI","Organization NPI","Full Name", "Organization Name","Organization Address","Speciality","Provider Phone Number","Email","Secondary Email")]
        datatable(NN, rownames = FALSE, filter = 'top', editable = "cell")
        #DD <-subset(Clusters, Clusters$`Organization Name` == agrep(input$Org_ID, Clusters$`Organization Name`,max.distance = input$precise,ignore.case = TRUE, value = TRUE))
        #NN <- DD[,c("HCP NPI","Organization NPI","Full Name", "Organization Name","Organization Address","Specialty")]
        #DD <- as.data.frame(amatch(input$Org_ID, Clusters$`Organization Name`,method = "osa", maxDist = 5))
        #colnames(DD)[1] <- c("match")
        #DD <- Clusters[DD$match,]
        
      })
      
      #output$downloadData <- downloadHandler(filename= function(){
      #data <- subset(Clusters, Clusters$`Organization Name` == agrep(input$Org_ID, Clusters$`Organization Name`, max.distance = input$precise, ignore.case = TRUE, value = TRUE))
      
      #paste("data-", Sys.Date(), "csv", sep = "")
      #},
      #content = function(file){write.csv(data, file)}
      
      #   )
      
      
    }) 
    
    
    ###
    ###
    ##
    observeEvent(input$go_places, {
      output$mymap <- renderLeaflet({
        
        inFile <- input$Geo_File
        Local_Home <- read.csv(inFile$datapath, header= TRUE, stringsAsFactors = FALSE)
        
        Local_Home_1 <- subset(Local_Home, Local_Home$Occ_Name== input$HCP_Speciality)
        
        leaflet(Local_Home_1)%>%
          addProviderTiles(providers$OpenStreetMap)%>% 
          setView(lng =-119.417931 , lat = 36.778259, zoom = 5)%>%
          addCircleMarkers(~Long, ~Lat, popup = Local_Home_1$Value_Name_Occupation,clusterOptions = markerClusterOptions())
        
      })
      
    })
    
    
    
    observeEvent(input$go, {
      
      
      ###
      output$Aggregate <- renderPlotly({
        inFile <- input$TOFU_File
        Ads_1 <- read.csv(inFile$datapath, header= TRUE, stringsAsFactors = FALSE)
        Ads_1 <- as.data.frame(Ads_1)
        Ads_1 <- subset(Ads_1, Ads_1$Campaign.Name==input$Campaing_Name)
        #original.parameters<- par( no.readonly = TRUE )
        #par(xaxt="n")
        
        
        
        #boxplot(as.numeric(Ads_1$CTR..Link.Click.Through.Rate.)~Ads_1$Ad.Set.Name, main= "Plot fo ANOVA, Ads Sets")
        All_Sets <- wilcox.test(Ads_1$CTR..Link.Click.Through.Rate.,conf.int=TRUE)
        CIntervals <- as.data.frame(substring(All_Sets$conf.int,0,8))    
        colnames(CIntervals)[1]<- c("Values")  
        
        
        
        Data_Medians <- as.data.frame(aggregate(as.numeric(Ads_1$CTR..Link.Click.Through.Rate.)~ Ads_1$Ad.Set.Name, FUN=median))
        colnames(Data_Medians)<- c("Set","Value")
        #lablist <- as.vector(colnames(Data_Medians)) 
        #axis(1, at=seq(1, 22, by=1), labels = FALSE)
        #text(seq(1, 22, by=1), par("usr")[3] - 0.2, labels = lablist, srt = 90, pos = 1, xpd = TRUE)
        C <- plot_ly(Data_Medians, x=~Set, y=~Value, type = "scatter", mode= "markers")
        C <- add_lines(C,y=CIntervals$Values[1])
        C<- add_lines(C,y=CIntervals$Values[2])
        #C %>% layout(abline(a=CIntervals$Values[2],b=0))
        #abline(a=CIntervals$Values[2],b=0)
        #abline(a=CIntervals$Values[1], b=0)
        
      }) 
      
      ###
      ###
      output$CTR_Hist <- renderPlotly({
        inFile <- input$TOFU_File
        Ads_1 <- read.csv(inFile$datapath, header= TRUE, stringsAsFactors = FALSE)
        Ads_1 <- as.data.frame(Ads_1)
        Ads_1 <- subset(Ads_1, Ads_1$Campaign.Name==input$Campaing_Name)
        plot_ly(Ads_1, x=~Ads_1$CTR..Link.Click.Through.Rate., type = "histogram", mode= "markers")
        
        
      }) 
      
      ###
      ###
      output$Presented_Offer <- renderPlotly({
        inFile <- input$TOFU_File
        Ads_1 <- read.csv(inFile$datapath, header= TRUE, stringsAsFactors = FALSE)
        Ads_1 <- as.data.frame(Ads_1)
        Ads_1 <- subset(Ads_1, Ads_1$Campaign.Name==input$Campaing_Name)
        #original.parameters<- par( no.readonly = TRUE )
        #par(xaxt="n")
        
        
        
        #boxplot(as.numeric(Ads_1$CTR..Link.Click.Through.Rate.)~Ads_1$Ad.Set.Name, main= "Plot fo ANOVA, Ads Sets")
        All_Sets <- wilcox.test(Ads_1$Presented.Offer.Per.Spend,conf.int=TRUE)
        CIntervals <- as.data.frame(substring(All_Sets$conf.int,0,8))    
        colnames(CIntervals)[1]<- c("Values")  
        
        
        
        Data_Medians <- as.data.frame(aggregate(as.numeric(Ads_1$Presented.Offer.Per.Spend)~ Ads_1$Ad.Set.Name, FUN=median))
        colnames(Data_Medians)<- c("Set","Value")
        #lablist <- as.vector(colnames(Data_Medians)) 
        #axis(1, at=seq(1, 22, by=1), labels = FALSE)
        #text(seq(1, 22, by=1), par("usr")[3] - 0.2, labels = lablist, srt = 90, pos = 1, xpd = TRUE)
        C <- plot_ly(Data_Medians, x=~Set, y=~Value, type = "scatter", mode= "markers")
        C <- add_lines(C,y=CIntervals$Values[1])
        C<- add_lines(C,y=CIntervals$Values[2])
        #C %>% layout(abline(a=CIntervals$Values[2],b=0))
        #abline(a=CIntervals$Values[2],b=0)
        #abline(a=CIntervals$Values[1], b=0)
        
      }) 
      
      ###
      ###
      output$Presented_Offer_Hist <- renderPlotly({
        inFile <- input$TOFU_File
        Ads_1 <- read.csv(inFile$datapath, header= TRUE, stringsAsFactors = FALSE)
        Ads_1 <- as.data.frame(Ads_1)
        Ads_1 <- subset(Ads_1, Ads_1$Campaign.Name==input$Campaing_Name)
        plot_ly(Ads_1, x=~Ads_1$Presented.Offer.Per.Spend, type = "histogram", mode= "markers")
        
        
      }) 
      ###
      
      ###
      output$Ads_Performance <- renderPlotly({
        inFile <- input$TOFU_File
        Ads_1 <- read.csv(inFile$datapath, header= TRUE, stringsAsFactors = FALSE)
        Ads_1 <- as.data.frame(Ads_1)
        #Ads_1 <- subset(Ads_1, Ads_1$Campaign.Name==input$Campaing_Name)
        #original.parameters<- par( no.readonly = TRUE )
        #par(xaxt="n")
        
        
        
        #boxplot(as.numeric(Ads_1$CTR..Link.Click.Through.Rate.)~Ads_1$Ad.Set.Name, main= "Plot fo ANOVA, Ads Sets")
        All_Sets <- wilcox.test(Ads_1$CTR..Link.Click.Through.Rate.,conf.int=TRUE)
        CIntervals <- as.data.frame(substring(All_Sets$conf.int,0,8))    
        colnames(CIntervals)[1]<- c("Values")  
        
        
        
        Data_Medians <- as.data.frame(aggregate(as.numeric(Ads_1$CTR..Link.Click.Through.Rate.)~ Ads_1$Ad.Name, FUN=median))
        colnames(Data_Medians)<- c("Set","Value")
        #lablist <- as.vector(colnames(Data_Medians)) 
        #axis(1, at=seq(1, 22, by=1), labels = FALSE)
        #text(seq(1, 22, by=1), par("usr")[3] - 0.2, labels = lablist, srt = 90, pos = 1, xpd = TRUE)
        C <- plot_ly(Data_Medians, x=~Set, y=~Value, type = "scatter", mode= "markers")
        C <- add_lines(C,y=CIntervals$Values[1])
        C<- add_lines(C,y=CIntervals$Values[2])
        #C %>% layout(abline(a=CIntervals$Values[2],b=0))
        #abline(a=CIntervals$Values[2],b=0)
        #abline(a=CIntervals$Values[1], b=0)
        
      }) 
      
      ###
      ###
      output$Campaing_Performance <- renderPlotly({
        inFile <- input$TOFU_File
        Ads_1 <- read.csv(inFile$datapath, header= TRUE, stringsAsFactors = FALSE)
        Ads_1 <- as.data.frame(Ads_1)
        Ads_1 <- subset(Ads_1, Ads_1$Campaign.Name==input$Campaing_Name)
        #original.parameters<- par( no.readonly = TRUE )
        #par(xaxt="n")
        
        
        
        #boxplot(as.numeric(Ads_1$CTR..Link.Click.Through.Rate.)~Ads_1$Ad.Set.Name, main= "Plot fo ANOVA, Ads Sets")
        All_Sets <- wilcox.test(Ads_1$CTR..Link.Click.Through.Rate.,conf.int=TRUE)
        CIntervals <- as.data.frame(substring(All_Sets$conf.int,0,8))    
        colnames(CIntervals)[1]<- c("Values")  
        
        
        
        Data_Medians <- as.data.frame(aggregate(as.numeric(Ads_1$CTR..Link.Click.Through.Rate.)~ Ads_1$Campaign.Name, FUN=median))
        colnames(Data_Medians)<- c("Set","Value")
        #lablist <- as.vector(colnames(Data_Medians)) 
        #axis(1, at=seq(1, 22, by=1), labels = FALSE)
        #text(seq(1, 22, by=1), par("usr")[3] - 0.2, labels = lablist, srt = 90, pos = 1, xpd = TRUE)
        C <- plot_ly(Data_Medians, x=~Set, y=~Value, type = "scatter", mode= "markers")
        C <- add_lines(C,y=CIntervals$Values[1])
        C<- add_lines(C,y=CIntervals$Values[2])
        #C %>% layout(abline(a=CIntervals$Values[2],b=0))
        #abline(a=CIntervals$Values[2],b=0)
        #abline(a=CIntervals$Values[1], b=0)
        
        
      }) 
      
      ###
      output$Ads_Performance_Campaing <- renderPlotly({
        inFile <- input$TOFU_File
        Ads_1 <- read.csv(inFile$datapath, header= TRUE, stringsAsFactors = FALSE)
        Ads_1 <- as.data.frame(Ads_1)
        Ads_1 <- subset(Ads_1, Ads_1$Campaign.Name==input$Campaing_Name)
        
        All_Sets <- wilcox.test(Ads_1$CTR..Link.Click.Through.Rate.,conf.int=TRUE)
        CIntervals <- as.data.frame(substring(All_Sets$conf.int,0,8))    
        colnames(CIntervals)[1]<- c("Values")  
        
        
        
        Data_Medians <- as.data.frame(aggregate(as.numeric(Ads_1$CTR..Link.Click.Through.Rate.)~ Ads_1$Ad.Name, FUN=median))
        colnames(Data_Medians)<- c("Set","Value")
        #lablist <- as.vector(colnames(Data_Medians)) 
        #axis(1, at=seq(1, 22, by=1), labels = FALSE)
        #text(seq(1, 22, by=1), par("usr")[3] - 0.2, labels = lablist, srt = 90, pos = 1, xpd = TRUE)
        C <- plot_ly(Data_Medians, x=~Set, y=~Value, type = "scatter", mode= "markers")
        C <- add_lines(C,y=CIntervals$Values[1])
        C<- add_lines(C,y=CIntervals$Values[2])
        #C %>% layout(abline(a=CIntervals$Values[2],b=0))
        #abline(a=CIntervals$Values[2],b=0)
        #abline(a=CIntervals$Values[1], b=0)
        
      }) 
      
    }, once = TRUE)
    
    
    observeEvent(input$go_2, {
      
      #####
      ##### Email Analytics
      #####
      
      output$Total_Sent <- renderPlot({
        
        inFile <- input$file
        
        Emails <- read.csv(inFile$datapath, header= TRUE, stringsAsFactors = FALSE)
        Emails <- as.data.frame(Emails)
        Emails$Date <- substr(Emails$Campaign.Send.Date..MM.DD.YYYY.,1,5)
        
        
        
        Emails$Date <- as.Date(as.numeric(Emails$Date),origin = "1899-12-30")
        
        Start_Date <-  input$Start_Date #c("2021-04-07") 
        End_Date <- input$End_Date #c("2021-04-14")  
        Brand <- input$Brand #c("C3i Golf")
        Metrics <- input$Metrics #c("Emails.Delivered")
        
        Emails_2 <- subset(Emails, Emails$Date > Start_Date &
                             Emails$Date < End_Date)
        Emails_3 <- subset(Emails_2, Emails_2$Brand == Brand)
        
        
        ggplot(Emails_3, aes(y= `Total.Emails.Sent`,x= `Date`))+geom_smooth(aes(y= `Total.Emails.Sent`), color="red")
        
        
        
      })
      output$Non_Smoothed <- renderPlot({    
        inFile <- input$file
        Emails <- read.csv(inFile$datapath, header= TRUE, stringsAsFactors = FALSE)
        Emails <- as.data.frame(Emails)
        Emails$Date <- substr(Emails$Campaign.Send.Date..MM.DD.YYYY.,1,5)
        
        
        
        Emails$Date <- as.Date(as.numeric(Emails$Date),origin = "1899-12-30")
        
        
        Start_Date <-  input$Start_Date #c("2021-04-07") 
        End_Date <- input$End_Date #c("2021-04-14")  
        Brand <- input$Brand #c("C3i Golf")
        Metrics <- input$Metrics #c("Emails.Delivered")
        
        Emails_2 <- subset(Emails, Emails$Date > Start_Date &
                             Emails$Date < End_Date)
        Emails_3 <- subset(Emails_2, Emails_2$Brand == Brand)
        
        
        ggplot(Emails_3, aes(y= `Total.Emails.Sent`,x= `Date`))+geom_line(aes(y= `Total.Emails.Sent`),color="red")
        
      })
      
      output$Delivered_Smoothed <- renderPlot({
        
        inFile <- input$file
        Emails <- read.csv(inFile$datapath, header= TRUE, stringsAsFactors = FALSE)
        Emails <- as.data.frame(Emails)
        Emails$Date <- substr(Emails$Campaign.Send.Date..MM.DD.YYYY.,1,5)
        
        
        Emails$Date <- as.Date(as.numeric(Emails$Date),origin = "1899-12-30")
        
        Start_Date <-  input$Start_Date #c("2021-04-07") 
        End_Date <- input$End_Date #c("2021-04-14")  
        Brand <- input$Brand #c("C3i Golf")
        Metrics <- input$Metrics #c("Emails.Delivered")
        
        Emails_2 <- subset(Emails, Emails$Date > Start_Date &
                             Emails$Date < End_Date)
        Emails_3 <- subset(Emails_2, Emails_2$Brand == Brand)
        
        
        ggplot(Emails_3, aes(y= `Emails.Delivered`,x= `Date`))+geom_smooth(aes(y= `Emails.Delivered`), color="blue")
        
      })
      output$Delivered_Non_Smoothed <- renderPlot({    
        inFile <- input$file
        Emails <- read.csv(inFile$datapath, header= TRUE, stringsAsFactors = FALSE)
        Emails <- as.data.frame(Emails)
        Emails$Date <- substr(Emails$Campaign.Send.Date..MM.DD.YYYY.,1,5)
        
        
        
        Emails$Date <- as.Date(as.numeric(Emails$Date),origin = "1899-12-30")
        
        
        Start_Date <-  input$Start_Date #c("2021-04-07") 
        End_Date <- input$End_Date #c("2021-04-14")  
        Brand <- input$Brand #c("C3i Golf")
        Metrics <- input$Metrics #c("Emails.Delivered")
        
        Emails_2 <- subset(Emails, Emails$Date > Start_Date &
                             Emails$Date < End_Date)
        Emails_3 <- subset(Emails_2, Emails_2$Brand == Brand)
        
        
        ggplot(Emails_3, aes(y= `Emails.Delivered`,x= `Date`))+geom_line(aes(y= `Emails.Delivered`), color="blue")
        
        
        
      })
      
      output$Open_Smoothed <- renderPlot({
        
        inFile <- input$file
        Emails <- read.csv(inFile$datapath, header= TRUE, stringsAsFactors = FALSE)
        Emails <- as.data.frame(Emails)
        Emails$Date <- substr(Emails$Campaign.Send.Date..MM.DD.YYYY.,1,5)
        
        
        
        Emails$Date <- as.Date(as.numeric(Emails$Date),origin = "1899-12-30")
        
        
        Start_Date <-  input$Start_Date #c("2021-04-07") 
        End_Date <- input$End_Date #c("2021-04-14")  
        Brand <- input$Brand #c("C3i Golf")
        Metrics <- input$Metrics #c("Emails.Delivered")
        
        Emails_2 <- subset(Emails, Emails$Date > Start_Date &
                             Emails$Date < End_Date)
        Emails_3 <- subset(Emails_2, Emails_2$Brand == Brand)
        
        
        ggplot(Emails_3, aes(y= `Total.Email.Opens`,x= `Date`))+geom_smooth(aes(y= `Total.Email.Opens`), color="green")
        
      })
      
      output$Open_Non_Smoothed <- renderPlot({
        
        inFile <- input$file
        Emails <- read.csv(inFile$datapath, header= TRUE, stringsAsFactors = FALSE)
        Emails <- as.data.frame(Emails)
        Emails$Date <- substr(Emails$Campaign.Send.Date..MM.DD.YYYY.,1,5)
        
        
        
        Emails$Date <- as.Date(as.numeric(Emails$Date),origin = "1899-12-30")
        
        
        Start_Date <-  input$Start_Date #c("2021-04-07") 
        End_Date <- input$End_Date #c("2021-04-14")  
        Brand <- input$Brand #c("C3i Golf")
        Metrics <- input$Metrics #c("Emails.Delivered")
        
        Emails_2 <- subset(Emails, Emails$Date > Start_Date &
                             Emails$Date < End_Date)
        Emails_3 <- subset(Emails_2, Emails_2$Brand == Brand)
        
        
        ggplot(Emails_3, aes(y= `Total.Email.Opens`,x= `Date`))+geom_line(aes(y= `Total.Email.Opens`), color="green")
        
      })  
      
      
    })
    
    
    
    
  }
)

