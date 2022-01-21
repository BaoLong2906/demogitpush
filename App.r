library(shiny)
library(shinythemes)
library(ggplot2)
library(DT)
library(shinyWidgets) 
library(dplyr)
library(shinycssloaders)
library("RColorBrewer")
library(magrittr)
#gan du lieu
datacuatoi <- read.csv(file = "C:/Users/Acer/Desktop/Cuoi Ky R/Family Income and Expenditure.csv") 
datarealnew <- datacuatoi
#khoi tao app
ui = fluidPage(
  tags$head(
    # Note the wrapping of the string in HTML()
    # tao css
    tags$style(HTML(" 
      @import url('https://fonts.googleapis.com/css?family=Alata&display=swap');
      body {
        background-color: #E6E6E6;
        color: black;
        font-family: 'Alata', sans-serif;
      }
      h2 {
        color: black;
        text-align: center;
      }
      .shiny-input-container {
        color: #b81a1a;
        font-size: 17px;
      }
      div.table-title {
        display: block;
        margin: auto;
         max-width: 600px;
        padding:5px;
         width: 100%;
      }
      .table-fill {
        background: white;
        border-radius:3px;
        border-collapse: collapse;
        height: 320px;
        margin: auto;
        max-width: 600px;
        padding:5px;
        width: 100%;
        box-shadow: 0 5px 10px rgba(0, 0, 0, 0.1);
        animation: float 5s infinite;
      }
   
      th {
        color:#D5DDE5;;
        background:#1b1e24;
        border-bottom:4px solid #9ea7af;
        border-right: 1px solid #343a45;
        font-size:20px;
        font-weight: 100;
        padding:24px;
        text-align:left;
        text-shadow: 0 1px 1px rgba(0, 0, 0, 0.1);
        vertical-align:middle;
      }
      
      th:first-child {
        border-top-left-radius:3px;
      }
       
      th:last-child {
        border-top-right-radius:3px;
        border-right:none;
      }
        
      tr {
        border-top: 1px solid #C1C3D1;
        border-bottom-: 1px solid #C1C3D1;
        color:#666B85;
        font-size:16px;
        font-weight:normal;
        text-shadow: 0 1px 1px rgba(256, 256, 256, 0.1);
      }
       
      tr:hover td {
        background:#4E5066;
        color:#FFFFFF;
        border-top: 1px solid #22262e;
      }
       
      tr:first-child {
        border-top:none;
      }
      
      tr:last-child {
        border-bottom:none;
      }
       
      tr:nth-child(odd) td {
        background:#EBEBEB;
      }
       
      tr:nth-child(odd):hover td {
        background:#4E5066;
      }
      
      tr:last-child td:first-child {
        border-bottom-left-radius:3px;
      }
       
      tr:last-child td:last-child {
        border-bottom-right-radius:3px;
      }
       
      td {
        background:#FFFFFF;
        padding:20px;
        text-align:left;
        vertical-align:middle;
        font-weight:300;
        font-size:15px;
        text-shadow: -1px -1px 1px rgba(0, 0, 0, 0.1);
        border-right: 1px solid #C1C3D1;
      }
      
      td:last-child {
        border-right: 0px;
      }
      
      th.text-left {
        text-align: left;
      }
      
      th.text-center {
        text-align: center;
      }
      
      th.text-right {
        text-align: right;
      }
      
      td.text-left {
        text-align: left;
      }
      
      td.text-center {
        text-align: center;
      }
      
      td.text-right {
        text-align: right;
      }
      .abc{
          border:1px solid transparent; 
          -webkit-transition: all 0.4s cubic-bezier(.5, .24, 0, 1);
          transition: all 0.4s cubic-bezier(.5, .24, 0, 1);
          padding: 15px 10px;
          margin:10px 4px;
          color: black;
          top: 0px;
          font-weight: 500;
          font-size: 14px;
          text-transform: uppercase!important;
          letter-spacing: 2px;
          color: black;
          cursor: hand;
          text-align: center;
          text-transform: capitalize;
          border: 1px solid #b81a1a;
          border-radius:50px;
          position: relative;
          overflow: hidden!important;
          -webkit-transition: all .3s ease-in-out;
          -moz-transition: all .3s ease-in-out;
          -o-transition: all .3s ease-in-out;
          transition: all .3s ease-in-out;
          background: transparent!important;
          z-index:10;
        
      }
      .abc::before {
        content: '';
        position: absolute;
        left: 0px;
        bottom:0px;
        z-index:-1;
        width: 0%;
        height:1px;
        background: #6098FF;
        box-shadow: inset 0px 0px 0px #6098FF;
        display: block;
        -webkit-transition: all 0.4s cubic-bezier(.5, .24, 0, 1);
        transition: all 0.4s cubic-bezier(.5, .24, 0, 1)
      }
      .abc:hover{
          border: 1px solid #071982;
          color: #80ffd3!important;
      }
      .abc::before {
          content: '';
          width: 0%;
          height: 100%;
          display: block;
          background: #071982;
          position: absolute;
          -ms-transform: skewX(-20deg);
          -webkit-transform: skewX(-20deg); 
          transform: skewX(-20deg);   
          left: -10%;
          opacity: 1;
          top: 0;
          z-index: -12;
          -moz-transition: all .7s cubic-bezier(0.77, 0, 0.175, 1);
        -o-transition: all .7s cubic-bezier(0.77, 0, 0.175, 1);
        -webkit-transition: all .7s cubic-bezier(0.77, 0, 0.175, 1);
        transition: all .7s cubic-bezier(0.77, 0, 0.175, 1);
           box-shadow:2px 0px 14px rgba(0,0,0,.6);
        } 
      
      .abc::after {
          content: '';
          width: 0%;
          height: 100%;
          display: block;
          background: #de4b4b;
          position: absolute;
         -ms-transform: skewX(-20deg);
          -webkit-transform: skewX(-20deg); 
          transform: skewX(-20deg);   
          left: -10%;
          opacity: 0;
          top: 0;
          z-index: -15;
          -webkit-transition: all .94s cubic-bezier(.2,.95,.57,.99);
          -moz-transition: all .4s cubic-bezier(.2,.95,.57,.99);
          -o-transition: all .4s cubic-bezier(.2,.95,.57,.99);
          transition: all .4s cubic-bezier(.2,.95,.57,.99);
          box-shadow: 2px 0px 14px rgba(0,0,0,.6);
      }
      .abc:hover::before, .btn1O:hover::before{
        opacity:1;
        width: 116%;
      }
      .abc:hover::after, .btn1O:hover::after{
        opacity:1;
        width: 120%;
      }
      .abcd {
        color: #b81a1a;
        font-weight: bold;
        font-size: 17px;
      }
      .abc123 {
        text-align: center;
        font-weight: bold;
        font-size: 16px;
      }"))),
  theme = shinytheme("united"),
               navbarPage( "Final", 
                tabPanel("Number of Items",#tabpanel 1
                         sidebarPanel( #slider xuat hien ben trai
                           radioButtons( # radioButton chon
                             inputId  = "characterstic",
                             label = "Select the physical assets of the households listed in the survey",
                             choices = c(
                               "Phone" = "Number.of.Cellular.phone",
                               "CD.VCD.DVD" = "Number.of.CD.VCD.DVD",
                               "Component stereo set" = "Number.of.Component.Stereo.set",
                               "Air-conditioner" = "Number.of.Airconditioner",
                               "Washing Machine" = "Number.of.Washing.Machine",
                               "Personal Computer" = "Number.of.Personal.Computer",
                               "Motorcycle,Tricycle" = "Number.of.Motorcycle.Tricycle",
                               "Refrigerator,Freezer" = "Number.of.Refrigerator.Freezer",
                               "Television" = "Number.of.Television"
                             ),
                             selected = "Number.of.Cellular.phone"
                           ),
                           selectInput( #select
                             inputId  = "smoker",
                             label = "Select Region",
                             choices = c("All" = "All",
                                         "CAR" = "CAR",
                                         "NCR" = "NCR",
                                         "ARMM" = " ARMM",
                                         "XI - Davao Region" = "XI - Davao Region",
                                         "Caraga" = "Caraga",
                                         "IX - Zasmboanga Peninsula" = "IX - Zasmboanga Peninsula",
                                         "VIII - Eastern Visayas" = "VIII - Eastern Visayas",
                                         "X - Northern Mindanao" = "X - Northern Mindanao",
                                         "VI - Western Visayas" = "VI - Western Visayas",
                                         "V - Bicol Region" = "V - Bicol Region"
                                         ),
                             selected = "All"
                           ),                           
                           div( #hien so luong su huu la 0
                             class= "abc",
                             p(textOutput("text_calc01"))),
                           div( #hien so luong so huu 1:5
                             class= "abc",
                             p(textOutput("text_calc02"))),                           
                           div( #hien so luong so huu >5
                             class= "abc",
                             p(textOutput("text_calc03")))
                           ),
                mainPanel(tabsetPanel( #hien bieu do his
                  tabPanel("Histogram",plotOutput("myplot2"),
                           div( #hien tong ho gia dinh
                             class= "abc",
                             p(textOutput("text_calc"))),
                           ),
                  tabPanel("Boxplot", plotOutput("myplot3"),
                           div(#hien bieu do boxplot
                             class= "abc",
                             p(textOutput("text_calc1")))),
                  tabPanel("Plot", plotOutput("myplot4"),
                           div(#hien bieu do plot
                             class= "abc",
                             p(textOutput("text_calc2"))))
                  ))),
                tabPanel("Expenditure of Food",
                         sidebarPanel( #panel thu 2
                           selectInput(
                             inputId  = "non",
                             label = "Select Region",
                             choices = c("All" = "All",
                                         "CAR" = "CAR",
                                         "NCR" = "NCR",
                                         "ARMM" = " ARMM",
                                         "XI - Davao Region" = "XI - Davao Region",
                                         "Caraga" = "Caraga",
                                         "IX - Zasmboanga Peninsula" = "IX - Zasmboanga Peninsula",
                                         "VIII - Eastern Visayas" = "VIII - Eastern Visayas",
                                         "X - Northern Mindanao" = "X - Northern Mindanao",
                                         "VI - Western Visayas" = "VI - Western Visayas",
                                         "V - Bicol Region" = "V - Bicol Region"
                             ),
                             selected = "All"
                           ),
                           div( 
                             class= "abcd",
                             p("The percent is:")),
                           div(#hien phan tram cua cac thuc pham
                             class= "abc",
                             p(textOutput("text_calc001")))
                         ),
                         mainPanel(tabsetPanel( 
                           tabPanel("Summary",tableOutput("tablene1")),# hien bang min,max,mean
                           tabPanel("Pie Chart", plotOutput("myplot5"))# hien bieu do hinh tron
                         ))),
                tabPanel("ViewData",
                         sidebarPanel( #panel thu 3
                           selectInput(
                             inputId  = "datoregion",
                             label = "Region",
                             choices = c("All" = "All",
                                         "CAR" = "CAR",
                                         "NCR" = "NCR",
                                         "ARMM" = " ARMM",
                                         "XI - Davao Region" = "XI - Davao Region",
                                         "Caraga" = "Caraga",
                                         "IX - Zasmboanga Peninsula" = "IX - Zasmboanga Peninsula",
                                         "VIII - Eastern Visayas" = "VIII - Eastern Visayas",
                                         "X - Northern Mindanao" = "X - Northern Mindanao",
                                         "VI - Western Visayas" = "VI - Western Visayas",
                                         "V - Bicol Region" = "V - Bicol Region"
                             ),
                             selected = "All"
                           ),
                           selectInput(
                             inputId  = "datoincome",
                             label = "Income",
                             choices = c("All" = "All",
                                         "High income (>350k)" = "high",
                                         "Ordinary income (<350k || >150k)" = "Normal",
                                         "Low income (<150k)" = "Low"
                             ),
                             selected = "All"
                           ),
                           selectInput(
                             inputId  = "datomember",
                             label = "Number of Family Number",
                             choices = c("All" = "All",
                                         "0" = "0",
                                         "1" = "1",
                                         "2" = "2",
                                         "3" = "3",
                                         "4" = "4",
                                         "5" = "5",
                                         "6" = "6",
                                         "7" = "7",
                                         "8" = "8",
                                         "9" = "9",
                                         "10" = "10",
                                         "11" = "11",
                                         "12" = "12"
                             ),
                             selected = "All"
                           ),
                           selectInput(
                             inputId  = "datohage",
                             label = "House Age",
                             choices = c("All" = "All",
                                         "New (<20)" = "New",
                                         "Old (>20 %% <45)" = "Old",
                                         "Very old (>45)" = "Very"
                             ),
                             selected = "All"
                           ),
                           selectInput(
                             inputId  = "datotype",
                             label = "Type of Household",
                             choices = c(
                               "Both" = "Both",
                               "Single Family" = "Single Family",
                               "Extended Family" = "Extended Family"
                             ),
                             selected = "Both"
                           ),
                           selectInput(
                             inputId  = "datomafa",
                             label = "Marital Status Head of Household",
                             choices = c("All" = "All",
                                         "Single" = "Single",
                                         "Married" = "Married",
                                         "Widowed" = "Widowed",
                                         "Divorced/Separated" = "Divorced/Separated"
                                         
                             ),
                             selected = "All"
                           ),
                           selectInput(
                             inputId  = "datoage",
                             label = "Age of Household Head",
                             choices = c("All" = "All",
                                         "Young (<20)" = "New",
                                         "Mature (>20 %% <55)" = "Old",
                                         "Old (>55)" = "Very"
                             ),
                             selected = "All"
                           ),
                           selectInput(
                             inputId  = "datoheadsex",
                             label = "Gender Head of Household",
                             choices = c(
                               "Both" = "Both",
                               "Male" = "Male",
                               "Female" = "Female"
                             ),
                             selected = "Both"
                           )),
                         mainPanel(                           
                           div( #hien tong so luong ho
                             class= "abc",
                             p(textOutput("text_calc3"))),
                           div( #hien phan tram cua cac ho thuoc dac diem o bang ben trai
                             class= "abc123",
                             p(textOutput("text_calc30"))),
                           tableOutput("tableviewdata1"))
                         ),
                tabPanel("Linear Regression Analysis", #panel so do tuyen tinh
                         titlePanel("Linear Regression Analysis"),
                         sidebarPanel(
                           h3(HTML(paste0("<b>","About:"))),
                           tags$hr(),
                           
                           h6("This is the linear model of group 4"),
                           h6(HTML('<a href="https://www.facebook.com/hodat74/">HoThanhDat</a>')),
                           h6(HTML('<a href="https://www.facebook.com/longdev99/">HuynhCaoBaoLong</a>')),
                           h6("This dataset is :"),
                           h6(HTML('<a href="https://www.kaggle.com/grosvenpaul/family-income-and-expenditure" target="_blank">Dataset</a>')),
                           fileInput(
                             inputId = "filedata",
                             label = "Please upload dataset",
                             multiple = FALSE,
                             accept = c(".csv"),
                             buttonLabel = "Upload",
                             placeholder = "No files selected yet"
                           ),
                           uiOutput("xvariable"),
                           uiOutput("yvariable"),
                           actionButton("runButton", "Run")
                         ),
                         mainPanel( 
                           #thuc hien summary
                           uiOutput("results"),
                           fluidRow(verbatimTextOutput('lmSummary')%>% withSpinner(color="#0dc5c1")) , 
                           fluidRow(plotOutput('diagnosticPlot')),
                           uiOutput("message"),
                           #hien cac so do
                           dataTableOutput("tbl")
                         ))))
options(shiny.maxRequestSize=300*1024^2)
              
server <- function(input, output) {
  
  yourdata <- reactive({
    req(input$filedata)
    inData <- input$filedata
    if (is.null(inData)){ return(NULL) }
    mydata <- read.csv(inData$datapath, 
                       header = TRUE, 
                       sep=",",
                       stringsAsFactors = T)
  })
  output$message <- renderText({
    req(lmModel())
    
    "<b>The Selected Data Frame:</b>"
    
  })
  
  output$xvariable <- renderUI({
    #chon bien x
    req(yourdata())
    xa<-colnames(yourdata())
    pickerInput(inputId = 'xvar',
                label = 'Select one or more independent variable(s).',
                choices = c(xa[1:length(xa)]), selected=xa[1],
                options = list(`style` = "btn-info"),
                multiple = TRUE)
    
  })
  output$yvariable <- renderUI({
    #chon bien y
    req(yourdata())
    ya<-colnames(yourdata()) 
    pickerInput(inputId = 'yvar',
                label = 'Select a dependent variable.',
                choices = c(ya[1:length(ya)]), selected=ya[length(ya)],
                options = list(`style` = "btn-info"),
                multiple = FALSE)
    
  })
  
  
  
  lmModel <- eventReactive(input$runButton, {
    
    req(yourdata(),input$xvar,input$yvar)
    
    current_formula <- paste0(input$yvar, " ~ ", paste0(input$xvar, collapse = " + "))
    f <- as.formula(current_formula)
    model <- lm(f, data = yourdata(), na.action=na.exclude)
    
    return(model)
  })
  output$results <- renderUI({
    #summary
    req(lmModel())
    withMathJax(
      h3("Summary:"),
      paste(
        
        "Adj. \\( R^2 = \\) ", round(summary(lmModel())$adj.r.squared, 3),
        " ,\\( \\beta_0 = \\) ", round(lmModel()$coef[[1]], 3),
        ", and more details: ")
    )
  })
  
  
  output$tbl <- renderDataTable({
    #xuat file
    req(lmModel())
    
    datatable(data.frame(yourdata()%>% select(input$xvar), yourdata()%>% select(input$yvar)),
              extensions = "Buttons",
              options = list(
                lengthChange = FALSE,
                dom = "Blfrtip",
                buttons = c("copy", "csv", "pdf", "print")
              )
    )
  })
  
  output$lmSummary <- renderPrint({
    #summary
    req(lmModel())
    summary(lmModel())
  })
  
  output$diagnosticPlot <- renderPlot({
    #tao bieu do
    req(lmModel())
    par(mfrow = c(2,2))
    plot(lmModel())
  })
  output$myplot2  = renderPlot({
    #ve bieu do hist
    if(input$smoker=="All"){
      x <- datacuatoi[, input$characterstic]
      yne <- 40000
    }else{
      x <- datacuatoi[datacuatoi$Region == input$smoker, ][, input$characterstic]
      yne <- 3000
    }
    hist(
      if(input$smoker=="All"){
        x
      }else{
        x
      },
      labels = paste0(round(hist(x,plot = FALSE)$count / length(x)*100,1),"%"),
      col = "orange",
      xlab = input$characterstic,
      ylim = c(0,yne),
      main = paste("Histogram of", input$characterstic)
    )
  })
  output$myplot4  = renderPlot({
    #ve bieu do plot
    plot(
      if(input$smoker=="All"){
        datacuatoi[, input$characterstic]
      }else{
        datacuatoi[datacuatoi$Region == input$smoker, ][, input$characterstic]
      },
      lwd=3,
      ann=FALSE,
      col="orange",
      las=2,
      xlab = input$characterstic,
      main = paste("Plot of", input$characterstic)
      )
  })
  output$myplot3  = renderPlot({
    #ve bieu do boxplot
    boxplot(
      if(input$smoker=="All"){
        datacuatoi[, input$characterstic]
      }else{
        datacuatoi[datacuatoi$Region == input$smoker, ][, input$characterstic]
      },
      col="orange",
      xlab = input$characterstic,
      main = paste("Boxplot of", input$characterstic)
      )
  })
  
  formulaALL <- reactive({
    #tao bang min max mean total
    med1 <- median(datacuatoi$Total.Rice.Expenditure)
    med2 <- median(datacuatoi$Bread.and.Cereals.Expenditure)
    med3 <- median(datacuatoi$Meat.Expenditure)
    med4 <- median(datacuatoi$Total.Fish.and..marine.products.Expenditure)
    med5 <- median(datacuatoi$Fruit.Expenditure)
    med6 <- median(datacuatoi$Vegetables.Expenditure)
    min1 <- min(datacuatoi$Total.Rice.Expenditure)
    min2 <- min(datacuatoi$Bread.and.Cereals.Expenditure)
    min3 <- min(datacuatoi$Meat.Expenditure)
    min4 <- min(datacuatoi$Total.Fish.and..marine.products.Expenditure)
    min5 <- min(datacuatoi$Fruit.Expenditure)
    min6 <- min(datacuatoi$Vegetables.Expenditure)
    max1 <- max(datacuatoi$Total.Rice.Expenditure)
    max2 <- max(datacuatoi$Bread.and.Cereals.Expenditure)
    max3 <- max(datacuatoi$Meat.Expenditure)
    max4 <- max(datacuatoi$Total.Fish.and..marine.products.Expenditure)
    max5 <- max(datacuatoi$Fruit.Expenditure)
    max6 <- max(datacuatoi$Vegetables.Expenditure)
    sum1 <- sum(datacuatoi$Total.Rice.Expenditure)
    sum2 <- sum(datacuatoi$Bread.and.Cereals.Expenditure)
    sum3 <- sum(datacuatoi$Meat.Expenditure)
    sum4 <- sum(datacuatoi$Total.Fish.and..marine.products.Expenditure)
    sum5 <- sum(datacuatoi$Fruit.Expenditure)
    sum6 <- sum(datacuatoi$Vegetables.Expenditure)
    Names = c("Rice","Bread and Cereals","Meat","Fish","Fruit","Vegetables")
    Mean = c(med1,med2,med3,med4,med5,med6)
    Max = c(max1,max2,max3,max4,max5,max6)
    Min = c(min1,min2,min3,min4,min5,min6)
    Total = c(sum1,sum2,sum3,sum4,sum5,sum6)
    df <- data.frame(Names,Min,Mean,Max,Total)
    df
  })
  formulachoice <- reactive({
    #tao bang min max mean total
    med1 <- median(datacuatoi[datacuatoi$Region== input$non ,]$Total.Rice.Expenditure)
    med2 <- median(datacuatoi[datacuatoi$Region== input$non ,]$Bread.and.Cereals.Expenditure)
    med3 <- median(datacuatoi[datacuatoi$Region== input$non ,]$Meat.Expenditure)
    med4 <- median(datacuatoi[datacuatoi$Region== input$non ,]$Total.Fish.and..marine.products.Expenditure)
    med5 <- median(datacuatoi[datacuatoi$Region== input$non ,]$Fruit.Expenditure)
    med6 <- median(datacuatoi[datacuatoi$Region== input$non ,]$Vegetables.Expenditure)
    min1 <- min(datacuatoi[datacuatoi$Region== input$non ,]$Total.Rice.Expenditure)
    min2 <- min(datacuatoi[datacuatoi$Region== input$non ,]$Bread.and.Cereals.Expenditure)
    min3 <- min(datacuatoi[datacuatoi$Region== input$non ,]$Meat.Expenditure)
    min4 <- min(datacuatoi[datacuatoi$Region== input$non ,]$Total.Fish.and..marine.products.Expenditure)
    min5 <- min(datacuatoi[datacuatoi$Region== input$non ,]$Fruit.Expenditure)
    min6 <- min(datacuatoi[datacuatoi$Region== input$non ,]$Vegetables.Expenditure)
    max1 <- max(datacuatoi[datacuatoi$Region== input$non ,]$Total.Rice.Expenditure)
    max2 <- max(datacuatoi[datacuatoi$Region== input$non ,]$Bread.and.Cereals.Expenditure)
    max3 <- max(datacuatoi[datacuatoi$Region== input$non ,]$Meat.Expenditure)
    max4 <- max(datacuatoi[datacuatoi$Region== input$non ,]$Total.Fish.and..marine.products.Expenditure)
    max5 <- max(datacuatoi[datacuatoi$Region== input$non ,]$Fruit.Expenditure)
    max6 <- max(datacuatoi[datacuatoi$Region== input$non ,]$Vegetables.Expenditure)
    sum1 <- sum(datacuatoi[datacuatoi$Region== input$non ,]$Total.Rice.Expenditure)
    sum2 <- sum(datacuatoi[datacuatoi$Region== input$non ,]$Bread.and.Cereals.Expenditure)
    sum3 <- sum(datacuatoi[datacuatoi$Region== input$non ,]$Meat.Expenditure)
    sum4 <- sum(datacuatoi[datacuatoi$Region== input$non ,]$Total.Fish.and..marine.products.Expenditure)
    sum5 <- sum(datacuatoi[datacuatoi$Region== input$non ,]$Fruit.Expenditure)
    sum6 <- sum(datacuatoi[datacuatoi$Region== input$non ,]$Vegetables.Expenditure)
    Names = c("Rice","Bread and Cereals","Meat","Fish","Fruit","Vegetables")
    Mean = c(med1,med2,med3,med4,med5,med6)
    Max = c(max1,max2,max3,max4,max5,max6)
    Min = c(min1,min2,min3,min4,min5,min6)
    Total = c(sum1,sum2,sum3,sum4,sum5,sum6)
    df <- data.frame(Names,Min,Mean,Max,Total)
    df
  })
  formulamedchoice <- reactive({
    #bieu do hinh tron trong tab pie trong panel Expenditute of Food chon region
    med1 <- median(datacuatoi[datacuatoi$Region== input$non ,]$Total.Rice.Expenditure)
    med2 <- median(datacuatoi[datacuatoi$Region== input$non ,]$Bread.and.Cereals.Expenditure)
    med3 <- median(datacuatoi[datacuatoi$Region== input$non ,]$Meat.Expenditure)
    med4 <- median(datacuatoi[datacuatoi$Region== input$non ,]$Total.Fish.and..marine.products.Expenditure)
    med5 <- median(datacuatoi[datacuatoi$Region== input$non ,]$Fruit.Expenditure)
    med6 <- median(datacuatoi[datacuatoi$Region== input$non ,]$Vegetables.Expenditure)
    Names = c("Rice","Bread and Cereals","Meat","Fish","Fruit","Vegetables")
    Mean = c(med1,med2,med3,med4,med5,med6)
    pct = round(Mean/sum(Mean)*100)
    new_labels = paste(Names,"-",pct,"%",sep=" ")
    pie(Mean,labels=new_labels,main="Pie chart",col=brewer.pal(length(pct),'Spectral'))
    legend("topleft",legend = new_labels,fill=brewer.pal(length(pct),'Spectral'),cex=0.8)
  })
  formulamedAll <- reactive({
    #bieu do hinh tron trong tab pie trong panel Expenditute of Food 
    med1 <- median(datacuatoi$Total.Rice.Expenditure)
    med2 <- median(datacuatoi$Bread.and.Cereals.Expenditure)
    med3 <- median(datacuatoi$Meat.Expenditure)
    med4 <- median(datacuatoi$Total.Fish.and..marine.products.Expenditure)
    med5 <- median(datacuatoi$Fruit.Expenditure)
    med6 <- median(datacuatoi$Vegetables.Expenditure)
    Names = c("Rice","Bread and Cereals","Meat","Fish","Fruit","Vegetables")
    Mean = c(med1,med2,med3,med4,med5,med6)
    pct = round(Mean/sum(Mean)*100)
    new_labels = paste(Names,"-",pct,"%",sep=" ")
    pie(Mean,labels=new_labels,main="Pie chart",col=brewer.pal(length(pct),'Spectral'))
    legend("topleft",legend = new_labels,fill=brewer.pal(length(pct),'Spectral'),cex=0.8)
  })
  output$tablene1 = renderTable({
    #bang min, max,mean, total
    if(input$non=="All"){
      formulaALL()
    }else{
      formulachoice()
    }
  })
  output$myplot5  = renderPlot({
    #bieu do hinh tron trong tab pie trong panel Expenditure of Food
    if(input$non=="All"){
      formulamedAll()
    }else{
      formulamedchoice()
    }
  })
  formulacountALL <- reactive(
    #dem
    count(datacuatoi)
  )
  formulacountchoice <- reactive(
    #dem
    count(datacuatoi[datacuatoi$Region== input$smoker ,])
  )
  output$text_calc <- renderText({
    #tong ho dan trong panel Number of Items tab histogram
    if(input$smoker=="All"){
      paste("The Total Household is =", formulacountALL())
    }else{
      paste("The Total Household is =", formulacountchoice())
    }
  })
  output$text_calc1 <- renderText({
    #tong ho dan trong panel Number of Items tab Boxplot
    if(input$smoker=="All"){
      paste("The Total Household is =", formulacountALL())
    }else{
      paste("The Total Household is =", formulacountchoice())
    }
  })
  output$text_calc2 <- renderText({
    #tong ho dan trong panel Number of Items tab plot
    if(input$smoker=="All"){
      paste("The Total Household is =", formulacountALL())
    }else{
      paste("The Total Household is =", formulacountchoice())
    }
  })
  output$tableviewdata1 = renderTable({
    #bang trong panel Expenditure of Food
    if(input$datoregion !="All"){
      datarealnew <- datarealnew[datarealnew$Region == input$datoregion,]
    }
    if(input$datoincome !="All"){
      if(input$datoincome =="Hight"){
        datarealnew <- datarealnew[datarealnew$Total.Household.Income > 350000,]
      }
      if(input$datoincome =="Normal"){
        datarealnew <- datarealnew[datarealnew$Total.Household.Income > 160000,]
        datarealnew <- datarealnew[datarealnew$Total.Household.Income < 350000,]
      }
      if(input$datoincome =="Low"){
        datarealnew <- datarealnew[datarealnew$Total.Household.Income < 160000,]
      }
    }
    if(input$datohage !="All"){
      if(input$datohage =="New"){
        datarealnew <- datarealnew[datarealnew$House.Age < 20,]
      }
      if(input$datohage =="Old"){
        datarealnew <- datarealnew[datarealnew$House.Age > 19,]
        datarealnew <- datarealnew[datarealnew$House.Age < 45,]
      }
      if(input$datohage =="Very"){
        datarealnew <- datarealnew[datarealnew$House.Age > 45,]
      }
    }
    if(input$datomember != "All"){
      datarealnew <- datarealnew[datarealnew$Total.Number.of.Family.members == input$datomember,]
    }
    if(input$datotype != "Both"){
      datarealnew <- datarealnew[datarealnew$Type.of.Household == input$datotype,]
    }
    if(input$datoheadsex != "Both"){
      datarealnew <- datarealnew[datarealnew$Household.Head.Sex == input$datoheadsex,]
    }
    if(input$datoage !="All"){
      if(input$datoage =="New"){
        datarealnew <- datarealnew[datarealnew$Household.Head.Age < 20,]
      }
      if(input$datoage =="Old"){
        datarealnew <- datarealnew[datarealnew$Household.Head.Age > 19,]
        datarealnew <- datarealnew[datarealnew$Household.Head.Age < 55,]
      }
      if(input$datoage =="Very"){
        datarealnew <- datarealnew[datarealnew$Household.Head.Age > 55,]
      }
    }
    if(input$datomafa !="All"){
      datarealnew <- datarealnew[datarealnew$Household.Head.Marital.Status == input$datomafa,]
    }
    datarealnew <- select(datarealnew,-names(datarealnew)[5:12],-names(datarealnew)[17],
                          -names(datarealnew)[19:20],-names(datarealnew)[22:23],-names(datarealnew)[25:60],
                          -Region,-Main.Source.of.Income,-names(datarealnew)[14])
    names(datarealnew)[1:9]
    colnames(datarealnew)[1] <- "Income"
    colnames(datarealnew)[2] <- "Food"
    colnames(datarealnew)[3] <- "Beverages"
    colnames(datarealnew)[4] <- "Clothing"
    colnames(datarealnew)[5] <- "Housing"
    colnames(datarealnew)[6] <- "Medical"
    colnames(datarealnew)[7] <- "Education"
    colnames(datarealnew)[8] <- "Farming"
    datarealnew
  })
  output$text_calc3 <- renderText({
    #tong soluong ho dan sau khi chon cac dac diem o tab ben trai trong panel Viewdata
    if(input$datoregion=="All" && input$datoheadsex=="Both" && input$datotype=="Both" &&
       input$datomember=="All" && input$datohage=="All" && input$datoincome=="All" &&
       input$datoage=="All" && input$datomafa=="All" ){
      paste("The Total Household is =", count(datacuatoi))
    }else{
      if(input$datoregion !="All"){
        datarealnew <- datarealnew[datarealnew$Region == input$datoregion,]
      }
      if(input$datoincome !="All"){
        if(input$datoincome =="Hight"){
          datarealnew <- datarealnew[datarealnew$Total.Household.Income > 350000,]
        }
        if(input$datoincome =="Normal"){
          datarealnew <- datarealnew[datarealnew$Total.Household.Income > 160000,]
          datarealnew <- datarealnew[datarealnew$Total.Household.Income < 350000,]
        }
        if(input$datoincome =="Low"){
          datarealnew <- datarealnew[datarealnew$Total.Household.Income < 160000,]
        }
      }
      if(input$datohage !="All"){
        if(input$datohage =="New"){
          datarealnew <- datarealnew[datarealnew$House.Age < 20,]
        }
        if(input$datohage =="Old"){
          datarealnew <- datarealnew[datarealnew$House.Age > 20,]
          datarealnew <- datarealnew[datarealnew$House.Age < 45,]
        }
        if(input$datohage =="Very"){
          datarealnew <- datarealnew[datarealnew$House.Age > 45,]
        }
      }
      if(input$datomember != "All"){
        datarealnew <- datarealnew[datarealnew$Total.Number.of.Family.members == input$datomember,]
      }
      if(input$datotype != "Both"){
        datarealnew <- datarealnew[datarealnew$Type.of.Household == input$datotype,]
      }
      if(input$datoheadsex != "Both"){
        datarealnew <- datarealnew[datarealnew$Household.Head.Sex == input$datoheadsex,]
      }
      if(input$datoage !="All"){
        if(input$datoage =="New"){
          datarealnew <- datarealnew[datarealnew$Household.Head.Age < 20,]
        }
        if(input$datoage =="Old"){
          datarealnew <- datarealnew[datarealnew$Household.Head.Age > 19,]
          datarealnew <- datarealnew[datarealnew$Household.Head.Age < 55,]
        }
        if(input$datoage =="Very"){
          datarealnew <- datarealnew[datarealnew$Household.Head.Age > 55,]
        }
      }
      if(input$datomafa !="All"){
        datarealnew <- datarealnew[datarealnew$Household.Head.Marital.Status == input$datomafa,]
      }
      paste("The Total Household is =", count(datarealnew))
    }
  })
  output$text_calc30 <- renderText({
    #phan tram khi chon cac diem dac biet thi thay duoc % so ho dan chiem duoc trong panel Viewdata
    if(input$datoregion=="All" && input$datoheadsex=="Both" && input$datotype=="Both" &&
       input$datomember=="All" && input$datohage=="All" && input$datoincome=="All" &&
       input$datoage=="All" && input$datomafa=="All"){
      paste("---", "---")
    }else{
      if(input$datoregion !="All"){
        datarealnew <- datarealnew[datarealnew$Region == input$datoregion,]
      }
      if(input$datoincome !="All"){
        if(input$datoincome =="Hight"){
          datarealnew <- datarealnew[datarealnew$Total.Household.Income > 350000,]
        }
        if(input$datoincome =="Normal"){
          datarealnew <- datarealnew[datarealnew$Total.Household.Income > 160000,]
          datarealnew <- datarealnew[datarealnew$Total.Household.Income < 350000,]
        }
        if(input$datoincome =="Low"){
          datarealnew <- datarealnew[datarealnew$Total.Household.Income < 160000,]
        }
      }
      if(input$datohage !="All"){
        if(input$datohage =="New"){
          datarealnew <- datarealnew[datarealnew$House.Age < 20,]
        }
        if(input$datohage =="Old"){
          datarealnew <- datarealnew[datarealnew$House.Age > 20,]
          datarealnew <- datarealnew[datarealnew$House.Age < 45,]
        }
        if(input$datohage =="Very"){
          datarealnew <- datarealnew[datarealnew$House.Age > 45,]
        }
      }
      if(input$datomember != "All"){
        datarealnew <- datarealnew[datarealnew$Total.Number.of.Family.members == input$datomember,]
      }
      if(input$datotype != "Both"){
        datarealnew <- datarealnew[datarealnew$Type.of.Household == input$datotype,]
      }
      if(input$datoheadsex != "Both"){
        datarealnew <- datarealnew[datarealnew$Household.Head.Sex == input$datoheadsex,]
      }
      if(input$datoage !="All"){
        if(input$datoage =="New"){
          datarealnew <- datarealnew[datarealnew$Household.Head.Age < 20,]
        }
        if(input$datoage =="Old"){
          datarealnew <- datarealnew[datarealnew$Household.Head.Age > 19,]
          datarealnew <- datarealnew[datarealnew$Household.Head.Age < 55,]
        }
        if(input$datoage =="Very"){
          datarealnew <- datarealnew[datarealnew$Household.Head.Age > 55,]
        }
      }
      if(input$datomafa !="All"){
        datarealnew <- datarealnew[datarealnew$Household.Head.Marital.Status == input$datomafa,]
      }
      a <- count(datacuatoi)
      b <- count(datarealnew)
      e <- round(b/a*100,2)
      d <- paste("The percentage is ", paste(e,"%"))
    }
  })
  output$text_calc001 <- renderText({
    #phan tram cua thuc pham trong panel Viewdata
    if(input$non=="All"){
      med1 <- median(datacuatoi$Total.Rice.Expenditure)
      med2 <- median(datacuatoi$Bread.and.Cereals.Expenditure)
      med3 <- median(datacuatoi$Meat.Expenditure)
      med4 <- median(datacuatoi$Total.Fish.and..marine.products.Expenditure)
      med5 <- median(datacuatoi$Fruit.Expenditure)
      med6 <- median(datacuatoi$Vegetables.Expenditure)
      Names = c("Rice","Bread and Cereals","Meat","Fish","Fruit","Vegetables")
      Mean = c(med1,med2,med3,med4,med5,med6)
      pct = round(Mean/sum(Mean)*100)
      new_labels = paste(Names,"-",pct,"%",sep="")
      new_labels
    }else{
      med1 <- median(datacuatoi[datacuatoi$Region== input$non ,]$Total.Rice.Expenditure)
      med2 <- median(datacuatoi[datacuatoi$Region== input$non ,]$Bread.and.Cereals.Expenditure)
      med3 <- median(datacuatoi[datacuatoi$Region== input$non ,]$Meat.Expenditure)
      med4 <- median(datacuatoi[datacuatoi$Region== input$non ,]$Total.Fish.and..marine.products.Expenditure)
      med5 <- median(datacuatoi[datacuatoi$Region== input$non ,]$Fruit.Expenditure)
      med6 <- median(datacuatoi[datacuatoi$Region== input$non ,]$Vegetables.Expenditure)
      Names = c("Rice","Bread and Cereals","Meat","Fish","Fruit","Vegetables")
      Mean = c(med1,med2,med3,med4,med5,med6)
      pct = round(Mean/sum(Mean)*100)
      new_labels = paste(Names,"-",pct,"%",sep="")
      new_labels
    }
  })
  
  output$text_calc01 <- renderText({
    #so luong ho dan so huu la 0 trong panel NumberofItem
    if(input$smoker=="All"){
      x <- datacuatoi[, input$characterstic]
      sum <- count(datacuatoi)
    }else{
      x <- datacuatoi[datacuatoi$Region == input$smoker, ][, input$characterstic]
      sum <- count(datacuatoi[datacuatoi$Region == input$smoker, ])
    }
    y <- round(hist(x,plot = FALSE)$count / length(x)*100,1)
    z <- round(y[1]/100*sum,0)
    paste("The Total Household has 0=", z)
  })
  
  output$text_calc02 <- renderText({
    #so luong ho dan so huu tu 1-5 trong panel NumberofItem
    if(input$smoker=="All"){
      x <- datacuatoi[, input$characterstic]
      sum <- count(datacuatoi)
    }else{
      x <- datacuatoi[datacuatoi$Region == input$smoker, ][, input$characterstic]
      sum <- count(datacuatoi[datacuatoi$Region == input$smoker, ])
    }
    y <- round(hist(x,plot = FALSE)$count / length(x)*100,1)
    yz <- y[2]+y[3]+y[4]+y[5]
    z <- round(yz/100*sum,0)
    paste("The Total Household has 1:5=", z)
  })
  
  output$text_calc03 <- renderText({
    #so luong ho dan so huu > 5 trong panel NumberofItem
    if(input$smoker=="All"){
      x <- datacuatoi[, input$characterstic]
      sum <- count(datacuatoi)
    }else{
      x <- datacuatoi[datacuatoi$Region == input$smoker, ][, input$characterstic]
      sum <- count(datacuatoi[datacuatoi$Region == input$smoker, ])
    }
    y <- round(hist(x,plot = FALSE)$count / length(x)*100,1)
    yz <- y[6]+y[7]+y[8]+y[9]+y[10]+y[11]+y[12]+y[13]+y[14]
    if(is.na(yz)){
      z = 0;
    }else{
      z <- round(yz/100*sum,0)
    }
    paste("The Total Household has >5=", z)
  })
}
shinyApp(ui = ui, server = server)