rm(list=ls())
library(shiny)
library(shinyWidgets)
library(data.table)
library(googleVis)
library(tidyverse)
library(ggplot2)
library(plotly)
library(scales)
library(reshape2)
library(shinythemes)
library(lmtest)
library(rmarkdown)
library(knitr)
library(pander)

for(i in 2013:2021){
  
  getNBPData <- function(year=i){
    
    ret <- data.frame()
    
    if(year>=2013){
      
      fileName <- paste0(year,"_NBP_data.csv")
      
      try({
        if(file.exists(fileName)){
          if(as.Date(file.info(fileName)$mtime)==Sys.Date()){
            cat(paste("Reading data from local file\n"))
            return(read.table(file=fileName,sep=";",dec=",",header=T,stringsAsFactor=F))
          }
        }
      })
      
      cat(paste("Downloading data\n"))
      
      res <- try({
        
        d <- readLines(paste0("https://www.nbp.pl/kursy/Archiwum/archiwum_tab_a_",year,".csv"))
        d <- d[-2]
        d <- d[-c((length(d)-3):length(d))]
        tmpColnames <- strsplit(d[1],";",useBytes=T)[[1]]
        tmpColnames <- tmpColnames[-c((length(tmpColnames)-1):length(tmpColnames))]
        d <- do.call("rbind",
                     lapply(strsplit(d[-1],";"),
                            function(x){
                              matrix(as.numeric(gsub(",",".",x[-c((length(x)-1):length(x))])),nrow=1)
                            })
        )
        colnames(d) <- tmpColnames
        d <- as.data.frame(d)
        
        d$data <- as.Date(as.character(d$data),format="%Y%m%d")
        ret <- d
        write.table(ret,file=fileName,sep=";",dec=",",row.names=F)
        
      },silent=T)
      
      if(inherits(res,"try-error")){
        cat(paste("An error occurred while downloading data!!!\n")) 
      }
      
      
    }
    
    return(ret)
    
  }
  
  getNBPData(i)
}

# UI ##################################
button_ui <- function(id) {
  actionButton(NS(id, "btn"))
}

ui <- fluidPage(
  
  titlePanel("Kursy walut - projekt"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput("fileInPath", 
                label= h4("Wybierz rok:")
      ),
      
      
      
      pickerInput(
        "choice1",
        "Pierwsza waluta:",
        multiple = TRUE,
        choices = as.vector(sort(c("USD", "TBH", "USD", "AUD", "HKD",	"CAD",	"NZD",	"SGD", "HUF",	
                                   "CHF",	"UAH", "JPY",	"CZK",	"DKK", "ISK",	"NOK", "SEK", "HRK",	
                                   "RON",	"BGN", "TRY",	"ILS", "CLP",	"PHP", "MXN",	"ZAR", "BRL",
                                   "MYR",	"RUB", "IDR", "INR", "KRW",	"CNY", "GBP", "EUR", "PLN", "XDR")),mode="list"),
        selected = "  "
      ),
      pickerInput(
        "choice2",
        "Druga waluta:",
        multiple = TRUE,
        choices = as.vector(sort(c("USD", "TBH", "USD", "AUD", "HKD",	"CAD",	"NZD",	"SGD", "HUF",	
                                   "CHF",	"UAH", "JPY",	"CZK",	"DKK", "ISK",	"NOK", "SEK", "HRK",	
                                   "RON",	"BGN", "TRY",	"ILS", "CLP",	"PHP", "MXN",	"ZAR", "BRL",
                                   "MYR",	"RUB", "IDR", "INR", "KRW",	"CNY", "GBP", "EUR", "PLN", "XDR")),mode="list"),
        selected = "  "
      ),
      actionButton("report","Wygeneruj raport")
    ),
    
    
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Tabela walut", DT::dataTableOutput("tabela")),
                  tabPanel("Mapa walut",htmlOutput("mapka")),
                  tabPanel("Szeregi czasowe kursow",plotlyOutput("szereg")),
                  tabPanel("Scatter plot",plotlyOutput("scatter")),
                  tabPanel("Histogram roznic",plotlyOutput("histogram")),
                  tabPanel("Oszacowania regresji",verbatimTextOutput("regression"),
                           verbatimTextOutput("test"),plotlyOutput("trend"))
      )
      
      
    )
    
    
  )
  
)

# SERVER ####################################
button_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive(input$btn)
  })
}


server <- function(input, output) {
  
  outVar <- reactiveValues(
    selectCurrencyVar = df
  )
  
  v <- reactiveValues(dataLoadDownload = FALSE)
  
  observeEvent(input$getDataFromServer,{
    v$dataLoadDownload <- !v$dataLoadDownload
  })
  
  button <- button_server("btn1")
  
  # dane do tabeli i wykresow
  inputdata <- reactive({
    
    try({
      return(
        read.table(file=input$fileInPath$datapath,sep=";",dec=",",header=TRUE,stringsAsFactors=FALSE)
        
      )
    },silent=T)
    
    return(dane)
  })
  
  # dane do mapki
  
  inputmap <- reactive({
    
    df <- data.frame(
      Country = c("Thailand", "United States", "Australia", "Hong Kong", "Canada", "New Zealand",
                  "Singapore", "Hungary", "Switzerland", 
                  "Ukraine", "Japan", "Czech Republic", "Denmark",
                  "Iceland", "Norway", "Sweden", "Croatia", "Romania", "Bulgaria",
                  "Turkey", "Israel", "Chile", "Philippines", "Mexico", "South Africa",
                  "Brazil", "Malaysia", "Russia", "Indonesia", "Indie", "South Korea",
                  "China", "United Kingdom", "Austria", "Belgium", "Cyprus", "Estonia",
                  "Finland", "France", "Grecce", "Spain", "Netherlands", "Ireland", 
                  "Lithuania", "Luxemburg", "Latvia", "Malta", "Germany", "Portugal", 
                  "Slovakia", "Slovenia", "Italy", "Andorra", "Monaco", "Montenegro",
                  "Kosovo", "Poland", "World"),
      Currency = c("TBH", "USD", "AUD", "HKD",	"CAD",	"NZD",	"SGD", "HUF",	
                   "CHF",	"UAH", "JPY",	"CZK",	"DKK", "ISK",	"NOK", "SEK", "HRK",	
                   "RON",	"BGN", "TRY",	"ILS", "CLP",	"PHP", "MXN",	"ZAR", "BRL",
                   "MYR",	"RUB", "IDR", "INR", "KRW",	"CNY", "GBP", "EUR", "EUR", "EUR",
                   "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR",
                   "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", "EUR", 
                   "PLN", "XDR")
    )
    
    button()
    isolate(
      df %>% filter(Currency %in% c(input$choice1,input$choice2)) 
    )
    
  })
  
  
  # tabela
  output$tabela <- DT::renderDataTable({
    DT::datatable(  
      inputdata(), 
      rownames = FALSE,
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        lengthMenu = seq(from=10,by=10,to=100)
      )
    )
  })
  
  # mapa
  output$mapka <- renderGvis({
    
    gvisGeoChart(inputmap(), locationvar="Country", hovervar="Currency",
                 options=list(
                   width=750, 
                   height=450))
    
  })
  
  # szereg czasowy  
  
  output$szereg <- renderPlotly({
    plot.data <- melt(inputdata(), id.vars = "data")
    
    plot.data$waluty <- substr(plot.data$variable, nchar(as.vector(plot.data$variable)) - 3 + 1, nchar(as.vector(plot.data$variable)))
    plot.data <- plot.data[plot.data$waluty %in% c(input$choice1,input$choice2),]
   plot.data$data <- as.Date(plot.data$data)
   ggplot(plot.data) +
      geom_line(mapping = aes(x = data, y = value,
                               colour = waluty,group = waluty), size=0.2)+
    scale_x_date(date_labels = "%B")+
      ylab("Kurs")+
      xlab("Data")
  })
  # scatter
  output$scatter <- renderPlotly({
    plot.data <- melt(inputdata(), id.vars = "data")
    plot.data$Waluty <- substr(plot.data$variable, nchar(as.vector(plot.data$variable)) - 3 + 1, nchar(as.vector(plot.data$variable)))
    plot.data <- dcast(plot.data,data~Waluty)
    ggplot(plot.data) +
      geom_point(mapping = aes(x = plot.data[,input$choice1], y = plot.data[,input$choice2], colour = "orange", size=0.1))+
      xlab("kurs1")+
      ylab("kurs2")
  })
  
  # histogram
  output$histogram <- renderPlotly({
    plot.data <- melt(inputdata(), id.vars = "data")
    plot.data$Waluty <- substr(plot.data$variable, nchar(as.vector(plot.data$variable)) - 3 + 1, nchar(as.vector(plot.data$variable)))
    plot.data <- plot.data[plot.data$Waluty %in% c(input$choice1,input$choice2), ]
    plot.data1 <- plot.data[plot.data$Waluty %in% input$choice1, ]
    plot.data2 <- plot.data[plot.data$Waluty %in% input$choice2, ]
    plot.data$roznica<-plot.data1$value-plot.data2$value
    ggplot(plot.data) +
      geom_histogram(aes(x = roznica),col="red", size=0.3)+
      xlab("Roznica kursow")+
      ylab("Czestosci")
  })
  
  # regresja
  output$regression <- renderPrint({
    plot.data <- melt(inputdata(), id.vars = "data")
    plot.data$Waluty <- substr(plot.data$variable, nchar(as.vector(plot.data$variable)) - 3 + 1, nchar(as.vector(plot.data$variable)))
    plot.data <- dcast(plot.data, data~Waluty)
    regresja <- lm(plot.data[,input$choice1]~plot.data[,input$choice2])
    names(regresja$coefficients) <- c("Intercept", input$choice2)
    summary(regresja)
  })
  output$test <- renderPrint({
    plot.data <- melt(inputdata(), id.vars = "data")
    plot.data$Waluty <- substr(plot.data$variable, nchar(as.vector(plot.data$variable)) - 3 + 1, nchar(as.vector(plot.data$variable)))
    plot.data <- dcast(plot.data, data~Waluty)
    regresja <- lm(plot.data[,input$choice1]~plot.data[,input$choice2])
    raintest(regresja)
  })
  output$trend <- renderPlotly({
    plot.data <- melt(inputdata(), id.vars = "data")
    plot.data$Waluty <- substr(plot.data$variable, nchar(as.vector(plot.data$variable)) - 3 + 1, nchar(as.vector(plot.data$variable)))
    plot.data <- dcast(plot.data, data~Waluty)
    wykres <- ggplot(plot.data, aes(x = plot.data[, input$choice2], y = plot.data[, input$choice1])) +
      geom_point(colour="green") +
      stat_smooth(method = "lm", colour="red")+
      ylab(input$choice1) +
      xlab(input$choice2)
    ggplotly(wykres)
  })
  outCur1 <- reactiveValues(
    selectCurrencyVar = " "
  )
  
  observeEvent(input$selectCurrency1,{
    outCur1$selectCurrencyVar <-input$choice1
  })
  outCur2 <- reactiveValues(
    selectCurrencyVar = " "
  )
  
  observeEvent(input$selectCurrency2,{
    outCur2$selectCurrencyVar <-input$choice2
  })
  observeEvent(input$report,{ 
    
    kurs1 <- outCur1$selectCurrencyVar
    kurs2 <- outCur2$selectCurrencyVar 
    
    params <- list()
    params$kurs1 <- kurs1
    params$kurs2 <- kurs2
    params$mapka <- inputmap()
    params$dane <- inputdata()
    
    if(file.exists("Raport.html")) unlink("Raport.html")
    if(file.exists("params")) unlink("params")
    if(dir.exists("cache")) unlink("cache",recursive = T, force = T)
    
    save(params,file="params")
    
    library(knitr)
    library(markdown) 
    library(rmarkdown)
    
    knit(input='Generator.Rmd', output="tmp.md",envir=new.env())
    markdownToHTML(file="tmp.md", output="Raport.html")
    
    unlink("tmp.md")      
    unlink("params")    
    
    
    
  })
}

# URUCHOMIENIE #######################
shinyApp(ui = ui, server = server)