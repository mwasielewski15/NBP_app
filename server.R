library(shiny)
library(data.table)
library(googleVis)
library(lattice)
source("pobieranie_danych.r")

country_currency <- data.frame(
  country =   c("Germany","France","Spain","United States"),
  currency =  c("EUR","EUR","EUR","USD"),
  stringsAsFactors=F
)



shinyServer(function(input, output) {
    #przypisane wartosci lat wybranych przez uzytkownika
    outVar <- reactiveValues(
        selectYearVar = "2020"
    )
    
    observeEvent(input$selectYear,{
        outVar$selectYearVar <- input$selectYear
    })
    
    # przypisane waluty wybrane przez uzytkownika
    outCur1 <- reactiveValues(
        selectCurrencyVar = "EUR"
    )
    
    observeEvent(input$selectCurrency1,{
        outCur1$selectCurrencyVar <-input$selectCurrency1
    })
    
    # przypisane waluty wybrane przez uzytkownika
    outCur2 <- reactiveValues(
        selectCurrencyVar = "EUR"
    )
    
    observeEvent(input$selectCurrency2,{
        outCur2$selectCurrencyVar <-input$selectCurrency2
    })
     
    #aktywacja funkcji dla wybranych kilku lat
    dane <- reactive({ 
        do.call("rbind",
                lapply(outVar$selectYearVar,function(r){
                    dd <- getNBPData(as.character(r))
                    colnames(dd) <- gsub("X","",colnames(dd))
                    dd[,1] <- as.character(dd[,1])
                    return(dd)
                })
                
        )
    })
    
    #tabela
    output$dataSample <- DT::renderDataTable({
        DT::datatable(  
            dane(), 
            rownames = FALSE,
            options = list(
                scrollX = TRUE,
                pageLength = 16,
                lengthMenu = seq(from=10,by=10,to=100) 
            )
        )
    })
    
    #dane do mapy
    daneMapa <- reactive ({
      df <- data.frame(
        country=unique(c(
          country_currency$country[country_currency$currency %in% outCur1$selectCurrencyVar],
          country_currency$country[country_currency$currency %in% outCur2$selectCurrencyVar]
          ))
        )
      return(df)
      
    })
    
  
    
    #mapa
    output$mapa <- renderGvis({
        
        gvisGeoChart(daneMapa(), 
                    locationvar="country",
                    options=list(width=650, height=650))
        
    })
    
     
       
    observeEvent(input$report,{ 
      
      kurs1 <- outCur1$selectCurrencyVar
      kurs2 <- outCur2$selectCurrencyVar 

      params <- list()
      params$kurs1 <- kurs1
      params$kurs2 <- kurs2
      params$daneMapa <- daneMapa()
      params$dane <- dane()

      if(file.exists("Raport.html")) unlink("Raport.html")
      if(file.exists("params")) unlink("params")
      if(dir.exists("cache")) unlink("cache",recursive = T, force = T)

      save(params,file="params")
      
      library(knitr)
      library(markdown) 
      library(rmarkdown)

      knit(input='Raport.Rmd', output="tmp.md",envir=new.env())
      markdownToHTML(file="tmp.md", output="Raport.html")
      
      unlink("tmp.md")      
      unlink("params")    


      
    })
 
    
})
