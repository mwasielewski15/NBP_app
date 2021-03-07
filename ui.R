
country_currency <- data.frame(
  country =   c("Germany","France","Spain","United States"),
  currency =  c("EUR","EUR","EUR","USD"),
  stringsAsFactors=F
)



library(shiny)
library(plotly)


shinyUI(fluidPage(
    
    # Tytul
    titlePanel("Projekt - notowania kursow walut NBP"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput(inputId = "selectYear",
                               label = "Wybierz rok danych",
                               choices = c(2020:2013),
                               selected = "2020"),
            actionButton("getDataFromServer", "Pobierz dane"),
            selectInput(inputId = "selectCurrency1",
                               label = "Wybierz pierwsza walute",
                               choices = c(unique(country_currency$currency))),
            selectInput(inputId = "selectCurrency2",
                        label   = "Wybierz druga walute",
                        choices = c(unique(country_currency$currency))),

            actionButton("report","Kompilacja raportu")
        ),
        
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Tabela", DT::dataTableOutput("dataSample")),
                        tabPanel("Wizualizacje pary walut",htmlOutput("mapa"))
            )
        )

  )
))
