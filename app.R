library(shiny)
require(ggplot2)
require(weathercan)
require(dplyr)

ui <- fluidPage(
  titlePanel("Airport Statistics"),
  sidebarLayout(
    sidebarPanel(
      actionButton("load", "Load Data"),
      selectInput("station", label = "Airport Selection", choices = c("Toronto", "Ottawa", "Windsor")),
      # selectInput("statistic", label = "Stat Choice", choices = c("Monthly Avg Max T", "Monthly Avg Min T", 
      #                                                             "Monthly Total Snow", "Monthly Total Rain")),
      selectInput("month", label = "Month", choices = c("01", "02", "03", "04", "05", "06",                                 
                                                        "07", "08", "09", "10", "11", "12")),
      renderText("lastUpdated")
      ),
    mainPanel(
      dataTableOutput("stats")
    )
  )
)

server <- function(input, output, session){
  observeEvent(input$load,{
               withProgress(message = 'Gathering data', detail = 'this may take a a while...', value  = 0,{
                 for (i in 1:15){
                   incProgress(1/15)
                   Sys.sleep(1)
                 }
               })
               source("stations_data_pull.R")
               })
  
  outputData <- reactive({
    if(input$station == "Toronto"){
      pearson
    }else if (input$station == "Ottawa"){
      yow
    }else if (input$station == "Windsor"){
      yqg
    }
  })
  
  outputMonth <- reactive({
    input$month
  })
  
  outputStat <- reactive({
    input$statistic
  })
  
  output$lastUpdated <- renderText(
    paste("Data last updated: ",outputData() %>% select(date) %>% arrange(desc(date)) %>% head(1))
  )
  
  output$stats <- renderDataTable(
    outputData() %>%
      filter(month == outputMonth()) %>%
      group_by(year) %>%
      summarise(avgMax = mean(max_temp, na.rm = TRUE),
                avgMin = mean(min_temp,na.rm = TRUE),
                avgMean = mean(mean_temp, na.rm = TRUE),
                maxT = max(max_temp, na.rm = TRUE),
                minT = min(min_temp, na.rm = TRUE),
                totRA = sum(total_rain, na.rm = TRUE),
                totSN = sum(total_snow, na.rm = TRUE),
                #missSN = sum(is.na(total_snow)),
                totPCP = sum(total_precip, na.rm = TRUE),
                # maxGust = max(spd_max_gust, na.rm = TRUE),
                # above0 = sum(min_temp > 0, na.rm = TRUE),
                # above10 = sum(max_temp > 10, na.rm = TRUE),
                # max_above0 = sum(max_temp >0, na.rm = TRUE),
                # below0 = sum(max_temp < 0, na.rm = TRUE),
                # miss_SN = sum(is.na(total_snow))
                ),
    options = list(pageLength = 10, searching = FALSE)
    # outputData() %>%
    #   filter(month == outputMonth()) %>%
    #   select(station_name, month, day, year, max_temp) %>% tail(5)
  )
}

shinyApp(ui, server)