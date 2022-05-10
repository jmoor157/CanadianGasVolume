library(shiny)
library(ggplot2)
library(dplyr)
library (magrittr)
library(tsibble)
library(feasts)
library(plotly)
library(fpp3)
tsibble(canadian_gas)


ui <- fluidPage(
    
   
    titlePanel("Canadian Gas Monthly Production"),
    
  
    sidebarLayout(
        sidebarPanel(
            radioButtons("buttons", label = h3("Display Plot:"),
                         choices = list("Seasonal", "Autocorrelation", "Decomposition", "Subseries", "Mean", "Naive", "Seasonal Naive", "Drift", "Holts", "Holts/Winters","Auto ARIMA", "Manual ARIMA")),
            textOutput("interpretations"),
            ),
        
        mainPanel(
            p("The top graphic shows a fully plotted time series for the production of gas in Canada from January 1960 to Novermer of 2004. The box to the left allows you to select between many different options for how the series is plotted.The first 4 buttons create plots to examine this data. The other 8 allow users to show different predictions for future data. Interpretations for the first 4 can be found under the buttons."),
            plotOutput("predict"),
            plotlyOutput("midterm"),
            )))

server <- function(input, output) {
    
    output$interpretations <- renderText(
        if(input$buttons == "Seasonal"){
            paste('The seasonality plot allows users to see trends over the course of a year, and shows users how the data is moving between years.')
        }
        else if(input$buttons == "Autocorrelation"){
            paste('The autocorrelation plots shows users the relationship between the lagged values of the time series.')
        }
        else if(input$buttons == "Decomposition"){
            paste(' Using a decomposition plot, users can view the trend, seasonality, and randomness plots for a time series. The trend plot shows users the direction the volume of gas is moving in. The seaonality plot allows users to view how volume is affected by season. Lastly, the randomness plot shows users how much of the variation in volume is random.')
        }
        else if(input$buttons == "Subseries"){
            paste('The subseries plot allows users to see how each month has individually chnaged over the years this data was collected.')
        }
    )
    
    output$predict <- renderPlot({
        if(input$buttons == "Mean"){
            fit <- canadian_gas %>%
                model(MEAN())
            fit %>%
                forecast(h = 60) %>%
                autoplot(canadian_gas)+
                labs(title = "Mean")
        }
        else if(input$buttons == "Naive"){
            fit <- canadian_gas %>%
                model(NAIVE(Volume))
            fit %>%
                forecast(h = 60) %>%
                autoplot(canadian_gas)+
                labs(title = "Naive")
        }
        else if(input$buttons == "Seasonal Naive"){
            fit <- canadian_gas %>%
                model(SNAIVE(Volume))
            fit %>%
                forecast(h = 60) %>%
                autoplot(canadian_gas)+
                labs(title = "Seasonal Naive")
        }
        else if(input$buttons == "Drift"){
            fit <- canadian_gas %>%
                model(RW(Volume~drift()))
            fit %>%
                forecast(h = 60) %>%
                autoplot(canadian_gas)+
                labs(title = "Drift")
        }
        else if(input$buttons == "Holts"){
            fit <- canadian_gas %>%
                model(
                    ETS(Volume ~ error("A") + trend("A") + season("N"))
                )
            fit %>%
                forecast(h = 60) %>%
                autoplot(canadian_gas)+
                labs(title = "Holts")
        }
        else if(input$buttons == "Auto ARIMA"){
            fit <- canadian_gas %>%
                model(ARIMA(Volume))
            fit %>%
                forecast(h = 60)
            autoplot(canadian_gas)+
                labs(title = "Auto ARIMA")
        }
        else if(input$buttons == "Manual ARIMA"){
            fit <- canadian_gas %>%
                model(ARIMA(Volume ~ pdq(1, 0 ,0) + PDQ(0, 1, 1)))
            fit %>%
                forecast(h = 60)
            autoplot(canadian_gas)+
                labs(title = "Manual ARIMA")
        }
        else if(input$buttons == "Holts"){
            fit <- canadian_gas %>%
                model(
                    ETS(Volume ~ error("A") + trend("A") + season("N"))
                )
            fit %>%
                forecast(h = 60) %>%
                autoplot(canadian_gas)+
                labs(title = "Holts")
        }
        else if(input$buttons == "Holts/Winters"){
            fit <- canadian_gas %>%
                model(
                    additive = ETS(Volume ~ error("A") + trend("A") +
                                       season("A")))
            fit %>%
                forecast(h= 60) %>%
                autoplot(canadian_gas)+
                labs(title = "Holts/Winters")
        }
        else {autoplot(canadian_gas)}})
    
    
    
    output$midterm <- renderPlotly({
        if(input$buttons == "Seasonal") {
            canadian_gas %>% gg_season(Volume)+
                labs(title = "Seasonal")}
        else if (input$buttons == "Decomposition") {
            canadian_gas %>%
                model(
                    classical_decomposition(Volume, type = "additive")
                ) %>%
                components() %>%
                autoplot() +
                labs(title = "Decomposition")}
        else if (input$buttons == "Autocorrelation") {
            canadian_gas %>% gg_lag(Volume) +
                labs(title = "Autocorrelation")}
        else if(input$buttons == "Subseries") {
            gg_subseries(canadian_gas)+
                labs(title = "Subseries")
        }
        
        
    })}

shinyApp(ui = ui, server = server)
