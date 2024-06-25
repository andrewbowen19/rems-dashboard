library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(bslib)
library(ggExtra)
library(glue)
library(tsibble)
library(fabletools)
library(fable)
library(gridExtra)

# Read in dataset
df <- read.csv("https://raw.githubusercontent.com/andrewbowen19/rems-dashboard/main/rems-data.csv",
               check.names=FALSE)

# Only keep numeric cols for scatter plot
df_num <- df %>% select(where(is.numeric), -c(`Program Office`,
                                              `Operations Office`,
                                              `Site`,
                                              `Reporting Organization`,
                                              `Facility Type`,
                                              `Labor Category`,
                                              `Occupation`,
                                              `Monitoring Status`))
                                

# Sidebar content
sidebar_content <- sidebarPanel(
  selectInput("site", "Site", unique(df$Site), "Savannah River Site", multiple=TRUE),
  varSelectInput("xvar", "X variable", df_num, selected = "Total Number Monitored"),
  varSelectInput("yvar", "Y variable", df_num, selected = "Average Meas. TED (mrem)"),
  selectInput("program_office", "Filter by Program Office", choices = unique(df$`Program Office`), selected = "National Nuclear Security Administration", multiple=TRUE),
  sliderInput("year", "Year Range", value = c(1986, 2022), min=1986, max=2022),
  checkboxInput("show_site", "Show Site", TRUE),
  checkboxInput("show_margins", "Show marginal plots", TRUE),
  tags$a(href="https://orise.orau.gov/cer/rems/definitions.pdf", "Definitions")
)

# UI definition
ui <- fluidPage(
  titlePanel("Radiation Exposure Monitoring System Dashboard"),
  sidebarLayout(
    sidebar_content,
    mainPanel(
      plotOutput("scatter"),
      plotOutput("timeSeriesForecast"),
      plotOutput("barGraph"),
      tags$a(href="https://www.energy.gov/ehss/occupational-radiation-exposure-rems-system-tools",
             "All data sourced from the Department of Energy REMS Query Tool")
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  subsetted <- reactive({
    print(input$yvar)
    df %>% filter(Site %in% input$site &
                  `Program Office` %in% input$program_office &
                  `Monitoring Year` %in% seq(input$year[1], input$year[2]))
  })
  
  subsettedTS <- reactive({
    print("bar")
    print(as.symbol(input$yvar))
    key_cols <- c("Program Office", "Operations Office", "Site", "Reporting Organization",
                  "Facility Type", "Labor Category", "Occupation", "Monitoring Status")
    df %>% filter(Site %in% input$site &
                  `Program Office` %in% input$program_office &
                  `Monitoring Year` %in% seq(input$year[1], input$year[2])) %>% 
      as_tsibble(index = `Monitoring Year`, key=key_cols) %>% 
      group_by(Site) %>% 
      summarise(yValue = mean(!!as.symbol(input$yvar))) %>%
      fill_gaps()
  })
  
  output$scatter <- renderPlot({
    # Plot selected data
    p <- ggplot(subsetted(), aes(!!input$xvar, !!input$yvar)) + geom_point()
    # Show marginal histograms if desired
    p <- ggExtra::ggMarginal(p, type = "density", margins = "both",
                             size = 8)#, groupColour = input$show_site, groupFill = input$show_site)
    
    p

  }, res = 100)
  


  # Number of individuals with TED chart
  output$barGraph <- renderPlot({
    num_with_ted <- subsetted() %>% 
      filter(`Monitoring Year` > 1988) %>%
      group_by(`Monitoring Year`) %>%
      summarise(`Number with TED` = sum(`Number with Meas. TED`))
    
    ced_avg <- subsetted() %>%
      group_by(`Monitoring Year`) %>%
      summarise(`Collective CED` = mean(`Collective CED (person-mrem)`))
  
    
    p1 <- num_with_ted %>% 
      ggplot(aes(x=`Monitoring Year`, 
                 y = `Number with TED`)) + geom_bar(stat='identity') + 
      labs(x="Year", 
           y ="Number of Individuals with Measured TED")
    p2 <-ced_avg %>%  ggplot(aes(x=`Monitoring Year`, y=`Collective CED`)) + geom_bar(stat='identity') + 
      labs(x="Year", 
           y ="Collective CED")
    grid.arrange(grobs=list(p1, p2), ncol=2)
  }, res = 100)
  

  
  # Create TS forecast tab & plot with basic ARIMA model
  output$timeSeriesForecast <- renderPlot({
    p <- subsettedTS() %>%
      model(ARIMA(yValue)) %>%
      forecast(h = 10) %>%
      autoplot(subsettedTS()) + labs(x = "Year",
                                     y=input$yvar,
                                     title="ARIMA Forecast")
    
    p
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
