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

# Read in dataset
df <- read.csv("https://raw.githubusercontent.com/andrewbowen19/rems-dashboard/main/rems-data.csv", check.names=FALSE)

# Only keep numeric cols for scatter plot
df_num <- df %>% select(where(is.numeric), -c(`Program Office`, `Operations Office`, `Site`, `Reporting Organization`, `Facility Type`, `Labor Category`, `Occupation`, `Monitoring Status`))

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

timeSeriesSidebarContent <- sidebarPanel(
  varSelectInput("tsYvar", "Y variable", df_num, selected = "Total Number Monitored"),
  selectInput("program_office", "Filter by Program Office", choices = unique(df$`Program Office`), selected = "National Nuclear Security Administration", multiple=TRUE),
  
)

# UI definition
ui <- page_navbar(
  title = "Radiation Exposure Monitoring System Dashboard",
  
  nav_panel("Plots",
            sidebarLayout(
              sidebar_content,
              mainPanel(
                plotOutput("scatter"),
                plotOutput("xHist"),
                plotOutput("yHist"),
                tags$a(href="https://www.energy.gov/ehss/occupational-radiation-exposure-rems-system-tools", "All data sourced from the Department of Energy REMS Query Tool")
              )
            )
  ),
  nav_panel("TED Segments",
            sidebarLayout(
              sidebar_content,
              mainPanel(plotOutput("tedGraph"))
            )
  ),
  nav_panel("Time Series",
            sidebarLayout(
              sidebar_content,
              mainPanel(plotOutput("timeSeriesForecast"))
            )
  )
)

# Server logic
server <- function(input, output, session) {
  subsetted <- reactive({
    df %>% filter(Site %in% input$site &
                  `Program Office` %in% input$program_office &
                  `Monitoring Year` %in% seq(input$year[1], input$year[2]))
  })
  
  subsettedTS <- reactive({
    df %>% filter(Site %in% input$site &
                  `Program Office` %in% input$program_office &
                  `Monitoring Year` %in% seq(input$year[1], input$year[2])) %>% as_tsibble(index = `Monitoring Year`,
                      key=c("Program Office", "Operations Office", "Site", "Reporting Organization",
                            "Facility Type", "Labor Category", "Occupation", "Monitoring Status")) %>% 
      group_by(Site) %>% summarise(yvar = mean(!!input$yvar)) %>% fill_gaps()
  })
  
  output$scatter <- renderPlot({
    p <- ggplot(subsetted(), aes_string(x = input$xvar, y = input$yvar)) + geom_point()
    if (input$show_margins) {
      p <- ggExtra::ggMarginal(p, type = "density", margins = "both", size = 8)
    }
    p
  }, res = 100)
  
  # Create X Var and Y Var histograms
  output$xHist <- renderPlot({
    p <- ggplot(subsetted(), aes_string(x = input$xvar)) + geom_histogram()

    p
  }, res = 100)
  
  output$yHist <- renderPlot({
    p <- ggplot(subsetted(), aes_string(x = input$yvar)) + geom_histogram() + labs(x=input$yvar)
    
    p
  }, res = 100)

  
  output$tedGraph <- renderPlot({
    p <- subsetted() %>%
      select(`Monitoring Year`,
             photon=`Collective ED Photon (person-mrem)`, 
             neutron=`Collective ED Neutron (person-mrem)`, 
             CED=`Collective CED (person-mrem)`) %>%
      pivot_longer(c(photon, neutron, CED), names_to="dose_type") %>%
      ggplot(aes(x=`Monitoring Year`, fill=dose_type)) + geom_bar(position="stack") + labs(x="Year", y="Collective Dose (Person-mrem)", title="Components of TED")
    p
  }, res = 100)
  
  # Create TS forecast tab & plot with basic ARIMA model
  output$timeSeriesForecast <- renderPlot({
    p <- subsettedTS() %>%
      model(ARIMA(yvar)) %>%
      forecast(h = 10) %>%
      autoplot(subsettedTS()) + labs(x = "Year",
                                     y=input$yvar,
                                     title="ARIMA Forecast")
    
    p
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
