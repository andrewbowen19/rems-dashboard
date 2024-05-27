#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(bslib)

# Read in dataset
df <- read.csv("https://raw.githubusercontent.com/andrewbowen19/rems-dashboard/main/rems-data.csv", 
               check.names=FALSE)

# Only keep numeric cols for scatter plot
df_num <- df %>% select(where(is.numeric), -c(`Monitoring Year`,
                                              `Program Office`,
                                              `Operations Office`,
                                              `Site`,
                                              `Reporting Organization`,
                                              `Facility Type`,
                                              `Labor Category`,
                                              `Occupation`,
                                              `Monitoring Status`))
# Set up sidebar
ui <- page_sidebar(
  title="Radiation Exposure Monitoring System (REMS)",
  sidebar = sidebar(
    varSelectInput("xvar", "X variable", df_num, selected = "Total Number Monitored"),
    varSelectInput("yvar", "Y variable", df_num, selected = "Average Meas. TED (mrem)"),
    selectInput(
      "Site", "Filter by Site",
      choices = unique(df$Site),
      selected = "Los Alamos National Laboratory",
      multiple=TRUE
    ),
    selectInput(
      "program_office", "Filter by Program Office",
      choices = unique(df$`Program Office`),
      selected = "National Nuclear Security Administration",
      multiple=TRUE
    ),
    hr(), # Add a horizontal rule
    checkboxInput("site", "Show Site", TRUE),
    checkboxInput("show_margins", "Show marginal plots", TRUE),
    checkboxInput("smooth", "Add smoother"),
  ),
  fillable=FALSE,
  tags$a(href="https://www.energy.gov/ehss/occupational-radiation-exposure-rems-system-tools",
         "Data sourced from the Department of Energy's Radiation Exposure Query tool."),
  plotOutput("scatter"),
  plotOutput("timeSeries")
)

server <- function(input, output, session) {
  subsetted <- reactive({
    req(input$Site)
    # Filter data based on user inputs
    df |> filter(Site %in% input$Site & `Program Office` %in% input$program_office)
  })
  
  subsettedTS <- reactive({
    req(input$`Monitoring Year`)
    # Filter data based on user inputs
    df |> filter(Site %in% input$Site & `Program Office` %in% input$program_office)
  })
  
  output$scatter <- renderPlot({
    # Plot selected data
    p <- ggplot(subsetted(), aes(!!input$xvar, !!input$yvar)) + list(theme(legend.position = "bottom"),
      if (input$site) aes(color = `Program Office`),
      geom_point(),
      if (input$smooth) geom_smooth()
    )
    
    # Add margins if necessary
    if (input$show_margins) {
      margin_type <- if (input$site) "density" else "histogram"
      p <- ggExtra::ggMarginal(p, type = margin_type, margins = "both",
                               size = 8, groupColour = input$site, groupFill = input$site)
    }
    
    
    p
  }, res = 100)
  print(subsettedTS)
  # Add time series
  output$timeSeries <- renderPlot({
    p2 <- ggplot(subsettedTS(),
                 aes(!!input$`Monitoring Year`,
                     !!input$yvar))  + geom_line()

    # return ts plot
    p2
  }, res=100)
  
  
}

shinyApp(ui, server)