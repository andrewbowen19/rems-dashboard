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
df <- read.csv("https://raw.githubusercontent.com/andrewbowen19/rems-dashboard/main/rems-data.csv", check.names=FALSE)

# Only keep numeric cols for scatter plot
df_num <- df %>% select(where(is.numeric), -c(`Program Office`,
                                              `Operations Office`,
                                              `Site`,
                                              `Reporting Organization`,
                                              `Facility Type`,
                                              `Labor Category`,
                                              `Occupation`,
                                              `Monitoring Status`))
# Set up sidebar
ui <- fluidPage(
  
  # Application title
  titlePanel("REMS"),
  
  sidebarLayout(
    
    # Sidebar with a slider input
    sidebarPanel(
      selectInput("site",
                  "Site", unique(df$Site), "Savannah River Site", multiple=TRUE),
      varSelectInput("xvar", "X variable", df_num, selected = "Total Number Monitored"),
      varSelectInput("yvar", "Y variable", df_num, selected = "Number with Meas. TED"),
      selectInput(
        "program_office", "Filter by Program Office",
        choices = unique(df$`Program Office`),
        selected = "National Nuclear Security Administration",
        multiple=TRUE
      ),
      hr(), # Add a horizontal rule
    ),
    
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("scatter"),
      plotOutput("timeSeries")
    )
  )
)

server <- function(input, output, session) {
  subsetted <- reactive({

    # Filter data based on user inputs
    dat <- df %>% filter(Site %in% input$site & `Program Office` %in% input$program_office & `Monitoring Year`)
    dat
  })
  
  output$scatter <- renderPlot({
    # Plot selected data
    p <- ggplot(subsetted(), aes(!!input$xvar, !!input$yvar)) + geom_point()

    p
  }, res = 100)

  
  # Plot time series
  output$timeSeries <- renderPlot({
    # Plot selected data

    p <- subsetted() %>% group_by(`Monitoring Year`) %>% mutate(Total = sum(!!input$yvar)) %>%
          ggplot(aes(x=`Monitoring Year`, y=Total)) + geom_line()
    p
  }, res = 100)
  
}

shinyApp(ui, server)

