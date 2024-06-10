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
library(tidyr)
library(ggplot2)
library(bslib)
library(ggExtra)
library(glue)

# Read in dataset: return to Git link when pushed!!
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

# Set up sidebar
ui <- fluidPage(
  
  # Application title
  titlePanel("Radiation Exposure Monitoring System Dashboard", windowTitle = "REMS Dashboard"),
  
  sidebarLayout(
    
    # Sidebar with a slider input
    sidebarPanel(
      selectInput("site",
                  "Site", unique(df$Site), "Savannah River Site", multiple=TRUE),
      varSelectInput("xvar", "X variable", df_num, selected = "Total Number Monitored"),
      varSelectInput("yvar", "Y variable", df_num, selected = "Average Meas. TED (mrem)"),
      selectInput(
        "program_office", "Filter by Program Office",
        choices = unique(df$`Program Office`),
        selected = "National Nuclear Security Administration",
        multiple=TRUE
      ),
      sliderInput("year", "Year Range", value = c(1986, 2022), min=1986, max=2022),
      checkboxInput("show_site", "Show Site", TRUE),
      checkboxInput("show_margins", "Show marginal plots", TRUE),
      tags$a(href="https://orise.orau.gov/cer/rems/definitions.pdf", "Definitions")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tags$a(href="https://www.energy.gov/ehss/occupational-radiation-exposure-rems-system-tools",
             "All data sourced from the Department of Energy REMS Query Tool"),
      tabsetPanel(tabPanel("Plots", plotOutput("scatter"),
        hr(),
        plotOutput("timeSeries"),
        hr(),
        plotOutput("tedGraph"), 
      ))
    )
  )
)

server <- function(input, output, session) {
  subsetted <- reactive({
    # Filter data based on user inputs
    dat <- df %>% filter(Site %in% input$site &
                        `Program Office` %in% input$program_office &
                        `Monitoring Year` %in% seq(input$year[1], input$year[2]))
    dat
  })
  
  output$scatter <- renderPlot({
    # Plot selected data
    p <- ggplot(subsetted(), aes(!!input$xvar, !!input$yvar)) + geom_point()
    # Show marginal histograms if desired
    p <- ggExtra::ggMarginal(p, type = "density", margins = "both",
                             size = 8)#, groupColour = input$show_site, groupFill = input$show_site)
    
    p

  }, res = 100)

  # Plot time series
  output$timeSeries <- renderPlot({
    # Plot selected data summed over timeframe
    p <- subsetted() %>% 
      group_by(`Monitoring Year`) %>% 
      mutate(Total = sum(!!input$yvar)) %>%
      ggplot(aes(x=`Monitoring Year`, y=Total)) + geom_line() + labs(x="Year", 
                                                                     y=glue("Total {input$yvar}"),
                                                                     title=glue("Total {input$yvar} by Year"))
    p
  }, res = 100)
  
  # Plot total effective dose (TODO: check dose def/original dashboard)
  output$tedGraph <- renderPlot({
    p <- subsetted() %>%
      select(`Monitoring Year`, 
             photon=`Collective ED Photon (person-mrem)`, 
             neutron=`Collective ED Neutron (person-mrem)`, 
             CED=`Collective CED (person-mrem)`) %>%
      pivot_longer(c(photon, neutron, CED), 
                   names_to="dose_type") %>%
      ggplot(aes(x=`Monitoring Year`, fill=dose_type)) + geom_bar() + labs(x="Year", 
                                                                           y="Collective Dose (Person-mrem)",
                                                                           title="Components of TED")
    p
  }, res = 100)
  
}

shinyApp(ui, server)

