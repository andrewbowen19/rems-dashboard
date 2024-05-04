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


df <- read.csv("https://raw.githubusercontent.com/andrewbowen19/rems-dashboard/main/rems-data.csv", check.names=FALSE)

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

ui <- page_sidebar(
  sidebar = sidebar(
    varSelectInput("xvar", "X variable", df_num, selected = "Total Number Monitored"),
    varSelectInput("yvar", "Y variable", df_num, selected = "Number with Meas. TED"),
    selectInput(
      "Site", "Filter by Site",
      choices = unique(df$Site),
      selected =  "Los Alamos National Laboratory",
      multiple=TRUE
    ),
    
    hr(), # Add a horizontal rule
    checkboxInput("site", "Show Site", TRUE),
    checkboxInput("show_margins", "Show marginal plots", TRUE),
    checkboxInput("smooth", "Add smoother"),
  ),
  plotOutput("scatter")
)

server <- function(input, output, session) {
  subsetted <- reactive({
    req(input$Site)
    df |> filter(Site %in% input$Site)
  })
  
  output$scatter <- renderPlot({
    p <- ggplot(subsetted(), aes(!!input$xvar, !!input$yvar)) + list(theme(legend.position = "bottom"),
      if (input$site) aes(color = `Program Office`),
      geom_point(),
      if (input$smooth) geom_smooth()
    )
    
    if (input$show_margins) {
      margin_type <- if (input$site) "density" else "histogram"
      p <- ggExtra::ggMarginal(p, type = margin_type, margins = "both",
                               size = 8, groupColour = input$site, groupFill = input$site)
    }
    
    p
  }, res = 100)
}

shinyApp(ui, server)