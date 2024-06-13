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
                tags$a(href="https://www.energy.gov/ehss/occupational-radiation-exposure-rems-system-tools", "All data sourced from the Department of Energy REMS Query Tool")
              )
            )
  ),
  nav_panel("Time Series",
            sidebarLayout(
              sidebar_content,
              mainPanel(plotOutput("timeSeries"))
            )
  ),
  nav_panel("TED Segments",
            sidebarLayout(
              sidebar_content,
              mainPanel(plotOutput("tedGraph"))
            )
  ),
  nav_panel("Time Series Fable",
            sidebarLayout(
              sidebar_content,
              mainPanel(plotOutput("timeSeriesFable"))
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
  
  output$scatter <- renderPlot({
    p <- ggplot(subsetted(), aes_string(x = input$xvar, y = input$yvar)) + geom_point()
    if (input$show_margins) {
      p <- ggExtra::ggMarginal(p, type = "density", margins = "both", size = 8)
    }
    p
  }, res = 100)
  
  output$timeSeries <- renderPlot({
    p <- subsetted() %>% 
      group_by(`Monitoring Year`) %>% 
      summarize(Total = sum(!!sym(input$yvar))) %>%
      ggplot(aes(x=`Monitoring Year`, y=Total)) + geom_line() + labs(x="Year", y=glue("Total {input$yvar}"), title=glue("Total {input$yvar} by Year"))
    p
  }, res = 100)
  
  output$tedGraph <- renderPlot({
    p <- subsetted() %>%
      select(`Monitoring Year`, photon=`Collective ED Photon (person-mrem)`, neutron=`Collective ED Neutron (person-mrem)`, CED=`Collective CED (person-mrem)`) %>%
      pivot_longer(c(photon, neutron, CED), names_to="dose_type") %>%
      ggplot(aes(x=`Monitoring Year`, fill=dose_type)) + geom_bar(position="stack") + labs(x="Year", y="Collective Dose (Person-mrem)", title="Components of TED")
    p
  }, res = 100)
  
  output$timeSeriesFable <- renderPlot({
    rems_tsbl <- subsetted() %>% as_tsibble(index = `Monitoring Year`,
                                            key=c("Program Office", "Operations Office", "Site", "Reporting Organization",
                                                  "Facility Type", "Labor Category", "Occupation", "Monitoring Status")) %>% 
      filter(`Program Office` == "National Nuclear Security Administration") %>% 
      group_by(Site) %>% summarise(ted = mean(`Average Meas. TED (mrem)`))
    p <- rems_tsbl %>% autoplot(ted)  + guides(colour = guide_legend(nrow = 10))
    p
  }, res=100)
}

# Run the application 
shinyApp(ui = ui, server = server)
