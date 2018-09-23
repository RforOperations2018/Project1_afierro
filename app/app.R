# Allyson Fierro
# Shiny Dashboard-Project 1

library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(readxl)
library(tidyverse)

Ben <- read_excel("Benedum.xlsx")
colnames(Ben) <- c("Area", "Organization", "Amt")
Ben$name <- as.factor(Ben$Organization)

Spark <- read_excel("Spark.xlsx")
colnames(Spark) <- c("Amt", "Name")
Spark$Name <- as.factor(Spark$Name)

APOST <-read_excel("APOST.xls")

mAPOST <- melt(APOST, id.vars = "Organization")
mAPOST$variable <- NULL

View(mAPOST)

pdf(NULL)

header <- dashboardHeader(title = "Remake Learning")

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("APOSTPlot", icon = icon("clock-o"), tabName = "APOST"),
    menuItem("SparkPlot", icon = icon("money"), tabName = "Spark Grants"),
    menuItem("BenedumPlot", icon = icon("money"), tabName = "Benedum Grants"))
)
body <- dashboardBody(tabItems(
  tabItem("APOST",
          fluidRow(
            box(
              selectInput("FocusSelect",
                          "Focus Area:",
                          choices = sort(unique(mAPOST$value)),
                          multiple = TRUE,
                          selectize = TRUE,
                          selected = c("STEM", "Arts & Culture"))
          ),
          fluidRow(
            box(title = "Focus Areas of After School Programs in Pittsburgh",
                   width = 12,
                   plotlyOutput("APOSTPlot"))
          )
        )
),
 tabItem("Spark Grants",
         fluidRow(
           box(
             sliderInput("SparkSelect",
                         "Grant Amount:",
                         min = min(Spark$Amt, na.rm = T),
                         max = max(Spark$Amt, na.rm = T),
                         value = c(min(Spark$Amt, na.rm = T), max(Spark$Amt, na.rm = T)),
                         step = 1,000)
           )
         ),
         fluidRow(
           box(title = "say something about the plot",
                  width = 12,
                  (plotlyOutput("SparkPlot"))
         )
       )
 ),
  tabItem("Benedum Grants",
          fluidRow(
            box(sliderInput("BenedumSelect",
                            "Grant Amount:",
                            min = min(Ben$Amt, na.rm = T),
                            max = max(Ben$Amt, na.rm = T),
                            value = c(min(Ben$Amt, na.rm = T), max(Ben$Amt, na.rm = T)),
                                  step = 10,000)
            )
          ),
          fluidRow(
            box(title = "say something about the plot",
                   width = 12,
                   (plotlyOutput("BenedumPlot"))
          )
      )
    )
  )
)

ui <- dashboardPage(header, sidebar, body)

# Define server logic
server <- function(input, output) {
  APOSTInput <- reactive({
    DF <- APOST
    # ORG Filter
    if (length(input$FocusSelect) > 0 ) {
      DF <- subset(DF, value %in% input$FocusSelect)
    }
    
    return(DF)
  })
  # Reactive melted data
  mAPOSTInput <- reactive({
    mAPOST <- melt(APOSTInput(), id.vars = "Organization")
    mAPOST$variable <- NULL
    
    return(mAPOST)
  })
  # APOST Plot
  output$APOSTPlot <- renderPlotly({
    mAPOSTInput() %>% 
      drop_na(value) %>%
      ggplot(aes(x = value, fill = "value", na.rm = TRUE)) + 
      geom_bar(stat = "count") + 
      labs(x = "Program Focus Areas", y = "Number of Programs", title = "APOST Programs' Focus Areas") +
      theme(legend.position="none")
  })
  
  SparkInput <- reactive({
    DF <- Spark %>%
    # ORG Filter
      filter(Amt >= input$SparkSelect[1] & Amt <= input$SparkSelect[2])

      return(DF)
  })
  # Spark Plot
  output$SparkPlot <- renderPlotly({
    SparkplotInput() %>% 
      group_by(name) %>% 
      summarise(Amt = sum(Amt))
    ggplot(aes(x = name, y = Amt)) +
      geom_bar(stat = "identity", fill = "#663096") +
      labs(x = "Grantee", y = "Total Amount Awarded", title = "Spark Grants") +
      coord_flip() +
      theme(legend.position="none")
  })
  # Benedum Plot
  output$BenedumPlot <- renderPlotly({
    BendedumplotInput() %>% 
      group_by(Organization) %>% 
      summarise(Amt = sum(Amt)) %>%
      drop_na(Amt) %>%
      ggplot(aes(x = Organization, y = Amt, fill = "Amt")) +
      geom_bar(stat = "identity") +
      labs(x = "Organization", y = "Total Amount Awarded", title = "Benedum Grants") +
      coord_flip() +
      theme(legend.position="none")
  })

}

# Run the application 
shinyApp(ui = ui, server = server)