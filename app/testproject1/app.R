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

pdf(NULL)

header <- dashboardHeader(title = "Remake Learning")

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("APOSTPlot", icon = icon("clock-o"), tabName = "APOST"),
    menuItem("SparkPlot", icon = icon("money"), tabName = "Spark Grants"),
    menuItem("BenedumPlot", icon = icon("money"), tabName = "Benedum Grants"))
  #    menuItem("Table", icon = icon("table"), tabName = "table", badgeLabel = "new", badgeColor = "green"),
)
body <- dashboardBody(
  tabItems(
    tabItem("APOSTPlot",
            fluidRow(
              box(
                selectInput("OrgSelect",
                            "Organization:",
                            choices = sort(unique(mAPOST$Organization)),
                            multiple = TRUE,
                            selectize = TRUE,
                            selected = c("University of Pittsburgh", "Carnegie Science Center"))
              )),
            fluidRow(
              box(title = "idk what to put here",
                  width = 12,
                  plotlyOutput("APOSTPlot"))
            ))
            )
    )

ui <- dashboardPage(header, sidebar, body)

# 
server <- function(input, output) {
  APOSTInput <- reactive({
    DF <- APOST
    # ORG Filter
    if (length(input$OrgSelect) > 0 ) {
      DF <- subset(DF, Organization %in% input$OrgSelect)
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
    dat <- mAPOSTInput()
      ggplot(data = dat, aes(x = value, fill = "value", na.rm = TRUE)) + 
      geom_bar(stat = "count") + 
      labs(x = "Program Focus Areas", y = "Number of Programs", title = "APOST Programs' Focus Areas") +
      theme(legend.position="none")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

