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
Spark$name <- as.factor(Spark$Name)

APOST <-read_excel("APOST.xls")

mAPOST <- melt(APOST, id.vars = "Organization")
mAPOST$variable <- NULL

pdf(NULL)

header <- dashboardHeader(title = "Remake Learning"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("APOSTPlot", icon = icon("clock-o"), tabName = "APOST"),
    menuItem("SparkPlot", icon = icon("money"), tabName = "Spark Grants"),
    menuItem("BenedumPlot", icon = icon("money"), tabName = "Benedum Grants"),
#    menuItem("Table", icon = icon("table"), tabName = "table", badgeLabel = "new", badgeColor = "green"),
    #Benedum Selection
      sliderInput("BenedumSelect",
                  "Grant Amount:",
                  min = min(Ben$Amt, na.rm = T),
                  max = max(Ben$Amt, na.rm = T),
                  value = c(min(Ben$Amt, na.rm = T), max(Ben$Amt, na.rm = T)),
                step = 1)
  )
)

body <- dashboardBody(tabItems(
  tabItem("APOSTPlot",
          fluidRow(
            box(#APOST Selection
              selectInput("OrgSelect",
                          "Organization:",
                          choices = sort(unique(mAPOST$Organization)),
                          multiple = TRUE,
                          selectize = TRUE,
                          selected = c("University of Pittsburgh", "Carnegie Science Center"))
              ),
            infoBoxOutput("mass"),
            valueBoxOutput("height")
          ),
          fluidRow(
            box(title = "idk what to put here",
                   width = 12,
                   plotlyOutput("plot_mass"))
          )
  ),
 tabItem("SparkPlot",
         fluidRow(
           box(
             sliderInput("SparkSelect",
                         "Grant Amount:",
                         min = min(Spark$Amt, na.rm = T),
                         max = max(Spark$Amt, na.rm = T),
                         value = c(min(Spark$Amt, na.rm = T), max(Spark$Amt, na.rm = T)),
                         step = 1)
           )
         )  ),
           infoBoxOutput("mass"),
           valueBoxOutput("height"),
         fluidRow(
           box(title = "Plot",
                  width = 12,
                  plotlyOutput("plot_mass"))
         )
 ),
  tabItem("BenedumPlot",
          fluidRow(
            infoBoxOutput("mass"),
            valueBoxOutput("height")
          ),
          fluidRow(
            box(title = "Plot",
                   width = 12,
                   plotlyOutput("plot_mass"))
          )
  ),
  tabItem("table",
          fluidPage(
            box(title = "Selected Character Stats", DT::dataTableOutput("table"), width = 12))
  )
)

ui <- dashboardPage(header, sidebar, body)

# Define server logic
server <- function(input, output) {
  swInput <- reactive({
    starwars <- starwars.load %>%
      # Slider Filter
      filter(birth_year >= input$birthSelect[1] & birth_year <= input$birthSelect[2])
    # Homeworld Filter
    if (length(input$worldSelect) > 0 ) {
      starwars <- subset(starwars, homeworld %in% input$worldSelect)
    }
    
    return(starwars)
  })
  # Reactive melted data
  mwInput <- reactive({
    swInput() %>%
      melt(id = "name")
  })
  # A plot showing the mass of characters
  output$plot_mass <- renderPlotly({
    dat <- subset(mwInput(), variable == "mass")
    ggplot(data = dat, aes(x = name, y = as.numeric(value), fill = name)) + geom_bar(stat = "identity")
  })
  # A plot showing the height of characters
  output$plot_height <- renderPlotly({
    dat <- subset(mwInput(),  variable == "height")
    ggplot(data = dat, aes(x = name, y = as.numeric(value), fill = name)) + geom_bar(stat = "identity")
  })
  # Data table of characters
  output$table <- DT::renderDataTable({
    subset(swInput(), select = c(name, height, mass, birth_year, homeworld, species, films))
  })
  # Mass mean info box
  output$mass <- renderInfoBox({
    sw <- swInput()
    num <- round(mean(sw$mass, na.rm = T), 2)
  #in here is where you put the sum of orgs 
    
    infoBox("Avg Mass", value = num, subtitle = paste(nrow(sw), "characters"), icon = icon("balance-scale"), color = "purple")
  })
  # Height mean value box
  output$height <- renderValueBox({
    sw <- swInput()
    num <- round(mean(sw$height, na.rm = T), 2)
    
    valueBox(subtitle = "Avg Height", value = num, icon = icon("sort-numeric-asc"), color = "green")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)