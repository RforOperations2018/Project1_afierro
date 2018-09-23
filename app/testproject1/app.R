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
body <- dashboardBody(tabItems(
  tabItem("APOST",
          fluidRow(
            selectInput("FocusSelect",
                        "Focus Area:",
                        choices = sort(unique(mAPOST$value)),
                        multiple = TRUE,
                        selectize = TRUE,
                        selected = c("STEM", "Arts & Culture"))
          )
          ),
          fluidRow(title = "Focus Areas of After School Programs in Pittsburgh",
                         width = 12, 
                         plotlyOutput("APOSTPlot")
              
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
}
# Run the application 
shinyApp(ui = ui, server = server)

#    menuItem("Table", icon = icon("table"), tabName = "table", badgeLabel = "new", badgeColor = "green"),


#  tabItem("table",
#          fluidPage(
#            box(title = "Selected Character Stats", DT::dataTableOutput("table"), width = 12))




# Data table of characters
#  output$table <- DT::renderDataTable({
#    subset(swInput(), select = c(name, height, mass, birth_year, homeworld, species, films))
#  })
# Mass mean info box
#  output$orgnumber <- renderInfoBox({
#    orgtotal <- length(unique(APOST$Organization))
#    infoBox("Total Number of Orgs", value = num, subtitle = "fill this in", icon = icon("balance-scale"), color = "purple")
#  })
# Height mean value box
#  output$height <- renderValueBox({
#    sw <- swInput()
#    num <- round(mean(sw$height, na.rm = T), 2)

#    valueBox(subtitle = "Avg Height", value = num, icon = icon("sort-numeric-asc"), color = "green")
#  })




## Body content
dashboardBody(
  tabItems(
    # First tab content
    tabItem(tabName = "dashboard",
            fluidRow(
              box(plotOutput("plot1", height = 250)),
              
              box(
                title = "Controls",
                sliderInput("slider", "Number of observations:", 1, 100, 50)
              )
            )
    ),
    
    # Second tab content
    tabItem(tabName = "widgets",
            h2("Widgets tab content")
    )
  )
)