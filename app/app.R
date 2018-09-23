# Allyson Fierro
# Shiny Dashboard-Project 1

library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(readxl)
library(tidyr)
library(tidyverse)

Ben1 <- read_excel("Benedum.xlsx")
colnames(Ben1) <- c("Area", "Organization", "Amt")
Ben1$name <- as.factor(Ben$Organization)
omit.Ben <- Ben1[240:1048451, ]
na.omit(omit.Ben)
Ben <- drop_na(Ben1)

Spark <- read_excel("Spark.xlsx")
colnames(Spark) <- c("Amt", "Name")
Spark$Name <- as.factor(Spark$Name)

APOST <-read_excel("APOST.xls")

mAPOST.1 <- melt(APOST, id.vars = "Organization")
mAPOST.1$variable <- NULL
mAPOST <- drop_na(mAPOST.1)

APOSTtable <- read_excel("APOSTtable.xls")

pdf(NULL)

header <- dashboardHeader(title = "Remake Learning")

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("APOST Plot", tabName = "APOST", icon = icon("bar-chart")),
    menuItem("Spark Grants Plot", tabName = "Spark Grants", icon = icon("money")),
    menuItem("Benedum Grants Plot", tabName = "Benedum Grants", icon = icon("money")),
    menuItem("APOST Table", tabName = "APOST Pittsburgh", icon = icon("clock-o")))

)
body <- dashboardBody(tabItems(
  tabItem("APOST",
          fluidRow(valueBox(length(unique(APOST$Organization)), "Number of Orgs", icon = icon("users"), color = "purple")
                   ),
          fluidRow(
            box(
              selectInput("FocusSelect",
                          "Focus Area:",
                          choices = sort(unique(mAPOST$value)),
                          multiple = TRUE,
                          selectize = TRUE,
                          selected = c("STEM", "Arts & Culture")),
              actionButton("reset", "Reset Filters", icon = icon("refresh")) 
          )
  ),
          fluidRow(
            box(title = "Focus Areas of After School Programs in Pittsburgh",
                   width = 12,
                   plotlyOutput("APOSTPlot")
          )
    )
  ),
 tabItem("Spark Grants",
         fluidRow(valueBox(length(unique(Spark$Name)), "Number of Grantees", icon = icon("users"), color = "red")
         ),
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
         fluidPage(
           box(title = "Spark Grantees and Amounts",
                  width = 12,
                  (plotlyOutput("SparkPlot"))
         )
       )
 ),
  tabItem("Benedum Grants",
          fluidRow(valueBox(length(unique(Ben$name)), "Number of Grantees", icon = icon("users"), color = "green")
          ),
          fluidRow(
            box(sliderInput("BenedumSelect",
                            "Grant Amount:",
                            min = min(Ben$Amt, na.rm = T),
                            max = max(Ben$Amt, na.rm = T),
                            value = c(min(Ben$Amt, na.rm = T), max(Ben$Amt, na.rm = T)),
                                  step = 10,000)
            )
          ),
          fluidPage(
            box(title = "Benedum Grantees and Amounts",
                   width = 12,
                   (plotlyOutput("BenedumPlot"))
          )
      )
),
 tabItem("APOST Pittsburgh",
           fluidRow(
             box(selectInput("OrgSelect",
                             "Organization:",
                             choices = sort(unique(APOSTtable$Organization)),
                             multiple = TRUE,
                             selectize = TRUE,
                             selected = c("University of Pittsburgh", "Carnegie Science Center"))
             )),
         fluidRow(
           box(inputPanel(
             downloadButton("downloadData","Download APOST Data")
           ))),
           fluidPage(
             box(DT::dataTableOutput("table")
                 
             ))
)
  ))
ui <- dashboardPage(header, sidebar, body)

# Define server logic
server <- function(input, output) {
  APOSTInput <- reactive({
    APOSTreac <- APOST %>%
    # ORG Filter
    if (length(input$FocusSelect) > 0 ) {
      APOSTreac <- subset(APOSTreac, value %in% input$FocusSelect)
    }
    
    return(APOSTreac)
  })
  
  observeEvent(input$reset, {
    updateSelectInput(session, "FocusSelect", selected = c("University of Pittsburgh", "Carnegie Science Center"))
  })
  # Reactive melted data
  mAPOSTInput <- reactive({
    APOSTInput() %>%
      melt(id = "Organization")
  })
  # APOST Plot
  output$APOSTPlot <- renderPlotly({
    dat <- mAPOST
      ggplot(data = dat, aes(x = value, fill = "value", na.rm = TRUE)) + 
      geom_bar(stat = "count") + 
      theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)) +
      labs(x = "Program Focus Areas", y = "Number of Programs", title = "APOST Programs' Focus Areas") +
      theme(legend.position="none")
  })
  
  SparkInput <- reactive({
    Sparkreac <- Spark %>%
    # Amt Filter
      filter(Amt >= input$SparkSelect[1] & Amt <= input$SparkSelect[2])

      return(Sparkreac)
  })
  # Spark Plot
  output$SparkPlot <- renderPlotly({
    dat <- Spark %>%
      group_by(Name) %>% 
      summarise(Amt = sum(Amt))
    ggplot(data = dat, aes(x = Name, y = Amt)) +
      geom_bar(stat = "identity", fill = "#663096") +
      labs(x = "Grantee", y = "Total Amount Awarded", title = "Spark Grants") +
      coord_flip() +
      theme(legend.position="none")
  })
  
  BenInput <- reactive({
    DF <- Ben %>%
      # Amt Filter
      filter(Amt >= input$BenedumSelect[1] & Amt <= input$BenedumSelect[2])
    
    return(DF)
  })
  # Benedum Plot
  output$BenedumPlot <- renderPlotly({
    dat <- Ben
      ggplot(data = dat, aes(x = name, y = Amt, fill = "Amt")) +
      geom_bar(stat = "identity") +
      labs(x = "Grantee", y = "Total Amount Awarded", title = "Benedum Grants") +
      coord_flip() +
      theme(legend.position="none")
  })
  #Table Filter
  APOSTTableInput <- reactive({
    DF <- APOSTtable
    # ORG Filter
    if (length(input$OrgSelect) > 0 ) {
      DF <- subset(DF, Organization %in% input$OrgSelect)
    }
    
    return(DF)
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$APOSTtable, Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(APOSTTbleInput(), file)
    }
  ) 
  #APOST Table
  output$table <- DT::renderDataTable({
    APOSTtable
  })
  }

# Run the application 
shinyApp(ui = ui, server = server)