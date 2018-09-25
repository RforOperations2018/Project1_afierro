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
library(rsconnect)

Ben1 <- read_excel("Benedum.xlsx")
colnames(Ben1) <- c("Area", "Organization", "Amt")
# Make sure you reset R before trying one last time.
Ben1$name <- as.factor(Ben1$Organization)
omit.Ben <- Ben1[240:1048451,]
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
    menuItem("Spark Grants Plot", tabName = "SparkGrants", icon = icon("money")),
    menuItem("Benedum Grants Plot", tabName = "BenedumGrants", icon = icon("money")),
    menuItem("APOST Table", tabName = "APOSTPittsburgh", icon = icon("clock-o")))

)
body <- dashboardBody(
  tabItems(
    tabItem("APOST",
            fluidRow(
              valueBox(length(unique(APOST$Organization)), "Number of Orgs", icon = icon("users"), color = "purple")
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
    tabItem("SparkGrants",
            fluidRow(
              valueBox(length(unique(Spark$Name)), "Number of Grantees", icon = icon("users"), color = "red")
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
         fluidRow(
           box(title = "Spark Grantees and Amounts",
                  width = 12,
                  (plotlyOutput("SparkPlot"))
               )
           )
         ),
    tabItem("BenedumGrants",
            fluidRow(
              valueBox(length(unique(Ben1$name)), "Number of Grantees", icon = icon("users"), color = "green")
              ),
            fluidRow(
              box(
                sliderInput("BenedumSelect",
                            "Grant Amount:",
                            min = min(Ben$Amt, na.rm = T),
                            max = max(Ben$Amt, na.rm = T),
                            value = c(min(Ben$Amt, na.rm = T), max(Ben$Amt, na.rm = T)),
                            step = 10,000)
            )
          ),
          fluidRow(
            box(title = "Benedum Grantees and Amounts",
                width = 12,
                plotlyOutput("BenedumPlot")
                )
            )
          ),
    tabItem("APOSTPittsburgh",
            fluidRow(
              box(
                selectInput("OrgSelect",
                            "Organization:",
                            choices = sort(unique(APOSTtable$Organization)),
                            multiple = TRUE,
                            selectize = TRUE,
                            selected = c("University of Pittsburgh", "Carnegie Science Center"))
                 )
             ),
         fluidRow(
           box(
               downloadButton("downloadData","Download APOST Data")
             )
           ),
           fluidRow(
             box(width = 12,
               DT::dataTableOutput("table")
               )
             )
         )
    )
  )

ui <- dashboardPage(header, sidebar, body)

# Define server logic
server <- function(input, output, session) {
  APOSTInput <- reactive({
    # Pipes need to be connected to something
    APOSTreac <- APOST
      # ORG Filter
    if (length(input$FocusSelect) > 0 ) {
      # Wrong column name
      APOSTreac <- subset(APOSTreac, `Primary Focus Area` %in% input$FocusSelect)
    }
    
    return(APOSTreac)
  })
  
  observeEvent(input$reset, {
    # You have the wrong selecteds here
    updateSelectInput(session, "FocusSelect", selected = c("University of Pittsburgh", "Carnegie Science Center"))
  })
  # Reactive melted data
  mAPOSTInput <- reactive({
    APOSTInput() %>%
      melt(id = "Organization")
  })
  # APOST Plot
  output$APOSTPlot <- renderPlotly({
    # You have to call your reactive data as a function
    dat <- mAPOSTInput()
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
    # Again with not calling the function
    dat <- SparkInput() %>%
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
    dat <- BenInput()
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
      paste("data", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      # Close but you misspelled it
      write.csv(APOSTTableInput(), file)
    }
  ) 
  #APOST Table
  output$table <- DT::renderDataTable({
    # Reactive functions need the parens, and spelled correctly
    APOSTTableInput()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)