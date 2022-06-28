# a minimal app trying to handle multiple data inputs/outputs in modules

library(shiny)
library(shinydashboard)
library(tidyverse)
library(glue)
library(tidymodels)
library(shinyjs)

tabUI <- function(id) {
  tagList(
    uiOutput(NS(id,"tab"))
  )
}

datasetInputUI <- function(id) {
  fileInput(NS(id,"dataset"), "Upload CSV File", accept = ".csv")
}

datasetServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {

    reactive(read_delim(input$dataset$datapath, delim=","))
  })
}

tabServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$tab <- renderUI({ 
      tabsetPanel(
        tabPanel("UPLOAD DATA",
                   box(
                     datasetInputUI("dataset"),
                     actionButton("data_go", "Go")
                   ),
                   box(
                     dataTableOutput("data")
                   )
        )
      )
    })
  })
}

ui <- dashboardPage(
  dashboardHeader(title = "DATA ANALYSIS"),
  dashboardSidebar(collapsed = F,
                   sidebarMenu(
                     menuItem("IRIS", tabName = "iris", icon = icon("leaf"),
                              menuSubItem('EXPERIMENT 1', tabName = 'iris1'),
                              menuSubItem('EXPERIMENT 2', tabName = 'iris2')
                              )
                     )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(tabName = "iris1", tabUI("iris1_tab")),
      tabItem(tabName = "iris2", tabUI("iris2_tab"))
      )
      )
)

server <- function(input, output, session) { 
  
  tabServer("iris1_tab")
  tabServer("iris2_tab")

  observeEvent(input$data_go, {
    data <- datasetServer("dataset")
    output$data <- renderDataTable(data())
  })
  
}

shinyApp(ui, server)


