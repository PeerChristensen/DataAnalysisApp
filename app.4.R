# a minimal app trying to handle multiple data inputs/outputs in modules

library(shiny)
library(shinydashboard)
library(tidyverse)
library(glue)
library(tidymodels)
library(shinyjs)

tabUI <- function(id) {
  ns <- NS(id)
  tabsetPanel(
    tabPanel("UPLOAD DATA",
             box(
               fileInput(ns("dataset"), "Upload CSV File", accept = ".csv"),
               actionButton(ns("data_go"), "Go")
             ),
             box(
               dataTableOutput(ns("table"))
             )
    )
  )

}

tabServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$data_go, {
      data <- reactive(read_delim(input$dataset$datapath, delim=","))
    })
    
    output$table <- renderDataTable({data()})
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
      tabItem(tabName = "iris1",h2("exp 1"), tabUI("iris1_tab")),
      tabItem(tabName = "iris2",h2("exp 2"), tabUI("iris2_tab"))
    )
  )
)


server <- function(input, output, session) { 
  
  tabServer("iris1_tab")
  tabServer("iris2_tab")
  
}

shinyApp(ui, server)


