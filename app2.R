library(shiny)
library(shinydashboard)
library(tidyverse)
library(glue)
library(tidymodels)
library(shinyjs)

columns <- names(iris)
species_choices <- unique(iris$Species)

instructions1 <- "To use this app, first select 'Upload Data' in the sidebar menu and upload a dataset"
instructions2 <- "Once read, the data undergoes a number of validation steps and you will be able to inspect the data"
instructions3 <- "If you wish to add rows to the data, select the 'Add data rows'menu item, which should be available upon data upload."
instructions4 <- "Next, you will be able to perform k-means clustering in an explorative way and inspect the results in a plot."
instructions5 <- "Finally, you will be able to generate and download a report of your analysis as well as the dataset, which will include all added rows."

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
    reactive(read_delim(input$data$datapath, delim=input$sep))
  })
}

tabServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$tab <- renderUI({ 
      tabsetPanel(
        tabPanel("UPLOAD DATA",
          fluidRow(
            box(
            datasetInputUI("dataset"),
            tags$style(".progress-bar {background-color: #00511D;}"),
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";"),
                         selected = ","),
            actionButton("data_go", "Go")
              ),
            box(
              dataTableOutput("data")
            )
            )
          ),
        tabPanel("ADD ROWS"),
        tabPanel("RUN SCRIPT"),
        tabPanel("GET REPORT")
      )
    })
  })
}

ui <- dashboardPage(
  dashboardHeader(title = "DATA ANALYSIS"),
  dashboardSidebar(collapsed = F,
                   sidebarMenu(
                     menuItem("HOME", tabName = "home", icon = icon("home"), selected = T),
                     menuItem("IRIS", tabName = "iris", icon = icon("leaf"),
                              menuSubItem('EXPERIMENT 1', tabName = 'iris1'),
                              menuSubItem('EXPERIMENT 2', tabName = 'iris2'),
                              menuItemOutput("new_exp_iris"),
                              menuSubItem("NEW EXPERIMENT", tabName = "iris_new")
                              ),
                     menuItem("TITANIC", tabName = "titanic", icon = icon("ship"))
                     )
                   ),
  dashboardBody(
    useShinyjs(),
    tabItems(
    tabItem(tabName = "home",
            ),
    tabItem(tabName = "iris1",
            h2("IRIS"),
            p("description here"),
            fluidRow(
              box(
                h2("EXPERIMENT 1"),
                tabUI("iris1_tab")
                ),
              )
            ),
    tabItem(tabName = "iris2",
            h2("IRIS"),
            p("description here"),
            fluidRow(
              box(
                h2("EXPERIMENT 2"),
                tabUI("iris2_tab"),
                ),
              )
            ),
    tabItem(tabName = "iris_new",
              h2("IRIS"),
              p("description here"),
              fluidRow(
                box(
                  h2("NEW EXPERIMENT"),
                  textInput("new_exp_iris_name", "Experiment name"),
                  actionButton("new_exp_iris", "Create new experiment")
                )
              )
            ),
    tabItem(tabName = "titanic",
            h2("TITANIC"),
            p("description here"),
            fluidRow(
              box(
                h2("NEW EXPERIMENT"),
                textInput("new_exp_titanic_name", "Experiment name"),
                actionButton("new_exp_titanic", "Create new experiment")
                )
              )
            )
    )
    )
)


server <- function(input, output, session) { 
  
  values <- reactiveValues(df = NULL, km=NULL, added=NULL)
  
  observeEvent(input$data_go, {
  
  values$df <- datasetServer("dataset")
  
  output$data <- renderDataTable(head(values$df))
  
  })
  
  tabServer("iris1_tab")
  tabServer("iris2_tab")
  

  # observeEvent(input$new_exp_iris, {
  #   
  #   output$new_exp_iris <- renderMenu({
  #     menuSubItem(input$new_exp_iris_name, tabName = "iris")
  #   })
  # })
  
}

shinyApp(ui, server)


