# a minimal app trying to handle multiple data inputs/outputs in modules

library(shiny)
library(shinydashboard)
library(tidyverse)
library(glue)
library(tidymodels)
library(shinyjs)

source("modules.R")

create_menu_items <- function(x) {
  
  renderMenu({
    menu_list <- lapply(
      unique(items$dataset),
      function(x) {
        sub_menu_list = lapply(
          items[items$dataset == x,]$experiment,
          function(y) {
            menuSubItem(y, tabName = str_replace_all(paste0("expID_", x, "_", y)," ","")
                        )
          }
        )
        menuItem(text = x, do.call(tagList, sub_menu_list))
      }
    )
    sidebarMenu(menu_list)
  })
}
  

create_tab_items <- function(x) {
  
  tab_names <- list(items$id)
  headers <- map(items$experiment,h2)
  
  
  renderMenu({
    tabItem(do.call(t,tab_names), do.call(tagList, sub_menu_list))
    
    # tab_list <- lapply(
    #   tab_names,
    #   function(x) {
    #     tab_list = lapply(
    #       items[items$dataset == x,]$experiment)
          
      # tabItems(
      #     tabItem(tabName = "expID_iris_exp1",h2("exp 1"), tabUI("iris1_tab")),
      #     tabItem(tabName = "iris2",h2("exp 2"), tabUI("iris2_tab"))
      #     )
    })
}

items <- read_csv("items.csv")
items$id <- str_replace_all(paste0("ExpID_", items$dataset, "_", items$experiment)," ","")
species_choices <- unique(iris$Species)
columns <- names(iris)


ui <- dashboardPage(
  dashboardHeader(title = "DATA ANALYSIS"),
  dashboardSidebar(collapsed = F,
                  uiOutput("menu")
                  # sidebarMenu(
                  #   menuItem("IRIS", tabName = "iris", icon = icon("leaf"),
                              #menuSubItem('EXPERIMENT 1', tabName = 'iris1'),
                              #menuSubItem('EXPERIMENT 2', tabName = 'iris2')
                  #   )
                  # )
  ),
  dashboardBody(
    useShinyjs(),
    # tabItems(
    #   tabItem(tabName = "iris1",h2("exp 1"), tabUI("iris1_tab")),
    #   tabItem(tabName = "iris2",h2("exp 2"), tabUI("iris2_tab"))
    # )
    uiOutput("tabs")
  )
)


server <- function(input, output, session) { 
  
  values <- reactiveValues(data = NULL, km=NULL, added_rows=NULL)
  
  output$menu <- create_menu_items()
  
  output$tabs <- create_tab_items()
  
  tabServer("iris1_tab", values = values)
  tabServer("iris2_tab", values = values)
  
}

shinyApp(ui, server)


