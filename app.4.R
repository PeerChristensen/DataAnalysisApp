# a minimal app with modules

library(shiny)
library(shinydashboard)
library(tidyverse)
library(glue)
library(tidymodels)
library(shinyjs)

source("modules.R")
  
items <- read_csv("items.csv")
items$id <- str_replace_all(paste0("ExpID_", items$dataset, "_", items$experiment)," ","")
species_choices <- unique(iris$Species)
columns <- names(iris)


ui <- dashboardPage(
  dashboardHeader(title = "DATA ANALYSIS"),
  dashboardSidebar(collapsed = F,
                  uiOutput("menu")
  ),
  dashboardBody(
    useShinyjs(),
     tabItems(
       uiOutput("tabs")
    #   tabItem(tabName = "iris1",h2("exp 1"), tabUI("iris1_tab")),
    #   tabItem(tabName = "iris2",h2("exp 2"), tabUI("iris2_tab"))
     )
  )
)


server <- function(input, output, session) { 
  
  values <- reactiveValues(data = NULL, km=NULL, added_rows=NULL)
  
  output$menu <- create_menu_items()
  
  output$tabs <- create_tab_items()
  
  tab_ui <- unlist(items$id)
  
  map(tab_ui,tabServer,values=values)

}

shinyApp(ui, server)


