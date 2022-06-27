library(shiny)
library(shinydashboard)
library(tidyverse)
library(glue)
library(tidymodels)
library(shinyjs)
library(fresh)

light <- "white"
medium <- "#F0F0E9"
dark <- "#E0E0D4"
green <- "#00511D"
text <- "#555555"


columns <- names(iris)
species_choices <- unique(iris$Species)
instructions1 <- "To use this app, first select 'Upload Data' in the sidebar menu and upload a dataset"
instructions2 <- "Once read, the data undergoes a number of validation steps and you will be able to inspect the data"
instructions3 <- "If you wish to add rows to the data, select the 'Add data rows'menu item, which should be available upon data upload."
instructions4 <- "Next, you will be able to perform k-means clustering in an explorative way and inspect the results in a plot."
instructions5 <- "Finally, you will be able to generate and download a report of your analysis as well as the dataset, which will include all added rows."

ui <- dashboardPage(
  title = "K-MEANS APP",
  dashboardHeader(title = span("K-MEANS APP", 
                               style = "color: #00511D; font-size: 25px; font-weight:bold;font-family:LFPressSans,Arial,Verdana,sans-serif;")),
  dashboardSidebar(collapsed=F,
    sidebarMenu(
      menuItem("HOME", tabName = "home", icon = icon("home")),
      menuItem("UPLOAD DATA", tabName = "data_upload", icon = icon("database")),
      menuItemOutput("add_rows_menu"),
      menuItemOutput("km_menu"),
      menuItemOutput("report_menu")
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "mytheme.css"),
      tags$style("h2 {color:#555555;}; h4 {color:#555555;}; p {color:#555555;}")
      ),
    #use_theme(mytheme),
    useShinyjs(),
    tabItems(
      tabItem(tabName = "home",
              h2("HOME"),
              fluidRow(box(h4("INSTRUCTIONS"),
                p(instructions1),
                p(instructions2),
                p(instructions3),
                p(instructions4),
                p(instructions5)))
              ),
      tabItem(tabName = "data_upload",
              h2("UPLOAD DATA"),
              fluidRow(
                box(
                  fileInput("data", "Upload CSV File", accept = ".csv"),
                  tags$style(".progress-bar {background-color: #00511D;}"),
                  radioButtons("sep", "Separator",
                             choices = c(Comma = ",",
                                         Semicolon = ";"),
                             selected = ","),
                  actionButton("data_go", "Go")
                  )
                ),
              fluidRow(
                uiOutput("validation_box")
                ),
              fluidRow(
                uiOutput("data_box")
                )
              ),
      tabItem(tabName = "kmeans",
              h2("K-MEANS CLUSTERING"),
              box(
                numericInput("nclusters", "Number of clusters", value = 3),
                varSelectInput("x_var", "X variable",c()),
                varSelectInput("y_var", "Y variable",c() ),
                actionButton("kmeansbutton", "Go"),
                br(),
                plotOutput("kmeansplot")
                )
              ),
      tabItem(tabName = "report",
              h2("DOWNLOAD REPORT AND DATA"),
              box(
                textInput("report_title","Report title", value = "Report"),
                textInput("report_author","Author name"),
                textAreaInput("description", "Description"),
                downloadButton("report", "Generate report")
                ),
              fluidRow(),
              box(
                downloadButton("download_data", "Download data")
                )
              ),
      tabItem(tabName = "data_entry",
              h2("ADD DATA ROWS"),
              box(
                numericInput("Sepal.Length","Sepal.Length", value=0),
                numericInput("Sepal.Width","Sepal.Width", value=0),
                numericInput("Petal.Length","Petal.Length", value=0),
                numericInput("Petal.Width","Petal.Width", value=0),
                selectInput("Species", "Species",choices = species_choices),
                actionButton("add_row", "Add row"),
                textOutput("new_rows")
                ),
              fluidRow(
                uiOutput("new_data_box")
                )
              )
      )
    )
  )

#------------- SERVER ----------------------------------------------------

server <- function(input, output, session) { 
  
    values <- reactiveValues(df = NULL, km=NULL, added=NULL)
    
    values$added_rows <- tibble(Sepal.Length = double(),
                                Sepal.Width  = double(),
                                Petal.Length = double(),
                                Petal.Width  = double(),
                                Species      = character()
                                )
    
    observeEvent(input$data_go, { # note the scope of this..
      
      values$df <- read_delim(input$data$datapath, delim=input$sep)
      
      output$add_rows_menu <- renderMenu({
        menuItem("ADD DATA ROWS", tabName = "data_entry", icon = icon("pen"))
        })
      output$km_menu <- renderMenu({
        menuItem("K-MEANS CLUSTERING", tabName = "kmeans", icon = icon("calculator"))
      })
      output$report_menu <- renderMenu({
        menuItem("DOWNLOAD REPORT AND DATA", tabName = "report", icon = icon("file-export"))
        
      })
  
#------------- Validation ----------------------------------------------------
  
      # column match
      output$col_match <- renderText(
        {
      if (identical( names(values$df), columns) ) {"Column names match"}
      else if (!identical( names(values$df), columns) )  {"Column names do not match"}
          })
    
      # missing values
      output$missing <- renderText(
      {glue("N rows containing missing values: {sum(!complete.cases(values$df))}") })

      output$validation_box <- renderUI(box(h4("VALIDATION"),
                                        textOutput("col_match"),
                                        textOutput("missing")))
      
      # show data
      output$data_box <- renderUI(box(dataTableOutput("data_contents")))
  
      output$data_contents <- renderDataTable(
          values$df, options = list(pageLength = 5,lengthMenu = c(5, 10, 50, 100)))
  
      #------------- k-means ----------------------------------------------------
      
      choices <- values$df %>% select_if(is.numeric) %>% names()
      updateSelectInput(session, "x_var",choices = choices, selected=choices[1])
      updateSelectInput(session, "y_var",choices = choices, selected=choices[2])
  
    }) # when data is uploaded
  
  observeEvent(input$kmeansbutton, {
    
    kmeans_data <- values$df %>%
      select_if(is.numeric)
    
    kmeans_result <- kmeans_data %>% 
      kmeans(centers = input$nclusters) %>%
      augment(kmeans_data)
    
    values$km <- kmeans_result
    
    output$kmeansplot <- renderPlot(
      
      kmeans_result %>%
        ggplot(aes(!!input$x_var, !!input$y_var, colour = .cluster)) +
        geom_point(size = 2, alpha = .75) +
        theme_minimal()
      )
  })
  
  #------------- Enter/download data ----------------------------------------------------
    
    msg <- reactiveVal()
    observeEvent(input$add_row, {
    
    msg("New row added")
      
    values$df <- values$df %>%
      add_row(Sepal.Length = input$Sepal.Length,
              Sepal.Width  = input$Sepal.Width,
              Petal.Length = input$Petal.Length,
              Petal.Width  = input$Petal.Width,
              Species      = input$Species)
    
    values$added_rows <- values$added_rows  %>%
      add_row(Sepal.Length = input$Sepal.Length,
              Sepal.Width  = input$Sepal.Width,
              Petal.Length = input$Petal.Length,
              Petal.Width  = input$Petal.Width,
              Species      = input$Species)
    
    output$new_df <- renderDataTable({values$added_rows})
    
    output$new_data_box <- renderUI(box(h4("NEW DATA ENTRIES"),
                                          dataTableOutput("new_df")))
    
    output$new_rows <- renderText({msg()})

    })
    
    observeEvent(input$add_row, {
      delay(ms = 1000, msg(NULL))
    })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("dataset.csv", sep = "")
    },
    content = function(file) {
      write_csv(values$df, file)
    }
  )
  
  #-------------  Report ----------------------------------------------------
  
  output$report <- downloadHandler(

    filename = function(){
      paste0(input$report_title,".html")
    },
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)

      params <- list(title = input$report_title,
                     author = input$report_author,
                     description = input$description,
                     kmeans_result = values$km,
                     x = input$x_var,
                     y = input$y_var)
      
      render_markdown <- function(){
        rmarkdown::render(tempReport,
                          output_file = file,
                          params = params,
                          #envir = new.env(parent = globalenv())
                          )}
        
        render_markdown()
    }
  )
}

shinyApp(ui, server)


