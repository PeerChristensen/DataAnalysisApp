library(shiny)
library(shinydashboard)
library(tidyverse)
library(glue)

columns <- names(iris)

ui <- dashboardPage(
  dashboardHeader(title = "SEGES Dashboard"),
  dashboardSidebar(collapsed=F,
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Upload Data", tabName = "data_upload", icon = icon("database")),
      menuItem("Enter Data", tabName = "data_entry", icon = icon("pen")),
      menuItem("K-means clustering", tabName = "kmeans", icon = icon("calculator")),
      menuItem("Get report", tabName = "report", icon = icon("file-export"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "home",
              h2("Home"),
              fluidRow(box(h4("Instructions"),
                "Instructions on how to use the app go here"))
              ),
      tabItem(tabName = "data_upload",
              h2("Upload data"),
              fluidRow(
              box(
                fileInput("data", "Upload CSV File", accept = ".csv"),
                radioButtons("sep", "Separator",
                             choices = c(Comma = ",",
                                         Semicolon = ";"),
                             selected = ","),
                width=10
              )
              ),
              fluidRow(
                uiOutput("validation_box")
              )
              ,
              fluidRow(
                uiOutput("data_box")#,
                #box(dataTableOutput("data_contents"),width=10)

              )
      ),
      tabItem(tabName = "kmeans",
              h2("K-means clustering"),
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
              h2("Download report"),
              textInput("report_title","Report title"),
              box(downloadButton("report", "Generate report"))
              )
    )
  )
)

server <- function(input, output, session) { 
  
  df <- reactive({
    file <- input$data
    if (is.null(file)) { return(NULL) }
    if (input$sep == ",") { read_csv(file$datapath, col_names = T) }
    else if (input$sep == ";") { read_csv2(file$datapath) }
  })
  
  #------------- Validation ----------------------------------------------------
  observeEvent(input$data, {
    
    output$col_match <- renderText(
    {
      if (identical( names(df()), columns) ) {"Column names match"}
      else if (!identical( names(df()), columns) )  {"Column names do not match"}
          })
    
    output$missing <- renderText(
    {glue("N rows containing missing values: {sum(!complete.cases(df()))}") })
  
    output$validation_box <- renderUI(box(h4("Validation"),
                                        textOutput("col_match"),
                                        textOutput("missing"),
                                        width=10))
  
  output$data_box <- renderUI(box(dataTableOutput("data_contents"),width=10))
  
  output$data_contents <- renderDataTable(
          df(), options = list(pageLength = 5,lengthMenu = c(5, 10, 50, 100)))
  
  choices <- df() %>% select_if(is.numeric) %>% names()
  updateSelectInput(session, "x_var",choices = choices, selected=choices[1])
  updateSelectInput(session, "y_var",choices = choices, selected=choices[2])
  
    })
  
  #------------- k-means ----------------------------------------------------
  
  observeEvent(input$kmeansbutton, {
    
    kmeans_data <- df() %>%
      select_if(is.numeric)
    
    kmeans_result <- kmeans_data %>% 
      kmeans(centers = input$nclusters) %>%
      augment(kmeans_data)
    
    output$kmeansplot <- renderPlot(
      
     kmeans_result %>%
        ggplot(aes(!!input$x_var, !!input$y_var, colour=.cluster)) +
        geom_point() +
        theme_minimal()
      
      )
  })
  
  #-------------  Report ----------------------------------------------------
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    #filename = "report.html",
    filename = function(){
      paste0(input$report_title,".html")
    },
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(title = input$report_title,
                    kmeans_result = kmeans_result,
                     x = input$x_var,
                     y = input$y_var)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}

shinyApp(ui, server)


