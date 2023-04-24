library(shiny)
library(shinythemes)
require(dplyr)
require(ggplot2)
library(purrr)

source("UserDefined_Functions.R")

ui <- fluidPage(
  #Add "flatly" theme from "shinythemes" package
  theme = shinytheme("flatly"),
  #Main Title of the ShinyApp 
  navbarPage("Post Illumination Burst Analyzer",
      tabPanel("Anaylze PIB", fluid = TRUE, icon = icon("table"),
           # No title for the sub tab
           titlePanel(" "),
           # Add a sidebar layout
           sidebarLayout(
             sidebarPanel(
               width = 3,
               # Define file input
               fileInput(
                 inputId = "file",
                 label = "Choose a CSV file",
                 accept = c("csv/text", ".csv", "text/comma-separated-values"),
                 multiple = FALSE,
                 buttonLabel = "Browse",
                 placeholder = "No file selected"
               ),
               p("Note: the CSV file being uploaded must contain a header row"),
               br(),
               # Define select input for x variable
               selectInput("x_var", "Select column containing time in seconds", NULL),
               # Define select input for y variable
               selectInput("y_var", "Select column containing net assimilation rate", NULL),
               br(),
               # p("If the selected data looks good, proceed to the next tab 
               #     to fit the uploaded data."),
               br(),
               br(),
               # Define checkbox for using default data
               checkboxInput("load_demo_oneBurst", strong("Demo Data"), value = FALSE),
               
          
               actionButton("Fit", "Fit"),
               
               checkboxInput("NewRd", "Change Rd fit"),
               conditionalPanel(
                 condition = "input.NewRd == true",
                 sliderInput(inputId = "LAST", label = "Dark Respiration Fit", min = 1, max = 1000, value = 50),
                 p("*Pick the number of points to include in the linear prediction of dark respiration")
               )

               
             ),
             # Define the main panel for displaying the output
             mainPanel(
               fluidRow(
                 cellWidths = c("100%"),
                 # Add "substrate" header
                 h3("Data Quality"),
                 # Add a place to display the substrate volume requirement
                 verbatimTextOutput("dataQuality"),
               ),
               fluidRow(
                 splitLayout(
                   cellWidths = c("25%", "75%"),
                   # Define table output for selected data
                   tableOutput("selected_data"),
                   # Define plot output
                   plotOutput("plot_original")),
                 dataTableOutput("fit_output"),
                 uiOutput("copyFit")
                 )
               )
             ))
))

server <- function(input, output, session) {
  
  output$dataQuality <- renderText({
    # Paste the output
    paste("Check:", "\n",
          "1) Is the assimilation rate at a steady state in the light?", "\n",
          "2) Is the respiration rate at a steady state in the dark?", "\n",
          "3) Is the noise acceptable?", "\n",
          "4) Do you see one or two burst(s)?")
  })

  # Reactive variable to track uploaded data
  data <- reactive({
    if (input$load_demo_oneBurst) {
      # Default data if check box load_demo is checked
      data <- read.csv("demo_data.csv") 
    } else if (!is.null(input$file)) {
      # Read uploaded file if one is provided
      data <- read.csv(input$file$datapath)
    } else {
      # No data if no file is uploaded and checkbox is unchecked
      data <- NULL
    }
    data
  })
  
  # Update select inputs when data is loaded
  observe({
    updateSelectInput(
      inputId = "x_var",
      #label = "Select X Variable",
      choices = names(data())
    )
    
    updateSelectInput(
      inputId = "y_var",
      #label = "Select Y Variable",
      choices = names(data())
    )
  })
  
  # Define reactive expression for selected data
  selected_data <- reactive({
    req(input$x_var, input$y_var) # Require x and y variable selections
    # Select x and y variables from data
    data() %>%
      dplyr::select(!!input$x_var,!!input$y_var)
  })
  
  data_x <- reactive({
    req(input$x_var) # Require x and y variable selections
    # Select x and y variables from data
    data() %>%
      pull(!!sym(input$x_var)) 
      
  })
  
  data_y <- reactive({
    req(input$y_var) # Require x and y variable selections
    # Select x and y variables from data
    data() %>%
      pull(!!sym(input$y_var)) 
  })
  
  # Define table output for selected data
  output$selected_data <- renderTable({
    # Check if data has been loaded
    if (!is.null(selected_data())) {
      data.frame(selected_data()) %>% head
    }
  })

  # Define plot output for original data
  output$plot_original <-
    renderPlot({
    # Check if x and y variables have been selected
    if (!is.null(input$x_var) && !is.null(input$y_var)) {
      # Create plot of selected data
      ggplot(selected_data(), aes(x = !!sym(input$x_var), y = !!sym(input$y_var))) +
        geom_point(color = "black", alpha = 0.25, size = 3) +
        geom_hline(yintercept = 0, color = "black") +
        labs(title = " ", 
             x = expression(paste("Time")), 
             y = expression(paste(italic(A), " (", mu, "mol ", m^-2, " ", s^-1, ")" ))) +
        
        theme_bw(base_size = 20)
    }
  })

  observeEvent(input$Fit, {
    observeEvent(input$NewRd, {
      output$plot_original <- renderPlot({
        plotPIB(x = data_x(), y = data_y(), LAST = input$LAST)
      })
    })
    output$plot_original <- renderPlot({
      plotPIB(x = data_x(), y = data_y(), LAST = input$LAST)
      })
  })
  
  observeEvent(input$Fit, {
    observeEvent(input$NewRd, {
      output$fit_output <- renderDataTable({
        PIB_table <<- PIBEstimation_table(x = data_x(), y = data_y(), LAST = input$LAST) 
        PIB_table},
        options = list(paging = FALSE, searching = FALSE, info = FALSE, digits = 3))
      })
    output$fit_output <- renderDataTable({
    PIBEstimation_table(x = data_x(), y = data_y(), LAST = input$LAST) },
    options = list(paging = FALSE, searching = FALSE, info = FALSE, digits = 3))
  })

  observeEvent(input$Fit, {
    output$copyFit <- renderUI({
      actionButton("copyFit", label = "Copy Fit") })
  })
  
  observeEvent(input$copyFit, 
               {clip <- pipe("pbcopy", "w")
               write.table(PIB_table, file = clip, sep = "\t", row.names = F)
               close(clip)})

  
}

shinyApp(ui = ui, server = server)
