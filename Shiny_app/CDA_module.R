# Required Libraries
library(shiny)
library(shinyjs)
library(ggstatsplot)
library(tidyverse)
library(ggside)
library(rlang)

# Sample dataset (Replace with your actual dataset)
Property_data <- read_csv("data/Property_data_cleaned.csv")

# Data Wrangling (You can skip this if it's already in your data)
Property_data$Season <- factor(Property_data$Season, 
                               levels = c("Spring", "Summer", "Fall", "Winter"), ordered = TRUE)

Property_data$BusStop_binned <- factor(Property_data$BusStop_binned, 
                                       levels = c("0-5", "6-10", "11-20", "21-30", ">30"), ordered = TRUE)
Property_data$Floor_binned <- factor(Property_data$Floor_binned, 
                                     levels = c("Low", "Middle", "Middle-high", "High", "Top"), ordered = TRUE)

# Removing spaces from column names
colnames(Property_data) <- gsub(" ", "_", colnames(Property_data))

# CDA Module UI
cda_ui <- function(id) {
  ns <- NS(id)  # Namespace for the module
  
  fluidPage(
    useShinyjs(),  # Enable shinyjs functionality
    
    tabsetPanel(
      # Correlation Heatmap & Scatterplot Tab
      tabPanel(
        "Correlation Analysis",
        sidebarLayout(
          sidebarPanel(
            # Scrollable checkbox list
            div(style = "height: 60px; overflow-y: scroll;",   
                checkboxGroupInput(ns("corr_vars"), "Select Continuous Variables", 
                                   choices = names(Property_data)[sapply(Property_data, is.numeric) & !names(Property_data) %in% c("Longitude", "Latitude")],
                                   selected = c("Property_Prices", "Floor","Parking"))),
            
            # Statistical approach selection
            selectInput(ns("stat_approach"), "Select Statistical Approach", 
                        choices = c("parametric", "nonparametric", "robust", "bayes"), 
                        selected = "parametric"),
            
            # Significance level input
            numericInput(ns("sig_level"), "Select Significance Level", 
                         min = 0, max = 1, value = 0.05, step = 0.01),
            
            # p.adjust.method input
            selectInput(ns("p_adjust"), "Select p.adjust.method", 
                        choices = p.adjust.methods, selected = "bonferroni"),
            
            # Select two variables for scatterplot
            selectInput(ns("scatter_x"), "Select X Variable for Scatterplot", 
                        choices = names(Property_data)[sapply(Property_data, is.numeric) & !names(Property_data) %in% c("Longitude", "Latitude")],
                        selected="Property_Prices"),
            selectInput(ns("scatter_y"), "Select Y Variable for Scatterplot", 
                        choices = names(Property_data)[sapply(Property_data, is.numeric) & !names(Property_data) %in% c("Longitude", "Latitude")],
                        selected="Floor"),
            
            # Range filter for scatterplot variables
            uiOutput(ns("scatter_range_ui")),
          ),
          
          mainPanel(
            fluidRow(
              column(12, 
                     div(
                       style = "border: 2px solid #2e6c40; padding: 0px; margin: 0;margin-bottom: 2px;",
                       plotOutput(ns("correlation_heatmap"), height = "300px"))
              )
            ),
            fluidRow(
              column(12, 
                     div(
                       style = "border: 2px solid #2e6c40; padding: 0px; margin: 0;",
                       plotOutput(ns("scatter_plot"), height = "350px"))
              )
            )
          )
        )
      ),
      
      # Second Tab - Boxplot
      tabPanel("Groupwise Comparisons",
               div(id = 'message', style = 'color:red; font-size:20px; text-align: center;', 'Please wait patiently for the graph to show up...'),
               sidebarLayout(
                 sidebarPanel(
                   # Select categorical variable
                   selectInput(ns("box_categorical"), "Select Categorical Variable", 
                               choices = names(Property_data)[sapply(Property_data, is.character) | sapply(Property_data, is.factor)]),
                   # Select continuous variable
                   selectInput(ns("box_continuous"), "Select Continuous Variable", 
                               choices = names(Property_data)[sapply(Property_data, is.numeric)& !names(Property_data) %in% c("Longitude", "Latitude")]),
                   # Select statistical approach
                   selectInput(ns("box_stat_approach"), "Select Statistical Approach", 
                               choices = c("parametric", "nonparametric", "robust", "bayes"), 
                               selected = "parametric"),
                   # Select pairwise comparisons
                   selectInput(ns("box_pairwise_display"), "Select Pairwise Comparisons", 
                               choices = c("significant" = "s", "non-significant" = "ns", "all" = "all"), 
                               selected = "s"),
                   # Select p.adjust.method
                   selectInput(ns("box_p_adjust"), "Select p.adjust.method", 
                               choices = p.adjust.methods, selected = "fdr")
                 ),
                 
                 mainPanel(
                   uiOutput(ns("boxplot_message")),  # Text message
                   div(
                     style = "border: 2px solid #2e6c40; padding: 0px; margin: 0;",
                   plotOutput(ns("boxplot"))),
                   div(style = "height: 300px; overflow-y: auto; overflow-x: auto;", 
                       tableOutput(ns("test_results_table"))  # Display the table
                   )
                 )
               )
      )
    ),
    
    # Add custom CSS to style the table background color
    tags$style(HTML("
      table {
        background-color: #f2f2f2;  /* Grey background color */
        border-collapse: collapse;  /* Ensure the table borders collapse into a single border */
        width: 100%;
      }
      table, th, td {
        border: 1px solid #ddd;  /* Light grey border */
      }
      th, td {
        padding: 8px;  /* Padding inside cells */
        text-align: left;
      }
      th {
        background-color: #d9d9d9;  /* Slightly darker grey for header */
        font-weight: bold;  /* Make header text bold */
      }
    "))
  )
}

# CDA Module Server Logic
cda_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive expression for filtering data based on selected variables for correlation heatmap
    filtered_corr_data <- reactive({
      req(input$corr_vars)
      Property_data[, input$corr_vars]
    })
    
    # Reactive expression for generating the UI for filtering the scatterplot variables by range
    output$scatter_range_ui <- renderUI({
      req(input$scatter_x, input$scatter_y)  # Ensure that x and y are selected
      
      # Get the min and max of the selected variables to set the range for the sliders
      x_min <- floor(min(Property_data[[input$scatter_x]], na.rm = TRUE))
      x_max <- ceiling(max(Property_data[[input$scatter_x]], na.rm = TRUE))
      y_min <- floor(min(Property_data[[input$scatter_y]], na.rm = TRUE))
      y_max <- ceiling(max(Property_data[[input$scatter_y]], na.rm = TRUE))
      
      # Return the UI components for the slider inputs
      tagList(
        sliderInput(session$ns("x_range"), "Filter X Variable Range", 
                    min = x_min, 
                    max = x_max, 
                    value = c(x_min, x_max),
                    step = 1),  # Step size of 1 for whole numbers
        
        sliderInput(session$ns("y_range"), "Filter Y Variable Range", 
                    min = y_min, 
                    max = y_max, 
                    value = c(y_min, y_max),
                    step = 1)  # Step size of 1 for whole numbers
      )
    })
    
    # Reactive expression for filtering scatterplot data based on selected ranges
    filtered_scatter_data <- reactive({
      req(input$scatter_x, input$scatter_y, input$x_range, input$y_range)
      
      # Filter the data based on selected x and y ranges
      Property_data %>%
        filter(between(.data[[input$scatter_x]], input$x_range[1], input$x_range[2]),
               between(.data[[input$scatter_y]], input$y_range[1], input$y_range[2]))
    })
    
    # Render the correlation heatmap using ggstatsplot::ggcorrmat
    output$correlation_heatmap <- renderPlot({
      req(input$corr_vars)
      corr_data <- Property_data[, input$corr_vars]
      
      ggstatsplot::ggcorrmat(
        data = corr_data,
        type = input$stat_approach,
        p.adjust.method = input$p_adjust,
        sig.level = input$sig_level
      )
    })
    
    # Render the scatterplot using ggscatterstats
    output$scatter_plot <- renderPlot({
      req(input$scatter_x, input$scatter_y)
      scatter_data <- filtered_scatter_data()
      
      # Convert column names to symbols using rlang::sym() and unquote with !!
      x_var <- sym(input$scatter_x)  # Convert to symbol
      y_var <- sym(input$scatter_y)  # Convert to symbol
      
      ggscatterstats(
        data = scatter_data,
        x = !!x_var,  # Unquote using !! to pass as symbol
        y = !!y_var,  # Unquote using !! to pass as symbol
        marginal = TRUE,
        type = input$stat_approach,
        p.adjust.method = input$p_adjust,
        sig.level = input$sig_level
      )
    })
    
    
    # Render boxplot
    # Boxplot logic
    useShinyjs()
    
    observe({
      runjs("$('#message').show();")  # Show the message as soon as the app starts
    })
    
    # Render the boxplot
    pairwise_comparisons_reactive <- reactiveVal(NULL)
    
    output$boxplot <- renderPlot({
      req(input$box_categorical, input$box_continuous)  # Ensure inputs are valid
      
      x_var <- sym(input$box_categorical)
      y_var <- sym(input$box_continuous)
      
      plot_result <- ggstatsplot::ggbetweenstats(
        data = Property_data,
        x = !!x_var,
        y = !!y_var,
        type = input$box_stat_approach,
        pairwise.comparisons = TRUE,
        pairwise.display = input$box_pairwise_display,
        p.adjust.method = input$box_p_adjust,
        messages = FALSE,
        output = "plot"
      )
      
      pairwise_comparisons <- extract_stats(plot_result)$pairwise_comparisons_data
      pairwise_comparisons <- pairwise_comparisons[, !names(pairwise_comparisons) %in% "expression"]
      pairwise_comparisons_df <- as.data.frame(pairwise_comparisons)
      
      pairwise_comparisons_reactive(pairwise_comparisons_df)
      
      runjs("$('#message').hide();")  # Hide the message when the plot is ready
      
      plot_result  # Return the plot to render
    })
    
    # Render the test results table
    output$test_results_table <- renderTable({
      req(input$box_categorical, input$box_continuous)
      
      num_groups <- length(unique(Property_data[[input$box_categorical]]))
      
      if (num_groups > 2) {
        pairwise_comparisons <- pairwise_comparisons_reactive()
        
        if (is.null(pairwise_comparisons) || nrow(pairwise_comparisons) == 0) {
          return(data.frame("No pairwise comparisons available."))
        } else {
          return(pairwise_comparisons)
        }
      } else {
        return(NULL)
      }
    })
  })
}

