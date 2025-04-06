# Required Libraries
library(shiny)
library(ggdist)
library(tidyverse)
library(parallelPlot)

Property_data <- read_csv("data/Property_data_cleaned.csv")

# Data Wrangling
Property_data$Season <- factor(Property_data$Season, 
                               levels = c("Spring", "Summer", "Fall", "Winter"), ordered = TRUE)
Property_data$BusStop_binned <- factor(Property_data$BusStop_binned, 
                                       levels = c("0-5", "6-10", "11-20", "21-30", ">30"), ordered = TRUE)
Property_data$Floor_binned <- factor(Property_data$Floor_binned, 
                                     levels = c("Low", "Middle", "Middle-high", "High", "Top"), ordered = TRUE)

colnames(Property_data) <- gsub(" ", "_", colnames(Property_data))

# UI for EDA Module
eda_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    tabsetPanel(
      # Boxplot & Raincloud Tab
      tabPanel("Boxplot with Raincloud",
               sidebarLayout(
                 sidebarPanel(
                   selectInput(ns("continuous_var"), "Select a Continuous Variable:",
                               choices = names(Property_data)[sapply(Property_data, is.numeric) & !names(Property_data) %in% c("Longitude", "Latitude")], 
                               selected = "Property Prices"),
                   selectInput(ns("categorical_var"), "Select a Categorical Variable (Optional):",
                               choices = c("None", names(Property_data)[sapply(Property_data, function(x) is.factor(x) || is.character(x))]), 
                               selected = "None"),
                   uiOutput(ns("range_filter_ui")),
                   uiOutput(ns("summary_table_ui"))
                 ),
                 mainPanel(
                   uiOutput(ns("summary_panels_ui")),
                   div(
                     style = "border: 2px solid #2e6c40; padding: 0px; margin: 0;",
                     plotOutput(ns("box_raincloud_plot"), height = "500px")
                   )
                 )
               )
      ),
      # Parallel Coordinates Tab
      tabPanel("Parallel Coordinates",
               sidebarLayout(
                 sidebarPanel(
                   div(style = "height: 100px; overflow-y: scroll;",   
                       checkboxGroupInput(ns("parallel_vars"), "Select Continuous Variables", 
                                          choices = names(Property_data)[sapply(Property_data, is.numeric) & !names(Property_data) %in% c("Longitude", "Latitude")],
                                          selected = NULL)),
                   actionButton(ns("generate_plot"), "Generate Plot")
                 ),
                 mainPanel(
                   div(
                     style = "border: 2px solid #2e6c40; padding: 0px; margin: 0;",
                   parallelPlotOutput(ns("parallel_plot"), width = "100%", height = "500px")  # Use `ns` here
                   )
                 )
               )
      )
    )
  )
}

# Server logic for EDA Module
eda_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Dynamic slider input for filtering the selected continuous variable
    output$range_filter_ui <- renderUI({
      req(input$continuous_var)
      sliderInput(session$ns("range_filter"), "Filter Continuous Variable Range:",
                  min = floor(min(Property_data[[input$continuous_var]], na.rm = TRUE)),
                  max = ceiling(max(Property_data[[input$continuous_var]], na.rm = TRUE)),
                  value = round(range(Property_data[[input$continuous_var]], na.rm = TRUE)),
                  step = 1)
    })
    
    # Filtered data based on selected continuous variable and slider range
    filtered_data <- reactive({
      req(input$continuous_var, input$range_filter)
      Property_data %>%
        filter(between(.data[[input$continuous_var]], input$range_filter[1], input$range_filter[2]))
    })
    
    # Boxplot & Raincloud plot
    output$box_raincloud_plot <- renderPlot({
      data <- filtered_data()
      p <- ggplot(data, aes_string( x = if (input$categorical_var == "None") NULL else input$categorical_var,
                                    y = input$continuous_var,
                                    fill = if (input$categorical_var == "None") NULL else input$categorical_var)) +
        stat_halfeye(adjust = 0.5, justification = -0.1, .width = 0, point_colour = NA, position = position_dodge(0.1)) +
        geom_boxplot(width = 0.2, position = position_dodge(0.3), outlier.shape = NA) +
        labs(x = ifelse(input$categorical_var == "None", "Density", input$categorical_var),
             y = input$continuous_var) +
        coord_flip() + scale_fill_brewer(palette="Pastel1")
      
      if (input$categorical_var != "None") {
        p <- p + stat_summary(fun = mean, geom = "point", shape = 20, size = 2, color = "red4")
      }
      
      print(p)
    })
    
    # Summary table in sidebar if categorical variable is selected
    output$summary_table_ui <- renderUI({
      if (input$categorical_var != "None") {
        tableOutput(session$ns("summary_table"))
      } else {
        NULL
      }
    })
    
    output$summary_table <- renderTable({
      data <- filtered_data()
      
      if (input$categorical_var != "None") {
        data %>%
          group_by(.data[[input$categorical_var]]) %>%
          summarise(Mean = mean(.data[[input$continuous_var]], na.rm = TRUE),
                    Median = median(.data[[input$continuous_var]], na.rm = TRUE),
                    Min = min(.data[[input$continuous_var]], na.rm = TRUE),
                    Max = max(.data[[input$continuous_var]], na.rm = TRUE),
                    SD = sd(.data[[input$continuous_var]], na.rm = TRUE))
      } else {
        NULL
      }
    })
    
    # Summary panels for continuous variable (without categorical variable)
    output$summary_panels_ui <- renderUI({
      if (input$categorical_var == "None") {
        data <- filtered_data()
        fluidRow(
          column(2, wellPanel(
            style = "background-color: #F0E68C; color: black; padding: 1px; text-align: center;",  
            strong("Mean"), textOutput(session$ns("mean_value"))
          )),
          column(2, wellPanel(
            style = "background-color: #98FB98; color: black; padding: 1px; text-align: center;",  
            strong("Median"), textOutput(session$ns("median_value"))
          )),
          column(2, wellPanel(
            style = "background-color: #ADD8E6; color: black; padding: 1px; text-align: center;",  
            strong("Min"), textOutput(session$ns("min_value"))
          )),
          column(2, wellPanel(
            style = "background-color: #FFB6C1; color: black; padding: 1px; text-align: center;",  
            strong("Max"), textOutput(session$ns("max_value"))
          )),
          column(2, wellPanel(
            style = "background-color: #FFD700; color: black; padding: 1px; text-align: center;",  
            strong("SD"), textOutput(session$ns("sd_value"))
          ))
        )
      } else {
        NULL
      }
    })
    
    # Individual summary statistics for panels
    output$mean_value <- renderText({
      round(mean(filtered_data()[[input$continuous_var]], na.rm = TRUE), 1)
    })
    
    output$median_value <- renderText({
      round(median(filtered_data()[[input$continuous_var]], na.rm = TRUE), 1)
    })
    
    output$min_value <- renderText({
      round(min(filtered_data()[[input$continuous_var]], na.rm = TRUE), 1)
    })
    
    output$max_value <- renderText({
      round(max(filtered_data()[[input$continuous_var]], na.rm = TRUE), 1)
    })
    
    output$sd_value <- renderText({
      round(sd(filtered_data()[[input$continuous_var]], na.rm = TRUE), 1)
    })
    
    # Parallel coordinate plot (Tab 2)
    # Reactive value to track the selection of continuous variables
    observeEvent(input$generate_plot, {
      # Show loading message while generating the plot
      withProgress(message = "Generating Parallel Coordinates Plot. Please wait...", value = 0, {
        
        # Ensure that the user selects between 3 and 5 continuous variables
        if(length(input$parallel_vars) >= 3 && length(input$parallel_vars) <= 5) {
          
          # Subset the data based on selected continuous variables
          cont_df <- Property_data[, input$parallel_vars]
          
          # Normalize the data
          df_normalized <- apply(cont_df, 2, function(x) (x - min(x)) / (max(x) - min(x)))
          df_normalized <- as.data.frame(df_normalized)
          
          # Set the histogram visibility for each variable
          histoVisibility <- rep(TRUE, ncol(df_normalized))
          
          # Simulate plot generation time with setProgress
          for (i in 1:5) {
            Sys.sleep(0.5)  # Simulate time delay
            setProgress(value = i / 5)  # Update progress
          }
          
          # Render the parallel plot using renderParallelPlot
          output$parallel_plot <- renderParallelPlot({
            parallelPlot(df_normalized,
                         rotateTitle = TRUE,
                         histoVisibility = histoVisibility)
          })
          
        } else {
          # Show a modal if the user selects less than 3 or more than 5 variables
          showModal(modalDialog(
            title = "Error",
            "Please select between 3 and 5 continuous variables.",
            easyClose = TRUE,
            footer = NULL
          ))
        }
      })
    })
    
  })
}
