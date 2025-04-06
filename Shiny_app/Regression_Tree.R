#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

pacman::p_load(shinyWidgets, tidyverse, SmartEDA, rpart, rpart.plot, randomForest, visNetwork, sparkline, caret, ranger, patchwork, doParallel, e1071, dplyr, ggplot2)

# Load and prepare data (already global)
Property_data <- read.csv("data_pred/Property_Price_and_Green_Index.csv")

# Data preparation
Property_data <- Property_data %>%
  select(-Longitude, -Latitude, -ID) %>%
  mutate(Summer = ifelse(Spring == 0 & Fall == 0 & Winter == 0, 1, 0),
         `Dist..CBD` = log(`Dist..CBD` + 1)) %>%
  select(-all_of(c("Dist..Green", "High.School")))


# Get independent variables (global scope, as in your original)
independent_vars <- names(Property_data)[names(Property_data) != "Property.Prices"]

# Define UI module
regression_tree_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        pickerInput(
          inputId = ns("predictors"),
          label = "Select Independent Variables",
          choices = independent_vars,
          selected = independent_vars,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `live-search` = TRUE,
            title = "Select variables"
          )
        ),
        numericInput(ns("seed"), "Set seed (0-9999)", value = 9999, min = 0, max = 9999),
        sliderInput(ns("train_ratio"), "Train-Test Partition Ratio", 
                    min = 0.5, max = 0.95, value = 0.8, step = 0.05),
        sliderInput(ns("minsplit"), "minsplit", 
                    min = 2, max = 20, value = 5, step = 1),
        sliderInput(ns("cp"), "complexity_parameter", 
                    min = 0.001, max = 0.5, value = 0.001, step = 0.001),
        sliderInput(ns("maxdepth"), "max_depth", 
                    min = 2, max = 20, value = 10, step = 1),
        actionButton(ns("build"), "Build Model")
      ),
      mainPanel(
        width = 9,
        fluidRow(
          column(
            width = 6,
            h3("Regression Tree"),
            visNetworkOutput(ns("tree_plot"), height = "300px")
          ),
          column(
            width = 6,
            tabsetPanel(
              tabPanel("Actual vs Predicted",
                       plotOutput(ns("actual_vs_predicted"), height = "300px")),
              tabPanel("Actual vs Predicted Boxplot",
                       plotOutput(ns("actual_vs_predicted_boxplot"), height = "300px")),
              tabPanel("Residuals vs Predicted",
                       plotOutput(ns("Residuals_vs_Predicted"), height = "300px")),
              tabPanel("Variable Importance",
                       plotOutput(ns("feature_importance"), height = "300px"))
            )
          )
        ),
        fluidRow(
          style = "display: flex; align-items: flex-end;",
          column(
            width = 3,
            wellPanel(
              div(style = "text-align: left; margin-bottom: 10px;",
                  strong("Test MSE:"),
                  h4(textOutput(ns("test_mse")), style = "margin-top: 5px;")),
              div(style = "text-align: left;",
                  strong("Test R-squared:"),
                  h4(textOutput(ns("test_r2")), style = "margin-top: 5px;")),
              div(style = "text-align: left; margin-bottom: 10px;",
                  strong("Train MSE:"),
                  h4(textOutput(ns("train_mse")), style = "margin-top: 5px;")),
              div(style = "text-align: left;",
                  strong("Train R-squared:"),
                  h4(textOutput(ns("train_r2")), style = "margin-top: 5px;"))
            )
          ),
          column(
            width = 9,
            h3("Cross-validation results table"),
            div(style = "overflow-x:auto;",
                DT::dataTableOutput(ns("cp_table")))
          )
        )
      )
    )
  )
}

# Define server module
regression_tree_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive data preparation
    model_data <- reactive({
      set.seed(input$seed)
      
      # Data partitioning
      trainIndex <- createDataPartition(Property_data$Property.Prices, 
                                        p = input$train_ratio, list = FALSE)
      df_train <- Property_data[trainIndex, ]
      df_test <- Property_data[-trainIndex, ]
      
      # Dynamically create the formula based on selected predictors
      selected_predictors <- if (is.null(input$predictors) || length(input$predictors) == 0) {
        independent_vars
      } else {
        input$predictors
      }
      
      formula_str <- paste("Property.Prices ~", paste(selected_predictors, collapse = " + "))
      model_formula <- as.formula(formula_str)
      
      # Train regression tree model
      fit_tree <- rpart(model_formula, 
                        data = df_train, 
                        method = "anova", 
                        control = rpart.control(minsplit = input$minsplit, 
                                                cp = input$cp, 
                                                maxdepth = input$maxdepth))
      
      # Prune tree
      bestcp <- fit_tree$cptable[which.min(fit_tree$cptable[,"xerror"]),"CP"]
      pruned_tree <- prune(fit_tree, cp = bestcp)
      
      # Calculate predictions
      train_pred <- predict(pruned_tree, newdata = df_train)
      test_pred <- predict(pruned_tree, newdata = df_test)
      
      # Calculate performance metrics
      train_mse <- mean((train_pred - df_train$Property.Prices)^2)
      test_mse <- mean((test_pred - df_test$Property.Prices)^2)
      train_r2 <- 1 - sum((train_pred - df_train$Property.Prices)^2) / 
        sum((df_train$Property.Prices - mean(df_train$Property.Prices))^2)
      test_r2 <- 1 - sum((test_pred - df_test$Property.Prices)^2) / 
        sum((df_test$Property.Prices - mean(df_test$Property.Prices))^2)
      
      # Add predictions and residuals to test data
      df_test$Predicted <- test_pred
      df_test$Residuals <- df_test$Predicted - df_test$Property.Prices  # Note: Original had this reversed
      
      list(pruned_tree = pruned_tree, df_test = df_test, 
           metrics = list(train_mse = train_mse, test_mse = test_mse, 
                          train_r2 = train_r2, test_r2 = test_r2),
           cp_table = fit_tree$cptable)
    })
    
    # Trigger rendering when "Build Model" is clicked
    observeEvent(input$build, {
      data <- model_data()
      
      # Decision tree plot
      output$tree_plot <- renderVisNetwork({
        visTree(data$pruned_tree, 
                edgesFontSize = 14, 
                nodesFontSize = 16, 
                width = "100%",
                height = "500px",
                legend = FALSE) %>%
          visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
      })
      
      # Actual vs Predicted scatter plot
      output$actual_vs_predicted <- renderPlot({
        ggplot(data$df_test, aes(x = Property.Prices, y = Predicted)) +
          geom_point() +
          geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
          labs(x = "Actual Property Prices",
               y = "Predicted Property Prices",
               title = paste0("R-squared (test): ", round(data$metrics$test_r2, 2))) +
          theme(axis.text = element_text(size = 5),
                axis.title = element_text(size = 8),
                plot.title = element_text(size = 8))
      })
      
      # Actual vs Predicted boxplot
      output$actual_vs_predicted_boxplot <- renderPlot({
        plot_data <- data.frame(
          Value = c(data$df_test$Property.Prices, data$df_test$Predicted),
          Type = rep(c("Actual", "Predicted"), each = nrow(data$df_test))
        )
        ggplot(plot_data, aes(x = Type, y = Value, fill = Type)) +
          geom_boxplot() +
          labs(x = NULL, y = "Property Prices") +
          theme_minimal() +
          theme(axis.text = element_text(size = 8),
                axis.title = element_text(size = 8))
      })
      
      # Residuals vs Predicted plot
      output$Residuals_vs_Predicted <- renderPlot({
        residuals <- data$df_test$Predicted - data$df_test$Property.Prices
        plot_data <- data.frame(Predicted = data$df_test$Predicted, Residuals = residuals)
        ggplot(plot_data, aes(x = Predicted, y = Residuals)) +
          geom_point() +
          geom_hline(yintercept = 0, color = "red") +
          labs(x = "Predicted Property Prices", y = "Residuals") +
          theme_minimal()
      })
      
      # Feature importance plot
      output$feature_importance <- renderPlot({
        vi <- data$pruned_tree$variable.importance
        if (!is.null(vi)) {
          vi_df <- data.frame(Variable = names(vi), Importance = as.numeric(vi))
          vi_df <- vi_df[order(-vi_df$Importance), ]
          ggplot(vi_df, aes(x = reorder(Variable, Importance), y = Importance)) +
            geom_bar(stat = "identity", fill = "lightblue") +
            coord_flip() +
            labs(x = "Variable", y = "Importance") +
            theme_minimal()
        } else {
          ggplot() + geom_text(aes(x = 1, y = 1, label = "No variable importance available"), size = 6) +
            theme_void()
        }
      })
      
      # Performance metrics
      output$train_mse <- renderText({ round(data$metrics$train_mse, 2) })
      output$test_mse <- renderText({ round(data$metrics$test_mse, 2) })
      output$train_r2 <- renderText({ round(data$metrics$train_r2, 4) })
      output$test_r2 <- renderText({ round(data$metrics$test_r2, 4) })
      
      # Cross-validation results table
      output$cp_table <- DT::renderDataTable({
        cp_df <- as.data.frame(data$cp_table)
        colnames(cp_df) <- c("CP", "nsplit", "rel_error", "xerror", "xstd")
        cp_df <- head(cp_df, 20)
        DT::datatable(cp_df, options = list(pageLength = 5))
      })
    })
  })
}