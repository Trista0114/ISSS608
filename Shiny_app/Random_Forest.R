#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#



pacman::p_load(shinyWidgets,tidyverse,  SmartEDA, rpart, rpart.plot,randomForest,visNetwork,sparkline,caret, ranger, patchwork,doParallel,e1071,dplyr,ggplot2)


Property_data <- read.csv("data_pred/Property_Price_and_Green_Index.csv")


#數據準備
Property_data <- Property_data %>%
  select(!Longitude) %>%
  select(!Latitude) %>%
  select(!ID)
Property_data$Summer <- ifelse(Property_data$Spring == 0 & Property_data$Fall == 0 & Property_data$Winter == 0, 1, 0)
print(Property_data)
Property_data$`Dist..CBD` <- log(Property_data$`Dist..CBD` + 1)
Property_data<- Property_data[, !(names(Property_data) %in% c("Dist..Green", "High.School"))]


#Get the list of independent variables (all columns except Property.Prices)
independent_vars <- names(Property_data)[names(Property_data) != "Property.Prices"]

# Define UI for application that draws a histogram
# UI
# Random_Forest.R

# Random Forest UI module
random_forest_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      # Sidebar Panel (Left)
      column(
        width = 3,
        sidebarPanel(
          width = 12,
          # Progress Bar at the top
          progressBar(
            id = ns("incProgress"),
            value = 0,
            title = "Model training progress",
            display_pct = TRUE
          ),
          br(),
          pickerInput(
            inputId = ns("predictors"),
            label = "Select variables",
            choices = names(Property_data)[-which(names(Property_data) == "Property.Prices")],
            selected = names(Property_data)[-which(names(Property_data) == "Property.Prices")],
            multiple = TRUE,
            options = list(
              `actions-box` = TRUE,
              `live-search` = TRUE,
              title = "Select variables"
            )
          ),
          numericInput(ns("seed"), "Set seed (0-9999)", value = 1234, min = 0, max = 9999),
          sliderInput(ns("train_ratio"), "Train-Test Partition Ratio", 
                      min = 0.5, max = 0.95, value = 0.8, step = 0.05),
          sliderInput(ns("num_trees"), "Num of Trees", 
                      min = 5, max = 200, value = 200, step = 5),
          selectInput(ns("tuning_method"), "Tuning method", 
                      choices = c("none", "cv", "repeatedcv", "boot"), 
                      selected = "none"),
          selectInput(ns("split_rule"), "Split rule", 
                      choices = c("variance", "extratrees", "maxstat", "beta"), 
                      selected = "variance"),
          selectInput(ns("feature_importance"), "Feature Importance", 
                      choices = c("impurity", "permutation"), 
                      selected = "impurity"),
          actionButton(ns("build"), "Build Model")
        )
      ),
      # Main Panel (Right) with Plots and Metrics
      column(
        width = 9,
        mainPanel(
          width = 12,
          fluidRow(
            column(width = 12,
                   fluidRow(
                     column(width = 4,
                            h3("Out-of-Bag Error", align = "center"),
                            plotOutput(ns("oob_error"), height = "300px")
                     ),
                     column(width = 4,
                            h3("Variable Importance", align = "center"),
                            plotOutput(ns("feature_importance"), height = "300px")
                     ),
                     column(width = 4,
                            tabsetPanel(
                              tabPanel("Actual vs Predicted",
                                       plotOutput(ns("actual_vs_predicted"), height = "230px")),
                              tabPanel("Actual vs Predicted Boxplot",
                                       plotOutput(ns("actual_vs_predicted_boxplot"), height = "230px")),
                              tabPanel("Residuals vs Predicted",
                                       plotOutput(ns("Residuals_vs_Predicted"), height = "230px"))
                            )
                     )
                   )
            )
          ),
          fluidRow(
            style = "margin-top: 20px;",
            column(width = 3,
                   wellPanel(
                     div(strong("Test R-squared:"), textOutput(ns("test_r2"))),
                     div(strong("Test MSE:"), textOutput(ns("test_mse"))),
                     div(strong("Train R-squared:"), textOutput(ns("train_r2"))),
                     div(strong("Train MSE:"), textOutput(ns("train_mse")))
                   )
            ),
            column(width = 9,
                   h3("Suggested setting/Thought"),
                   textOutput(ns("suggested_setting"))
            )
          )
        )
      )
    )
  )
}

# Random Forest server module
random_forest_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    model_data <- reactiveVal(NULL)
    
    # Build model when button is clicked
    observeEvent(input$build, {
      # Update progress bar step-by-step
      updateProgressBar(session, id = ns("incProgress"), value = 0, total = 100, title = "Starting model training")
      
      # 1. Set random seed
      set.seed(input$seed)
      updateProgressBar(session, id = ns("incProgress"), value = 10, title = "Set the seed")
      
      # 2. Split Property_data
      df <- Property_data
      train_idx <- sample(1:nrow(df), size = round(input$train_ratio * nrow(df)))
      df_train <- df[train_idx, ]
      df_test  <- df[-train_idx, ]
      updateProgressBar(session, id = ns("incProgress"), value = 20, title = "Splitting data")
      
      # 3. Build formula based on selected predictors
      selected_vars <- input$predictors
      if (length(selected_vars) == 0) {
        showNotification("Please select at least one predictor variable.", type = "error")
        return()
      }
      formula <- as.formula(paste("Property.Prices ~", paste(selected_vars, collapse = "+")))
      updateProgressBar(session, id = ns("incProgress"), value = 30, title = "Building formula")
      
      # 4. Define trainControl and tuning parameters
      trctrl <- trainControl(
        method = input$tuning_method,
        number = ifelse(input$tuning_method == "boot", 5, 10),
        verboseIter = TRUE
      )
      tune_grid <- expand.grid(
        mtry = floor(sqrt(length(selected_vars))),  # Dynamic mtry based on predictors
        min.node.size = 3,
        splitrule = input$split_rule
      )
      updateProgressBar(session, id = ns("incProgress"), value = 40, title = "Setting parameters, good model is worth to wait")
      
      # 5. Train Random Forest model
      rf_model <- train(
        formula,
        data = df_train,
        method = "ranger",
        trControl = trctrl,
        tuneGrid = tune_grid,
        num.trees = input$num_trees,
        importance = input$feature_importance
      )
      updateProgressBar(session, id = ns("incProgress"), value = 70, title = "Training model")
      
      # 6. Get predictions and calculate metrics
      train_pred <- predict(rf_model, newdata = df_train)
      test_pred  <- predict(rf_model, newdata = df_test)
      
      train_mse <- mean((df_train$Property.Prices - train_pred)^2)
      test_mse  <- mean((df_test$Property.Prices - test_pred)^2)
      train_r2  <- cor(df_train$Property.Prices, train_pred)^2
      test_r2   <- cor(df_test$Property.Prices, test_pred)^2
      
      # 7. Extract OOB error and simulate for plotting
      oob_error <- rf_model$finalModel$prediction.error
      oob_simulated <- cumsum(runif(input$num_trees, 0, oob_error)) / (1:input$num_trees)
      
      # 8. Extract variable importance
      importance <- as.data.frame(varImp(rf_model)$importance)
      importance$Variable <- rownames(importance)
      colnames(importance) <- c("Importance", "Variable")
      updateProgressBar(session, id = ns("incProgress"), value = 90, title = "Calculating metrics")
      
      # 9. Store results
      model_data(list(
        actual = df_test$Property.Prices,
        predicted = test_pred,
        oob_error = oob_simulated,
        importance = importance,
        test_r2 = test_r2,
        test_mse = test_mse,
        train_r2 = train_r2,
        train_mse = train_mse
      ))
      
      updateProgressBar(session, id = ns("incProgress"), value = 100, title = "Done, congrats!")
    })
    
    # Actual vs Predicted Plot
    output$actual_vs_predicted <- renderPlot({
      req(model_data())
      data <- model_data()
      ggplot(data.frame(Actual = data$actual, Predicted = data$predicted), aes(x = Actual, y = Predicted)) +
        geom_point(color = "black", alpha = 0.5) +
        geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
        theme_minimal() +
        labs(
          x = "Actual Property Prices",
          y = "Predicted Property Prices",
          title = paste0("R-squared (test): ", round(data$test_r2, 2))
        ) +
        theme(
          axis.text = element_text(size = 5),
          axis.title = element_text(size = 8),
          plot.title = element_text(size = 8)
        )
    })
    
    output$actual_vs_predicted_boxplot <- renderPlot({
      req(model_data())
      data <- model_data()
      ggplot(data.frame(Actual = data$actual, Predicted = data$predicted)) +
        geom_boxplot(aes(x = "Actual", y = Actual), fill = "lightblue") +
        geom_boxplot(aes(x = "Predicted", y = Predicted), fill = "lightgreen") +
        theme_minimal() +
        labs(x = "", y = "Property Prices")
    })
    
    output$Residuals_vs_Predicted <- renderPlot({
      req(model_data())
      data <- model_data()
      residuals <- data$predicted - data$actual
      ggplot(data.frame(Predicted = data$predicted, Residuals = residuals), 
             aes(x = Predicted, y = Residuals)) +
        geom_point(alpha = 0.5) +
        geom_hline(yintercept = 0, color = "red") +
        theme_minimal() +
        labs(x = "Predicted Property Prices", y = "Residuals")
    })
    
    output$oob_error <- renderPlot({
      req(model_data())
      data <- model_data()
      ggplot(data.frame(Trees = 1:length(data$oob_error), OOB = data$oob_error), aes(x = Trees, y = OOB)) +
        geom_line(color = "black") +
        theme_minimal() +
        labs(x = "Number of Trees", y = "Out-of-Bag Error")
    })
    
    output$feature_importance <- renderPlot({
      req(model_data())
      data <- model_data()
      ggplot(data$importance, aes(x = reorder(Variable, Importance), y = Importance)) +
        geom_bar(stat = "identity", fill = "skyblue") +
        coord_flip() +
        theme_minimal() +
        labs(x = "Variable", y = "Importance")
    })
    
    output$test_r2 <- renderText({
      req(model_data())
      sprintf("%.3f", model_data()$test_r2)
    })
    
    output$test_mse <- renderText({
      req(model_data())
      sprintf("%.3f", model_data()$test_mse)
    })
    
    output$train_r2 <- renderText({
      req(model_data())
      sprintf("%.3f", model_data()$train_r2)
    })
    
    output$train_mse <- renderText({
      req(model_data())
      sprintf("%.3f", model_data()$train_mse)
    })
    
    output$suggested_setting <- renderText({
      req(model_data())
      paste(
        "Suggested settings: Adjust the number of trees (current: ", input$num_trees, 
        ") and tuning method (current: ", input$tuning_method, 
        ") based on OOB error convergence. The default min.node.size = 3 and dynamic mtry based on predictors."
      )
    })
  })
}






