
#Please put parallelPlot as the last loaded package, otherwise it wont work!
pacman::p_load(shiny, shinydashboard, ggdist, tidyverse, shinyjs, 
               ggstatsplot, ggside, rlang, shinyWidgets, caret, ranger, ggplot2, parallelPlot,fresh)

source("EDA_module.R")
source("CDA_module.R")
source("Regression_Tree.R")
source("Random_Forest.R")

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#A8D08D",  # Soft green for highlights
    green = "#2E6C40"       # Darker green for active states
  ),
  adminlte_sidebar(
    dark_bg = "#DFFFD6",       # Light green sidebar background
    dark_hover_bg = "#C1F0B9", # Hover state
    dark_color = "#333333",    # Dark gray text for readability
    dark_hover_color = "#FFFFFF" # White text on hover
  ),
  adminlte_global(
    content_bg = "#F4F7F6",    # Subtle off-white content background
    box_bg = "#FFFFFF",        # White boxes for contrast
    info_box_bg = "#E3F6E5"    # Light green for info boxes
  )
)

# Custom CSS for styling with default font
custom_css <- "
  /* Sidebar styling */
  .skin-green .main-sidebar {
    background-color: #E3F6E5 !important;
    border-right: 1px solid #C1F0B9;
  }
  .skin-green .sidebar-menu > li.active > a {
    background-color: #2E6C40 !important;
    color: #FFFFFF !important;
    border-left: 4px solid #A8D08D;
  }
  .skin-green .sidebar a {
    color: #333333 !important;
    font-weight: 500;
  }
  .skin-green .sidebar a:hover {
    background-color: #3B8C53 !important;
    color: #FFFFFF !important;
  }
  .skin-green .treeview-menu {
    background-color: #F0FFF0 !important;
  }
  .skin-green .treeview-menu > li > a {
    color: #333333 !important;
  }
  .skin-green .treeview-menu > li > a:hover {
    background-color: #C1F0B9 !important;
    color: #FFFFFF !important;
  }

  /* Header styling */
  .skin-green .main-header {
    background-color: #2E6C40 !important;
    border-bottom: 1px solid #A8D08D;
  }
  .skin-green .main-header .logo {
    background-color: #2E6C40 !important;
    color: #FFFFFF !important;
    font-weight: bold;
    font-size: 20px;
    padding: 0 15px;
  }
  .skin-green .main-header .navbar {
    background-color: #FFFFFF !important;
  }
  .skin-green .main-header .navbar .sidebar-toggle {
    color: #3B8C53 !important;
  }
  .skin-green .main-header .navbar .sidebar-toggle:hover {
    background-color: #F4F7F6 !important;
  }

  /* Content area */
  .skin-green .content-wrapper {
    background-color: #F4F7F6 !important;
  }
  .skin-green .box {
    border-radius: 8px;
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    border: 1px solid #E3F6E5;
  }
  .skin-green .box-header {
    background-color: #E3F6E5;
    border-bottom: 1px solid #C1F0B9;
  }
  .skin-green .box-title {
    color: #2E6C40;
    font-weight: 600;
    font-size: 18px;
  }

  /* Typography */
  h3 {
    color: #2E6C40;
    font-size: 24px;
  }
  h4 {
    color: #2E6C40;
    font-size: 18px;
  }
  .content {
    padding: 20px;
  }

  /* Buttons */
  .btn {
    border-radius: 4px;
    background-color: #A8D08D;
    color: #FFFFFF;
    border: none;
    padding: 8px 16px;
  }
  .btn:hover {
    background-color: #3B8C53;
  }
"

# UI
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(
    title = span("Property Price Analysis Dashboard", style = "font-size: 20px; font-weight: bold;"),
    titleWidth = 300
  ),
  dashboardSidebar(
    width = 250,
    use_theme(mytheme),
    sidebarMenu(
      menuItem("EDA", tabName = "eda", icon = icon("chart-bar", class = "fa-lg")),
      menuItem("CDA", tabName = "cda", icon = icon("cogs", class = "fa-lg")),
      menuItem("Predictive Modeling", tabName = "predictive", icon = icon("code", class = "fa-lg"), startExpanded = TRUE,
               menuSubItem("Regression Tree", tabName = "regression_tree", icon = icon("tree", class = "fa-lg")),
               menuSubItem("Random Forest", tabName = "random_forest", icon = icon("tree", class = "fa-lg"))
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML(custom_css))
    ),
    tabItems(
      tabItem(tabName = "eda",
              fluidRow(
                box(
                  title = "Exploratory Data Analysis",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  eda_ui("eda_module")
                )
              )
      ),
      tabItem(tabName = "cda",
              fluidRow(
                box(
                  title = "Confirmatory Data Analysis",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  cda_ui("cda_module")
                )
              )
      ),
      tabItem(tabName = "regression_tree",
              fluidRow(
                box(
                  title = "Regression Tree Model",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  regression_tree_ui("regression_tree_module")
                )
              )
      ),
      tabItem(tabName = "random_forest",
              fluidRow(
                box(
                  title = "Random Forest Model",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  random_forest_ui("random_forest_module")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  eda_server("eda_module")
  cda_server("cda_module")
  regression_tree_server("regression_tree_module")
  random_forest_server("random_forest_module")
}

# Run the app
shinyApp(ui, server)