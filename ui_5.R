library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(ggplot2)
library(ggcorrplot)
library(shinycssloaders)

# Assume  'game_data_all.csv' is in the working directory for simplicity
my_data <- read.csv('game_data_all.csv')
numeric_data <- my_data %>% 
  select(where(is.numeric))

ui <- dashboardPage(
  dashboardHeader(title = "Steam Gaming Hits!",
                  tags$li(class = "dropdown",
                          tags$a(href = "https://www.linkedin.com/in/sanved-bartakke-241858160/",
                                 icon("linkedin"), "LinkedIn", target = "_blank")),
                  tags$li(class = "dropdown",
                          tags$a(href = "https://github.com/Hash8e",
                                 icon("github"), "GitHub", target = "_blank")),
                  tags$li(class = "dropdown",
                          tags$a(href = "https://steamcommunity.com/profiles/76561198857023052/",
                                 icon("steam"), "Steam", target = "_blank"))
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dataset", tabName = "data", icon = icon("database")),
      menuItem("Visualisation", tabName = "visualisation", icon = icon("chart-line")),
      # Conditional inputs and correlation menu items have been commented for simplicity
      # Future implementation can uncomment and adjust as needed
      menuItem("Analysis", tabName = "Analysis", icon = icon("map")),
      menuItem("Reference", tabName = "References", icon = icon("info-circle"))
      
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "data",
              tabBox(width = 14,
                     tabPanel("About", icon = icon("address-card"),
                              fluidRow(
                                column(width = 8, tags$br(),
                                tags$p("This dataset offers a comprehensive overview of Steam games released between 2006 and 2023. It encompasses a wide range of details such as the publishers, technologies employed, tags, developers, player statistics, and ratings. Serving as a publicly accessible alternative to Steamdb data, it provides valuable resources for regression analyses and various other applications."))
                              )
                     ),
                     tabPanel("Dataset", icon = icon("address-card"), dataTableOutput("data_table")),
                     tabPanel("Structure", icon = icon("address-card"), verbatimTextOutput("structure")),
                     tabPanel("More", icon = icon("info-circle"), verbatimTextOutput("summary"))
              )),
      tabItem(tabName = "visualisation",
              fluidRow(
                box(title = "Top Games by:", status = "warning", solidHeader = TRUE,width = 12,
                    selectInput("var_select", "Select Variable:", choices = names(my_data), selected = "rating"),
                    plotlyOutput("var_plot") %>% withSpinner()
                )
                ),
              fluidRow(
                box(title = "Bottom Games by:", status = "warning", solidHeader = TRUE,width = 12,
                    selectInput("var_select4", "Select Variable:", choices = names(my_data), selected = "rating"),
                    plotlyOutput("var_plot2") %>% withSpinner()
                )
              )
              ),
      tabItem(tabName = "Analysis", 
              tabBox(id="t2", width=12, 
                     tabPanel("Review & Ratings", value="trends",
                              fluidRow(
                                selectInput("var_select1", "Select Variable 1:", choices = names(my_data), selected = "game"),
                                selectInput("var_select2", "Select Variable 2:", choices = names(numeric_data), selected = "rating"),
                              ),
                              plotlyOutput("bar") %>% withSpinner()
                     ),
                    tabPanel("Correlation Matrix", id="corr", plotlyOutput("cor") %>% withSpinner())
     
              )
      ),
      tabItem(tabName = "References",
              tabBox(width = 14,
                     tabPanel("About", icon = icon("address-card"),
                              fluidRow(
                                column(width = 12, tags$br(),
                                        tags$p("The following codes, channels and websites were refered in making of the Dashboard")),
                                        tags$p("        YouTube Channel Abhinav Agrawal :https://www.youtube.com/watch?v=tlOBVZx8Hy0&t=1553s"),
                                        tags$p("        Github: https://github.com/aagarw30/R-Shiny-Dashboards/tree/main/USArrestDashboard"),
                                        tags$p("        Data Source: https://www.kaggle.com/datasets/whigmalwhim/steam-releases")
                                 )
                     )
              ))
    )
  )
  
)

server <- function(input, output) {
  
  # For Structure output
  output$structure <- renderPrint({
    my_data %>% 
      str()
  })
  # For Summary Output
  output$summary <- renderPrint({
    my_data %>% 
      summary()
  })
  # Data table output
  output$data_table <- renderDataTable({
    my_data
  })
  
  # Variable selection for visualization
  output$var_plot <- renderPlotly({
    req(input$var_select)  # Ensure that input$var_select is available
    
    filtered_data <- my_data %>%
      arrange(desc(!!sym(input$var_select))) %>%
      head(20)
    
    plot_ly(filtered_data, x = ~game, y = ~get(input$var_select), type = 'bar',
            marker = list(color = 'rgba(50, 171, 96, 0.7)')) %>%
      layout(title = paste("Top 20 Games by", input$var_select))
  })
  
  output$bar <- renderPlotly({
    req(input$var_select1, input$var_select2)  # Make sure the input values are available
    
    # Arrange the data in descending order based on input$var_select1 and select the top 20
    sorted_data <- my_data %>%
      arrange(desc(!!sym(input$var_select2))) %>%
      head(20)
    
    plot_ly(data = sorted_data) %>%
      add_bars(x = ~get(input$var_select1), y = ~get(input$var_select2)) %>%
      layout(title = paste(input$var_select1, "vs", input$var_select2),
             xaxis = list(title = input$var_select1),
             yaxis = list(title = paste(input$var_select2, "Variable 2")))
  })
  
  output$var_plot2 <- renderPlotly({
    req(input$var_select4)  # Ensure that input$var_select is available
    
    filtered_data <- my_data %>%
      arrange(!!sym(input$var_select4)) %>%
      head(20)
    
    plot_ly(filtered_data, x = ~game, y = ~get(input$var_select4), type = 'bar',
            marker = list(color = 'rgba(50, 171, 96, 0.7)')) %>%
      layout(title = paste("Top 20 Games in Year", input$var_select4))
  })
  
  output$cor <- renderPlotly({
    numeric_data <- my_data %>% 
      select(where(is.numeric))
    
    # Display the structure of the new dataset to confirm
    str(numeric_data)
    numeric_data_clean <- numeric_data %>%
      filter_all(all_vars(!is.infinite(.) & !is.na(.) & !is.nan(.)))
    
    
    # Compute a correlation matrix
    corr <- round(cor( numeric_data_clean), 1)
    
    # Compute a matrix of correlation p-values
    p.mat <- cor_pmat( numeric_data_clean)
    
    corr.plot <- ggcorrplot(
      corr, 
      hc.order = TRUE, 
      lab= TRUE,
      outline.col = "white",
      p.mat = p.mat
    )
    
    ggplotly(corr.plot)
    
  })
}

shinyApp(ui, server)
