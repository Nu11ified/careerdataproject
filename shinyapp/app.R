library(shiny)
library(tidyverse)
library(plotly)
library(shinythemes)
library(DT)

game_data <- read_csv("../Data/Game_Logs_Defensive_Lineman.csv", show_col_types = FALSE)

if ("Sacks" %in% colnames(game_data)) {
  game_data$Sacks <- as.numeric(as.character(game_data$Sacks))
  game_data$Sacks[is.na(game_data$Sacks)] <- 0
}
if ("Total Tackles" %in% colnames(game_data)) {
  game_data$`Total Tackles` <- as.numeric(as.character(game_data$`Total Tackles`))
  game_data$`Total Tackles`[is.na(game_data$`Total Tackles`)] <- 0
}
if ("Forced Fumbles" %in% colnames(game_data)) {
  game_data$`Forced Fumbles` <- as.numeric(as.character(game_data$`Forced Fumbles`))
  game_data$`Forced Fumbles`[is.na(game_data$`Forced Fumbles`)] <- 0
}
if ("Year" %in% colnames(game_data)) {
  game_data$Year <- as.integer(game_data$Year)
} else if ("Season" %in% colnames(game_data)) { 
  game_data$Year <- as.integer(game_data$Season)
  game_data <- game_data %>% rename(SeasonTmp = Season)
}

print("Column names in game_data:")
print(colnames(game_data))

ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  titlePanel("Defensive Lineman Performance Analysis"),
  sidebarLayout(
    sidebarPanel(
      h4("Data Controls"),
      width = 3,
      sliderInput("year_range",
                  "Year Range:",
                  min = min(game_data$Year, na.rm = TRUE),
                  max = max(game_data$Year, na.rm = TRUE),
                  value = c(max(game_data$Year, na.rm = TRUE) - 5, max(game_data$Year, na.rm = TRUE)),
                  sep = "",
                  step = 1
      ),
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        type = "tabs",
        tabPanel("Key Stat Distributions",
                 br(),
                 fluidRow(
                   column(4, plotlyOutput("sacksHist", height = "400px")),
                   column(4, plotlyOutput("tacklesHist", height = "400px")),
                   column(4, plotlyOutput("ffHist", height = "400px"))
                 )
        ),
        tabPanel("Performance Trends (Average per Player-Game)",
                 br(),
                 fluidRow(
                   column(12, plotlyOutput("avgSacksTrendPlot", height = "350px"))
                 ),
                 hr(), 
                 fluidRow(
                   column(12, plotlyOutput("avgTacklesTrendPlot", height = "350px"))
                 ),
                 hr(), 
                 fluidRow(
                   column(12, plotlyOutput("avgFFTrendPlot", height = "350px"))
                 )
        ),
        tabPanel("Player Game Simulation",
                 br(),
                 sidebarLayout(
                   sidebarPanel(
                     h4("Simulation Setup"),
                     width = 4,
                     selectizeInput("selected_player_sim", 
                                 "Select Player:", 
                                 choices = NULL, # Choices will be populated server-side
                                 options = list(placeholder = 'Type to search for a player...')
                                 ),
                     numericInput("num_games_sim", 
                                  "Number of Games to Simulate:", 
                                  value = 17, min = 1, max = 100, step = 1),
                     actionButton("run_simulation_btn", "Run Simulation", class = "btn-primary")
                   ),
                   mainPanel(
                     width = 8,
                     h5("Simulated Game Log"),
                     DT::dataTableOutput("simulated_games_table"),
                     hr(),
                     h5("Simulation Summary"),
                     verbatimTextOutput("simulation_summary_output")
                   )
                 )
        )
      )
    )
  )
)

server <- function(input, output, session) {

  filtered_data <- reactive({
    req(input$year_range[1] <= input$year_range[2])
    req("Year" %in% colnames(game_data)) 

    game_data %>%
      filter(Year >= input$year_range[1] & Year <= input$year_range[2])
  })

  # Tab 1: Key Stat Distributions
  output$sacksHist <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 0, "Sacks" %in% colnames(df))
    plot_ly(data = df, x = ~Sacks, type = "histogram", 
            xbins = list(size = 1), # Set bin size to 1 for Sacks
            marker = list(color = "#1f77b4")) %>%
      layout(title = "Distribution of Sacks", 
             xaxis = list(title = "Sacks per Game (bins of 1)"), 
             yaxis = list(title = "Frequency"))
  })

  output$tacklesHist <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 0, "Total Tackles" %in% colnames(df))
    plot_ly(data = df, x = ~`Total Tackles`, type = "histogram", 
            nbinsx = 20, # Suggest number of bins for Total Tackles
            marker = list(color = "#ff7f0e")) %>%
      layout(title = "Distribution of Total Tackles", 
             xaxis = list(title = "Total Tackles per Game"), 
             yaxis = list(title = "Frequency"))
  })

  output$ffHist <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 0, "Forced Fumbles" %in% colnames(df))
    plot_ly(data = df, x = ~`Forced Fumbles`, type = "histogram", 
            xbins = list(size = 1), # Set bin size to 1 for Forced Fumbles
            marker = list(color = "#2ca02c")) %>%
      layout(title = "Distribution of Forced Fumbles", 
             xaxis = list(title = "Forced Fumbles per Game (bins of 1)"), 
             yaxis = list(title = "Frequency"))
  })

  # Tab 2: Performance Trends
  yearly_avg_stats <- reactive({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    df %>%
      group_by(Year) %>%
      summarise(
        Avg_Sacks = mean(Sacks, na.rm = TRUE),
        Avg_Total_Tackles = mean(`Total Tackles`, na.rm = TRUE),
        Avg_Forced_Fumbles = mean(`Forced Fumbles`, na.rm = TRUE),
        .groups = "drop" # Removed Player_Games as it's not directly used in these specific trend plots
      ) %>%
      arrange(Year)
  })
  
  # Plot for Average Sacks Trend
  output$avgSacksTrendPlot <- renderPlotly({
    summary_df <- yearly_avg_stats()
    req(nrow(summary_df) > 0, "Avg_Sacks" %in% colnames(summary_df))
    plot_ly(summary_df, x = ~Year, y = ~Avg_Sacks, type = 'scatter', mode = 'lines+markers', name = "Avg Sacks", line = list(color = "#1f77b4")) %>%
      layout(title = "Average Sacks per Player-Game Over Years",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Avg Sacks per Game"))
  })
  
  output$avgTacklesTrendPlot <- renderPlotly({
    summary_df <- yearly_avg_stats()
    req(nrow(summary_df) > 0, "Avg_Total_Tackles" %in% colnames(summary_df))
    plot_ly(summary_df, x = ~Year, y = ~Avg_Total_Tackles, type = 'scatter', mode = 'lines+markers', name = "Avg Total Tackles", line = list(color = "#ff7f0e")) %>%
      layout(title = "Average Total Tackles per Player-Game Over Years",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Avg Total Tackles per Game"))
  })
  
  output$avgFFTrendPlot <- renderPlotly({
    summary_df <- yearly_avg_stats()
    req(nrow(summary_df) > 0, "Avg_Forced_Fumbles" %in% colnames(summary_df))
    plot_ly(summary_df, x = ~Year, y = ~Avg_Forced_Fumbles, type = 'scatter', mode = 'lines+markers', name = "Avg Forced Fumbles", line = list(color = "#2ca02c")) %>%
      layout(title = "Average Forced Fumbles per Player-Game Over Years",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Avg Forced Fumbles per Game"))
  })

  observe({
    player_names <- sort(unique(game_data$Name))
    updateSelectizeInput(session, "selected_player_sim", choices = player_names, server = TRUE) # Switched to updateSelectizeInput with server = TRUE
  })

  simulation_results_rv <- reactiveVal(NULL)

  observeEvent(input$run_simulation_btn, {
    req(input$selected_player_sim, input$num_games_sim)
    
    player_data <- game_data %>% filter(Name == input$selected_player_sim)
    num_actual_games_played <- nrow(player_data)

    if (num_actual_games_played > 0) {
      avg_sacks_per_game <- sum(player_data$Sacks, na.rm = TRUE) / num_actual_games_played
      avg_tackles_per_game <- sum(player_data$`Total Tackles`, na.rm = TRUE) / num_actual_games_played
      avg_ff_per_game <- sum(player_data$`Forced Fumbles`, na.rm = TRUE) / num_actual_games_played
    } else {
      avg_sacks_per_game <- 0
      avg_tackles_per_game <- 0
      avg_ff_per_game <- 0
    }

    lambda_sacks <- pmax(0, avg_sacks_per_game) 
    lambda_tackles <- pmax(0, avg_tackles_per_game)
    lambda_ff <- pmax(0, avg_ff_per_game)

    sim_sacks <- rpois(input$num_games_sim, lambda = lambda_sacks)
    sim_tackles <- rpois(input$num_games_sim, lambda = lambda_tackles)
    sim_ff <- rpois(input$num_games_sim, lambda = lambda_ff)

    sim_df <- data.frame(
      Game = 1:input$num_games_sim,
      Sacks = sim_sacks,
      `Total Tackles` = sim_tackles,
      `Forced Fumbles` = sim_ff
    )
    simulation_results_rv(sim_df)
  })

  output$simulated_games_table <- DT::renderDataTable({
    results <- simulation_results_rv()
    if (is.null(results)) return(DT::datatable(data.frame(Message = "Click 'Run Simulation' to generate data."), rownames = FALSE))
    DT::datatable(results, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$simulation_summary_output <- renderPrint({
    results <- simulation_results_rv()
    if (is.null(results)) return(cat("No simulation run yet."))

    total_sacks <- sum(results$Sacks)
    total_tackles <- sum(results$`Total Tackles`)
    total_ff <- sum(results$`Forced Fumbles`)
    num_sim_games <- nrow(results)

    cat(paste("Simulation Summary for", input$selected_player_sim, "over", num_sim_games, "games:\n"))
    cat(paste("----------------------------------------------------\n"))
    cat(paste("Total Sacks:", total_sacks, "(Avg per game:", round(total_sacks/num_sim_games, 2), ")\n"))
    cat(paste("Total Tackles:", total_tackles, "(Avg per game:", round(total_tackles/num_sim_games, 2), ")\n"))
    cat(paste("Total Forced Fumbles:", total_ff, "(Avg per game:", round(total_ff/num_sim_games, 2), ")\n"))
  })
}

shinyApp(ui = ui, server = server)
