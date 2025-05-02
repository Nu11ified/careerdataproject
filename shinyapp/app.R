library(shiny)
library(tidyverse)
library(plotly)
library(shinythemes)
library(DT) 

ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"), 
  
  titlePanel("Enhanced Interactive Game Simulation Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Simulation Controls"),
      width = 3, # Make sidebar slightly narrower
      
      # Slider for Number of Games
      sliderInput("num_games",
                  "Number of Games to Simulate:",
                  min = 50,
                  max = 1000,
                  value = 200,
                  step = 50
      ),
      
      # Slider for Year Range
      sliderInput("year_range",
                  "Year Range:",
                  min = 2010,
                  max = 2025,
                  value = c(2018, 2024), # Default range
                  sep = "" # Don't use comma separator for years
      ),
      
      # Slider for Win Probability
      sliderInput("win_prob",
                  "Probability of Winning:",
                  min = 0,
                  max = 1,
                  value = 0.55, # Default win probability
                  step = 0.05
      ),
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        type = "tabs",
        # Tab 1: Wins per Year Bar Chart
        tabPanel(
          "Wins per Year",
          br(), 
          plotlyOutput("winPlot", height = "500px")
        ),
        # Tab 2: Win Percentage Trend Line Chart
        tabPanel(
          "Win Percentage Trend",
          br(),
          plotlyOutput("winPercPlot", height = "500px")
        ),
        # Tab 3: Overall Win/Loss Distribution Pie Chart
        tabPanel(
          "Overall Win/Loss",
          br(),
          plotlyOutput("winLossPie", height = "500px")
        ),
        # Tab 4: Data Table
        tabPanel(
          "Simulated Data",
          br(),
          p("Summary of simulated games per year:"),
          DT::dataTableOutput("summaryTable")
        )
      )
    )
  )
)


server <- function(input, output, session) {
  # --- Reactive Data Generation ---
  # (input$num_games, input$year_range, input$win_prob)
  simulated_data <- reactive({
    # Ensure the year range is valid
    req(input$year_range[1] <= input$year_range[2])
    
    # Generate years based on the slider range
    years_to_sample <- input$year_range[1]:input$year_range[2]
    
    # Simulate game data using slider inputs
    games <- data.frame(
      year = sample(years_to_sample, input$num_games, replace = TRUE),
      week = sample(1:17, input$num_games, replace = TRUE),
      result = sample(c("win", "loss"),
                      input$num_games,
                      replace = TRUE,
                      prob = c(input$win_prob, 1 - input$win_prob)
      ) # Use win probability slider
    )
    
    # Calculate summary statistics wins/total games per year
    summary_per_year <- games %>%
      # Count total games and wins per year
      group_by(year) %>%
      summarise(
        total_games = n(),
        wins = sum(result == "win"),
        .groups = "drop"
      ) %>%
      # Calculate win percentage
      mutate(win_percentage = (wins / total_games) * 100) %>%
      # Ensure all years in the selected range are present
      complete(
        year = years_to_sample,
        fill = list(total_games = 0, wins = 0, win_percentage = 0)
      ) %>%
      arrange(year) # Sort by year
    
    # Return a list containing both the raw games and the summary
    list(games = games, summary = summary_per_year)
  })
  
  # --- Output 1: Wins per Year Bar Chart ---
  output$winPlot <- renderPlotly({
    # Access the summary data from the reactive block
    summary_df <- simulated_data()$summary
    
    # Create the ggplot object
    p <- ggplot(summary_df, aes(
      x = factor(year),
      y = wins,
      text = paste(
        "Year:", year,
        "<br>Wins:", wins,
        "<br>Total Games:", total_games
      )
    )) +
      geom_col(aes(fill = wins), show.legend = FALSE) +
      scale_fill_gradient(low = "#a8dadc", high = "#1d3557") + # New color scheme
      labs(
        title = paste("Simulated Wins per Year (", input$num_games, " Games Total)"),
        x = "Year",
        y = "Number of Wins"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank()
      )
    
    # Convert to interactive plotly plot
    ggplotly(p, tooltip = "text")
  })
  
  output$winPercPlot <- renderPlotly({
    summary_df <- simulated_data()$summary
    
    p <- ggplot(summary_df, aes(
      x = year,
      y = win_percentage,
      group = 1, # Needed for a single line
      text = paste(
        "Year:", year,
        "<br>Win Percentage:", sprintf("%.1f%%", win_percentage),
        "<br>Record:", wins, "-", (total_games - wins)
      )
    )) +
      geom_line(color = "#e63946", size = 1) + # Red line
      geom_point(aes(size = total_games), color = "#e63946", alpha = 0.6) + # Points sized by games played
      scale_y_continuous(labels = scales::percent_format(scale = 1)) + # Format y-axis as %
      scale_size_continuous(range = c(2, 8)) + # Control size range of points
      labs(
        title = "Simulated Win Percentage Trend by Year",
        x = "Year",
        y = "Win Percentage",
        size = "Games Played" # Legend title for size
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom"
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  output$winLossPie <- renderPlotly({
    # Get the raw game results
    games_df <- simulated_data()$games
    
    # Count wins and losses
    win_loss_counts <- games_df %>%
      count(result, name = "count") %>%
      mutate(
        percentage = count / sum(count),
        label = paste0(
          str_to_title(result), ": ", count,
          " (", scales::percent(percentage, accuracy = 0.1), ")"
        )
      )
    
    # Create a Plotly pie chart
    plot_ly(win_loss_counts,
            labels = ~ str_to_title(result),
            values = ~ count,
            type = "pie",
            hole = 0.4, # Make it a donut chart
            textinfo = "percent", # Show percentage on slices
            hoverinfo = "text", # Show custom text on hover
            text = ~ label,
            marker = list(
              colors = c("loss" = "#e63946", "win" = "#457b9d"), # Specify colors
              line = list(color = "#FFFFFF", width = 1)
            )
    ) %>%
      layout(
        title = list(text = "Overall Win/Loss Distribution", x = 0.5, xanchor = "center"),
        showlegend = TRUE,
        legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.1)
      )
  })
  
  output$summaryTable <- DT::renderDataTable({
    summary_df <- simulated_data()$summary
    
    # Select and rename columns for better presentation
    display_df <- summary_df %>%
      select(
        Year = year,
        `Total Games` = total_games,
        Wins = wins,
        Losses = total_games - wins,
        `Win Percentage` = win_percentage
      )
    
    # Use DT::datatable for an interactive table with options
    DT::datatable(
      display_df,
      rownames = FALSE, # Don't show row numbers
      options = list(
        pageLength = 10, 
        searching = FALSE, # Disable search box for this simple table
        lengthChange = FALSE # Disable "show X entries" dropdown
      ),
      class = "cell-border stripe hover" 
    ) %>%
      # Format the percentage column
      DT::formatPercentage("Win Percentage", digits = 1)
  })
}

shinyApp(ui = ui, server = server)
