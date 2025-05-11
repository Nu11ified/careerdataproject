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

if ("Year" %in% colnames(game_data)) {
  game_data$Year <- as.integer(game_data$Year)
}

print("Column names in game_data:")
print(colnames(game_data))

ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"), 
  
  titlePanel("Game Data Analysis"),
  
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
  simulated_data <- reactive({
    req(input$year_range[1] <= input$year_range[2])
    
    if (!"Year" %in% colnames(game_data) || !"Outcome" %in% colnames(game_data)) {
      return(list(games = data.frame(), summary = data.frame(year = integer(0), total_games = integer(0), wins = integer(0), win_percentage = numeric(0)))) # Return empty df with correct column names/types for summary
    }

    processed_data <- game_data %>%
      filter(Year >= input$year_range[1] & Year <= input$year_range[2]) %>%
      mutate(result = case_when(
        Outcome == "W" ~ "win",
        Outcome == "L" ~ "loss",
        TRUE ~ "other" 
      )) %>%
      filter(result %in% c("win", "loss")) 

    summary_per_year <- processed_data %>%
      group_by(Year) %>% 
      summarise(
        total_games = n(),
        wins = sum(result == "win"),
        .groups = "drop"
      ) %>%
      mutate(win_percentage = if_else(total_games > 0, (wins / total_games) * 100, 0)) %>% 
      complete(
        Year = seq(input$year_range[1], input$year_range[2]), 
        fill = list(total_games = 0, wins = 0, win_percentage = 0)
      ) %>%
      rename(year = Year) %>% 
      arrange(year) 
    
    list(games = processed_data, summary = summary_per_year)
  })
  
  # --- Output 1: Wins per Year Bar Chart ---
  output$winPlot <- renderPlotly({
    summary_df <- simulated_data()$summary
    
    if (nrow(summary_df) == 0 || !all(c("year", "wins", "total_games") %in% names(summary_df))) {
      return(NULL) 
    }
    
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
      scale_fill_gradient(low = "#a8dadc", high = "#1d3557") + 
      labs(
        title = paste("Wins per Year (Filtered Data)"), 
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
    
    if (nrow(summary_df) == 0 || !all(c("year", "win_percentage", "total_games", "wins") %in% names(summary_df))) {
      return(NULL)
    }

    p <- ggplot(summary_df, aes(
      x = year,
      y = win_percentage,
      group = 1, 
      text = paste(
        "Year:", year,
        "<br>Win Percentage:", sprintf("%.1f%%", win_percentage),
        "<br>Record:", wins, "-", (total_games - wins)
      )
    )) +
      geom_line(color = "#e63946", size = 1) + # Red line
      geom_point(aes(size = total_games), color = "#e63946", alpha = 0.6) + 
      scale_y_continuous(labels = scales::percent_format(scale = 1)) + 
      scale_size_continuous(range = c(2, 8)) + 
      labs(
        title = "Win Percentage Trend by Year (Filtered Data)", 
        x = "Year",
        y = "Win Percentage",
        size = "Games Played" 
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom"
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  output$winLossPie <- renderPlotly({
    games_df <- simulated_data()$games
    
    if (nrow(games_df) == 0 || !"result" %in% names(games_df)) {
      return(NULL)
    }
    
    win_loss_counts <- games_df %>%
      count(result, name = "count") %>%
      mutate(
        percentage = count / sum(count),
        label = paste0(
          str_to_title(result), ": ", count,
          " (", scales::percent(percentage, accuracy = 0.1), ")"
        )
      )
    
    plot_ly(win_loss_counts,
            labels = ~ str_to_title(result),
            values = ~ count,
            type = "pie",
            hole = 0.4, 
            textinfo = "percent", 
            hoverinfo = "text", 
            text = ~ label,
            marker = list(
              colors = c("loss" = "#e63946", "win" = "#457b9d"), 
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
    
    if (nrow(summary_df) == 0 || !all(c("year", "total_games", "wins", "win_percentage") %in% names(summary_df))) {
      return(DT::datatable(data.frame(Message = "No data available for the selected range."), rownames = FALSE, options = list(searching = FALSE, lengthChange = FALSE)))
    }
    
    display_df <- summary_df %>%
      select(
        Year = year,
        `Total Games` = total_games,
        Wins = wins,
        Losses = total_games - wins,
        `Win Percentage` = win_percentage
      )


    DT::datatable(
      display_df,
      rownames = FALSE, 
      options = list(
        pageLength = 10, 
        searching = FALSE, 
        lengthChange = FALSE 
      ),
      class = "cell-border stripe hover" 
    ) %>%
      DT::formatPercentage("Win Percentage", digits = 1)
  })
}

shinyApp(ui = ui, server = server)
