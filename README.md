# NFL Quarterback Stats Analysis: Identifying Key Predictors of Win Percentage

This project explores the relationship between various NFL quarterback performance statistics and their contribution to team win percentage. Using historical game log data, the analysis aims to identify which key metrics are statistically significant predictors of a quarterback's success in terms of winning games.

## Project Goal

The primary objective is to understand and quantify how individual quarterback statistics (such as passing yards, touchdowns, interceptions, passer rating, etc.) correlate with and predict a quarterback's career win percentage as a starter.

## Analysis Steps

The project involves the following key steps:

1.  **Data Acquisition & Cleaning:**
    *   Loading raw game log data for NFL quarterbacks.
    *   Filtering for games where the quarterback was the designated starter.
    *   Aggregating game-by-game statistics to calculate per-game averages and overall win/loss records for each unique starting quarterback.
    *   Handling missing values (imputed as 0 for statistics).
    *   Calculating `Win_Percentage` (`Wins / Games Started`).

2.  **Exploratory Data Analysis (EDA):**
    *   Visualizing the distribution of key statistics (e.g., Passing Yards, Passer Rating) using histograms.
    *   Exploring the relationship between total career wins and average passing yards per game using box plots.
    *   Calculating and visualizing the Pearson correlation coefficients between various statistics and `Win_Percentage` using a bar chart.

3.  **Predictive Modeling:**
    *   Splitting the aggregated data into training and testing sets (70/30 split).
    *   Building and evaluating two regression models to predict `Win_Percentage`:
        *   **Lasso Regression:** Uses L1 regularization for feature selection and coefficient shrinkage.
        *   **Stepwise Linear Regression:** Automatically selects variables based on statistical criteria (bidirectional).

4.  **Model Evaluation:**
    *   Comparing the performance of the Lasso and Stepwise models using the R-squared (\(R^2\)) metric on both training and test datasets.

## Data Source

The data used in this analysis comes from the **"NFL Statistics" dataset** available on Kaggle.

*   **Dataset Name:** NFL Statistics
*   **Compiled by:** Kendall Gillies
*   **Source Link:** [https://www.kaggle.com/datasets/kendallgillies/nflstatistics/data](https://www.kaggle.com/datasets/kendallgillies/nflstatistics/data)

The specific file used is `Game_Logs_Quarterback.csv`. **Please ensure this file is placed in a `Data` subdirectory relative to the R Markdown file for the code to run correctly.**

## Key Findings

Based on the analysis:

*   **Strong Negative Predictors:** Interceptions (`Ints`), Fumbles (`Fumbles`), and Sacks (`Sacks`) consistently showed a strong negative correlation with win percentage and were significant negative predictors in the models. Ball security and avoiding negative yardage plays are crucial.
*   **Strong Positive Predictors:** Passer Rating (`PasserRating`), TD Passes (`TD Passes`), and Passing Yards (`PassingYards`) were positively correlated with win percentage. Passer Rating and TD Passes were key predictors in the Lasso model, while Passing Yards was significant in both models.
*   **Model Performance:** Both Lasso and Stepwise models achieved a similar explanatory power, with an \(R^2\) of approximately **0.37 - 0.38** on unseen test data. This indicates that the selected quarterback statistics explain a significant portion (around 37-38%) of the variance in win percentage, but other factors outside of these individual stats play a larger role in determining game outcomes.
*   **Model Differences:** Lasso highlighted `PasserRating` and `TD Passes` more prominently, while Stepwise included `PassingYards` and `Rushing Yards` but excluded `PasserRating` and `TD Passes`, likely due to multicollinearity.

## Project Structure

```
.
├── Data/
│   └── Game_Logs_Quarterback.csv  # Raw data file (must be placed here)
└── nfl_qb_analysis.Rmd          # The main R Markdown file containing the analysis code and report
```

## How to Run the Analysis

To reproduce the analysis and generate the report:

1.  **Install R and RStudio:** If you don't have them, download and install R ([https://cran.r-project.org/](https://cran.r-project.org/)) and RStudio ([https://posit.co/download/rstudio-desktop/](https://posit.co/download/rstudio-desktop/)).
2.  **Download the Data:** Download the `Game_Logs_Quarterback.csv` file from the Kaggle dataset link provided above.
3.  **Set up Project Directory:** Create a directory for this project. Place the `nfl_qb_analysis.Rmd` file in the root of this directory. Create a subdirectory named `Data` inside the project directory and place the downloaded `Game_Logs_Quarterback.csv` file inside the `Data` folder.
4.  **Install Required R Packages:** Open RStudio, open the `nfl_qb_analysis.Rmd` file, and install the necessary packages by running the following command in the R console:

    ```R
    install.packages(c("readr", "tidyverse", "dplyr", "glmnet", "tidyr", "ggplot2", "knitr", "RColorBrewer", "rmarkdown"))
    ```

5.  **Run the Analysis:** Open the `nfl_qb_analysis.Rmd` file in RStudio. Click the "Knit" button (usually located in the top toolbar) and select "Knit to PDF".

This will execute the R code chunks within the R Markdown file, perform the data cleaning, EDA, modeling, and evaluation, and generate a PDF report (`nfl_qb_analysis.pdf`) containing the full analysis write-up, including tables and figures.

## Limitations

*   **Omitted Variables:** The models do not include crucial factors like team defense, special teams, coaching, opponent strength, or situational game factors.
*   **NA Handling:** Imputing `NA` values with `0` is a simple assumption and may not be accurate for all statistics.
*   **Correlation vs. Causation:** The analysis identifies statistical associations but cannot definitively prove that better stats *cause* more wins (or vice versa).
*   **Data Granularity:** Aggregating to per-game career averages loses the nuance of play-by-play or game-specific performance and context.

## Future Work

Potential extensions to this project include:

*   Incorporating team-level metrics (e.g., defensive DVOA, offensive line performance).
*   Adjusting quarterback stats for opponent strength or defensive scheme.
*   Exploring more complex non-linear modeling techniques.
*   Analyzing data at a play-by-play or drive-by-drive level.

## Contributors

*   Cooper C
*   Manas R
*   Mark B
*   Elijah R


## Acknowledgements

Statistical modeling inspiration and guidance were sought from Google Gemini 2.5 (2025) and ChatGPT Model 4o (2025).

```
