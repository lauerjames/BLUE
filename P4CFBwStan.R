# College Football Bayesian RAPM Model using Stan
# Install and load required packages
if (!require("cfbfastR")) install.packages("cfbfastR")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("Matrix")) install.packages("Matrix")
if (!require("rstan")) install.packages("rstan")
if (!require("bayesplot")) install.packages("bayesplot")

library(cfbfastR)
library(tidyverse)
library(Matrix)
library(rstan)
library(bayesplot)

# Set Stan options for better performance
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Function to create RAPM matrix for a given season
create_rapm_matrix <- function(season, conference = NULL) {
  # Fetch play-by-play data for the season
  cat("Fetching play-by-play data for season", season, "...\n")
  pbp_data <- cfbfastR::load_cfb_pbp(season)
  
  # Check columns present in the data
  col_names <- colnames(pbp_data)
  cat("Checking for required columns...\n")
  cat("'rush' present:", "rush" %in% col_names, "\n")
  cat("'sack' present:", "sack" %in% col_names, "\n")
  cat("'sack_vec' present:", "sack_vec" %in% col_names, "\n")
  cat("'pass' present:", "pass" %in% col_names, "\n")
  
  # Filter for Power 4 + Notre Dame games only
  if ("offense_conference" %in% col_names && "defense_conference" %in% col_names) {
    # Hard-code the Power 4 teams + Notre Dame with CURRENT conference alignments
    power_teams <- c(
      # ACC Teams (updated with new members)
      "Boston College", "Clemson", "Duke", "Florida State", "Georgia Tech", 
      "Louisville", "Miami", "NC State", "North Carolina", "Pittsburgh", 
      "Syracuse", "Virginia", "Virginia Tech", "Wake Forest",
      "California", "Stanford", "SMU", # New ACC members
      
      # Big Ten Teams (updated with new members)
      "Illinois", "Indiana", "Iowa", "Maryland", "Michigan", 
      "Michigan State", "Minnesota", "Nebraska", "Northwestern", "Ohio State", 
      "Penn State", "Purdue", "Rutgers", "Wisconsin",
      "Oregon", "USC", "UCLA", "Washington", # New Big Ten members
      
      # Big 12 Teams (updated with new members)
      "Baylor", "Iowa State", "Kansas", "Kansas State", "TCU", 
      "Texas Tech", "West Virginia",
      "BYU", "Cincinnati", "UCF", "Houston", # 2023 additions
      "Arizona", "Arizona State", "Colorado", "Utah", # 2024 additions
      
      # SEC Teams (updated with new members)
      "Alabama", "Arkansas", "Auburn", "Florida", "Georgia", 
      "Kentucky", "LSU", "Mississippi State", "Missouri", "Ole Miss", 
      "South Carolina", "Tennessee", "Vanderbilt",
      "Oklahoma", "Texas", "Texas A&M", # Updated SEC (Texas A&M was already there)
      
      # Notre Dame
      "Notre Dame"
    )
    
    cat("Number of Power 4 + Notre Dame teams with current alignments:", length(power_teams), "\n")
    
    # Filter to only include plays where both teams are power conference teams
    initial_rows <- nrow(pbp_data)
    pbp_data <- pbp_data %>%
      filter(pos_team %in% power_teams,
             def_pos_team %in% power_teams)
    filtered_rows <- nrow(pbp_data)
    
    cat("Filtered to Power 4 + Notre Dame games only:", filtered_rows, "of", initial_rows, "plays retained\n")
  } else {
    warning("Conference columns not found, skipping Power 4 filtering")
  }
  
  # Apply specific conference filter if needed
  if (!is.null(conference)) {
    if ("offense_conference" %in% col_names && "defense_conference" %in% col_names) {
      pbp_data <- pbp_data %>% 
        filter(offense_conference == conference | defense_conference == conference)
    } else {
      warning("Conference columns not found, skipping conference filtering")
    }
  }
  
  # Clean and prepare the data
  play_data <- pbp_data %>%
    # Filter for relevant plays (exclude timeouts, penalties without plays, etc.)
    filter(
      !is.na(play_type),
      !play_type %in% c("Timeout", "End Period", "End of Half", "End of Game"),
      !is.na(EPA)
    ) %>%
    # Calculate success rate
    mutate(
      epa = EPA,
      success = ifelse(EPA > 0, 1, -1),  # Binary success indicator
      
      # Categorize play types using available columns
      play_category = case_when(
        # Pass plays
        play_type %in% c("Pass Reception", "Pass Incompletion", "Interception", "Passing Touchdown") |
          play_type == "Sack" |
          (if("pass" %in% col_names) pass == 1 else FALSE) ~ "pass",
        
        # Run plays
        play_type %in% c("Rush", "Rushing Touchdown") |
          (if("rush" %in% col_names) rush == 1 & 
             (if("sack_vec" %in% col_names) sack_vec == 0 else TRUE) &
             (if("sack" %in% col_names) sack == 0 else TRUE) else FALSE) ~ "run",
        
        # Special teams plays
        play_type %in% c("Punt", "Field Goal Good", "Field Goal Missed", 
                         "Kickoff", "Kickoff Return", "Kickoff Return Touchdown",
                         "Punt Return", "Punt Return Touchdown",
                         "Missed Field Goal Return", "Missed Field Goal Return Touchdown",
                         "Safety", "Blocked Field Goal", "Blocked Punt") |
          (if("kick_play" %in% col_names) kick_play == 1 else FALSE) |
          (if("punt_play" %in% col_names) punt_play == 1 else FALSE) |
          (if("kickoff_play" %in% col_names) kickoff_play == 1 else FALSE) ~ "special",
        
        # Default to NA for other play types
        TRUE ~ NA_character_
      )
    ) %>%
    # Remove plays that don't fit our categories
    filter(!is.na(play_category))
  
  cat("Plays after categorization:", nrow(play_data), "\n")
  cat("Play categories distribution:", table(play_data$play_category), "\n")
  
  # Create RAPM matrix
  rapm_matrix <- play_data %>%
    mutate(
      # Identify teams
      offense_team = pos_team,
      defense_team = def_pos_team,
      
      # Create identifiers for the offense/defense by play type
      off_pass = ifelse(play_category == "pass", 1, 0),
      def_pass = ifelse(play_category == "pass", -1, 0),
      off_run = ifelse(play_category == "run", 1, 0),
      def_run = ifelse(play_category == "run", -1, 0), 
      special = ifelse(play_category == "special", 1, 0)
    )
  
  # Get unique teams
  teams <- unique(c(unique(rapm_matrix$offense_team), unique(rapm_matrix$defense_team)))
  teams <- teams[!is.na(teams)]  # Remove NA values if any
  
  cat("Number of unique teams:", length(teams), "\n")
  
  # Define team indices for Stan (1-indexed)
  team_to_index <- setNames(1:length(teams), teams)
  
  # Create matrices for each component
  rapm_data <- rapm_matrix %>%
    mutate(
      offense_idx = team_to_index[offense_team],
      defense_idx = team_to_index[defense_team],
      
      # Flag which kind of play it is
      is_pass = as.integer(play_category == "pass"),
      is_run = as.integer(play_category == "run"),
      is_special = as.integer(play_category == "special")
    )
  
  # Return the processed data for Stan
  return(list(
    play_data = rapm_data,
    epa = rapm_data$epa,
    teams = teams,
    n_teams = length(teams),
    n_plays = nrow(rapm_data)
  ))
}

# Define the Stan model
stan_model_code <- "
data {
  int<lower=1> n_teams;                     // Number of teams
  int<lower=1> n_plays;                     // Number of plays
  
  vector[n_plays] epa;                      // EPA for each play
  
  int<lower=1, upper=n_teams> offense_idx[n_plays];  // Offense team index
  int<lower=1, upper=n_teams> defense_idx[n_plays];  // Defense team index
  
  int<lower=0, upper=1> is_pass[n_plays];   // Indicator for pass play
  int<lower=0, upper=1> is_run[n_plays];    // Indicator for run play
  int<lower=0, upper=1> is_special[n_plays]; // Indicator for special teams play
}

parameters {
  // Team-specific intercepts
  vector[n_teams] pass_offense;     // Pass offense ratings
  vector[n_teams] run_offense;      // Run offense ratings
  vector[n_teams] pass_defense;     // Pass defense ratings
  vector[n_teams] run_defense;      // Run defense ratings
  vector[n_teams] special_teams;    // Special teams ratings
  
  // Hierarchical variance parameters
  real<lower=0> sigma_pass_off;     // SD for pass offense
  real<lower=0> sigma_run_off;      // SD for run offense
  real<lower=0> sigma_pass_def;     // SD for pass defense
  real<lower=0> sigma_run_def;      // SD for run defense
  real<lower=0> sigma_special;      // SD for special teams
  
  real<lower=0> sigma_y;            // Residual SD
}

model {
  // Priors for team effects
  pass_offense ~ normal(0, sigma_pass_off);
  run_offense ~ normal(0, sigma_run_off);
  pass_defense ~ normal(0, sigma_pass_def);
  run_defense ~ normal(0, sigma_run_def);
  special_teams ~ normal(0, sigma_special);
  
  // Priors for variance parameters
  sigma_pass_off ~ cauchy(0, 2.5);
  sigma_run_off ~ cauchy(0, 2.5);
  sigma_pass_def ~ cauchy(0, 2.5);
  sigma_run_def ~ cauchy(0, 2.5);
  sigma_special ~ cauchy(0, 2.5);
  sigma_y ~ cauchy(0, 5);
  
  // Likelihood
  {
    vector[n_plays] mu;
    
    for (i in 1:n_plays) {
      // Initialize predicted value
      mu[i] = 0;
      
      // Add the appropriate components based on play type
      if (is_pass[i] == 1) {
        mu[i] += pass_offense[offense_idx[i]] + pass_defense[defense_idx[i]];
      } else if (is_run[i] == 1) {
        mu[i] += run_offense[offense_idx[i]] + run_defense[defense_idx[i]];
      } else if (is_special[i] == 1) {
        mu[i] += special_teams[offense_idx[i]] - special_teams[defense_idx[i]];
      }
    }
    
    // Likelihood
    epa ~ normal(mu, sigma_y);
  }
}

generated quantities {
  // Overall offensive and defensive ratings
  vector[n_teams] overall_offense;
  vector[n_teams] overall_defense;
  vector[n_teams] overall_rating;
  
  for (t in 1:n_teams) {
    overall_offense[t] = pass_offense[t] + run_offense[t];
    overall_defense[t] = pass_defense[t] + run_defense[t];
    overall_rating[t] = overall_offense[t] + overall_defense[t] + special_teams[t];
  }
}
"

# Fit the Stan model
fit_bayesian_rapm <- function(rapm_data) {
  cat("Preparing data for Stan...\n")
  
  # Format data for Stan
  stan_data <- list(
    n_teams = rapm_data$n_teams,
    n_plays = rapm_data$n_plays,
    epa = rapm_data$play_data$epa,
    offense_idx = rapm_data$play_data$offense_idx,
    defense_idx = rapm_data$play_data$defense_idx,
    is_pass = rapm_data$play_data$is_pass,
    is_run = rapm_data$play_data$is_run,
    is_special = rapm_data$play_data$is_special
  )
  
  cat("Fitting Bayesian RAPM model...\n")
  # Fit Stan model - adjust iterations as needed for your dataset
  fit <- stan(
    model_code = stan_model_code,
    data = stan_data,
    iter = 10000,
    chains = 4,
    warmup = 5000,
    thin = 1,
    control = list(adapt_delta = 0.95, max_treedepth = 12)
  )
  
  return(fit)
}

# Function to extract and organize team ratings from Stan model
extract_team_ratings <- function(fit, rapm_data, season) {
  cat("Extracting team ratings...\n")
  
  # Extract posterior samples
  posterior_samples <- rstan::extract(fit)
  
  # Function to calculate posterior mean and 95% intervals
  summarize_posterior <- function(param) {
    apply(param, 2, function(x) c(
      mean = mean(x),
      sd = sd(x),
      lower = quantile(x, 0.025),
      upper = quantile(x, 0.975)
    ))
  }
  
  # Summarize all parameters
  pass_offense_summary <- summarize_posterior(posterior_samples$pass_offense)
  run_offense_summary <- summarize_posterior(posterior_samples$run_offense)
  pass_defense_summary <- summarize_posterior(posterior_samples$pass_defense)
  run_defense_summary <- summarize_posterior(posterior_samples$run_defense)
  special_teams_summary <- summarize_posterior(posterior_samples$special_teams)
  overall_offense_summary <- summarize_posterior(posterior_samples$overall_offense)
  overall_defense_summary <- summarize_posterior(posterior_samples$overall_defense)
  overall_rating_summary <- summarize_posterior(posterior_samples$overall_rating)
  
  # Create ratings data frame
  ratings <- data.frame(
    team = rapm_data$teams,
    season = season,
    
    # Passing offense ratings
    pass_offense = pass_offense_summary[1, ],
    pass_offense_sd = pass_offense_summary[2, ],
    pass_offense_lower = pass_offense_summary[3, ],
    pass_offense_upper = pass_offense_summary[4, ],
    
    # Run offense ratings
    run_offense = run_offense_summary[1, ],
    run_offense_sd = run_offense_summary[2, ],
    run_offense_lower = run_offense_summary[3, ],
    run_offense_upper = run_offense_summary[4, ],
    
    # Pass defense ratings
    pass_defense = pass_defense_summary[1, ],
    pass_defense_sd = pass_defense_summary[2, ],
    pass_defense_lower = pass_defense_summary[3, ],
    pass_defense_upper = pass_defense_summary[4, ],
    
    # Run defense ratings
    run_defense = run_defense_summary[1, ],
    run_defense_sd = run_defense_summary[2, ],
    run_defense_lower = run_defense_summary[3, ],
    run_defense_upper = run_defense_summary[4, ],
    
    # Special teams ratings
    special_teams = special_teams_summary[1, ],
    special_teams_sd = special_teams_summary[2, ],
    special_teams_lower = special_teams_summary[3, ],
    special_teams_upper = special_teams_summary[4, ],
    
    # Overall offensive ratings
    overall_offense = overall_offense_summary[1, ],
    overall_offense_sd = overall_offense_summary[2, ],
    overall_offense_lower = overall_offense_summary[3, ],
    overall_offense_upper = overall_offense_summary[4, ],
    
    # Overall defensive ratings
    overall_defense = overall_defense_summary[1, ],
    overall_defense_sd = overall_defense_summary[2, ],
    overall_defense_lower = overall_defense_summary[3, ],
    overall_defense_upper = overall_defense_summary[4, ],
    
    # Overall team ratings
    overall_rating = overall_rating_summary[1, ],
    overall_rating_sd = overall_rating_summary[2, ],
    overall_rating_lower = overall_rating_summary[3, ],
    overall_rating_upper = overall_rating_summary[4, ]
  )
  
  return(ratings)
}

# Function to visualize ratings with uncertainty
visualize_bayesian_ratings <- function(ratings, metric = "overall_rating", top_n = 25, 
                                       title = "Overall Team Ratings") {
  cat("Visualizing", metric, "ratings...\n")
  
  # Sort and select top and bottom teams
  ratings_sorted <- ratings %>%
    arrange(desc(.data[[metric]]))
  
  top_teams <- head(ratings_sorted, top_n)
  bottom_teams <- tail(ratings_sorted, top_n)
  
  # Combine and remove duplicates
  viz_teams <- bind_rows(top_teams, bottom_teams) %>% 
    distinct(team, .keep_all = TRUE)
  
  # Define error bars based on metric
  lower_col <- paste0(metric, "_lower")
  upper_col <- paste0(metric, "_upper")
  
  # Create visualization with error bars
  p <- ggplot(viz_teams, aes(x = reorder(team, .data[[metric]]), y = .data[[metric]])) +
    geom_point(size = 3, aes(color = .data[[metric]] > 0)) +
    geom_errorbar(aes(ymin = .data[[lower_col]], ymax = .data[[upper_col]],
                      color = .data[[metric]] > 0), width = 0.2) +
    coord_flip() +
    labs(
      title = title,
      subtitle = paste("Season:", unique(ratings$season), "- With 95% Credible Intervals"),
      x = "Team",
      y = "Rating (EPA per Play)"
    ) +
    theme_minimal() +
    scale_color_manual(values = c("red", "blue"), guide = "none") +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5)
  
  # Print and return the plot
  print(p)
  return(p)
}

# Function to compare multiple components of team performance
visualize_team_components <- function(ratings, team_name) {
  cat("Visualizing components for", team_name, "...\n")
  
  # Filter for the specific team
  team_data <- ratings %>%
    filter(team == team_name)
  
  if (nrow(team_data) == 0) {
    stop("Team not found in ratings data")
  }
  
  # Prepare data for plotting
  components <- data.frame(
    component = c("Pass Offense", "Run Offense", "Pass Defense", "Run Defense", "Special Teams"),
    rating = c(team_data$pass_offense, team_data$run_offense, 
               team_data$pass_defense, team_data$run_defense, 
               team_data$special_teams),
    lower = c(team_data$pass_offense_lower, team_data$run_offense_lower, 
              team_data$pass_defense_lower, team_data$run_defense_lower, 
              team_data$special_teams_lower),
    upper = c(team_data$pass_offense_upper, team_data$run_offense_upper, 
              team_data$pass_defense_upper, team_data$run_defense_upper, 
              team_data$special_teams_upper),
    category = c("Offense", "Offense", "Defense", "Defense", "Special")
  )
  
  # Plot
  p <- ggplot(components, aes(x = reorder(component, rating), y = rating, fill = category)) +
    geom_col() +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
    coord_flip() +
    labs(
      title = paste("Performance Components for", team_name),
      subtitle = paste("Season:", unique(ratings$season)),
      x = "Component",
      y = "Rating (EPA per Play)",
      fill = "Category"
    ) +
    theme_minimal() +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5)
  
  print(p)
  return(p)
}

# Function to get RAPM ratings for a season
get_cfb_bayesian_rapm <- function(season, conference = NULL) {
  cat("Processing season:", season, "\n")
  
  # Create RAPM matrix
  rapm_data <- create_rapm_matrix(season, conference)
  
  # Fit Stan model
  fit <- fit_bayesian_rapm(rapm_data)
  
  # Extract ratings
  ratings <- extract_team_ratings(fit, rapm_data, season)
  
  return(list(
    fit = fit,
    ratings = ratings,
    rapm_data = rapm_data
  ))
}

# Main execution function
# Run the full analysis for a specific season and visualize results
analyze_season_bayesian <- function(season, conference = NULL) {
  # Get Bayesian RAPM ratings
  result <- get_cfb_bayesian_rapm(season, conference)
  
  # Save ratings to CSV
  output_file <- paste0("cfb_bayesian_rapm_ratings_", season, ".csv")
  write.csv(result$ratings, output_file, row.names = FALSE)
  cat("Ratings saved to", output_file, "\n")
  
  # Create visualizations
  # Overall ratings
  visualize_bayesian_ratings(result$ratings, "overall_rating", top_n = 25,
                             title = "Overall Team Ratings")
  
  # Offensive ratings
  visualize_bayesian_ratings(result$ratings, "overall_offense", top_n = 25,
                             title = "Offensive Team Ratings")
  
  # Defensive ratings
  visualize_bayesian_ratings(result$ratings, "overall_defense", top_n = 25,
                             title = "Defensive Team Ratings")
  
  # Special teams ratings
  visualize_bayesian_ratings(result$ratings, "special_teams", top_n = 25,
                             title = "Special Teams Ratings")
  
  # Look at top teams' components
  top_teams <- result$ratings %>%
    arrange(desc(overall_rating)) %>%
    head(5) %>%
    pull(team)
  
  for (team in top_teams) {
    visualize_team_components(result$ratings, team)
  }
  
  # Check model diagnostics
  print(summary(result$fit, pars = c("sigma_pass_off", "sigma_run_off", 
                                     "sigma_pass_def", "sigma_run_def", 
                                     "sigma_special", "sigma_y")))
  
  # Return results
  return(result)
}

# Function to compare Bayesian RAPM with ridge regression RAPM
compare_models <- function(bayesian_ratings, ridge_ratings) {
  # Join the two datasets
  comparison <- bayesian_ratings %>%
    dplyr::select(team, season, overall_rating) %>%
    rename(bayesian_rating = overall_rating) %>%
    inner_join(
      ridge_ratings %>%
        dplyr::select(team, season, overall_epa) %>%
        rename(ridge_rating = overall_epa),
      by = c("team", "season")
    )
  
  # Create comparison plot
  p <- ggplot(comparison, aes(x = ridge_rating, y = bayesian_rating)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = "Comparison of Bayesian RAPM vs Ridge Regression RAPM",
      subtitle = paste("Season:", unique(comparison$season)),
      x = "Ridge Regression RAPM Rating",
      y = "Bayesian RAPM Rating"
    ) +
    theme_minimal()
  
  print(p)
  return(comparison)
}

# Example usage:
# Analyze a single season with the Bayesian model
# result_2024 <- analyze_season_bayesian(2024)

# Run the Bayesian analysis for multiple seasons
run_multiple_seasons_bayesian <- function(seasons, conference = NULL) {
  all_results <- list()
  all_ratings <- list()
  
  for (season in seasons) {
    cat("\n========== Processing season", season, "==========\n")
    result <- get_cfb_bayesian_rapm(season, conference)
    all_results[[as.character(season)]] <- result
    all_ratings[[as.character(season)]] <- result$ratings
  }
  
  combined_ratings <- do.call(rbind, all_ratings)
  
  # Save combined ratings
  output_file <- paste0("cfb_bayesian_rapm_ratings_", min(seasons), "_", max(seasons), ".csv")
  write.csv(combined_ratings, output_file, row.names = FALSE)
  cat("Combined ratings saved to", output_file, "\n")
  
  return(list(
    all_results = all_results,
    combined_ratings = combined_ratings
  ))
}

# Example usage:
# For a single season analysis:
result_2024 <- analyze_season_bayesian(2024)



# Modified visualize_bayesian_ratings function for individual pass/rush components
visualize_component_ratings <- function(ratings, metric, top_n = 25, 
                                        title, flip_direction = FALSE) {
  cat("Visualizing", metric, "ratings...\n")
  
  # Sort and select top and bottom teams
  # If flip_direction is TRUE (for defense), we'll put negative values at the top
  if(flip_direction) {
    ratings_sorted <- ratings %>%
      arrange(.data[[metric]]) # For defense, lower is better, so no desc()
  } else {
    ratings_sorted <- ratings %>%
      arrange(desc(.data[[metric]])) # For offense, higher is better
  }
  
  top_teams <- head(ratings_sorted, top_n)
  bottom_teams <- tail(ratings_sorted, top_n)
  
  # Combine and remove duplicates
  viz_teams <- bind_rows(top_teams, bottom_teams) %>% 
    distinct(team, .keep_all = TRUE)
  
  # Define error bars based on metric
  lower_col <- paste0(metric, "_lower")
  upper_col <- paste0(metric, "_upper")
  
  # Create visualization with error bars
  p <- ggplot(viz_teams, aes(x = reorder(team, .data[[metric]], FUN = if(flip_direction) function(x) -x else identity), 
                             y = .data[[metric]])) +
    geom_point(size = 3, aes(color = if(flip_direction) .data[[metric]] < 0 else .data[[metric]] > 0)) +
    geom_errorbar(aes(ymin = .data[[lower_col]], ymax = .data[[upper_col]],
                      color = if(flip_direction) .data[[metric]] < 0 else .data[[metric]] > 0), 
                  width = 0.2) +
    coord_flip() +
    labs(
      title = title,
      subtitle = paste("Season:", unique(ratings$season), "- With 95% Credible Intervals"),
      x = "Team",
      y = "Rating (EPA per Play)"
    ) +
    theme_minimal() +
    scale_color_manual(values = c("red", "blue"), guide = "none") +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5)
  
  # Print and return the plot
  print(p)
  return(p)
}

# Usage of the modified function:
# Assuming result_2024 has been created and has ratings component

# 1. Passing Offense Ratings
pass_offense_plot <- visualize_component_ratings(
  result_2024$ratings, 
  "pass_offense", 
  top_n = 25,
  title = "Passing Offense Ratings", 
  flip_direction = FALSE
)

# 2. Rushing Offense Ratings
rush_offense_plot <- visualize_component_ratings(
  result_2024$ratings, 
  "run_offense", 
  top_n = 25,
  title = "Rushing Offense Ratings", 
  flip_direction = FALSE
)

# 3. Passing Defense Ratings (flip direction)
pass_defense_plot <- visualize_component_ratings(
  result_2024$ratings, 
  "pass_defense", 
  top_n = 25,
  title = "Passing Defense Ratings", 
  flip_direction = TRUE
)

# 4. Rushing Defense Ratings (flip direction)
rush_defense_plot <- visualize_component_ratings(
  result_2024$ratings, 
  "run_defense", 
  top_n = 25,
  title = "Rushing Defense Ratings", 
  flip_direction = TRUE
)

# Save plots if needed
ggsave("pass_offense_ratings.png", pass_offense_plot, width = 10, height = 12)
ggsave("rush_offense_ratings.png", rush_offense_plot, width = 10, height = 12)
ggsave("pass_defense_ratings.png", pass_defense_plot, width = 10, height = 12)
ggsave("rush_defense_ratings.png", rush_defense_plot, width = 10, height = 12)






# Function to save Stan fit object and associated data
save_bayesian_results <- function(result, season, prefix = "cfb_bayesian") {
  cat("Saving Bayesian RAPM results...\n")
  
  # Create filename
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  base_filename <- paste0(prefix, "_", season, "_", timestamp)
  
  # Save the Stan fit object (this is the big one)
  fit_filename <- paste0(base_filename, "_fit.rds")
  saveRDS(result$fit, file = fit_filename)
  cat("Stan fit object saved to:", fit_filename, "\n")
  
  # Save the processed data
  data_filename <- paste0(base_filename, "_data.rds")
  saveRDS(result$rapm_data, file = data_filename)
  cat("RAPM data saved to:", data_filename, "\n")
  
  # Save the extracted ratings (CSV for easy viewing)
  ratings_filename <- paste0(base_filename, "_ratings.csv")
  write.csv(result$ratings, ratings_filename, row.names = FALSE)
  cat("Ratings saved to:", ratings_filename, "\n")
  
  # Save everything together as an R object
  full_filename <- paste0(base_filename, "_complete.RData")
  save(result, file = full_filename)
  cat("Complete results saved to:", full_filename, "\n")
  
  # Return the filenames for reference
  return(list(
    fit_file = fit_filename,
    data_file = data_filename,
    ratings_file = ratings_filename,
    complete_file = full_filename,
    base_name = base_filename
  ))
}


saved_files <- save_bayesian_results(result_2024, season = 2024)

