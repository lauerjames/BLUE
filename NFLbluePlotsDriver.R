# NFL Plotting Driver Script
# Simple driver to run NFL Bayesian analysis and create plots
# Assumes plotting functions are already loaded in the R environment

# Load your saved results
# Adjust the filename to match your saved data
# load("nfl_bayesian_2024_20250619_002546_complete.RData")

# Method 1: Run everything at once (recommended)
#cat("Running complete NFL analysis...\n")

# First run the main analysis if you haven't already
#result_2024 <- analyze_nfl_season_bayesian(2024)

load("nfl_bayesian_2024_20250727_172824_complete.RData")


# Then create all plots
results <- analyze_nfl(
  fit = result_2024$fit, 
  BLUE_data = result_2024$BLUE_data, 
  save_plots = TRUE
)

cat("NFL Analysis complete! Created plots:\n")
cat("- 4 top/bottom plots (pass_offense, run_offense, pass_defense, run_defense)\n")
cat("- 32 division plots (8 divisions × 4 metrics)\n") 
cat("- 8 conference plots (2 conferences × 4 metrics)\n")
cat("- Total: 44 plots saved as PNG files\n")

# Method 2: Create individual plots if you want more control
cat("\nAlternative: Create individual plots...\n")

# Extract posterior samples first
posterior_samples <- extract_nfl_posterior_samples(result_2024$fit, result_2024$BLUE_data)

# Create specific plots
# Examples:
# pass_offense_plot <- plot_nfl_top_bottom_teams(posterior_samples, "pass_offense")
# afc_east_defense_plot <- plot_nfl_division_rankings(posterior_samples, "pass_defense", "AFC_East")
# nfc_offense_plot <- plot_nfl_conference_rankings(posterior_samples, "overall_offense", "NFC")

# Save individual plots
# ggsave("custom_nfl_pass_offense.png", pass_offense_plot, width = 10, height = 12)
# ggsave("custom_afc_east_defense.png", afc_east_defense_plot, width = 10, height = 8)

# Method 3: Create all plots but don't auto-save
all_plots <- create_nfl_plots(posterior_samples, save_plots = FALSE)

# Then examine or save specific plots
print(names(all_plots))  # See available plot names

# Example: Save a specific plot with custom settings
ggsave("my_favorite_nfl_plot.png", 
       all_plots$nfl_div_afc_east_pass_offense, 
       width = 12, height = 8, dpi = 300)

# Method 4: Create plots for specific weeks (if you ran partial season analysis)
# If you analyzed specific weeks:
# result_weeks_1_10 <- analyze_nfl_season_bayesian(2024, weeks = 1:10)
# weekly_results <- analyze_nfl(
#   fit = result_weeks_1_10$fit,
#   BLUE_data = result_weeks_1_10$BLUE_data,
#   save_plots = TRUE
# )

# Example usage for creating component-specific plots
cat("\nCreating individual component plots...\n")

# 1. Passing Offense Ratings
# pass_offense_plot <- visualize_nfl_component_ratings(
#   result_2024$ratings, 
#   "pass_offense", 
#   top_n = 16,
#   title = "NFL Passing Offense Ratings", 
#   flip_direction = FALSE
# )

# 2. Rushing Offense Ratings  
# rush_offense_plot <- visualize_nfl_component_ratings(
#   result_2024$ratings, 
#   "run_offense", 
#   top_n = 16,
#   title = "NFL Rushing Offense Ratings", 
#   flip_direction = FALSE
# )

# 3. Passing Defense Ratings
# pass_defense_plot <- visualize_nfl_component_ratings(
#   result_2024$ratings, 
#   "pass_defense", 
#   top_n = 16,
#   title = "NFL Passing Defense Ratings", 
#   flip_direction = TRUE
# )

# 4. Rushing Defense Ratings
# rush_defense_plot <- visualize_nfl_component_ratings(
#   result_2024$ratings, 
#   "run_defense", 
#   top_n = 16,
#   title = "NFL Rushing Defense Ratings", 
#   flip_direction = TRUE
# )

# Save component plots if needed
# ggsave("nfl_pass_offense_ratings.png", pass_offense_plot, width = 10, height = 12)
# ggsave("nfl_rush_offense_ratings.png", rush_offense_plot, width = 10, height = 12)
# ggsave("nfl_pass_defense_ratings.png", pass_defense_plot, width = 10, height = 12)
# ggsave("nfl_rush_defense_ratings.png", rush_defense_plot, width = 10, height = 12)

# Compare team components for top teams
cat("\nCreating team component breakdowns...\n")

# Look at top 5 overall teams' components
top_teams <- result_2024$ratings %>%
  arrange(desc(overall_rating)) %>%
  head(5) %>%
  pull(team)

cat("Top 5 teams:", paste(top_teams, collapse = ", "), "\n")

# Create component plots for each top team
# for (team in top_teams) {
#   visualize_nfl_team_components(result_2024$ratings, team)
# }

# Create division-specific analyses
cat("\nDivision-specific analysis examples...\n")

# Example: AFC East detailed analysis
# afc_east_teams <- NFL_DIVISIONS$AFC_East
# afc_east_ratings <- result_2024$ratings %>% filter(team %in% afc_east_teams)
# print("AFC East Overall Ratings:")
# print(afc_east_ratings %>% select(team, overall_rating) %>% arrange(desc(overall_rating)))

# Example: NFC West detailed analysis  
# nfc_west_teams <- NFL_DIVISIONS$NFC_West
# nfc_west_ratings <- result_2024$ratings %>% filter(team %in% nfc_west_teams)
# print("NFC West Overall Ratings:")
# print(nfc_west_ratings %>% select(team, overall_rating) %>% arrange(desc(overall_rating)))

# Save comprehensive results
cat("\nSaving comprehensive results...\n")
# saved_files <- save_nfl_bayesian_results(result_2024, season = 2024)
# cat("Results saved to:", saved_files$complete_file, "\n")

cat("\nNFL Driver script complete!\n")
cat("Key differences from CFB version:\n")
cat("- 32 NFL teams instead of ~70 college teams\n") 
cat("- 8 divisions instead of 4 conferences\n")
cat("- AFC/NFC conference groupings\n")
cat("- Different team abbreviations (3-letter codes)\n")
cat("- Uses nflreadr instead of cfbfastR\n")