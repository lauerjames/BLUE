# CFB Plotting Driver Script
# Simple driver to run CFB Bayesian analysis and create plots
# Assumes plotting functions are already loaded in the R environment

# Load your saved results
# Adjust the filename to match your saved data
load("cfb_bayesian_2024_20250619_002546_complete.RData")

# Method 1: Run everything at once (recommended)
cat("Running complete CFB analysis...\n")
results <- analyze_cfb(
  fit = result$fit, 
  rapm_data = result$rapm_data, 
  save_plots = TRUE
)

cat("Analysis complete! Created plots:\n")
cat("- 4 top/bottom plots (pass_offense, run_offense, pass_defense, run_defense)\n")
cat("- 16 conference plots (4 conferences × 4 metrics)\n")
cat("- Total: 20 plots saved as PNG files\n")

# Method 2: Create individual plots if you want more control
cat("\nAlternative: Create individual plots...\n")

# Extract posterior samples first
posterior_samples <- extract_posterior_samples(result$fit, result$rapm_data)

# Create specific plots
#pass_offense_plot <- plot_top_bottom_teams(posterior_samples, "pass_offense")
#sec_defense_plot <- plot_conference_rankings(posterior_samples, "pass_defense", "SEC")

# Save individual plots
#ggsave("custom_pass_offense.png", pass_offense_plot, width = 10, height = 12)
#ggsave("custom_sec_defense.png", sec_defense_plot, width = 10, height = 10)

# Method 3: Create all plots but don't auto-save
all_plots <- create_cfb_plots(posterior_samples, save_plots = FALSE)

# Then examine or save specific plots
print(names(all_plots))  # See available plot names

# Example: Save a specific plot with custom settings
ggsave("my_favorite_plot.png", 
       all_plots$conf_sec_pass_offense, 
       width = 12, height = 8, dpi = 300)

cat("\nDriver script complete!\n")