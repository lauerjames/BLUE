# Simplified CFB Bayesian Distribution Plotting
# Focus on essential functionality with team colors

# Required packages
library(ggridges)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

# Install teamcolors if needed
if (!require("teamcolors")) {
  install.packages("teamcolors")
  library(teamcolors)
}

# Define P4 Conference Teams
P4_CONFERENCES <- list(
  ACC = c("Boston College", "Clemson", "Duke", "Florida State", "Georgia Tech", 
          "Louisville", "Miami", "NC State", "North Carolina", "Pittsburgh", 
          "Syracuse", "Virginia", "Virginia Tech", "Wake Forest",
          "California", "Stanford", "SMU"),
  
  Big_Ten = c("Illinois", "Indiana", "Iowa", "Maryland", "Michigan", 
              "Michigan State", "Minnesota", "Nebraska", "Northwestern", "Ohio State", 
              "Penn State", "Purdue", "Rutgers", "Wisconsin",
              "Oregon", "USC", "UCLA", "Washington"),
  
  Big_12 = c("Baylor", "Iowa State", "Kansas", "Kansas State", "TCU", 
             "Texas Tech", "West Virginia", "BYU", "Cincinnati", "UCF", "Houston",
             "Arizona", "Arizona State", "Colorado", "Utah"),
  
  SEC = c("Alabama", "Arkansas", "Auburn", "Florida", "Georgia", 
          "Kentucky", "LSU", "Mississippi State", "Missouri", "Ole Miss", 
          "South Carolina", "Tennessee", "Vanderbilt",
          "Oklahoma", "Texas", "Texas A&M")
)

# Simple function to get team colors
get_team_colors <- function(team_names) {
  data("teamcolors")
  
  # Complete name mappings for teamcolors package
  name_map <- c(
    # ACC
    "Boston College" = "Boston College Eagles",
    "Clemson" = "Clemson Tigers", 
    "Duke" = "Duke Blue Devils",
    "Florida State" = "Florida State Seminoles",
    "Georgia Tech" = "Georgia Tech",
    "Louisville" = "Louisville Cardinals",
    "Miami" = "Miami (FL)",
    "NC State" = "NC State",
    "North Carolina" = "North Carolina",
    "Pittsburgh" = "Pittsburgh Panthers",
    "Syracuse" = "Syracuse Orange",
    "Virginia" = "Virginia Cavaliers",
    "Virginia Tech" = "Virginia Tech Hokies",
    "Wake Forest" = "Wake Forest Demon Deacons",
    "California" = "California",
    "Stanford" = "Stanford Cardinal",
    "SMU" = "SMU",
    
    # Big Ten
    "Illinois" = "Illinois",
    "Indiana" = "Indiana Hoosiers",
    "Iowa" = "Iowa Hawkeyes",
    "Maryland" = "Maryland",
    "Michigan" = "Michigan Wolverines",
    "Michigan State" = "Michigan State Spartans",
    "Minnesota" = "Minnesota Golden Gophers",
    "Nebraska" = "Nebraska",
    "Northwestern" = "Northwestern Wildcats",
    "Ohio State" = "Ohio State",
    "Penn State" = "Penn State",
    "Purdue" = "Purdue Boilermakers",
    "Rutgers" = "Rutgers Scarlet Knights",
    "Wisconsin" = "Wisconsin",
    "Oregon" = "Oregon Ducks",
    "USC" = "Southern California Trojans",
    "UCLA" = "UCLA",
    "Washington" = "Washington Huskies",
    
    # Big 12
    "Baylor" = "Baylor Bears",
    "Iowa State" = "Iowa State Cyclones",
    "Kansas" = "Kansas Jayhawks",
    "Kansas State" = "Kansas State Wildcats",
    "TCU" = "TCU",
    "Texas Tech" = "Texas Tech Red Raiders",
    "West Virginia" = "West Virginia Mountaineers",
    "BYU" = "BYU",
    "Cincinnati" = "Cincinnati Bearcats",
    "UCF" = "UCF",
    "Houston" = "Houston Cougars",
    "Arizona" = "Arizona Wildcats",
    "Arizona State" = "Arizona State Sun Devils",
    "Colorado" = "Colorado",
    "Utah" = "Utah Utes",
    
    # SEC
    "Alabama" = "Alabama Crimson Tide",
    "Arkansas" = "Arkansas Razorbacks",
    "Auburn" = "Auburn Tigers",
    "Florida" = "Florida Gators",
    "Georgia" = "Georgia Bulldogs",
    "Kentucky" = "Kentucky Wildcats",
    "LSU" = "LSU",
    "Mississippi State" = "Mississippi State Bulldogs",
    "Missouri" = "Missouri Tigers",
    "Ole Miss" = "Ole Miss",
    "South Carolina" = "South Carolina Gamecocks",
    "Tennessee" = "Tennessee Volunteers",
    "Vanderbilt" = "Vanderbilt Commodores",
    "Oklahoma" = "Oklahoma Sooners",
    "Texas" = "Texas",
    "Texas A&M" = "Texas A&M Aggies",
    
    # Notre Dame
    "Notre Dame" = "Notre Dame Fighting Irish"
  )
  
  # Fallback colors for teams not in teamcolors
  fallback_colors <- list(
    "SMU" = "#C8102E",  # SMU red
    "UCF" = "#000000"   # UCF black
  )
  
  colors_df <- data.frame(
    team = team_names,
    primary = "#1f77b4",  # Default blue
    stringsAsFactors = FALSE
  )
  
  for(i in seq_along(team_names)) {
    team <- team_names[i]
    lookup_name <- ifelse(team %in% names(name_map), name_map[team], team)
    
    # Try to find team colors
    team_colors <- teamcolors %>% 
      filter(league == "ncaa", name == lookup_name)
    
    if(nrow(team_colors) > 0) {
      colors_df$primary[i] <- team_colors$primary[1]
    } else {
      # Try without league filter
      team_colors <- teamcolors %>% 
        filter(name == lookup_name)
      
      if(nrow(team_colors) > 0) {
        colors_df$primary[i] <- team_colors$primary[1]
      } else {
        # Check for fallback colors
        if(team %in% names(fallback_colors)) {
          colors_df$primary[i] <- fallback_colors[[team]]
          cat("Using fallback color for", team, "\n")
        } else {
          cat("Warning: No colors found for", team, "(looked up as", lookup_name, ")\n")
        }
      }
    }
  }
  
  return(colors_df)
}

# Simple top/bottom teams plot - FIXED VERSION
plot_top_bottom_teams <- function(posterior_samples, metric, n_teams = 5) {
  
  # Calculate team means
  team_stats <- posterior_samples %>%
    filter(parameter == metric) %>%
    group_by(team) %>%
    summarise(
      mean_value = mean(value),
      median_value = median(value),
      q25 = quantile(value, 0.25),
      q75 = quantile(value, 0.75),
      .groups = 'drop'
    )
  
  # Simple logic: sort all teams by EPA value
  if(grepl("defense", metric)) {
    # For defense: want lowest EPA at top, highest EPA at bottom
    all_teams_sorted <- arrange(team_stats, mean_value)  # Ascending order (best defense first)
  } else {
    # For offense: want highest EPA at top, lowest EPA at bottom  
    all_teams_sorted <- arrange(team_stats, desc(mean_value))  # Descending order (best offense first)
  }
  
  # Take top n_teams and bottom n_teams from the sorted list
  top_teams <- head(all_teams_sorted, n_teams)$team      # Best teams
  bottom_teams <- tail(all_teams_sorted, n_teams)$team   # Worst teams
  selected_teams <- c(top_teams, bottom_teams)
  
  # For plotting order: best teams at top of plot, worst teams at bottom of plot
  # ggplot puts the first factor level at the bottom, so we need to reverse the order
  # Also reverse within each group so rankings are correct within top/bottom sections
  team_order <- c(rev(bottom_teams), rev(top_teams))
  
  # Get colors
  team_colors <- get_team_colors(selected_teams)
  fill_colors <- setNames(team_colors$primary, team_colors$team)
  
  # Prepare plot data with stats for labels
  plot_data <- posterior_samples %>%
    filter(parameter == metric, team %in% selected_teams) %>%
    mutate(
      team = factor(team, levels = team_order)
    )
  
  # Get stats for labels
  label_stats <- team_stats %>%
    filter(team %in% selected_teams) %>%
    mutate(
      team = factor(team, levels = team_order)
    )
  
  # FIXED: Define axis limits first
  x_min <- -0.5
  x_max <- 0.5
  x_range <- x_max - x_min
  
  # Create plot
  ggplot(plot_data, aes(x = value, y = team)) +
    geom_density_ridges(
      aes(fill = team),
      alpha = 0.7,
      scale = 0.8,
      quantile_lines = TRUE,
      quantiles = c(0.25, 0.75)
    ) +
    scale_fill_manual(values = fill_colors, guide = "none") +
    # FIXED: Add mean EPA values positioned relative to axis limits
    geom_text(
      data = label_stats,
      aes(x = x_max + 0.08 * x_range, y = as.numeric(team), 
          label = sprintf("%.3f", mean_value)),
      color = "black", size = 4.5, hjust = 2.3, fontface = "bold", vjust = -1
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.6) +
    # FIXED: Expand limits to accommodate text
    scale_x_continuous(
      limits = c(x_min, x_max + 0.15 * x_range), 
      breaks = seq(x_min, x_max, by = 0.1), 
      minor_breaks = NULL
    ) +
    labs(
      title = paste("Top & Bottom", n_teams, str_to_title(gsub("_", " ", metric))),
      subtitle = "Numbers show mean EPA per play",
      x = "EPA per Play",
      y = NULL
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 28, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.text.y = element_text(size = 12),  # Team names font size
      legend.position = "none",
      panel.grid.major.x = element_line(color = "gray90", size = 0.5),
      panel.grid.minor = element_blank()
    )
}

# Simple conference rankings plot - FIXED VERSION
plot_conference_rankings <- function(posterior_samples, metric, conference_name) {
  
  # Get conference teams
  conf_teams <- P4_CONFERENCES[[conference_name]]
  
  # Calculate team stats
  team_stats <- posterior_samples %>%
    filter(parameter == metric, team %in% conf_teams) %>%
    group_by(team) %>%
    summarise(
      mean_value = mean(value),
      .groups = 'drop'
    )
  
  if(nrow(team_stats) == 0) {
    warning("No data found for ", conference_name, " teams")
    return(NULL)
  }
  
  # FIXED: Proper sorting for defense vs offense
  if(grepl("defense", metric)) {
    # For defense: want lowest EPA at top (best defense allows fewest points)
    team_stats <- arrange(team_stats, mean_value) %>%
      mutate(rank = row_number())
  } else {
    # For offense: want highest EPA at top (best offense scores most points)
    team_stats <- arrange(team_stats, desc(mean_value)) %>%
      mutate(rank = row_number())
  }
  
  # Get colors
  team_colors <- get_team_colors(team_stats$team)
  fill_colors <- setNames(team_colors$primary, team_colors$team)
  
  # Prepare plot data
  plot_data <- posterior_samples %>%
    filter(parameter == metric, team %in% conf_teams) %>%
    mutate(
      team = factor(team, levels = rev(team_stats$team))
    )
  
  # FIXED: Define axis limits first
  x_min <- -0.5
  x_max <- 0.5
  x_range <- x_max - x_min
  
  # Create plot
  ggplot(plot_data, aes(x = value, y = team)) +
    geom_density_ridges(
      aes(fill = team),
      alpha = 0.7,
      scale = 0.8,
      quantile_lines = TRUE,
      quantiles = c(0.25, 0.75)
    ) +
    scale_fill_manual(values = fill_colors, guide = "none") +
    # FIXED: Add ranking numbers positioned relative to axis limits
    geom_text(
      data = team_stats %>% mutate(team = factor(team, levels = rev(team_stats$team))),
      aes(x = x_min - 0.08 * x_range, y = as.numeric(team), label = rank),
      color = "black", size = 4.5, fontface = "bold", hjust = 0.5, vjust = -1
    ) +
    # FIXED: Add mean EPA values positioned relative to axis limits
    geom_text(
      data = team_stats %>% mutate(team = factor(team, levels = rev(team_stats$team))),
      aes(x = x_max + 0.08 * x_range, y = as.numeric(team), 
          label = sprintf("%.3f", mean_value)),
      color = "black", size = 4.5, hjust = 2.1, fontface = "bold", vjust = -1
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.6) +
    # FIXED: Expand limits to accommodate text on both sides
    scale_x_continuous(
      limits = c(x_min - 0.15 * x_range, x_max + 0.15 * x_range), 
      breaks = seq(x_min, x_max, by = 0.1), 
      minor_breaks = NULL
    ) +
    labs(
      title = paste(conference_name, str_to_title(gsub("_", " ", metric)), "Rankings"),
      subtitle = "Numbers show rank and mean EPA per play",
      x = "EPA per Play",
      y = NULL
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 28, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.text.y = element_text(size = 12),  # Team names font size
      legend.position = "none",
      panel.grid.major.x = element_line(color = "gray90", size = 0.5),
      panel.grid.minor = element_blank()
    )
}
# Main function to create all plots
create_cfb_plots <- function(posterior_samples, save_plots = TRUE) {
  
  metrics <- c("pass_offense", "run_offense", "pass_defense", "run_defense")
  conferences <- names(P4_CONFERENCES)
  
  plots <- list()
  
  # Create top/bottom plots
  for(metric in metrics) {
    cat("Creating top/bottom plot for", metric, "\n")
    plot_name <- paste0("top_bottom_", metric)
    plots[[plot_name]] <- plot_top_bottom_teams(posterior_samples, metric)
    
    if(save_plots) {
      filename <- paste0("cfb_", plot_name, ".png")
      ggsave(filename, plots[[plot_name]], width = 10, height = 12, dpi = 300)
      cat("Saved:", filename, "\n")
    }
  }
  
  # Create conference plots
  for(conf in conferences) {
    for(metric in metrics) {
      cat("Creating conference plot for", conf, metric, "\n")
      plot_name <- paste0("conf_", tolower(conf), "_", metric)
      plots[[plot_name]] <- plot_conference_rankings(posterior_samples, metric, conf)
      
      if(!is.null(plots[[plot_name]]) && save_plots) {
        filename <- paste0("cfb_", plot_name, ".png")
        ggsave(filename, plots[[plot_name]], width = 10, height = 10, dpi = 300)
        cat("Saved:", filename, "\n")
      }
    }
  }
  
  return(plots)
}



# Function to extract posterior samples for plotting
extract_posterior_samples <- function(fit, rapm_data) {
  cat("Extracting posterior samples for visualization...\n")
  
  # Extract all posterior samples
  posterior_samples <- rstan::extract(fit)
  
  # Get team names
  teams <- rapm_data$teams
  n_teams <- length(teams)
  
  # Convert posterior arrays to long format data frames
  convert_to_long <- function(param_array, param_name) {
    # param_array is n_samples x n_teams
    n_samples <- nrow(param_array)
    
    # Create long format
    long_data <- data.frame(
      team = rep(teams, each = n_samples),
      value = as.vector(param_array),
      parameter = param_name,
      sample_id = rep(1:n_samples, n_teams)
    )
    return(long_data)
  }
  
  # Extract all parameters
  pass_offense_long <- convert_to_long(posterior_samples$pass_offense, "pass_offense")
  run_offense_long <- convert_to_long(posterior_samples$run_offense, "run_offense")
  pass_defense_long <- convert_to_long(posterior_samples$pass_defense, "pass_defense")
  run_defense_long <- convert_to_long(posterior_samples$run_defense, "run_defense")
  special_teams_long <- convert_to_long(posterior_samples$special_teams, "special_teams")
  overall_offense_long <- convert_to_long(posterior_samples$overall_offense, "overall_offense")
  overall_defense_long <- convert_to_long(posterior_samples$overall_defense, "overall_defense")
  overall_rating_long <- convert_to_long(posterior_samples$overall_rating, "overall_rating")
  
  # Combine all into one data frame
  all_samples <- bind_rows(
    pass_offense_long,
    run_offense_long,
    pass_defense_long,
    run_defense_long,
    special_teams_long,
    overall_offense_long,
    overall_defense_long,
    overall_rating_long
  )
  
  return(all_samples)
}

# Simple wrapper function
analyze_cfb <- function(fit, rapm_data, save_plots = TRUE) {
  # Extract posterior samples (assumes you have this function)
  posterior_samples <- extract_posterior_samples(fit, rapm_data)
  
  # Create plots
  plots <- create_cfb_plots(posterior_samples, save_plots)
  
  return(list(
    posterior_samples = posterior_samples,
    plots = plots
  ))
}

cat("Simplified CFB plotting functions loaded!\n")
cat("Main functions:\n")
cat("- analyze_cfb(fit, rapm_data): Run everything\n")
cat("- plot_top_bottom_teams(): Individual top/bottom plot\n")
cat("- plot_conference_rankings(): Individual conference plot\n")