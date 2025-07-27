# NFL Bayesian Distribution Plotting
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

# Define NFL Divisions
NFL_DIVISIONS <- list(
  AFC_East = c("BUF", "MIA", "NE", "NYJ"),
  AFC_North = c("BAL", "CIN", "CLE", "PIT"),
  AFC_South = c("HOU", "IND", "JAX", "TEN"),
  AFC_West = c("DEN", "KC", "LV", "LAC"),
  NFC_East = c("DAL", "NYG", "PHI", "WAS"),
  NFC_North = c("CHI", "DET", "GB", "MIN"),
  NFC_South = c("ATL", "CAR", "NO", "TB"),
  NFC_West = c("ARI", "LAR", "SF", "SEA")
)

# Simple function to get NFL team colors
get_nfl_team_colors <- function(team_names) {
  data("teamcolors")
  
  # Fallback colors for teams not in teamcolors
  fallback_colors <- list(
    "WAS" = "#5A1414",  # Washington Commanders burgundy
    "LV" = "#000000",   # Las Vegas Raiders black
    "LAC" = "#0080C6",  # Los Angeles Chargers blue
    "LAR" = "#003594"   # Los Angeles Rams blue
  )
  nfl_name_map <- c(
    # AFC East
    "BUF" = "Buffalo Bills",
    "MIA" = "Miami Dolphins", 
    "NE" = "New England Patriots",
    "NYJ" = "New York Jets",
    
    # AFC North
    "BAL" = "Baltimore Ravens",
    "CIN" = "Cincinnati Bengals",
    "CLE" = "Cleveland Browns", 
    "PIT" = "Pittsburgh Steelers",
    
    # AFC South
    "HOU" = "Houston Texans",
    "IND" = "Indianapolis Colts",
    "JAX" = "Jacksonville Jaguars",
    "TEN" = "Tennessee Titans",
    
    # AFC West
    "DEN" = "Denver Broncos",
    "KC" = "Kansas City Chiefs",
    "LV" = "Las Vegas Raiders",
    "LAC" = "Los Angeles Chargers",
    
    # NFC East
    "DAL" = "Dallas Cowboys",
    "NYG" = "New York Giants",
    "PHI" = "Philadelphia Eagles",
    "WAS" = "Washington Commanders",
    
    # NFC North
    "CHI" = "Chicago Bears",
    "DET" = "Detroit Lions",
    "GB" = "Green Bay Packers",
    "MIN" = "Minnesota Vikings",
    
    # NFC South
    "ATL" = "Atlanta Falcons",
    "CAR" = "Carolina Panthers",
    "NO" = "New Orleans Saints",
    "TB" = "Tampa Bay Buccaneers",
    
    # NFC West
    "ARI" = "Arizona Cardinals",
    "LAR" = "Los Angeles Rams",
    "SF" = "San Francisco 49ers",
    "SEA" = "Seattle Seahawks"
  )
  
  colors_df <- data.frame(
    team = team_names,
    primary = "#1f77b4",  # Default blue
    stringsAsFactors = FALSE
  )
  
  for(i in seq_along(team_names)) {
    team <- team_names[i]
    lookup_name <- ifelse(team %in% names(nfl_name_map), nfl_name_map[team], team)
    
    # Try to find team colors
    team_colors <- teamcolors %>% 
      filter(league == "nfl", name == lookup_name)
    
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

# Simple top/bottom teams plot for NFL
plot_nfl_top_bottom_teams <- function(posterior_samples, metric, n_teams = 8) {
  
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
  team_colors <- get_nfl_team_colors(selected_teams)
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
  
  # Define axis limits first
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
    # Add mean EPA values positioned relative to axis limits
    geom_text(
      data = label_stats,
      aes(x = x_max + 0.08 * x_range, y = as.numeric(team), 
          label = sprintf("%.3f", mean_value)),
      color = "black", size = 4.5, hjust = 2.3, fontface = "bold", vjust = -1
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.6) +
    # Expand limits to accommodate text
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
      panel.grid.major.x = element_line(color = "gray90", linewidth = 0.5),
      panel.grid.minor = element_blank()
    )
}

# Simple division rankings plot for NFL
plot_nfl_division_rankings <- function(posterior_samples, metric, division_name) {
  
  # Get division teams
  div_teams <- NFL_DIVISIONS[[division_name]]
  
  # Calculate team stats
  team_stats <- posterior_samples %>%
    filter(parameter == metric, team %in% div_teams) %>%
    group_by(team) %>%
    summarise(
      mean_value = mean(value),
      .groups = 'drop'
    )
  
  if(nrow(team_stats) == 0) {
    warning("No data found for ", division_name, " teams")
    return(NULL)
  }
  
  # Proper sorting for defense vs offense
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
  team_colors <- get_nfl_team_colors(team_stats$team)
  fill_colors <- setNames(team_colors$primary, team_colors$team)
  
  # Prepare plot data
  plot_data <- posterior_samples %>%
    filter(parameter == metric, team %in% div_teams) %>%
    mutate(
      team = factor(team, levels = rev(team_stats$team))
    )
  
  # Define axis limits first
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
    # Add ranking numbers positioned relative to axis limits
    geom_text(
      data = team_stats %>% mutate(team = factor(team, levels = rev(team_stats$team))),
      aes(x = x_min - 0.08 * x_range, y = as.numeric(team), label = rank),
      color = "black", size = 4.5, fontface = "bold", hjust = 0.5, vjust = -1
    ) +
    # Add mean EPA values positioned relative to axis limits
    geom_text(
      data = team_stats %>% mutate(team = factor(team, levels = rev(team_stats$team))),
      aes(x = x_max + 0.08 * x_range, y = as.numeric(team), 
          label = sprintf("%.3f", mean_value)),
      color = "black", size = 4.5, hjust = 2.1, fontface = "bold", vjust = -1
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.6) +
    # Expand limits to accommodate text on both sides
    scale_x_continuous(
      limits = c(x_min - 0.15 * x_range, x_max + 0.15 * x_range), 
      breaks = seq(x_min, x_max, by = 0.1), 
      minor_breaks = NULL
    ) +
    labs(
      title = paste(gsub("_", " ", division_name), str_to_title(gsub("_", " ", metric)), "Rankings"),
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
      panel.grid.major.x = element_line(color = "gray90", linewidth = 0.5),
      panel.grid.minor = element_blank()
    )
}

# Conference-level plots (AFC vs NFC)
plot_nfl_conference_rankings <- function(posterior_samples, metric, conference_name) {
  
  # Get conference teams
  if(conference_name == "AFC") {
    conf_teams <- unlist(NFL_DIVISIONS[c("AFC_East", "AFC_North", "AFC_South", "AFC_West")])
  } else if(conference_name == "NFC") {
    conf_teams <- unlist(NFL_DIVISIONS[c("NFC_East", "NFC_North", "NFC_South", "NFC_West")])
  } else {
    stop("Conference must be 'AFC' or 'NFC'")
  }
  
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
  
  # Proper sorting for defense vs offense
  if(grepl("defense", metric)) {
    # For defense: want lowest EPA at top
    team_stats <- arrange(team_stats, mean_value) %>%
      mutate(rank = row_number())
  } else {
    # For offense: want highest EPA at top
    team_stats <- arrange(team_stats, desc(mean_value)) %>%
      mutate(rank = row_number())
  }
  
  # Get colors
  team_colors <- get_nfl_team_colors(team_stats$team)
  fill_colors <- setNames(team_colors$primary, team_colors$team)
  
  # Prepare plot data (show top 8 and bottom 8, but avoid duplicates)
  top_8 <- head(team_stats, 8)$team
  bottom_8 <- tail(team_stats, 8)$team
  
  # Remove duplicates in case there's overlap
  selected_teams <- unique(c(top_8, bottom_8))
  
  # Create proper ordering - best teams at top of plot
  if(length(selected_teams) < 16) {
    # If we have fewer than 16 unique teams (overlap), just show all teams
    team_order <- rev(team_stats$team)
  } else {
    # Normal case: separate top and bottom
    team_order <- c(rev(bottom_8), rev(top_8))
  }
  
  plot_data <- posterior_samples %>%
    filter(parameter == metric, team %in% selected_teams) %>%
    mutate(
      team = factor(team, levels = team_order)
    )
  
  # Get stats for labels (only for selected teams)
  label_stats <- team_stats %>%
    filter(team %in% selected_teams) %>%
    mutate(
      team = factor(team, levels = team_order)
    )
  
  # Define axis limits first
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
    # Add ranking numbers
    geom_text(
      data = label_stats,
      aes(x = x_min - 0.08 * x_range, y = as.numeric(team), label = rank),
      color = "black", size = 4.5, fontface = "bold", hjust = 0.5, vjust = -1
    ) +
    # Add mean EPA values
    geom_text(
      data = label_stats,
      aes(x = x_max + 0.08 * x_range, y = as.numeric(team), 
          label = sprintf("%.3f", mean_value)),
      color = "black", size = 4.5, hjust = 2.1, fontface = "bold", vjust = -1
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.6) +
    scale_x_continuous(
      limits = c(x_min - 0.15 * x_range, x_max + 0.15 * x_range), 
      breaks = seq(x_min, x_max, by = 0.1), 
      minor_breaks = NULL
    ) +
    labs(
      title = paste(conference_name, str_to_title(gsub("_", " ", metric)), "Rankings (Top & Bottom 8)"),
      subtitle = "Numbers show conference rank and mean EPA per play",
      x = "EPA per Play",
      y = NULL
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 28, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      legend.position = "none",
      panel.grid.major.x = element_line(color = "gray90", linewidth = 0.5),
      panel.grid.minor = element_blank()
    )
}

# Main function to create all NFL plots
create_nfl_plots <- function(posterior_samples, save_plots = TRUE) {
  
  metrics <- c("pass_offense", "run_offense", "pass_defense", "run_defense")
  divisions <- names(NFL_DIVISIONS)
  conferences <- c("AFC", "NFC")
  
  plots <- list()
  
  # Create top/bottom plots
  for(metric in metrics) {
    cat("Creating NFL top/bottom plot for", metric, "\n")
    plot_name <- paste0("nfl_top_bottom_", metric)
    plots[[plot_name]] <- plot_nfl_top_bottom_teams(posterior_samples, metric)
    
    if(save_plots) {
      filename <- paste0("nfl_", plot_name, ".png")
      ggsave(filename, plots[[plot_name]], width = 10, height = 12, dpi = 300)
      cat("Saved:", filename, "\n")
    }
  }
  
  # Create division plots
  for(div in divisions) {
    for(metric in metrics) {
      cat("Creating NFL division plot for", div, metric, "\n")
      plot_name <- paste0("nfl_div_", tolower(div), "_", metric)
      plots[[plot_name]] <- plot_nfl_division_rankings(posterior_samples, metric, div)
      
      if(!is.null(plots[[plot_name]]) && save_plots) {
        filename <- paste0("nfl_", plot_name, ".png")
        ggsave(filename, plots[[plot_name]], width = 10, height = 8, dpi = 300)
        cat("Saved:", filename, "\n")
      }
    }
  }
  
  # Create conference plots
  for(conf in conferences) {
    for(metric in metrics) {
      cat("Creating NFL conference plot for", conf, metric, "\n")
      plot_name <- paste0("nfl_conf_", tolower(conf), "_", metric)
      plots[[plot_name]] <- plot_nfl_conference_rankings(posterior_samples, metric, conf)
      
      if(!is.null(plots[[plot_name]]) && save_plots) {
        filename <- paste0("nfl_", plot_name, ".png")
        ggsave(filename, plots[[plot_name]], width = 10, height = 12, dpi = 300)
        cat("Saved:", filename, "\n")
      }
    }
  }
  
  return(plots)
}

# Function to extract posterior samples for plotting (NFL version)
extract_nfl_posterior_samples <- function(fit, rapm_data) {
  cat("Extracting NFL posterior samples for visualization...\n")
  
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

# Simple wrapper function for NFL
analyze_nfl <- function(fit, rapm_data, save_plots = TRUE) {
  # Extract posterior samples
  posterior_samples <- extract_nfl_posterior_samples(fit, rapm_data)
  
  # Create plots
  plots <- create_nfl_plots(posterior_samples, save_plots)
  
  return(list(
    posterior_samples = posterior_samples,
    plots = plots
  ))
}

cat("NFL plotting functions loaded!\n")
cat("Main functions:\n")
cat("- analyze_nfl(fit, rapm_data): Run everything\n")
cat("- plot_nfl_top_bottom_teams(): Individual top/bottom plot\n")
cat("- plot_nfl_division_rankings(): Individual division plot\n")
cat("- plot_nfl_conference_rankings(): Individual conference plot\n")