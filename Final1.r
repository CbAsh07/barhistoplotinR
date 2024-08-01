# Load the necessary libraries
library(ggplot2)
library(dplyr)
library(ggsignif) # Load the ggsignif package

# Function to create a bar plot with error bars and significance
create_plot <- function(data_file, cld_col, title) {
  # Read the data from the CSV file
  data <- read.csv(data_file)
  
  # Ensure 'Group' is a factor with the correct order
  data$Group <- factor(data$Group, levels = c("Ctrl", "A. tim."))
  
  # Calculate mean and standard error for each group
  summary_data <- data %>%
    group_by(Group) %>%
    summarise(
      mean_value = mean(.data[[cld_col]]),
      se = sd(.data[[cld_col]]) / sqrt(n())
    )
  
  # Perform a t-test to determine significance
  t_test_result <- t.test(data[[cld_col]] ~ data$Group)
  significance_label <- ifelse(t_test_result$p.value < 0.05, "*", "NS")
  
  # Create the bar plot with error bars and significance
  ggplot(summary_data, aes(x = Group, y = mean_value, fill = Group)) +
    geom_bar(stat = "identity", position = position_dodge(), color = "black", width = 0.5, size = 1.5) + # Increased size of bar lines
    geom_errorbar(aes(ymin = mean_value - se, ymax = mean_value + se), width = 0.2, position = position_dodge(0.9)) +
    geom_jitter(data = data, aes(x = Group, y = .data[[cld_col]]), width = 0.1, size = 3, color = "black") +
    labs(title = title, x = "", y = "Relative Fold Change") +
    scale_fill_manual(values = c("Ctrl" = "#32CD32", "A. tim." = "red")) +
    scale_x_discrete(labels = c("Ctrl", expression(italic("A. tim.")))) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 2)) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 50, face = "bold"),
      axis.title.x = element_text(size = 40, face = "bold", color = "black"),
      axis.title.y = element_text(size = 40, face = "bold", color = "black"),
      axis.text.y = element_text(size = 30, face = "bold", color = "black"), # Adjusted size for y-axis text
      axis.text.x = element_text(size = 30, face = "bold", color = "black"), # Adjusted size and bold for x-axis text
      axis.line = element_line(color = "black", size = 1.5, linetype = "solid"), # Add axis lines
      axis.ticks = element_line(color = "black", size = 1.5), # Increase ticks thickness
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line.x = element_line(color = "black", size = 1.5, linetype = "solid"),
      axis.line.y = element_line(color = "black", size = 1.5, linetype = "solid"),
      axis.ticks.length = unit(15, "pt"), # Specify tick length
      legend.position = "none" # Remove legend
    ) +
    geom_signif(comparisons = list(c("Ctrl", "A. tim.")), 
                annotations = significance_label,
                textsize = 15, 
                tip_length = 0.00, 
                vjust = 0.1)
}

# Create plots for cld-1 and cld-7
plot_cld1 <- create_plot("cld-1_to_ash.csv", "Cld.1", "Cld-1")
plot_cld7 <- create_plot("cld-7_to_ash.csv", "Cld.7", "Cld-7")

# Print the plots
print(plot_cld1)
print(plot_cld7)
