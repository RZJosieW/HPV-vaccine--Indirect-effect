library(ggplot2)
library(dplyr)
library(patchwork)
library(scales)
  plot_cols <- c(
  
  indirect = "#66C2A5",
  coverage = "#E3A45B",
  age      = "#8DA0CB",
  year     = "#C77CFF"
)
theme_paper <- function() {
  
  theme_classic(base_size = 13) +
    
    theme(
      
      axis.title = element_text(
        size = 13,
        face = "bold"
      ),
      
      axis.text = element_text(
        size = 11,
        colour = "black"
      ),
      
      plot.title = element_text(
        size = 14,
        face = "bold",
        hjust = 0.5
      ),
      
      plot.tag = element_text(
        size = 15,
        face = "bold"
      ),
      
      axis.line = element_line(
        linewidth = 0.7
      ),
      
      axis.ticks = element_line(
        linewidth = 0.6
      ),
      
      plot.margin = margin(
        8, 8, 8, 8
      )
    )
}
p1 <- ggplot(hpv, aes(x = indirect)) +
  
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 18,
    fill = alpha(plot_cols["indirect"], 0.55),
    colour = "white",
    linewidth = 0.3
  ) +
  
  geom_density(
    colour = plot_cols["indirect"],
    linewidth = 1.2
  ) +
  
  labs(
    x = "Indirect effect (%)",
    y = "Density",
    title = "Distribution of indirect effect",
    tag = "A"
  ) +
  
  theme_paper()
p2 <- ggplot(hpv, aes(x = coverage)) +
  
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 18,
    fill = alpha(plot_cols["coverage"], 0.55),
    colour = "white",
    linewidth = 0.3
  ) +
  
  geom_density(
    colour = plot_cols["coverage"],
    linewidth = 1.2
  ) +
  
  scale_x_continuous(
    labels = label_number()
  ) +
  
  labs(
    x = "Vaccine coverage (%)",
    y = "Density",
    title = "Distribution of vaccine coverage",
    tag = "B"
  ) +
  
  theme_paper()
p3 <- ggplot(hpv, aes(x = agemid)) +
  
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 16,
    fill = alpha(plot_cols["age"], 0.55),
    colour = "white",
    linewidth = 0.3
  ) +
  
  geom_density(
    colour = plot_cols["age"],
    linewidth = 1.2
  ) +
  
  labs(
    x = "Age (years)",
    y = "Density",
    title = "Distribution of age",
    tag = "C"
  ) +
  
  theme_paper()
p4 <- ggplot(hpv, aes(x = year)) +
  
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 14,
    fill = alpha(plot_cols["year"], 0.55),
    colour = "white",
    linewidth = 0.3
  ) +
  
  geom_density(
    colour = plot_cols["year"],
    linewidth = 1.2
  ) +
  
  labs(
    x = "Years since vaccine introduction",
    y = "Density",
    title = "Distribution of years since introduction",
    tag = "D"
  ) +
  
  theme_paper()
final_plot <- (
  p1 + p2
) / (
  p3 + p4
)
final_plot

















