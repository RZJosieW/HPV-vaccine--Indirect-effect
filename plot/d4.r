
library(dplyr)
library(ggplot2)
library(scales)
intro <- tibble::tribble(
  ~country,     ~id, ~girls, ~boys, ~pause_start, ~pause_end,
  "US",          1,   2006,   2011,  NA, NA,
  "UK",          2,   2008,   2019,  NA, NA,
  "Australia",   3,   2007,   2013,  NA, NA,
  "Sweden",      4,   2012,   2020,  NA, NA,
  "Scotland",    5,   2008,   2019,  NA, NA,
  "Denmark",     6,   2008,   2019,  NA, NA,
  "Norway",      7,   2009,   2018,  NA, NA,
  "Netherlands", 8,   2009,   2021,  NA, NA,
  "Japan",       9,   2010,     NA,  2013.5, 2022.25,
  "Spain",      10,   2007,   2022,  NA, NA,
  "Finland",    11,   2013,   2020,  NA, NA
)
desired_order <- c("Finland","Spain","Scotland","Norway","Denmark",
                   "Australia","Netherlands","Japan","Sweden","UK","US")

intro_ord <- intro %>%
  mutate(country = factor(country, levels = rev(desired_order)))
end_year <- 2023
seg_normal <- intro_ord %>%
  filter(country != "Japan")
seg_japan_1 <- intro_ord %>%
  filter(country == "Japan") %>%
  mutate(x_start = girls, x_end = pause_start)
seg_japan_2 <- intro_ord %>%
  filter(country == "Japan") %>%
  mutate(x_start = pause_end, x_end = end_year)
girls_df <- intro_ord %>%
  transmute(country, year = girls)

boys_df <- intro_ord %>%
  filter(is.finite(boys)) %>%
  transmute(country, year = boys)
col_girls <- "#E41A1C"
col_boys  <- "#377EB8"
x_min <- floor(min(intro_ord$girls, na.rm = TRUE)) - 1
x_max <- 2025
p_dumbbell_band_opt <- ggplot() +
    geom_segment(
    data = seg_normal,
    aes(x = girls, xend = end_year, y = country, yend = country),
    colour = "grey70",
    alpha = 0.55,
    linewidth = 3.5,
    lineend = "round"
  ) +
    geom_segment(
    data = seg_japan_1,
    aes(x = x_start, xend = x_end, y = country, yend = country),
    colour = "grey70",
    alpha = 0.55,
    linewidth = 3.5,
    lineend = "round"
  ) +
    geom_segment(
    data = seg_japan_2,
    aes(x = x_start, xend = x_end, y = country, yend = country),
    colour = "grey70",
    alpha = 0.55,
    linewidth = 3.5,
    lineend = "round"
  ) +
    geom_segment(
    data = intro_ord %>% filter(country == "Japan"),
    aes(
      x = pause_start + 0.2,
      xend = pause_end - 0.2,
      y = country,
      yend = country
    ),
    colour = "grey60",
    linewidth = 1.0,
    linetype = "dashed",
    alpha = 0.7
  )+
  geom_point(
    data = girls_df,
    aes(x = year, y = country),
    shape = 16,
    colour = col_girls,
    size = 3.0
  ) +
    geom_point(
    data = boys_df,
    aes(x = year, y = country),
    shape = 16,
    colour = col_boys,
    size = 3.0
  ) +
    scale_x_continuous(
    breaks = seq(2005, 2025, by = 5),
    limits = c(x_min, x_max),
    expand = c(0, 0)
  ) +
    scale_y_discrete(
    limits = rev(desired_order),
    drop = FALSE
  ) +
    labs(
    title = "(A) Vaccination programme timeline (girls vs boys)",
    x = "Calendar year",
    y = NULL
  ) +
    theme_classic(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.line  = element_line(colour = "black", linewidth = 0.6),
    axis.ticks = element_line(colour = "black", linewidth = 0.4),
    
    axis.text.y = element_text(size = 10, face = "bold", margin = margin(r = 8)),
    axis.text.x = element_text(size = 10),
    
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    
    legend.position = "none"
  )
p_dumbbell_band_opt































library(dplyr)
library(ggplot2)
library(scales)
intro <- tibble::tribble(
  ~country,     ~id, ~girls, ~boys,
  "US",          1,   2006,   2011,
  "UK",          2,   2008,   2019,
  "Australia",   3,   2007,   2013,
  "Sweden",      4,   2012,   2020,
  "Scotland",    5,   2008,   2019,
  "Denmark",     6,   2008,   2019,
  "Norway",      7,   2009,   2018,
  "Netherlands", 8,   2009,   2021,
  "Japan",       9,   2010,     NA,
  "Spain",      10,   2007,   2022,
  "Finland",    11,   2013,   2020
)

desired_order <- c("Finland","Spain","Scotland","Norway","Denmark",
                   "Australia","Netherlands","Japan","Sweden","UK","US")

intro_ord <- intro %>%
  mutate(country = factor(country, levels = rev(desired_order)))
end_year <- 2023
girls_df <- intro_ord %>%
  transmute(country, year = girls, sex = "Girls")

boys_df <- intro_ord %>%
  filter(is.finite(boys)) %>%
  transmute(country, year = boys, sex = "Boys")
col_girls <- "#E41A1C"
col_boys  <- "#377EB8"
x_min <- floor(min(intro_ord$girls, na.rm = TRUE)) - 1
x_max <- end_year + 1
p_dumbbell_band_opt <- ggplot() +
    geom_segment(
    data = intro_ord,
    aes(x = girls, xend = end_year, y = country, yend = country),
    colour = "grey70",
    alpha = 0.6,
    linewidth = 6,
    lineend = "round"
  ) +
    geom_point(
    data = girls_df,
    aes(x = year, y = country, colour = sex),
    size = 3.4
  ) +
    geom_point(
    data = boys_df,
    aes(x = year, y = country, colour = sex),
    size = 3.4
  ) +
    scale_colour_manual(
    values = c(Girls = col_girls, Boys = col_boys),
    name = NULL
  ) +
    scale_x_continuous(
    breaks = pretty_breaks(6),
    limits = c(x_min, x_max),
    expand = c(0, 0)
  ) +
    scale_y_discrete(limits = rev(desired_order), drop = FALSE) +
    labs(
    title = "(A) Vaccination programme timeline (girls vs boys)",
    x = "Calendar year",
    y = NULL
  ) +
    theme_classic(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.line  = element_line(colour = "black", linewidth = 0.6),
    axis.ticks = element_line(colour = "black", linewidth = 0.4),
    
    axis.text.y = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(size = 10),
    
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    
    legend.position = c(0.02, 0.98),
    legend.justification = c("left","top"),
    legend.background = element_rect(fill = alpha("white", 0.6), colour = NA),
    legend.text = element_text(size = 9)
  )
p_dumbbell_band_opt
