library(dplyr)
library(ggplot2)
library(stringr)
library(scales)
col_line <- c(routine="#66C2A5", catchup="#E3A45B", unvaccinated="#8DA0CB")
col_band <- c(routine="#B2DF8A", catchup="#FDBF6F", unvaccinated="#A6CEE3")
hpv_ie <- hpv %>%
  filter(!is.na(indirect), is.finite(indirect), !is.na(type)) %>%
  mutate(
    type = str_trim(tolower(as.character(type))),
    type = str_replace_all(type, "[^a-z]", ""), 
    type = factor(type, levels = c("routine","catchup","unvaccinated"))
  ) %>%
  droplevels()
med_df <- hpv_ie %>%
  group_by(type) %>%
  summarise(med = median(indirect, na.rm = TRUE), .groups = "drop")
levs <- levels(hpv_ie$type)
col_line <- col_line[levs]; names(col_line) <- levs
col_band <- col_band[levs]; names(col_band) <- levs
p_IE_dist <- ggplot(hpv_ie, aes(x = indirect, colour = type, fill = type)) +
  geom_density(alpha = 0.35, adjust = 0.9, linewidth = 0.7) +
  geom_vline(data = med_df, aes(xintercept = med, colour = type),
             linewidth = 0.7, linetype = "dashed", alpha = 1.0, show.legend = FALSE) +
  scale_colour_manual(
    values = col_line,
    breaks = c("routine","catchup","unvaccinated"),
    labels = c("Routine","Catch-up","Unvaccinated"),
    name   = "Cohort"
  ) +
  scale_fill_manual(
    values = col_band,
    breaks = c("routine","catchup","unvaccinated"),
    labels = c("Routine","Catch-up","Unvaccinated"),
    name   = "Cohort"
  ) +
  coord_cartesian(xlim = c(-10, 10)) +
  labs(
    title = "(D) Indirect Effect – Distribution by Cohort",
    x = "Indirect effect (pp)",
    y = "Density"
  ) +
  theme_classic(base_size = 11) +
  theme(
    panel.grid   = element_blank(),
    axis.line    = element_line(colour = "black", linewidth = 0.35),
    axis.ticks   = element_line(colour = "black", linewidth = 0.35),
    axis.text.x  = element_text(size = 10, margin = margin(t = 4)),
    axis.text.y  = element_text(size = 10, margin = margin(r = 6)),
    plot.title   = element_text(size = 13, hjust = 0, colour = "black"),
    legend.position   = "right",
    legend.background = element_blank(),
    legend.title = element_text(face = "bold"),
    legend.text  = element_text(face = "bold")
  )
p_IE_dist


