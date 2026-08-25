col_line <- c(
  routine      = "#66C2A5",
  catchup      = "#E3A45B",
  unvaccinated = "#8DA0CB"
)
col_band <- c(
  routine      = "#B2DF8A",
  catchup      = "#FDBF6F",
  unvaccinated = "#A6CEE3"
)

hpv_cov <- hpv %>%
  dplyr::filter(is.finite(coverage), !is.na(type)) %>%
  dplyr::mutate(
    type = factor(type, levels = c("routine","catchup","unvaccinated")),
    coverage_disp = if (max(coverage, na.rm=TRUE) <= 1 + 1e-8) coverage*100 else coverage
  )
levs <- levels(hpv_cov$type)
col_line <- col_line[levs]; names(col_line) <- levs
col_band <- col_band[levs]; names(col_band) <- levs

p_cov_cohort <- ggplot(hpv_cov, aes(x = type, y = coverage_disp,
                                    fill = type, colour = type)) +
  geom_violin(width = 0.9, alpha = 0.55, colour = NA, trim = TRUE) +
  geom_boxplot(width = 0.35, alpha = 0.3, outlier.shape = NA, linewidth = 0.7,
               position = position_identity()) +
  geom_jitter(width = 0.10, alpha = 0.4, size = 1.9, stroke = 0) +
    scale_fill_manual(values = col_band,
                    breaks = c("routine","catchup","unvaccinated"),
                    labels = c("Routine","Catch-up","Unvaccinated"),
                    name = "Cohort") +
 
  scale_color_manual(values = col_line,
                     breaks = c("routine","catchup","unvaccinated"),
                     labels = c("Routine","Catch-up","Unvaccinated"),
                     name = "Cohort") +
  labs(
    title = "(C) Vaccine Coverage Distribution by Cohort",
    x = NULL,
    y = if (max(hpv_cov$coverage_disp, na.rm=TRUE) > 1.5) "Vaccine coverage (%)" else "Vaccine coverage"
  ) +
  theme_classic(base_size = 11) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5), 
    
    panel.grid = element_blank(),
    axis.line   = element_blank(), 
    axis.ticks  = element_line(colour = "black", linewidth = 0.35),
    axis.text.x = element_text(size = 11, margin = margin(t = 6)),
    axis.text.y = element_text(size = 10, margin = margin(r = 6)),
    axis.title.y= element_text(size = 12),
    plot.title  = element_text(size = 11, hjust = 0, colour = "black"),
    legend.position = "none"
  )
p_cov_cohort

library(dplyr)

stats_tbl <- hpv_cov %>%
  group_by(type) %>%
  summarize(
    n      = sum(is.finite(coverage_disp)),
    min    = min(coverage_disp, na.rm = TRUE),
    q1     = quantile(coverage_disp, 0.25, na.rm = TRUE),
    median = median(coverage_disp, na.rm = TRUE),
    q3     = quantile(coverage_disp, 0.75, na.rm = TRUE),
    max    = max(coverage_disp, na.rm = TRUE),
    iqr    = IQR(coverage_disp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  rowwise() %>%
  mutate(
    whisker_low  = max(min,  q1 - 1.5*iqr),
    whisker_high = min(max,  q3 + 1.5*iqr)
  ) %>%
  ungroup()

stats_tbl

