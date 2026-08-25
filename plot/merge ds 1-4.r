library(ggplot2)
library(patchwork)
title_size <- 11
base_size  <- 12
font_family <- NULL
p_dumbbell_band_opt <- p_dumbbell_band_opt +
  theme(
    legend.position      = "none",
        legend.text       = element_text(size = 8),
    legend.title      = element_text(size = 8),
    
    legend.key.height = unit(6, "pt"),
    legend.key.width  = unit(8, "pt"),
    
    legend.spacing.y  = unit(1, "pt"),
    legend.spacing.x  = unit(2, "pt"),
    
    legend.box.margin = margin(0, 0, 0, 0),
    legend.margin     = margin(0, 0, 0, 0)
  )
p_sample_opt <- p_sample +
  theme_classic(base_size = base_size) +
  theme(
    text        = element_text(family = font_family),
    plot.title  = element_text(size = title_size, face = "bold", hjust = 0),
    axis.text.y = element_text(margin = margin(r = 3)),
    axis.text.x = element_text(margin = margin(t = 4)),
    legend.position   = c(0.86, 0.78),                
    legend.background = element_rect(fill = alpha("white", 0.6), colour = NA),
    legend.title      = element_text(face = "bold"),
    legend.text       = element_text(face = "bold")
  )
p_cov_cohort_opt <- p_cov_cohort +
  theme_classic(base_size = base_size) +
  theme(
    text        = element_text(family = font_family),
    plot.title  = element_text(size = title_size, face = "bold", hjust = 0),
    axis.title.y = element_text(margin = margin(r = 1, l = 0)), 
    axis.text.y  = element_text(margin = margin(r = 3)),
    legend.position = "none"                        
  )
p_IE_dist_opt <- p_IE_dist +
  theme_classic(base_size = base_size) +
  theme(
    text        = element_text(family = font_family),
    plot.title  = element_text(size = title_size, face = "bold", hjust = 0),
    axis.text.x = element_text(margin = margin(t = 4)),
    axis.text.y = element_text(margin = margin(r = 6)),
    legend.position   = c(0.86, 0.78),              
    legend.background = element_rect(fill = alpha("white", 0.6), colour = NA),
    legend.title      = element_text(face = "bold"),
    legend.text       = element_text(face = "bold")
  )

combo_2x2_opt <- (
  p_dumbbell_band_opt | p_sample_opt
) /
  (
    p_cov_cohort_opt    | p_IE_dist_opt
  ) +
  plot_layout(widths  = c(1, 1), heights = c(1, 1)) +
  plot_annotation(theme = theme(plot.margin = margin(8, 8, 8, 8)))

combo_2x2_opt


























library(ggplot2)
library(patchwork)

title_size <- 11
base_size  <- 12
font_family <- NULL
p_dumbbell_band_opt <- p_dumbbell_band_opt +
  theme(
    legend.position      = c(0.02, 0.98),
    legend.justification = c("left","top"),
    
    legend.text       = element_text(size = 8),
    legend.title      = element_text(size = 8),
    
    legend.key.height = unit(6, "pt"),
    legend.key.width  = unit(8, "pt"),
    
    legend.spacing.y  = unit(1, "pt"),
    legend.spacing.x  = unit(2, "pt"),
    
    legend.box.margin = margin(0, 0, 0, 0),
    legend.margin     = margin(0, 0, 0, 0)
  ) +
  guides(
    fill = guide_legend(   
      override.aes = list(size = 2.0)
    )
  )
for (i in seq_along(p_dumbbell_band_opt$layers)) {
  if (inherits(p_dumbbell_band_opt$layers[[i]]$geom, "GeomSegment")) {
    p_dumbbell_band_opt$layers[[i]]$aes_params$colour <- "grey70"
    p_dumbbell_band_opt$layers[[i]]$aes_params$alpha  <- 0.45
    p_dumbbell_band_opt$layers[[i]]$aes_params$linewidth <- 6.2
    p_dumbbell_band_opt$layers[[i]]$show.legend <- FALSE
  }
  if (inherits(p_dumbbell_band_opt$layers[[i]]$geom, "GeomPoint")) {
    p_dumbbell_band_opt$layers[[i]]$show.legend <- TRUE
  }
}
p_sample_opt <- p_sample +
  theme_classic(base_size = base_size) +
  theme(
    text        = element_text(family = font_family),
    plot.title  = element_text(size = title_size, face = "bold", hjust = 0),
    axis.text.y = element_text(margin = margin(r = 3)),
    axis.text.x = element_text(margin = margin(t = 4)),
    legend.position   = c(0.86, 0.78),              
    legend.background = element_rect(fill = alpha("white", 0.6), colour = NA),
    legend.title      = element_text(face = "bold"),
    legend.text       = element_text(face = "bold")
  )
p_cov_cohort_opt <- p_cov_cohort +
  theme_classic(base_size = base_size) +
  theme(
    text        = element_text(family = font_family),
    plot.title  = element_text(size = title_size, face = "bold", hjust = 0),
    axis.title.y = element_text(margin = margin(r = 1, l = 0)),  
    axis.text.y  = element_text(margin = margin(r = 3)),
    legend.position = "none"                         
  )
p_IE_dist_opt <- p_IE_dist +
  theme_classic(base_size = base_size) +
  theme(
    text        = element_text(family = font_family),
    plot.title  = element_text(size = title_size, face = "bold", hjust = 0),
    axis.text.x = element_text(margin = margin(t = 4)),
    axis.text.y = element_text(margin = margin(r = 6)),
    legend.position   = c(0.86, 0.78),                
    legend.background = element_rect(fill = alpha("white", 0.6), colour = NA),
    legend.title      = element_text(face = "bold"),
    legend.text       = element_text(face = "bold")
  )
combo_2x2_opt <- (
  p_dumbbell_band_opt | p_sample_opt
) /
  (
    p_cov_cohort_opt    | p_IE_dist_opt
  ) +
  plot_layout(widths  = c(1, 1), heights = c(1, 1)) +
  plot_annotation(theme = theme(plot.margin = margin(8, 8, 8, 8)))

combo_2x2_opt
