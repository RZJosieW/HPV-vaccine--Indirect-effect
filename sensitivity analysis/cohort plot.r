library(ggplot2)
  library(patchwork)
  library(brms)
  library(grid)
cohort_line_col <- c(
  routine      = "#66C2A5",
  catchup      = "#E3A45B",
  nontarget = "#8DA0CB"
)

cohort_ribbon_col <- c(
  routine      = "#B2DF8A",
  catchup      = "#FDBF6F",
  nontarget = "#A6CEE3"
)

rib_alpha <- 0.24
pt_size   <- 1.7
ln_width  <- 1.2
smooth_spline_df <- function(x, y, n = 400) {
  
  sp <- stats::spline(
    x = x,
    y = y,
    n = n,
    method = "fmm"
  )
  
  data.frame(
    x = sp$x,
    y = sp$y
  )
}
plot_overall_single_cohort <- function(
    fit_model,
    df,
    cohort = c("routine","catchup","nontarget"),
    xvar   = c("coverage","year","agemid"),
    ndraws = 800,
    ngrid  = 320
){
  
  cohort <- match.arg(cohort)
  xvar   <- match.arg(xvar)
  
  d <- df[df$type == cohort, , drop = FALSE]
  
  if(nrow(d) == 0) return(NULL)
  xr <- switch(
    xvar,
    coverage = c(0, 100),
    year     = c(0, 10),
    agemid   = c(0, 50)
  )
  
  grid <- seq(
    xr[1],
    xr[2],
    length.out = ngrid
  )
  newd <- data.frame(
    
    countries = NA,
    id        = NA,
    
    type = factor(
      cohort,
      levels = levels(df$type)
    ),
    
    coverage =
      if (xvar == "coverage")
        grid
    else
      median(d$coverage, na.rm = TRUE),
    
    year =
      if (xvar == "year")
        grid
    else
      median(d$year, na.rm = TRUE),
    
    agemid =
      if (xvar == "agemid")
        grid
    else
      median(d$agemid, na.rm = TRUE),
    
    se = median(
      d$se,
      na.rm = TRUE
    )
  )
  E <- brms::posterior_epred(
    fit_model,
    newdata = newd,
    draws = ndraws,
    re_formula = NA
  )
  
  m  <- apply(E, 2, mean)
  ql <- apply(E, 2, quantile, 0.025)
  qh <- apply(E, 2, quantile, 0.975)
  
  dd <- data.frame(
    x    = grid,
    mean = m,
    lo   = ql,
    hi   = qh
  )
  dm <- smooth_spline_df(dd$x, dd$mean)
  names(dm)[2] <- "mean"
  
  dlo <- smooth_spline_df(dd$x, dd$lo)
  names(dlo)[2] <- "lo"
  
  dhi <- smooth_spline_df(dd$x, dd$hi)
  names(dhi)[2] <- "hi"
  
  dplot <- Reduce(
    function(L, R) merge(L, R, by = "x"),
    list(dm, dlo, dhi)
  )
  line_col <- cohort_line_col[[cohort]]
  rib_col  <- cohort_ribbon_col[[cohort]
  d$w <- 1 / d$se
  
  d$w[!is.finite(d$w)] <- NA
  rng <- range(
    d$w,
    na.rm = TRUE
  )
  if (!is.finite(rng[1]) || diff(rng) == 0) {
    d$alpha_pt <- 0.6
    } else {
    
    d$alpha_pt <- (d$w - rng[1]) / diff(rng)
     d$alpha_pt <-
      d$alpha_pt * (0.9 - 0.25) + 0.25
  }
  ggplot(dplot, aes(x = x)) +
    
    geom_hline(
      yintercept = 0,
      colour = "#555555",
      linewidth = 0.5
    ) +
    geom_ribbon(
      aes(
        ymin = lo,
        ymax = hi
      ),
      fill = rib_col,
      alpha = rib_alpha,
      colour = NA
    ) +
    
    geom_line(
      aes(y = mean),
      colour = line_col,
      linewidth = ln_width
    ) +
    
    geom_point(
      data = d,
      aes(
        x = .data[[xvar]],
        y = indirect,
        alpha = alpha_pt
      ),
      colour = line_col,
      shape = 16,
      size = pt_size
    ) +
    
    scale_alpha_identity() +
  scale_x_continuous(
    breaks = switch(
      xvar,
      coverage = seq(0, 100, 20),
      year     = seq(0, 10, 2),
      agemid   = seq(0, 50, 10)
    )
  ) +
    scale_y_continuous(
      breaks = seq(-10, 10, 5)
    ) +
    
    coord_cartesian(
      xlim = xr,
      ylim = c(-10, 10)
    ) +
    
    labs(
      x = switch(
        xvar,
        coverage = "Vaccine coverage (%)",
        year     = "Years since introduction",
        agemid   = "Age"
      ),
      y = "Indirect effect"
    ) +
    
    theme_classic(base_size = 11) +
    
    theme(
      
      plot.title = element_text(
        face = "bold",
        hjust = 0
      ),
      
      axis.line = element_line(
        colour = "#3E3E3E",
        linewidth = 0.4
      ),
      
      axis.ticks = element_line(
        colour = "#3E3E3E",
        linewidth = 0.4
      ),
      
      axis.ticks.length = unit(3, "pt"),
      
      axis.text = element_text(
        colour = "black"
      ),
      
      axis.title = element_text(
        colour = "black"
      )
    )
}
hpv$type <- factor(
  hpv$type,
  levels = c(
    "routine",
    "catchup",
    "nontarget"
  )
)

cohorts <- c(
  "routine",
  "catchup",
  "nontarget"
)

xvars <- c(
  "coverage",
  "year",
  "agemid"
)

plots <- vector(
  "list",
  length = 9
)

k <- 1

for (cc in cohorts) {
  
  for (xv in xvars) {
    
    plots[[k]] <-
      plot_overall_single_cohort(
        fit_model = fit_B,
        df        = hpv,
        cohort    = cc,
        xvar      = xv
      )
    
    k <- k + 1
  }
}
panel_3x3 <- (
  plots[[1]] | plots[[2]] | plots[[3]]
) / (
  plots[[4]] | plots[[5]] | plots[[6]]
) / (
  plots[[7]] | plots[[8]] | plots[[9]]
)

panel_3x3 +
  patchwork::plot_annotation(
    tag_levels = "A"
  )