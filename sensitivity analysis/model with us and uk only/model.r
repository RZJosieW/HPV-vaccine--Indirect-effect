library(readxl)
library(dplyr)
library(brms)
library(ggplot2)

hpvdata <- read_excel("/Users/V/Desktop/hpcdatafinalse.xlsx")
colnames(hpvdata)[colnames(hpvdata) == "vaccine coverage"] <- "coverage"


hpvdata$id <- factor(hpvdata$id)

hpvdata$countries <- factor(hpvdata$countries, levels = 1:11)
hpvdata$se <- as.numeric(hpvdata$SE)
hpvdata$type <- factor(as.integer(hpvdata$type),
                       levels = c(1, 2, 3),
                       labels = c("routine", "catchup", "nontarget"))
hpvdata$countries <- factor(hpvdata$countries,
                            levels = sort(unique(hpvdata$countries)))
# hpv list
hpv <- hpvdata %>%
  filter(!is.na(indirect), !is.na(coverage), !is.na(year),
         !is.na(agemid),  !is.na(type),     !is.na(countries),
         !is.na(se),
         !is.na(id)) %>%  
  mutate(
    coverage  = as.numeric(coverage),
    year      = as.numeric(year),
    agemid    = as.numeric(agemid),
    se        = as.numeric(se),
    type      = factor(type,      levels = c("routine","catchup","nontarget")),
    countries = factor(countries, levels = levels(hpvdata$countries)),
    id        = factor(id) 
  ) %>%
  droplevels()

hpv <- hpv %>%
  filter(!(type == "nontarget" & coverage < 5))

hpv_12 <- hpv %>%
  filter(countries %in% c("1", "2")) %>%
  droplevels()
stopifnot(all(c("indirect","coverage","year","agemid","type","countries", "id") %in% names(hpv_12))) # 
pri_B <- c(
  prior(student_t(3, 0, 5), class = "Intercept"),
  prior(student_t(3, 0, 5), class = "b"),
  prior(gamma(2, 0.1),      class = "nu"),        
  prior(exponential(0.5),   class = "sds"),
  prior(exponential(1.5),   class = "sd", group = "countries:type"),
  prior(exponential(1.5),   class = "sd", group = "id")
)
form_B <- bf(
  indirect | se(se, sigma = TRUE) ~ 1 + type +
    s(coverage, bs = "tp", k = 8) +
    s(year,     bs = "tp", k = 6) +
    s(agemid,   bs = "tp", k = 6) +
    s(coverage, by = type, bs = "tp", k = 6) +
    s(year,     by = type, bs = "tp", k = 4) +
    s(agemid,   by = type, bs = "tp", k = 4) +
    (1 | countries:type) +
    (1 | id)
)
fit_B <- brm(
  form_B, data = hpv_12,
  family = student(),
  prior  = pri_B,
  chains = 4, cores = 4,
  iter   = 4000, warmup = 1000,
  control = list(adapt_delta = 0.99, max_treedepth = 13)
)

