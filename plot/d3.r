library(dplyr)
library(ggplot2)
library(stringr)
library(scales)
hpv_clean <- hpv %>%
  mutate(
    type = str_trim(tolower(as.character(type))),
    type = str_replace_all(type, "[^a-z]", ""),   
    type = case_when(
      type %in% c("routine") ~ "routine",
      type %in% c("catchup") ~ "catchup",
      type %in% c("unvaccinated") ~ "unvaccinated",
      TRUE ~ NA_character_
    ),
    type = factor(type, levels = c("routine","catchup","unvaccinated")),
    countries = factor(
      countries,
      levels = 1:11,
      labels = c("US","UK","Australia","Sweden","Scotland",
                 "Denmark","Norway","Netherlands","Japan","Spain","Finland")
    )
  ) %>%
  filter(!is.na(type), !is.na(countries)) %>%
  droplevels()
count_df <- hpv_clean %>%
  count(countries, type, name = "n")

totals_df <- count_df %>%
  group_by(countries) %>%
  summarise(total = sum(n), .groups = "drop")

country_order <- totals_df %>%
  arrange(desc(total)) %>%
  pull(countries) %>%
  as.character()

count_df  <- count_df  %>% mutate(country_lbl = factor(as.character(countries), levels = country_order))
totals_df <- totals_df %>% mutate(country_lbl = factor(as.character(countries), levels = country_order))
levs_type <- c("routine","catchup","unvaccinated")
pal_fill  <- c(routine="#B2DF8A", catchup="#FDBF6F", unvaccinated="#A6CEE3")
pal_fill  <- alpha(pal_fill[levs_type], 0.70)  
names(pal_fill) <- levs_type
lab_use   <- c("Routine","Catch-up","Unvaccinated")
p_sample <- ggplot(count_df, aes(x = country_lbl, y = n, fill = type)) +
  geom_col(
    width = 0.64,
    colour = NA,        
    linewidth = 0       
  ) +
  coord_flip(clip = "off") +
  scale_fill_manual(
    values = pal_fill,                            
    breaks = c("routine","catchup","unvaccinated"),
    labels = c("Routine","Catch-up","Unvaccinated"),
    name   = "Cohort"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = "(B) Sample size by country and cohort",
    x = "Country",
    y = "Number of observations"
  ) +
  theme_classic(base_size = 11) +
  theme(
    panel.grid      = element_blank(),
    panel.background= element_blank(),
    panel.border    = element_rect(colour = "black", fill = NA, linewidth = 0.3),
    axis.line       = element_blank(),
    axis.ticks      = element_line(colour = "black", linewidth = 0.35),
    axis.text.x     = element_text(size = 10, margin = margin(t = 6)),
    axis.text.y     = element_text(size = 10, face = "plain", margin = margin(r = 6)),
    axis.title      = element_text(size = 12),
    plot.title      = element_text(size = 12, hjust = 0),
    legend.title    = element_text(face = "plain", size = 10),
    legend.text     = element_text(face = "plain", size = 10),
    plot.margin     = margin(6, 16, 6, 16)
  )

p_sample
