# How does exposure to PM2.5 rank in terms of causes of life expectancy loss?

library(tidyverse)

raw <- read_csv("https://raw.githubusercontent.com/Air-Quality-Data-Gaps/opportunity-score/main/data/input/gbd_results_master.csv")

raw %>%
  group_by(country) %>%
  summarise(share_lyl_pm25 = sum(lyl * grepl("PM2.5", cause_of_death)) / sum(lyl)) %>%
  arrange(desc(share_lyl_pm25)) %>%
  clipr::write_clip() %>%
  write_csv('results/share_lyl_pm25.csv')
