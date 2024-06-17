# Analyse cities and urban demographic data
library(tidyverse)
library(countrycode)

url <- "https://public.opendatasoft.com/api/explore/v2.1/catalog/datasets/geonames-all-cities-with-a-population-1000/exports/csv?select=name%2Ccountry_code%2Cpopulation"
raw_pop <- read.csv(url, sep = ";")


# Extract share of top-1, top-2, top3, etc. for each country
top <- raw_pop %>%
  mutate(country = countrycode(country_code, "iso2c", "country.name")) %>%
  group_by(country) %>%
  mutate(rank = rank(-population, ties.method = "first")) %>%
  ungroup() %>%
  filter(!is.na(country)) %>%
  tidyr::crossing(top_n=seq(1,10)) %>%
  group_by(country, top_n) %>%
  summarise(share = sum(population[rank <= top_n]) / sum(population)) %>%
  ungroup() %>%
  tidyr::pivot_wider(names_from = top_n, values_from = share, names_prefix = "share_of_top_")



top %>%
  clipr::write_clip() %>%
  write_csv("results/share_of_top_cities.csv")
