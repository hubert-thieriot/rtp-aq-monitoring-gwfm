# Looking at the relationship between GDP and the introduction of NAAQS


# Argentina
# link
# 1973
# Sri Lanka
# link
# 1994
# Paraguay
# link
# 2014
# Bangladesh
# link
# 1997
# Thailand
# link
# 1992
#
# Indonesia
# link
# 1999
# South Africa
# link
# 2004
# Jamaica
# link
# 1996
# Kenya
# link
# 2014
#
# Chile
# link
# 1978
# Columbia
# link
# 1982
# Mexico
# link
# 1994
# Ghana
# link
# 2019
# Uganda
# link
# 2024
#



library(tidyverse)
library(wbstats)

countries_lst <- list(
  "Argentina" = 1973,
  "Sri Lanka" = 1994,
  "Paraguay" = 2014,
  "Bangladesh" = 1997,
  "Thailand" = 1992,
  "Indonesia" = 1999,
  "South Africa" = 2004,
  "Jamaica" = 1996,
  "Kenya" = 2014,
  "Chile" = 1978,
  "Colombia" = 1982,
  "Mexico" = 1994,
  "Ghana" = 2019,
  "Uganda" = 2024
)

countries <- tibble(
  country = names(countries_lst),
  year_naaqs = unlist(countries_lst)
) %>%
  mutate(iso3 = countrycode::countrycode(country, "country.name", "iso3c"))


gdp <- wb_data(indicator = "NY.GDP.PCAP.PP.CD", country = countries$iso3, start_date = 1950, end_date = 2024)

data <- gdp %>%
  select(iso3=iso3c, year=date, gdp=NY.GDP.PCAP.PP.CD) %>%
  left_join(countries, by="iso3") %>%
  filter(!is.na(gdp), !is.na(year), !is.na(year_naaqs))

# Project Uganda til 2024
data_uganda <- data %>%
  filter(country=="Uganda") %>%
  arrange(desc(year)) %>%
  mutate(yoy = gdp/lead(gdp)-1) %>%
  tidyr::complete(iso3, country, year_naaqs, year=seq(min(data$year), 2024)) %>%
  arrange(desc(year)) %>%
  tidyr::fill(yoy, .direction="up") %>%
  # Twice 2022 -> 2024
  mutate(gdp = if_else(is.na(gdp), lead(gdp)*(1+yoy), gdp)) %>%
  mutate(gdp = if_else(is.na(gdp), lead(gdp)*(1+yoy), gdp)) %>%
  select(-c(yoy)) %>%
  filter(year %in% c(2023, 2024))

data <- bind_rows(data, data_uganda)


# install.packages("ggside")
library(ggside)
ggthemr::ggthemr('dust')
data %>%
  filter(!is.na(gdp), !is.na(year), !is.na(year_naaqs)) %>%
  filter(year==year_naaqs) %>%
  ggplot(aes(x=year_naaqs, y=gdp)) +
  geom_line(data=data, aes(x=year, y=gdp, group=country, color=year>=year_naaqs), show.legend = F) +
  geom_point(aes(fill="Introduction of NAAQS"), shape=21, col="darkred") +
  scale_color_manual(values=c("grey60", "grey90")) +
  scale_fill_manual(values=c("darkred")) +
  scale_y_continuous(labels=scales::comma) +
  geom_ysidedensity(fill="darkred", col="transparent") +
  theme_ggside_minimal() +
  theme(ggside.axis.text = element_text(size=8, color="transparent")) +
  geom_text(data=function(x){x[x$year==x$year_naaqs,]}, aes(x=year_naaqs, y=gdp, label=country), hjust=0.5, vjust=-1.2, color="grey50", size=3) +
  theme(ggside.axis.text = element_text(color="transparent"),
        ggside.axis.line = element_line(color="transparent"),
        ggside.axis.ticks = element_line(color="transparent"),
        # ggside.panel.background = element_rect(fill="grey95"),
        legend.position = c(0.1,0.95),
        legend.background  = element_blank(),
        legend.frame = element_rect(fill="grey95"),
        # Legend within the plot

        ) +
  labs(title="GDP per capita and introduction of National Air Quality Standards",
       x=NULL,
       subtitle="GDP per capita, PPP (current international $)",
       y=NULL,
       caption="Source: World Bank, various sources for the introduction of NAAQS. Uganda 2024 GDP estimated assuming constant growth since 2022.",
       col=NULL,
       fill=NULL)

ggsave("results/gdp_naaqs.png", width=10, height=6, dpi=300)


countries %>%
  left_join(
    data %>%
      select(iso3, gdp, year)
  ) %>%
  filter(year == year_naaqs) %>%
  select(country, year, gdp) %>%
  clipr::write_clip()

