get_airqo_count <- function(){

  # Airqo
  url <- "https://analytics.airqo.net/api/v2/devices/readings/recent?token=6N8ASSGWAH8XDR7J"
  response <- fromJSON(url)

  # Read sitedetails in for each item in response$measurements
  response$measurements$siteDetails %>%
    filter(data_provider=='AirQo') %>%
    group_by(country) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    mutate(iso2 = countrycode(country, 'country.name', 'iso2c'),
           type = 'lowcost',
           source='airqo') %>%
    select(-c(country))
}
