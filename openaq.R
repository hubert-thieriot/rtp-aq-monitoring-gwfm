get_openaq_count <- function(reference=T){

  count <- tibble(
    iso2=countrycode::codelist$iso2c,
    type=ifelse(reference, TYPE_REFERENCE, TYPE_LOWCOST),
    source="openaq") %>%
    filter(!is.na(iso2))


  count$count <- NA

  # Make it so that it only updates missing ones
  # Repeat until most regions are filled
  count <- count %>%
    rowwise() %>%
    mutate(count = get_stations_count(iso2, reference=reference, default=count)) %>%
    select(iso2, count, type, source)


  return(count)
}


get_stations_count <- function(iso2, reference=TRUE, retry=4, sleep=5, backoff=2, default=NA){

  if(!is.na(default)){
    return(default)
  }

  try_get_stations_count <- function(iso2, reference){
    tryCatch({
      url <- paste0("https://api.openaq.org/v3/locations?order_by=id&sort_order=asc&monitor=", reference, "&iso=", iso2, "&limit=10&page=1")
      response <- fromJSON(url)
      return(response$meta$found)
    }, error = function(e) {
      return(NA)
    })
  }

  while(retry > 0){
    count <- try_get_stations_count(iso2, reference)
    if(!is.na(count)){
      print(count)
      return(count)
    }
    Sys.sleep(sleep)
    sleep <- sleep * backoff
    retry <- retry - 1
  }
  print("Failed")
  return(NA)
}


