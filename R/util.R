make_country_classifiers <- function(con, touchstone_source = "201910gavi-1") {
  country_metadata <- DBI::dbGetQuery(con, "SELECT * FROM country_metadata WHERE touchstone = $1", touchstone_source)
  country_metadata$id <- NULL
  country_metadata$touchstone <- NULL
  
  country_fragility <- DBI::dbGetQuery(con, "SELECT tab1.*, tab2.is_fragile_2018 FROM
                                       (SELECT country, is_fragile AS is_fragile_2017
                                       FROM country_fragility
                                       WHERE touchstone = $1 and year = 2017) as tab1
                                       JOIN
                                       (SELECT country, is_fragile AS is_fragile_2018
                                       FROM country_fragility
                                       WHERE touchstone = $2 and year = 2018) as tab2
                                       ON tab1.country = tab2.country", list(touchstone_source, touchstone_source))
  
  country_cofinance <- DBI::dbGetQuery(con, "SELECT tab1.*, tab2.cofinance_status_2018 FROM
                                       (SELECT country, cofinance_status AS cofinance_status_2017
                                       FROM country_cofinance
                                       WHERE touchstone = $1 and year = 2017) as tab1
                                       JOIN
                                       (SELECT country, cofinance_status AS cofinance_status_2018
                                       FROM country_cofinance
                                       WHERE touchstone = $2 and year = 2018) as tab2
                                       ON tab1.country = tab2.country", list(touchstone_source, touchstone_source))
  
  country_worldbank_status <- DBI::dbGetQuery(con, "SELECT tab1.*, tab2.worldbank_status_2017 FROM
                                            (SELECT country, worldbank_status AS worldbank_status_2016
                                            FROM country_worldbank_status
                                            WHERE touchstone = $1 and year = 2016) as tab1
                                            JOIN
                                            (SELECT country, worldbank_status AS worldbank_status_2017
                                            FROM country_worldbank_status
                                            WHERE touchstone = $2 and year = 2017) as tab2
                                            ON tab1.country = tab2.country", list(touchstone_source, touchstone_source))
  i <- country_worldbank_status$country == "SRB"
  country_worldbank_status$country[i] <- "XK"
  
  dat <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "country", all.x = TRUE),
                list(country_metadata, country_fragility, country_cofinance, country_worldbank_status))
  return(dat)
}