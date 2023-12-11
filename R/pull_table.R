pull_table <- function(annex, con,
                       schema_name =  "_202210covidimpact_default_update",
                       table = "cross_all",
                       period = 2000:2030,
                       short_run = TRUE){
  # country classifiers
  meta <- DBI::dbGetQuery(con, "SELECT country, gavi74, who_region 
                          FROM country_metadata WHERE touchstone = '201910gavi-1'")
  touchpoint <- DBI::dbGetQuery(annex, sprintf( "SELECT * FROM %s.touchpoint", schema_name))
  # list col names
  cols <- names(DBI::dbGetQuery(annex, sprintf("SELECT * FROM %s.%s LIMIT 1", schema_name, table)))
  # find burden and impact cols
  outcome_cols <- cols[grepv(c("deaths", "cases", "dalys"), cols, FALSE) & !grepl("rate", cols)]
  # make up cols to be aggregated
  sql_cols <-  paste0("sum(", outcome_cols, ") AS ", outcome_cols)
  
  if(!short_run)run_id_list <- 1:200 else run_id_list <- 1:2
  
  
  sql <- paste(sprintf("SELECT touchpoint_id, run_id, country, year, %s", paste(sql_cols, collapse = ",")),
               sprintf("FROM %s.%s", schema_name, table),
               sprintf("WHERE year IN %s", vimpact:::sql_in(period, FALSE)),
               sprintf("AND run_id IN %s", vimpact:::sql_in(run_id_list, FALSE)),
               "GROUP BY touchpoint_id, run_id, country, year")
  
  # extract aggregated data
  print("Extracting data...")
  d <- DBI::dbGetQuery(annex, sql)
  # add disease model
  d <- merge(d, touchpoint, by.x = "touchpoint_id", by.y = "id")
  
  # add country meta
  d <- d %>% left_join(meta, by = "country")
  
  d
}