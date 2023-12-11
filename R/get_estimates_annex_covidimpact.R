## 
grepv <- function(patterns, value, is_and = TRUE) {
  stopifnot(length(patterns) >= 1)
  j <- rep(0, length(value))
  
  for(i in patterns) {
    j <- j + grepl(i, value)
  }
  
  if(is_and) {
    v <- j == length(patterns)
  } else {
    v <- j >= 1
  }
  return(v)
}

## Get mean and quantile ranges
get_stats <- function(d, group, get_all_quantiles = TRUE){
  
  if(get_all_quantiles == TRUE){
    b1 <- 
      d %>%
      group_by_at(group) %>%
      summarise(across(matches("deaths|dalys|cases"), 
                       list(mid = ~ mean(., na.rm = TRUE), 
                            med =  ~ quantile(., 0.50, na.rm = TRUE),
                            `95_lo` = ~ quantile(., 0.025, na.rm = TRUE), 
                            `95_hi` = ~ quantile(., 0.975, na.rm = TRUE), 
                            `80_lo` = ~ quantile(., 0.1, na.rm = TRUE), 
                            `80_hi` = ~ quantile(., 0.9, na.rm = TRUE), 
                            `50_lo` = ~ quantile(., 0.25, na.rm = TRUE), 
                            `50_hi` = ~ quantile(., 0.75, na.rm = TRUE))), .groups = "keep") 
    
  }else{
    b1 <- 
      d %>%
      group_by_at(group) %>%
      summarise(across(matches("deaths|dalys|cases"), 
                       list(q1 = ~ quantile(., 0.025, na.rm = TRUE), 
                            q3 = ~ quantile(., 0.975, na.rm = TRUE))), .groups = "keep") 
  }
  
  return(b1)
}

###############################################################################
### extract estimates 202210covidimpact touch #################################
###############################################################################
##' Get aggregate data
##' this function provides aggregated burden and impact estimates for given set of countries
aggregate_annex <- function(annex, con,
                            schema_name = "_202210covidimpact_default_update",
                            table = "cross_all", 
                            period = 2000:2030,
                            short_run = FALSE){
  # country classifiers
  meta <- DBI::dbGetQuery(con, "SELECT country, gavi74, who_region 
                          FROM country_metadata WHERE touchstone = '201910gavi-1'")
  touchpoint <- DBI::dbGetQuery(annex, sprintf(
    "SELECT * FROM %s.touchpoint", schema_name))
  # list col names
  cols <- names(DBI::dbGetQuery(annex, sprintf("SELECT * FROM %s.%s LIMIT 1", schema_name, table)))
  # find burden and impact cols
  outcome_cols <- cols[grepv(c("deaths", "cases", "dalys"), cols, FALSE) & !grepl("rate", cols)]
  # make up cols to be aggregated
  sql_cols <-  paste0("sum(", outcome_cols, ") AS ", outcome_cols)
  
  if(!short_run)run_id_list <- 1:200 else run_id_list <- 1:10
  
  if (!(table == "intervention_all")){
    
    sql <- paste(sprintf("SELECT touchpoint_id, run_id, country, %s", paste(sql_cols, collapse = ",")),
                 sprintf("FROM %s.%s", schema_name, table),
                 sprintf("WHERE year IN %s", vimpact:::sql_in(period, FALSE)),
                 sprintf("AND run_id IN %s", vimpact:::sql_in(run_id_list, FALSE)),
                 "GROUP BY touchpoint_id, run_id, country")
    
    # extract aggregated data
    print("Extracting data...")
    d <- DBI::dbGetQuery(annex, sql)
    # add disease model
    d <- merge(d, touchpoint, by.x = "touchpoint_id", by.y = "id")
  } else {
    
    sql <- paste(sprintf("SELECT modelling_group, disease, run_id, country, %s", paste(sql_cols, collapse = ",")),
                 sprintf("FROM %s.%s", schema_name, table),
                 sprintf("WHERE year IN %s", vimpact:::sql_in(period, FALSE)),
                 sprintf("AND run_id IN %s", vimpact:::sql_in(run_id_list, FALSE)),
                 "GROUP BY modelling_group, disease, run_id, country")
    
    # extract aggregated data
    print("Extracting data...")
    d <- DBI::dbGetQuery(annex, sql)
  }
  # add country meta
  d <- d %>% left_join(meta, by = "country")
  print("Summarising data...")
  dat <- list()
  ## by country
  dat[["iso"]] <- list(
    by_disease = get_stats(d, c("disease", "country")),
    by_model = get_stats(d, c("modelling_group", "disease", "country"))
  )
  
  ## by who_region
  d2 <- d %>%
    group_by(modelling_group, disease, who_region, run_id) %>%
    summarise(across(matches("deaths|dalys|cases"), sum, na.rm = TRUE), .groups = "keep")
  
  dat[["who_region"]] <- list(
    by_disease = get_stats(d2, c("disease", "who_region")),
    by_model = get_stats(d2, c("modelling_group", "disease", "who_region"))
  )
  ## by gavi74 or not
  d2 <- d %>%
    group_by(modelling_group, disease, gavi74, run_id)%>%
    summarise(across(matches("deaths|dalys|cases"), sum, na.rm = TRUE), .groups = "keep")
  
  dat[["gavi74"]] <- list(
    by_disease = get_stats(d2, c("disease", "gavi74")),
    by_model = get_stats(d2, c("modelling_group", "disease", "gavi74"))
  )
  ## global
  d2 <- d %>%
    group_by(modelling_group, disease, run_id) %>%
    summarise(across(matches("deaths|dalys|cases"), sum, na.rm = TRUE), .groups = "keep")
  
  dat[["global"]] <- list(
    by_disease = get_stats(d2, c("disease")),
    by_model = get_stats(d2, c("modelling_group", "disease"))
  )
  
  return(dat)
}

##' this function compares different scenario_types for given set of countries
##' difference = base - focal
aggregate_annex2 <- function(annex, con,
                             schema_base = "_202210covidimpact_default_nocovid",
                             schema_focal = "_202210covidimpact_default_update",
                             table = "cross_all", 
                             exclude_catchup = TRUE,
                             period = 2000:2030,
                             short_run = FALSE){
  # country classifiers
  meta <- DBI::dbGetQuery(con, "SELECT country, gavi74, who_region 
                          FROM country_metadata WHERE touchstone = '201910gavi-1'")
  touchpoint_base <- DBI::dbGetQuery(annex, sprintf(
    "SELECT * FROM %s.touchpoint", schema_base))
  touchpoint_focal <- DBI::dbGetQuery(annex, sprintf(
    "SELECT * FROM %s.touchpoint", schema_focal))
  # list col names
  cols_base <- names(DBI::dbGetQuery(annex, sprintf("SELECT * FROM %s.%s LIMIT 1", schema_base, table)))
  cols_focal <- names(DBI::dbGetQuery(annex, sprintf("SELECT * FROM %s.%s LIMIT 1", schema_focal, table)))
  
  if (!(table == "intervention_all")){
    # find burden and impact cols
    if(!exclude_catchup){
      outcome_cols_base <- cols_base[grepv(c("deaths", "cases", "dalys"), cols_base, FALSE) & grepl("impact", cols_base)]
      outcome_cols_focal <- cols_focal[grepv(c("deaths", "cases", "dalys"), cols_focal, FALSE) & grepl("impact", cols_focal)]
    } else {
      outcome_cols_base <- cols_base[grepv(c("deaths", "cases", "dalys"), cols_base, FALSE) & grepl("impact", cols_base) & !grepl("catchup", cols_base)]
      outcome_cols_focal <- cols_focal[grepv(c("deaths", "cases", "dalys"), cols_focal, FALSE) & grepl("impact", cols_focal) & !grepl("catchup", cols_focal)]
    }
    
    # make up cols to be aggregated
    sql_cols_base <-  paste0("sum(", outcome_cols_base, ") AS ", outcome_cols_base)
    sql_cols_focal <-  paste0("sum(", outcome_cols_focal, ") AS ", outcome_cols_focal)
    
    if(!short_run)run_id_list <- 1:200 else run_id_list <- 1:10
    
    sql_base <- paste(sprintf("SELECT touchpoint_id, run_id, country, %s", paste(sql_cols_base, collapse = ",")),
                      sprintf("FROM %s.%s", schema_base, table),
                      sprintf("WHERE year IN %s", vimpact:::sql_in(period, FALSE)),
                      sprintf("AND run_id IN %s", vimpact:::sql_in(run_id_list, FALSE)),
                      "GROUP BY touchpoint_id, run_id, country")
    sql_focal <- paste(sprintf("SELECT touchpoint_id, run_id, country, %s", paste(sql_cols_focal, collapse = ",")),
                       sprintf("FROM %s.%s", schema_focal, table),
                       sprintf("WHERE year IN %s", vimpact:::sql_in(period, FALSE)),
                       sprintf("AND run_id IN %s", vimpact:::sql_in(run_id_list, FALSE)),
                       "GROUP BY touchpoint_id, run_id, country")
    # extract aggregated data
    print("Extracting data...")
    d_base <- DBI::dbGetQuery(annex, sql_base)
    d_focal <- DBI::dbGetQuery(annex, sql_focal)
    
    if(!exclude_catchup){
      if (any(grepl("catchup", names(d_base)))){
        d_base <- d_base %>%
          mutate(deaths_impact = deaths_impact + deaths_catchup_impact,
                 cases_impact = cases_impact + cases_catchup_impact,
                 dalys_impact = dalys_impact + dalys_catchup_impact) %>%
          select(-deaths_catchup_impact, -cases_catchup_impact, -dalys_catchup_impact)
      }
      if(any(grepl("catchup", names(d_focal)))){
        d_focal <- d_focal %>%
          mutate(deaths_impact = deaths_impact + deaths_catchup_impact,
                 cases_impact = cases_impact + cases_catchup_impact,
                 dalys_impact = dalys_impact + dalys_catchup_impact) %>%
          select(-deaths_catchup_impact, -cases_catchup_impact, -dalys_catchup_impact)
      }
      
    }
    # add disease model
    d_base <- merge(d_base, touchpoint_base, by.x = "touchpoint_id", by.y = "id") %>% select(-touchpoint_id, -touchstone)
    d_focal <- merge(d_focal, touchpoint_focal, by.x = "touchpoint_id", by.y = "id") %>% select(-touchpoint_id, - touchstone)
    d <- merge(d_base, d_focal, by = c("modelling_group", "disease", "country", "run_id"), all = TRUE)
    d <- d %>%
      mutate(deaths_impact = deaths_impact.x - deaths_impact.y,
             cases_impact = cases_impact.x - cases_impact.y,
             dalys_impact = dalys_impact.x - dalys_impact.y) %>%
      select(modelling_group, disease, country, run_id, deaths_impact, cases_impact, dalys_impact)
  } else {
    message("YoV view is not relevant - nocovid scenario_type doesn't have PIRI to compare with.")
  }
  # calculate the difference
  # add country meta
  d <- d %>% left_join(meta, by = "country")
  print("Summarising data...")
  dat <- list()
  ## by country
  dat[["iso"]] <- list(
    by_disease = get_stats(d, c("disease", "country")),
    by_model = get_stats(d, c("modelling_group", "disease", "country"))
  )
  
  ## by who_region
  d2 <- d %>%
    group_by(modelling_group, disease, who_region, run_id) %>%
    summarise(across(matches("deaths|dalys|cases"), sum, na.rm = TRUE), .groups = "keep")
  
  
  dat[["who_region"]] <- list(
    by_disease = get_stats(d2, c("disease", "who_region")),
    by_model = get_stats(d2, c("modelling_group", "disease", "who_region"))
  )
  ## by gavi74 or not
  d2 <- d %>%
    group_by(modelling_group, disease, gavi74, run_id)%>%
    summarise(across(matches("deaths|dalys|cases"), sum, na.rm = TRUE), .groups = "keep")
  
  dat[["gavi74"]] <- list(
    by_disease = get_stats(d2, c("disease", "gavi74")),
    by_model = get_stats(d2, c("modelling_group", "disease", "gavi74"))
  )
  ## global
  d2 <- d %>%
    group_by(modelling_group, disease, run_id) %>%
    summarise(across(matches("deaths|dalys|cases"), sum, na.rm = TRUE), .groups = "keep")
  
  dat[["global"]] <- list(
    by_disease = get_stats(d2, c("disease")),
    by_model = get_stats(d2, c("modelling_group", "disease"))
  )
  
  return(dat)
}
