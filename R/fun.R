fun_join_fvps_df_iu <- function(fvps, df, RI_scaling, dis_subset = NULL, ctry_subset = NULL){
  
  #filter if needed
  if(!is.null(dis_subset )){fvps <- fvps %>% filter(disease %in% dis_subset); df <- df %>% filter(disease %in% dis_subset)}
  if(!is.null(ctry_subset)){fvps <- fvps %>% filter(country %in% ctry_subset); df <- df %>% filter(country %in% ctry_subset)}
  
  # routine-intensified
  
  test <- df %>% filter(activity_type == "routine") %>%
    mutate(activity_type = "routine-intensified") %>%
    mutate(deaths_averted_rate = deaths_averted_rate * RI_scaling,
           cases_averted_rate = cases_averted_rate * RI_scaling,
           dalys_averted_rate = dalys_averted_rate * RI_scaling)
  
  #deal with multiple vaccines for routine-intensified
  if(test %>% filter(activity_type == "routine-intensified") %>% pull(vaccine) %>% unique() %>% length()>1){
    test <- test %>% group_by(across(!contains("mod|rate"))) %>% summarise(across(contains("rate"), ~mean(.)))
    
    test <- test %>% mutate(vaccine = ifelse(activity_type =="routine-intensified" & disease=="Measles", "Measles", vaccine))
    test <- test %>% mutate(vaccine = ifelse(activity_type =="routine-intensified" & disease=="Rubella", "Rubella", vaccine))
  }
  
  df <- df %>% bind_rows(test)
  
  
  # join em up
  fvps_df <- fvps %>% full_join(df, by = c("country", "disease", "vaccine", "activity_type"))
  
  fvps_df <- fvps_df %>% filter(fvps_adjusted>0)
  
  # fill the empties
  fvps_df <- fvps_df %>%
    group_by(country, disease, vaccine, activity_type, run_id) %>%
    mutate(deaths_averted_rate = ifelse(is.na(deaths_averted_rate),
                                        max( mean(deaths_averted_rate, na.rm = TRUE), 0, na.rm = TRUE) ,
                                        deaths_averted_rate),
           cases_averted_rate = ifelse(is.na(cases_averted_rate),
                                       max( mean(cases_averted_rate, na.rm = TRUE), 0, na.rm = TRUE),
                                       cases_averted_rate),
           dalys_averted_rate = ifelse(is.na(dalys_averted_rate),
                                       max( mean(dalys_averted_rate, na.rm = TRUE), 0, na.rm = TRUE),
                                       dalys_averted_rate))
  
  #remove the NA
  fvps_df <- fvps_df  %>% distinct()
  
  # do some IU
  fvps_df <-  fvps_df %>%
    mutate(deaths = deaths_averted_rate * fvps_adjusted,
           cases =  cases_averted_rate *  fvps_adjusted,
           dalys =  dalys_averted_rate *  fvps_adjusted)
  
  #return
  return(fvps_df)
}



# -> append DTP3
append_dtp <- function(df, con){
  country_names <- DBI::dbGetQuery(con, "SELECT distinct country, name AS country_name
                                   FROM country_metadata
                                   JOIN country
                                   ON country.id = country")
  df2 <- readRDS("source/dtp_bcg_impact_ratios.rds") %>%
    filter(vaccine == "DTP3") %>%
    rename(deaths_averted_rate = impact_factor) %>%
    select(disease, vaccine, country, deaths_averted_rate) %>%
    as.data.frame()
  if (!("XK" %in% df2$country)){
    df2 <- df2 %>%
      filter(country == "SRB") %>%
      mutate(country = "XK") %>%
      bind_rows(df2)
  }
  if (!("PSE" %in% df2$country)){
    df2 <- df2 %>%
      filter(country == "LBN") %>%
      mutate(country = "PSE") %>%
      bind_rows(df2)
  }
  df2 <- df2 %>%
    right_join(country_names, by = "country") %>%
    mutate(modelling_group = "IA2030-Carter",
           activity_type = "routine") %>%
    bind_rows(df) %>%
    mutate(run_id = -1)
  
}

tidy_qi <- function(vec, roundamt = 0){
  paste0(prettyNum(round(mean(vec, na.rm = TRUE), roundamt),big.mark = ","), 
         " (95%[",
         prettyNum(round(quantile(vec, 0.025, na.rm = TRUE), roundamt), big.mark=","), ", ",
         prettyNum(round(quantile(vec, 0.975, na.rm = TRUE), roundamt),big.mark = ","), "])")
}
