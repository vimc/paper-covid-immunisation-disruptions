ci_all <- function(df){
  df %>% mutate(tidy_out=paste0(nice_num(round(mean_out)), " (95%[", nice_num(round(lo_95)), ", ", 
                                nice_num(round(hi_95)), "]) med=", nice_num(round(median_out))))
}

mitigate_table <- function(d_def, yar = 2020:2030, grouping = c("who_region", "disease")){
  tmp <-  d_def %>%
    select(run_id, country, year, ends_with("catchup_impact"), modelling_group, disease, who_region) %>%
    filter(year %in% yar)
  
  tmp <- tmp %>%
    group_by(run_id, country, year, disease, who_region) %>%
    summarise(deaths_catchup_impact = mean(deaths_catchup_impact, na.rm = TRUE),
              cases_catchup_impact =  mean(cases_catchup_impact, na.rm = TRUE),
              dalys_catchup_impact =  mean(dalys_catchup_impact, na.rm = TRUE))
  
  tmp %>%
    group_by(across(all_of(c(grouping, "run_id")))) %>%
    summarise(deaths_catchup_impact = sum(deaths_catchup_impact),
              cases_catchup_impact = sum(cases_catchup_impact),
              dalys_catchup_impact = sum(dalys_catchup_impact)) %>%
    pivot_longer(names_to = "outcome", values_to = "value", -c(grouping, "run_id"))  %>%
    group_by(across(all_of(c(grouping, "outcome")))) %>%
    summarise(mean_out = mean(value),
              median_out = median(value),
              lo_95 = quantile(value, 0.025),
              lo_80 = quantile(value, 0.1),
              lo_50 = quantile(value, 0.25),
              hi_95 = quantile(value, 0.975),
              hi_80 = quantile(value, 0.9),
              hi_50 = quantile(value, 0.75))
  
}

mitigate_who_table <- function(d_def, outc="deaths", tot){
  t_new <- mitigate_table(d_def, grouping = c("disease", "who_region"))
  
  if(outc=="deaths"){outcom = "deaths_catchup_impact"} else if(outc=="cases"){outcom= "cases_catchup_impact"}else{outcom = "dalys_catchup_impact"}
  tot <- tot %>% filter(outcome==outcom)
  tot <- ci_all(tot)
  
  main <- t_new %>%
    filter(outcome==outcom) %>% 
    rowwise() %>%
    ci_all() %>%
    select(disease, who_region, tidy_out) %>%
    pivot_wider(names_from = who_region, values_from = tidy_out)
  
  t_who <- mitigate_table(d_def, grouping = "who_region")
  t_dis <- mitigate_table(d_def, grouping = "disease")
  
  t_who <- t_who %>% 
    filter(outcome==outcom) %>%
    rowwise() %>%
    ci_all() %>%
    select(who_region, tidy_out) %>%
    pivot_wider(names_from = who_region, values_from = tidy_out) %>%
    mutate(disease = "Total")
  
  t_dis <- t_dis %>%
    filter(outcome==outcom) %>% 
    rowwise() %>%
    ci_all() %>%
    select(disease,  tidy_out) %>%
    rename(Total = tidy_out)
  
  main <- main %>% left_join(t_dis) %>% bind_rows(t_who)
  
  main <- main %>% mutate(Total = ifelse(is.na(Total), tot$tidy_out, Total))
  
  main
}


#---------------
headline_diff_full <- function(d_def=d_def_cross_2030,d_noc=d_noc_cross_2030, yar = 2020:2030, grouping = c("who_region", "disease")){
  tmp_def <-  d_def %>%
    select(run_id, country, year, ends_with("s_impact"), modelling_group, disease, who_region) %>%
    filter(year %in% yar)
  
  tmp_noc <- d_noc %>%
    select(run_id, country, year, ends_with("s_impact"), modelling_group, disease, who_region) %>%
    filter(year %in% yar)
  
  tmp_noc <- tmp_noc %>%
    rename(deaths_noc_impact = deaths_impact, cases_noc_impact = cases_impact, dalys_noc_impact = dalys_impact)
  
  tmp <- left_join(tmp_def, tmp_noc, by = join_by(run_id, country, year, modelling_group, disease, who_region))
  
  tmp <- tmp %>%
    mutate(death_diff = deaths_noc_impact - deaths_impact,
           case_diff = cases_noc_impact - cases_impact,
           daly_diff = dalys_noc_impact - dalys_impact)
  
  tmp <- tmp %>%
    group_by(run_id, country, year, disease, who_region) %>%
    summarise(death_diff = mean(death_diff, na.rm = TRUE),
              case_diff =  mean(case_diff, na.rm = TRUE),
              daly_diff =  mean(daly_diff, na.rm = TRUE))
  
  tmp %>%
    group_by(across(all_of(c(grouping, "run_id")))) %>%
    summarise(death_diff = sum(death_diff),
              case_diff = sum(case_diff),
              daly_diff = sum(daly_diff)) %>%
    pivot_longer(names_to = "outcome", values_to = "value", -c(grouping, "run_id"))  %>%
    group_by(across(all_of(c(grouping, "outcome")))) %>%
    summarise(mean_out = mean(value),
              median_out = median(value),
              lo_95 = quantile(value, 0.025),
              lo_80 = quantile(value, 0.1),
              lo_50 = quantile(value, 0.25),
              hi_95 = quantile(value, 0.975),
              hi_80 = quantile(value, 0.9),
              hi_50 = quantile(value, 0.75))
  
}

headline_diff_full_who <- function(d_def=d_def_cross_2030,d_noc=d_noc_cross_2030,  outc="deaths"){
  t_new <- headline_diff_full(d_def,d_noc, grouping = c("disease", "who_region"))
  
  if(outc=="deaths"){outcom = "death_diff"} else if(outc=="cases"){outcom= "case_diff"}else{outcom = "daly_diff"}
  tot <- headline_diff_full(d_def, d_noc,grouping=NULL)  %>% filter(outcome==outcom)
  tot <- ci_all(tot)
  
  main <- t_new %>%
    filter(outcome==outcom) %>% 
    rowwise() %>%
    ci_all() %>%
    select(disease, who_region, tidy_out) %>%
    pivot_wider(names_from = who_region, values_from = tidy_out)
  
  t_who <- headline_diff_full(d_def, d_noc, grouping = "who_region")
  t_dis <- headline_diff_full(d_def, d_noc, grouping = "disease")
  
  t_who <- t_who %>% 
    filter(outcome==outcom) %>%
    rowwise() %>%
    ci_all() %>%
    select(who_region, tidy_out) %>%
    pivot_wider(names_from = who_region, values_from = tidy_out) %>%
    mutate(disease = "Total")
  
  t_dis <- t_dis %>%
    filter(outcome==outcom) %>% 
    rowwise() %>%
    ci_all() %>%
    select(disease,  tidy_out) %>%
    rename(Total = tidy_out)
  
  main <- main %>% left_join(t_dis) %>% bind_rows(t_who)
  
  main <- main %>% mutate(Total = ifelse(is.na(Total), tot$tidy_out, Total))
  
  main
}
