R.utils::sourceDirectory("R", modifiedOnly = FALSE)

options(dplyr.summarise.inform = FALSE)

ctry <- DBI::dbReadTable(con, "country_metadata") 
ctry <- ctry %>% filter(touchstone == max(unique(ctry$touchstone)))
ctry <- ctry %>% mutate(who_region = ifelse(who_region=="PAHO", who_region, gsub("O", "", who_region)))

#-----------------------------------------------------------------------------------------#
d_def_cohort_2030 <- pull_table(annex, con, 
                                schema_name =  "_202210covidimpact_default_update",
                                table = "cohort_all",
                                period = 2019:2030, 
                                short_run=short_run) %>% mutate(who_region = ifelse(who_region=="PAHO", who_region, gsub("O", "", who_region)))

d_noc_cohort_2030 <- pull_table(annex, con, 
                                schema_name =  "_202210covidimpact_default_nocovid",
                                table = "cohort_all",
                                period = 2019:2030, 
                                short_run=short_run) %>% mutate(who_region = ifelse(who_region=="PAHO", who_region, gsub("O", "", who_region)))

d_def_cross_2030 <- pull_table(annex, con, 
                               schema_name =  "_202210covidimpact_default_update",
                               table = "cross_all",
                               period = 2019:2030, 
                               short_run=short_run) %>% mutate(who_region = ifelse(who_region=="PAHO", who_region, gsub("O", "", who_region)))

d_noc_cross_2030 <- pull_table(annex, con, 
                               schema_name =  "_202210covidimpact_default_nocovid",
                               table = "cross_all",
                               period = 2019:2030, 
                               short_run=short_run) %>% mutate(who_region = ifelse(who_region=="PAHO", who_region, gsub("O", "", who_region)))
#-----------------------------------------------------------------------------------------#
#-----------------------------------------------------------------------------------------#
## IU
# get the impact ratios
df <- readRDS("source/native-impact-estimates-post-2017-runs.rds")

df <- df %>% filter(touchstone_src == "202110gavi")

df <- df %>% select(disease, modelling_group, activity_type, vaccine, country, country_name, 
                    deaths_averted_rate, cases_averted_rate, dalys_averted_rate) %>%
  distinct()

df <- df %>%
  rename(cases_averted_rate_central = cases_averted_rate,
         deaths_averted_rate_central = deaths_averted_rate,
         dalys_averted_rate_central = dalys_averted_rate)

#----------------
# uncert
df_uncert <- DBI::dbGetQuery(annex, 
                             "select distinct disease, modelling_group, run_id, vaccine, activity_type, country, deaths_averted_rate, cases_averted_rate, dalys_averted_rate from intervention_all_2021 where deaths_averted_rate is not NULL")

# -> add DTP3
df <- append_dtp(df, con)

# join full uncert
df <- full_join(df, df_uncert, multiple="all")

dis_subset_parm <- NULL
ctry_subset_parm <- NULL

# get model averages
df <- df %>% 
  group_by(disease, vaccine, country, country_name, activity_type, run_id) %>%
  summarise(deaths_averted_rate = mean(deaths_averted_rate, na.rm = TRUE),
            cases_averted_rate = mean(cases_averted_rate, na.rm = TRUE),
            dalys_averted_rate = mean(dalys_averted_rate, na.rm = TRUE))

#get the FVPS
fvps <- bind_rows(lapply(c("default_update", "default_nocovid"),
                         FUN = function(x)vimpact::extract_vaccination_history(con, 
                                                                               touchstone_cov = "202210covidimpactiu",
                                                                               demographic_source ="dds-202208",
                                                                               coverage_scenario_type = x,
                                                                               disease_to_extract = dis_subset_parm,
                                                                               countries_to_extract =  ctry_subset_parm,
                                                                               year_max = 2030)
))

fvps <- fvps %>% mutate(disease = ifelse(is.na(disease), vaccine, disease)) %>%
  mutate(disease = ifelse(disease == "PCV3", "PCV", disease)) %>%
  mutate(disease = ifelse(disease == "Hib3", "Hib", disease)) %>%
  mutate(disease = ifelse(disease == "DTP3", "DTP", disease)) 

# -> duplicate DTP for D, T, and P
fvps <- bind_rows(fvps,
                  fvps[fvps$disease == "DTP", ] %>% mutate(disease = "Diphtheria"),
                  fvps[fvps$disease == "DTP", ] %>% mutate(disease = "Tetanus"),
                  fvps[fvps$disease == "DTP", ] %>% mutate(disease = "Pertussis")
) %>%
  filter(disease != "DTP")

fvps <- fvps %>%
  mutate(scenario_type = ifelse(scenario_type=="default_update",
                                "default_update_catchup",
                                scenario_type)) %>%
  bind_rows(fvps %>% filter(scenario_type %in% c("default_update"),
                            activity_type != "routine-intensified")) 

fvps <- fvps %>% select(scenario_description, country, disease, scenario_type, vaccine, activity_type, year, gender, age, fvps_adjusted)

# then remove routine-intensfied from the update ones
fvps <- fvps %>% filter(!(scenario_type %in% c("default_update") & activity_type %in% "routine-intensified"))

# tidy fvps
fvps <- fvps %>%
  filter(fvps_adjusted >0) %>%
  group_by(scenario_description, country, disease, scenario_type, vaccine, activity_type, year) %>%
  summarise(fvps_adjusted = sum(fvps_adjusted))

#-----------------------------------------------------------------------------------------------------------------------------------------
# presentation prep
df_int <- readRDS("intervention_perspective_impact.rds")
df_cro <- readRDS("cross_sectional_impact.rds")
df_lif <- readRDS("life_time_impact.rds")
df_iu <-  readRDS("combined_data.rds")

df_burd22 <- readRDS("burden_202210covidimpact.rds") 
df_burd21 <- readRDS("burden_202110gavi.rds") 

pop <- vimpact::get_population(con, touchstone_pop = "202210covidimpact-2")
pop <- pop %>% group_by(country, year) %>% summarise(value = sum(value, na.rm = TRUE))
pop <- pop %>% left_join(ctry %>% select(country, who_region), by="country")
pop <- pop %>% left_join(df_burd22 %>% select(country, country_name) %>% distinct(), by="country")

#-----------------------------------------------------------------------------------------------------------------------------------------
# Run report
rmarkdown::render("report.Rmd")



