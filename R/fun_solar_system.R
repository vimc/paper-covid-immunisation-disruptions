fun_solar_system <- function(d_noc = d_noc_cross_2030, d_def = d_def_cross_2030,
                             who = TRUE, yar = 2020:2030, outc="deaths", determ=FALSE){
  
  noc <- d_noc %>%
    select(run_id, country, year, deaths_default, cases_default, dalys_default, modelling_group, disease, who_region)
  
  def <- d_def %>%
    select(run_id, country, year, deaths_default, deaths_catchup, cases_default, cases_catchup, dalys_default, dalys_catchup, modelling_group, disease, who_region)
  
  if(outc=="deaths"){
    def <- def  %>%
      rename(outc_covid = deaths_default,
             outc_catchup = deaths_catchup)
    noc <- noc  %>%
      rename(outc_nocovid = deaths_default)
  } else if(outc =="cases"){
    def <- def  %>%
      rename(outc_covid = cases_default,
             outc_catchup = cases_catchup)
    noc <- noc  %>%
      rename(outc_nocovid = cases_default)
  } else {
    def <- def  %>%
      rename(outc_covid = dalys_default,
             outc_catchup = dalys_catchup)
    noc <- noc  %>%
      rename(outc_nocovid = dalys_default)
    
    outc <- "DALYs"
  }
  
  comb <- left_join(def, noc, by = join_by(run_id, country, year, modelling_group, disease, who_region))
  
  comb <- comb %>% filter(year %in% yar)
  
  comb <- comb %>%
    mutate(outc_diff = outc_covid - outc_nocovid,
           outc_catch = outc_covid - outc_catchup)
  
  comb <- comb %>%
    group_by(disease, run_id, who_region, year, country) %>%
    summarise(outc_diff = median(outc_diff, na.rm = TRUE),
              outc_catch = median(outc_catch, na.rm = TRUE))
  
  if(who){
    comb <- comb %>%
      group_by(disease, who_region, run_id)%>%
      summarise(outc_diff = sum(outc_diff),
                outc_catch = sum(outc_catch)) %>%
      rename(geo = who_region)
  } else {
    comb <- comb %>%
      group_by(disease, country, run_id)%>%
      summarise(outc_diff = sum(outc_diff),
                outc_catch = sum(outc_catch)) %>%
      rename(geo = country)
  }
  
  comb2 <- comb %>% 
    mutate(disease = factor(disease)) %>%
    filter(outc_diff>0, outc_catch>=0) %>%
    mutate(propn = round(outc_catch)/round(outc_diff) * 100) %>%
    mutate(propn = ifelse(is.infinite(propn), NA, ifelse(is.nan(propn), NA, propn))) %>%
    group_by(disease, geo) %>%
    summarise(propn_mean = mean(propn, na.rm=TRUE),
              propn_median = median(propn, na.rm = TRUE),
              propn_sd = sd(propn, na.rm=TRUE),
              outc_diff_median = median(outc_diff),
              outc_diff_95_hi = quantile(outc_diff, 0.975),
              outc_diff_95_lo = quantile(outc_diff, 0.025)) %>%
    mutate(propn_coeff_of_var = propn_sd/propn_mean)
  
  comb2 <- comb2 %>%
    mutate(propn_median_disc = case_when(propn_median>=0 & propn_median<10 ~ "0-10%",
                                         propn_median>=10 & propn_median<20 ~ "10-20%",
                                         propn_median>=20 & propn_median<30 ~ "20-30%",
                                         propn_median>=30 & propn_median<40 ~ "30-40%",
                                         propn_median>=40 & propn_median<50 ~ "40-50%",
                                         propn_median>=50 & propn_median<60 ~ "50-60%",
                                         propn_median>=60 & propn_median<70 ~ "60-70%",
                                         propn_median>=70 & propn_median<80 ~ "70-80%",
                                         propn_median>=80 & propn_median<90 ~ "80-90%",
                                         propn_median>=90 & propn_median<100 ~ "90-100%",
                                         propn_median>=100 & propn_median<200 ~ "100-200%",
                                         propn_median>=200  ~ "200+%",
                                         TRUE ~ NA)) %>%
    mutate(propn_median_disc = factor(propn_median_disc, levels = c("0-10%", "10-20%","20-30%","30-40%","40-50%",
                                                                    "50-60%","60-70%","70-80%","80-90%","90-100%","100-200%","200+%")))
  
  if(!determ){
    p <-  comb2 %>%
      ggplot()+
      aes(x = disease, y = reorder(geo, outc_diff_median), colour = propn_median_disc, alpha = propn_coeff_of_var*100)+
      geom_point(aes(size = outc_diff_95_hi), shape=1, alpha = 1, stroke=1.5)+
      geom_point(aes(size = outc_diff_median) ) +
      geom_point(aes(size = outc_diff_95_lo), shape=1, alpha = 1, stroke=1.5)+
      theme_minimal()+
      scale_size(range = c(5,40), breaks = c(100,500,1000,5000, 10000), labels =c("100", "500", "1,000", "5,000", "10,000"))+
      scale_alpha(range = c(0.9, 0.2))+
      labs(x = "Disease/ Vaccine", y = ifelse(who,"WHO region","Country"), size = paste0("Excess ", outc), 
           colour = "Proportion mitigated", alpha = "Coefficient of variation for proportion (%)")+
      theme(legend.position = "bottom", legend.box = "vertical", text = element_text(size=18))+
      scale_colour_manual(values = darken(met.brewer("Homer1", n = length(unique(comb2$propn_median_disc))), factor = 1.2))+ 
      guides(colour = guide_legend(override.aes = list(size=5)), alpha = guide_legend(override.aes = list(size=5)))
  } else {
    p <-  comb2 %>%
      ggplot()+
      aes(x = disease, y = reorder(geo, outc_diff_median), colour = propn_median_disc)+
      geom_point(aes(size = outc_diff_median) ) +
      theme_minimal()+
      scale_size(range = c(5,40), breaks = c(100,500,1000,5000, 10000), labels =c("100", "500", "1,000", "5,000", "10,000"))+
      labs(x = "Disease/ Vaccine", y = ifelse(who,"WHO region","Country"), size = paste0("Excess ", outc), 
           colour = "Proportion mitigated")+
      theme(legend.position = "bottom", legend.box = "vertical", text = element_text(size=18))+
      scale_colour_manual(values = darken(met.brewer("Homer1", n = length(unique(comb2$propn_median_disc))), factor = 1.2))+ 
      guides(colour = guide_legend(override.aes = list(size=5)))
  }
  p
}