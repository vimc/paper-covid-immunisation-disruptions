plot_mitigate_timing <- function(d_def = d_def_cross_2030, d_noc = d_noc_cross_2030, yar=2020:2030,
                                 outc = "deaths", thresh2 = 50){
  tmp <-  d_def %>%
    filter(year %in% yar)
  
  tmp <- tmp %>%
    group_by(run_id, country, year, disease, who_region) %>%
    summarise(deaths_catchup_impact = mean(as.integer(deaths_catchup_impact), na.rm = TRUE),
              cases_catchup_impact =  mean(as.integer(cases_catchup_impact), na.rm = TRUE),
              dalys_catchup_impact =  mean(as.integer(dalys_catchup_impact), na.rm = TRUE),
              deaths_impact =         mean(as.integer(deaths_impact), na.rm = TRUE),
              cases_impact =          mean(as.integer(cases_impact), na.rm = TRUE),
              dalys_impact =          mean(as.integer(dalys_impact), na.rm = TRUE))
  
  
  
  tmp_noc <- d_noc %>%
    filter(year %in% yar)
  
  tmp_noc <- tmp_noc %>%
    group_by(run_id, country, year, disease, who_region) %>%
    summarise(deaths_impact = mean(as.integer(deaths_impact), na.rm = TRUE),
              cases_impact =  mean(as.integer(cases_impact), na.rm = TRUE),
              dalys_impact =  mean(as.integer(dalys_impact), na.rm = TRUE))
  
  tmp_noc <- tmp_noc %>%
    rename(deaths_impact_noc = deaths_impact,
           cases_impact_noc = cases_impact,
           dalys_impact_noc = dalys_impact) %>%
    select(run_id, country, year, disease,ends_with("impact_noc"), who_region)
  
  #--------------------------------------------------------------------------------
  tmp2 <- tmp %>% 
    left_join(tmp_noc, by = join_by(run_id, country, year, disease, who_region)) %>%
    group_by(disease, year, run_id) %>%
    summarise(deaths_catchup_impact = sum(as.integer(deaths_catchup_impact)),
              cases_catchup_impact = sum(as.integer(cases_catchup_impact)),
              dalys_catchup_impact = sum(as.integer(dalys_catchup_impact)),
              
              deaths_excess = sum(as.integer(deaths_impact_noc-deaths_impact)),
              cases_excess = sum(as.integer(cases_impact_noc-cases_impact)),
              dalys_excess = sum(as.integer(dalys_impact_noc-dalys_impact)),) %>%
    group_by(disease, year) 
  
  if(outc=="deaths"){
    tmp2 <- tmp2 %>%
    summarise(catchup_impact_mean = mean(deaths_catchup_impact),
              catchup_impact_median = median(deaths_catchup_impact),
              excess_mean = mean(deaths_excess),
              excess_median = median(deaths_excess),
              excess_lo_95=quantile(deaths_excess, 0.025),
              excess_hi_95=quantile(deaths_excess, 0.975),
              excess_lo_80=quantile(deaths_excess, 0.1),
              excess_hi_80=quantile(deaths_excess, 0.9), 
              excess_lo_50=quantile(deaths_excess, 0.25),
              excess_hi_50=quantile(deaths_excess, 0.75),
              catchup_lo_95=quantile(deaths_catchup_impact, 0.025),
              catchup_hi_95=quantile(deaths_catchup_impact, 0.975),
              catchup_lo_80=quantile(deaths_catchup_impact, 0.1),
              catchup_hi_80=quantile(deaths_catchup_impact, 0.9),
              catchup_lo_50=quantile(deaths_catchup_impact, 0.25),
              catchup_hi_50=quantile(deaths_catchup_impact, 0.75))
  } else if (outc=="cases"){
    tmp2 <- tmp2 %>%
      summarise(catchup_impact_mean = mean(cases_catchup_impact),
                catchup_impact_median = median(cases_catchup_impact),
                excess_mean = mean(cases_excess),
                excess_median = median(cases_excess),
                excess_lo_95=quantile(cases_excess, 0.025),
                excess_hi_95=quantile(cases_excess, 0.975),
                excess_lo_80=quantile(cases_excess, 0.1),
                excess_hi_80=quantile(cases_excess, 0.9), 
                excess_lo_50=quantile(cases_excess, 0.25),
                excess_hi_50=quantile(cases_excess, 0.75),
                catchup_lo_95=quantile(cases_catchup_impact, 0.025),
                catchup_hi_95=quantile(cases_catchup_impact, 0.975),
                catchup_lo_80=quantile(cases_catchup_impact, 0.1),
                catchup_hi_80=quantile(cases_catchup_impact, 0.9),
                catchup_lo_50=quantile(cases_catchup_impact, 0.25),
                catchup_hi_50=quantile(cases_catchup_impact, 0.75))
  } else {
    tmp2 <- tmp2 %>%
      summarise(catchup_impact_mean = mean(dalys_catchup_impact),
                catchup_impact_median = median(dalys_catchup_impact),
                excess_mean = mean(dalys_excess),
                excess_median = median(dalys_excess),
                excess_lo_95=quantile(dalys_excess, 0.025),
                excess_hi_95=quantile(dalys_excess, 0.975),
                excess_lo_80=quantile(dalys_excess, 0.1),
                excess_hi_80=quantile(dalys_excess, 0.9), 
                excess_lo_50=quantile(dalys_excess, 0.25),
                excess_hi_50=quantile(dalys_excess, 0.75),
                catchup_lo_95=quantile(dalys_catchup_impact, 0.025),
                catchup_hi_95=quantile(dalys_catchup_impact, 0.975),
                catchup_lo_80=quantile(dalys_catchup_impact, 0.1),
                catchup_hi_80=quantile(dalys_catchup_impact, 0.9),
                catchup_lo_50=quantile(dalys_catchup_impact, 0.25),
                catchup_hi_50=quantile(dalys_catchup_impact, 0.75))
  }
  
  tmp2 <- tmp2  %>% mutate(keepin = any(abs(excess_median)>thresh2)) %>%
    filter(keepin)
  
  tmp2 %>%
    ggplot()+
    aes(x = year, fill = disease, colour = disease)+
    geom_ribbon(aes(ymin = excess_lo_95, ymax = excess_hi_95), alpha = 0.3)+
    geom_ribbon(aes(ymin = excess_lo_80, ymax = excess_hi_80), alpha = 0.3)+
    geom_ribbon(aes(ymin = excess_lo_50, ymax = excess_hi_50), alpha = 0.3)+
    geom_line(aes(y = excess_median), alpha=0.8, size=2)+
    geom_ribbon(aes(ymin = catchup_lo_95, ymax = catchup_hi_95), alpha = 0.2, fill = "black", colour = "black")+
    geom_ribbon(aes(ymin = catchup_lo_80, ymax = catchup_hi_80), alpha = 0.2, fill = "black", colour = "black")+
    geom_ribbon(aes(ymin = catchup_lo_50, ymax = catchup_hi_50), alpha = 0.2, fill = "black", colour = "black")+
    geom_line(aes(y = catchup_impact_median), alpha = 0.5, colour = "black", size = 2)+
    facet_wrap(.~disease, scales = "free_y", ncol = 2)+
    theme_minimal()+
    scale_fill_manual(values = disease_palette(unique(tmp2$disease))$pal, aesthetics = c("fill"))+
    scale_colour_manual(values = darken(disease_palette(unique(tmp2$disease))$pal, factor=1.5))+
    labs(x = "Year", y = paste("Excess ", R.utils::capitalize(outc), "/ ",  R.utils::capitalize(outc), " mitigated"))+
    theme(legend.position = "none", axis.text.x = element_text(angle=90, hjust=1), text = element_text(size=18))+
    geom_hline(yintercept = 0, colour = "grey50")+
    scale_x_continuous(breaks = 2020:2030, minor_breaks = 2020:2030)
}

#-------------

plot_mitigate_timing_95 <- function(d_def = d_def_cross_2030, d_noc = d_noc_cross_2030, yar=2020:2030,
                                 outc = "deaths", thresh2 = 50){
  tmp <-  d_def %>%
    filter(year %in% yar)
  
  tmp <- tmp %>%
    group_by(run_id, country, year, disease, who_region) %>%
    summarise(deaths_catchup_impact = mean(as.integer(deaths_catchup_impact), na.rm = TRUE),
              cases_catchup_impact =  mean(as.integer(cases_catchup_impact), na.rm = TRUE),
              dalys_catchup_impact =  mean(as.integer(dalys_catchup_impact), na.rm = TRUE),
              deaths_impact =         mean(as.integer(deaths_impact), na.rm = TRUE),
              cases_impact =          mean(as.integer(cases_impact), na.rm = TRUE),
              dalys_impact =          mean(as.integer(dalys_impact), na.rm = TRUE))
  
  
  
  tmp_noc <- d_noc %>%
    filter(year %in% yar)
  
  tmp_noc <- tmp_noc %>%
    group_by(run_id, country, year, disease, who_region) %>%
    summarise(deaths_impact = mean(as.integer(deaths_impact), na.rm = TRUE),
              cases_impact =  mean(as.integer(cases_impact), na.rm = TRUE),
              dalys_impact =  mean(as.integer(dalys_impact), na.rm = TRUE))
  
  tmp_noc <- tmp_noc %>%
    rename(deaths_impact_noc = deaths_impact,
           cases_impact_noc = cases_impact,
           dalys_impact_noc = dalys_impact) %>%
    select(run_id, country, year, disease,ends_with("impact_noc"), who_region)
  
  #--------------------------------------------------------------------------------
  tmp2 <- tmp %>% 
    left_join(tmp_noc, by = join_by(run_id, country, year, disease, who_region)) %>%
    group_by(disease, year, run_id) %>%
    summarise(deaths_catchup_impact = sum(as.integer(deaths_catchup_impact)),
              cases_catchup_impact = sum(as.integer(cases_catchup_impact)),
              dalys_catchup_impact = sum(as.integer(dalys_catchup_impact)),
              
              deaths_excess = sum(as.integer(deaths_impact_noc-deaths_impact)),
              cases_excess = sum(as.integer(cases_impact_noc-cases_impact)),
              dalys_excess = sum(as.integer(dalys_impact_noc-dalys_impact)),) %>%
    group_by(disease, year) 
  
  if(outc=="deaths"){
    tmp2 <- tmp2 %>%
      summarise(catchup_impact_mean = mean(deaths_catchup_impact),
                catchup_impact_median = median(deaths_catchup_impact),
                excess_mean = mean(deaths_excess),
                excess_median = median(deaths_excess),
                excess_lo_95=quantile(deaths_excess, 0.025),
                excess_hi_95=quantile(deaths_excess, 0.975),
                catchup_lo_95=quantile(deaths_catchup_impact, 0.025),
                catchup_hi_95=quantile(deaths_catchup_impact, 0.975))
  } else if (outc=="cases"){
    tmp2 <- tmp2 %>%
      summarise(catchup_impact_mean = mean(cases_catchup_impact),
                catchup_impact_median = median(cases_catchup_impact),
                excess_mean = mean(cases_excess),
                excess_median = median(cases_excess),
                excess_lo_95=quantile(cases_excess, 0.025),
                excess_hi_95=quantile(cases_excess, 0.975),
                catchup_lo_95=quantile(cases_catchup_impact, 0.025),
                catchup_hi_95=quantile(cases_catchup_impact, 0.975))
  } else {
    tmp2 <- tmp2 %>%
      summarise(catchup_impact_mean = mean(dalys_catchup_impact),
                catchup_impact_median = median(dalys_catchup_impact),
                excess_mean = mean(dalys_excess),
                excess_median = median(dalys_excess),
                excess_lo_95=quantile(dalys_excess, 0.025),
                excess_hi_95=quantile(dalys_excess, 0.975),
                catchup_lo_95=quantile(dalys_catchup_impact, 0.025),
                catchup_hi_95=quantile(dalys_catchup_impact, 0.975))
  }
  
  tmp2 <- tmp2  %>% mutate(keepin = any(abs(excess_median)>thresh2)) %>%
    filter(keepin)
  
 p <-  tmp2 %>%
    ggplot()+
    aes(x = year, fill = disease, colour = disease)+
    geom_ribbon(aes(ymin = excess_lo_95, ymax = excess_hi_95), alpha = 0.3)+
    geom_line(aes(y = excess_median), alpha=0.8, size=2)+
    geom_ribbon(aes(ymin = catchup_lo_95, ymax = catchup_hi_95), alpha = 0.2, fill = "black", colour = "black")+
    geom_line(aes(y = catchup_impact_median), alpha = 0.5, colour = "black", size = 2)+
    facet_wrap(.~disease, scales = "free_y", ncol = 2)+
    theme_minimal()+
    scale_fill_manual(values = disease_palette(unique(tmp2$disease))$pal, aesthetics = c("fill"))+
    scale_colour_manual(values = darken(disease_palette(unique(tmp2$disease))$pal, factor=1.5))+
    labs(x = "Year", y = paste("Excess ", R.utils::capitalize(outc), "/ ",  R.utils::capitalize(outc), " mitigated"))+
    theme(legend.position = "none", axis.text.x = element_text(angle=90, hjust=1), text = element_text(size=18))+
    geom_hline(yintercept = 0, colour = "grey50")+
    scale_x_continuous(breaks = 2020:2030, minor_breaks = 2020:2030)
 
 tag_facet2(p,x=Inf,y=Inf, hjust=1, tag_pool = c("", "Ribbon represents 95%CrI, line is median", "", ""), open = "", close="")
}
