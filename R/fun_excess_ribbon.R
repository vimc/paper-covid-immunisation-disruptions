fun_excess_ribbon <- function(comb, outcome="deaths", xax="Year", thresh=500, thresh2=50){

  
  comb2 <- comb %>%
    group_by(year, who_region, run_id, disease)
  
  if(outcome=="deaths"){
    comb2 <- comb2%>%summarise(out_diff = sum(death_diff, na.rm = TRUE)) 
    outc <- outcome
  } else if(outcome=="cases") {
    comb2 <- comb2%>%summarise(out_diff = sum(case_diff, na.rm = TRUE))
    outc <- outcome
  } else {
    comb2 <- comb2%>%summarise(out_diff = sum(daly_diff, na.rm = TRUE)) 
    outc <- "DALYs"
  }
  
  comb2 <- comb2%>%
    group_by(year, who_region, disease) %>%
    summarise(mean_diff =      mean(out_diff, na.rm = TRUE),
              median_diff = median(out_diff, na.rm=TRUE),
              diff_95_lo = quantile(out_diff, 0.025, na.rm = TRUE),
              diff_95_hi = quantile(out_diff, 0.975, na.rm = TRUE),
              diff_80_lo = quantile(out_diff, 0.1, na.rm = TRUE),
              diff_80_hi = quantile(out_diff, 0.9, na.rm = TRUE),
              diff_50_lo = quantile(out_diff, 0.25, na.rm = TRUE),
              diff_50_hi = quantile(out_diff, 0.75, na.rm = TRUE)) %>%
    filter(disease != "MenA") 
  
  comb2 <- comb2 %>%
    group_by(who_region, disease) %>%
    mutate(keep = max(abs(median_diff))>thresh2) %>%
    filter(keep)
  
  dis <- comb2 %>% filter(year>2019) %>% pull(disease) %>% unique()
  
  p <- comb2 %>%
    filter(year>2019) %>%
    ggplot()+
    aes(x = year, y = round(median_diff), colour = disease, fill = disease, group = interaction(disease, who_region))+
    geom_line(size=2)+
    geom_ribbon(aes(ymin = round(diff_95_lo), ymax = round(diff_95_hi)), alpha=0.3)+
    geom_ribbon(aes(ymin = round(diff_80_lo), ymax = round(diff_80_hi)), alpha=0.3)+
    geom_ribbon(aes(ymin = round(diff_50_lo), ymax = round(diff_50_hi)), alpha=0.3)+
    geom_hline(yintercept = 0, colour = "grey50")+
    
    facet_wrap(disease~who_region, scales = "free_y")+
    
    scale_fill_manual(values = disease_palette(dis)$pal)+
    scale_colour_manual(values = darken(disease_palette(dis)$pal, factor=1.5))+
    theme_minimal()+
    labs(x = xax, y = paste0("Excess ",outc), fill = "Disease/ Vaccine", colour = "Disease/ Vaccine")+
    scale_x_continuous(breaks=2020:2030, minor_breaks = 2020:2030)+
    theme(axis.text.x = element_text(angle = 90, hjust=0, vjust=1), legend.position = "bottom", text = element_text(size=18))
  
  tag_facet2(p,x=Inf,y=Inf, hjust=1, tag_pool = c("", "Ribbon represents 95%CrI, line is median", "", ""), open = "", close="")
}

tag_facet2 <- function(p, open = "(", close = ")", tag_pool = letters, x = -Inf, y = Inf, 
                       hjust = -0.5, vjust = 1.5, fontface = 2, family = "", ...) {
  
  gb <- ggplot_build(p)
  lay <- gb$layout$layout
  tags <- cbind(lay, label = paste0(open, tag_pool[lay$PANEL], close), x = x, y = y)
  p + geom_text(data = tags, aes_string(x = "x", y = "y", label = "label"), ..., hjust = hjust, 
                vjust = vjust, fontface = fontface, family = family, inherit.aes = FALSE, na.rm=TRUE)
}

fun_excess_ribbon_95 <- function(comb, outcome="deaths", xax="Year", thresh=500, thresh2=50){
  
  
  comb2 <- comb %>%
    group_by(year, who_region, run_id, disease)
  
  if(outcome=="deaths"){
    comb2 <- comb2%>%summarise(out_diff = sum(death_diff, na.rm = TRUE)) 
    outc <- outcome
  } else if(outcome=="cases") {
    comb2 <- comb2%>%summarise(out_diff = sum(case_diff, na.rm = TRUE))
    outc <- outcome
  } else {
    comb2 <- comb2%>%summarise(out_diff = sum(daly_diff, na.rm = TRUE)) 
    outc <- "DALYs"
  }
  
  comb2 <- comb2%>%
    group_by(year, who_region, disease) %>%
    summarise(mean_diff =      mean(out_diff, na.rm = TRUE),
              median_diff = median(out_diff, na.rm=TRUE),
              diff_95_lo = quantile(out_diff, 0.025, na.rm = TRUE),
              diff_95_hi = quantile(out_diff, 0.975, na.rm = TRUE),
              diff_80_lo = quantile(out_diff, 0.1, na.rm = TRUE),
              diff_80_hi = quantile(out_diff, 0.9, na.rm = TRUE),
              diff_50_lo = quantile(out_diff, 0.25, na.rm = TRUE),
              diff_50_hi = quantile(out_diff, 0.75, na.rm = TRUE)) %>%
    filter(disease != "MenA") 
  
  comb2 <- comb2 %>%
    group_by(who_region, disease) %>%
    mutate(keep = max(abs(median_diff))>thresh2) %>%
    filter(keep)
  
  dis <- comb2 %>% filter(year>2019) %>% pull(disease) %>% unique()
  
  p <- comb2 %>%
    filter(year>2019) %>%
    ggplot()+
    aes(x = year, y = round(median_diff), colour = disease, fill = disease, group = interaction(disease, who_region))+
    geom_line(size=2)+
    geom_ribbon(aes(ymin = round(diff_95_lo), ymax = round(diff_95_hi)), alpha=0.3)+
    geom_hline(yintercept = 0, colour = "grey50")+
    
    facet_wrap(disease~who_region, scales = "free_y")+
    
    scale_fill_manual(values = disease_palette(dis)$pal)+
    scale_colour_manual(values = darken(disease_palette(dis)$pal, factor=1.5))+
    theme_minimal()+
    labs(x = xax, y = paste0("Excess ",outc), fill = "Disease/ Vaccine", colour = "Disease/ Vaccine")+
    scale_x_continuous(breaks=2020:2030, minor_breaks = 2020:2030)+
    theme(axis.text.x = element_text(angle = 90, hjust=0, vjust=1), legend.position = "bottom", text = element_text(size=18))
  
  tag_facet2(p,x=Inf,y=Inf, hjust=1, tag_pool = c("", "Ribbon represents 95%CrI, line is median", "", "","", "","", "", ""), open = "", close="")
}
