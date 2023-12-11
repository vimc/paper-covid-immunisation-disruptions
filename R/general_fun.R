#general functions
# unzip all
unzip_fun <- function(name_to_unzip){
  zip::unzip(paste0(name_to_unzip,".zip"))
  
  # get the list of new files
  new_files <-  list.files(pattern = paste0("^",name_to_unzip))
  new_files <- new_files[grep("zip", new_files, invert=TRUE)]
  
  for(fil in new_files){
    tmp <- readRDS(fil)
    assign(gsub(".rds", "", fil), value = tmp ,envir = globalenv()) #assign to variable of same name
  }
  
  #remove the new files
  file.remove(new_files)
  
  return()
}

#puts thousands in a nice format
nice_num <- function(num){
  num <- prettyNum(round(num, 2), big.mark = ",")
}

#combines numbers to give a nicely formatted CrI
cred_int <- function(mean_num, q1, q3){
  paste0( nice_num(mean_num), " (95%CrI[", nice_num(q1), ", ", nice_num(q3), "])")
}

#groups FVPs by country, disease, activity and year
aggregate_fvps <- function(fvps){
  fvps %>%
    group_by(country, disease, activity_type, year) %>%
    summarise(fvps = sum(fvps_adjusted, na.rm = TRUE),
              cohort_size = sum(cohort_size, na.rm = TRUE))
}

#darkens a colour - useful for making things match but stand out on plots
darken <- function(color, factor=1.4){
  col <- col2rgb(color)
  col <- col/factor
  col <- rgb(t(col), maxColorValue=255)
  col
}

ssum <- function(...){
  sum(..., na.rm = TRUE)
}

#Add country metadata
add_country_metadata <- function(dat){
  
  country_metadata <- 
    tbl(con, "country_metadata") %>% 
    left_join(tbl(con, "country"), by = c("country" = "id")) %>%
    filter(grepl("201910", touchstone)) %>% 
    select(country_name = name, country, gavi73, who_region) %>%
    collect()
  
  dat <- dat %>% left_join(country_metadata, by = "country")
  dat <- dat %>%
    mutate(who_region=str_sub(who_region, 1, nchar(who_region) - 1)) ## removing trailing "O" from each level of WHO region (which stands for "office")  
  return(dat)
}
