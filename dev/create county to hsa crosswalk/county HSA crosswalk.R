# zip to county
# county to HSA crosswalk

library(tidyverse)
library(stringr)

rm(list = ls())

# read zip to hsa crosswalk
crosswalk <-  read_csv("ZipHsaHrr13.csv", col_types = "iiccccc") %>% 
    filter(hsastate %in% c("ME","NH")) %>% 
    select(zipcode13, hsanum, hsacity, hsastate)


# we will use third quarter crosswalks since population esimates are for July


# read in HUS crosswalks
df_list <- map(2010:2016, function(yr){
    readxl::read_excel(paste0("./create county to hsa crosswalk/COUNTY_ZIP_09", yr, ".xlsx"),
                       col_types = c("text", "text", rep("numeric", 4))) %>% 
        mutate(year = yr)})


df_list2 <- map(df_list, function(df){
    
    df2 <- df %>% 
    rename(county = COUNTY, zip = ZIP) %>% 
    filter(str_detect(county, "^23")) %>% 
    mutate(zip = as.numeric(zip)) %>% 
    inner_join(crosswalk, by = c("zip" = "zipcode13")) %>% 
    group_by(county, hsanum, hsacity, hsastate) %>% 
    summarise(ratio = sum(RES_RATIO)) %>% 
    ungroup() 
    
    names(df2)[names(df2)=="ratio"] <- paste0("ratio", unique(df$year))
    
    df2

})



df <- reduce(df_list2, full_join, by = c("county", "hsanum", "hsacity", "hsastate"))

# round
df <- dmap_if(df, is.numeric, ~round(., digits = 5))

# replace NA with zero
df[is.na(df)] <- 0



df$county %>% unique() %>% length()
# 16 counties

df$hsacity %>% unique() %>% length()
# 34 HSAs

# check that county totals to 1
df %>% 
    select(matches("county|ratio")) %>% 
    group_by(county) %>% 
    summarise_all(sum) 
# should be table of ones or close to it

df_long <- df %>% 
    gather("key", "ratio", ratio2010:ratio2016) %>% 
    mutate(year = str_extract(key, "[0-9]+")) %>% 
    mutate(year = as.integer(year)) %>% 
    select(county, year, hsanum, hsacity, hsastate, ratio)
    
df_long
    
write_csv(df, "./create county to hsa crosswalk/county to hsa crosswalk.csv")
write_csv(df_long, "./create county to hsa crosswalk/county to hsa crosswalk long.csv")

    