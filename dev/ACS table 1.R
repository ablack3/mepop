
library(tidyverse)

options(stringsAsFactors = F)

# read in Dartmouth zip to HSA crosswalk
crosswalk <-  read.csv("ZipHsaHrr13.csv", stringsAsFactors = F) %>% 
    filter(hsastate %in% c("ME","NH")) %>% 
    select(zipcode13, hsanum, hsacity, hsastate)
# setwd("./ACS_raw")

###########################################################
######## % in poverty #####################################
all_content = readLines("ACS_raw/ACS_14_5YR_S1701_with_ann.csv")
skip_second = all_content[-2]
poverty = read.csv(textConnection(skip_second), header = TRUE, stringsAsFactors = FALSE)
poverty_key = read.csv("ACS_raw/ACS_14_5YR_S1701_metadata.csv", stringsAsFactors = F, col.names = c("var","label"))

names(poverty_key)
poverty_key %>% filter(var %in% c("HC02_EST_VC01", "HC01_EST_VC01"))
# these are the two variables we need to determing % below poverty level by HSA    

head(poverty_key,10)

poverty <-  poverty %>% 
    mutate(num = HC02_EST_VC01, denom = HC01_EST_VC01) %>% 
    select(GEO.id2,  num, denom) %>% 
    mutate(zipcode13 = GEO.id2) %>% 
    left_join(crosswalk, by = "zipcode13" ) %>% 
    filter(hsastate == "ME") 
    
hsa_poverty <- poverty %>% 
    group_by(hsacity) %>% 
    summarise(poverty_pct = sum(num)/sum(denom))

poverty_state_rate = poverty %>%  summarise(poverty_pct = sum(num)/sum(denom)); poverty_state_rate
# 0.1398397 - round to 14.0
hsa_poverty <- rbind(hsa_poverty, c(hsacity = "All", poverty_state_rate))




#############  % without HS education   ################
# S1501
# EDUCATIONAL ATTAINMENT 

all_content <-  readLines("./ACS_raw/ACS_14_5YR_S1501_with_ann.csv")
skip_second <-  all_content[-2]
education <- read.csv(textConnection(skip_second), header = T, stringsAsFactors = FALSE)
education_key <- read.csv("./ACS_raw/ACS_14_5YR_S1501_metadata.csv", col.names = c("var", "desc"), stringsAsFactors = F)

# filter variable list - remove magin of errors and M/F categories
education_key <- education_key %>% filter(!str_detect(desc, "Male|Female|Margin of Error|IMPUTED"))

education_key %>% filter(var %in% c("HC01_EST_VC07","HC01_EST_VC16"))

?dmap_at
as.numeric(NA)

# convert missing to NA
education <- dmap(education, ~ifelse(. %in% c("**","-"), NA, .)) 

max(education$HC03_EST_VC12, na.rm = T)


write.csv(education, "acs.csv")

names(crosswalk)
hsa_edu <- education %>%
    dmap_at(paste0("HC01_EST_VC", c(7, 16)), as.numeric) %>% 
    mutate(denom = HC01_EST_VC07, pct = HC01_EST_VC16/100, num = denom*pct) %>% 
    rename(zip = GEO.id2)  %>% 
    select(zip, denom, pct, num) %>% 
    left_join(crosswalk, by = c("zip" = "zipcode13")) %>% 
    filter(hsastate == "ME") %>% 
    group_by(hsacity) %>% 
    summarize(num = sum(num, na.rm = T), denom = sum(denom, na.rm = T)) %>% 
    mutate(pct_less_HS = 1 - num/denom)
     

# match census summary measure
hsa_edu %>%  summarise(HS_pct = sum(num)/sum(denom))
#.9128677

# % without HS
hsa_edu %>%  summarise(HS_pct = sum(num)/sum(denom))*-1 + 1
# 0.08713228

lessHS_state_rate <- hsa_edu %>%  summarise(pct_less_HS = 1 - sum(num)/sum(denom)); lessHS_state_rate
#0.08713228
# 8.7

hsa_edu <- hsa_edu %>%
    select(hsacity, pct_less_HS) %>% 
    rbind(c("hsacity" = "All", lessHS_state_rate))



### for paper1 table2 
table1 <- full_join(hsa_agesex, hsa_poverty,"hsacity") %>% 
    full_join(hsa_edu, "hsacity")
table1

write.csv(table1, "table1.csv", row.names = F)


# hsacity	pct_age_65_and_up	pct_female	poverty_pct	pct_less_HS
# All	    17.1%	            51.1%	    14.0%	    8.7%
    
