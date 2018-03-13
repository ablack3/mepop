# get total population estimates by HSA
library(tidyverse)


dec <- read_csv("./dev/Census_raw/DEC_10_SF1_P1_with_ann.csv", skip = 1)

cw <- read_csv("./dev/zip to hsa crosswalks/ZipHsaHrr13.csv") %>%
    filter(hsastate == "ME" | (hsastate == "NH" & hsacity %in% c("Dover", "Berlin"))) %>%
    select(zip = zipcode13, hsanum, hsacity, hsastate)

hsa_pop_cen <- dec %>%
    transmute(zip = as.integer(Id2), hsa_pop = Total) %>%
    right_join(cw, ., by = "zip") %>%
    group_by(hsanum, hsacity, hsastate) %>%
    summarize(hsa_pop = sum(hsa_pop)) %>%
    ungroup() %>%
    mutate(year = 2010L, method = "Census")


write_csv(hsa_pop_cen, "./dev/output tables/hsa_pop_cen")

# ~~~~~~~~~~ create hsa pop census by age sex group ~~~~~~~~~~~~

# get population estimates from cen 5 yr by hsa, age, and sex



######### get population by age and sex groups by HSA from census ##################
# Table ID: QT-P1
# Age Groups and Sex: 2010
# Source: U.S. Census Bureau, 2010 Census.


cen <- read_csv("./dev/Census_raw/DEC_10_SF1_QTP1_with_ann.csv", skip = 1)

cen_long <- cen %>%
    mutate(zip = as.integer(Id2)) %>%
    select(-Id, -Id2, -Geography) %>%
    gather("key", "val", -zip)


keys <- cen_long %>%
    filter(grepl("^Number - [Male|Female]", key) & grepl("[years|over]$", key)) %>%
    distinct(key) %>%
    pull(key)

keys <- keys[1:38]



cen_long2 <- cen_long %>%
    filter(key %in% keys) %>%
    separate(key, c("type", "sex", "pop", "age"), sep = "-|;") %>%
    select(zip, sex, age, val) %>%
    mutate(age2 = case_when(
        stringr::str_detect(age, "Under 5") ~ "00_04",
        stringr::str_detect(age, "5 to 9 years") ~ "05_09",
        stringr::str_detect(age, "85|90") ~ "85_110",
        T ~ stringr::str_replace(age, "([:digit:]+) to ([:digit:]+) years", "\\1_\\2")
    )) %>%
    mutate(agesex_grp = paste0(stringr::str_sub(sex, end = 2), "_", age2)) %>%
    mutate(agesex_grp = stringr::str_replace_all(agesex_grp, " ", "")) %>%
    mutate(val = as.numeric(val))


cen_long2$agesex_grp %>% unique()

# save zipcode population file
zip_pop_cen_asgrp <- cen_long2 %>%
    mutate(year = 2010L, source = "Census") %>%
    select(year, source, zip, agesex_grp, pop = val)

write_csv(zip_pop_cen_asgrp, "./dev/output tables/zip_pop_cen_asgrp.csv")


hsa_pop_cen_asgrp <- cen_long2 %>%
    left_join(cw, by = "zip") %>%
    # filter(is.na(hsastate)) # check that all zips have a match in the crosswalk
    group_by(hsanum, hsacity, hsastate, agesex_grp) %>%
    summarise(population = sum(val)) %>%
    ungroup()


write_csv(hsa_pop_cen_asgrp, "./dev/output tables/hsa_pop_cen_asgrp.csv")




