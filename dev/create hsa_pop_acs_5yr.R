# create hsa_pop_acs_15_yr

# Use the age and sex table?
library(tidyverse)
library(stringr)

acs <- read_csv("./dev/ACS_raw/ACS_2015_5YR/ACS_15_5YR_B01001_with_ann.csv", skip = 1)

acs_long <- acs %>%
    mutate(zip = as.integer(Id2)) %>%
    select(-Id, -Id2, -Geography) %>%
    gather("colname", "pop", -zip) %>%
    filter(!str_detect(colname, "Margin of Error|Total|:$|Id$|Geography$")) %>%
    mutate(colname = str_replace_all(colname, "Estimate;|-|years", "")) %>%
    separate(colname, c("sex", "age"), ":") %>%
    mutate_at(c("age", "sex"), trimws) %>%
    mutate(sex = ifelse(sex == "Male", "M", "F")) %>%
    mutate(agegrp = case_when(
               str_detect(age, "Under 5") ~ "00_05",
               str_detect(age, "5 to 9") ~ "05_09",
               str_detect(age, "^15|^18") ~ "15_19",
               str_detect(age, "^20|^21|^22") ~ "20_24",
               str_detect(age, "^60|^62") ~ "60_64",
               str_detect(age, "^65|^67") ~ "65_69",
               str_detect(age, "^85") ~ "85_110",
               T ~ str_replace(age, " to ", "_"))) %>%
    mutate(agesex_grp = paste(sex, agegrp, sep = "_")) %>%
    mutate(year = 2015L, source = "ACS 5YR") %>%
    select(zip, year, source, agesex_grp, pop)


hsa_pop_acs_14_5yr <- acs_long %>%
    left_join(crosswalk, by = c("zip" = "zipcode13")) %>%
    group_by(year, source, hsanum, hsacity, hsastate, agesex_grp) %>%
    summarise(hsa_pop = sum(pop)) %>%
    ungroup()


hsa_pop_acs_14_5yr %>%
    filter(!hsacity == "Berlin") %>% # filter out Berlin NH HSA
    write_csv("./dev/output tables/hsa_pop_acs_14_5YR.csv")







