# get population estimates from acs 5 yr by hsa, age, and sex
# lets use the 2015 5yr estimates


######### get population by age and sex groups by HSA from ACS ##################
# B01001: SEX BY AGE
# Universe: Total population

library(tidyverse)
library(stringr)



crosswalk <-  read.csv("./dev/zip to hsa crosswalks/ZipHsaHrr13.csv", stringsAsFactors = F) %>%
    filter(hsastate %in% c("ME","NH")) %>%
    select(zipcode13, hsanum, hsacity, hsastate)



all_content <-  readLines("./dev/ACS_raw/ACS_2015_5YR/ACS_15_5YR_B01001_with_ann.csv")
acs <-  read.csv(textConnection(all_content[-1]), header = TRUE, stringsAsFactors = FALSE)

sum(acs$Estimate..Total.)
# 1329174
sum(acs$Estimate..Male.)
# 650053
sum(acs$Estimate..Female.)
# 679121


names(acs) <- str_replace_all(names(acs), "[:punct:]", "")

acs <- select(acs, -matches("MarginofError"))

# age sex groups
age_start <- seq(0,85, by =5)
age_end <- c(age_start[-1] - 1, 110)
agesex_groups <- c(paste0("F_", age_start, "_", age_end), paste0("M_", age_start, "_", age_end)) %>%
    str_replace("_(0|5)_", "_0\\1_") %>%
    str_replace("_(4|9)$", "_0\\1")


# shorten variable names
names(acs) <- str_replace_all(names(acs), "Estimate|years.*", "") %>%
    str_replace("^(F|M)[:alpha:]+([:digit:]+)[:alpha:]+([:digit:]+)", "\\1_\\2_\\3") %>%
    str_replace("^(F|M)[:alpha:]+Under5", "\\1_0_4") %>%
    str_replace("^(F|M)[:alpha:]+([:digit:]+)", "\\1_\\2") %>%
    str_replace("^(F|M)_85", "\\1_85_110") %>%
    str_replace("_(0|5)_", "_0\\1_") %>%
    str_replace("_(4|9)$", "_0\\1")

# make sure all the zipcodes are in the hsa crosswalk
all(acs$Id2 %in% crosswalk$zipcode13)

names(acs)

acs2 <- acs %>%
    as.tibble() %>%
    rename(zip = Id2) %>%
    select(zip, matches("^(M|F)_")) %>%
    gather("agesex_grp", "pop", -zip) %>%
    mutate(agesex_grp = str_replace(agesex_grp, "^(M|F)_(15_17|18_19)$", "\\1_15_19")) %>%
    mutate(agesex_grp = str_replace(agesex_grp, "^(M|F)_(20|21|22_24)$", "\\1_20_24")) %>%
    mutate(agesex_grp = str_replace(agesex_grp, "^(M|F)_(60_61|62_64)$", "\\1_60_64")) %>%
    mutate(agesex_grp = str_replace(agesex_grp, "^(M|F)_(65_66|67_69)$", "\\1_65_69")) %>%
    group_by(zip, agesex_grp) %>%
    summarise(pop = sum(pop)) %>%
    ungroup()
    # filter(agesex_grp %in% agesex_groups)

acs2
names(crosswalk)
hsa_acs_pop <- acs2 %>%
    left_join(crosswalk, by = c("zip" = "zipcode13")) %>%
    group_by(hsanum, hsacity, hsastate, agesex_grp) %>%
    summarise(pop = sum(pop)) %>%
    ungroup()

# should have 34 HSAs * 36 agesex groups = 1224 rows
36*34 == nrow(hsa_acs_pop)
hsa_acs_pop %>% group_by(hsacity) %>% summarise(nrow = n()) %>%  print(n=34)

hsa_acs_pop


write_csv(hsa_acs_pop, "hsa_pop_acs5yr_2014.csv")

