library(tidyverse)

# get population estimates by age-sex, county and year
pop <- read_csv("./PEP_2016_PEPAGESEX/PEP_2016_PEPAGESEX.csv")
pop_meta <- read_csv("./PEP_2016_PEPAGESEX/PEP_2016_PEPAGESEX_metadata.csv", col_names = c("var","description"))

pop

# total population counts
pop_check <- pop %>% select(matches("est7201.sex0_age999")) %>%
    summarise_all(sum) %>%
    gather("key", "val")




# age sex groups
age_start <- seq(0,85, by =5)
age_end <- c(age_start[-1] - 1, 110)
agesex_groups <- c(paste0("F_", age_start, "_", age_end), paste0("M_", age_start, "_", age_end)) %>%
    str_replace("_(0|5)_", "_0\\1_") %>%
    str_replace("_(4|9)$", "_0\\1")

agesex_groups

pop




pop_long <- gather(pop, "key", "val", 4:ncol(pop)) %>%
    select(-1) %>%
    rename(fips = GEO.id2, county = GEO.displaylabel) %>%
    left_join(pop_meta, by = c("key"="var")) %>%
    filter(!str_detect(description, "Both Sexes|Under 18 years")) %>%
    filter(str_detect(description, "Population Estimate")) %>%
    filter(str_detect(key, "(age([:digit:]+)to([:digit:]+))|(age85plus$)")) %>%
    mutate(key = str_replace(key, "sex1", "sexM")) %>%
    mutate(key = str_replace(key, "sex2", "sexF")) %>%
    mutate(key = str_replace(key, "age85plus", "age85to110")) %>%
    mutate(agesex_grp = str_replace(key, "^[:alnum:]+sex(M|F)_age([:digit:]+)to([:digit:]+)", "\\1_\\2_\\3")) %>%
    mutate(agesex_grp = str_replace(agesex_grp, "_(0|5)_", "_0\\1_")) %>%
    mutate(agesex_grp = str_replace(agesex_grp, "_(4|9)$", "_0\\1")) %>%
    filter(agesex_grp %in% agesex_groups) %>%
    mutate(county = str_replace(county, "County, Maine", "")) %>%
    mutate(year = str_extract(key, "201[:digit:]")) %>%
    mutate(year = as.integer(year)) %>%
    select(fips, county, year, agesex_grp, val) %>%
    rename(pop = val)

# pop_long should have 4032 rows
# 32 agesex groups * 16 counties * 7 years

check <- filter(pop_long, year == 2010)

check$agesex_grp %>% unique() %>% sort()

pop_long$key %>% unique() %>% sort()

# check that age sex group are correct
cbind(sort(unique(check$agesex_grp)), sort(agesex_groups))

unique(pop_long$year)

pop_long %>% group_by(year) %>% summarise(pop = sum(pop)) %>%
    bind_cols(pop_check)
# totals match



# read in county to hsa crosswalk
hsa_cw <- read_csv("./create county to hsa crosswalk/county to hsa crosswalk long.csv") %>%
    rename(fips = county)
hsa_cw
pop_long

hsa_pop <- pop_long %>%
    left_join(hsa_cw, by = c("fips", "year")) %>%
    mutate(hsa_pop = pop*ratio) %>%
    group_by(year, agesex_grp, hsanum, hsacity, hsastate) %>%
    summarise(hsa_pop = sum(hsa_pop)) %>%
    ungroup()

#check that totals match
hsa_pop %>% group_by(year) %>%
    summarise(pop = sum(hsa_pop)) %>%
    mutate(pop = round(pop, 0)) %>%
    bind_cols(pop_check) %>%
    mutate(dif = pop - val)
# hsa totals match very closely

# check against 5 yr estimates?

hsa_pop

write_csv(hsa_pop, "hsa_pop.csv")
