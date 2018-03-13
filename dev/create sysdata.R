#  create sysdata

library(tidyverse)
list.files("./output tables")

# read in tables
hsa_pop_hud <- read_csv("./dev/output tables/hsa_pop.csv", col_types = "iciccd")
# hsa_pov <- read_csv("./output tables/hsa_pov.csv")
hsa_pop_cen <- read_csv("./dev/output tables/hsa_pop_cen_asgrp.csv")
# hsa_pop_cen_totals <- read_csv("./dev/output tables/hsa_pop_cen.csv")
hsa_pop_acs <- read_csv("./dev/output tables/hsa_pop_acs_14_5YR.csv")
zip_pop_cen <- read_csv("./dev/output tables/zip_pop_cen_asgrp.csv")

### standardize the tables ########

hsa_pop_hud <- hsa_pop_hud %>%
    filter(hsastate == "ME" | hsacity == "Dover") %>%
    mutate(source = "Census+HUD") %>%
    rename(pop = hsa_pop) %>%
    select(year, source, hsanum, hsacity, hsastate, agesex_grp, pop)

hsa_pop_acs <- hsa_pop_acs %>%
    filter(hsastate == "ME" | hsacity == "Dover") %>%
    rename(pop = hsa_pop) %>%
    select(year, source, hsanum, hsacity, hsastate, agesex_grp, pop)

hsa_pop_cen <- hsa_pop_cen %>%
    filter(hsastate == "ME" | hsacity == "Dover") %>%
    mutate(year = 2010L, source = "Census") %>%
    rename(pop = population) %>%
    select(year, source, hsanum, hsacity, hsastate, agesex_grp, pop)



##############
# testing uniformity
df_list <- lst(hsa_pop_hud, hsa_pop_acs, hsa_pop_cen)

# how many HSAs does each dataset have
map_int(df_list, ~length(unique(.x$hsacity)))
# all 33, 32 in Maine + Dover NH

# check that NH HSA is dover
map(df_list, ~.x %>%
        filter(hsastate == "NH") %>%
        distinct(hsacity))

# check that names match
map(df_list, names)


################# create package data

devtools::use_data(hsa_pop_cen,
                   hsa_pop_acs,
                   hsa_pop_hud,
                   zip_pop_cen,
                   overwrite = T)

# write csv files
walk2(df_list, names(df_list), ~write_csv(.x, paste0("./csv/", .y, ".csv")))
write_csv(zip_pop_cen, "./csv/zip_pop_cen.csv")

# create sas files
walk2(df_list, names(df_list), ~haven::write_sas(.x, paste0("./sas/", .y, ".sas7bdat")))
haven::write_sas(zip_pop_cen, "./sas/zip_pop_cen.sas7bdat")
