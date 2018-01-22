# tests
library(tidyverse)
devtools::load_all()
df_list <- lst(hsa_pop_acs, hsa_pop_cen, hsa_pop_hud)


map(df_list, ~.x %>%
        group_by(year) %>%
        summarise(pop = sum(pop)))








