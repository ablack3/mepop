
# investigate hsa_pop created from county estimates mapped to hsa through HUD crosswalk

library(tidyverse)

rm(list=ls())

hsa_pop <- read_csv("./hsa_pop.csv")


hsa_pop %>% 
    group_by(hsacity, year) %>%
    summarise(pop = sum(hsa_pop)) %>% 
    ggplot(aes(x = year, y = pop)) +
    geom_point() + geom_line() +
    facet_wrap(~hsacity, scales = "free")


hsa_pop %>% 
    group_by(agesex_grp, year) %>%
    summarise(pop = sum(hsa_pop)) %>% 
    ggplot(aes(x = year, y = pop)) +
    geom_point() + geom_line() +
    facet_wrap(~agesex_grp, scales = "free")


hsa_pop %>% 
    filter(hsacity == "Portland") %>% 
    group_by(agesex_grp, year) %>%
    summarise(pop = sum(hsa_pop)) %>% 
    ggplot(aes(x = year, y = pop)) +
    geom_point() + geom_line() +
    facet_wrap(~agesex_grp, scales = "free")

# population distribution
hsa_pop %>% 
    mutate(age_grp = str_extract(agesex_grp, "[:digit:]+_[:digit:]+$")) %>% 
    group_by(age_grp, year) %>% 
    summarise(pop = sum(hsa_pop)) %>% 
    ggplot(aes(age_grp, pop)) +
    geom_bar(stat = "identity") +
    facet_wrap(~year) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))

# compare with acs 5 yr estimates by hsa
acs <- read_csv("hsa_pop_acs5yr_2014.csv")
acs
hsa_pop

pop_cmp <- left_join(hsa_pop, acs) %>% 
    rename(pop_cw = hsa_pop, pop_acs = pop) %>% 
    mutate(pop_acs = ifelse(is.na(pop_acs), 0, pop_acs))

pop_cmp

# compare overall population totals
pop_cmp %>% group_by(year) %>% 
    summarise(pop_cw = sum(pop_cw), pop_acs = sum(pop_acs)) %>% 
    mutate(dif = pop_cw - pop_acs)

# lets take the average of the crosswalk estimates from 2010 to 2014 and compare with acs
pop_cmp <- hsa_pop %>% 
    filter(year %in% 2010:2014) %>% 
    group_by(agesex_grp, hsanum, hsacity, hsastate) %>% 
    summarise(hsa_pop = mean(hsa_pop)) %>% 
    left_join(acs) %>% 
    rename(pop_cw = hsa_pop, pop_acs = pop) %>% 
    mutate(pop_acs = ifelse(is.na(pop_acs), 0, as.numeric(pop_acs))) %>% 
    ungroup()

pop_cmp

# check state total
pop_cmp %>% 
    summarise(pop_cw = sum(pop_cw), pop_acs = sum(pop_acs)) %>% 
    mutate(dif = pop_cw - pop_acs, dif_pct = round(dif/pop_acs, 5))


# summarise by hsa
pop_cmp %>% 
    group_by(hsacity) %>% 
    summarise(pop_cw = sum(pop_cw), pop_acs = sum(pop_acs)) %>% 
    mutate(dif = round(pop_cw - pop_acs,0), dif_pct = round(dif/pop_acs, 5)) %>% 
    arrange(pop_cw) %>% 
    print(n=34)

# greenville is a problem
pop_cmp %>% 
    filter(hsacity == "Greenville") %>% 
    mutate(age = str_replace(agesex_grp, "M_|F_", "")) %>% 
    group_by(age) %>% 
    summarise(pop_cw = sum(pop_cw), pop_acs = sum(pop_acs)) %>% 
    mutate(dif = round(pop_cw - pop_acs,0), dif_pct = round(dif/pop_acs, 5)) %>% 
    arrange(age) %>% 
    print(n=34)

# summarise by age
pop_cmp %>% 
    mutate(age = str_replace(agesex_grp, "M_|F_", "")) %>% 
    group_by(age) %>% 
    summarise(pop_cw = sum(pop_cw), pop_acs = sum(pop_acs)) %>% 
    mutate(dif = round(pop_cw - pop_acs,0), dif_pct = round(dif/pop_acs, 5)) %>% 
    arrange(age) %>% 
    print(n=34)

# This actually looks pretty good

dif_pct = round(dif/pop_acs, 5)

pop_cmp


# it would be great to revalidate hsa definitions 
# boothbay, greenville, are these still valid?
# where is the planning analyst's hsa crosswalk update?
