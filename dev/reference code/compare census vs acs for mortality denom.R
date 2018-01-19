# compare population census vs amer com survey population estimates
# We need a denominator for the mortality rates and thus need hsa population estimates by age and sex 


library(dplyr)
library(tidyr)
library(stringr)


######### get population by age and sex groups by HSA from census ##################
all_content <-  readLines("./Census_raw/DEC_10_SF1_QTP1_with_ann.csv")
cen <-  read.csv(textConnection(all_content[-1]), header = TRUE, stringsAsFactors = FALSE)

names(cen)
sum(cen$Estimate..Total.)
sum(cen$Estimate..Male.)
sum(cen$Estimate..Female.)

names(cen) <- str_replace_all(names(cen), "[:punct:]", "")

cen <- select(cen, -matches("Percent|Bothsexes|Malesper100females"))

names(cen) <- str_replace_all(names(cen), "Number", "")

cen <- select(cen, 1:43) %>% 
    select(-matches("Totalpopulation$")) %>% 
    select(-Id, -Geography) %>% 
    rename(zip = Id2)

names(cen)

# shorten variable names
names(cen) <- str_replace_all(names(cen), "Totalpopulation|years.*", "") %>% 
    str_replace("^(F|M)[:alpha:]+([:digit:]+)[:alpha:]+([:digit:]+)", "\\1_\\2_\\3") %>% 
    str_replace("^(F|M)[:alpha:]+Under5", "\\1_0_4") %>% 
    str_replace("^(F|M)[:alpha:]+90", "\\1_90")

names(cen)    

# make sure all the zipcodes are in the hsa crosswalk
all(cen$zip %in% crosswalk$zipcode13)


# transpose from wide to long
# shorten variable names using regex (with capture groups)
cen_long <- cen %>% 
    gather(key = "asgroup", value = "pop", -zip) %>% 
    mutate(pop = as.numeric(str_replace(pop, "-", ""))) %>% 
    left_join(crosswalk, by = c("zip" = "zipcode13")) %>% 
    filter(hsastate == "ME") 

# some maine residents do live in NH HSAs
names(cen_long)
sum(cen_long$pop, na.rm=T)

#need to collapse some age categories
unique(cen_long$asgroup)

hsa_cen_pop <- cen_long %>% 
    mutate(asgroup = str_replace(asgroup, "^(F|M)_(80_84|85_89|90)", "\\1_80+")) %>% 
    group_by(hsacity, hsanum, asgroup) %>% 
    summarise(pop = sum(pop))

# simple checks
unique(hsa_cen_pop$asgroup)
sum(hsa_cen_pop$pop)
head(hsa_cen_pop)
cen_long$hsacity %>% unique() %>% length()
# 32 HSAs
getwd()

write.csv(hsa_cen_pop, "hsa_agesexgroup_population.csv", row.names = F)

# filter only population age 35 and up
hsa_cen_pop35 <- hsa_cen_pop %>% 
    filter(!str_detect(asgroup, "_0_|_5_|10|15|20|25|30"))

unique(hsa_cen_pop35$asgroup)
write.csv(hsa_cen_pop35, "hsa_agesexgroup_population35.csv", row.names = F)





######### get population by age and sex groups by HSA from ACS ##################
all_content <-  readLines("./ACS_raw/ACS_14_5YR_B01001_with_ann.csv")
acs <-  read.csv(textConnection(all_content[-1]), header = TRUE, stringsAsFactors = FALSE)

sum(acs$Estimate..Total.)
sum(acs$Estimate..Male.)
sum(acs$Estimate..Female.)

names(acs) <- str_replace_all(names(acs), "[:punct:]", "")

acs <- select(acs, -matches("MarginofError"))

# shorten variable names
names(acs) <- str_replace_all(names(acs), "Estimate|years.*", "") %>% 
    str_replace("^(F|M)[:alpha:]+([:digit:]+)[:alpha:]+([:digit:]+)", "\\1_\\2_\\3") %>% 
    str_replace("^(F|M)[:alpha:]+Under5", "\\1_0_4") %>% 
    str_replace("^(F|M)[:alpha:]+([:digit:]+)", "\\1_\\2")

names(acs)    
acs2 <- acs %>% 
    select(-Id, -Geography, -Total, -Male, -Female) %>% 
    rename(zip = Id2)

names(acs2)

# make sure all the zipcodes are in the hsa crosswalk
all(acs2$zip %in% crosswalk$zipcode13)


# transpose from wide to long
# shorten variable names using regex (with capture groups)
acs_long <- acs2 %>% 
    gather(key = "asgroup", value = "pop", -zip) %>% 
    mutate(pop = as.numeric(str_replace(pop, "-", ""))) %>% 
    left_join(crosswalk, by = c("zip" = "zipcode13")) %>% 
    filter(hsastate == "ME") 

# some maine residents do live in NH HSAs
names(acs_long)
sum(acs_long$pop, na.rm=T)

#need to collapse some age categories
unique(acs_long$asgroup)

hsa_acs_pop <- acs_long %>% 
    mutate(asgroup = str_replace(asgroup, "^(F|M)_(15_17|18_19)", "\\1_15_19")) %>% 
    mutate(asgroup = str_replace(asgroup, "^(F|M)_(20|21|22_24)", "\\1_20_24")) %>% 
    mutate(asgroup = str_replace(asgroup, "^(F|M)_(60_61|62_64)", "\\1_60_64")) %>% 
    mutate(asgroup = str_replace(asgroup, "^(F|M)_(65_66|67_69)", "\\1_65_69")) %>% 
    mutate(asgroup = str_replace(asgroup, "^(F|M)_(80_84|85)", "\\1_80+")) %>% 
    group_by(hsacity, hsanum, asgroup) %>% 
    summarise(pop = sum(pop))

# simple checks
unique(hsa_acs_pop$asgroup)
sum(hsa_acs_pop$pop)
head(hsa_acs_pop)
acs_long$hsacity %>% unique() %>% length()
# 32 HSAs
getwd()
write.csv(hsa_acs_pop, "hsa_agesexgroup_population.csv", row.names = F)

# filter only population age 35 and up
hsa_acs_pop35 <- hsa_acs_pop %>% 
    filter(!str_detect(asgroup, "_0_|_5_|10|15|20|25|30"))

unique(hsa_acs_pop35$asgroup)
write.csv(hsa_acs_pop35, "hsa_agesexgroup_population35.csv", row.names = F)

######### compare

head(hsa_acs_pop)
head(hsa_cen_pop)
unique(hsa_acs_pop$asgroup) %>% setdiff( unique(hsa_cen_pop$asgroup))
cbind(unique(hsa_acs_pop$asgroup), unique(hsa_cen_pop$asgroup))



pop <- full_join(hsa_acs_pop, hsa_cen_pop, by = c("hsacity", "hsanum", "asgroup")) %>% 
    ungroup() %>% 
    mutate(dif = pop.y - pop.x, dif_pct = round(dif/pop.x, 2)) 

attributes(pop)

pop %>% ungroup() %>% 
    select(pop.x, pop.y) %>% 
    summarise_all(sum, na.rm = T)

hsa_pop_compare <- pop %>% ungroup() %>%
    group_by(hsacity) %>% 
    select(hsacity, pop.x, pop.y) %>% 
    summarise_all(sum, na.rm = T) %>% 
    mutate(dif = pop.y - pop.x, dif_pct = round(dif/pop.x, 2)) 


hsa_acs_pop %>% ungroup() %>%
    group_by(hsacity) %>% 
    select(hsacity, pop) %>% 
    summarise_all(sum, na.rm = T) 
cor(pop$pop.x,pop$pop.y)
?summarise_
head(pop)
