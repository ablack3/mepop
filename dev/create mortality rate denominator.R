# 2010 census population esitmates by HSA, 



# create mortality rate denominator
# with 2010 census data

crosswalk <-  read.csv("ZipHsaHrr13.csv", stringsAsFactors = F) %>% 
    filter(hsastate %in% c("ME","NH")) %>% 
    select(zipcode13, hsanum, hsacity, hsastate)



######### get population by age and sex groups by HSA from census ##################
all_content <-  readLines("./Census_raw/DEC_10_SF1_QTP1_with_ann.csv")
cen <-  read.csv(textConnection(all_content[-1]), header = TRUE, stringsAsFactors = FALSE)

names(cen)
total <- sum(cen$Number...Both.sexes..Total.population)
sum(cen$Number...Male..Total.population) + sum(cen$Number...Female..Total.population)

# strip out punctuation
names(cen) <- str_replace_all(names(cen), "[:punct:]", "")

# subset variables
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
    left_join(crosswalk, by = c("zip" = "zipcode13"))  

cen_long_NH <- cen_long %>% filter(hsastate == "NH") 

cen_long <- filter(cen_long, hsastate == "ME")

# some maine residents do live in NH HSAs (Berlin and Dover)
names(cen_long)
sum(cen_long$pop)
sum(cen_long_NH$pop)
total_in_hsas <- sum(cen_long$pop)

#number in Maine but in NH HSA
total - total_in_hsas

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

sum(hsa_cen_pop35$pop)
# What percentage of people are in the all payer data (age 35 and up)?
606427/784873
# about 77%





