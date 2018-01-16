#  create sysdata

library(tidyverse)
list.files("./output tables")
hsa_pop <- read_csv("./output tables/hsa_pop.csv", col_types = "iciccd")
hsa_pov <- read_csv("./output tables/hsa_pov.csv")

unique(hsa_pop$hsacity)

# drop portsmouth hsa. keep normal 32 amine hsas and Dover.
hsa_pop <- hsa_pop %>% filter(hsacity != "Portsmouth")


devtools::use_data(hsa_pop, hsa_pov)


# build
getwd()

devtools::install()

