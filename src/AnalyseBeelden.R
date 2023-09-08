library(camtraptor)
library(tidyverse)

precensus <- read_camtrap_dp("./input/AgoutiZemstPre-census")

get_n_individuals(precensus, species = "brown rat")

ratten <- get_record_table(precensus) %>% 
  filter(Species == "Rattus norvegicus") %>% 
  mutate(Graan = ifelse(DateTimeOriginal > ymd_hms("2023-02-10 15:00:00"),
                        "ja",
                        "nee"))
ratten %>% 
  group_by(Station, Graan) %>% 
  summarise(n())

