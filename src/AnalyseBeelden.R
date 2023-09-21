library(camtraptor)
library(tidyverse)

precensus <- read_camtrap_dp("./input/AgoutiZemstPre-census")
postcensus <- read_camtrap_dp("./input/AgoutiZemstPost-census/")

# Get comparable subsets
precensus_sub <- get_record_table(precensus) %>% 
  filter(Species == "Rattus norvegicus",
         DateTimeOriginal < ymd_hms("2023-02-10 15:00:00")) %>% 
  mutate(test = "pre-census")

postcensus_sub <- get_record_table(postcensus) %>% 
  filter(Species == "Rattus norvegicus",
         DateTimeOriginal > ymd_hms("2023-03-31 15:00:00"),
         DateTimeOriginal < ymd_hms("2023-04-04 15:00:00")) %>% 
  mutate(test = "post-census")

combined <- precensus_sub %>% 
  add_row(postcensus_sub)

ratten <- combined %>% 
  group_by(Station, test) %>% 
  summarise(n())

combined %>% 
  mutate(Census = factor(test, levels = c("pre-census", "post-census"))) %>%
  mutate(Station = factor(Station, levels = c("Voor houtkot", 
                                              "Kippenhok",
                                              "Pad achter stal",
                                              "Achter houtkot",
                                              "Koeienstal"))) %>%
  ggplot(aes(x = Station, fill = Census)) +
  geom_histogram(stat = "count", position = "dodge") +
  ylab("# waarnemingen Bruine rat") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0))) +
  xlab("")

chisq.test(table(combined[,c(12)]))




















get_n_individuals(precensus, species = "brown rat")

ratten <- get_record_table(precensus) %>% 
  filter(Species == "Rattus norvegicus") %>% 
  mutate(Graan = ifelse(DateTimeOriginal > ymd_hms("2023-02-10 15:00:00"),
                        "ja",
                        "nee"))
ratten %>% 
  group_by(Station, Graan) %>% 
  summarise(n())

