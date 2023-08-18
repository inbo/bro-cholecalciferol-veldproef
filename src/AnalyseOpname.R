# Analyse opname

library(readxl)
library(tidyverse)

# Pre-census
precensus <- read_xlsx("input/Opname graan, pre-bait en gif.xlsx",
          sheet = "Blad1") %>% 
  pivot_longer(2:30) %>% 
  mutate(name = as.factor(name))

ggplot(precensus, aes(x = Datum, y = value, color = name)) +
  geom_point() +
  geom_line() +
  ylim(800, 1050) +
  ylab("Gewicht lokaasdoos + graan (g)") + 
  theme(legend.position = "none")  
       

# Bestrijding
bestrijding <- read_xlsx("input/Opname graan, pre-bait en gif.xlsx",
                       sheet = "Blad2") %>% 
  pivot_longer(2:30) %>% 
  mutate(name = as.factor(name))

ggplot(bestrijding, aes(x = Datum, y = value, color = name)) +
  geom_point() +
  geom_line() +
  ylim(800, 1000) +
  ylab("Gewicht lokaasdoos + Harmonix (g)") + 
  theme(legend.position = "none")  

