#load packages
library(tidyverse)

#read in data
cover_fall2024 <- read_csv("~/Library/CloudStorage/OneDrive-UNCG/Documents/GitHub/IheartLLP/LLP Tripartite Team/Data/LLPtripartite_Cover_Fall2024.csv")
View(LLPtripartite_Cover_Fall2024)

#check species names
sort(unique(cover_fall2024$species))
##still several unknowns; need to work on ID still

cover_fall2024$species <- cover_fall2024$species %>% 
  dplyr::recode("Vernonia_angustifolia(MPS pic)" = "Vernonia_angustifolia") #renaming a known 'unknown' because I forgot on excel


#calculating relative abundance
relative_abundance <- cover_fall2024 %>% group_by(plot, .drop=FALSE) %>%
  mutate(plot.abundance = sum(cover)) %>% 
  ungroup() %>% 
  mutate(rel.abundance = cover/plot.abundance) %>% 
  group_by(species) %>% reframe(average_rel_abundance = mean(rel.abundance))
  
 
#calculating relative frequency
relative_frequency <-cover_fall2024_dci %>% 
  group_by(species) %>% 
  summarize(species_count=n(),.groups = 'drop') %>%
  mutate(relative_freq = species_count/72)


#putting relative abundance and relative frequency together then calculating dci
fall2024_dci <- left_join(relative_abundance, relative_frequency, by = "species") %>%
  mutate(dci = (average_rel_abundance + relative_freq)/2)


fall_dci2024_all <- left_join(fall2024_dci, )


fall2024_dci %>% ggplot(aes(x=reorder(species, -dci), y=dci)) + geom_point() +  
  theme(axis.text.x = element_text(angle=90, hjust=1))
