
#clear environment
rm(list=ls())

#load packages
library(tidyverse)
library(cowplot)

#read in data
cover_fall2024 <- read_csv("~/Library/CloudStorage/OneDrive-UNCG/Documents/GitHub/IheartLLP/LLP Tripartite Team/Data/LLPtripartite_Cover_Fall2024.csv")
View(cover_fall2024)

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
relative_frequency <-cover_fall2024 %>% 
  group_by(species, growth_form) %>% 
  summarize(species_count=n(),.groups = 'drop') %>%
  mutate(relative_freq = species_count/72)


#putting relative abundance and relative frequency together then calculating dci
fall2024_dci <- left_join(relative_abundance, relative_frequency, by = "species") %>%
  mutate(dci = (average_rel_abundance + relative_freq)/2)


#removing litter, moss, and bare ground from the dataset
fall2024_dci_short <- fall2024_dci[fall2024_dci$growth_form != "litter", ]
fall2024_dci_short <- fall2024_dci_short[fall2024_dci_short$growth_form != "moss", ]
fall2024_dci_short <- fall2024_dci_short[fall2024_dci_short$growth_form != "bare-ground", ]

#just looking at the entirety of the data 
fall2024_dci_short %>% ggplot(aes(x=reorder(species, -dci), y=dci, color= growth_form)) + 
  geom_point(size=1) +  
  scale_color_manual(values = c("orchid4", "olivedrab3", "cornflowerblue", "goldenrod3")) +
  theme_bw()+
  theme(axis.text.x = element_text(angle=45, hjust=1, size = 4)) +
  labs(x = "Species", y = "DCI")




#looking at top 5 dci values for grass
grass_dci <- fall2024_dci_short %>% subset(growth_form=="grass")
sort(grass_dci$dci, decreasing = TRUE)



fall2024_dci_short %>% subset(growth_form=="grass") %>% 
  ggplot(aes(x=reorder(species, -dci), y=dci)) + 
  geom_point(color="olivedrab3") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  labs(x="Grass species", y="DCI")
  




#looking at top 5 dci values for legumes
legume_dci <- fall2024_dci_short %>% subset(growth_form=="legume")
sort(legume_dci$dci, decreasing = TRUE)



fall2024_dci_short %>% subset(growth_form=="legume") %>% 
  ggplot(aes(x=reorder(species, -dci), y=dci)) + 
  geom_point(color="cornflowerblue") +  
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1, size=7))+
  labs(x="Legume species", y="DCI")





#looking at top 5 dci values for forbs
forb_dci <- fall2024_dci_short %>% subset(growth_form=="forb")
sort(forb_dci$dci, decreasing = TRUE)


fall2024_dci_short %>% subset(growth_form=="forb") %>% 
  ggplot(aes(x=reorder(species, -dci), y=dci)) + 
  geom_point(color ="orchid4") +  
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1, size=7)) +
  labs(x="Forb species", y = "DCI")

view(forb_dci_plot)

