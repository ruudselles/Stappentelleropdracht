#Data analysescript Stappentelleropdracht 
#Visualisatie van de data
#Ruud Selles
#Augustus 2020


rm(list = ls()) #clear the workspace-------

#laden relevante packages----
library(tidyverse)
library(here)
library(readr)
library(ggbeeswarm)

#VIZWHIZ --------
#Opdracht 1: het plotten van ruwe data (VizWhiz 1, filmpje 1)-----

#importeren data_combined ----
data_combined <- read_csv("data/data_combined.csv")

#plotten aantal stappen per week voor mannen en vrouwen
data_combined %>% 
  ggplot(aes(x=geslacht, y = stap_om_week_aantal)) +
           geom_point()

#dezelfde plot, maar dan met Jitter
data_combined %>% 
  ggplot(aes(x=geslacht, y = stap_om_week_aantal)) +
  geom_jitter()

#dezelfde plot, maar dan met quasirandom
data_combined %>% 
  ggplot(aes(x=geslacht, y = stap_om_week_aantal)) +
  geom_quasirandom()

#Opdracht 2: gebruik van kleuren en meer (VizWhiz 1, filmpje 2) ----
data_combined %>% 
  na.omit() %>% 
  ggplot(aes(x=geslacht, y = stap_om_week_aantal)) +
  geom_jitter() 

# verdeling op uit en thuiswonenden
data_combined %>% 
  na.omit() %>% 
  ggplot(aes(x=woon, y = stap_om_week_aantal)) +
  geom_jitter() 

#coordinate fli
data_combined %>% 
  na.omit() %>% 
  ggplot(aes(x=woon, y = stap_om_week_aantal)) +
  geom_point() + 
  coord_flip()

#kleur voor mannen en vrouwen
data_combined %>% 
  na.omit() %>% 
  ggplot(aes(x=woon, y = stap_om_week_aantal, colour = geslacht)) +
  geom_jitter() + 
  coord_flip()


# Opdracht 3 Facet Wraps (vizwhiz 1, derde filmpje)---------
data_combined %>% 
  na.omit() %>% 
  ggplot(aes(x=geslacht, y = stap_om_week_aantal)) +
  geom_jitter() +
  facet_wrap( ~ woon)

#add colour
data_combined %>% 
  na.omit() %>% 
  ggplot(aes(x=geslacht, y = stap_om_week_aantal, colour = geslacht)) +
  geom_jitter() +
  facet_wrap( ~ woon)


#Opdracht Pipe the plot (vizwhiz 1, vierde filmpje)
data_combined %>% 
  na.omit() %>% 
  filter (stap_om_week_aantal > 1000) %>% 
  filter (stap_om_week_aantal < 80000) %>% 
  ggplot(aes(x=geslacht, y = stap_om_week_aantal, colour = geslacht)) +
  geom_jitter() +
  facet_wrap( ~ woon)


#vizwhiz 1, opslaan, vijfde filmpje
  ggsave("stappenMannenVrouwen.png")

#vizwhiz 3, tweede filmpje, error bars
  data_combined %>% 
    na.omit() %>% 
    group_by(geslacht) %>% 
    summarise (mean = mean(stap_om_week_aantal),
               sd = sd(stap_om_week_aantal),
               n = n(),
               stderr = sd/sqrt(n)) %>% 
    ggplot(aes(x=geslacht, y = mean)) +
    geom_col() +
    geom_errorbar(aes(x=geslacht, ymin = mean - stderr, ymax = mean+stderr))
  
  #vizwhiz 3, derde filmpje, scatter plot
  data_combined %>% 
    na.omit() %>% 
    ggplot(aes(x=stap_om_1_aantal, y = stap_om_2_aantal, color = geslacht)) +
    geom_point() + 
    geom_smooth() 
    
  #vizwhiz 4, eerste filmpje, themes
  data_combined %>% 
    na.omit() %>% 
    ggplot(aes(x=stap_om_1_aantal, y = stap_om_2_aantal)) +
    geom_point() + 
    geom_smooth() + 
    theme_classic() +
    labs(title = "relatie tussen aantal op maandag en aantal stappen op dinsdag", 
         x = 'aanstal stappen op maandag',
         y= 'aantal opstappen op dinsdag')
  
  
