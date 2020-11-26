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
CleanData <- read_csv(here("data/CleanData.csv"))

#plotten aantal stappen per week voor mannen en vrouwen
CleanData %>% 
  ggplot(aes(x=geslacht, y = stap_om_week_aantal)) +
           geom_point()

#dezelfde plot, maar dan met Jitter
CleanData %>% 
  ggplot(aes(x=geslacht, y = stap_om_week_aantal)) +
  geom_jitter()

#dezelfde plot, maar dan met quasirandom
CleanData %>% 
  ggplot(aes(x=geslacht, y = stap_om_week_aantal)) +
  geom_quasirandom()

#Opdracht 2: gebruik van kleuren en meer (VizWhiz 1, filmpje 2) ----
CleanData %>% 
  na.omit() %>% 
  ggplot(aes(x=geslacht, y = stap_om_week_aantal)) +
  geom_jitter() 

# verdeling op uit en thuiswonenden
CleanData %>% 
  na.omit() %>% 
  ggplot(aes(x=woon, y = stap_om_week_aantal)) +
  geom_jitter() 

#coordinate fli
CleanData %>% 
  na.omit() %>% 
  ggplot(aes(x=woon, y = stap_om_week_aantal)) +
  geom_point() + 
  coord_flip()

#kleur voor mannen en vrouwen
CleanData %>% 
  na.omit() %>% 
  ggplot(aes(x=woon, y = stap_om_week_aantal, colour = geslacht)) +
  geom_jitter() + 
  coord_flip()


# Opdracht 3 Facet Wraps (vizwhiz 1, derde filmpje)---------
CleanData %>% 
  na.omit() %>% 
  ggplot(aes(x=geslacht, y = stap_om_week_aantal)) +
  geom_jitter() +
  facet_wrap( ~ woon)

#add colour
CleanData %>% 
  na.omit() %>% 
  ggplot(aes(x=geslacht, y = stap_om_week_aantal, colour = geslacht)) +
  geom_jitter() +
  facet_wrap( ~ woon)


#Opdracht Pipe the plot (vizwhiz 1, vierde filmpje)
CleanData %>% 
  na.omit() %>% 
  filter (stap_om_week_aantal > 1000) %>% 
  filter (stap_om_week_aantal < 80000) %>% 
  ggplot(aes(x=geslacht, y = stap_om_week_aantal, colour = geslacht)) +
  geom_jitter() +
  facet_wrap( ~ woon)


#vizwhiz 1, opslaan, vijfde filmpje
  ggsave("stappenMannenVrouwen.png")

#vizwhiz 3, tweede filmpje, error bars
  CleanData %>% 
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
  CleanData %>% 
    na.omit() %>% 
    ggplot(aes(x=stap_om_1_aantal, y = stap_om_2_aantal, color = geslacht)) +
    geom_point() + 
    geom_smooth() 
    
  #vizwhiz 4, eerste filmpje, themes
  CleanData %>% 
    na.omit() %>% 
    ggplot(aes(x=stap_om_1_aantal, y = stap_om_2_aantal, color = geslacht)) +
    geom_point() + 
    geom_smooth() + 
    theme_classic() +
    labs(title = "relatie tussen aantal op maandag en aantal stappen op dinsdag", 
         x = 'aanstal stappen op maandag',
         y= 'aantal opstappen op dinsdag')

  