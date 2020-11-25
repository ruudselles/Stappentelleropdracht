#Data analysescript Stappentelleropdracht 
#Basics versie met inladen van data en preprocessing/clearning
#Ruud Selles / Nienja Langerak
#November 2020

#BASIC BASICS --------
#laden relevante packages----
library(tidyverse)
library(here)
library(readr)

rm(list = ls()) #clear the workspace-------
Data_Full_anonymous <- read_csv(here("data/Data_Full_anonymous.csv"))

#importeren twee databestanden van de stappentelleropdracht
Data <- read_csv(here("data/Data_Full_anonymous.csv"))

rm(Data_Full_anonymous)

#CLEAN IT UP 1 ----------
#Opdracht variabele Namen Opschonen (CleanItUp Lesson 1, video 1)-----------
library(janitor)

# Dit komt kunnen ze uitproberen, maar komt pas echt aanbod in Opdracht pipe je opschoonacties
##CleanIntake <- clean_names(Data)  

#Opdracht variabele herorganiseren (CleanItUp Lesson 1, video 2)-----------
DataSelect <- select(Data, geslacht, woon, everything()) #drie variabelen voorop zetten
DataSelect <- select(DataSelect, -submitdate.x) # variabelen verwijderen


#Opdracht Pipe je opschoonacties (CleanItUp Lesson 1, video 3)----
CleanData <- Data  %>% 
  clean_names() %>% 
  select(geslacht, woon, everything())  #zet drie variabelen vooraan in dataframe

rm(Data)

# Clean it up, Lesson 1, Video 2, selecteren van variabelen voor vervolganalyse----
CleanData <- CleanData %>% 
  select (geslacht, woon, stap_om_1_aantal, stap_om_2_aantal, 
          stap_om_3_aantal, stap_om_4_aantal, stap_om_5_aantal, 
          stap_om_6_aantal, stap_om_7_aantal)


#CLEAN IT UP 2----------
#Opdracht sorteren van waarden (CleanItUp Lesson 1, video 1) ----
summary(CleanData)

Data_Stappen <- CleanData %>% 
  arrange(desc(stap_om_1_aantal)) %>% 
  select(stap_om_1_aantal)

Data_Stappen_Vrouwen <- CleanData %>% 
  filter(geslacht == "Vrouw") %>% 
  arrange(desc(stap_om_1_aantal)) %>% 
  select(stap_om_1_aantal)

Data_Stappen_Mannen <- CleanData %>% 
  filter(geslacht == 'Man') %>% 
  arrange(desc(stap_om_1_aantal)) %>% 
  select(stap_om_1_aantal)


#Opdracht Group_By CleanItUp 2, filmpje 2 ----
ManVrouwUitwonendThuiswonend <- CleanData %>% 
  group_by(geslacht) %>% 
  summarize(maxsteps = max(stap_om_1_aantal, na.rm=TRUE), 
            minsteps = min(stap_om_1_aantal,na.rm=TRUE), 
            meansteps = mean(stap_om_1_aantal,na.rm=TRUE), 
            mediansteps = median(stap_om_1_aantal,na.rm=TRUE), 
            sdsteps = sd(stap_om_1_aantal,na.rm=TRUE))


#Opdracht opslaan summary data frame (CleanItUp 2, filmpje 3) -----------
ManVrouwUitThuis <- CleanData %>%   
  group_by(geslacht, woon) %>% 
  summarize(maxsteps = max(stap_om_1_aantal, na.rm=TRUE), 
            minsteps = min(stap_om_1_aantal,na.rm=TRUE), 
            meansteps = mean(stap_om_1_aantal,na.rm=TRUE), 
            mediansteps = median(stap_om_1_aantal,na.rm=TRUE), 
            sdsteps = sd(stap_om_1_aantal,na.rm=TRUE))


#Opdracht Mutate (CleanItUp 3, filmpje 2 en 3) ------------
CleanData <- CleanData %>% 
  mutate(stap_om_week_aantal = stap_om_1_aantal + stap_om_2_aantal+ stap_om_3_aantal+ stap_om_4_aantal+ 
           stap_om_5_aantal+ stap_om_6_aantal+ stap_om_7_aantal) 

CleanData <- CleanData %>% 
  mutate(stap_om_week_aantal = stap_om_1_aantal + stap_om_2_aantal+ stap_om_3_aantal+ stap_om_4_aantal+ 
           stap_om_5_aantal+ stap_om_6_aantal+ stap_om_7_aantal) %>% 
  mutate(stap_om_1_10000 = stap_om_1_aantal > 10000)

#opslaan van de CleanData file
write.csv(CleanData, here("data", "CleanData.csv"))



#Data analysescript Stappentelleropdracht 
#Visualisatie van de data
#Ruud Selles / Nienja Langerak
#November 2020


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



# load packages
library(tidyverse)
library(here)

#load data 'Data_Combined
CleanData <- read_csv(here("data", "CleanData.csv" ))



# hyopthese toetsen
# Op basis van deel 1 van video 2 (tot 8.50 minuten)
# is het verschil tussen het gemiddelde aantal stappen per week van mannen en vrouwen significant?
# is de data normaal verdeeld (density plot)?

CleanData %>%
  na.omit() %>%
  ggplot(aes(x = stap_om_week_aantal, fill = geslacht))+
  geom_density() +
  facet_wrap(~geslacht, ncol= 1)

# voor vrouwen redelijk normaal verdeeld, voor mannen niet. > wilcoxon test
wilcox.test(stap_om_week_aantal ~ geslacht, data = CleanData, mu= 0)
# outcome: p = 0.5109

# ook een optie om evt uit te proberen
CleanData %>%
  na.omit() %>%
  ggplot(aes(x =geslacht, y = stap_om_week_aantal))+
  geom_violin()


# Op basis van deel 2 van video 2 (van 8.50 minuten tot
# Zetten KT studenten 10.000 stappen per dag dus 70.000 per week? 
# is de data normaal verdeeld?
CleanData %>%
  na.omit() %>%
  ggplot(aes(x = stap_om_week_aantal)) +
  geom_histogram()

wilcox.test(CleanData$stap_om_week_aantal, mu = 70000)

#en als het aantal stappen per dag maar 7000 zou zijn (49000 per week)?
wilcox.test(CleanData$stap_om_week_aantal, mu = 49000)


# is er een relatie tussen het aantal stappen op maandag en zaterdag?
# is de data normaal verdeeld (histogram)?

CleanData %>%
  na.omit() %>%
  ggplot(aes(x =  stap_om_1_aantal)) +
  geom_histogram()

CleanData %>%
  na.omit() %>%
  ggplot(aes(x =  stap_om_6_aantal)) +
  geom_histogram()

CleanData %>% 
  na.omit() %>%   
  ggplot(aes(x =  stap_om_1_aantal, y = stap_om_6_aantal)) +
  geom_point()

# correlatie test met de data 
cor.test(CleanData$stap_om_1_aantal, CleanData$stap_om_6_aantal, method = 'pearson')

cor.test(CleanData$stap_om_1_aantal, CleanData$stap_om_6_aantal, method = 'spearman', exact=F)

# outcome: p = 0.5387

#Error: Cannot compute exact p-value with ties?
#Spearmn is a non-parametric, thus it is not possible to get CIs. There is a error message because R cannot compute exact p values (the test is based on ranks).
#We can get rid off the warning letting R know that approximate values are fine
cor.test(CleanData$stap_om_1_aantal, CleanData$stap_om_6_aantal, method = 'pearson', exact=F)
