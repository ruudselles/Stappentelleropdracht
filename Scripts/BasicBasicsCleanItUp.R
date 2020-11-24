#Data analysescript Stappentelleropdracht 
#Basics versie met inladen van data en preprocessing/clearning
#Ruud Selles
#Augustus 2020

#BASIC BASICS --------
#laden relevante packages----
library(tidyverse)
library(here)
library(readr)
library(janitor)


rm(list = ls()) #clear the workspace-------
#importeren twee databestanden van de stappentelleropdracht
Intake <- read_csv("data/Klinische_Technologie_Intake.csv")
Data <- read_csv("data/Klinische_Technologie_Stappenteller_Data.csv")

#rm(intake)

#CLEAN IT UP 1 ----------
#Opdracht variabele Namen Opschonen (CleanItUp Lesson 1, video 1)-----------
CleanIntake <- clean_names(Intake)
view(CleanIntake)

#Opdracht variabele herorganiseren (CleanItUp Lesson 1, video 2)-----------
CleanIntake <- select(CleanIntake, id, geslacht, woon, everything()) #drie variabelen voorop zetten
CleanIntake <- select(CleanIntake, -submitdate, -startlanguage) #2 variabelen verwijderen



#Opdracht Pipe je opschoonacties (CleanItUp Lesson 1, video 3)----
CleanData <- Data  %>% 
  clean_names() %>% 
  select(id, nummer, everything()) %>% #zet drie variabelen vooraan in dataframe
  select(-submitdate,startdate, datestamp, startlanguage, lastpage) #verwijder variabelen uit dataframe
  
#verwijder alles wat niet nodig is voor het vervolg uit de global environment
rm(Intake, Data)

# Opdracht Data samenvoegen ----
data_combined <- inner_join(CleanData, CleanIntake, by="nummer")


# Clean it up, Lesson 1, Video 2, selecteren van variabelen voor vervolganalyse----
data_combined <- data_combined %>% 
  select (geslacht, woon, stap_om_1_aantal, stap_om_2_aantal, 
          stap_om_3_aantal, stap_om_4_aantal, stap_om_5_aantal, 
          stap_om_6_aantal, stap_om_7_aantal)


#CLEAN IT UP 2----------
#Opdracht sorteren van waarden (CleanItUp Lesson 1, video 1) ----
  summary(data_combined)

data_omrom <- data_combined %>% 
  arrange(desc(stap_om_1_aantal)) %>% 
  select(stap_om_1_aantal)

data_omrom_vrouwen <- data_combined %>% 
  filter(geslacht == "Vrouw") %>% 
  arrange(desc(stap_om_1_aantal)) %>% 
  select(stap_om_1_aantal)
  
data_omrom_mannen <- data_combined %>% 
  filter(geslacht == 'Man') %>% 
  arrange(desc(stap_om_1_aantal)) %>% 
  select(stap_om_1_aantal)


#Opdracht Group_By CleanItUp 2, filmpje 2 ----
data_combined_summarize <- data_combined %>% 
  group_by(geslacht) %>% 
  summarize(maxsteps = max(stap_om_1_aantal, na.rm=TRUE), 
            minsteps = min(stap_om_1_aantal,na.rm=TRUE), 
            meansteps = mean(stap_om_1_aantal,na.rm=TRUE), 
            mediansteps = median(stap_om_1_aantal,na.rm=TRUE), 
            sdsteps = sd(stap_om_1_aantal,na.rm=TRUE))


#Opdracht opslaan summary data frame (CleanItUp 2, filmpje 3) -----------
ManVrouwUitThuis <- data_combined %>%   
  group_by(geslacht, woon) %>% 
  summarize(maxsteps = max(stap_om_1_aantal, na.rm=TRUE), 
            minsteps = min(stap_om_1_aantal,na.rm=TRUE), 
            meansteps = mean(stap_om_1_aantal,na.rm=TRUE), 
            mediansteps = median(stap_om_1_aantal,na.rm=TRUE), 
            sdsteps = sd(stap_om_1_aantal,na.rm=TRUE))


#Opdracht Mutate (CleanItUp 3, filmpje 2 en 3) ------------
data_combined <- data_combined %>% 
  mutate(stap_om_week_aantal = stap_om_1_aantal + stap_om_2_aantal+ stap_om_3_aantal+ stap_om_4_aantal+ 
                                stap_om_5_aantal+ stap_om_6_aantal+ stap_om_7_aantal) 

data_combined <- data_combined %>% 
  mutate(stap_om_week_aantal = stap_om_1_aantal + stap_om_2_aantal+ stap_om_3_aantal+ stap_om_4_aantal+ 
           stap_om_5_aantal+ stap_om_6_aantal+ stap_om_7_aantal) %>% 
  mutate(stap_om_1_10000 = stap_om_1_aantal > 10000)

#opslaan van de data_combined file
write.csv(data_combined, here("data", "data_combined.csv"))


