#Data analysescript Stappentelleropdracht 
#Basics versie met inladen van data en preprocessing/clearning
#Ruud Selles
#Augustus 2020

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
