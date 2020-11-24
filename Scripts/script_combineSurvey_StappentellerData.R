# install and loading packages
install.packages("readxl")

library('here')
library("tidyverse")
library("readxl")
library("janitor")
library("")

# load data
survey_2020 <-  read_xls(here("datasets", "KT1952 2019-2020 ZO Bewegingsmonitoring survey results Intake.xls"))
stappenteller_2020 <- read_xls(here("datasets", "KT1952 2019-2020 ZO Bewegingsmonitoring survey results Stappenteller Data.xls"))
survey_2021 <- read_xls(here("datasets", "KT1952 2020-2021 ZO Bewegingsmonitoring survey results Intake.xls"))
stappenteller_2021 <- read_xls(here("datasets", "KT1952 2020-2021 ZO Bewegingsmonitoring survey results Stappenteller Data.xls"))

# clean names 
Cleansurvey_2020 <- clean_names(survey_2020)
Cleansurvey_2021 <- clean_names(survey_2021)
Cleanstappenteller_2020 <- clean_names(stappenteller_2020)
Cleanstappenteller_2021 <- clean_names(stappenteller_2021)

# check data
glimpse(Cleansurvey_2020)
summary(Cleansurvey_2020)
glimpse(Cleansurvey_2021)
summary(Cleansurvey_2021)
glimpse(Cleanstappenteller_2020)
summary(Cleanstappenteller_2020)
glimpse(Cleanstappenteller_2021)
summary(Cleanstappenteller_2021)

# nr 193 cleansurvey = 195 (op basis van IPAQ score)
View(Cleansurvey)
Cleansurvey_2021[100,2] <- 195
# delete r. 40 (2x nr 80) 
Cleansurvey_2021 <- Cleansurvey_2021[-40,]

#delete r 14 (2x nr 30), delete r. 20, 21, 22 (3x nr 44)
Cleanstappenteller_2021 <- Cleanstappenteller_2021[-c(14,20,21,22), ]

# delete r 82 (2x nr. 179)
Cleansurvey_2020 <- Cleansurvey_2020[-82, ]

# delete r 65 (2x 164) delete unrealistic omron nummer and missings in stappenteller_2020
Cleanstappenteller_2020 <- Cleanstappenteller_2020[-c(65,65525), ]

# delete rows with only NA 
Cleanstappenteller_2020 <- Cleanstappenteller_2020[-c(85:65523), ]

# leftjoin to keep all observations from survey data an match to stapentellerdata

Combined_2019_2020 <- left_join(Cleansurvey_2020, Cleanstappenteller_2020, by="nummer")
Combined_2020_2021 <- left_join(Cleansurvey_2021, Cleanstappenteller_2021, by="nummer")

Full_data <- rbind(Combined_2019_2020, Combined_2020_2021)
Data_Full_anonymous <- select(Full_data, -nummer )

# wirte cvs Data_Full_anonymous
write.csv(Data_Full_anonymous, here("data", "Data_Full_anonymous.csv"))