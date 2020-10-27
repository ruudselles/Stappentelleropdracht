# load packages
library(tidyverse)
library(here)

#load data 'Data_Combined
Data_Combined <- read_csv(here("data", "Data_Combined.csv" ))

# hyopthese toetsen
# Op basis van deel 1 van video 2 (tot 8.50 minuten)
# is het verschil tussen het gemiddelde aantal stappen per week van mannen en vrouwen significant?
# is de data normaal verdeeld (density plot)?

Data_Combined %>%
  na.omit() %>%
  ggplot(aes(x = stap_om_week_aantal, fill = geslacht))+
  geom_density() +
  facet_wrap(~geslacht, ncol= 1)

# voor vrouwen redelijk normaal verdeeld, voor mannen niet. > wilcoxon test
wilcox.test(stap_om_week_aantal ~ geslacht, data = Data_Combined, mu= 0)
# outcome: p = 0.2899

Data_Combined %>%
  na.omit() %>%
  ggplot(aes(x =geslacht, y = stap_om_week_aantal))+
  geom_violin()
  

# Op basis van deel 2 van video 2 (van 8.50 minuten tot
# Zetten KT studenten 10.000 stappen per dag dus 70.000 per week? 
# is de data normaal verdeeld?
Data_Combined %>%
  na.omit() %>%
  ggplot(aes(x = stap_om_week_aantal)) +
  geom_histogram()

wilcox.test(data_combined$stap_om_week_aantal, mu = 70000)

#en als het aantal stappen per dag maar 5000 zou zijn (35000 per week)?
wilcox.test(data_combined$stap_om_week_aantal, mu = 49000)


# is er een relatie tussen het aantal stappen op maandag en zaterdag onder vrouwen?
# is de data normaal verdeeld (histogram)?

Data_Combined %>%
  na.omit() %>%
  ggplot(aes(x =  stap_om_1_aantal)) +
  geom_histogram()

Data_Combined %>%
  na.omit() %>%
  ggplot(aes(x =  stap_om_6_aantal)) +
  geom_histogram()

#make a scatterplot
Data_Combined %>% 
  na.omit() %>%   
  ggplot(aes(x =  stap_om_1_aantal, y = stap_om_6_aantal)) +
  geom_point()

# correlatie test met de data van vrouwen
cor.test(Data_Combined$stap_om_1_aantal, Data_Combined$stap_om_6_aantal, method = 'pearson')
cor.test(Data_Combined$stap_om_1_aantal, Data_Combined$stap_om_6_aantal, method = 'pearson', exact=F)
cor.test(Data_Combined$stap_om_1_aantal, Data_Combined$stap_om_6_aantal, method = 'spearman', exact=F)

# outcome: p = 0.5387

#Error: Cannot compute exact p-value with ties?
#Spearmn is a non-parametric, thus it is not possible to get CIs. There is a error message because R cannot compute exact p values (the test is based on ranks).
#We can get rid off the warning letting R know that approximate values are fine
cor.test(Data_Combined_Vrouw$stap_om_1_aantal,Data_Combined_Vrouw$stap_om_6_aantal, method="pearson", exact=F) 



# is er een significant verschil in het gemiddelde aantal stappen op maandag en zaterdag onder vrouwen?
# is de data normaal verdeeld (histogram)?

Data_Combined %>%
  na.omit() %>%
  filter(geslacht == "Vrouw") %>%
  gather(variable, value, stap_om_1_aantal, stap_om_6_aantal)%>%
  ggplot(aes(x = value)) +
  facet_wrap(~variable) +
  geom_histogram() 

# niet normaal verdeeld > spearman correlatie uitrekenen

# filter vrouwen 
Data_Combined_Vrouw <- Data_Combined %>%
  na.omit() %>%
  filter(geslacht == "Vrouw")

# correlatie test met de data van vrouwen
cor.test(Data_Combined_Vrouw$stap_om_1_aantal, 
         Data_Combined_Vrouw$stap_om_6_aantal, 
         method = 'spearman')
# outcome: p = 0.5387

#Error: Cannot compute exact p-value with ties?
#Spearmn is a non-parametric, thus it is not possible to get CIs. There is a error message because R cannot compute exact p values (the test is based on ranks).
#We can get rid off the warning letting R know that approximate values are fine
cor.test(Data_Combined_Vrouw$stap_om_1_aantal,Data_Combined_Vrouw$stap_om_6_aantal, method="pearson", exact=F) 


