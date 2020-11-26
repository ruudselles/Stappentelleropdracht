# load packages
library(tidyverse)
library(here)

#load data 'Data_Combined
CleanData <- read_csv(here("data", "CleanData.csv" ))

# hyopthese toetsen
# Op basis van deel 1 van video 2 (tot 8.50 minuten)
# is het verschil tussen het gemiddelde aantal stappen per week van mannen en vrouwen significant?

# Eerst te beantwoorden: zijn de data normaal verdeeld (density plot)?
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
wilcox.test(CleanData$stap_om_week_aantal, mu = 36000)


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
  geom_point() + 
  coord_fixed(ratio = 1)

#filter er bij om uitschieters te verwijderen
CleanData %>% 
  filter(stap_om_1_aantal < 20000) %>% 
  na.omit() %>%   
  ggplot(aes(x =  stap_om_1_aantal, y = stap_om_6_aantal)) +
  geom_point() + 
  coord_fixed(ratio = 1)

# correlatie test met de data 
cor.test(CleanData$stap_om_1_aantal, CleanData$stap_om_6_aantal, method = 'pearson')

cor.test(CleanData$stap_om_1_aantal, CleanData$stap_om_6_aantal, method = 'spearman', exact=F)

# outcome: p = 0.5387

#Error: Cannot compute exact p-value with ties?
#Spearmn is a non-parametric, thus it is not possible to get CIs. There is a error message because R cannot compute exact p values (the test is based on ranks).
#We can get rid off the warning letting R know that approximate values are fine
cor.test(CleanData$stap_om_1_aantal, CleanData$stap_om_6_aantal, method = 'pearson', exact=F)


