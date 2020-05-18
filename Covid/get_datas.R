
data <- coronavirus::coronavirus

library(tidyverse)

data %>% 
  filter(country == 'Russia', type == 'confirmed') %>% 
  group_by(date, country) %>% 
  summarise(soma = sum(cases)) %>% 
  ggplot(aes(date, soma)) +
  geom_col() +
  theme_minimal()
  

