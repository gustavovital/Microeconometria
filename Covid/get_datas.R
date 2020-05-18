# Base de dados e manipulação inicial dos dados
# Autor: gustavo vital
# Data: 17/05/2020 

# not run
# install.packages(c('WDI', 'coronavirus'))

library(tidyverse)
library(WDI)

paises <- c('BR', 'AR', 'CL', 'COL', 'EC', 'PE',
            'FR', 'ITA', 'PT', 'ESP', 'DEU')


data_wdi <- tibble(WDI(country = paises,
    indicator = c('mais_de_65'='SP.POP.65UP.TO',
                  'de_15_a_65'='SP.POP.1564.TO',
                  'menos_de_14'='SP.POP.0014.TO',
                  'mulher'='SP.POP.TOTL.FE.IN',
                  'homem'='SP.POP.TOTL.MA.IN',
                  'mortalidade_infantil'='SP.DYN.IMRT.IN' # a cada 1000 nascidos
                  ), 
    start = 2018,
    end = 2018,
    extra = FALSE)) 

names <- data_wdi$country

data <- coronavirus::coronavirus

data %>% 
  filter(country == names[1]) %>% 
  group_by(date, country,type) %>% 
  summarise(soma = sum(cases)) %>% 
  pivot_wider(names_from = type, values_from = soma) %>% 
  left_join(data_wdi)


for(name in names){
  
  data %>% 
    filter(country == name) %>% 
    group_by(date, country,type) %>% 
    summarise(soma = sum(cases)) %>% 
    pivot_wider(names_from = type, values_from = soma) %>% 
    left_join(data_wdi) -> data_roll
  
  write.csv2(data_roll, paste('Covid/Datas/', name, '.csv', sep = ''), dec = '.')
  
}




saveRDS(data, 'Covid/Datas/teste.rds')
