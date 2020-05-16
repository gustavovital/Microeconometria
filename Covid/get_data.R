
library(data.table)
library(tidyverse)

fread()

confirmed <- fread('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv')
deaths <- fread('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv')

confirmed_clean <- confirmed %>% 
  pivot_longer(-c(`Province/State`, `Country/Region`, Lat, Long), names_to = 'Date', values_to = 'Total') %>% 
  select(`Country/Region`, Total, Date)

deaths_clean <- deaths %>% 
  pivot_longer(-c(`Province/State`, `Country/Region`, Lat, Long), names_to = 'Date', values_to = 'Total') %>% 
  select(`Country/Region`, Total, Date)

start <- as.Date('2020-01-22')

data.confirmed <- seq(start, length.out = length(confirmed[,5:ncol(confirmed)]), by = 'd')
data.deaths <- seq(start, length.out = length(deaths[,5:ncol(deaths)]), by = 'd')

confirmed_clean$Date <- rep(data.confirmed, nrow(confirmed))
deaths_clean$Date <- rep(data.confirmed, nrow(deaths))

confirmed_clean$Case <- 'Confirmed'
deaths_clean$Case <- 'Deaths'

data <- confirmed_clean %>% 
  full_join(deaths_clean) %>% 
  group_by(Date, `Country/Region`, Case) %>% 
  summarise(Total = sum(Total)) 

data_wider <- data %>% 
  pivot_wider(names_from = Case, values_from = Total) %>% 
  mutate(Ativos = Confirmed - Deaths)
