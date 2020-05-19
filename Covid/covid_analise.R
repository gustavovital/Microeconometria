# Autor: gustavoovital
# Data: 18/05/2020

# Pacotes Necessários ----

library(Synth)

# Base de dados ----

data <- read_csv2('teste.csv')
data$unit <- rep(1:8, each = 32)

data %>% 
  arrange(Time) -> data_teste

paises <- c('BR','AR','BOL','CL','COL','EC','PY','PE','VEN')

data_wdi <- tibble(WDI::WDI(country = paises,
                            indicator = c('mais_de_65'='SP.POP.65UP.TO',
                                          'de_15_a_65'='SP.POP.1564.TO',
                                          'menos_de_14'='SP.POP.0014.TO'), 
                            start = 2018,
                            end = 2018,
                            extra = FALSE))

names(data_wdi)[2] <- 'Country'

data_wdi %>% 
  left_join(data_teste) -> covid

covid <- as.data.frame(covid)

# Estimação pelo Synth ----

# Casos Confirmados ----

dataprep.out.confirmed <-
  dataprep(covid,
           predictors = c("mais_de_65", "de_15_a_65"),
           dependent     = c("Confirmed"),
           unit.variable = "unit",
           time.variable = "Time",
           unit.names.variable = "Country",
           treatment.identifier  = 'Brazil',
           controls.identifier   = c('Argentina', 'Bolivia', 'Chile', 'Colombia', 'Ecuador', 'Peru', 'Paraguay'),
           time.predictors.prior = c(1:16),
           time.optimize.ssr     = c(1:16),
           time.plot             = c(1:32)
  )

synth.out.confirmed <- synth(dataprep.out.confirmed)

print(synth.tables.confirmed   <- synth.tab(
  dataprep.res = dataprep.out.confirmed,
  synth.res    = synth.out.confirmed)
)

path.plot(synth.res    = synth.out.confirmed,
          dataprep.res = dataprep.out.confirmed,
          Ylab         = c("Casos Diários"),
          Xlab         = c("Dias Após o Primeiro Caso"),
          Legend       = c("Brasil","Brasil Sintético"),
          Legend.position = c("topleft")
)
abline(v = 16)
text(22, 600, 'possível lockdown se\nfeito no 16 dia após o primeiro caso')

# Mortes ----

dataprep.out.death <-
  dataprep(covid,
           predictors = c("mais_de_65", "de_15_a_65", 'Confirmed'),
           dependent     = c("Death"),
           unit.variable = "unit",
           time.variable = "Time",
           unit.names.variable = "Country",
           treatment.identifier  = 'Brazil',
           controls.identifier   = c('Argentina', 'Bolivia', 'Chile', 'Colombia', 'Ecuador', 'Peru', 'Paraguay'),
           time.predictors.prior = c(1:16),
           time.optimize.ssr     = c(1:16),
           time.plot             = c(1:32)
  )

synth.out.death <- synth(dataprep.out.death)

print(synth.tables.death   <- synth.tab(
  dataprep.res = dataprep.out.death,
  synth.res    = synth.out.death)
)

path.plot(synth.res    = synth.out.death,
          dataprep.res = dataprep.out.death,
          Ylab         = c("Mortes Diárias"),
          Xlab         = c("Dias Após o Primeiro Caso"),
          Legend       = c("Brasil","Brasil Sintético"),
          Legend.position = c("topleft")
)
abline(v = 16)
text(22, 20, 'possível lockdown se\nfeito no 16 dia após o primeiro caso')

# Plotando o gap ----

# Casos confirmados ----

gaps.plot(synth.res    = synth.out.confirmed,
          dataprep.res = dataprep.out.confirmed,
          Ylab         = c("Gap"),
          Xlab         = c("Dias Após o Primeiro Caso"),
          # Ylim         = c(-30, 30),
          Main         = "Gap Casos Confirmados"
)

abline(v   = 16,
       lty = 2)

# Mortes ----

gaps.plot(synth.res    = synth.out.death,
          dataprep.res = dataprep.out.death,
          Ylab         = c("Gap"),
          Xlab         = c("Dias Após o Primeiro Caso"),
          # Ylim         = c(-30, 30),
          Main         = "Gap Mortes"
)

abline(v   = 16,
       lty = 2)

# Graficos ----

