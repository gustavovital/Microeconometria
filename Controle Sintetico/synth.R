# Exemplo de funcionamento do pacote synth e gsynth

# Autor : gustavoovital
# Data : 18/05/2020

# Pacotes ----

library("Synth")
library("gsynth")

# Simulando uma base de dados ----

set.seed(1)
year <- rep(1:30, 10) 
state <- rep(LETTERS[1:10], each = 30)
X1 <- round(rnorm(300, mean = 2, sd = 1), 2)
X2 <- round(rbinom(300, 1, 0.5) + rnorm(300), 2)
Y <- round(1 + 2*X1 + rnorm(300), 2)
df <- as.data.frame(cbind(Y, X1, X2, state, year))
df$Y <- as.numeric(as.character(df$Y))
df$X1 <- as.numeric(as.character(df$X1))
df$X2 <- as.numeric(as.character(df$X2))
df$year <- as.numeric(as.character(df$year))
df$state.num <- rep(1:10, each = 30)
df$state <- as.character(df$state)
df$T <- ifelse(df$state == "A" & df$year >= 15, 1, 0)
df$Y <- ifelse(df$state == "A" & df$year >= 15, df$Y + 20, df$Y)

str(df)

# Visualizando o df ----

head(df)

# Estimando ----

dataprep.out <-
  dataprep(df,
           predictors = c("X1", "X2"),
           dependent     = "Y",
           unit.variable = "state.num",
           time.variable = "year",
           unit.names.variable = "state",
           treatment.identifier  = 1,
           controls.identifier   = c(2:10),
           time.predictors.prior = c(1:14),
           time.optimize.ssr     = c(1:14),
           time.plot             = c(1:30)
  )

synth.out <- synth(dataprep.out)

print(synth.tables   <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res    = synth.out)
)

path.plot(synth.res    = synth.out,
          dataprep.res = dataprep.out,
          Ylab         = c("Y"),
          Xlab         = c("Year"),
          Legend       = c("State A","Synthetic State A"),
          Legend.position = c("topleft")
)
