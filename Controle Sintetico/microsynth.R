# Analise de controle sintético a partir do pacote Microsynth:
#   
#   - Calculo dos pesos das ponderações entre controle e observáveis
#   - Medidas adicionais de significancia, isso é, gerar um "tratamento placebo"
#   utilizando permutações aleatorias nos tratamentos unitarios, bem como 
#   normaliza-los, gerar estimadores de variancias e p-valores novos
#   - A estatistica omnibus é calculada para se ter acesso as multiplas 
#   estatisticas meio as variaveis, bem como nos cenarios
#   - Resultados podem ser apresentados a partir de varios cenarios de
#   acompanhamento
#   
# Autor: gustavo vital
# Data: 14/05/2020

library(microsynth)

# Exemplo: Drug Market Intervention ----

seattledmi <- seattledmi # base de dados comparando intervenções no trafico
                         # a partir de quadras em seattle

colnames(seattledmi)

set.seed(99199)

# Queremos saber se o programa foi efetivo onde as intervenções foram
# aplicadas, isso é, houve uma recução nos crimes? antes de tudo, vamos 
# especificar os parametros minimos para a pesquisa

# definindo as colunas ID ----

# Vamos comparar areas com intervenção contra areas sem intervenção, para cada
# unidadae dado o periodo do tempo. Isso é:
#   
#   intvar - intenverção (1 = intervenção, 0 c.c)
#   timevar - tempo
#   idvar - observação
  
# definindo parametros de tempo ----

# isso é, pre intervenção e seu fim periodo a frente estimado. para o periodo
# estimado variaveis de resultado e covariaveis serão utilizadas para a 
# comparação 

# definindo covariaveis e variaveis ----

# diferenças: variaveis estimadas não tendem a ser constante, enquanto 
# covariaveis tendem, dado a intervenção 

# For this study, we would like to estimate the effect of the DMI on rates 
# of crime. Specifically we are interested in the effects on four types of 
# incidences of crime: felony arrests, misdemeanor arrests, drug arrests, 
# and any criminal arrest.

# indicamos essas variaveis abaixo, saida:

match.out <- c("i_felony", "i_misdemea", "i_drugs", "any_crime") 
                  
# Passing these variables to match.out instructs microsynth to calculate weights
# that provide exact matches on these variables; assigning result.var = match.out 
# identifies them as outcome variables for which we would like effects estimated;
# omnibus.var will include them in the omnibus statistic. After the microsynth
# object is created, we can plot results with with the argument set to 
# plot.var = match.out to indicate variables to appear on plots.

# covariaveis:

cov.var <- c("TotalPop", "BLACK", "HISPANIC", "Males_1521", "HOUSEHOLDS", 
             "FAMILYHOUS", "FEMALE_HOU", "RENTER_HOU", "VACANT_HOU")

# Primeira estimação ----

sea1 <- microsynth(seattledmi, 
                   idvar="ID", timevar="time", intvar="Intervention", 
                   start.pre=1, end.pre=12, end.post=16, 
                   match.out=match.out, match.covar=cov.var, 
                   result.var=match.out, omnibus.var=match.out,
                   test="lower",
                   n.cores = min(parallel::detectCores(), 2))

summary(sea1)
sea1

# After the call to microsynth has been made, the function displays a brief
# description of the parameters used in the call along with the results
# (if available). Also, the function can be used to display a summary of the 
# matching between treatment, synthetic control, and the population, and the 
# results table. Below we reproduce the results that were saved to file in
# the previous example, with one row for each of the variables entered to
# result.var, which have each been used to calculate an omnibus statistic
# (omnibus.var = TRUE), and two columns corresponding to the confidence interval
# (confidence) resulting from the variance estimator generated by linearization.
# The first row of the output (16) refers to the maximum post-intervention time 
# used to compile results (end.post).

plot_microsynth(sea1)
