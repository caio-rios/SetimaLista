library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyverse)
library(dotwhisker)
library(broom)
library(foreign)

ecovote <- read.dta("fair.dta")

#Questão 1-------------------------------------------------------------

# A)

summary(ecovote)

Variáveis <- c("Year", "Vote", "Party", "Person", "Duration", "War", "Growth",
               "Inflation", "Good News")
Mínimo <- c(1880, 36.12, -1, 0, 0, 0, -14.56, 0, 0)
PrimeiroQuartil <- c(1911, 49.43, -1, 0, 0, 0, -1.67, 1.40, 3.75)
Mediana <- c(1942, 52.03, -1, 1, 1, 0, 2.24, 2.16, 5)
Média <- c(1942, 52.27, -0.12, 0.59, 0.70, 0.09, 0.63, 2.66, 5.29)
TerceitoQuartil <- c(1973, 55.69, 1, 1, 1.25, 0, 4.06, 3.35, 7.25)
Máximo <- c(2004, 62.46, 1, 1, 2, 1, 11.68, 7.93, 10)

desc <- data.frame(Variáveis, Mínimo, PrimeiroQuartil, Mediana, Média, 
                   TerceitoQuartil, Máximo)

# B)

regbi <- lm(VOTE ~ GROWTH, data = ecovote)


# I)

# A variável dependente mede a porcentagem do voto do incumbente e a variável
# independente mede o crescimento econômico. Logo, a relação esperada, de 
# acordo com a teoria do voto econômico, é que o crescimento econômico tenha
# um impacto positivo nos votos do incumbente. Isto é, eleitores tendem a 
# recompensar o incumbente quando a economia vai bem.

# II)

summary(regbi)

# A relação encontrada corrobora a hipótese do voto econômico. Quando há 
# crescimento econômico, os eleitores tendem a votar no candidato incumbente.
# A relação foi estatísticamente significante a um p-valor menor que 0,000 e
# quanto a magnitude do efeito, ao aumentar uma unidade da variável crescimento,
# o incumbente receberia 0,65% a mais dos votos.

# III)

# O R² ajustado do modelo foi de 0,3341. Isto quer dizer que o modelo explica
# serca de 33,5% dos casos. Por ser um modelo bivariado, sua capacidade
# explicativa é limitada.


# C)

regmu <- lm(VOTE ~ GROWTH + GOODNEWS, data = ecovote)
summary(regmu)
