library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyverse)
library(dotwhisker)
library(broom)
library(foreign)

ecovote <- read.dta("fair.dta")

# GitHub - https://github.com/caio-rios/SetimaLista

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
# A relação foi estatisticamente significante a um p-valor menor que 0,000 e
# quanto a magnitude do efeito, ao aumentar uma unidade da variável crescimento,
# o incumbente receberia 0,65% a mais dos votos.

# III)

# O R² ajustado do modelo foi de 0,3341. Isto quer dizer que o modelo explica
# serca de 33,5% dos casos. Por ser um modelo bivariado, sua capacidade
# explicativa é limitada.


# C)

regmu <- lm(VOTE ~ GROWTH + GOODNEWS, data = ecovote)


# I)

# A nova variável acrescentada no modelo de regressão foi GOODNEWS. Esta mede
# a quantidade de notícias positivas em relação ao governo vigente. Ela deve
# interagir positivamente com a variável dependente (% de votos do incumbente)
# pois boas notícias refletem um bom governo e o eleitor tende a recompensar 
# bons governos.

# II)

summary(regmu)

# A relação entre crescimento econômico e votos do incumbente permaneceu
# positiva e estatisticamente significante. A variável GOODNEWS também tem uma
# relação positiva com voto, contudo com um p-valor mais modesto de 0,022.
# Ainda sim, podemos dizer que existe significância estatística se considerarmos,
# como ponto de corte, o p-valor de 0,05. A cada boa notícia a mais, o voto do
# incumbente cresce em 0,71%.

# III)

# Ao acrescentarmos mais uma variável ao modelo, ele ficou mais ajustado do que
# o primeiro. O R² ajustado foi de 0,4269, isto é, o modelo explica, 
# aproximadamente, 42,7% dos casos. Em consonância com o R², o erro padrão dos
# resíduos foi de 4,596. Este valor também nos informa o quão ajustado é o nosso
# modelo com os dados reais. Ele calcula a raiz quadrada da média dos resíduos 
# ao quadrado divido pelo grau de liberdade, sendo seu valor "perfeito" igual a 0.

# IV)

# No modelo 1, o efeito de crescimento econômico sobre o voto do incumbente foi
# de 0,65 enquanto no modelo 2, ao adicionarmos a variável GOODNEWS, o efeito
# caiu para 0,57. O p-valor em ambos os modelos foram estatisticamente
# significantes, mas o modelo 2 apresentou um ajuste melhor aos dados. 

# V)

options(scipen=999)
ecovote$residuos <- residuals.lm(regmu)
ecovote$predict <- predict(regmu)

mean(ecovote$residuos)
# A média dos resíduos foi aproximadamente 0. Para observar se existe 
# heterocedasticidade no modelo, podemos ver se existe alguma relação entre os
# resíduos e os valores preditos pelo modelo. Caso os dados estejam 
# aleatoriamente distribuidos ao redor do 0, podemos dizer que o modelo não 
# tem heterocedasticidade. De fato, como mostra o gráfico abaixo, não há 
# relação entre essas variáveis.

theme_set(theme_classic())
ggplot(ecovote, aes(x = predict, y = residuos)) +
  geom_point() +
  geom_smooth(method = "lm", se = F)


# D)

regmu2 <- lm(VOTE ~ GROWTH + GOODNEWS + WAR, data = ecovote)
summary(regmu2)

# I)

# A variável WAR é uma variável binária que representa a presença (ou não)
# de guerras por ano. É provável que em tempos de guerra, o crescimento 
# econômico seja afetado negativamente. Porém, a relação com votos no incumbente
# é incerta, pois os eleitores podem apoiar o incumbente almejando estabilidade 
# que ajudaria na resulação da guerra, ou, punir o candidato mandatário por 
# causa da insatisfação de ter o país em guerra. Essa variável pode estar 
# relacionada a boas notícias, pois em tempos de guerra, é provável que a 
# quantidade de boas notícias sejam bem menores.


# II)

summary(regmu2)

# A relação de crescimento econômico e votos do incumbente continua 
# positiva (0,57) e estatisticamente significante a um p-valor menor que 0,000.
# As demais variáveis (GOODNEWS e WAR) estão positivamente correlacionadas a 
# variável dependente contudo o nível de significância foram, de ambas, menor 
# que 0,05. 

# III)

# O modelo apresenta um R² ajustado de 0,4065. Isto quer dizer que o modelo 
# explica aproxidamente 40,5% dos casos. O erro padrão dos resíduos foi de 
# 4,677 que representa o quão próximo os dados reais estão do modelo. 
# Quanto menor o seu valor, melhor.


# IV)

# Ambos os modelos mostram que o crescimento econômico e a quantidade de 
# votos obtidos pelo incumbente são positivos e estatisticamente significantes.
# Ao acrescentarmo uma variável ao modelo (GOODNEWS), o R² ajustado foi de 
# aproximadamente 0,43, contudo, esse valor cai para 0,41 ao adicionarmos a 
# variável WAR. Isto é, o modelo ficou levemente menos ajustado. Outro ponto 
# interessante é que a variável GOODNEWS foi estatísticamente significante a 
# um p-valor menor que 0,05, mas ao adicionarmos WAR no modelo, seu nível de 
# significância caiu para 0,06, tornando-se não significante a um ponte de 
# corte de 0,05. 

# V)

ecovote$residuos2 <- residuals.lm(regmu2)
ecovote$predict2 <- predict(regmu2)
print(mean(ecovote$residuos2))

# A média do resíduo foi aproximadamente 0. Para observar a presença de 
# heterocedasticidade vamos observar a relação entre resíduos e valores 
# preditos. Como o gráfico abaixo mostra, os resíduos se dispõem de forma 
# aleatória ao redor do 0. Isso indica presença de homocedasticidade no modelo.

ggplot(ecovote, aes(x = predict2, y = residuos2)) +
  geom_point() +
  geom_smooth(method = "lm", se = F)

# VI)

# A variável que apresenta maior efeito sobre os votos no incumbente foi 
# GOODNEWS. Esta apresentou um efeito de 0,72% para cada boa notícia que a
# mídia apresenta. Porém essa relação obteve um p-valor maior que 0,05. A 
# variável crescimento econômico é mais robusta vista que o p-valor foi 
# menor que 0,000 ainda que a magnitude do seu efeito (0,5729) seja menor 
# do que a de GOODNEWS.