## Ajustando a suavização via Médias Móveis Simples - MMS

# Limpando a memória 
rm(list = ls())

# Carregando os pacotes
library(readxl)
library(tidyverse)
library(TTR)

# Lendo os dados - Poluição
poluicao <- read_xls("series temporais/Aula 8/Aula 8/poluicao.xls", sheet = "Plan1")
View(poluicao)
poluicao %>% head()

# Tamanho da base dados
dim(poluicao)

# Os dados utilizados serão apenas do dia 1° de janeiro até 30 de abril

poluicao_red <- poluicao[1:120,c(1,5)]

poluicao_red %>% head()

# Transformando os dados em uma série temporal

poluicao_series <- ts(poluicao_red[,2])

# Transformando os dados em uma série temporal destacando-se os meses

poluicao_series2 <- ts(poluicao_series, start = c(1997,1), 
                      frequency = 365)

# Ajuste de Médias Móveis - 7 dias

poluicaoSMA7 <- SMA(poluicao_series2, n = 7)
poluicaoSMA7

plot(poluicao_series)
points(poluicaoSMA7, pch = 20)

# Ajuste de Médias Móveis - 14 dias

poluicaoSMA14 <- SMA(poluicao_series2, n = 14)
poluicaoSMA14

plot(poluicao_series)
points(poluicaoSMA14, pch = 20)

# Ajuste de Médias Móveis - 21 dias

poluicaoSMA21 <- SMA(poluicao_series2, n = 21)
poluicaoSMA21

plot(poluicao_series)
points(poluicaoSMA21, pch = 20)

# Previsão

poluicaoSMA7

# Exemplo 2 - Carregando a base Kings - idade da morte de sucessivos reis da Inglaterra

kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3) # ignora as 3 primeiras linhas da base
# save(kings, file = "kings.RData")
kings # avaliando a base
kingstimeseries <- ts(kings) # salvando os dados no formato de séries temporais (ST)
kingstimeseries # visualizando a série criada

# Estimando as MMS

kingstimeseriesSMA3 <- SMA(kingstimeseries,n=3) # estimar com uma média móvel simples de ordem 3
plot.ts(kingstimeseriesSMA3)

plot(kingstimeseries)
lines(kingstimeseriesSMA3, pch = 20, col = 2)

# Ainda existem muitas flutuações, assim, vamos estimar com uma média superior

kingstimeseriesSMA8 <- SMA(kingstimeseries,n=8)
plot.ts(kingstimeseriesSMA8)

plot(kingstimeseries)
lines(kingstimeseriesSMA8, pch = 20, col = 3,
      lwd = 2)
lines(kingstimeseriesSMA3, pch = 20, col = 2,
      lwd = 2)
