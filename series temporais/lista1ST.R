library(tidyverse)
library(TTR)
library(forecast)
library(readxl)

bebida <- read_excel("series temporais/Conjuntos de dados.XLS", 
                     sheet = "M-Bebida")
energia <- read_excel("series temporais/Conjuntos de dados.XLS", 
                      sheet = "M-Energia")
IPI <- read_excel("series temporais/Conjuntos de dados.XLS", 
                  sheet = "M-IPI")

#questão 1

#selecionando o periodo de tempo de janeiro de 1968 à dezembro de 1969
energia2 <- energia[1:24,3]
#transformando em série temporal 

energia_serie <- ts(energia2, start=1968, freq=12)
View(energia_serie)

N <- nrow(energia_serie)
Tempo <- 1:N
X.lm <- lm(energia_serie ~ Tempo)
X.pred <- predict(X.lm) 
M <- seq(as.Date("1968/1/1"),as.Date("1969/12/1"),"months")
plot(M, energia_serie, type="l")
lines(M, X.pred, col= "red")

#media móvel por trimestre

energiaM3<- SMA(energia_serie, n=3)

plot(energia_serie)
points(energiaM3, pch=20)

#media movel por quadrimestre

energiaM4<- SMA(energia_serie, n=4)

plot(energia_serie)
points(energiaM4, pch=20)

###################################################################################
#questão 2 

#letra a
#modelo aditivo 

bebida_serie <- ts(bebida[,2], start=1985, freq=12)

bebida_adt <- decompose(bebida_serie)
plot(bebida_adt)

# para o modelo multiplicativo

bebida_mult =decompose(bebida_serie,type = "mult")
plot(bebida_mult)

# estimar tendencia

bebida_trend <- bebida_mult$trend
plot(bebida_trend)

#letra b 

IPI_serie <- ts(IPI[,2], start=1985, freq=12)

#modelo aditivo 

IPI_adt <- decompose(IPI_serie)
plot(IPI_adt)

# para o modelo multiplicativo

IPI_mult =decompose(IPI_serie,type = "mult")
plot(IPI_mult)

# estimar tendencia

IPI_trend <- IPI_mult$trend
plot(IPI_trend)

#letra c 

energia2_serie <- ts(energia[,2], start=1968, freq=12)
#modelo aditivo 

energia2_adt <- decompose(energia2_serie)
plot(energia2_adt)

# para o modelo multiplicativo

energia2_mult =decompose(energia2_serie,type = "mult")
plot(energia2_mult)

# estimar tendencia

energia2_trend <- energia2_mult$trend
plot(energia2_trend)

#################################################################################

# questão 3

#letra a

#transformando bebida em serie temporal

bebida_serie <- ts(bebida[,2], start=1985, freq=12)

#Gráfico

plot(bebida_serie)

#neste caso iremos utilizar a Suavização Exponencial de Holt 
#pois a serie posui tendencia 

modelo_bebida <- HoltWinters(bebida_serie, gamma = FALSE)
modelo_bebida

#valores estimados da serie

modelo_bebida$fitted

#gráfico 

plot(bebida_serie)
lines(modelo_bebida$fitted[,1],pch = 18, col = "red", type = "b", 
      lty = 2, lwd = 1)

#letra b

#transformando IPI em serie temporal

IPI_serie <- ts(IPI[,2], start=1985, freq=12)

#Gráfico

plot(IPI_serie)
# utilizando a Suavização Exponencial de Holt Winters
#pois a serie possui tendencia de sazonalidade aditiva

modelo_IPI <- HoltWinters(IPI_serie)
modelo_IPI

#valores estimados da serie

modelo_IPI$fitted

#gráfico 

plot(IPI_serie)
lines(modelo_IPI$fitted[,1],pch = 18, col = "red", type = "b", 
      lty = 2, lwd = 1)

#letra c 

#transformando em serie temporal

energia2_serie <- ts(energia[,2], start=1968, freq=12)

#Gráfico

plot(energia2_serie)

# Ajustando a suavização exponencial Simples
# pois a serie não possui tendencia

modelo_energia2 <- HoltWinters(energia2_serie, beta = F,gamma = F)
modelo_energia2

#valores estimados da serie

modelo_energia2$fitted

#gráfico 

plot(energia2_serie)
lines(modelo_energia2$fitted[,1],pch = 18, col = "red", type = "b", 
      lty = 2, lwd = 1)


