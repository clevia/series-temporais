#################################################################################
################### Lista 1 de exercicio S�ries Temporais #######################


rm(list = ls())

#banco de dados utilizado para resolu��o da lista

library(readxl)
bebida <- read_excel("series temporais/Conjuntos de dados.XLS", 
                                 sheet = "M-Bebida")
energia <- read_excel("series temporais/Conjuntos de dados.XLS", 
                                 sheet = "M-Energia")
IPI <- read_excel("series temporais/Conjuntos de dados.XLS", 
                                 sheet = "M-IPI")

#pacotes utilizados

library(tidyverse)
library(TTR)
library(forecast)

# Quest�o 1
#Considerando os dados (M-Energia) do anos de janeiro de 1968 � dezembro de 1969
#sobre consumo de energia el�trica no estado do Esp�rito Santo:
# letra a - Fa�a um gr�fico das observa��es.
#letra b - Estime a tend�ncia por meio da utiliza��o de um polin�mio de primeira ordem.
#letra c - Calcule as m�dias m�veis por trimestre e por quadrimestre. Em seguida, adicione
#essas informa��es ao gr�co das observa��es obtido no item a).

#selecionando o periodo de tempo de janeiro de 1968 � dezembro de 1969

energia2 <- energia[1:24,3]

#transformando em s�rie temporal 

energia_serie <- ts(energia2, start=1968, freq=12)
View(energia_serie)

#linha de tend�ncia

N <- nrow(energia_serie)
Tempo <- 1:N
X.lm <- lm(energia_serie ~ Tempo)
X.pred <- predict(X.lm) ## Previs�o da s�rie observada
M <- seq(as.Date("1968/1/1"),as.Date("1969/12/1"),"months")
plot(M, energia_serie, type="l")
lines(M, X.pred, col= "red", lwd=2,lty=2)

#media m�vel por trimestre

energiaM3<- SMA(energia_serie, n=3)

plot(energia_serie)
points(energiaM3, pch=20)
plot(energiaM3)
#media movel por quadrimestre

energiaM4<- SMA(energia_serie, n=4)

plot(energia_serie)
points(energiaM4, pch=20)


#quest�o 2 

#Analise cada um dos conjuntos de dados abaixo e ajuste um modelo para tend�ncias,
#sazonalidade ou contendo ambas de acordo com o que for adequado a cada problema.

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


# quest�o 3
#Analise cada um dos conjuntos de dados abaixo e ajuste um modelo de suaviza��o
#exponencial de acordo com o que for adequado a cada problema.

#letra a

#transformando bebida em serie temporal

bebida_serie <- ts(bebida[,2], start=1985, freq=12)

#Gr�fico

plot(bebida_serie)
N2 <- nrow(bebida_serie)
Tempo <- 1:N2
X.lm2 <- lm(bebida_serie ~ Tempo)
X.pred2 <- predict(X.lm2) ## Previs�o da s�rie observada
M2 <- seq(as.Date("1985/1/1"),as.Date("2000/7/1"),"months")
plot(M2, bebida_serie, type="l")
lines(M2, X.pred2, col= "red", lwd=2,lty=2)


#neste caso iremos utilizar a Suaviza��o Exponencial de Holt 
#pois a serie posui tendencia 

modelo_bebida <- HoltWinters(bebida_serie, gamma = FALSE)
modelo_bebida

#valores estimados da serie

modelo_bebida$fitted

#gr�fico 

plot(bebida_serie)
lines(modelo_bebida$fitted[,1],pch = 18, col = "red", type = "b", 
      lty = 2, lwd = 1)

#letra b

#transformando IPI em serie temporal

IPI_serie <- ts(IPI[,2], start=1985, freq=12)

#Gr�fico

plot(IPI_serie)
N3 <- nrow(IPI_serie)
Tempo <- 1:N3
X.lm3 <- lm(IPI_serie ~ Tempo)
X.pred3 <- predict(X.lm3) ## Previs�o da s�rie observada
M3 <- seq(as.Date("1985/1/1"),as.Date("2000/7/1"),"months")
plot(M3, IPI_serie, type="l")
lines(M3, X.pred3, col= "red", lwd=2,lty=2)

# utilizando a Suaviza��o Exponencial de Holt Winters
#pois a serie possui tendencia de sazonalidade aditiva

modelo_IPI <- HoltWinters(IPI_serie)
modelo_IPI

#valores estimados da serie

modelo_IPI$fitted

#gr�fico 

plot(IPI_serie)
lines(modelo_IPI$fitted[,1],pch = 18, col = "red", type = "b", 
      lty = 2, lwd = 1)

#letra c 

#transformando em serie temporal

energia2_serie <- ts(energia[,2], start=1968, freq=12)

#Gr�fico

plot(energia2_serie)
N4 <- nrow(energia2_serie)
Tempo <- 1:N4
X.lm4 <- lm(energia2_serie ~ Tempo)
X.pred4 <- predict(X.lm4) ## Previs�o da s�rie observada
M4 <- seq(as.Date("1968/1/1"),as.Date("1979/9/1"),"months")
plot(M4, energia2_serie, type="l")
lines(M4, X.pred4, col= "red", lwd=2,lty=2)

# Ajustando a suaviza��o exponencial Simples
# pois a serie n�o possui tendencia

modelo_energia2 <- HoltWinters(energia2_serie, beta = F,gamma = F)
modelo_energia2

#valores estimados da serie

modelo_energia2$fitted

#gr�fico 

plot(energia2_serie)
lines(modelo_energia2$fitted[,1],pch = 18, col = "red", type = "b", 
      lty = 2, lwd = 1)




