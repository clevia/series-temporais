## Ajustando a suavização exponencial de Holt-Winters (SEHW)

# Limpando a memória 
rm(list = ls())

# Carregando os pacotes
library(readxl)
library(tidyverse)
library(TTR)
library(forecast)

# Suavização Exponencial de Holt Winters

# Holt-Winters Suavização Exponencial

# Caso tenha uma série que pode ser descrita por meio de modelos aditivos, tendência crescente
# ou decrescente e sazonalidade, o uso da suavização exponencial de Holt-Winders é indicada 
# para previsões de curto prazo

# Estima o nível, inclinação e componente sazonal no ponto de tempo atual. A suavização é
# controlada por três parâmetros: alfa, beta e gama para estimar o nível, inclinação e o 
# componente de tendência e sazonal a partir do ponto atual. Os parâmetros variam entre 0 e 1.
# Valores próximos a 0 significam que é colocado relativamente pouco peso nas observações mais 
# recentes ao fazer as previsões.

# Exemplo 1 - Dados sobre IPI

ipi <- read_xls("IPI.xls", sheet = "Plan1")
ipi

# Tamanho da base dados
dim(ipi)

# Os dados utilizados serão apenas do dia 1° de janeiro até 30 de abril

ipi_red <- ipi[1:127,2]

ipi_red %>% head()

# Transformando os dados em uma série temporal

ipi_series <- ts(ipi_red, frequency = 12)

# Suavização exponencial simples via pacote TTR

sehw_ex1 <- HoltWinters(ipi_series)
sehw_ex1

# Valores estimados da série
sehw_ex1$fitted

# Gráficos

plot(ipi_series)
lines(sehw_ex1$fitted[,1],pch = 18, col = "blue", type = "b", 
      lty = 2, lwd = 1)
# 4. Add a legend to the plot and set legend lty
legend("topleft", legend = c("Dados Originais", "SEHW"),
       col = c("black", "blue"), lty = 1:2, cex = 0.6)

# Exemplo 2 - Carregando a base Souvenir - venda de souvernirs entre Janeiro de 1987 a Dezembro de 1993
# Fonte: Wheelwright and Hyndman, 1998

souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat") 
# carregando a base (caso não esteja carregada)
souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1))  
# salvando o período de início, mês 01 de 1987

logsouvenirtimeseries <- log(souvenirtimeseries)
souvenirtimeseriesforecasts <- HoltWinters(logsouvenirtimeseries)
souvenirtimeseriesforecasts

# Os valores estimados de alfa, beta e gama são 0.41, 0.00 e 0.95. O alfa é relativamente baixo
# indicando que a estimativa do nível no momento atual é baseada em observações no passado mais
# distante. O valor de beta indica que a estimativa da inclinação b do componente de tendência não
# é atualizado ao longo da série temporal e, em vez disso, é definida igual ao valor inicial. Assim,
# o nível muda bastante ao longo da série temporal, mas a inclinaçào do componente de tendência
# permanece praticamente a mesma. Já o valor gama é alto, indicandp que a estimativa do componente
# sazonal no momento atual é baseada apenas em observações recentes.

plot(souvenirtimeseriesforecasts)

# A técnica consegue prever os picos sazonais que ocorrem nos meses finais do ano.
# Vamos agora prever períodos que não estão na base, ou seja, de 1994 a 1998 (48 m)

souvenirtimeseriesforecasts2 <- forecast(souvenirtimeseriesforecasts, h=48)
plot(souvenirtimeseriesforecasts2)

# As previsões são mostradas com uma linha azul e as áreas sombreadas em cores claras e escuras mostram intervalos de previsão de 80% a 95%. 

# Os erros de previsão têm variância constante ao longo do tempo e são normalmente distribuídos com a média zero, fazendo um gráfico de tempo de erros de previsão e um histograma.

plot.ts(souvenirtimeseriesforecasts2$residuals)           

# É compreensível que os erros de previsão tenham variação constante ao longo do tempo. A partir do histograma de erros 
# de previsão, os erros de previsão parecem ser normalmente distribuídos com média zero. Assim, há pouca evidência de 
# autocorrelação nas defasagens 1-20 para os erros de previsão, e os erros de previsão parecem ser normalmente distribuídos 
# com média zero e variância constante ao longo do tempo. Assim, a suavização exponencial de Holt-Winters fornece um modelo 
# preditivo adequado e que provavelmente não pode ser melhorado. Além disso, as suposições nas quais os intervalos de 
# predição foram baseados são provavelmente válidas.