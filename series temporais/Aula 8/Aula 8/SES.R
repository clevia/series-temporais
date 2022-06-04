## Ajustando a suavização exponencial simples

# Limpando a memória 
rm(list = ls())

# Carregando os pacotes
library(readxl)
library(tidyverse)
library(TTR)

# Suavização Exponencial Simples

# Sendo possível descrever por meio do modelo aditivo 
# com nível constante e sem sazonalidade.
# A suaviazação ocorre pelo parâmetro alfa entre 0 e 1. 
# Sendo, 0 pouco peso nas observações
# mais recentes ao fazer previsões de valores futuros. 

######################################################

# Exemplo 1 - 

# Utilizando o pacote TTR

# Lendo os dados - Poluição
poluicao <- read_xls("poluicao.xls", sheet = "Plan1")

poluicao %>% head()

# Tamanho da base dados
dim(poluicao)

# Os dados utilizados serão apenas do dia 1° de janeiro até 30 de abril

poluicao_red <- poluicao[1:120,7]

poluicao_red %>% head()

# Transformando os dados em uma série temporal

poluicao_series <- ts(poluicao_red)

# Suavização exponencial simples via pacote TTR

ses_ex1 <- HoltWinters(poluicao_series, beta = F,gamma = F)
ses_ex1

# Valores estimados da série
ses_ex1$fitted

# Gráficos

plot(poluicao_series)
lines(ses_ex1$fitted[,1],pch = 18, col = "blue", type = "b", 
      lty = 2, lwd = 1)
# 4. Add a legend to the plot and set legend lty
legend("topleft", legend = c("Dados Originais", "SES"),
       col = c("black", "blue"), lty = 1:2, cex = 0.8)

# Exemplo 2 - Carregando a base chuva em Londres entre os anos de 1813-1912
# Fonte: Hipel and McLeod, 1994

rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
#save(rain, file = "rain.RData")
rainseries <- ts(rain,start=c(1813))
plot.ts(rainseries)

# A média permanece, quase, constante em aproximadamente em 25, o que indica o uso 
# de um modelo aditivo. 

# Vamos usar a função HoltWinters() para isso é preciso definir os parâmetros beta e gamma.
rainseriesforecasts <- HoltWinters(rainseries, beta=FALSE, gamma=FALSE)
rainseriesforecasts

# O valor estimado do parâmetro alfa é de 0.024. Como o valor é próximo a zero a previsão
# está baseada em observações recentes e menos recentes.Por default a previsão é feita apenas
# para o mesmo período avaliado na série temporal. Logo, entre os anos de 1813-1912.

rainseriesforecasts$fitted # avaliandos os valores estimados
plot(rainseriesforecasts)

# Como medida de previsão calculamos o erro da soma dos quadrados para os erros de previsão dentro da amostra. 

rainseriesforecasts$SSE # o valor do erro da soma dos quadrados
HoltWinters(rainseries, beta=FALSE, gamma=FALSE, l.start=23.56) # utilizando o primeiro valor previsto

# Gráficos
plot(rainseries)
lines(rainseriesforecasts$fitted[,1],pch = 18, 
      col = "blue", lty = 2, lwd = 1)
# 4. Add a legend to the plot and set legend lty
legend("topleft", legend = c("Dados Originais", "SES"),
       col = c("black", "blue"), lty = 1:2, cex = 0.8)
