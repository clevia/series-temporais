## Ajustando a suavização exponencial de Holt (SEH)

# Limpando a memória 
rm(list = ls())

# Carregando os pacotes
library(readxl)
library(tidyverse)
library(TTR)
library(forecast)

# Suavização Exponencial de Holt

# Holt's Suavização Exponencial

# Usado quando é possível utilizar um modelo aditivo com acréscimo ou decréscimo na tendência e sazonalidade
# O método estima o nível e a inclinação no ponto de tempo atual e é controlada por dois parâmetros alfa (ponto atual)
# e beta para inclinação do componente da tendência no ponto do tempo atual.
# Alfa e beta terão valores entre 0 e 1, sendo que próximo a zero temos pouco peso nas previsões mais recentes.

# Exemplo 1 - Índices de Custo de Vida em SP

icv <- read_xls("ICV.xls", sheet = "Plan1")
view(icv)

# Tamanho da base dados
dim(icv)

# Os dados utilizados serão apenas no período de janeiro de 1970
#  até junho de 1979.

icv_red <- icv[1:114,2]

icv_red %>% head()

# Transformando os dados em uma série temporal

icv_series <- ts(icv_red)

# Visualização gráfica

plot.ts(icv_series)

# Ajustando a suavização exponencial de Holt

seh_icv <- HoltWinters(icv_series, gamma = FALSE)
seh_icv

# Previsões

prev_icv <- forecast(seh_icv, h = 12)
plot(prev_icv)

# Exemplo 2 - Carregando a base skirts - diâmetro anual das saias femininas na bainha, de 1866 a 1911
# Fonte: McLeod, 1994

skirts <- scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5)
# save(skirts, file = "skirts.RData")
skirtsseries <- ts(skirts,start=c(1866))
plot.ts(skirtsseries)

# É preciso configurar os parâmetros gama
skirtsseriesforecasts <- HoltWinters(skirtsseries, gamma=FALSE)
skirtsseriesforecasts

# O valor do alpha foi de 0.83 e beta 1. Os valores são altos e indicam a estimativa do valor atual do nível,
# quando a inclinação do componente de tendência se baseiam principalmente em observações recentes da série.

skirtsseriesforecasts$SSE

# Assim, o nível e a inclinação mudam ao longo do tempo. O valor da soma dos erros quadrados é 16954.

plot(skirtsseriesforecasts) # atenção para lag antes dos dados observados na previsão.
skirtsseries

# para corrigir o nível do valor inicial, e a diferença entre a segunda e a primeira observação
HoltWinters(skirtsseries, gamma=FALSE, l.start=608, b.start=9) 

# Prevendo 19 pontos a mais que a série temporal
skirtsseriesforecasts2 <- forecast(skirtsseriesforecasts, h=19)

# linha azul representa com intervalos de predição de 80% com uma área sombreada em azul escuro e os 
# intervalos de predição de 95% com a área na cor clara 
plot(skirtsseriesforecasts2)
