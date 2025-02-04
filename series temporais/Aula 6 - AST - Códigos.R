# Aula 6 - An�lise de S�ries Temporais

# Carregando o pacote

library(tseries)

# Criando um Ru�do Branco

serie <- rnorm(1000)

# Tranformando o Ru�do Branco em uma s�rie temporal
serie <- ts(serie,start =c(2000,01), freq = 12)

# Gr�fico

plot(serie)

# Exemplo - Dados sobre Pre�os di�rios de uma a��o

tempos <- c(47.9,46.0,45.8,48.9,49.4,50.7,50.6,51.2,50.1,51.3)
serie_prec <- ts(tempos)

# Gr�fico para a S�rie

plot(serie_prec)

# Exemplo - Dados sobre os �ndices do Produto Industrial do Brasil.



# Exemplo 1

t <- 1:24
zt <- c(84.6,89.9,81.9,95.4,91.2,89.8,89.7,97.9,103.4,107.6,120.4,109.6,100.3,118.1,116.5,134.2,134.7,144.8,144.4,159.2,168.2,175.2,174.5,173.7)

Zt <- ts(zt)

plot(Zt,
     xlab = "Tempos",
     ylab = "Consumo de Energia")

# Modelo de Regress�o Polinomial de Grau 1

lm(Zt~t)

# curva de m�nimos quadrados

abline(a = 68.076, b = 4.238, col = 2)

# Exemplo das diferen�as

diff(zt)
dif1 <- ts(diff(zt, lag = 1))

plot(dif1)
