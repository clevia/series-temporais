### Aula 7 

# Exemplos


# Método de Suavização Lowess

zt <- c(84.6,89.9,81.9,95.4,91.2,89.8,89.7,97.9,103.4,107.6,120.4,
            109.6,110.3,118.1,116.5,134.2,134.7,144.8,144.4,159.2,168.2,
            175.2,174.5,173.7)
tempos <- 1:24

estimativas_lowess_1 <- lowess(zt,tempos, f = 2/3)
estimativas_lowess_2 <- lowess(zt,tempos, f = 1/3)

par(mfrow = c(1,2))
plot(tempos,zt, 
     xlab = "Tempos", ylab = "Consumo de Energia", 
     type = "l")
points(estimativas_lowess_1$y,estimativas_lowess_1$x)

plot(tempos,zt, 
     xlab = "Tempos", ylab = "Consumo de Energia", 
     type = "l")
points(estimativas_lowess_2$y,estimativas_lowess_2$x)
par(mfrow = c(1,1))

# Exemplo para diferenças

# Dados

zt <- c(84.6,89.9,81.9,95.4,91.2,89.8,89.7,97.9,103.4,107.6,120.4,
        109.6,110.3,118.1,116.5,134.2,134.7,144.8,144.4,159.2,168.2,
        175.2,174.5,173.7)

# Tranformando em dados de séries temporais
dados <- ts(cbind(zt))

# Calculando a primeira diferença

diff_1 <- diff(dados, differences = 1)
diff_1
plot(diff_1)

# Calculando a segunda diferença
diff_2 <- diff(dados, differences = 2)
plot(diff_2)

