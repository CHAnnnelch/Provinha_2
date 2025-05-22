rm(list=ls())

library(wooldridge)
library(tidyverse)
library(dplyr)
library(performance)
library(see)
library(ggplot2)

cement <- wooldridge::cement
head(cement)

?cement

par(mfrow = c(1,2))
plot(cement$year, cement$ipcem, type = "l", col = "red",
     main = "Produção Industrial",
     xlab = "Ano", ylab="Produção")
grid()

plot(cement$year, cement$rresc, type = 'l', col = 'red',
     main = "Construção Residencial",
     xlab = "Ano", ylab= "Construção")
grid()

modelo_ts <- lm(ipcem ~ milemp, data=cement)
summary(modelo_ts)

cement$milemp_1 <- c(NA, head(cement$milemp, -1))
cement$milemp_2 <- c(NA, NA, head(cement$milemp, -2))
cement$milemp_3 <- c(NA, NA, NA, head(cement$milemp, -3))

cement <- na.omit(cement)

modelo_ddf <- lm(ipcem ~ milemp + milemp_1 + milemp_2 + milemp_3, 
                 data=cement)
summary(modelo_ddf)

M <- cor(cement[, c("milemp", "milemp_1", "milemp_2", "milemp_3")])

par(mfrow=c(1,1))
library(corrplot)
corrplot(M, method="circle")

library(car)
library(lmtest)

vif(modelo_ddf)

reset_test <- reset(modelo_ddf, power=3:4, type="fitted")
reset_test

plot(modelo_ddf$fitted.values, modelo_ddf$residuals,
     main='Grafico Residuos',
     xlab="Fitted", ylab="Residuals")
abline(h=0, col='red')

bptest(modelo_ddf)

dwtest(modelo_ddf)

bgtest(modelo_ddf, order = 1)

bgtest(modelo_ddf, order = 3)

acf(modelo_ddf$residuals)

qqnorm(modelo_ddf$residuals)
qqline(modelo_ddf$residuals)

check_model(modelo_ddf)