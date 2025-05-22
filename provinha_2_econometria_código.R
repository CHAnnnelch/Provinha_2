rm(list=ls())

library(ggplot2)
library(dplyr)
library(lmtest)
library(car)
library(tseries)

base <- ggplot2::economics

base <- filter(base, base$date < "2008-01-01")

base$date <- as.Date(base$date)

base$mes <- factor(format(base$date, "%m"))

dummies_mes <- model.matrix(~ mes - 1, data = base)

base <- cbind(base, dummies_mes)

base$tempo <- 1:nrow(base)

base$psavert_1 <- c(NA, head(base$psavert, -1))
base$pce_1 <- c(NA, head(base$pce, -1))
base$psavert_l2 <- c(NA, head(base$psavert_1, -1))

base$log_pce <- log(base$pce)

base <- na.omit(base)

par(mfrow=c(1,2))
plot(base$date, base$psavert, type = "l", col="blue",
     main="Taxa de Juros (1967-2007)",
     xlab="Ano", ylab="Taxas de Juros (%)")
grid()

plot(base$date, base$log_pce, type = "l", col="blue",
     main="Log do consumo em bilhões (1967-2007)",
     xlab="Ano", ylab="Log do gasto com consumo")
grid()

modelo_ddf <- lm(log_pce ~ psavert + psavert_1 + psavert_l2 + tempo, data=base)
summary(modelo_ddf)

modelo_ddf_dummies <- lm(log_pce ~ psavert + psavert_1 + psavert_l2 + tempo + mes01 + mes02 +
                   mes03 +
                   mes04 +
                   mes05 +
                   mes06 +
                   mes07 +
                   mes08 +
                   mes09 +
                   mes10 +
                   mes11 +
                   mes12,
                 data=base)

library(performance)
library(see)

#todas os pressupostos
check_model(modelo_ddf)

compare_performance(modelo_ddf, modelo_ddf_dummies)
#as dummies pioram o AIC, BIC e R2 ajustado

#gráfico dos resíduos
par(mfrow=c(1,1))
plot(modelo_ddf$fitted.values, modelo_ddf$residuals,
     main="Residuais vs. Fitted",
     xlab="Fitted", ylab="Residuais")
abline(h=0, col='red')

#multicolinearidade
M <- cor(base[,c("psavert", "psavert_1", "psavert_l2", "tempo")])
M

library(corrplot)
corrplot(M, type="full")

vif(modelo_ddf)

#heteroscedasticidade
bptest(modelo_ddf)

#independencia dos residuos
dwtest(modelo_ddf)

#media condicional zero
reset_test <- reset(modelo_ddf, power=1:2)
print(reset_test)

#normalidade dos resíduos
par(mfrow=c(1,1))
qqnorm(modelo_ddf$residuals)
qqline(modelo_ddf$residuals)

shapiro.test(modelo_ddf$residuals)

#acf e pacf plot
par(mfrow=c(1,2))
acf(modelo_ddf$residuals, 
    main="Gráfico ACF")
grid()

pacf(modelo_ddf$residuals,
     main="Gráfico PACF")
grid()

#equação
library(equatiomatic)
extract_eq(modelo_ddf)

#modelos em latex
library(stargazer)
stargazer(modelo_ddf, type="latex", align=TRUE, title="Resultados do modelo DDF",
          single.row=TRUE)

library(xtable)
xtable(M)