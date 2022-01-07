##Q1.
???????? = (1 ??? 0.4????)(1 ??? 0.8????
                   4
)???????





## Q2.
The possible CYCLES ARE 10 and 1/0.35 (3)


## Q5

install.packages("FinTS")

library(FinTS)

data(m.ibmspln)

plot(m.ibmspln)

acf(m.ibmspln$SP)
pacf(m.ibmspln$SP)


plot((m.ibmspln$SP)^2)

acf((m.ibmspln$SP)^2)

pacf((m.ibmspln$SP)^2)

arima(m.ibmspln$SP, order=c(3,0,0))

# mean is 0.54

iV)

install.packages("rugarch")

library(rugarch)


egarchspec=ugarchspec(variance.model = list(model = "fGARCH",submodel="GARCH", garchOrder = c(1,1)),
                      mean.model=list(armaOrder=c(3,0), include.mean=TRUE))

fit = ugarchfit(data = m.ibmspln$SP, spec = egarchspec)
## AR terms are insignificant


V)  



egarchspec=ugarchspec(variance.model = list(model = "fGARCH",submodel="GARCH", garchOrder = c(1,1)))

fit = ugarchfit(data = m.ibmspln$SP, spec = egarchspec)


as=ugarchforecast(fit,10)


plot(as)



#Q4.

#eq=1/(1-0.8B+0,6G^2)
b <- c(1,-0.8,0.6)

# roots
polyroot(b)
