

Q2.

yt=c(-0)

wt=rnorm(1000)


for( i in 2:1000)
{
  yt[i]=0.8*yt[i-1]+ wt[i]- 0.45*wt[i-1]
  
}

plot(yt,type="l")

acf1=acf(yt)

acf1$acf

pacf1=pacf(yt)

pacf1$acf




Q3.


set.seed(8675309)
arma = arima.sim(list(order=c(1,0,1), ar=c(0.7),ma=c(-0.4)), n = 72,innov = rnorm(72))
win.graph(width=4.875,height=3,pointsize=8)
plot(arma, axes=FALSE, xlab="Time")
axis(2); axis(1, at=seq(0,144,by=12)); box()
abline(v=seq(0,144,by=12), lty=2)

ar.yw(arma, order=1)
##estimates of p,q for the equation
acf_1=acf(arma)
pacf(arma)
acf_1$acf

yt=c(0)

wt=rnorm(72)

for( i in 2:72)
  
{
  yt[i]=0.7*yt[i-1]-0.4*wt[i-1] #+wt[i]
}

series=arima.sim(n=72,list(ar=0.7,ma=-0.4))

arima(series,order=c(1,0,1),method='CSS')
arima(series,order=c(1,0,1),method='ML')

plot(series)

plot(yt,type="l")

acf(series)

m1=auto.arima(yt)

m1

acf_value=acf(yt)

acf_value$acf

pacf(yt)


install.packages("TSA")
library(TSA)


data(hare)


plot(hare)


can_hare=acf(hare)



can_hare


Q4.

install.packages("fma")

library(fma)

i) tsdisplay(bicoal)

aa=tsdisplay(bicoal)

ii) (p,d,q) -> (4,0,0)

iii) The PACF 


iv) 

aa=pacf(bicoal)

aa$acf

[1,]  0.678627017
[2,] -0.030562125
[3,]  0.215794551
[4,] -0.333380608


library(fma)

auto.arima(bicoal)

lm(bicoal~time(bicoal))

plot(bicoal)
ggseasonplot(bicoal)

dec1=decompose(bicoal)

dec$x

dec$trend
##estimating the mean

mean1=mean(bicoal)


bicoal


484- 0.678627017*565 + 0.030562125* 422 - 0.215794551*416 + -0.333380608*569
