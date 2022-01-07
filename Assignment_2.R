yt=c(-100)

wt=rnorm(200)





for( i in 2:200)
{
  yt[i]=0.4*yt[i-1]+ wt[i]
  
}


plot(yt)
d1=acf(yt)
d1$acf

## How does this happen if all the coefficients are supposed to be alternating


pacf(yt)

#########################################################3

Q1. 

(1-0.9*B^2)Zt=wt


1-0.9*B^2=0

#Roots are

sqrt(10/9)

+- 1.054 #lies outside the unit circle thus the series converges

yt=c(-1,0)

wt=rnorm(200)

for( i in 3:200)
{
  yt[i]=0.9*yt[i-2]+ wt[i]
  
}

plot(yt,type = "l")

acf(yt)

pacf(yt)



Q2.##Homogenous stationary it is a non stationary process

wt=rnorm(1000,100)

acf(wt)

pacf(wt)

wt1= diff(wt)

acf(wt1)

pacf(wt1)



Q3. #wronmg approach taken here

#Equation :(1-2B+B^2)Zt=(1-0.3B-0.5B^2)Wt


zt=c(0,0)

wt=rnorm(1000)


for( i in 3:1000)
{
  zt[i]=2*zt[i-1]-1*zt[i-2]+wt-0.3*wt[i-1]-0.5*wt[i-2]
  
}

plot(zt,type = "l")

acf(zt)

pacf(zt)

#why is the pacf significant at the lag 1 alone


i)

Cov(zt,zt-k)

## check with the prof


ii)


#
vt=c(0,0,0)

wt=rnorm(1000)



for( i in 3:1000)
{
  vt[i]=  -2*zt[i-1]+1*zt[i-2]+zt[i]
  
}

plot(vt,type = "l")

acf(vt)

pacf(vt)

#not stationary


iii)

plot(diff(vt,2))

acf(diff(vt,2))



Q5.

install.packages("Rtools")
library('Rtools')

install.packages("expsmooth")

library(expsmooth)


data(mcopper)

plot(mcopper)

## Differencing

plot(diff(mcopper))


plot(diff(mcopper,2))


mcopper_2=BoxCox(mcopper,lambda = "auto")

lambda <- BoxCox.lambda(mcopper,lower=-2)

plot(mcopper_2)

plot(diff(mcopper_2))

acf(diff(mcopper_2))
pacf(diff(mcopper_2))



## evidence of overdifferencing
plot(diff(mcopper_2,2))

acf(diff(mcopper_2,2))

pacf(diff(mcopper_2,2))


Q6.


z <- ts(numeric(100))
w <- rnorm(100)
for(i in 2:100)
  z[i] <- 0.2*z[i-1] + w[i]

plot(z)
acf(z)
pacf(z)



Q7.

z <- ts(numeric(100))
w <- rnorm(100)

for(i in 2:100)
{
  z[i]=z[i-1] + 5+ w[i]-0.6*w[i-1]
}

plot(z)

acf(z)

pacf(z)

acf(diff(z))
pacf(diff(z))

##
