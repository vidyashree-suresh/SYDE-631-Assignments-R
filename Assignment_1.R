Yt=c(0)
Wt=c(0)

Wt=rnorm(20)


#par(fig = c(1,1), new = TRUE) # the insert

par(mfrow=c(1,1))

t=1:20
plot((t+Wt),type="l")

for(i in 2:20)
{
  
  Yt[i]=i + wt[i]
}

plot(Wt,type="l")

plot(Yt,type="l")

acf_yt<-acf(Yt,7)

diff_y=diff(Yt)

plot(diff_y,type = "l")


acf(diff_y,7)


wt=rnorm(1000)

Yt=c(0)

#Zt=wt+0.5Zt-1

for()


for(i in 2:1000)
{
  Yt[i]=1*Yt[i-1]+wt[i]
  
}

acf(Yt)

#Gives a linearly increasing trend 

#The diff of the series gives the plot for ACF which has less signficant value after coordinate




#########################################


#1
wt=rnorm(1000)

Yt=c(0)

#Zt=wt+0.5Zt-1


for(i in 2:100)
{
  Yt[i]=0.5*Yt[i-1]+wt[i]
  
}

plot(Yt,type="l")

plot(acf(Yt,5))

plot(pacf(Yt,5))

acf1=acf(yt,5
         )


wt=rnorm(1000)

Yt1=c(0,0)

for(i in 3:1000)
{
  Yt1[i]=+1.2*Yt1[i-1]-0.8*Yt1[i-2]+wt
  
}

plot(Yt1, type="l")

plot(acf(Yt1,50))  #damping signal

plot(pacf(Yt1,50)) # gives the AR component of the time series models 

#Q3

t=c(1.6, 0.8, 1.2, 0.5, 0.9, 1.1, 1.1, 0.6, 1.5, 0.8, 0.9, 1.2, 0.5, 1.3, 0.8, 1.2)

plot(ts(t),type="l")

acf=acf(t)

acf$acf


n=1

plot(t[1:(length(t)-n)],t[(n+1):length(t)]) ## type =l is a problem because the lines are connected, now it gives the negative correlation


p1=E[(xt-Ut) (x(t+1)-Ut)]
  
acf(t)
pacf(t)


ybar=mean(t)

rand_pro=t

mean=mean(rand_pro)

#S0
var=sum((rand_pro-mean)^2/length(rand_pro))

#s1

md1<-(rand_pro[1:15]-mean)
md2<-(rand_pro[2:16]-mean)

s1=sum(md1*md2)/length(rand_pro)

cov<-stats::acf(rand_pro,type="covariance")

cov

p1= s1/var ##perfect

# Q5
Xt-X12t


Wt12=c(rep(0,12))


for(i in 13:length(Wt))
{
  print(i)
  Wt12[i]=Wt[i] -Wt[i-12]
  
  
}


pacf(Wt12)

plot(ts(rnorm(2000),frequency = 12))


pacf(ts(rnorm(2000),frequency=12)) #how to read this plot?

acf(Wt12)

#solution

Xt=(a+bt+St+Wt)
Xt-12 = a+b(t-12)+St-12+Wt-12

Xt-Xt12= bt+St+Wt-bt+12b-St-12-Wt-12


b) The double seasonal differencing gives the equation with just white noise

wt-2wt-12+wt-24







#5 b)

install.packages("forecast")

library(forecast)
#Q5 a)


t=seq(1,200,1)


plot(sin(2*3.1414/12*t),type="l")


sint=sin(2*3.1414/12*t)


model <- Arima(ts(rnorm(200),freq=12), order=c(0,0,0), seasonal=c(1,0,0), fixed=c(theta=-0.8, Theta=-0.8))
x <- simulate(model, nsim=200)


Wt=rnorm(200)

plot(x)
xt=c(0)

for(i in 1:200)
  
{

  
xt[i]=0.02+0.071* i+Wt[i]+sint[i]  
  
  
} #key is giving the right coeffecients 


plot(xt,type = "l")


rt=c(rep(0,12))


for (i in 13:200)
{
  print(i)
  rt[i]=xt[i]-xt[i-12]
  
}


acf(xt)

acf(rt,40)


acf(diff())
#Q5. b)



Wt=rnorm(200)

plot(x)
xt=c(0)

for(i in 1:200)
  
{
  
  xt[i]=(-0.02+0.071* i)*sint[i]+Wt[i]
  
  
} #key is giving the right coeffecients 


plot(xt,type="l")


rt=c(rep(0,12))


for (i in 13:200)
{
  print(i)
  rt[i]=xt[i]-xt[i-12]
  
}


plot(rt,type="l")
acf(rt)


acf(diff(diff(rt)))

acf(log(rt[13:200]))
#?????????????
##############################################################

#Q6

zt=c(0)

wt=rnorm(200,sd=0.012)

acf(wt)


for(i in 2:200)
{
  zt[i]= 0.75*zt[i-1]+wt[i]-0.12
  
}

plot(zt)



ln(yt)-ln(yt-1)= ln(yt/(yt-1))



yt/(yt-1)= e^zt




(yt-1)/yt=1/e^zt


1-1/yt=1/e^zt


1-1/e^zt=1/yt #ct


ct=c(0)



for(i in 2:200)
{
  ct[i]=1-1/(exp(zt[i]))
  
  
}

ct=1/ct

plot(ct)


plot(zt)

##########

#Q7.


install.packages("fpp2")

library(fpp2)

help("usdeaths")


data("usdeaths")

plot(usdeaths)

plot(goog)


library(ggplot2)


autoplot(usdeaths)
ggseasonplot(usdeaths)

ggAcf(usdeaths)


autoplot(goog)

ggseasonplot(goog)

ggAcf(goog)


Q8.

dec=decompose(plastics)

dec$x

dec$trend


plot(plastics)

lines(forecast::seasadj(decompose(plastics) ))

plot(p1)

ggseasonplot(plastics)


plastics_1=plastics


plastics_1[30]=10000

ggseasonplot(plastics_1)

p2=seasadj(decompose(plastics_1) ) 



##end of assignment 1












#

wt=rnorm(1000)+100

plot(wt,type="l")

acf(wt)


gt=c(0)


for ( i in 2:1000)
  
{
  
  gt[i]=wt[i]-wt[i-1]
  
  
}

plot(gt,type="l")

acf(gt[2:1000])



m<-matrix(NA,100,10)

for(i in 1:10)
{
  m[,i]<-rnorm(100,10)
}
M<-apply(m,2,diff)

plot(ts(m[,1]),ylab="",xlab="",main="noise")
for(i in 2:10)
{
  lines(ts(m[,i]))
}
dev.new()
plot(ts(M[,1]),ylab="",xlab="",main="differenced noise")
for(i in 2:10)
{
  lines(ts(M[,i]))
}


#MA(1)

# 

#Just check with him

Wt=rnorm(2000)


-15/(1+15^2)

MA=c(0)


for (i in 2:100)
{
  MA[i]=Wt[i]+0.99*Wt[i-1]  ## why would it be less than half?
  
  
}


plot(MA,type="l")  


gf=acf(MA)

pacf(MA)

ma1=arima.sim(model=list(ma=-c(-0.99)),n=200)

acf(ma1)
d=acf(ma1)

d$acf


ma2=arima.sim(model=list(ma=-c(-9)),n=200)

acf(ma2)

e=acf(ma2)
e$acf


acf_v=c(0)

acf$acf[2]

j=-0.1

while (j<5)
{
for(i in 2:200)
{
 
  MA[i]=Wt[i]-j*Wt[i-1] 
 
  
}
d=acf(MA)  
acf_v=c(acf_v,d$acf)  
j=j+0.25
}


hist(acf_v,breaks = 12)



########################################################################
ddf=acf(ma1)
ddf$acf

ddg=acf(MA)

ddg$acf


############################
#Q2.13 uiowa state 


Zt=at-theta(at-1)^2


at=rnorm(200)

Zt=c(0)


theta=0.5

for (i in 2:length(at))
{
  Zt[i]=at[i]-theta*at[i-1]
  
}

plot(Zt,type = "l")

acf(Zt) #MA model

acfv=acf(Zt)
acfv$acf
