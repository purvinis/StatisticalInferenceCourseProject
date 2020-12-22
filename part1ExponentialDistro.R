##comments
library(ggplot2)

lambda = 0.2
xs <- seq(0,40, by = .05)
dvalues <- dexp(xs,rate=lambda)
data <-data.frame(xs,dvalues)

p1 <- ggplot(data,aes(x=xs,y=dvalues)) + 
      geom_line(size = 2)+
   geom_area( fill="#69b3a2", alpha=0.4) +
      xlab("x")+
      ylab("0.2 exp(x/0.2)")+
   labs(caption="Exponential Distribution Function with rate lambda = 0.2",
        title ="Exponential Distribution Function")
print(p1)

#for the normal distribution that we will compare to
xn <-seq(0,10,by = 10.01/1000)
nvalues <- dnorm(xn,mean=1/lambda,sd=1/lambda)
ndata <-data.frame(xn,nvalues)

p1n <- ggplot(ndata,aes(x=xn,y=nvalues)) + 
      geom_line()
print(p1n)

#rexp(n,rate) generates a vector of exponentially distributed random numbers
set.seed(12172020)  #set seed so result is reproducible
histdata <-data.frame(hx =rexp(1000,lambda))
p2 <- ggplot(data=histdata,aes(x=hx))+
      geom_histogram(aes(y=..density..),fill="blue",alpha = 0.4,binwidth = 1)+
   xlab("x")+
   ylab("distribution")+
   labs(caption="Distribution of 1000 random exponentially distributed numbers",
        title ="Distribution of 1000 random exponentially distributed numbers")

print(p2)


meansOf40samples =sdOf40samples =NULL
for (i in 1:1000) meansOf40samples = c(meansOf40samples,mean(rexp(40,rate = lambda)))
for (i in 1:1000) sdOf40samples = c(sdOf40samples,sd(rexp(40,rate = lambda)))
hist(meansOf40samples,breaks = 20)

mOf40 <-data.frame(hx40=meansOf40samples)
p3 <- ggplot(data=mOf40,aes(x=hx40))+
      geom_histogram(aes(y=..density..,binwidth = 0.5), fill="purple",color="black",alpha = 0.5)+
      geom_density(size=2)
print(p3)

popmean <-mean(rexp(1000,lambda))
popsd <-sd(rexp(1000,lambda))
mOfm <-mean(meansOf40samples)
mOfsd <-mean(sdOf40samples)
sdOfm <-sd(meansOf40samples)  #5/sqrt(40)= 0.7905694

#The sample variance, S^2, estimates the population variance
#The distribution of the sample variance is centered around the population variance
#The variance of the sample mean is S^2/n
#5/sqrt(40)

#Difference betw distribution of 1000 random exponentials and 
#the distribution of 1000 (averages of 40 exponentials).

#Confidence Interval for the difference in two means
ans <-popmean - mOfm +c(-1,1)*qnorm(0.975)*sqrt(popsd^2/1000 +sdOfm^2/1000)

