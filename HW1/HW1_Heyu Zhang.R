# Question 1
#1.
VaR1=100+100*log(0.99)
VaR1

#2.
VaR2=-100-100*log(1-0.99)
VaR2

# Question 2 
#1.
VaR1=-1*qnorm((1-0.99), mean=0.07, sd=0.1)
VaR2=-1*qnorm((1-0.99), mean=0.07, sd=0.15)
sd3=0.6*0.1+0.4*0.15
VaR3=-1*qnorm((1-0.99), mean=0.07, sd=sd3)

#2.
pi= seq(0,1,0.01)
sd_pi=(pi*0.1)+(1-pi)*0.15
VaR_pi=-1*qnorm((1-0.99), mean=0.07, sd=sd_pi)
plot(pi, VaR_pi)

#3.

VaR_gamma<-function(w0,alpha, beta, c, mu){
  sigma=rgamma(10000,alpha,beta)
  return=c()
  for (i in 1:10000){
    return=c(return,rnorm(10000,mean=mu,sd=sigma[i]))
  }
  return<-sort(return, decreasing = FALSE)
  VaR_gamma=-w0*return[(1-c)*length(return)]
}

ln(3)
