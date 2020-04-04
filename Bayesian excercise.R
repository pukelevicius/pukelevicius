#question 1
# 1.
 an=c(0,1)
 i=3
 while(i <= 100){ 
     an=c(an,an[i-1]-an[i-2]+i)
     i=i+1
 }
#creating while loop,in which first 2 values are an array (0 and 1). every loop adds one more value to array
a100=an[100]
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2:
t=1:100
b=rep(0,100) 
i=1
for (i in t)
{
  b[i]=2*an[i]/max(an)
}
mean(b)
sd(b)
var(b)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3
#3.1
y=1
prior1=1/100
likelihood1=rep(0,100)
i=1
while(i<=100)
{
  likelihood1[i]=b[i]^y * exp(1)^(-b[i]) #likelihood is poisson equation filled with b as mu value.
  i=i+1
}
posterior_density1=prior1*likelihood1/sum(prior1*likelihood1) #posterior is obtained by prior*likelihood/sumation of prior*likelihood
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.2

poisson=rep(0,100)
i=1
for (i in 1:100)
{
  poisson[i]=(b[i]^y *exp(1)^-b[i])/factorial(y)
  
}
which.max(likelihood1) #likelihood and posterior are proportional because our prior is constant.
# other way:
which.max(posterior_density1) 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4:
f=function(i){
  
  y=b[i]^2/((b[i]^3) + 2) 
  
} 
# f argument is an integer value of i 
z=sum(f(seq(1,99,2))/50)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#question 2

#1.1
arrivals=c(5,8,4,6,8,6,6,5,6,4) #bus arrivals
r1.1=sum(arrivals)+1 #r parameter
v1.1=length(arrivals)#v parameter
posterior_mean1.1=r1.1/v1.1 #posterior mean: obtained by dividing r by v
posterior_var1.1=r1.1/v1.1^2 #posterior variance: obtained by dividing r by v^2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.2
miu=seq(0,10,0.1)
prior_uniform=dunif(miu,0,10) # uniform prior
posterior_density2=dgamma(miu,r1.1,v1.1) #posterior density which is gamma
plot(miu,prior_uniform,type = "l",col = "red",ylim =c(0,max(posterior_density2)),ylab = "y",xlab = "x",main = " posterior and prior densities") #plotting prior uniform
lines(miu,posterior_density2,col = "blue") # plotting posterior density
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2
#2.1
mu_mean= 6
mu_sd=2
parameters_f=function(mean,sd) #function with mean and stadard deviation as an arguments. function calculates prior gamma density
{
  r=mean^2/sd^2
  v=mean/sd^2
  print("printing r")
  print(r)
  print("printing v")
  print(v)
  prior=(dgamma(miu,r,v))
}
prior2.1=parameters_f(mu_mean,mu_sd)
r0=9
v0=1.5
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.2
P_more7=1-pgamma(7,r0,v0) #prior probability P(µ > 7)
P_less4=pgamma(4,r0,v0)   #prior probability P(µ < 4)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.3
r1=r0+sum(arrivals) #posterior r (r1)
v1=v0+length(arrivals)#posterior v (v1)
posterior_mean2=r1/v1 #posterior mean
posterior_var2=r1/v1^2#posterior variance
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.4
pos_P_more7=1-pgamma(7,r1,v1) #posterior probability P(µ|data > 7)
pos_P_less4=pgamma(4,r1,v1)   # posterior probability P(µ|data < 4)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.5
lower_end=qgamma(0.025,r1,v1)
#using normal approximation
lower_endnorm=qnorm(0.025,posterior_mean2,sqrt(posterior_var2))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.6
posterior_density2.1=dgamma(miu,r1,v1) #posterior density for second part of question 2
lim=max(posterior_density2.1,prior2.1) # y axis limiting value
plot(miu,prior2.1,type = "l",col = "blue",ylim = c(0,lim),xlab = "x",ylab = "y",main = "prior and posterior gamma densities")
lines(miu,posterior_density2.1,type = "l", col ="red",ylim=c(0,lim))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.7
r0
v0
observations=function(y,R,V){
  i=1
  while(i <= length(arrivals)){
  R=R+y[i]
  V=V+1
  parameters=c(R,V)
  i=i+1
  }
  return(parameters)
  }
observations(arrivals,r0,v0)
r1 
v1
#r1 and v1 are posterior parameters by analyzing observations all together in a single step. So we can compare both analyzation methods