#generate data
param = c(10,0, 1)
#param= [n,mu,phi]
n=param[1]
#set.seed(42)
data = rnorm(n=param[1], mean=param[2],sd=sqrt(1/param[3]))
runs=10000;

#allocate, and initialize parameters to expected values
mu=t(rep(NA,runs)); mu[1]=0;
phi=t(rep(NA,runs)); phi[1]=1
  
#initialize hyperparameters
mu1=t(rep(NA,runs));mu1[1]=0; 
phi1=t(rep(NA,runs)); phi1[1]=1

alpha=0.25; beta=0.25
normvalues = rnorm(runs-1,mean=0,sd=1)

for (i in 1:(runs-1)){
  temp=sum((data-mu[i])^2)
  #update hyperparameters
  phi1[i+1]=phi[i]*n+phi1[i]
  mu1[i+1] =(phi[i]*temp+phi1[i]*mu1[i])/phi1[i+1]
  
  alpha=alpha+0.5*n
  beta=beta+0.5*temp
  
  mu[i+1]=rnorm(1,mean=mu1[i+1],sd=sqrt(1/phi1[i+1]))
  #mu[i+1]=normvalues[i+1]*(1/phi1)+mu1
  phi[i+1]=rgamma(1,shape = alpha,scale=1/beta)
}

hist(mu[50:runs])


tester=normvalues[2:(runs)]-normvalues[1:(runs-1)]
plot(10:(runs-1),tester[10:(runs-1)])
lines(10:(runs-1),rep(0,runs-10))

tester=mu[2:(runs)]-mu[1:(runs-1)]
plot(10:(runs-1),tester[10:(runs-1)])
lines(10:(runs-1),rep(0,runs-10))

