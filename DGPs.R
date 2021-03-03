
##############################################
#              Script to Simulate DGP
##############################################


library(purrr)
library(gtools)
library('igraph')
library(gridExtra)
library(tidyverse)
library(reshape2)
library(MASS)




netgen=function(n=100,m=3,blim=2,seed=300, model='Poisson',rho=0.25,mu = c(0,0), parameter=10){
#number of nodes
  thetas=c(-1,1)
  
  for (v in 1:5){
    thetas=rbind(thetas,c(-1,1))
  }
  
  
#set.seed(seed)
n <- n

#Simulating individual fixed effects
omega <- matrix(c(1,rho,rho,1), ncol=2)

log_fixed_effects = matrix( nrow=n,ncol=2)

for (i in 1:n){
  log_fixed_effects[i,]=mvrnorm(mu=mu,Sigma=omega)
}  

alpha=exp(log_fixed_effects[,1])
gamma=exp(log_fixed_effects[,2])


# generate covariates
# first (binary) covariate:
threshold <-  -qnorm(sqrt(1/2))*(1-rho) 



temp <- log(alpha)-log(gamma) > sqrt(2)*threshold
x2 <- (temp %*% t(rep(1,n))) * (rep(1,n) %*% t(temp) )

# second (continous) covariate:

#parameters for cov1
mu0 = 0; beta0 = 0; rho0 = beta0/sqrt(1+beta0^2); 
sigma0 = 1; mean0 = mu0+sigma0*rho0*sqrt(2/pi); 
mu0 = -sigma0*rho0*sqrt(2/pi)+1;

#parameters for cov2
mu1 = 0; beta1 = 3; rho1 = beta1/sqrt(1+beta1^2); 
sigma1 = 1; mean1 = mu1+sigma1*rho1*sqrt(2/pi); 
mu1 = -sigma1*rho1*sqrt(2/pi)-1;


u0 = matrix(rnorm(n*n), ncol = n);
v0 = matrix(rnorm(n*n), ncol = n); 

w0 = rho0*u0+sqrt(1-rho0^2)*v0; 
z0 = w0*(u0>=0)- w0*(u0<0); 
xx0 = mu0 + sigma0*z0;


u1 = matrix(rnorm(n*n), ncol = n); v1 = matrix(rnorm(n*n), ncol = n); 
w1 = rho1*u1+sqrt(1-rho1^2)*v1; 
z1 = w1*(u1>=0)-w1*(u1<0); 
xx1 = mu1 + sigma1*z1;

x1 = xx0*(x2==0)+xx1*(x2==1);


##############################################
#   simulate layers
##############################################

# define
perm <- function(n, r){
  return(factorial(n)/factorial(n - r))
}


# number of layers
m <- m

#number of inter and intra layers
lnum <-  perm(m,2)
 

# layer fixed effect vector
c <- exp(runif(m,0,1.5))

# layer fixed effect combinations 
lfex=combinations(m,2,c,repeats.allowed=T)


# layer fixed effect combinations product
lfex_p <- lfex[,1]*lfex[,2]

###########################################
## define parameters
###########################################

thetas <- thetas

parname <- c('b11','b12','b13','b22','b23','b33')

# par name
names(thetas) <- parname


#simulate the connection level noise

#eps <- matrix(runif(n^2),nrow = n) Jochmans leaves the noise unspecified

##############################################
#   simulate network
##############################################

#create conditional moment

varphi=lapply(c(1:6), function(X){
  exp(x1*thetas[X,1]+x2*thetas[X,2])*(alpha %*% t(rep(1,n)))*(rep(1,n) %*% t(gamma))*lfex_p[X]
})


#simulate connections






y=lapply(varphi, function(VRPHI){
  Y=  matrix(0, ncol = n, nrow = n)
  if (model=='Poisson'){
    for (i in 1:n){
      for (j in 1:n){
        Y[i,j]=rpois(1,VRPHI[i,j]) 
      }
    }
  }
  
  if(model=='Negbin2'){
    for (i in 1:n){
      for (j in 1:n){
        Y[i,j]= rnbinom(1,parameter,parameter/(parameter+VRPHI[i,j]))
      }
    }
  }
  
  if(model=='Lnormal'){
    for (i in 1:n){
      for (j in 1:n){
        if (parameter==1) variance = 1              # log-normal errors are standard normal
        if (parameter==2) variance =   1/VRPHI[i,j]    # Poisson-type variance
        if (parameter==3) variance = 1+1/VRPHI[i,j]    # Negbin2-type variance with theta = 1;
        if (parameter==4) variance =   1/VRPHI[i,j]^2 # outcomes are homoskedastic outcomes
        sigma2 = log(1+variance); 
        mu = -sigma2/2; 
        e = mu+sqrt(sigma2)*rnorm(1); 
        epsilon = exp(e);
        Y[i,j]= VRPHI[i,j]*epsilon
        Y[i,j]= round(Y[i,j])
      }
    }
  }
  
  
  if(model=='Mixture'){
    M=matrix(0, ncol=n , nrow=n)
    for (i in 1:n){
      for (j in 1:n){
        M[i,j]= rnbinom(1,parameter,parameter/(parameter+VRPHI[i,j]))
        Y[i,j]=sum(rchisq(M[i,j],1))
      }
    }
  }
  
  
  return(Y)
    
  
})


return(list(y,x1,x2))


}




