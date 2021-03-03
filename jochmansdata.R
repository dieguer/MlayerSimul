#######################################
#           Jochmans Data generation
######################################
library(MASS)

psi <- c(-1,1)
n <- 100
rho <- 0.25
mu <- c(0,0)
model <- "Poisson"
parameter=10

set.seed(300)
# Generate correlated fixed effects with mean zero and covar matrix omega
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
#X{1} = x1; X{2} = x2;

# generate condition-mean function
varphi = exp(x1*psi[1]+x2*psi[2])*(alpha %*% t(rep(1,n)))*(rep(1,n) %*% t(gamma))

#generate outcome variable


Y=  matrix(0, ncol = n, nrow = n)

if (model=='Poisson'){
  for (i in 1:n){
    for (j in 1:n){
      Y[i,j]=rpois(1,varphi[i,j]) 
       }
  }
  }

if(model=='Negbin2'){
  for (i in 1:n){
    for (j in 1:n){
      Y[i,j]= rnbinom(1,parameter,parameter/(parameter+varphi[i,j]))
    }
  }
}

if(model=='Lnormal'){
  for (i in 1:n){
    for (j in 1:n){
      if (parameter==1) variance = 1              # log-normal errors are standard normal
      if (parameter==2) variance =   1/varphi[i,j]    # Poisson-type variance
      if (parameter==3) variance = 1+1/varphi[i,j]    # Negbin2-type variance with theta = 1;
      if (parameter==4) variance =   1/varphi[i,j]^2 # outcomes are homoskedastic outcomes
      sigma2 = log(1+variance); 
      mu = -sigma2/2; 
      e = mu+sqrt(sigma2)*rnorm(1); 
      epsilon = exp(e);
      Y[i,j]= varphi[i,j]*epsilon
      Y[i,j]= round(Y[i,j])
    }
  }
}


if(model=='Mixture'){
  M=matrix(0, ncol=n , nrow=n)
  for (i in 1:n){
    for (j in 1:n){
      M[i,j]= rnbinom(1,parameter,parameter/(parameter+varphi[i,j]))
      Y[i,j]=sum(rchisq(M[i,j],1))
    }
  }
}
