

library("MASS")
library("ivreg")
library('gmm')


fr <- function(b,x) {   ## Rosenbrock Banana function
  b1 <- b[1]
  b2 <- b[2]
  b3 <- b[3]
  y=x[,1]
  c=x[,2]
  x1=x[,3]
  x2=x[,4]
  z1=x[,5]
  z2=x[,6]
  z3=x[,7]
  m1=(z1*(y-b1*x1-b2*x2-c*b3))
  m2=(z2*(y-b1*x1-b2*x2-c*b3))
  m3=(z3*(y-b1*x1-b2*x2-c*b3))
  f <- cbind(m1,m2,m3)
  return(f)
}

sim <- 1000
x1ols=rep(0,sim)
x1iv=numeric(sim)
x1gmm=numeric(sim)
set.seed(156)
n <- 1000
for (i in 1:sim){

pred.vars <- mvrnorm(n, c(20, 15), matrix(c(1, 0.75, 0.75, 1), 2, 2))

x.star  <- pred.vars[, 1] 
W <- pred.vars[, 2]


z3=rnorm(n)
z1=rnorm(n)
z2=rnorm(n)


x1 <- z1+z1+z3+x.star

x2 <- rnorm(n)

y= 1+W+ x1 + x2 + rnorm(n)




# Generate X* and W as correlated normal random variables ------------

equis=cbind(y,rep(1,n),x1,x2,z1,z2,z3)  


iv <- ivreg( y ~ x1 + x2  | z1 + z2 + z3)
mdel=lm(y ~ x1 + x2)

x1iv[i]= iv$coefficients[2]


x1ols[i]= mdel$coefficients[2]

x1gmm[i]=coefficients(gmm( g=fr,x=equis, c(b1=0,b2=0,b2=0),type = "iterative"))[1]
}

hist(x1gmm)
hist(x1iv)
hist(x1ols)

  
  


  





