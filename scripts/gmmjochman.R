source('jochmansdata.R')

rm(list= ls()[!(ls() %in% c('x1','x2','Y','psi'))])


dimY <-  dim(Y  ); # sample size
n <- dimY[1]
m <- dimY[2]

d <- length(psi); # number of regressors

# variable definitions

index <-  matrix(0,nrow=n,ncol=m); 

cnt=0
for (k in list(x1,x2)){
  cnt=cnt+1
    index = index+ k*psi[cnt] 
 
  } 

phi   = exp(index); # exponential transform  


error =     Y/phi # % disturbance

d_error <- lapply(list(x1,x2), function(l){
  error*l
})

for (k in 1:d){
  d_error{k} = error.*X{k}
}

