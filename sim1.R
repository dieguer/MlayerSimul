


install.packages(c('purrr','gtools','igraph'))
library(purrr)
library(gtools)
library('igraph')
##############################################
#               simulate nodes fixed effect and covariates
##############################################

#number of nodes

n <- 100

#Simulating individual fixed effects
alpha <- runif(n)
beta  <- runif(n)


#covariates
x <- runif(n)




##############################################
#   simulate layers
##############################################

# define
perm <- function(n, r){
  return(factorial(n)/factorial(n - r))
}


# number of layers
m <- 3

#number of inter and intra layers
lnum <-  perm(m,2)


# layer fixed effect vector
c <- runif(m)

# layer fixed effect combinations 
lfex=combinations(m,2,c,repeats.allowed=T)


# layer fixed effect combinations product
lfex_p <- lfex[,1]*lfex[,2]


# define parameters
thetas <- runif(lnum, 0, 1) 

parname <- c('b11','b12','b13','b22','b23','b33')

# par name
names(thetas) <- parname


#simulate the connection level noise

eps <- matrix(runif(n^2),nrow = n)

##############################################
#   simulate network
##############################################

#create matrix of observables for connections
x_i_j <-  matrix(nrow=n, ncol = n)

for (i in 1:n){
  for (j in 1:n){
     if (i!=j){
    x_i_j[i,j] <- (x[i]+ x[j])/2

     }
  }
}


#create matrix of to store index
emat <- lapply(c(1:length(parname)), function(x){x_i_j})

#multiply each layer times the specific layer parameter
emat <- map2(emat,thetas,function(x,y){exp(x*y)} )

#multiply each layer  by layer fixed effect
emat <- map2(emat,lfex_p,function(x,y){x*y} )


#multiply each layer  by node fixed effect
emat <- lapply(emat,function(x){
        for(i in 1:n){
          x[,i]= x[,i]*beta[i]
          x[i,]=  x[i,]*alpha[i]
        }
        return(x)
      } )



#simulate connections

y <- lapply(emat,function(x){
  for(i in 1:n){
    for(j in 1:n){
    x[i,j]= rbinom(1,size=1,p=x[i,j])    
    
  }}
  return(x)
} )




par(mfrow=c(2,3))
lapply(y,function(x){
  g=graph_from_adjacency_matrix( x)
  g$layout <- layout_on_sphere
  #print(edge_density(g))
  hist(degree_distribution(g))
  #plot(g)   

  })


lapply(y,function(x){
  g=graph_from_adjacency_matrix( x)
  #g$layout <- layout_on_sphere
  print(edge_density(g))
  #hist(degree_distribution(g))
  #plot(g)   
  
})


par(mfrow=c(2,3))
lapply(y,function(x){
  g=graph_from_adjacency_matrix( x)
  g$layout <- layout_on_sphere
  print(edge_density(g))
  #hist(degree_distribution(g))
  plot(g)   
  
})




