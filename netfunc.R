



library(purrr)
library(gtools)
library('igraph')
library(gridExtra)
library(tidyverse)
library(reshape2)
##############################################
#               simulate nodes fixed effect and covariates
##############################################


netgen=function(n=100,m=3,blim=2,seed=300,thetas){
#number of nodes

  set.seed(seed)
n <- n

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
m <- m

#number of inter and intra layers
lnum <-  perm(m,2)


# layer fixed effect vector
c <- runif(m)

# layer fixed effect combinations 
lfex=combinations(m,2,c,repeats.allowed=T)


# layer fixed effect combinations product
lfex_p <- lfex[,1]*lfex[,2]


# define parameters
#thetas <- runif(lnum, 0, blim) 
thetas <- thetas

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



return(y)


}








########################################
#  change of parameter effect on degree distribution
###############################


parchange <- function(seed=200, ene=50, parl=-5,paru=25, incre=0.25, FUN=mean){
cnt=0

repos=seq(from = parl, to = paru, by=incre)
mato1 <- matrix(nrow =length(repos) ,ncol = 6)
mato2 <- matrix(nrow =length(repos) ,ncol = 6)
mato3 <- matrix(nrow =length(repos) ,ncol = 6)
#for (t in seq(from = 0.1, to = 20, by=0.5)){
for (t in repos){  
 cnt=cnt+1
   paro <- rep(t,6)
  glist=netgen(ene,3,7,seed,paro)
  deglist=lapply(glist,function(x){
    g=graph_from_adjacency_matrix( x)
    g$layout <- layout_on_sphere
    
    #FUN()
    degree(g)
    #plot(g)   
  })
  
  mlist =lapply(deglist, function(x){ median(x)})
  q1 =lapply(deglist, function(x){ quantile(x,0.25)})
  q3 =lapply(deglist, function(x){ quantile(x,0.75)})
  
  mato1[cnt,]=unlist(mlist)
  mato2[cnt,]=unlist(q1)
  mato3[cnt,]=unlist(q3)
}

return(cbind(repos,mato1,mato2,mato3))
}






ploto=function(mato){
graphmat=as.data.frame(mato)
names(graphmat) <- c("parameter", 'l11','l12','l13','l22','l23','l33',
                                  'q_l11','q_l12','q_l13','q_l22','q_l23','q_l33',
                                  's_l11','s_l12','s_l13','s_l22','s_l23','s_l33')




A1=graphmat %>% select(starts_with('p')|starts_with('l')) %>% melt(id.vars=c("parameter"))

A2=graphmat %>% select(starts_with('p')|starts_with('s')) %>% melt(id.vars=c("parameter"))
A3=graphmat %>% select(starts_with('p')|starts_with('q')) %>% melt(id.vars=c("parameter"))

ploto=bind_cols(A1,A2,A3)




ploto %>%ggplot( aes(x=parameter...1, y=value...3, colour=variable...2)) + 
  geom_errorbar(aes(ymin=value...6, ymax=value...9, colour=variable...2), width=.1,alpha=0.3) +
  geom_point( size=1)

}








plots=lapply(sample.int(200, 9), function(x){
  yui=parchange(seed=x,ene=100)
  g=ploto(yui)
  return(g)
})

plots2= lapply(plots, function(x){
  x+theme(legend.position = "none") 
})
grid.arrange(grobs=plots2, ncol=3)


