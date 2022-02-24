# require(parallel)
# library(purrr)
# library(gtools)
# library('igraph')
# library(gridExtra)
# library(tidyverse)
# library(reshape2)
# library(MASS)

# source("./scripts/DGPs.R")
# source("./scripts/evalo.r")


# m  <- 3

#  lay <- dim(combinations(m,2, repeats.allowed=T))[1]
#  parmo  <-  numeric(0)

#  for (v in 1:lay){
#     parmo<- rbind(parmo, c(-1, 1))
#  }
#  n=100


 
#  simat=netgen(n=25,thetas=parmo)

#  simat[[4]]=simat[[3]]*matrix(data=runif((dim(simat[[3]])[1]^2)),nrow=dim(simat[[3]])[1])


# parmo  <-  c(-1, 1,1)
#   n=dim(simat[[2]])[1]
 
#  covnum=2
#   parnum=6
#   source("./scripts/structo1.R")

#   g=as.list(as.data.frame(structo1(m)))


# simmat is a list where the first positon is the list of matrix of covariates.
#   the second is the network itself 
# to do: allow for several different


# THis program is the n covariates evaluation version. 
#In theory should work the same as evalfunc_i2
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################





evalo_g <- function(parmo, simat, strobject,chosen) {
  numl <- length(simat[[1]])
#pepe= Sys.time()

  thetas <- numeric(0)
  for (v in 1:numl) {
    thetas <- rbind(thetas, parmo)
  }

  thetas <- t(thetas)
  ene <- dim(simat[[2]])[1]
  index <- lapply(c(1:numl), function(x) {
      aggo <- matrix(0, ene, ene)
      for (i in 2:length(simat)) {
        j <- i - 1
      aggo <- aggo + simat[[i]] * thetas[j, x]
      }
     return(exp(aggo))
  })

  # create u's

  u <- map2(index, simat[[1]], function(x, y) {
    y / x
  })

 lengo <- length(simat)-1
 d_u <- vector(mode = "list", length = lengo)

d_u <- lapply(u, function(e) {
    lapply(c(2:length(simat)),function(x){
        e*simat[[x]]
    } )
  })


  uidot <- lapply(u, function(x) {
  rowSums(x)
  })

  d_uidot <- lapply(d_u, function(e) {
    lapply(e,function(x){
        rowSums(x)
    } )
  })


  udotj <- lapply(u, function(x) {
  colSums(x)
  })
  
  d_ujdot <- lapply(d_u, function(e) {
    lapply(e,function(x){
        colSums(x)
    } )
  })

  udotdot <- lapply(u, function(x) {
    sum(rowSums(x))
  })

  d_udotdot  <- lapply(d_u, function(e) {
    lapply(e,function(x){
        sum(rowSums(x))
    } )
  })


slist <- lapply(strobject, function(x) {
     aggo=numeric(length(simat)-1)
      for(i in 2:length(simat)){
        j=i-1
      aggo[j]<- sum(simat[[i]] * u[[x[1]]] * udotdot[[x[2]]]) -
      sum((uidot[[x[1]]] %*% t(udotj[[x[2]]])) * simat[[i]])
      }
      return((1/chosen) * aggo)
     
})


  svec <- Reduce(`+`, slist)
# criterion <- t(svec) %*% svec

 hlist <- mclapply(strobject, function(x) {

       ache=matrix(0,nrow=length(simat)-1,ncol=length(simat)-1)

       for(i in 2:length(simat)) {
         j=i-1
           for(p in 2:length(simat)) {
                 q = p - 1
           ache[j, q] <-  sum(simat[[i]] * u[[x[1]]] * (simat[[p]] *
           udotdot[[x[2]]] + d_udotdot[[x[2]]][[q]]) -
           simat[[i]] * (uidot[[x[1]]] %*% t(d_ujdot[[x[2]]][[q]]) +
            d_uidot[[x[1]]][[q]] %*% t(udotj[[x[2]]])))

       } }
       return((1/chosen) * ache)
 }, mc.cores = 6 )
  hvec <- Reduce(`+`, hlist)
 score  <-  -2*t(hvec)%*%svec
# Hessian   = -2*t(hvec)%*%hvec

  return(score)

  }

