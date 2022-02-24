
require(parallel)
library(purrr)
library(gtools)
library('igraph')
library(gridExtra)
library(tidyverse)
library(reshape2)
library(MASS)

source("./scripts/DGPs.R")

m  <- 3

lay <- dim(combinations(m,2, repeats.allowed=T))[1]
parmo  <-  numeric(0)

 for (v in 1:lay){
   parmo<- rbind(parmo, c(-1, 1))
 }
 n=100


 
simat=netgen(n=25,thetas=parmo)

simat[[4]]=simat[[3]]*matrix(data=runif((dim(simat[[3]])[1]^2)),nrow=dim(simat[[3]])[1])


parmo  <-  c(-1, 1,1)
 n=dim(simat[[2]])[1]
 
 covnum=2
 parnum=6
 source("./scripts/structo1.R")

 g=as.list(as.data.frame(structo1(m)))




Vmat <- function(parmo, simat){

  numl <- length(simat[[1]])
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

# mm_u 


  udotdot <- lapply(u, function(x) {
    sum(x)
  })


return(udotdot)



 lengo <- length(simat)-1
 d_u <- vector(mode = "list", length = lengo)



d_u <- lapply(u, function(e) {
    lapply(c(2:length(simat)),function(x){
        e*simat[[x]]
    } )
  })


}


