require(parallel)
library(purrr)
library(gtools)
library('igraph')
library(gridExtra)
library(tidyverse)
library(reshape2)
library(MASS)

source("./scripts/DGPs.R")
source("./scripts/evalo.r")


m  <- 3

 lay <- dim(combinations(m,2, repeats.allowed=T))[1]
 parmo  <-  numeric(0)

 for (v in 1:lay){
    parmo<- rbind(parmo, c(-1, 1))
 }
 n=3


 
 simat=netgen(n=5,thetas=parmo)

 simat[[4]]=simat[[3]]*matrix(data=runif((dim(simat[[3]])[1]^2)),nrow=dim(simat[[3]])[1])


parmo  <-  c(-1, 1,1)
  n=dim(simat[[2]])[1]
 
 covnum=2
  parnum=6
  source("./scripts/structo1.R")

  g=as.list(as.data.frame(structo1(m)))




# THis program is the n covariates evaluation version. 
#In theory should work the same as evalfunc_i2
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################





evalo_V <- function(parmo, simat, strobject,chosen) {
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


# lambda error matrix
 ulk <- Reduce(`+`, u)


# first term ij
m_1  <-  sum(ulk)

lengo <- dim(u[[1]])[1]

# second term 

m_2  <-  lapply(u, function(U){
        eme <- matrix(0, ncol = lengo,nrow = lengo)
        for(i in 1:lengo) {
          for(j in 1:lengo) {
          # substract ij for i prime not 
        #eme[i,j] <- sum(U[-i,j] %*% t(ulk[i,-j]) )
        eme[i,j] <- sum(U[,j] %*% t(ulk[i,]) )
       
        }
        }
        return(eme)
  })

# seventh term 

m_7  <- lapply(2:length(simat), function(k) {
sum(simat[[k]]*ulk)
})

# third and 5th term
 
m_3_5  <- lapply(2:length(simat), function(k) {
   eme <- rep(0,  lengo)
   ema <- rep(0,  lengo)
   for(i in 1:lengo){
     # col
  eme[i] <- sum( (simat[[k]][,i] %*% t(rep(1,lengo))) * ulk )
  # row
  ema[i] <- sum(  (rep(1,lengo) %*% t(simat[[k]][i,])) * ulk )
  }
  return(list(eme,ema))
 })
 



m_4  <-  
        lapply(u, function(U){
              lapply(2:length(simat), function(k) {
              m4 <- matrix(0, ncol = lengo,nrow = lengo)
              m6 <- matrix(0, ncol = lengo,nrow = lengo)
                  for(i in 1:lengo){
                      for(j in 1:lengo){
                        m4[i,j] <- sum((simat[[k]][,j] %*% t(rep(1,lengo))) * (U[,j] %*% t(rep(1,lengo)))  *  (rep(1,lengo) %*% t(ulk[i,])))
                        m6[i,j] <- sum((rep(1,lengo) %*% t(simat[[k]][i,])) * (U[,j] %*% t(rep(1,lengo)))  *  (rep(1,lengo) %*% t(ulk[i,])))
                      }
                  }
              return(m6)
              })
          })






  return(m_4)

  }


evalo_V(parmo,simat,strato,3)
