# require(parallel)
# library(purrr)
# library(gtools)
# library('igraph')
# library(gridExtra)
# library(tidyverse)
# library(reshape2)
# library(MASS)

#  source("./scripts/DGPs.R")

# m  <- 3

# lay <- dim(combinations(m,2, repeats.allowed=T))[1]
# thetas  <-  numeric(0)

# for (v in 1:lay){
#   thetas <- rbind(thetas, c(-1, 1))
# }
# n=100


 
# mato=netgen(n=25,thetas=thetas)

# # n=dim(simat[[2]])[1]
# # a
# # covnum=2
# # parnum=6
# source("./scripts/structo1.R")

# g=as.list(as.data.frame(structo1(m)))



crito <- function(parmo, simat, strobject,chosen) {
  numl <- length(simat[[1]])

  thetas <- numeric(0)



  for (v in 1:numl) {
    thetas <- rbind(thetas, parmo)
  }

  #
  thetas <- t(thetas)
  index <- lapply(c(1:numl), function(x) {
    exp(simat[[2]] * thetas[1, x] + simat[[3]] * thetas[2, x])
  })

  # create u's

  u <- map2(index, simat[[1]], function(x, y) {
    y / x
  })



  uidot <- lapply(u, function(x) {
  rowSums(x)
  })

  udotj <- lapply(u, function(x) {
  colSums(x)
  })



  udotdot <- lapply(u, function(x) {
    sum(rowSums(x))
  })


slist <- lapply(strobject, function(x) {
    
    (1/chosen) * c(sum(simat[[2]] * u[[x[1]]] * udotdot[[x[2]]]) -
      sum((uidot[[x[1]]] %*% t(udotj[[x[2]]])) * simat[[2]]),
      sum(simat[[3]] * u[[x[1]]] * udotdot[[x[2]]]) -
      sum((uidot[[x[1]]] %*% t(udotj[[x[2]]])) * simat[[3]]))
})

  svec <- Reduce(`+`, slist)

  
  criterion <- t(svec) %*% svec

  return(criterion)

  }




  




#crito(thetas,mato,g, cho)