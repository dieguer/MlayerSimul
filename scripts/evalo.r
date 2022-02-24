
#######   Evaluation function to minimize with data

# require(parallel)
# library(purrr)
# library(gtools)
# library('igraph')
# library(gridExtra)
# library(tidyverse)
# library(reshape2)
# library(MASS)

# source("./scripts/DGPs.R")

# m  <- 3

# lay <- dim(combinations(m,2, repeats.allowed=T))[1]
# parmo  <-  numeric(0)

#  for (v in 1:lay){
#    parmo<- rbind(parmo, c(-1, 1))
#  }
#  n=100


 
# simat=netgen(n=25,thetas=parmo)

# simat[[4]]=simat[[3]]*matrix(data=runif((dim(simat[[3]])[1]^2)),nrow=dim(simat[[3]])[1])


# parmo  <-  c(-1, 1,1)
#  n=dim(simat[[2]])[1]
 
#  covnum=2
#  parnum=6
#  source("./scripts/structo1.R")

#  g=as.list(as.data.frame(structo1(m)))


# simmat is a list where the first positon is the list of matrix of covariates.
#   the second is the network itself 
# to do: allow for several different


# THis program is the n covariates evaluation version. 
#In theory should work the same as evalfunc_i2

evalo <- function(parmo, simat, strobject,chosen) {
  numl <- length(simat[[1]])

  thetas <- numeric(0)
  for (v in 1:numl) {
    thetas <- rbind(thetas, parmo)
  }

  #
  thetas <- t(thetas)
 
  #index <- lapply(c(1:numl), function(x) {
  #  exp(simat[[2]] * thetas[1, x] + simat[[3]] * thetas[2, x])
  #})

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
     aggo=numeric(length(simat)-1)
      for(i in 2:length(simat)){
        j=i-1
      aggo[j]<- sum(simat[[i]] * u[[x[1]]] * udotdot[[x[2]]]) -
      sum((uidot[[x[1]]] %*% t(udotj[[x[2]]])) * simat[[i]])
      }
      return((1/chosen) * aggo)
     
})

  svec <- Reduce(`+`, slist)

  
  criterion <- t(svec) %*% svec

  return(criterion)

  }



# evalo(parmo,simat,g,25^2)


# optim(c(-1, 1,1), evalo, hessian = T, 
#                 simat = simat,strobject=g,chosen=25^2)$par




# dpath  <-  "/media/diego/921CA5BF1CA59EAB/Users/diego/Dropbox/datatesis/New folder"

# library(data.table) 
# data <- fread(paste0(dpath,"/mlayerdata.csv"))


# wdata <- data  %>% dplyr::select(c(iso3num_o,
#             iso3num_d,year,tourism,fdi, contig:tradeflow_imf_d))  %>% 
#             filter(year==2017)  %>% dplyr::select(-c(empire))

# wdata  <- wdata %>% mutate(
#     across(c(tourism:tradeflow_imf_d), ~replace_na(.x, 0))
#   )


# str(wdata)


# lista=names(wdata %>% dplyr::select(contig:entry_tp_d))

# length(unique(wdata$iso3num_o))
# length(unique(wdata$iso3num_d))

 

#  wdata$RowID <- match(wdata$iso3num_o,wdata$iso3num_d)
#  wdata$ColID <- match(wdata$iso3num_d,wdata$iso3num_d)

# library(Matrix)
 


# lvar <-  lapply(wdata[,..lista],function(x){
# selo <- which(x==0)
#     sparseMatrix(
#      i = wdata$RowID[-selo],
#      j = wdata$ColID[-selo],
#      x = x[-selo],dims=c(247,247))
# })
    
# str(lvar)

# listo=names(wdata %>% dplyr::select(tourism,fdi,tradeflow_comtrade_o))

# yvar <-  lapply(wdata[,..listo],function(x){
# selo <- which(x==0)
#     sparseMatrix(
#      i = wdata$RowID[-selo],
#      j = wdata$ColID[-selo],
#      x = x[-selo],dims=c(247,247))
     
# })


# lister= list(x=yvar)

# lister <- append(lister,lvar)

# strato <- list(c(1 ,2),c(1 ,3),c(2 ,1),
#               c(3 ,1),c(2 ,3),c(3 ,2))

# listerin  <- lister[c(1,7:40)]
# listerin  <- lister

# init <- rep(1,length(listerin)-1)

#evalo(init,listerin,strato,250^2)

#optim(init, evalo, hessian = T, 
#                simat = listerin,strobject=strato,chosen=(250^4)/4)$par

#library(optimParallel)

#cl <- makeCluster(5)     # set the number of processor cores
#setDefaultCluster(cl=cl)

#optimParallel(init, evalo, hessian = T, 
              #  simat = lister,strobject=strato,chosen=25^2)