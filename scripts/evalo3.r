#######   Variance function
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
#  n=3


 
#  simat=netgen(n=3,thetas=parmo)

#  simat[[4]]=simat[[3]]*matrix(data=runif((dim(simat[[3]])[1]^2)),nrow=dim(simat[[3]])[1])


# parmo  <-  c(-1, 1,1)
#   n=dim(simat[[2]])[1]
 
#  covnum=2
#   parnum=6
#   source("./scripts/structo1.R")

#   g=as.list(as.data.frame(structo1(m)))




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
# This error matrix sums across errors of all layers.  
 ulk <- Reduce(`+`, u)


# first term: bot x and u do not depend on i' or k' only Ul 
# summation collapses to scalar
m_1  <-  sum(ulk)

lengo <- dim(u[[1]])[1]

# second term: x does not depende on primes, hence 
#using outer product of j vector in U vs row i in ulk, 
#sum across all dim to get scalar ij for position ij

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

# seventh term : x does not dep on primes, element wise product of matrix and sum
#across dimensions yields summation term  

m_7  <- lapply(2:length(simat), function(k) {
sum(simat[[k]]*ulk)
})

# third and 5th term: u independent of primes. 
 
m_3_5  <- lapply(2:length(simat), function(k) {
   eme <- rep(0,  lengo)
   ema <- rep(0,  lengo)
   for(i in 1:lengo){
     # col
  eme[i] <- sum( (simat[[k]][,i] %*% t(rep(1,lengo))) * ulk )
  # row
  ema[i] <- sum(  (rep(1,lengo) %*% t(simat[[k]][i,])) * ulk )
  }
  return(list(m3=eme,m5=ema))
 })
 


#fourth, sixth and eight term: all terms dependent on primes

m_4_6_8  <-  
        lapply(u, function(U){
              lapply(2:length(simat), function(k) {
              m4 <- matrix(0, ncol = lengo,nrow = lengo)
              m6 <- matrix(0, ncol = lengo,nrow = lengo)
              m8 <- matrix(0, ncol = lengo,nrow = lengo)
                  for(i in 1:lengo){
                      for(j in 1:lengo){

    m4[i,j] <- sum((simat[[k]][,j] %*% t(rep(1,lengo))) * 
            (U[,j] %*% t(rep(1,lengo)))  *  (rep(1,lengo) %*% t(ulk[i,])))
    m6[i,j] <- sum((rep(1,lengo) %*% t(simat[[k]][i,])) * 
              (U[,j] %*% t(rep(1,lengo)))  *  (rep(1,lengo) %*% t(ulk[i,])))
    m8[i,j] <- sum(simat[[k]] * 
              (U[,j] %*% t(rep(1,lengo)))  *  (rep(1,lengo) %*% t(ulk[i,])))

                      }
                  }
              return(list( m4=m4,m6=m6,m8=m8))
              })
          })



#return(m_7)

m_obj  <-  
        lapply(u, function(U){
          q <- 1
              lmat <- lapply(2:length(simat), function(k) {
            mato <- ( simat[[k]] * U *(m_1) -
            simat[[k]] * m_2[[q]]  -
            U * (rep(1,lengo) %*% t(m_3_5[[k-1]]$m3)) +
            m_4_6_8[[q]][[k-1]]$m4 -
            U * ( m_3_5[[k-1]]$m5 %*% t(rep(1,lengo)) ) + #error.*(m_xu{k}*ones(1,m))+            
            m_4_6_8[[q]][[k-1]]$m6 +
            U * m_7[[k-1]] - #error*mm_ux{k}-
            m_4_6_8[[q]][[k-1]]$m8 ) * 4 /( ((lengo-1)^2) * ((length(u)-1)^2 ) )  #m_uxu{k}; )
    #xi{k} = 4*xi{k}/(n-1)^2;

    return(mato)
              }) 
              q <- q + 1
              return(lmat)
              })
regnum <- length(simat)-1

ldos <- lapply(1:regnum, function(k){
         lone <- lapply(m_obj, function(V){
            V[[k]]
        })

        return(Reduce(`+`, lone))

      })



v_obj <- matrix(0,ncol=regnum,nrow = regnum)
            for(k in 1:regnum){
              for(j in 1:regnum){
              v_obj[k,j] <- sum(ldos[[k]]*ldos[[j]])/(lengo^2)*(length(u)^2)
            }
            }

#for k=1:d,
#    for j=1:d,
#        mVar(k,j) = mean(mean(xi{k}.*xi{j})); 
#    end
#end

return(v_obj)



  }


