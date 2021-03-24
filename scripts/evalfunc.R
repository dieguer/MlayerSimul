# requires source("DGPs.R")




# n=dim(simat[[2]])[1]
# 
# covnum=2
# parnum=6



crito <- function(parmo, simat) {
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

  # d_u <- lapply(u, function(x) {
  # list(x * simat[[2]], x * simat[[3]])
  # })


  uidot <- lapply(u, function(x) {
  rowSums(x)
  })

  udotj <- lapply(u, function(x) {
  colSums(x)
  })

  # d_udotj=lapply(d_u,function(x){
  #   lapply(x,function(y){colSums(y)})
  #   
  # })

  # d_uidot=lapply(d_u,function(x){
  #   lapply(x,function(y){rowSums(y)})
  #   
  # })



  udotdot <- lapply(u, function(x) {
    sum(rowSums(x))
  })


  # d_udotdot=lapply(d_u,function(x){
  #   lapply(x,function(y){sum(colSums(y))})
  #   
  # })




  slist <- lapply(c(1:numl), function(x) {
    c(sum(simat[[2]] * u[[x]] * udotdot[[x]]) -
      sum((uidot[[x]] %*% t(udotj[[x]])) * simat[[2]]),
      sum(simat[[3]] * u[[x]] * udotdot[[x]]) -
      sum((uidot[[x]] %*% t(udotj[[x]])) * simat[[3]]))
})

  svec <- Reduce(`+`, slist)

  criterion <- t(svec) %*% svec

  return(criterion)

}