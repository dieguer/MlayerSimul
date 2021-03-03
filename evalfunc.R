source("DGPs.R")




# n=dim(Mat[[2]])[1]
# 
# covnum=2
# parnum=6




crito=function(parmo){
  

  
  thetas <- numeric(0)
  
  for (v in 1:6){
    thetas=rbind(thetas,parmo)
  } 
  
  thetas <- t(thetas)
  
index=lapply(c(1:6),function(x){ 
  exp(Mat[[2]]*thetas[1,x] + Mat[[3]]*thetas[2,x] )
  
})

# create u's

u=map2(index,Mat[[1]],function(x,y){
  y/x
  
})

d_u=lapply(u,function(x){
  list(x*Mat[[2]], x*Mat[[3]])
  
})


uidot=lapply(u,function(x){
  rowSums(x)
  
})

udotj=lapply(u,function(x){
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



udotdot=lapply(u,function(x){
  sum(rowSums(x))
  
})


# d_udotdot=lapply(d_u,function(x){
#   lapply(x,function(y){sum(colSums(y))})
#   
# })




Slist=lapply(c(1:6), function(x){
  c(sum(Mat[[2]]*u[[x]]*udotdot[[x]]) - sum((uidot[[x]]%*%t(udotj[[x]]))*Mat[[2]]) ,
  sum(Mat[[3]]*u[[x]]*udotdot[[x]]) - sum((uidot[[x]]%*%t(udotj[[x]]))*Mat[[3]]))
})

Svec=Reduce(`+`, Slist)

criterion=t(Svec)%*%Svec

return(criterion)

}





simnum=1000


resvec=matrix(nrow =simnum, ncol = 2)
set.seed(200)


for(l in 1:simnum){
Mat <-  netgen()
resvec[l,]= optim(c(-1,1 ),crito, hessian = T)$par
}



qqnorm((resvec[,2]-mean(resvec[,2]))/sd(resvec[,2]))
qqline((resvec[,2]-mean(resvec[,2]))/sd(resvec[,2]))


qqnorm((resvec[,1]-mean(resvec[,1]))/sd(resvec[,1]))
qqline((resvec[,1]-mean(resvec[,1]))/sd(resvec[,1]))



resvec2=matrix(nrow =simnum, ncol = 2)
set.seed(200)


for(l in 1:simnum){
  Mat <-  netgen(n=500)
  resvec[l,]= optim(c(-1,1 ),crito, hessian = T)$par
}



qqnorm((resvec[,2]-mean(resvec[,2]))/sd(resvec[,2]))
qqline((resvec[,2]-mean(resvec[,2]))/sd(resvec[,2]))
text(x = 2.5,y=-3,"1000 sim, n=500")


qqnorm((resvec[,1]-mean(resvec[,1]))/sd(resvec[,1]))
qqline((resvec[,1]-mean(resvec[,1]))/sd(resvec[,1]))
text(x = 2.5,y=-3,"1000 sim, n=500")

hist(resvec[,2], # histogram
     col="peachpuff", # column color
     border="black",
     prob = TRUE, # show densities instead of frequencies
     xlab = "temp",
     main = "Beaver #1",breaks = 50)
lines(density(resvec[,2]), # density plot
      lwd = 2, # thickness of line
      col = "chocolate3")




hist(resvec[,1], # histogram
     col="peachpuff", # column color
     border="black",
     prob = TRUE, # show densities instead of frequencies
     xlab = "temp",
     main = "Beaver #1",breaks = 50)
lines(density(resvec[,1]), # density plot
      lwd = 2, # thickness of line
      col = "chocolate3")
