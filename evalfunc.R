source("DGPs.R")

Mat <-  netgen()


n=dim(Mat[[2]])[1]

covnum=2
parnum=6

thetas=t(tet)




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

d_udotj=lapply(d_u,function(x){
  lapply(x,function(y){colSums(y)})
  
})

d_uidot=lapply(d_u,function(x){
  lapply(x,function(y){rowSums(y)})
  
})



udotdot=lapply(u,function(x){
  sum(rowSums(x))
  
})


d_udotdot=lapply(d_u,function(x){
  lapply(x,function(y){sum(colSums(y))})
  
})




for (i in 1:n){
  for (j in 1:n){
  
    
    
}}

Slist=lapply(c(1:6), function(x){
  c(sum(Mat[[2]]*u[[x]]*udotdot[[x]]) - sum((uidot[[x]]%*%t(udotj[[x]]))*Mat[[2]]) ,
  sum(Mat[[3]]*u[[x]]*udotdot[[x]]) - sum((uidot[[x]]%*%t(udotj[[x]]))*Mat[[3]]))
})

Svec=Reduce(`+`, Slist)

cirterion=t(Svec)%*%Svec
