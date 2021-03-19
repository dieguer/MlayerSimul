source('DGPs.R')
source('evalfunc.R')
#Experiments


############################################

#initial values

thetas=c(-1,1)

for (v in 1:5){
  thetas=rbind(thetas,c(-1,1))
}




##############################################
#     n=100, sim=100, poisson.
##############################################
set.seed(200)
simnum=5000




##############################################
#     n=100, sim=100, poisson.
##############################################

set.seed(200)
simnum=5000
resvec=matrix(nrow =simnum, ncol = 2)
for(l in 1:simnum){
  Mat <-  netgen(thetas=thetas, n=100)
  resvec[l,]= optim(c(-1,1 ),crito, hessian = T)$par
}


P1=resvec


simnum=100
resvec=matrix(nrow =simnum, ncol = 2)
for(l in 1:simnum){
  Mat <-  netgen(thetas=thetas, n=200)
  resvec[l,]= optim(c(-1,1 ),crito, hessian = T)$par
}


P2=resvec

simnum=100
resvec=matrix(nrow =simnum, ncol = 2)
for(l in 1:simnum){
  Mat <-  netgen(thetas=thetas, n=50)
  resvec[l,]= optim(c(-1,1 ),crito, hessian = T)$par
}


P3=resvec


P1=cbind(P1,rep(100,100))
P2=cbind(P2,rep(200,100))
P3=cbind(P3,rep(50,100))

mat=rbind(P3,P1,P2)

mat=as.data.frame(mat)

library(tidyverse)

ggplot(mat, aes(x=factor(V3),V1 ))+ geom_boxplot() +geom_hline(yintercept = -1, color="red") + 
  xlab("n") + ylab("B1 (continuos)")

ggplot(mat, aes(x=factor(V3),V2 ))+ geom_boxplot() + 
  xlab("n") + ylab("B2 (binary)") +geom_hline(yintercept = 1, color="red")


par(mfrow=c(1,3))

mat1=mat %>% group_by(V3) %>% mutate(V4=((V1-mean(V1))/sd(V1))) %>% ungroup()

mat1=mat %>% group_by(V3) %>% mutate(V5=((V2-mean(V2))/sd(V2))) %>% ungroup()

ggplot(mat1, aes(sample=V4))+stat_qq()+stat_qq_line() + facet_grid(cols = vars(V3))

ggplot(mat1, aes(sample=V5))+stat_qq()+stat_qq_line() + facet_grid(cols = vars(V3))


qqnorm(mat1$V5[which(mat1$V3==50)])
qqline(mat1$V5[which(mat1$V3==50)])

qqnorm(mat1$V5[which(mat1$V3==100)])
qqline(mat1$V5[which(mat1$V3==100)])

qqnorm(mat1$V5[which(mat1$V3==200)])
qqline(mat1$V5[which(mat1$V3==200)])
##############################################
#     n=100, sim=100, negbin.
##############################################


set.seed(300)
simnum=100
resvec=matrix(nrow =simnum, ncol = 2)
for(l in 1:simnum){
  Mat <-  netgen(thetas=thetas, n=100,model='Negbin2', parameter=1)
  resvec[l,]= optim(c(-1,1 ),crito, hessian = T)$par
}


P1=resvec


simnum=100
resvec=matrix(nrow =simnum, ncol = 2)
for(l in 1:simnum){
  Mat <-  netgen(thetas=thetas, n=200, model='Negbin2', parameter=1)
  resvec[l,]= optim(c(-1,1 ),crito, hessian = T)$par
}


P2=resvec

simnum=100
resvec=matrix(nrow =simnum, ncol = 2)
for(l in 1:simnum){
  Mat <-  netgen(thetas=thetas, n=50, model='Negbin2', parameter=1)
  resvec[l,]= optim(c(-1,1 ),crito, hessian = T)$par
}


P3=resvec


P1=cbind(P1,rep(100,100))
P2=cbind(P2,rep(200,100))
P3=cbind(P3,rep(50,100))

mat=rbind(P3,P1,P2)

mat=as.data.frame(mat)


ggplot(mat, aes(x=factor(V3),V1 ))+ geom_boxplot() +geom_hline(yintercept = -1, color="red") + 
  xlab("n") + ylab("B1 (continuos)")

ggplot(mat, aes(x=factor(V3),V2 ))+ geom_boxplot() + 
  xlab("n") + ylab("B2 (binary)") +geom_hline(yintercept = 1, color="red")

ggplot(mat )+stat_qq()+stat_qq_line()

ggplot(mat, aes(sample=V2, color=factor(V3)))+stat_qq()+stat_qq_line()
##############################################

##############################################
#     n=100, sim=100, negbin.
##############################################


set.seed(350)
simnum=100
resvec=matrix(nrow =simnum, ncol = 2)
for(l in 1:simnum){
  Mat <-  netgen(thetas=thetas, n=100,model="Lnormal", parameter=1)
  resvec[l,]= optim(c(-1,1 ),crito, hessian = T)$par
}


P1=resvec


simnum=100
resvec=matrix(nrow =simnum, ncol = 2)
for(l in 1:simnum){
  Mat <-  netgen(thetas=thetas, n=200, model = "Lnormal", parameter = 1)
  resvec[l,]= optim(c(-1,1 ),crito, hessian = T)$par
}


P2=resvec

simnum=100
resvec=matrix(nrow =simnum, ncol = 2)
for(l in 1:simnum){
  Mat <-  netgen(thetas=thetas, n=50, model = "Lnormal", parameter = 1)
  resvec[l,]= optim(c(-1,1 ),crito, hessian = T)$par
}


P3=resvec


P1=cbind(P1,rep(100,100))
P2=cbind(P2,rep(200,100))
P3=cbind(P3,rep(50,100))

mat=rbind(P3,P1,P2)

mat=as.data.frame(mat)


ggplot(mat, aes(x=factor(V3),V1 ))+ geom_boxplot() +geom_hline(yintercept = -1, color="red") + 
  xlab("n") + ylab("B1 (continuos)")

ggplot(mat, aes(x=factor(V3),V2 ))+ geom_boxplot() + 
  xlab("n") + ylab("B2 (binary)") +geom_hline(yintercept = 1, color="red")

ggplot(mat )+stat_qq()+stat_qq_line()

ggplot(mat, aes(sample=V2, color=factor(V3)))+stat_qq()+stat_qq_line()

###################################################
#   n=100, sim=100, mixture.
##########################################

simnum=100


resvec=matrix(nrow =simnum, ncol = 2)
set.seed(385)


for(l in 1:simnum){
  Mat <-  netgen(thetas=thetas, model = "Mixture")
  resvec[l,]= optim(c(-1,1 ),crito, hessian = T)$par
}


qqnorm((resvec[,2]-mean(resvec[,2]))/sd(resvec[,2]))
qqline((resvec[,2]-mean(resvec[,2]))/sd(resvec[,2]))

hist(resvec[,2])


qqnorm((resvec[,1]-mean(resvec[,1]))/sd(resvec[,1]))
qqline((resvec[,1]-mean(resvec[,1]))/sd(resvec[,1]))

hist(resvec[,1])




#####################################################
#    n=100, sim=100, lnormal parameter 1
###################################################


simnum=100


resvec=matrix(nrow =simnum, ncol = 2)
set.seed(85)


for(l in 1:simnum){
  Mat <-  netgen(thetas=thetas, model = "Lnormal", parameter = 1)
  resvec[l,]= optim(c(-1,1 ),crito, hessian = T)$par
}


qqnorm((resvec[,2]-mean(resvec[,2]))/sd(resvec[,2]))
qqline((resvec[,2]-mean(resvec[,2]))/sd(resvec[,2]))

hist(resvec[,2])


qqnorm((resvec[,1]-mean(resvec[,1]))/sd(resvec[,1]))
qqline((resvec[,1]-mean(resvec[,1]))/sd(resvec[,1]))

hist(resvec[,1])






#####################################################
#    n=100, sim=100, lnormal parameter 1
###################################################


simnum=100


resvec=matrix(nrow =simnum, ncol = 2)
set.seed(85)


for(l in 1:simnum){
  Mat <-  netgen(thetas=thetas, model = "Lnormal", parameter = 2)
  resvec[l,]= optim(c(-1,1 ),crito, hessian = T)$par
}


qqnorm((resvec[,2]-mean(resvec[,2]))/sd(resvec[,2]))
qqline((resvec[,2]-mean(resvec[,2]))/sd(resvec[,2]))

hist(resvec[,2])


qqnorm((resvec[,1]-mean(resvec[,1]))/sd(resvec[,1]))
qqline((resvec[,1]-mean(resvec[,1]))/sd(resvec[,1]))

hist(resvec[,1])


#####################################################
#    n=100, sim=100, lnormal parameter 1
###################################################


simnum=100


resvec=matrix(nrow =simnum, ncol = 2)
set.seed(8521)


for(l in 1:simnum){
  Mat <-  netgen(thetas=thetas, model = "binom",n=200)
  resvec[l,]= optim(c(-1,1 ),crito, hessian = T)$par
}


qqnorm((resvec[,2]-mean(resvec[,2]))/sd(resvec[,2]))
qqline((resvec[,2]-mean(resvec[,2]))/sd(resvec[,2]))

hist(resvec[,2])


qqnorm((resvec[,1]-mean(resvec[,1]))/sd(resvec[,1]))
qqline((resvec[,1]-mean(resvec[,1]))/sd(resvec[,1]))

hist(resvec[,1])






#####################################################
#    n=100, sim=100, lnormal parameter 1
###################################################

resvec2=matrix(nrow =simnum, ncol = 2)
set.seed(200)


for(l in 1:simnum){
  Mat <-  netgen(n=500, thetas=thetas)
  resvec2[l,]= optim(c(-1,1 ),crito, hessian = T)$par
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



simnum=1 

resvec2=matrix(nrow =simnum, ncol = 2)
set.seed(200)

thetas <- numeric()
for (v in 1:6){
  thetas=rbind(thetas,c(-1,1)+runif(2))
}

for(l in 1:simnum){
  Mat <-  netgen(n=100, thetas=thetas)
  resvec[l,]= optim(c(-1,1 ),crito, hessian = T)$par
}
