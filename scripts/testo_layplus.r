require(parallel)
library(purrr)
library(gtools)
library('igraph')
library(gridExtra)
library(tidyverse)
library(reshape2)
library(MASS)


source("./scripts/DGPs.R")
source("./scripts/evalfunc_i2.R")
source("./scripts/simulafunc.R")
source("./scripts/structo1.R")

#Experiments


############################################

#initial values
m  <- 4

lay <- dim(combinations(m,2, repeats.allowed=T))[1]
thetas  <-  numeric(0)

for (v in 1:lay){
  thetas <- rbind(thetas, c(-1, 1))
}

sizes <- c(50)

dir <- getwd()

g <- structo1(m)
################################################
#  Negative binomial
###############################################

nb1 <- lapply(sizes, function(x) {
  res <- simula(n = x, m = m, modelo = "Negbin2",
          parameter = 5,  sim = 1000, cores = 16, semilla = 201, strobj = g)
  return(Reduce(rbind, res))})


pv1=nb1[[1]][,1]
outliers <- boxplot(pv1, plot=FALSE)$out
pv1p=pv1[-which(pv1 %in% outliers)]
pv1p=(pv1p-mean(pv1p))/sd(pv1p)

qqnorm(pv1p)
qqline(pv1p)

hist(pv1p)

boxplot(pv1p)

pv1=nb1[[1]][,2]
outliers <- boxplot(pv1, plot=FALSE)$out

pv1p=pv1[-which(pv1 %in% outliers)]

qqnorm(pv1p)
qqline(pv1p)

hist(pv1p)

shapiro.test(pv1p)