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
m  <-3 
cores  <- 20

simo <- 1000
lay <- dim(combinations(m,2, repeats.allowed=T))[1]
thetas  <-  numeric(0)

for (v in 1:lay){
  thetas <- rbind(thetas, c(-1, 1))
}

g <- structo1(m)

sizes <- c(25, 50, 100, 200)

dir <- getwd()
################################################
#  Negative binomial
###############################################

nb1 <- lapply(sizes, function(x) {
  res <- simula(n = x, m = m, modelo = "Negbin2",
          parameter = 1,  sim = simo, 
          cores = cores, semilla = 201,  strobj = g)
  return(Reduce(rbind, res))})

  dato <- as.data.frame(unname(Reduce(rbind, nb1)))
  write.csv(dato,paste0(dir, "/simoutput/data/nb1_", m, ".csv"))

nb5 <- lapply(sizes, function(x) {
  res <- simula(n = x, m = m, modelo = "Negbin2",
          parameter = 5,  sim = simo, cores = cores,
          semilla = 301, strobj = g)
  return(Reduce(rbind, res))})

  dato <- as.data.frame(unname(Reduce(rbind, nb5)))
  write.csv(dato,paste0(dir, "/simoutput/data/nb5_", m, ".csv"))

nb10 <- lapply(sizes, function(x) {
  res <- simula(n = x, modelo = "Negbin2", m = m,
          parameter = 10,  sim = simo, cores = cores,
          semilla = 4021, strobj = g)
  return(Reduce(rbind, res))})

  dato <- as.data.frame(unname(Reduce(rbind, nb10)))
  write.csv(dato,paste0(dir, "/simoutput/data/nb10_", m, ".csv"))
################################################
#  Poisson
###############################################

poism <- lapply(sizes, function(x) {
  res <- simula(n = x,m = m, modelo = "Poisson",
          parameter = 1,  sim = simo, cores = cores, 
          semilla = 6208,  strobj = g)
  return(Reduce(rbind, res))})
 
  dato <- as.data.frame(unname(Reduce(rbind, poism)))
  write.csv(dato,paste0(dir, "/simoutput/data/poisson_", m, ".csv"))

################################################
#  Lognormal
###############################################

lnorm1 <- lapply(sizes, function(x) {
  res <- simula(n = x, m = m, modelo = "Lnormal",
          parameter = 1,  sim = simo, cores = cores, 
          semilla = 208,  strobj = g)
  return(Reduce(rbind, res))})

  dato <- as.data.frame(unname(Reduce(rbind, lnorm1 )))
  write.csv(dato,paste0(dir, "/simoutput/data/logn1_", m, ".csv"))


lnorm2 <- lapply(sizes, function(x) {
  res <- simula(n = x, m = m, modelo = "Lnormal",
          parameter = 2,  sim = simo, cores = cores,
          semilla = 2508, strobj = g)
  return(Reduce(rbind, res))})

  dato <- as.data.frame(unname(Reduce(rbind, lnorm2)))
  write.csv(dato,paste0(dir, "/simoutput/data/logn2_", m, ".csv"))

lnorm3 <- lapply(sizes, function(x) {
  res <- simula(n = x, m = m, modelo = "Lnormal",
          parameter = 3,  sim = simo, cores = cores,
          semilla = 2048, strobj = g)
  return(Reduce(rbind, res))})

  dato <- as.data.frame(unname(Reduce(rbind, lnorm3)))
  write.csv(dato,paste0(dir, "/simoutput/data/logn3_", m, ".csv"))


lnorm4 <- lapply(sizes, function(x) {
  res <- simula(n = x, m = m, modelo = "Lnormal",
          parameter = 4,  sim = simo, cores = cores,
          semilla = 2708,  strobj = g)
  return(Reduce(rbind, res))})

  dato <- as.data.frame(unname(Reduce(rbind, lnorm4)))
  write.csv(dato,paste0(dir, "/simoutput/data/logn4_", m, ".csv"))
