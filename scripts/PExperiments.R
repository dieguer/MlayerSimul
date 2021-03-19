require(parallel)
library(purrr)
library(gtools)
library('igraph')
library(gridExtra)
library(tidyverse)
library(reshape2)
library(MASS)


source("DGPs.R")
source("evalfunc.R")
source("simulafunc.R")

#Experiments


############################################

#initial values

thetas  <-  c(-1, 1)

for (v in 1:5){
  thetas <- rbind(thetas, c(-1, 1))
}

sizes <- c(25, 50, 100, 200, 400, 800)


dir <- getwd()
################################################
#  Negative binomial
###############################################

nb1 <- lapply(sizes, function(x) {
  res <- simula(n = x, modelo = "Negbin2",
          parameter = 1,  sim = 1000, cores = 16,semilla = 201)
  return(Reduce(rbind, res))})

  dato <- as.data.frame(unname(Reduce(rbind, nb1)))
  write.csv(dato,paste0(dir, "/simoutput/data/nb1.csv"))

nb5 <- lapply(sizes, function(x) {
  res <- simula(n = x, modelo = "Negbin2",
          parameter = 5,  sim = 1000, cores = 16,semilla = 301)
  return(Reduce(rbind, res))})

  dato <- as.data.frame(unname(Reduce(rbind, nb5)))
  write.csv(dato,paste0(dir, "/simoutput/data/nb5.csv"))

nb10 <- lapply(sizes, function(x) {
  res <- simula(n = x, modelo = "Negbin2",
          parameter = 10,  sim = 1000, cores = 16,semilla = 4021)
  return(Reduce(rbind, res))})

  dato <- as.data.frame(unname(Reduce(rbind, nb10)))
  write.csv(dato,paste0(dir, "/simoutput/data/nb10.csv"))
################################################
#  Poisson
###############################################

poism <- lapply(sizes, function(x) {
  res <- simula(n = x, modelo = "Poisson",
          parameter = 1,  sim = 1000, cores = 16,semilla = 6208)
  return(Reduce(rbind, res))})
 
  dato <- as.data.frame(unname(Reduce(rbind, nb10)))
  write.csv(dato,paste0(dir, "/simoutput/data/poisson.csv"))

################################################
#  Lognormal
###############################################

lnorm1 <- lapply(sizes, function(x) {
  res <- simula(n = x, modelo = "Lnormal",
          parameter = 1,  sim = 1000, cores = 16,semilla = 208)
  return(Reduce(rbind, res))})

  dato <- as.data.frame(unname(Reduce(rbind, nb10)))
  write.csv(dato,paste0(dir, "/simoutput/data/logn1.csv"))


lnorm2 <- lapply(sizes, function(x) {
  res <- simula(n = x, modelo = "Lnormal",
          parameter = 2,  sim = 1000, cores = 16,semilla = 2508)
  return(Reduce(rbind, res))})

  dato <- as.data.frame(unname(Reduce(rbind, nb10)))
  write.csv(dato,paste0(dir, "/simoutput/data/logn2.csv"))

lnorm3 <- lapply(sizes, function(x) {
  res <- simula(n = x, modelo = "Lnormal",
          parameter = 3,  sim = 1000, cores = 16,semilla = 2048)
  return(Reduce(rbind, res))})

  dato <- as.data.frame(unname(Reduce(rbind, nb10)))
  write.csv(dato,paste0(dir, "/simoutput/data/logn3.csv"))


lnorm4 <- lapply(sizes, function(x) {
  res <- simula(n = x, modelo = "Lnormal",
          parameter = 4,  sim = 1000, cores = 16,semilla = 2708)
  return(Reduce(rbind, res))})

  dato <- as.data.frame(unname(Reduce(rbind, nb10)))
  write.csv(dato,paste0(dir, "/simoutput/data/logn4.csv"))


