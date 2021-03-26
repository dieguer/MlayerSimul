######################################################################
# make plots
#########################################################################
library(tidyverse)
library(ggthemes)
dir <- getwd()
m <- 3
sez=1
alfa <- 1
colo <- 'red'
files <- list.files(path = paste0(dir, "/simoutput/data"))

if (m==4 ) {
files <- files[grepl("_4.csv",files)]

} else {
outdir  <- "/simoutput/plots/3layer/"
 files <- files[!grepl("_4.csv",files)]     
}


source("./scripts/qqthin2.R")
source("./scripts/plotwork.R")

substrRight <- function(x, n) {
  substr(x, 1, nchar(x) - 4)
}

lb  <- function(x, r) {
qus <- quantile(x, c(0.25, 0.75), method = 5)
l <- qus[1] - (r * (qus[2] - qus[1]))
return(l)
}

ub  <- function(x, r) {
qus <- quantile(x, c(0.25, 0.75), method = 5)
l <- qus[2] + (r * (qus[2] - qus[1]))
return(l)
}

lapply(files,function(x) {
file <- x
dato <- read.csv(paste0(dir, "/simoutput/data/", file))

 p1  <- ggplot(dato, aes(x = factor(V3), V1)) + geom_boxplot(outlier.shape=NA) +
        geom_hline(yintercept = -1, color = "red") +
        xlab("n") + ylab("B1 (continuos)") + theme_bw()

 p2  <- ggplot(dato, aes(x = factor(V3),V2 )) + geom_boxplot(outlier.shape=NA) +
  xlab("n") + ylab("B2 (binary)") + geom_hline(yintercept = 1, color = "red") + theme_bw()


filo <- substrRight(file, 4)

ggsave(filename = paste0(dir, outdir,"box/", filo, "_box_cont.png"),
     plot = p1)

ggsave(filename = paste0(dir, outdir,"box/", filo, "_box_bi.png"),
      plot = p2)


dato <- dato %>% group_by(V3) %>%
                mutate(V4 = ((V1 - mean(V1)) / sd(V1)),
                            V5 = ((V2 - mean(V2)) / sd(V2))) %>%
                ungroup()

############# INTERQUANTILE RANGE CENSORING X cont###################
 s1  <-  dato %>% select(V1,V3) %>%
           group_by(V3) %>%
           filter( V1 > lb(V1, 1.5),  V1 < ub(V1, 1.5)) %>%
           mutate(V4 = ((V1 - mean(V1)) / sd(V1))) %>%
           ungroup()


p3 <- ploto(s1 ,"V3", "V4")

p5<- ploti(s1,"V3","V4", alfa=0.3)

############# INTERQUANTILE RANGE CENSORING X binary###################

  s2  <-  dato %>% select(V2,V3) %>%
           group_by(V3) %>%
           filter( V2 > lb(V2, 1.5),  V2 < ub(V2, 1.5)) %>%
           mutate(V4 = ((V2 - mean(V2)) / sd(V2))) %>%
           ungroup()


p4 <- ploto(s2, "V3", "V4")
p6 <- ploti(s2,"V3","V4", alfa=0.3)

ggsave(filename = paste0(dir, outdir,"qqthin/", filo, "_qq_cont.png"),
      plot = p3,   width =8,
  height = 5, units="in")

ggsave(filename = paste0(dir, outdir,"qqcens/", filo, "_qqc_cont.png"),
      plot = p5,   width =8,
  height = 5, units="in")

ggsave(filename = paste0(dir, outdir,"qqcens/", filo, "_qqc_bi.png"),
      plot = p6,   width =8,
  height = 5, units="in")
ggsave(filename = paste0(dir, outdir,"qqthin/", filo, "_qq_bi.png"),
      plot = p4,   width =8,
  height = 5, units="in")


return(TRUE)
      })