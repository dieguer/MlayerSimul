######################################################################
# make plots
#########################################################################
library(tidyverse)
dir <- getwd()
files <- list.files(path = paste0(dir, "/simoutput/data"))
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

 p1  <- ggplot(dato, aes(x = factor(V3), V1)) + geom_boxplot() +
        geom_hline(yintercept = -1, color = "red") +
        xlab("n") + ylab("B1 (continuos)")

 p2  <- ggplot(dato, aes(x = factor(V3),V2 )) + geom_boxplot() +
  xlab("n") + ylab("B2 (binary)") + geom_hline(yintercept = 1, color = "red")


filo <- substrRight(file, 4)

ggsave(filename = paste0(dir, "/simoutput/plots/", filo, "_box_cont.png"),
     plot = p1)

ggsave(filename = paste0(dir, "/simoutput/plots/", filo, "_box_bi.png"),
      plot = p2)


dato <- dato %>% group_by(V3) %>%
                mutate(V4 = ((V1 - mean(V1)) / sd(V1)),
                            V5 = ((V2 - mean(V2)) / sd(V2))) %>%
                ungroup()




lim <- c(min(dato$V4), max(dato$V4))
lim2 <- c(min(dato$V5), max(dato$V5))

p3 <- ggplot(dato, aes(sample = V4)) + stat_qq(shape=1) + stat_qq_line(fullrange = T) +
geom_abline( intercept = 0, slope = 1, color = "blue",linetype = "longdash") +
coord_fixed(ratio = 1, xlim=lim, ylim = lim) +
    facet_wrap(facets = vars(V3),nrow=2) 


p4 <- ggplot(dato, aes(sample=V5))+stat_qq( shape=1)+stat_qq_line(fullrange = T) +
    geom_abline( intercept=0, slope=1,color = "blue",linetype = "longdash")+
    coord_fixed(ratio = 1, xlim=lim2, ylim = lim2)+
     facet_wrap(facets = vars(V3),nrow=2) 


ggsave(filename = paste0(dir, "/simoutput/plots/", filo, "_qq_cont.png"),
      plot = p3)

ggsave(filename = paste0(dir, "/simoutput/plots/", filo, "_qq_bi.png"),
      plot = p4)


############# INTERQUANTILE RANGE CENSORING X cont###################
 s1  <-  dato %>% select(V1,V3) %>%
           group_by(V3) %>%
           filter( V1 > lb(V1, 1.5),  V1 < ub(V1, 1.5)) %>%
           mutate(V4 = ((V1 - mean(V1)) / sd(V1))) %>%
           ungroup()

lim <- c(min(s1$V4), max(s1$V4))

p6 <- ggplot(s1, aes(sample = V4)) + stat_qq(shape=1) + stat_qq_line(fullrange = T) +
geom_abline( intercept = 0, slope = 1, color = "blue",linetype = "longdash") +
coord_fixed(ratio = 1, xlim=lim, ylim = lim) +
    facet_wrap(facets = vars(V3),nrow=2) 


############# INTERQUANTILE RANGE CENSORING X binary###################

  s2  <-  dato %>% select(V2,V3) %>%
           group_by(V3) %>%
           filter( V2 > lb(V2, 1.5),  V2 < ub(V2, 1.5)) %>%
           mutate(V4 = ((V2 - mean(V2)) / sd(V2))) %>%
           ungroup()

lim <- c(min(s2$V4), max(s2$V4))

p5 <- ggplot(s2, aes(sample = V4)) + stat_qq(shape=1) + stat_qq_line(fullrange = T) +
geom_abline( intercept = 0, slope = 1, color = "blue",linetype = "longdash") +
coord_fixed(ratio = 1, xlim=lim, ylim = lim) +
    facet_wrap(facets = vars(V3),nrow=2) 


ggsave(filename = paste0(dir, "/simoutput/plots/", filo, "_qqc_cont.png"),
      plot = p5)

ggsave(filename = paste0(dir, "/simoutput/plots/", filo, "_qqc_bi.png"),
      plot = p6)


return(TRUE)
      })