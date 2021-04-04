m=3

for (l in 1:m){
    for(k in l:m){
        kp  <-k+1
        while(kp<=m){
                lp  <-kp+1
                while(lp<=m){
                    print(paste0("U",l,k," ",kp,lp) 
                    lp  <-lp+1
                    }
                kp  <-kp+1
            }
        }
    }
       


for (l in 1:m) {
    for (k in 1:m) {

        print(paste0("U",l,k," ","U",k,l)) 
        lp  <-lp+1
        }
    }

m=3
reso=character(0)

for (l in 1:m) {
    for (k in 1:m) {
        for (kp in 1:m) {
            for (kpp in 1:m) {
            if((l==k & l!=kp & l!=kpp & kp!=kpp)|(l!=k & k!=kp & l!=kp & kp==kpp) 
                |(l!=k & l!=kp & l!=kpp & k!=kp & l!=kpp & kp!=kpp )){
            p <-paste0("U",l,k," ",kp,kpp, "  -   U",l,kp," ",kpp,k)
            reso <-c(reso, p)
            }
            }
            }
        }
    }




reso=character(0)

for (l in 1:m) {
    for (k in 1:m) {
            p <-paste0("U",l,k," ",k,kpp)
            reso <-c(reso, p)
            
            
            }
        }
m=3



for (i in 1:3){
    j=1
    while(j<2){
        print(i)
        j=j+1
    }
}



for (l in 1:m) {
    for (k in 1:m) {

        print(paste0("U",l,k," ","U",k,l)) 
        lp  <-lp+1
        }
    }




reso=character(0)
m=50
for (l in 1:m) {
    for (k in l:m) {
        veco=1:m
       veco=veco[-which(veco %in% c(l,k))]
        for(lp in veco){
        voco=veco[which(veco >=lp)]
        for(kp in voco){
            reso=c(reso,paste0("U",l,k," ","U",lp,kp))
        }
        }
            }
            }
        



c <- exp(rnorm(m))

# layer fixed effect combinations 
lfex=combinations(m,2,1:m,repeats.allowed=T)


# layer fixed effect combinations product
lfex_p <- lfex[,1]*lfex[,2]

#number of inter and intra layers
lnum <-  dim(lfex)[1]

library(tidyverse)
library(gtools)
source("./scripts/structo1.R")

g=structo1(2)

g=structo1(100)

