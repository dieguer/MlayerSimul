# requires gtools and tidyverse 

structo1<-function(m) {   
comb=matrix(nrow=round((m^4)/4),ncol=4 )
r<-0
for (l in 1:m) {
    for (k in l:m) {
        veco=1:m
       veco=veco[-which(veco %in% c(l,k))]
        for(lp in veco){
        voco=veco[which(veco >=lp)]
        for(kp in voco){
            p <-c(l,k,lp,kp)
            r <- r+1
            comb[r,] <- p
            }
            }
            }
        }
    
comb <- na.omit(comb)
attributes(comb)$na.action <- NULL
library(tidyverse)

comb <- as.data.frame(comb)
names(comb) <- c("col_l","col_k","col_lp","col_kp")
c <- 1:m

maper <- combinations(m,2,c,repeats.allowed=T)

maper  <-  as.data.frame(cbind(maper,1:dim(maper)[1]))
names(maper) <- c("col_l","col_k","mapo")

comb  <- comb %>% left_join(maper,by = c("col_l" = "col_l", "col_k" = "col_k"))  %>% 
        left_join(maper,by = c("col_lp" = "col_l", "col_kp" = "col_k"))  %>% 
        dplyr::select(starts_with("mapo")) %>% rename(lk=mapo.x,lpkp=mapo.y)

comb <- as.list(as.data.frame(t(as.matrix(comb))))


return(comb)

}
