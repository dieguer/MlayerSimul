######################################################################
# make plots qqthin
#########################################################################

ploto <- function(base,gvar,pvar){
require(dplyr)
source("./scripts/qqthin2.R")
ggvar <- rlang::enquo(gvar)
ppvar <- rlang::enquo(pvar)

qq1  <- base %>% mutate(VM = !!sym(pvar) )


qq1 <- qq1 %>% nest(data = c(-!!ggvar) )

 qq1 <- qq1 %>% mutate(qplot = map(data, function(z){
     as_tibble(qqnorm(z$VM , plot.it = F ))
 }))

  qq1 <-qq1%>% mutate(qtplot=map(qplot, function(z){
     as_tibble(qqthin2(x=z$x,y=z$y,eps = 0.006))
     }))

  qq1  <- qq1 %>% select(V3, qtplot) %>% unnest(qtplot)


 p <- ggplot(qq1) +
      geom_point(aes(x = V1, y = V2), shape = 1) +
      geom_abline(intercept = 0, slope = 1, color = "blue",
                  linetype = "longdash") +
      #coord_fixed(ratio = 1) +
      facet_wrap(facets = vars(V3), nrow = 2) +   theme_bw() + 
      theme(strip.background =element_rect(fill="white"))+
      theme(strip.text = element_text(colour = 'black'))
  return(p)    
}



ploti <- function(base,gvar,pvar,sez=1, alfa=1, printo=F) {
  
lim <- c(min(base[pvar]), max(base[pvar]))

p=ggplot(base, aes_string(sample = base[[pvar]] )) + 
      geom_abline( intercept = 0, slope = 1, color = "blue",linetype = "longdash") +
      stat_qq_line(fullrange = T) + 
      stat_qq(shape=1,size=sez, alpha = alfa) +
      #coord_fixed(  xlim=lim, ylim = lim) + 
      facet_wrap(facets = vars(!!sym(gvar)),nrow=2) + 
      theme_bw() + 
      theme(strip.background =element_rect(fill="white"))+
      theme(strip.text = element_text(colour = 'black'))
if(printo==T){
print( p)
}

return(p)

}

