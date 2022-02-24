
makedata <- function(Ymat,Xmat, idvar_o, idvar_d){
row_ID <- match(idvar_o, idvar_d)
col_ID <- match(idvar_d, idvar_d)



 ene <- length(unique(idvar_o))


 lvar <-  lapply(Xmat,function(x){
 #selo <- which(x == 0)
    cr <-  sparseMatrix(
      i = row_ID,
      j = col_ID,
      x = x,dims=c(ene,ene))
 diag(cr)<-0
 return(cr)
 })



 yvar <-  lapply(Ymat, function(x){
 #selo <- which(x == 0)
    cr <-  sparseMatrix(
      i = row_ID,
      j = col_ID,
      x = x,dims=c(ene,ene))
 diag(cr)<-0
 return(cr)
 })

 lister  <-  list(yvar)

return(append(lister, lvar))

}