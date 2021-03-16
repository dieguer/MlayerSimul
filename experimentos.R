#Print summations terms

n=dim(Mat[[2]])[1]

n=4
cn=0
numcon=0
ids=matrix
charcon=character(0)
c=numeric(4)

for (i in 1:(n-1)){
   for (I in (i+1):n){
      for (j in 1:(n-1)){
      for (J in (j+1):n){
 if(i!=j & i!=J & I!=i & J!=i & I!=j & J!=j & I!=J){
   u1=paste0('u',i,j)
   u2=paste0('u',I,J)
   u3=paste0('u',i,J)
   u4=paste0('u',I,j)
        cn=cn+1
        charcon[cn]=c(paste0('(x',i,j," - ",'x',i,J,") - (",'x',I,j," - ",'x',I,J,")",'(',u1,u2,"-",u3,u4,")"))
        #c[cn]=cn
        #numcon=numcon+ ifelse( ((Mat[[2]][i,j] - Mat[[2]][i,J]) - (Mat[[2]][I,j] - Mat[[2]][I,J])) !=0 ,1,0 )
        #charcon[cn]= ifelse( ((Mat[[2]][i,j] - Mat[[2]][i,J]) - (Mat[[2]][I,j] - Mat[[2]][I,J])) !=0 ,
               #               paste0('(x',i,j," - ",'x',i,J,") - (",'x',I,j," - ",'x',I,J,")"),"" )  
        }}
      
      }
    
  }
}



View(charcon)

####################################################################################


n=4
cn=0
numcon=0
ids=matrix
charcon=character(0)
c=numeric(4)

for (i in 1:(n)){
  for (I in (1):n){
    for (j in 1:(n)){
      for (J in (1):n){
        if(i!=j & i!=J & I!=i & J!=i & I!=j & J!=j & I!=J){
          cn=cn+1
          u1=paste0('u',i,j)
          u2=paste0('u',I,J)
          u3=paste0('u',i,J)
          u4=paste0('u',I,j)
          charcon[cn]=c(paste0('x',i,j,'(',u1,u2,"-",u3,u4,")"))
          #c[cn]=cn
          #numcon=numcon+ ifelse( ((Mat[[2]][i,j] - Mat[[2]][i,J]) - (Mat[[2]][I,j] - Mat[[2]][I,J])) !=0 ,1,0 )
          #charcon[cn]= ifelse( ((Mat[[2]][i,j] - Mat[[2]][i,J]) - (Mat[[2]][I,j] - Mat[[2]][I,J])) !=0 ,
          #               paste0('(x',i,j," - ",'x',i,J,") - (",'x',I,j," - ",'x',I,J,")"),"" )  
        }}
      
    }
    
  }
}




charcon[grepl('x13|x31',charcon)]


#################################################################









n=4 
m=3
cn=character()
c=0
  for (l in 1:(m)){ 
    for (L in (l):m){
      for (k in 1:(m)){
        for (K in (k):m){
        for (i in 1:(n-1)){
          for (I in (i+1):n){
            for (j in 1:(n-1)){
              for (J in (j+1):n){
              
                if(i!=j & i!=J & I!=i  & I!=j & J!=j & I!=J & ( (l==k & L!=K & L!=k & K!=k) | (L==K & l!=k & l!=K & K!=k) ) ){
                
                u1=paste0('(u',"(",i,",",j,")","(",l,",",k,")")
                u2=paste0('(u',"(",I,",",J,")","(",L,",",K,")")
                u3=paste0('(u',"(",I,",",j,")","(",l,",",k,")")
                u4=paste0('(u',"(",i,",",J,")","(",L,",",K,")")
                equis=paste0('(x',i,j," - ",'x',i,J,") - (",'x',I,j," - ",'x',I,J,")")
                c=1+c
                cn[c]=(paste(equis,u1,u2,'-',u3,u4)) }
        #numcon=numcon+ ifelse( ((Mat[[2]][i,j] - Mat[[2]][i,J]) - (Mat[[2]][I,j] - Mat[[2]][I,J])) ==0 ,1,0 )
  
                }
             
              }
          }
        }
      }
    }}
  }
View(cn)

case=cn[ grepl("\\(x13 - x14\\) - \\(x23 - x24\\)",cn) | grepl("\\(x31 - x32\\) - \\(x41 - x42\\)",cn) ]


sort(case)

unique(case)
#################################################################









n=4 
m=3
cnn=character()
c=0
for (l in 1:(m)){ 
  for (L in (1):m){
    for (k in 1:(m)){
      for (K in (1):m){
        for (i in 1:(n)){
          for (I in (1):n){
            for (j in 1:(n)){
              for (J in (1):n){
                
                if(i!=j & i!=J & I!=i  & I!=j & J!=j & I!=J & ( (l==k & L!=K)  ) ){
                  
                  u1=paste0('(u',"(",i,",",j,")","(",l,",",k,")")
                  u2=paste0('(u',"(",I,",",J,")","(",L,",",K,")")
                  u3=paste0('(u',"(",I,",",j,")","(",l,",",k,")")
                  u4=paste0('(u',"(",i,",",J,")","(",L,",",K,")")
                  equis=paste0('x',i,j)
                  c=1+c
                  cnn[c]=(paste(equis,u1,u2,'-',u3,u4)) }
                #numcon=numcon+ ifelse( ((Mat[[2]][i,j] - Mat[[2]][i,J]) - (Mat[[2]][I,j] - Mat[[2]][I,J])) ==0 ,1,0 )
                
              }
              
            }
          }
        }
      }
    }}
}



cnn

caser=cnn[ (grepl("\\(u\\(1,3\\)",cnn) & grepl("\\(u\\(1,4\\)",cnn) & grepl("\\(u\\(2,4\\)",cnn)
           & grepl("\\(u\\(2,3\\)",cnn)) |  (grepl("\\(u\\(3,1\\)",cnn) & grepl("\\(u\\(3,2\\)",cnn) &
            grepl("\\(u\\(4,2\\)",cnn)
           & grepl("\\(u\\(4,1\\)",cnn))]




################################################################################

n=3
cn=0
numcon=0
ids=matrix
charcon=character(0)
c=numeric(4)

for (i in 1:(n)){
  for (I in (1):n){
    for (j in 1:(n)){
      for (J in (1):n){
        if( (i!=J & I!=i) & ( I!=j & J!=j ) ){
          u1=paste0('u',i,j)
          u2=paste0('u',I,J)
          u3=paste0('u',i,j)
          u4=paste0('u',I,J)
          cn=cn+1
          charcon[cn]=c(paste0('(',u1,u2,"-",u3,u4,")"))
          #c[cn]=cn
          #numcon=numcon+ ifelse( ((Mat[[2]][i,j] - Mat[[2]][i,J]) - (Mat[[2]][I,j] - Mat[[2]][I,J])) !=0 ,1,0 )
          #charcon[cn]= ifelse( ((Mat[[2]][i,j] - Mat[[2]][i,J]) - (Mat[[2]][I,j] - Mat[[2]][I,J])) !=0 ,
          #               paste0('(x',i,j," - ",'x',i,J,") - (",'x',I,j," - ",'x',I,J,")"),"" )  
        }}
      
    }
    
  }
}
