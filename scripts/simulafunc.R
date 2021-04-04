simula <- function(n=100,m=3, modelo= "Poisson",
          parameter=1,  sim=1000, semilla=200, cores=16, strobj=structo1(3)) {
    RNGkind("L'Ecuyer-CMRG")
    set.seed(semilla)
    cho=choose(n,2)
    resvec  <-
              mclapply (c(1:sim), function(x) {
                mat <-  netgen(thetas = thetas, n = n, m = m,
                model = modelo, parameter = parameter)
                metro=optim(c(-1, 1), crito, hessian = T, 
                simat = mat,strobject=strobj,chosen=cho)$par
                return(c(metro,n))
               }, mc.cores = cores, mc.set.seed = T)
    return(resvec)
    }