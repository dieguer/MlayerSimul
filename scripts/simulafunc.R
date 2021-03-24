simula <- function(n=100,m=3, modelo= "Poisson",
          parameter=1,  sim=1000, semilla=200, cores=16) {
    RNGkind("L'Ecuyer-CMRG")
    set.seed(semilla)
    resvec  <-
              mclapply (c(1:sim), function(x) {
                mat <-  netgen(thetas = thetas, n = n, m = m,
                model = modelo, parameter = parameter)
                metro=optim(c(-1, 1), crito, hessian = T, simat = mat)$par
                return(c(metro,n))
               }, mc.cores = cores, mc.set.seed = T)
    return(resvec)
    }