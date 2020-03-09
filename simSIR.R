require(generalSEIR)

simSIR <- function(tmax,
                   var=NULL,
                   par,
                   inc=12) {
  
  .b <- as.numeric(par['b'])
  .m <- as.numeric(par['m'])
  .R0 <- as.numeric(par['R0'])
  .gamma <- as.numeric(par['gamma'])
  .N <- as.numeric(par['N'])
  .v <- 0
  if(!is.null(par['v'])) .v <- par['v']
  
  epsilon <- 0
  omega <- 0
  gamma <- par$gamma
  rho <- 0
  sigma <- 0
  
  eq <- TRUE
  S0 <- NULL
  I0 <- NULL
  E0 <- NULL
  if(!is.null(var)){
    S0 <- var[1]
    E0 <- 0
    I0 <- var[2]
    
    eq <- FALSE
  }
  sim <- simSEIRV(N=.N, S0=S0, E0=E0, I0=I0, model=41,
                  R0=.R0, gamma=gamma, sigma=sigma, rho=rho, 
                  epsilon=epsilon, omega=omega, v=.v,
                  b=.b, m=.m,
                  tmax=tmax, inc=inc, equilibrium=eq)
  
  return(sim)
}
