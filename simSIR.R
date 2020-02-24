require(generalSEIR)

simSIR <- function(tmax,
                   var,
                   par,
                   inc=12) {
  nevents <- 11
  
  .b <- as.numeric(par['b'])
  .m <- as.numeric(par['m'])
  .R0 <- as.numeric(par['R0'])
  .gamma <- as.numeric(par['gamma'])
  .N <- as.numeric(par['N'])
  
  epsilon <- 0
  omega <- 0
  gamma <- par$gamma
  rho <- 0
  sigma <- 0
  whichBeta <- 2
  whichGamma <- 2
  whichSigma <- 1
  beta <- generalSEIR::getBeta(.R0, rho, epsilon, sigma, .m, .gamma, .N, whichBeta)
  
  
  par <- c(beta, .gamma, sigma, rho, epsilon, omega, .b, .m)
  spec <- as.integer(c(whichBeta, whichSigma, whichGamma))
  
  #simulate nsim times and plot together
  init <- as.integer(c(var[1], 0, var[2], var[3], 0))
  sim <- generalSEIR::simulate1(init, par, spec, nevents, tmax, inc)
  S <- sim[1:(tmax*inc)]
  I <- sim[(2*tmax*inc+1):(3*tmax*inc)]
  R <- sim[(3*tmax*inc+1):(4*tmax*inc)]
  incidence <- sim[(4*tmax*inc+1):(5*tmax*inc)]
  
  N <- S + I + R
  N[N==0] <- .N
  extinct <- I==0
  
  return(list(incidence=incidence, extinct=extinct, N = N))
}
