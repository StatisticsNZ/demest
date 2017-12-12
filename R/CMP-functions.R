## ###########################################################################
## # The CMP desity function is f(y|theta,nu)= (theta^y/y!)^nu*1/Z(theta,nu), 
## # Z(theta,nu) is the intractable normalising constant
## # So f is split in two parts: Z and q(y|theta,nu)=(theta^y/y!)^nu
## logDensCMP1 <- function(y, gamma, nu, useC = FALSE) {
##     ## 'y'
##     stopifnot(is.integer(y))
##     stopifnot(identical(length(y), 1L))
##     stopifnot(!is.na(y))
##     stopifnot(y >= 0L)
##     ## 'gamma'
##     stopifnot(is.double(gamma))
##     stopifnot(identical(length(gamma), 1L))
##     stopifnot(!is.na(gamma))
##     stopifnot(gamma >= 0L)
##     ## 'nu'
##     stopifnot(is.double(nu))
##     stopifnot(identical(length(nu), 1L))
##     stopifnot(!is.na(nu))
##     stopifnot(nu >= 0L)
##                                         #
##     if(gamma == 0){
##       gamma <- exp(-100)
##     } else {
##       gamma <- gamma
##     }
##     if (useC) {
##         .Call(logDensCMP1_R, y, gamma, nu)
##     }
##     else {
##         nu * (y * log(gamma) - lgamma(y + 1))
##     }
## }

# Function to generate underdispersed data

## rcmpUnder <- function(mu, nu, maxAttempt){
##     ## 'mu'
##     stopifnot(is.double(mu))
##     stopifnot(identical(length(mu), 1L))
##     stopifnot(!is.na(mu))
##     stopifnot(mu >= 0)
##     ## 'nu'
##     stopifnot(is.double(nu))
##     stopifnot(identical(length(nu), 1L))
##     stopifnot(!is.na(nu))
##     stopifnot(nu > 0)
##     ## 'maxAttempt'
##     stopifnot(is.double(maxAttempt))
##     stopifnot(identical(length(maxAttempt), 1L))
##     stopifnot(!is.na(maxAttempt))
##     stopifnot(maxAttempt >= 0L)
##                                         #
##     if( mu == 0){
##       lmu <- exp(-100)
##     } else {
##       lmu <- log(mu)
##     }
##     fl <- floor(mu)
##     logm <- lgamma(fl + 1)
##     for (i in seq_len(maxAttempt)) {
##         ynew <- rpois(n = 1L, lambda = mu)
##         logy <- lgamma(ynew + 1)
##         log_a <- (nu - 1) * (log(mu) * (ynew - fl) - logy + logm)
##         u <- log(runif(n = 1L))
##         if(u < log_a) 
##             return(ynew)
##     }
##     return(-Inf)
## }

# Function to generate overdispersed data 

## rcmpOver <- function(mu, nu, maxAttempt){
##     ## 'mu'
##     stopifnot(is.double(mu))
##     stopifnot(identical(length(mu), 1L))
##     stopifnot(!is.na(mu))
##     stopifnot(mu >= 0L)
##     ## 'nu'
##     stopifnot(is.double(nu))
##     stopifnot(identical(length(nu), 1L))
##     stopifnot(!is.na(nu))
##     stopifnot(nu >= 0L)
##     ## 'maxAttempt'
##     stopifnot(is.integer(maxAttempt))
##     stopifnot(identical(length(maxAttempt), 1L))
##     stopifnot(!is.na(maxAttempt))
##     stopifnot(maxAttempt >= 1L)
##                                         #
##     if( mu == 0){
##        lmu <- exp(-100)
##        p <- 0.99999
##     } else {
##        lmu <- log(mu)
##        p <- 2 * nu / (2 * mu * nu + 1 + nu)
##     }
##     p <- 2 * nu / (2 * mu * nu + 1 + nu)
##     fl <- floor(mu / ((1 - p)^(1 / nu)))
##     logfl <- lgamma(fl + 1)
##     for (i in seq_len(maxAttempt)) {
##         ynew <- rgeom(n = 1L, prob = p)
##         logy <- lgamma(ynew + 1)
##         log_a <- (ynew - fl) * (nu * log(mu) - log1p(-p)) + nu * (logfl - logy)
##         u <- log(runif(n = 1L))
##         if(u < log_a) 
##             return(ynew)
##     }
##     return(-Inf)
## }

## # Random generation function for CMP data
## rcmp1 <- function(mu, nu, maxAttempt){
##   ## 'mu'
##   stopifnot(is.double(mu))
##   stopifnot(identical(length(mu), 1L))
##   stopifnot(!is.na(mu))
##   stopifnot(mu >= 0L)
##   ## 'nu'
##   stopifnot(is.double(nu))
##   stopifnot(identical(length(nu), 1L))
##   stopifnot(!is.na(nu))
##   stopifnot(nu >= 0L)
##   ## 'maxAttempt'
##   stopifnot(is.double(maxAttempt))
##   stopifnot(identical(length(maxAttempt), 1L))
##   stopifnot(!is.na(maxAttempt))
##   stopifnot(maxAttempt >= 0L)
##   #
##   if( nu < 1){
##     rcmpOver(mu = mu , nu = nu, maxAttempt = maxAttempt)
##   } else {
##     rcmpUnder(mu = mu , nu = nu, maxAttempt = maxAttempt)
##   }
## } 


# Update Theta function
  
            
