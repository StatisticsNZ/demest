###########################################################################
# The CMP desity function is f(y|theta,nu)= (theta^y/y!)^nu*1/Z(theta,nu), 
# Z(theta,nu) is the intractable normalising constant
# So f is split in two parts: Z and q(y|theta,nu)=(theta^y/y!)^nu

logDensCMP1 <- function(y, gamma, nu, useC = FALSE) {
    ## 'y'
    stopifnot(is.integer(y))
    stopifnot(identical(length(y), 1L))
    stopifnot(!is.na(y))
    stopifnot(y >= 0L)
    ## 'gamma'
    stopifnot(is.double(gamma))
    stopifnot(identical(length(gamma), 1L))
    stopifnot(!is.na(gamma))
    stopifnot(gamma >= 0L)
    ## 'nu'
    stopifnot(is.double(nu))
    stopifnot(identical(length(nu), 1L))
    stopifnot(!is.na(nu))
    stopifnot(nu >= 0L)
    if (useC) {
        .Call(logDensCMP1_R, y, gamma, nu)
    }
    else {
        nu * (y * log(gamma) - lgamma(y + 1))
    }
}

<<<<<<< HEAD
# Function to generate underdispersed data

rcmpUnder <- function(mu, nu, max){
  ## 'mu'
  stopifnot(is.double(mu))
  stopifnot(identical(length(mu), 1L))
  stopifnot(!is.na(mu))
  stopifnot(mu >= 0)
  ## 'nu'
  stopifnot(is.double(nu))
  stopifnot(identical(length(nu), 1L))
  stopifnot(!is.na(nu))
  stopifnot(nu > 0)
  ## 'max'
  stopifnot(is.double(max))
  stopifnot(identical(length(max), 1L))
  stopifnot(!is.na(max))
  stopifnot(max >= 0L)
  #
  fl <- floor(mu)
  logm <- lgamma(fl + 1)
  for(i in seq_len(max)){
    ynew <- rpois(n = 1, lambda = mu)
    logy <- lgamma(ynew + 1)
    log_a <- (nu - 1) * (log(mu) * (ynew - fl) - logy + logm)
    u <- log(runif(n = 1))
    if(u < log_a) 
      return(ynew)
  }
  return(-Inf)
}

# Function to generate overdispersed data 

rcmpOver <- function(mu, nu, max){
  ## 'mu'
  stopifnot(is.double(mu))
  stopifnot(identical(length(mu), 1L))
  stopifnot(!is.na(mu))
  stopifnot(mu >= 0L)
  ## 'nu'
  stopifnot(is.double(nu))
  stopifnot(identical(length(nu), 1L))
  stopifnot(!is.na(nu))
  stopifnot(nu >= 0L)
  ## 'max'
  stopifnot(is.double(max))
  stopifnot(identical(length(max), 1L))
  stopifnot(!is.na(max))
  stopifnot(max >= 0L)
  #
  p <- 2 * nu / (2 * mu * nu + 1 + nu)
  fl <- floor(mu / ((1 - p)^(1 / nu)))
  logfl <- lgamma(fl + 1)
  for(i in seq_len(max)){
    ynew <- rgeom(n = 1, prob = p)
    logy <- lgamma(ynew + 1)
    log_a <- (ynew - fl) * (nu * log(mu) - log(1 - p)) + nu * (logfl - logy)
    u <- log(runif(n = 1))
    if(u < log_a) 
      return(ynew)
  }
  return(-Inf)
}

# Random generation function for CMP data
rcmp1 <- function(mu, nu, max){
  ## 'mu'
  stopifnot(is.double(mu))
  stopifnot(identical(length(mu), 1L))
  stopifnot(!is.na(mu))
  stopifnot(mu >= 0L)
  ## 'nu'
  stopifnot(is.double(nu))
  stopifnot(identical(length(nu), 1L))
  stopifnot(!is.na(nu))
  stopifnot(nu >= 0L)
  ## 'max'
  stopifnot(is.double(max))
  stopifnot(identical(length(max), 1L))
  stopifnot(!is.na(max))
  stopifnot(max >= 0L)
  #
  if( nu < 1){
    rcmpOverLog(mu = mu , nu = nu, max = max)
  } else {
    rcmpUnderLog(mu = mu , nu = nu, max = max)
  }
} 


# Update Theta function
  
updateTheta_CMPVaryingUseExp <- function(object, y, exposure, useC = FALSE) {
  ## object
  stopifnot(methods::is(object, "CMPVaryingUseExp"))
  stopifnot(methods::validObject(object))
  ## y
  stopifnot(methods::is(y, "Counts"))
  stopifnot(identical(length(y), length(object@theta)))
  stopifnot(is.integer(y))
  stopifnot(all(y[!is.na(y)] >= 0L))
  ## exposure
  stopifnot(methods::is(exposure, "Counts"))
  stopifnot(is.double(exposure))
  stopifnot(all(exposure[!is.na(exposure)] >= 0))
  ## y and exposure
  stopifnot(identical(length(exposure), length(y)))
  stopifnot(all(is.na(exposure) <= is.na(y)))
  stopifnot(all(y[!is.na(y)][exposure[!is.na(y)] == 0] == 0L))
  if (useC) {
    .Call(updateTheta_PoissonVaryingUseExp_R, object, y, exposure)
  }
  else {
    theta <- object@theta
    scale <- object@scaleTheta
    scale.multiplier <- object@scaleThetaMultiplier
    nu <- object@nu # vector same length as 'theta'
    sigma <- object@sigma@.Data
    mean.log.nu <- object@meanLogNu@.Data # scalar
    sd.log.nu <- object@sdLogNu@.Data
    betas <- object@betas
    iterator <- object@iteratorBetas
    n.failed.prop.y.star <- 0L
    n.failed.prop.theta <- 0L
    n.accept.y <- 0L
    n.accept.theta <- 0L
    scale <- scale * scale.multiplier
    iterator <- resetB(iterator)
    for (i in seq_along(theta)) {
      indices <- iterator@indices
      mu <- 0
      for (b in seq_along(betas))
        mu <- mu + betas[[b]][indices[b]]
      y.is.missing <- is.na(y[i]) 
      if (y.is.missing) {
        mean <- mu
        sd <- sigma
      }
      else {
        th.curr <- theta[i]
        log.th.curr <- log(th.curr)
        mean <- log.th.curr
        sd <- scale
      }
      found.prop.theta <- FALSE
      while (!found.prop.theta && (attempt < max.attempt)) {
        attempt <- attempt + 1L
        log.th.prop <- stats::rnorm(n = 1L, mean = mean, sd = sd)
        found.prop.theta <- ((log.th.prop > lower + tolerance)
                             && (log.th.prop < upper - tolerance))
      }
      
      if(found.prop.theta){
        log.nu.prop <- stats::rnorm(n = 1L, mean = mean.log.nu, sd = sd.log.nu)
        nu.curr <- nu[i]
        th.prop <- exp(log.th.prop)
        nu.prop <- exp(log.nu.prop)
        y.star <- rcmp1(mu = th.prop, nu = nu.prop, max = max.attempt)
        found.y.star <- is.finite(y.star)
        if(found.y.star){
          if (y.is.missing)
            theta[i] <- th.prop
          else { 
            log.lik.curr <- logDensCMP1(y = y[i], theta = theta.curr, nu = nu.curr)
            log.lik.prop <- logDensCMP1(y = y[i], theta = theta.prop, nu = nu.prop)
            log.lik.curr.star <- logDensCMP1(y = y.star, theta = theta.curr, nu = nu.curr)
            log.lik.prop.star <- logDensCMP1(y = y.star, theta = theta.prop, nu = nu.prop)
            
            log.dens.th.curr <- stats::dnorm(x = log.th.curr, mean = mu, sd = sigma, log = TRUE) 
            log.dens.th.prop <- stats::dnorm(x = log.th.prop, mean = mu, sd = sigma, log = TRUE)
            log.dens.nu.curr <- stats::dnorm(x = log.nu.curr, mean = mean.nu, sd = sd.nu, log = TRUE) 
            log.dens.nu.prop <- stats::dnorm(x = log.nu.prop, mean = mean.nu, sd = sd.nu, log = TRUE)
            log.diff <- log.lik.prop -log.lik.curr + log.lik.curr.star - log.lik.prop.star 
            + log.dens.th.prop - log.dens.th.curr + log.dens.nu.prop - log.dens.nu.curr
            accept <- (log.diff >= 0) || (stats::runif(n = 1L) < exp(log.diff))
            if (accept) {
              n.accept.theta <- n.accept.theta + 1L
              theta[i] <- th.prop
              nu[i] <- nu.prop
            }
          }
        }
        else
          n.failed.prop.y.star <- n.failed.prop.y.star + 1L
      }
      else
        n.failed.prop.theta <- n.failed.prop.theta + 1L
      iterator <- advanceB(iterator)
    }
    object@theta <- theta
    object@nFailedPropYStar@.Data <- n.failed.prop.y.star
    object@nFailedPropTheta@.Data <- n.failed.prop.theta
    object@nAcceptTheta@.Data <- n.accept.theta
    object
  }
}
=======

## Function to calculate the enveloping bounds
## p is set by default to the optimum suggested in Benson & Friel 2017  
makeBoundsCMP <- function(theta, nu, p, useC = FALSE) {
    ## 'theta'
    stopifnot(is.double(theta))
    stopifnot(identical(length(theta), 1L))
    stopifnot(!is.na(theta))
    stopifnot(theta > 0)
    ## 'nu'
    stopifnot(is.double(nu))
    stopifnot(identical(length(nu), 1L))
    stopifnot(!is.na(nu))
    stopifnot(nu > 0)
    ## 'p'
    stopifnot(is.double(p))
    stopifnot(identical(length(p), 1L))
    stopifnot(!is.na(p))
    stopifnot(p >= 0)
    stopifnot(p <= 1)
    if (useC) {
        .Call(makeBoundsCMP_R, theta, nu, p)
    }
    else {
        if (nu < 1) {
            fl <- floor(theta / ((1 - p)^(1 / nu)))
            (theta^(nu * fl)) / (p * (1 - p)^fl * factorial(fl)^nu)
        }
        else {
            fl <- floor(theta)
            (theta^fl / factorial(fl))^(nu - 1)
        }
    }
}

    

updateTheta_CMPVaryingUseExp <- function(object, y, exposure, useC = FALSE) {
    ## object
    stopifnot(methods::is(object, "CMPVaryingUseExp"))
    stopifnot(methods::validObject(object))
    ## y
    stopifnot(methods::is(y, "Counts"))
    stopifnot(identical(length(y), length(object@theta)))
    stopifnot(is.integer(y))
    stopifnot(all(y[!is.na(y)] >= 0L))
    ## exposure
    stopifnot(methods::is(exposure, "Counts"))
    stopifnot(is.double(exposure))
    stopifnot(all(exposure[!is.na(exposure)] >= 0))
    ## y and exposure
    stopifnot(identical(length(exposure), length(y)))
    stopifnot(all(is.na(exposure) <= is.na(y)))
    stopifnot(all(y[!is.na(y)][exposure[!is.na(y)] == 0] == 0L))
    if (useC) {
        .Call(updateTheta_PoissonVaryingUseExp_R, object, y, exposure)
    }
    else {
        theta <- object@theta
        scale <- object@scaleTheta
        scale.multiplier <- object@scaleThetaMultiplier
        nu <- object@nu # vector same length as 'theta'
        sigma <- object@sigma@.Data
        mean.nu <- object@meanNu@.Data # scalar
        sd.nu <- object@sdNu@.Data
        betas <- object@betas
        iterator <- object@iteratorBetas
        n.failed.prop.y <- 0L
        n.failed.prop.theta <- 0L
        n.accept.y <- 0L
        n.accept.theta <- 0L
        scale <- scale * scale.multiplier
        iterator <- resetB(iterator)
        for (i in seq_along(theta)) {
            indices <- iterator@indices
            mu <- 0
            for (b in seq_along(betas))
                mu <- mu + betas[[b]][indices[b]]
            y.is.missing <- is.na(y[i]) ## what do we do if y is missing?
            p <- 2 * nu[i] / (2 * theta[i] * nu[i] + 1 + nu[i])
            th.curr <- theta[i]
            nu.curr <- nu[i]
            bounds <- makeBoundsCMP(theta = th.curr,
                                    nu = nu.curr,
                                    p = p)
            found.prop.y <- FALSE
            attempt.y <- 0L
            while (!found.prop.y && (attempt.y < max.attempt)) {
                attempt.y <- attempt.y + 1L
                if (nu.curr < 1) 
                    y.aux <- stats::rgeom(n = 1L, prob = p)
                else
                    y.aux <- stats::rpois(n = 1L, lambda = th.curr)
                y.fact <- factorial(y.aux)
                numerator.alpha <- ((th.curr ^ y.aux) / factorial(y)) ^ nu
                if (nu.curr < 1)
                    denominator.alpha <- B * ((1 - p) ^ y.aux) * p
                else
                    denominator.alpha <- B * (theta ^ y.aux) / y.fact
                alpha <- numerator.alpha / denominator.alpha
                found.y.aux <- stats::runif(1) < alpha
            }
            if (found.prop.y) {
                if (y.is.missing) {
                    mean <- mu
                    sd <- sigma
                }
                else {
                    log.th.curr <- log(th.curr)
                    mean <- log.th.curr
                    sd <- scale / sqrt(1 + y[i])
                }
                found.prop.theta <- FALSE
                attempt.theta <- 0L
                while (!found.prop.theta && (attempt.theta < max.attempt)) {
                    attempt.theta <- attempt.theta + 1L
                    log.th.prop <- stats::rnorm(n = 1L, mean = mean, sd = sd)
                    found.prop <- ((log.th.prop > lower + tolerance)
                        && (log.th.prop < upper - tolerance))
                }
                if (found.prop) {
                    th.prop <- exp(log.th.prop)
                    if (y.is.missing)
                        theta[i] <- th.prop
                    else {
                        log.lik <- (logDensCMP1(y = y[i], theta = theta.prop, nu = nu.prop)
                            - logDensCMP1(y = y[i], theta = theta.curr, nu = nu.curr)
                            + logDensCMP1(y = y.aux, theta = theta.prop, nu = nu.prop)
                            - logDensCMP1(y = y.aux, theta = theta.curr, nu = nu.curr))
                        log.u <- (log(stats::runif(n = 1L)) # is the ratio the right way around?
                            + stats::dnorm(x = log.th.curr, mean = mu, sd = sigma, log = TRUE) 
                            - stats::dnorm(x = log.th.prop, mean = mu, sd = sigma, log = TRUE)  
                            + stats::dnorm(x = log.nu.curr, mean = mean.nu, sd = sd.nu, log = TRUE) 
                            - stats::dnorm(x = log.nu.prop, mean = mean.nu, sd = sd.nu, log = TRUE))
                        accept <- log.lik > log.u
                        if (accept) {
                            n.accept.theta <- n.accept.theta + 1L
                            theta[i] <- th.prop
                        }
                    }
                }
                else
                    n.failed.prop.theta <- n.failed.prop.theta + 1L
            }
            else
                n.failed.prop.y <- n.failed.prop.y + 1L
            iterator <- advanceB(iterator)
        }
        object@theta <- theta
        object@nFailedPropY@.Data <- n.failed.prop.y
        object@nFailedPropTheta@.Data <- n.failed.prop.theta
        object@nAcceptTheta@.Data <- n.accept.theta
        object
    }
}


>>>>>>> 5c01aa465210030bcf5416c965524d10b4222c5e

