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

##########################################################

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
  stopifnot(nu < 1L)  # Do we need it here or the choice in rcmp is enough?
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


######################################################
  

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


########################################################################################
### Benson and Friel 2017 Algorithm
########################################################################################


# the function allows for both density and log-density


q.pdf(7,5,1.2, log=FALSE)
q.pdf(7,5,1.2, log=TRUE)

# Test they give the same results
dcmpois(x=7, mu=5, nu=1.2, unnormalised=TRUE)
dcmpois(x=7, mu=5, nu=1.2, unnormalised=TRUE, log=TRUE)


# Approximation of mean and variance of the CMP distribution starting from gamma and nu

mmt.approx <- function(gamma, nu){
  
  #Mean
  Ey <- gamma + 0.5*1/nu -0.5
  
  # Variance
  Vy <- gamma/nu
  
  return(list(mean=Ey, var=Vy))
}

mmt.approx(10,2)

# Approximation for optimal choice of p, for envelope Geo(p)

m2p <- function(gamma, nu){
  2*nu/(2*gamma*nu+1+nu)
}

m2p(10,2)

# Function to calculate the enveloping bounds
# p is set by default to the optimum suggested in Benson & Friel 2017  
# but I think the function should allow for other choices

bounds<- function(p=m2p(gamma,nu),gamma,nu){
  fl<- floor(gamma/((1-p)^(1/nu)))
  
  if(nu<1){
    (gamma^(nu*fl))/ (p*(1-p)^fl*prod(1:fl)^nu) 
  } else {
    (gamma^(floor(gamma))/prod(1:floor(gamma)))^(nu-1)
  }
}

bounds(gamma=9,nu=4)
bounds(gamma=4,nu=0.4)

# Function to generate values for the rejection sampling algorithm

benson.friel <- function(p=m2p(gamma,nu),gamma,nu){
  
  # Calculate the bounds
  B<-  bounds(p,gamma,nu)
  
  # Generate new proposed values
  ynew<- ifelse(nu<1,rgeom(1,p),rpois(1, gamma))
  
  # Calculate the factorial for ynew
  fact <- factorial(ynew)
  
  # Calculate the acceptance probability alpha
  alpha <- ifelse(nu<1, (gamma^ynew/fact)^nu/(B*(1-p)^ynew*p), 
                  (gamma^ynew/fact)^nu/(B*(gamma^ynew/fact)))
  
  return(list(ynew=ynew, alpha=alpha))
}


# Rejection sample algorithm

myrcmp <- function(n,gamma,nu){ # n is the number of draw to generate
  
  # r is a vector containing for each accepted draw the number of draw needed until acceptance
  r<-rep(0, n)
  
  # vec is going to contain the accepted draws
  vec <- c()
  
  
  # For each draw i=1,...,n the function keep on generating candidates until acceptance, i.e. runif(1)>cand$alpha
  # each time there is a rejection this is counted in vector r

  for(i in 1:n){
    # generate first candidate  
    cand<-benson.friel(gamma=gamma,nu=nu)
    
    while(runif(1)>cand$alpha) {
      cand <- benson.friel(gamma=gamma,nu=nu)
      r[i] <- r[i]+1
    }
    vec[i]<- cand$ynew
  }
  return(list(ynew=vec, eff=r))
}

par(mfrow=c(1,2))
hist(myrcmp(1000,gamma=11,nu=0.92)$ynew, main="CMP Charlotte")
hist(rcmpois(1000,mu=11,nu=0.92), main="CMP Chanialidis")
myrcmp(100,gamma=9,nu=0.9)
rcmpois(100,mu=9,nu=0.9)
# Exchange algorithm, acceptance ratio alpha for log gamma ~ N(mu,sigma) and log nu ~ N(delta, tau)

# Computation of the ratio of priors and the random uniform are together in the Chanialisi algorithm
# I keep this formulation to not overcomplicate the 'a.exch' 
# splitting the ratio allows the prior means and sds to not be included in the input of 'a.exch'
# and also to choose the prior distribution to use in a easier way 
# it can be modified of course
}
# onedimensional but prior mean is a linear combination of paramters depending on age,sex,region amd time 

log_u<- (log(runif(1))+ dnorm(loggamma.curr,mu,sigma,log=TRUE) 
- dnorm(loggamma.cand,mu,sigma,log=TRUE)  
+ dnorm(lognu.curr,delta,tau,log=TRUE) 
- dnorm(lognu.cand,delta,tau,log=TRUE) )

# mu= X^T B
# sigma ~ t^+(0,A_gamma)
# delta = -X^T B
# tau ~ t^+(0,A_nu)

# The likelihood ration calculations needs current and candidate values for 
# gamma=exp(loggamma) and nu=exp(lognu)
# in the input I considered gamma.curr, nu.curr, gamma.cand, nu.cand as already in the right form
# so previously there is a step where gamma.cand = exp(loggamma.cand) and nu.cand = exp(lognu.cand)
# the current values should be already in the right form
# the function give as result TRUE if candidate values are accepted and FALSE otherwise

# The function take as input current and values
a.exch <- function(y, gamma.curr, nu.curr, gamma.cand, nu.cand, log_u ){
  
  # Generate the auxiliary data
  y.new <- myrcmp(1,gamma.cand, nu.cand)$ynew
  
  # Ratio for current and candidate values with actual data and auxiliary data
  llik.ratio <- (sum(q.pdf(y,gamma.cand,nu.cand, log=TRUE))
                -sum(q.pdf(y,gamma.curr,nu.curr, log=TRUE))
                +sum(q.pdf(y.new,gamma.cand,nu.cand, log=TRUE))
                -sum(q.pdf(y.new,gamma.curr,nu.curr, log=TRUE)))
  
  llik.ratio>log_u  
  
} 


#############################################
###                Example                ###
#############################################

mu <- 2
delta <- 1

sigma <- 0.1
tau <- 0.1

loggamma.curr <- rnorm(1, mu, sigma)
lognu.curr<- rnorm(1, delta, tau)

loggamma.cand <- rnorm(1, mu, sigma)
lognu.cand <- rnorm(1, delta, tau)

log_u<- (log(runif(1))+ dnorm(loggamma.curr,mu,sigma,log=TRUE) 
         - dnorm(loggamma.cand,mu,sigma,log=TRUE)  
         + dnorm(lognu.curr,delta,tau,log=TRUE) 
         - dnorm(lognu.cand,delta,tau,log=TRUE) )


log_u
gamma.curr <- exp(loggamma.curr)
nu.curr<- exp(-lognu.curr)
gamma.cand <- exp(loggamma.cand)-3
nu.cand<- exp(-lognu.cand)
y<- rpois(1,10)


a.exch(y, gamma.curr, nu.curr, gamma.cand, nu.cand, log_u )



