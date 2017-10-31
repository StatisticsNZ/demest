

dqcmp1 <- function(x, gamma, nu, log = FALSE, useC = FALSE) {
    ## x
    
    if (useC) {
        .Call(dqcmp1_R, x, gamma, nu, log)
    }
    else {
        if (log) {
            nu * y * log(gamma) - sum(log(seq_len(y)))
        }
        else {
            ((gamma ^ y) / factorial(y)) ^ nu
        }
    }
}



                                        # Model without exposure
# y ~ CMP(gamma, nu)
# log gamma ~ N(mu, sigma)
# log nu ~ N(delta, tau)

########################################################################################
### Benson and Friel 2017 Algorithm
########################################################################################

library(combayes)

# The CMP desity function is f(y|gamma,nu)= (gamma^y/y!)^nu*1/Z(gamma,nu), 
# Z(gamma,nu) is the intractable normalising constant
# So f is split in two parts: Z and q(y|gamma,nu)=(gamma^y/y!)^nu

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



