 #########################################################
 context("CMP-functions")

 n.test <- 5
 test.identity <- FALSE
 test.extended <- TRUE


 test_that("logDensCMP1 works", {
   logDensCMP1 <- demest:::logDensCMP1
     mydcmp<- function(y,gamma,nu,log=FALSE){
       pdf <- nu*(y*log(gamma)-lgamma(y+1))
       if(log==FALSE){ pdf <- exp(pdf)}
      return(pdf)
    }

    for (seed in seq_len(n.test)) {
        set.seed(seed)
        y <- rpois(n = 1, lambda = 10)
        gamma <- runif(n = 1, max = 10)
        nu <- runif(n = 1, max = 10)
        ans.obtained <- logDensCMP1(x = x, gamma = gamma, nu = nu)
        ans.expected <- mydcmp(x = x, gamma = gamma, nu = nu, log = TRUE)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of dqcmp1 give same answer", {
    dqcmp <- demest:::dqcmp
    q.pdf<- function(y,gamma,nu, log=FALSE){
        if(log==FALSE){
            fact <- factorial(y)
            (gamma^y/fact)^nu
        } else {
            nu*(y*log(gamma)-(lgamma(y + 1)))
        }
    }
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        y <- rpois(n = 1, lambda = 10)
        gamma <- runif(n = 1, max = 10)
        nu <- runif(n = 1, max = 10)
        log <- runif(n = 1) < 0.5
        ans.obtained <- dqcmp1(x = x, gamma = gamma, nu = nu, log = log)
        ans.expected <- q.pdf(x = x, gamma = gamma, nu = nu, log = log)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("rcmpUnder okay", {
  for( seed in seq_len(n.test)){
    set.seed(seed)
    mu <- runif(n = 1, max = 10000)
    nu <- runif(n = 1, min = 1, max = 10)
    max <- 100
    y <- replicate(n = 10000, rcmpUnder(mu = mu, nu = nu, max = max))
    y_fin <- y[is.finite(y) == TRUE]
    expect_equal(mean(y_fin), mu + 1 / (2 * nu) - 0.5, tolerance = 0.02)
    expect_equal(var(y_fin), mu / nu , tolerance = 0.02)
  }
})

test_that("rcmpOver okay", {
  for( seed in seq_len(n.test)){
    set.seed(seed)
    mu <- runif(n = 1, max = 10000)
    nu <- runif(n = 1, max = (1 - 10^( -7)))
    max <- 100
    y <- replicate(n = 10000, rcmpOver(mu = mu, nu = nu, max = max))
    y_fin <- y[is.finite(y) == TRUE]
    expect_equal(mean(y_fin), mu + 1 / (2 * nu) - 0.5, tolerance = 0.02)
    expect_equal(var(y_fin), mu / nu , tolerance = 0.02)
  }
})

test_that("rcmp1 okay", {
  for( seed in seq_len(n.test)){
    set.seed(seed)
    mu <- runif(n = 1, max = 10000)
    nu <- runif(n = 1, max = 10)
    max <- 100
    y <- replicate(n = 1000, rcmp1(mu = mu, nu = nu, max = max))
    y_fin <- y[is.finite(y) == TRUE]
    if( nu < 1){
      disp <- mean(y_fin) < var(y_fin)
    }else{
      disp <- mean(y_fin) >= var(y_fin)
    }
    expect_equal(disp, TRUE)
  }
})



