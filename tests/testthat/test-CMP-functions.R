
context("CMP-functions")

n.test <- 5
test.identity <- FALSE
test.extended <- TRUE


test_that("dqcmp1 works", {
    dqcmp <- demest:::dqcmp
    q.pdf<- function(y,gamma,nu, log=FALSE){
        if(log==FALSE){
            fact <- factorial(y)
            (gamma^y/fact)^nu
        } else {
            nu*(y*log(gamma)-(sum(log(1:y))))
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


test_that("R and C versions of dqcmp1 give same answer", {
    dqcmp <- demest:::dqcmp
    q.pdf<- function(y,gamma,nu, log=FALSE){
        if(log==FALSE){
            fact <- factorial(y)
            (gamma^y/fact)^nu
        } else {
            nu*(y*log(gamma)-(sum(log(1:y))))
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



