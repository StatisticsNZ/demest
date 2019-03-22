
context("iterators-methods")

n.test <- 5
test.identity <- FALSE
test.extended <- FALSE

## AlongIterator

test_that("advanceA gives valid answer", {
    advanceA <- demest:::advanceA
    AlongIterator <- demest:::AlongIterator
    ## dim = 3:4, iAlong = 1L
    x <- AlongIterator(dim = 3:4, iAlong = 1L)
    expect_identical(x@indices, 1:3)
    x <- advanceA(x)
    expect_identical(x@indices, 4:6)
    x <- advanceA(x)
    expect_identical(x@indices, 7:9)
    x <- advanceA(x)
    expect_identical(x@indices, 10:12)
    x <- advanceA(x)
    expect_identical(x@indices, 1:3)
    ## dim = 2:4, iAlong = 2L
    x <- AlongIterator(dim = 2:4, iAlong = 2L)
    expect_identical(x@indices, c(1L, 3L, 5L))
    x <- advanceA(x)
    expect_identical(x@indices, c(2L, 4L, 6L))
    x <- advanceA(x)
    expect_identical(x@indices, c(7L, 9L, 11L))
    x <- advanceA(x)
    expect_identical(x@indices, c(8L, 10L, 12L))
    x <- advanceA(x)
    expect_identical(x@indices, c(13L, 15L, 17L))
    x <- advanceA(x)
    expect_identical(x@indices, c(14L, 16L, 18L))
    x <- advanceA(x)
    expect_identical(x@indices, c(19L, 21L, 23L))
    x <- advanceA(x)
    expect_identical(x@indices, c(20L, 22L, 24L))
    x <- advanceA(x)
    expect_identical(x@indices, c(1L, 3L, 5L))
    ## dim = c(4L, 3L, 2L, 2L), iAlong = 4L
    x <- AlongIterator(dim = c(4L, 3L, 2L, 2L), iAlong = 4L)
    expect_identical(x@indices, c(1L, 25L))
    x <- advanceA(x)
    expect_identical(x@indices, c(2L, 26L))
    x <- advanceA(x)
    expect_identical(x@indices, c(3L, 27L))
    for (i in 1:22)
        x <- advanceA(x)
    expect_identical(x@indices, c(1L, 25L))
})

test_that("R and C versions of advanceA give same answer", {
    advanceA <- demest:::advanceA
    AlongIterator <- demest:::AlongIterator
    finishedAlong <- function(x) (x@iWithin == x@nWithin) && (x@iBetween == x@nBetween)
    n.dim <- round(runif(n = 1, min = 1, max = 4))
    dim <- as.integer(round(runif(n = n.dim, min = 1, max = 10)))
    iAlong <- sample.int(n = n.dim, size = 1)
    x.R <- AlongIterator(dim = dim, iAlong = iAlong)
    x.C <- x.R
    while (!finishedAlong(x.R)) {
        x.R <- advanceA(x.R, useC = FALSE)
        x.C <- advanceA(x.C, useC = TRUE)
        expect_identical(x.R, x.C)
    }
})

test_that("resetA gives valid answer", {
    advanceA <- demest:::advanceA
    resetA <- demest:::resetA
    AlongIterator <- demest:::AlongIterator
    ## dim = 3:4, iAlong = 1L
    x <- AlongIterator(dim = 3:4, iAlong = 1L)
    x.initial <- x
    x <- advanceA(x)
    expect_false(identical(x, x.initial))
    x <- resetA(x)
    expect_identical(x, x.initial)
    ## dim = 2:4, iAlong = 2L
    x <- AlongIterator(dim = 2:4, iAlong = 2L)
    x.initial <- x
    x <- advanceA(x)
    expect_false(identical(x, x.initial))
    x <- resetA(x)
    expect_identical(x, x.initial)
    ## dim = c(4L, 3L, 2L, 2L), iAlong = 4L
    x <- AlongIterator(dim = c(4L, 3L, 2L, 2L), iAlong = 4L)
    x.initial <- x
    x <- advanceA(x)
    expect_false(identical(x, x.initial))
    x <- resetA(x)
    expect_identical(x, x.initial)
})

test_that("R and C versions of resetA give same answer", {
    resetA <- demest:::resetA
    advanceA <- demest:::advanceA
    finishedAlong <- function(x) (x@iWithin == x@nWithin) && (x@iBetween == x@nBetween)
    AlongIterator <- demest:::AlongIterator
    n.dim <- round(runif(n = 1, min = 1, max = 4))
    dim <- as.integer(round(runif(n = n.dim, min = 1, max = 10)))
    iAlong <- sample.int(n = n.dim, size = 1)
    x <- AlongIterator(dim = dim, iAlong = iAlong)
    if (!finishedAlong(x))
        x <- advanceA(x)
    x.R <- resetA(x, useC = FALSE)
    x.C <- resetA(x, useC = TRUE)
    expect_identical(x.R, x.C)
})


## BetaIterator

test_that("advanceB gives valid answer", {
    advanceB <- demest:::advanceB
    BetaIterator <- demest:::BetaIterator
    ## two dimensions, all terms, in order
    x <- BetaIterator(dim = 3:4, margins = list(0L, 1L, 2L, 1:2))
    expect_identical(x@indices, c(1L, 1L, 1L, 1L))
    x <- advanceB(x)
    expect_identical(x@indices, c(1L, 2L, 1L, 2L))
    x <- advanceB(x)
    expect_identical(x@indices, c(1L, 3L, 1L, 3L))
    x <- advanceB(x)
    expect_identical(x@indices, c(1L, 1L, 2L, 4L))
    x <- advanceB(x)
    expect_identical(x@indices, c(1L, 2L, 2L, 5L))
    x <- advanceB(x)
    expect_identical(x@indices, c(1L, 3L, 2L, 6L))
    x <- advanceB(x)
    expect_identical(x@indices, c(1L, 1L, 3L, 7L))
    x <- advanceB(x)
    expect_identical(x@indices, c(1L, 2L, 3L, 8L))
    x <- advanceB(x)
    expect_identical(x@indices, c(1L, 3L, 3L, 9L))
    x <- advanceB(x)
    expect_identical(x@indices, c(1L, 1L, 4L, 10L))
    x <- advanceB(x)
    expect_identical(x@indices, c(1L, 2L, 4L, 11L))
    x <- advanceB(x)
    expect_identical(x@indices, c(1L, 3L, 4L, 12L))
    x <- advanceB(x)
    expect_identical(x@indices, c(1L, 1L, 1L, 1L))
    ## two dimensions, all terms, out of order
    x <- BetaIterator(dim = 3:4, margins = list(0L, 2:1, 2L, 1L))
    expect_identical(x@indices, c(1L, 1L, 1L, 1L))
    x <- advanceB(x)
    expect_identical(x@indices, c(1L, 5L, 1L, 2L))
    x <- advanceB(x)
    expect_identical(x@indices, c(1L, 9L, 1L, 3L))
    x <- advanceB(x)
    expect_identical(x@indices, c(1L, 2L, 2L, 1L))
    x <- advanceB(x)
    expect_identical(x@indices, c(1L, 6L, 2L, 2L))
    x <- advanceB(x)
    expect_identical(x@indices, c(1L, 10L, 2L, 3L))
    x <- advanceB(x)
    expect_identical(x@indices, c(1L, 3L, 3L, 1L))
    x <- advanceB(x)
    expect_identical(x@indices, c(1L, 7L, 3L, 2L))
    x <- advanceB(x)
    expect_identical(x@indices, c(1L, 11L, 3L, 3L))
    x <- advanceB(x)
    expect_identical(x@indices, c(1L, 4L, 4L, 1L))
    x <- advanceB(x)
    expect_identical(x@indices, c(1L, 8L, 4L, 2L))
    x <- advanceB(x)
    expect_identical(x@indices, c(1L, 12L, 4L, 3L))
    x <- advanceB(x)
    expect_identical(x@indices, c(1L, 1L, 1L, 1L))
    ## three dimensions, not all terms, out of order
    x <- BetaIterator(dim = 2:4, margins = list(0L, 3L, 1L, 1:3))
    expect_identical(x@indices, c(1L, 1L, 1L, 1L))
    x <- advanceB(x)
    identical(x@indices, c(1L, 1L, 2L, 2L))
    x <- advanceB(x)
    identical(x@indices, c(1L, 1L, 1L, 1L))
    x <- advanceB(x)
    identical(x@indices, c(1L, 1L, 2L, 2L))
    x <- advanceB(x)
    identical(x@indices, c(1L, 1L, 1L, 1L))
    x <- advanceB(x)
    identical(x@indices, c(1L, 1L, 2L, 2L))
    x <- advanceB(x)
    identical(x@indices, c(1L, 2L, 1L, 3L))
    x <- advanceB(x)
    identical(x@indices, c(1L, 2L, 2L, 4L))
    x <- advanceB(x)
    identical(x@indices, c(1L, 2L, 1L, 3L))
    x <- advanceB(x)
    identical(x@indices, c(1L, 2L, 2L, 4L))
    x <- advanceB(x)
    identical(x@indices, c(1L, 2L, 1L, 3L))
    x <- advanceB(x)
    identical(x@indices, c(1L, 2L, 2L, 4L))
    x <- advanceB(x)
    identical(x@indices, c(1L, 3L, 1L, 5L))
    x <- advanceB(x)
    identical(x@indices, c(1L, 3L, 2L, 6L))
    x <- advanceB(x)
    identical(x@indices, c(1L, 3L, 1L, 5L))
    x <- advanceB(x)
    identical(x@indices, c(1L, 3L, 2L, 6L))
    x <- advanceB(x)
    identical(x@indices, c(1L, 3L, 1L, 5L))
    x <- advanceB(x)
    identical(x@indices, c(1L, 3L, 2L, 6L))
    x <- advanceB(x)
    identical(x@indices, c(1L, 4L, 1L, 7L))
    x <- advanceB(x)
    identical(x@indices, c(1L, 4L, 2L, 8L))
    x <- advanceB(x)
    identical(x@indices, c(1L, 4L, 1L, 7L))
    x <- advanceB(x)
    identical(x@indices, c(1L, 4L, 2L, 8L))
    x <- advanceB(x)
    identical(x@indices, c(1L, 4L, 1L, 7L))
    x <- advanceB(x)
    identical(x@indices, c(1L, 4L, 2L, 8L))
    x <- advanceB(x)
    identical(x@indices, c(1L, 1L, 1L, 1L))
    ## intercept only
    x <- BetaIterator(dim = 2:4, margins = list(0L))
    expect_identical(x@indices, 1L)
    for (i in 1:5) {
        x <- advanceB(x)
        expect_identical(x@indices, 1L)
    }
})

test_that("advanceB can be used to calculate 'mu' from 'betas'", {
    BetaIterator <- demest:::BetaIterator
    advanceB <- demest:::advanceB
    ## saturated model
    betas <- list(rnorm(1), rnorm(5), rnorm(4), rnorm(20))
    mu.expected <- betas[[1]] + betas[[2]] + rep(betas[[3]], each = 5) + betas[[4]]
    iterator <- BetaIterator(dim = c(5L, 4L), margin = list(0L, 1L, 2L, 1:2))
    mu.obtained <- numeric(20)
    for (i in 1:20) {
        ind <- iterator@indices
        mu.obtained[i] <- (betas[[1]][ind[1]] + betas[[2]][ind[2]] +
                           betas[[3]][ind[3]] + betas[[4]][ind[4]])
        iterator <- advanceB(iterator)
    }
    expect_identical(mu.obtained, mu.expected)
    expect_identical(iterator@indices, rep(1L, 4))
    ## dimension 2 main effect only
    betas <- list(rnorm(1), rnorm(4))
    mu.expected <- betas[[1]] + rep(betas[[2]], each = 5)
    iterator <- BetaIterator(dim = c(5L, 4L), margin = list(0L, 2L))
    mu.obtained <- numeric(20)
    for (i in 1:20) {
        ind <- iterator@indices
        mu.obtained[i] <- (betas[[1]][ind[1]] + betas[[2]][ind[2]])
        iterator <- advanceB(iterator)
    }
    expect_identical(mu.obtained, mu.expected)
    expect_identical(iterator@indices, rep(1L, 2))
})

test_that("R and C versions of advanceB give same answer", {
    advanceB <- demest:::advanceB
    BetaIterator <- demest:::BetaIterator
    ## two dimensions, all terms, in order
    x <- BetaIterator(dim = 3:4, margins = list(0L, 1L, 2L, 1:2))
    x.C <- x.R <- x
    for (i in 1:20) {
        x.R <- advanceB(x.R)
        x.C <- advanceB(x.C)
        expect_identical(x.R, x.C)
    }
    ## two dimensions, all terms, out of order
    x <- BetaIterator(dim = 3:4, margins = list(0L, 2:1, 2L, 1L))
    x.C <- x.R <- x
    for (i in 1:20) {
        x.R <- advanceB(x.R)
        x.C <- advanceB(x.C)
        expect_identical(x.R, x.C)
    }
    ## three dimensions, not all terms, out of order
    x <- BetaIterator(dim = 2:4, margins = list(0L, 3L, 1L, 1:3))
    x.C <- x.R <- x
    for (i in 1:30) {
        x.R <- advanceB(x.R)
        x.C <- advanceB(x.C)
        expect_identical(x.R, x.C)
    }
    ## intercept only
    x <- BetaIterator(dim = 2:4, margins = list(0L))
    x.C <- x.R <- x
    for (i in 1:20) {
        x.R <- advanceB(x.R)
        x.C <- advanceB(x.C)
        expect_identical(x.R, x.C)
    }
})

test_that("resetB gives valid answer", {
    resetB <- demest:::resetB
    advanceB <- demest:::advanceB
    BetaIterator <- demest:::BetaIterator
    ## two dimensions, all terms, in order
    x <- BetaIterator(dim = 3:4, margins = list(0L, 1L, 2L, 1:2))
    x0 <- x
    for (i in 1:10)
        x <- advanceB(x)
    x <- resetB(x)
    expect_identical(x, x0)
    ## two dimensions, all terms, out of order
    x <- BetaIterator(dim = 3:4, margins = list(0L, 2:1, 2L, 1L))
    x0 <- x
    for (i in 1:10)
        x <- advanceB(x)
    x <- resetB(x)
    expect_identical(x, x0)
    ## three dimensions, not all terms, out of order
    x <- BetaIterator(dim = 2:4, margins = list(0L, 3L, 1L, 1:3))
    x0 <- x
    for (i in 1:10)
        x <- advanceB(x)
    x <- resetB(x)
    expect_identical(x, x0)
})

test_that("R and C versions of resetB give same answer", {
    resetB <- demest:::resetB
    advanceB <- demest:::advanceB
    BetaIterator <- demest:::BetaIterator
    ## two dimensions, all terms, in order
    x <- BetaIterator(dim = 3:4, margins = list(0L, 1L, 2L, 1:2))
    for (i in 1:10)
        x <- advanceB(x)
    x.R <- resetB(x, useC = FALSE)
    x.C <- resetB(x, useC = TRUE)
    expect_identical(x.R, x.C)
    ## two dimensions, all terms, out of order
    x <- BetaIterator(dim = 3:4, margins = list(0L, 2:1, 2L, 1L))
    for (i in 1:10)
        x <- advanceB(x)
    x.R <- resetB(x, useC = FALSE)
    x.C <- resetB(x, useC = TRUE)
    expect_identical(x.R, x.C)
    ## three dimensions, not all terms, out of order
    x <- BetaIterator(dim = 2:4, margins = list(0L, 3L, 1L, 1:3))
    for (i in 1:10)
        x <- advanceB(x)
    x.R <- resetB(x, useC = FALSE)
    x.C <- resetB(x, useC = TRUE)
    expect_identical(x.R, x.C)
})


## CohortIteratorAccession

test_that("advanceCA gives valid answer", {
    advanceCA <- demest:::advanceCA
    ## dim = 3:4, iAge = 2L, iTime = 1L
    x <- new("CohortIteratorAccession",
             i = 4L,
             nTime = 3L,
             stepTime = 1L,
             iTime = 1L,
             hasAge = TRUE,
             nAge = 4L,
             stepAge = 3L,
             iAge = 2L,
             finished = FALSE)
    ans.obtained <- advanceCA(x)
    ans.expected <- new("CohortIteratorAccession",
                        i = 8L,
                        nTime = 3L,
                        stepTime = 1L,
                        iTime = 2L,
                        hasAge = TRUE,
                        nAge = 4L,
                        stepAge = 3L,
                        iAge = 3L,
                        finished = FALSE)
    expect_identical(ans.obtained, ans.expected)
    ## dim = 3:4, iAge = 3L, iTime = 3L
    x <- new("CohortIteratorAccession",
             i = 8L,
             nTime = 4L,
             stepTime = 3L,
             iTime = 3L,
             hasAge = TRUE,
             nAge = 3L,
             stepAge = 1L,
             iAge = 2L,
             finished = FALSE)
    ans.obtained <- advanceCA(x)
    ans.expected <- new("CohortIteratorAccession",
                        i = 12L,
                        nTime = 4L,
                        stepTime = 3L,
                        iTime = 4L,
                        hasAge = TRUE,
                        nAge = 3L,
                        stepAge = 1L,
                        iAge = 3L,
                        finished = TRUE)
    expect_identical(ans.obtained, ans.expected)
    ## dim = 2:5, iAge = 3L, iTime = 4L
    x <- new("CohortIteratorAccession",
             i = 85L,
             nTime = 5L,
             stepTime = 24L,
             iTime = 4L,
             hasAge = TRUE,
             nAge = 4L,
             stepAge = 6L,
             iAge = 3L,
             finished = FALSE)
    ans.obtained <- advanceCA(x)
    ans.expected <- new("CohortIteratorAccession",
                        i = 115L,
                        nTime = 5L,
                        stepTime = 24L,
                        iTime = 5L,
                        hasAge = TRUE,
                        nAge = 4L,
                        stepAge = 6L,
                        iAge = 4L,
                        finished = TRUE)
    expect_identical(ans.obtained, ans.expected)
})


test_that("R and C versions of advanceCA give same answer", {
    advanceCA <- demest:::advanceCA
    ## dim = 3:4, iAge = 2L, iTime = 1L
    x <- new("CohortIteratorAccession",
             i = 4L,
             nTime = 3L,
             stepTime = 1L,
             iTime = 1L,
             hasAge = TRUE,
             nAge = 4L,
             stepAge = 3L,
             iAge = 2L,
             finished = FALSE)
    ans.R <- advanceCA(x, useC = FALSE)
    ans.C <- advanceCA(x, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## dim = 3:4, iAge = 3L, iTime = 3L
    x <- new("CohortIteratorAccession",
             i = 8L,
             nTime = 4L,
             stepTime = 3L,
             iTime = 3L,
             hasAge = TRUE,
             nAge = 3L,
             stepAge = 1L,
             iAge = 2L,
             finished = FALSE)
    ans.R <- advanceCA(x, useC = FALSE)
    ans.C <- advanceCA(x, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## dim = 2:5, iAge = 3L, iTime = 4L
    x <- new("CohortIteratorAccession",
             i = 85L,
             nTime = 5L,
             stepTime = 24L,
             iTime = 4L,
             hasAge = TRUE,
             nAge = 4L,
             stepAge = 6L,
             iAge = 3L,
             finished = FALSE)
    ans.R <- advanceCA(x, useC = FALSE)
    ans.C <- advanceCA(x, useC = TRUE)
    expect_identical(ans.R, ans.C)
})

test_that("advanceCP gives valid answer", {
    advanceCP <- demest:::advanceCP
    ## dim = 3:4, iAge = 2L, iTime = 1L
    x <- new("CohortIteratorPopulation",
             i = 4L,
             nTime = 3L,
             stepTime = 1L,
             iTime = 1L,
             hasAge = TRUE,
             nAge = 4L,
             stepAge = 3L,
             iAge = 2L,
             finished = FALSE)
    ans.obtained <- advanceCP(x)
    ans.expected <- new("CohortIteratorPopulation",
                        i = 8L,
                        nTime = 3L,
                        stepTime = 1L,
                        iTime = 2L,
                        hasAge = TRUE,
                        nAge = 4L,
                        stepAge = 3L,
                        iAge = 3L,
                        finished = FALSE)
    expect_identical(ans.obtained, ans.expected)
    ## dim = 3:4, iAge = 1L, iTime = 2L
    x <- new("CohortIteratorPopulation",
             i = 9L,
             nTime = 4L,
             stepTime = 3L,
             iTime = 3L,
             hasAge = TRUE,
             nAge = 3L,
             stepAge = 1L,
             iAge = 3L,
             finished = FALSE)
    ans.obtained <- advanceCP(x)
    ans.expected <- new("CohortIteratorPopulation",
                        i = 12L,
                        nTime = 4L,
                        stepTime = 3L,
                        iTime = 4L,
                        hasAge = TRUE,
                        nAge = 3L,
                        stepAge = 1L,
                        iAge = 3L,
                        finished = TRUE)
    expect_identical(ans.obtained, ans.expected)
    ## dim = 3:4, iAge = 0L, iTime = 2L 
    x <- new("CohortIteratorPopulation",
             i = 3L,
             nTime = 4L,
             stepTime = 3L,
             iTime = 1L,
             hasAge = FALSE,
             nAge = as.integer(NA),
             stepAge = as.integer(NA),
             iAge = as.integer(NA),
             finished = FALSE)
    ans.obtained <- advanceCP(x)
    ans.expected <- new("CohortIteratorPopulation",
                        i = 6L,
                        nTime = 4L,
                        stepTime = 3L,
                        iTime = 2L,
                        hasAge = FALSE,
                        nAge = as.integer(NA),
                        stepAge = as.integer(NA),
                        iAge = as.integer(NA),
                        finished = FALSE)
    expect_identical(ans.obtained, ans.expected)
    ## dim = 2:5, iAge = 3L, iTime = 4L
    x <- new("CohortIteratorPopulation",
             i = 85L,
             nTime = 5L,
             stepTime = 24L,
             iTime = 4L,
             hasAge = TRUE,
             nAge = 4L,
             stepAge = 6L,
             iAge = 3L,
             finished = FALSE)
    ans.obtained <- advanceCP(x)
    ans.expected <- new("CohortIteratorPopulation",
                        i = 115L,
                        nTime = 5L,
                        stepTime = 24L,
                        iTime = 5L,
                        hasAge = TRUE,
                        nAge = 4L,
                        stepAge = 6L,
                        iAge = 4L,
                        finished = TRUE)
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of advanceCP give same answer", {
    advanceCP <- demest:::advanceCP
    ## dim = 3:4, iAge = 2L, iTime = 1L
    x <- new("CohortIteratorPopulation",
             i = 4L,
             nTime = 3L,
             stepTime = 1L,
             iTime = 1L,
             hasAge = TRUE,
             nAge = 4L,
             stepAge = 3L,
             iAge = 2L,
             finished = FALSE)
    ans.R <- advanceCP(x, useC = FALSE)
    ans.C <- advanceCP(x, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## dim = 3:4, iAge = 1L, iTime = 2L
    x <- new("CohortIteratorPopulation",
             i = 9L,
             nTime = 4L,
             stepTime = 3L,
             iTime = 3L,
             hasAge = TRUE,
             nAge = 3L,
             stepAge = 1L,
             iAge = 3L,
             finished = FALSE)
    ans.R <- advanceCP(x, useC = FALSE)
    ans.C <- advanceCP(x, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## dim = 3:4, iAge = 0L, iTime = 2L 
    x <- new("CohortIteratorPopulation",
             i = 3L,
             nTime = 4L,
             stepTime = 3L,
             iTime = 1L,
             hasAge = FALSE,
             nAge = as.integer(NA),
             stepAge = as.integer(NA),
             iAge = as.integer(NA),
             finished = FALSE)
    ans.R <- advanceCP(x, useC = FALSE)
    ans.C <- advanceCP(x, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## dim = 2:5, iAge = 3L, iTime = 4L
    x <- new("CohortIteratorPopulation",
             i = 85L,
             nTime = 5L,
             stepTime = 24L,
             iTime = 4L,
             hasAge = TRUE,
             nAge = 4L,
             stepAge = 6L,
             iAge = 3L,
             finished = FALSE)
    ans.R <- advanceCP(x, useC = FALSE)
    ans.C <- advanceCP(x, useC = TRUE)
    expect_identical(ans.R, ans.C)
})

test_that("advanceCC works", {
    advanceCC <- demest:::advanceCC
    resetCC <- demest:::resetCC
    CohortIterator <- demest:::CohortIterator
    EntriesMovements <- dembase:::EntriesMovements
    ## with age - last age group open
    entries <- Counts(array(1:36,
                            dim = c(3, 3, 2, 2),
                            dimnames = list(age = c("0-4", "5-9", "10+"),
                                region = 1:3,
                                time = c("2001-2005", "2006-2010"),
                                triangle = c("Lower", "Upper"))))
    template <- Counts(array(0L,
                             dim = c(3, 3, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 region = 1:3,
                                 time = c("2001-2005", "2006-2010"),
                                 triangle = c("Lower", "Upper"))))
    set.seed(1)
    component <- EntriesMovements(entries = entries,
                                  template = template,
                                  name = "immigration")
    iterator <- CohortIterator(component)
    iterator <- resetCC(iterator, i = 19L)
    iterator <- advanceCC(iterator)
    expect_identical(iterator@i, 2L)
    iterator <- advanceCC(iterator)
    expect_identical(iterator@i, 29L)
    expect_false(iterator@finished)    
    iterator <- advanceCC(iterator)
    expect_identical(iterator@i, 12L)
    expect_true(iterator@finished)
    iterator <- resetCC(iterator, i = 21L)
    expect_false(iterator@finished)
    iterator <- advanceCC(iterator)
    expect_identical(iterator@i, 3L)
    iterator <- advanceCC(iterator)
    expect_identical(iterator@i, 30L)
    iterator <- advanceCC(iterator)
    expect_identical(iterator@i, 12L)
    expect_true(iterator@finished)
    ## with age - last age group closed
    entries <- Counts(array(1:36,
                            dim = c(3, 3, 2, 2),
                            dimnames = list(age = c("0-4", "5-9", "10-14"),
                                region = 1:3,
                                time = c("2001-2005", "2006-2010"),
                                triangle = c("Lower", "Upper"))))
    template <- Counts(array(0L,
                             dim = c(3, 3, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10-14"),
                                 region = 1:3,
                                 time = c("2001-2005", "2006-2010"),
                                 triangle = c("Lower", "Upper"))))
    set.seed(1)
    component <- EntriesMovements(entries = entries,
                                  template = template,
                                  name = "immigration")
    iterator <- CohortIterator(component)
    iterator <- resetCC(iterator, i = 19L)
    iterator <- advanceCC(iterator)
    expect_identical(iterator@i, 2L)
    iterator <- advanceCC(iterator)
    expect_identical(iterator@i, 29L)
    expect_false(iterator@finished)    
    iterator <- advanceCC(iterator)
    expect_identical(iterator@i, 12L)
    expect_true(iterator@finished)
    iterator <- resetCC(iterator, i = 21L)
    expect_true(iterator@finished)
    iterator <- resetCC(iterator, i = 3L)
    expect_false(iterator@finished)
    iterator <- advanceCC(iterator)
    expect_identical(iterator@i, 30L)
    expect_true(iterator@finished)
    ## without age
    entries <- Counts(array(1:12,
                            dim = c(3, 4),
                            dimnames = list(region = 1:3,
                                time = c("2001-2005", "2006-2010", "2011-2015", "2016-2020"))))
    template <- Counts(array(0L,
                             dim = c(3, 4),
                             dimnames = list(region = 1:3,
                                 time = c("2001-2005", "2006-2010", "2011-2015", "2016-2020"))))
    set.seed(1)
    component <- EntriesMovements(entries = entries,
                                  template = template,
                                  name = "immigration")
    iterator <- CohortIterator(component)
    iterator <- resetCC(iterator, i = 5L)
    expect_false(iterator@finished)
    iterator <- advanceCC(iterator)
    expect_identical(iterator@i, 8L)
    expect_false(iterator@finished)
    iterator <- advanceCC(iterator)
    expect_identical(iterator@i, 11L)
    expect_true(iterator@finished)
})

test_that("R and C versions of advanceCC give same answer", {
    advanceCC <- demest:::advanceCC
    resetCC <- demest:::resetCC
    CohortIterator <- demest:::CohortIterator
    EntriesMovements <- dembase:::EntriesMovements
    ## with age - last age group open
    entries <- Counts(array(1:36,
                            dim = c(3, 3, 2, 2),
                            dimnames = list(age = c("0-4", "5-9", "10+"),
                                region = 1:3,
                                time = c("2001-2005", "2006-2010"),
                                triangle = c("Lower", "Upper"))))
    template <- Counts(array(0L,
                             dim = c(3, 3, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 region = 1:3,
                                 time = c("2001-2005", "2006-2010"),
                                 triangle = c("Lower", "Upper"))))
    set.seed(1)
    component <- EntriesMovements(entries = entries,
                                  template = template,
                                  name = "immigration")
    iterator <- CohortIterator(component)
    iterator.R <- resetCC(iterator, i = 19L)
    iterator.C <- resetCC(iterator, i = 19L)
    while (!iterator.R@finished) {
        iterator.R <- advanceCC(iterator.R, useC = FALSE)
        iterator.C <- advanceCC(iterator.C, useC = TRUE)
        expect_identical(iterator.R, iterator.C)
    }
    iterator.R <- resetCC(iterator, i = 21L)
    iterator.C <- resetCC(iterator, i = 21L)
    iterator.R <- advanceCC(iterator.R, useC = FALSE)
    iterator.C <- advanceCC(iterator.C, useC = TRUE)
    expect_identical(iterator.R, iterator.C)
    iterator.R <- resetCC(iterator, i = 3L)
    iterator.C <- resetCC(iterator, i = 3L)
    while (!iterator.R@finished) {
        iterator.R <- advanceCC(iterator.R, useC = FALSE)
        iterator.C <- advanceCC(iterator.C, useC = TRUE)
        expect_identical(iterator.R, iterator.C)
    }
    ## with age - last age group closed
    entries <- Counts(array(1:36,
                            dim = c(3, 3, 2, 2),
                            dimnames = list(age = c("0-4", "5-9", "10-14"),
                                region = 1:3,
                                time = c("2001-2005", "2006-2010"),
                                triangle = c("Lower", "Upper"))))
    template <- Counts(array(0L,
                             dim = c(3, 3, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10-14"),
                                 region = 1:3,
                                 time = c("2001-2005", "2006-2010"),
                                 triangle = c("Lower", "Upper"))))
    set.seed(1)
    component <- EntriesMovements(entries = entries,
                                  template = template,
                                  name = "immigration")
    iterator <- CohortIterator(component)
    iterator.R <- resetCC(iterator, i = 19L)
    iterator.C <- resetCC(iterator, i = 19L)
    while (!iterator.R@finished) {
        iterator.R <- advanceCC(iterator.R, useC = FALSE)
        iterator.C <- advanceCC(iterator.C, useC = TRUE)
        expect_identical(iterator.R, iterator.C)
    }
    iterator.R <- resetCC(iterator, i = 21L)
    iterator.C <- resetCC(iterator, i = 21L)
    iterator.R <- advanceCC(iterator.R, useC = FALSE)
    iterator.C <- advanceCC(iterator.C, useC = TRUE)
    expect_identical(iterator.R, iterator.C)
    iterator.R <- resetCC(iterator, i = 3L)
    iterator.C <- resetCC(iterator, i = 3L)
    while (!iterator.R@finished) {
        iterator.R <- advanceCC(iterator.R, useC = FALSE)
        iterator.C <- advanceCC(iterator.C, useC = TRUE)
        expect_identical(iterator.R, iterator.C)
    }
    ## without age
    entries <- Counts(array(1:12,
                            dim = c(3, 4),
                            dimnames = list(region = 1:3,
                                time = c("2001-2005", "2006-2010", "2011-2015", "2016-2020"))))
    template <- Counts(array(0L,
                             dim = c(3, 4),
                             dimnames = list(region = 1:3,
                                 time = c("2001-2005", "2006-2010", "2011-2015", "2016-2020"))))
    set.seed(1)
    component <- EntriesMovements(entries = entries,
                                  template = template,
                                  name = "immigration")
    iterator <- CohortIterator(component)
    iterator.R <- resetCC(iterator, i = 5L)
    iterator.C <- resetCC(iterator, i = 5L)
    while (!iterator.R@finished) {
        iterator.R <- advanceCC(iterator.R, useC = FALSE)
        iterator.C <- advanceCC(iterator.C, useC = TRUE)
        expect_identical(iterator.R, iterator.C)
    }
})

test_that("resetCA gives valid answer", {
    resetCA <- demest:::resetCA
    ## dim = 3:4, iAge = 2, iTime = 1
    x <- new("CohortIteratorAccession",
             i = 4L,
             nTime = 3L,
             stepTime = 1L,
             iTime = 1L,
             hasAge = TRUE,
             nAge = 4L,
             stepAge = 3L,
             iAge = 1L,
             finished = FALSE)
    ans.obtained <- resetCA(x, i = 1L)
    ans.expected <- new("CohortIteratorAccession",
                        i = 1L,
                        nTime = 3L,
                        stepTime = 1L,
                        iTime = 1L,
                        hasAge = TRUE,
                        nAge = 4L,
                        stepAge = 3L,
                        iAge = 1L,
                        finished = FALSE)
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- resetCA(x, i = 12L)
    ans.expected <- new("CohortIteratorAccession",
                        i = 12L,
                        nTime = 3L,
                        stepTime = 1L,
                        iTime = 3L,
                        hasAge = TRUE,
                        nAge = 4L,
                        stepAge = 3L,
                        iAge = 4L,
                        finished = TRUE)
    expect_identical(ans.obtained, ans.expected)
    ## dim = 3:4, iAge = 2L, iTime = 1L 
    x <- new("CohortIteratorAccession",
             i = 1L,
             nTime = 3L,
             stepTime = 1L,
             iTime = 1L,
             hasAge = TRUE,
             nAge = 4L,
             stepAge = 3L,
             iAge = 1L,
             finished = FALSE)
    ans.obtained <- resetCA(x, i = 7L)
    ans.expected <- new("CohortIteratorAccession",
                        i = 7L,
                        nTime = 3L,
                        stepTime = 1L,
                        iTime = 1L,
                        hasAge = TRUE,
                        nAge = 4L,
                        stepAge = 3L,
                        iAge = 3L,
                        finished = FALSE)
    expect_identical(ans.obtained, ans.expected)
    ## dim = 2:5, iTime = 4L, iAge = 3L
    x <- new("CohortIteratorAccession",
             i = 1L,
             nTime = 5L,
             stepTime = 24L,
             iTime = 1L,
             hasAge = TRUE,
             nAge = 4L,
             stepAge = 6L,
             iAge = 1L,
             finished = FALSE)
    ans.obtained <- resetCA(x, i = 3L)
    ans.expected <- new("CohortIteratorAccession",
                        i = 3L,
                        nTime = 5L,
                        stepTime = 24L,
                        iTime = 1L,
                        hasAge = TRUE,
                        nAge = 4L,
                        stepAge = 6L,
                        iAge = 1L,
                        finished = FALSE)
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- resetCA(x, i = 39L)
    ans.expected <- new("CohortIteratorAccession",
                        i = 39L,
                        nTime = 5L,
                        stepTime = 24L,
                        iTime = 2L,
                        hasAge = TRUE,
                        nAge = 4L,
                        stepAge = 6L,
                        iAge = 3L,
                        finished = FALSE)
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of resetCA give same answer", {
    resetCA <- demest:::resetCA
    ## dim = 3:4, iAge = 2, iTime = 1
    x <- new("CohortIteratorAccession",
             i = 4L,
             nTime = 3L,
             stepTime = 1L,
             iTime = 1L,
             hasAge = TRUE,
             nAge = 4L,
             stepAge = 3L,
             iAge = 1L,
             finished = FALSE)
    ans.R <- resetCA(x, i = 1L, useC = FALSE)
    ans.C <- resetCA(x, i = 1L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ans.R <- resetCA(x, i = 12L, useC = FALSE)
    ans.C <- resetCA(x, i = 12L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## dim = 3:4, iAge = 2L, iTime = 1L 
    x <- new("CohortIteratorAccession",
             i = 1L,
             nTime = 3L,
             stepTime = 1L,
             iTime = 1L,
             hasAge = TRUE,
             nAge = 4L,
             stepAge = 3L,
             iAge = 1L,
             finished = FALSE)
    ans.R <- resetCA(x, i = 7L, useC = FALSE)
    ans.C <- resetCA(x, i = 7L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## dim = 2:5, iTime = 4L, iAge = 3L
    x <- new("CohortIteratorAccession",
             i = 1L,
             nTime = 5L,
             stepTime = 24L,
             iTime = 1L,
             hasAge = TRUE,
             nAge = 4L,
             stepAge = 6L,
             iAge = 1L,
             finished = FALSE)
    ans.R <- resetCA(x, i = 3L, useC = FALSE)
    ans.C <- resetCA(x, i = 3L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ans.expected <- new("CohortIteratorAccession",
                        i = 39L,
                        nTime = 5L,
                        stepTime = 24L,
                        iTime = 2L,
                        hasAge = TRUE,
                        nAge = 4L,
                        stepAge = 6L,
                        iAge = 3L,
                        finished = FALSE)
    ans.R <- resetCA(x, i = 39L, useC = FALSE)
    ans.C <- resetCA(x, i = 39L, useC = TRUE)
    expect_identical(ans.R, ans.C)
})

test_that("resetCP gives valid answer", {
    resetCP <- demest:::resetCP
    ## dim = 3:4, iAge = 2, iTime = 1
    x <- new("CohortIteratorPopulation",
             i = 4L,
             nTime = 3L,
             stepTime = 1L,
             iTime = 1L,
             hasAge = TRUE,
             nAge = 4L,
             stepAge = 3L,
             iAge = 1L,
             finished = FALSE)
    ans.obtained <- resetCP(x, i = 1L)
    ans.expected <- new("CohortIteratorPopulation",
                        i = 1L,
                        nTime = 3L,
                        stepTime = 1L,
                        iTime = 1L,
                        hasAge = TRUE,
                        nAge = 4L,
                        stepAge = 3L,
                        iAge = 1L,
                        finished = FALSE)
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- resetCP(x, i = 12L)
    ans.expected <- new("CohortIteratorPopulation",
                        i = 12L,
                        nTime = 3L,
                        stepTime = 1L,
                        iTime = 3L,
                        hasAge = TRUE,
                        nAge = 4L,
                        stepAge = 3L,
                        iAge = 4L,
                        finished = TRUE)
    expect_identical(ans.obtained, ans.expected)
    ## dim = 3:4, iAge = 2L, iTime = 1L 
    x <- new("CohortIteratorPopulation",
             i = 1L,
             nTime = 3L,
             stepTime = 1L,
             iTime = 1L,
             hasAge = TRUE,
             nAge = 4L,
             stepAge = 3L,
             iAge = 1L,
             finished = FALSE)
    ans.obtained <- resetCP(x, i = 7L)
    ans.expected <- new("CohortIteratorPopulation",
                        i = 7L,
                        nTime = 3L,
                        stepTime = 1L,
                        iTime = 1L,
                        hasAge = TRUE,
                        nAge = 4L,
                        stepAge = 3L,
                        iAge = 3L,
                        finished = FALSE)
    expect_identical(ans.obtained, ans.expected)
    ## dim = 3:4, iAge = 0L, iTime = 2L 
    x <- new("CohortIteratorPopulation",
             i = 10L,
             nTime = 4L,
             stepTime = 3L,
             iTime = 4L,
             hasAge = FALSE,
             nAge = as.integer(NA),
             stepAge = as.integer(NA),
             iAge = as.integer(NA),
             finished = TRUE)
    ans.obtained <- resetCP(x, i = 8L)
    ans.expected <- new("CohortIteratorPopulation",
                        i = 8L,
                        nTime = 4L,
                        stepTime = 3L,
                        iTime = 3L,
                        hasAge = FALSE,
                        nAge = as.integer(NA),
                        stepAge = as.integer(NA),
                        iAge = as.integer(NA),
                        finished = FALSE)
    expect_identical(ans.obtained, ans.expected)
    ## dim = 2:5, iTime = 4L, iAge = 3L
    x <- new("CohortIteratorPopulation",
             i = 1L,
             nTime = 5L,
             stepTime = 24L,
             iTime = 1L,
             hasAge = TRUE,
             nAge = 4L,
             stepAge = 6L,
             iAge = 1L,
             finished = FALSE)
    ans.obtained <- resetCP(x, i = 3L)
    ans.expected <- new("CohortIteratorPopulation",
                        i = 3L,
                        nTime = 5L,
                        stepTime = 24L,
                        iTime = 1L,
                        hasAge = TRUE,
                        nAge = 4L,
                        stepAge = 6L,
                        iAge = 1L,
                        finished = FALSE)
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- resetCP(x, i = 39L)
    ans.expected <- new("CohortIteratorPopulation",
                        i = 39L,
                        nTime = 5L,
                        stepTime = 24L,
                        iTime = 2L,
                        hasAge = TRUE,
                        nAge = 4L,
                        stepAge = 6L,
                        iAge = 3L,
                        finished = FALSE)
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of resetCP give same answer", {
    resetCP <- demest:::resetCP
    ## dim = 3:4, iAge = 2, iTime = 1
    x <- new("CohortIteratorPopulation",
             i = 4L,
             nTime = 3L,
             stepTime = 1L,
             iTime = 1L,
             hasAge = TRUE,
             nAge = 4L,
             stepAge = 3L,
             iAge = 1L,
             finished = FALSE)
    ans.R <- resetCP(x, i = 1L, useC = FALSE)
    ans.C <- resetCP(x, i = 1L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ans.R <- resetCP(x, i = 12L, useC = FALSE)
    ans.C <- resetCP(x, i = 12L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## dim = 3:4, iAge = 2L, iTime = 1L 
    x <- new("CohortIteratorPopulation",
             i = 1L,
             nTime = 3L,
             stepTime = 1L,
             iTime = 1L,
             hasAge = TRUE,
             nAge = 4L,
             stepAge = 3L,
             iAge = 1L,
             finished = FALSE)
    ans.R <- resetCP(x, i = 7L, useC = FALSE)
    ans.C <- resetCP(x, i = 7L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## dim = 3:4, iAge = 0L, iTime = 2L 
    x <- new("CohortIteratorPopulation",
             i = 10L,
             nTime = 4L,
             stepTime = 3L,
             iTime = 4L,
             hasAge = FALSE,
             nAge = as.integer(NA),
             stepAge = as.integer(NA),
             iAge = as.integer(NA),
             finished = TRUE)
    ans.R <- resetCP(x, i = 8L, useC = FALSE)
    ans.C <- resetCP(x, i = 8L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## dim = 2:5, iTime = 4L, iAge = 3L
    x <- new("CohortIteratorPopulation",
             i = 1L,
             nTime = 5L,
             stepTime = 24L,
             iTime = 1L,
             hasAge = TRUE,
             nAge = 4L,
             stepAge = 6L,
             iAge = 1L,
             finished = FALSE)
    ans.R <- resetCP(x, i = 3L, useC = FALSE)
    ans.C <- resetCP(x, i = 3L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ans.expected <- new("CohortIteratorPopulation",
                        i = 39L,
                        nTime = 5L,
                        stepTime = 24L,
                        iTime = 2L,
                        hasAge = TRUE,
                        nAge = 4L,
                        stepAge = 6L,
                        iAge = 3L,
                        finished = FALSE)
    ans.R <- resetCP(x, i = 39L, useC = FALSE)
    ans.C <- resetCP(x, i = 39L, useC = TRUE)
    expect_identical(ans.R, ans.C)
})


test_that("resetCC works", {
    resetCC <- demest:::resetCC
    CohortIterator <- demest:::CohortIterator
    EntriesMovements <- dembase:::EntriesMovements
    ## with age - last age group open
    entries <- Counts(array(1:36,
                            dim = c(3, 3, 2, 2),
                            dimnames = list(age = c("0-4", "5-9", "10+"),
                                region = 1:3,
                                time = c("2001-2005", "2006-2010"),
                                triangle = c("Lower", "Upper"))))
    template <- Counts(array(0L,
                             dim = c(3, 3, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 region = 1:3,
                                 time = c("2001-2005", "2006-2010"),
                                 triangle = c("Lower", "Upper"))))
    component <- EntriesMovements(entries = entries,
                                  template = template,
                                  name = "immigration")
    iterator <- CohortIterator(component)
    ## i = 2
    iterator <- resetCC(iterator, i = 2L)
    expect_identical(iterator@iTime, 1L)
    expect_identical(iterator@iAge, 2L)
    expect_identical(iterator@iTriangle, 1L)
    expect_false(iterator@finished)
    ## i = 18
    iterator <- resetCC(iterator, i = 18L)
    expect_identical(iterator@iTime, 2L)
    expect_identical(iterator@iAge, 3L)
    expect_identical(iterator@iTriangle, 1L)
    expect_true(iterator@finished)
    ## i = 30L
    iterator <- resetCC(iterator, i = 30L)
    expect_identical(iterator@iTime, 2L)
    expect_identical(iterator@iAge, 3L)
    expect_identical(iterator@iTriangle, 2L)
    expect_false(iterator@finished)
    ## with age - last age group closed
    entries <- Counts(array(1:36,
                            dim = c(3, 3, 2, 2),
                            dimnames = list(age = c("0-4", "5-9", "10-14"),
                                region = 1:3,
                                time = c("2001-2005", "2006-2010"),
                                triangle = c("Lower", "Upper"))))
    template <- Counts(array(0L,
                             dim = c(3, 3, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10-14"),
                                 region = 1:3,
                                 time = c("2001-2005", "2006-2010"),
                                 triangle = c("Lower", "Upper"))))
    component <- EntriesMovements(entries = entries,
                                  template = template,
                                  name = "immigration")
    iterator <- CohortIterator(component)
    ## i = 2
    iterator <- resetCC(iterator, i = 2L)
    expect_identical(iterator@iTime, 1L)
    expect_identical(iterator@iAge, 2L)
    expect_identical(iterator@iTriangle, 1L)
    expect_false(iterator@finished)
    ## i = 18
    iterator <- resetCC(iterator, i = 18L)
    expect_identical(iterator@iTime, 2L)
    expect_identical(iterator@iAge, 3L)
    expect_identical(iterator@iTriangle, 1L)
    expect_true(iterator@finished)
    ## i = 30L
    iterator <- resetCC(iterator, i = 30L)
    expect_identical(iterator@iTime, 2L)
    expect_identical(iterator@iAge, 3L)
    expect_identical(iterator@iTriangle, 2L)
    expect_true(iterator@finished)
    ## without age
    entries <- Counts(array(1:12,
                            dim = c(2, 3, 2),
                            dimnames = list(sex = c("f", "m"),
                                region = 1:3,
                                time = c("2001-2005", "2006-2010"))))
    template <- Counts(array(0L,
                             dim = c(2, 3, 2),
                             dimnames = list(sex = c("f", "m"),
                                region = 1:3,
                                time = c("2001-2005", "2006-2010"))))
    component <- EntriesMovements(entries = entries,
                                  template = template,
                                  name = "immigration")
    iterator <- CohortIterator(component)
    ## i = 5L
    iterator <- resetCC(iterator, i = 5L)
    expect_identical(iterator@iTime, 1L)
    expect_identical(iterator@iAge, NA_integer_)
    expect_identical(iterator@iTriangle, NA_integer_)
    expect_false(iterator@finished)
    ## i = 8L
    iterator <- resetCC(iterator, i = 8L)
    expect_identical(iterator@iTime, 2L)
    expect_identical(iterator@iAge, NA_integer_)
    expect_identical(iterator@iTriangle, NA_integer_)
    expect_true(iterator@finished)
})

test_that("R and C versions of resetCC give same answer", {
    resetCC <- demest:::resetCC
    CohortIterator <- demest:::CohortIterator
    EntriesMovements <- dembase:::EntriesMovements
    ## with age
    entries <- Counts(array(1:36,
                            dim = c(3, 3, 2, 2),
                            dimnames = list(age = c("0-4", "5-9", "10+"),
                                region = 1:3,
                                time = c("2001-2005", "2006-2010"),
                                triangle = c("Lower", "Upper"))))
    template <- Counts(array(0L,
                             dim = c(3, 3, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 region = 1:3,
                                 time = c("2001-2005", "2006-2010"),
                                 triangle = c("Lower", "Upper"))))
    component <- EntriesMovements(entries = entries,
                                  template = template,
                                  name = "immigration")
    iterator <- CohortIterator(component)
    ## i = 2
    ans.R <- resetCC(iterator, i = 2L, useC = FALSE)
    ans.C <- resetCC(iterator, i = 2L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## i = 18
    ans.R <- resetCC(iterator, i = 18L, useC = FALSE)
    ans.C <- resetCC(iterator, i = 18L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## i = 30L
    ans.R <- resetCC(iterator, i = 30L, useC = FALSE)
    ans.C <- resetCC(iterator, i = 30L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## without age
    entries <- Counts(array(1:12,
                            dim = c(2, 3, 2),
                            dimnames = list(sex = c("f", "m"),
                                region = 1:3,
                                time = c("2001-2005", "2006-2010"))))
    template <- Counts(array(0L,
                             dim = c(2, 3, 2),
                             dimnames = list(sex = c("f", "m"),
                                region = 1:3,
                                time = c("2001-2005", "2006-2010"))))
    component <- EntriesMovements(entries = entries,
                                  template = template,
                                  name = "immigration")
    iterator <- CohortIterator(component)
    ## i = 5L
    ans.R <- resetCC(iterator, i = 5L, useC = FALSE)
    ans.C <- resetCC(iterator, i = 5L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## i = 8L
    ans.R <- resetCC(iterator, i = 8L, useC = FALSE)
    ans.C <- resetCC(iterator, i = 8L, useC = TRUE)
    expect_identical(ans.R, ans.C)
})


## CODPCP iterator

## updating of age, time, etc based on advanceCC,
## so only check that tracking orig-dest correctly
test_that("advanceCODPCP works", {
    advanceCODPCP <- demest:::advanceCODPCP
    resetCODPCP <- demest:::resetCODPCP
    CohortIterator <- demest:::CohortIterator
    InternalMovements <- dembase:::InternalMovements
    internal <- Counts(array(1:108,
                             dim = c(3, 3, 3, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 reg_orig = 1:3,
                                 reg_dest = 1:3,
                                 time = c("2001-2005", "2006-2010"),
                                 triangle = c("Lower", "Upper"))))
    template <- Counts(array(0L,
                             dim = c(3, 3, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 reg = 1:3,
                                 time = c("2001-2005", "2006-2010"),
                                 triangle = c("Lower", "Upper"))))
    internal <- InternalMovements(internal = internal,
                                  template = template)
    iterator <- CohortIterator(internal)
    internal.df <- as.data.frame(internal, direction = "long")
    ## i = 2
    iterator <- resetCODPCP(object = iterator, i = 8L)
    expect_true(all(internal.df[iterator@iVec, "reg_orig"] == 3))
    expect_true(all(internal.df[iterator@iVec, "reg_dest"] == 1:3))
    iterator <- advanceCODPCP(iterator)
    expect_true(all(internal.df[iterator@iVec, "reg_orig"] == 3))
    expect_true(all(internal.df[iterator@iVec, "reg_dest"] == 1:3))
    ## i = 85L
    iterator <- resetCODPCP(iterator, i = 85L)
    expect_true(all(internal.df[iterator@iVec, "reg_orig"] == 2))
    expect_true(all(internal.df[iterator@iVec, "reg_dest"] == 1:3))
    iterator <- advanceCODPCP(iterator)
    expect_true(all(internal.df[iterator@iVec, "reg_orig"] == 2))
    expect_true(all(internal.df[iterator@iVec, "reg_dest"] == 1:3))
    ## i = 57L
    iterator <- resetCODPCP(iterator, i = 57L)
    expect_true(all(internal.df[iterator@iVec, "reg_orig"] == 1))
    expect_true(all(internal.df[iterator@iVec, "reg_dest"] == 1:3))
    iterator <- advanceCODPCP(iterator)
    expect_true(all(internal.df[iterator@iVec, "reg_orig"] == 1))
    expect_true(all(internal.df[iterator@iVec, "reg_dest"] == 1:3))
})

test_that("R and C versions of advanceCODPCP give same answer", {
    advanceCODPCP <- demest:::advanceCODPCP
    resetCODPCP <- demest:::resetCODPCP
    CohortIterator <- demest:::CohortIterator
    InternalMovements <- dembase:::InternalMovements
    internal <- Counts(array(1:108,
                             dim = c(3, 3, 3, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 reg_orig = 1:3,
                                 reg_dest = 1:3,
                                 time = c("2001-2005", "2006-2010"),
                                 triangle = c("Lower", "Upper"))))
    template <- Counts(array(0L,
                             dim = c(3, 3, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 reg = 1:3,
                                 time = c("2001-2005", "2006-2010"),
                                 triangle = c("Lower", "Upper"))))
    internal <- InternalMovements(internal = internal,
                                  template = template)
    iterator <- CohortIterator(internal)
    ## i = 2
    iterator <- resetCODPCP(object = iterator, i = 8L)
    iterator.R <- iterator
    iterator.C <- iterator
    while (!iterator.R@finished) {
        iterator.R <- advanceCODPCP(iterator.R, useC = FALSE)
        iterator.C <- advanceCODPCP(iterator.C, useC = TRUE)
        expect_identical(iterator.R, iterator.C)
    }
    ## i = 85L
    iterator <- resetCODPCP(iterator, i = 85L)
    iterator.R <- iterator
    iterator.C <- iterator
    while (!iterator.R@finished) {
        iterator.R <- advanceCODPCP(iterator.R, useC = FALSE)
        iterator.C <- advanceCODPCP(iterator.C, useC = TRUE)
        expect_identical(iterator.R, iterator.C)
    }
    ## i = 57L
    iterator <- resetCODPCP(iterator, i = 57L)
    iterator.R <- iterator
    iterator.C <- iterator
    while (!iterator.R@finished) {
        iterator.R <- advanceCODPCP(iterator.R, useC = FALSE)
        iterator.C <- advanceCODPCP(iterator.C, useC = TRUE)
        expect_identical(iterator.R, iterator.C)
    }
})

test_that("resetCODPCP works", {
    resetCODPCP <- demest:::resetCODPCP
    CohortIterator <- demest:::CohortIterator
    InternalMovements <- dembase:::InternalMovements
    ## with age
    internal <- Counts(array(1:108,
                             dim = c(3, 3, 3, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 reg_orig = 1:3,
                                 reg_dest = 1:3,
                                 time = c("2001-2005", "2006-2010"),
                                 triangle = c("Lower", "Upper"))))
    template <- Counts(array(0L,
                             dim = c(3, 3, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 reg = 1:3,
                                 time = c("2001-2005", "2006-2010"),
                                 triangle = c("Lower", "Upper"))))
    internal <- InternalMovements(internal = internal,
                                  template = template)
    iterator <- CohortIterator(internal)
    ## i = 2
    iterator <- resetCODPCP(object = iterator, i = 8L)
    expect_identical(iterator@iTime, 1L)
    expect_identical(iterator@iAge, 2L)
    expect_identical(iterator@iTriangle, 1L)
    expect_identical(iterator@iVec, c(8L, 17L, 26L))
    expect_false(iterator@finished)
    ## i = 85L
    iterator <- resetCODPCP(iterator, i = 85L)
    expect_identical(iterator@iTime, 2L)
    expect_identical(iterator@iAge, 1L)
    expect_identical(iterator@iTriangle, 2L)
    expect_identical(iterator@iVec, c(85L, 94L, 103L))
    expect_false(iterator@finished)
    ## i = 57L
    iterator <- resetCODPCP(iterator, i = 57L)
    expect_identical(iterator@iTime, 1L)
    expect_identical(iterator@iAge, 3L)
    expect_identical(iterator@iTriangle, 2L)
    expect_identical(iterator@iVec, c(57L, 66L, 75L))
    expect_false(iterator@finished)
    ## i = 108L
    iterator <- resetCODPCP(iterator, i = 108L)
    expect_identical(iterator@iTime, 2L)
    expect_identical(iterator@iAge, 3L)
    expect_identical(iterator@iTriangle, 2L)
    expect_false(iterator@finished)
    ## without age
    internal <- Counts(array(1:72,
                             dim = c(3, 3, 2, 2, 2),
                             dimnames = list(reg_orig = 1:3,
                                 reg_dest = 1:3,
                                 eth_orig = 1:2,
                                 eth_dest = 1:2,
                                 time = c("2001-2005", "2006-2010"))))
    template <- Counts(array(0L,
                             dim = c(3, 2, 2),
                             dimnames = list(reg = 1:3,
                                 eth = 1:2,
                                 time = c("2001-2005", "2006-2010"))))
    internal <- InternalMovements(internal = internal,
                                  template = template)
    iterator <- CohortIterator(internal)
    ## i = 10L
    iterator <- resetCODPCP(iterator, i = 10L)
    expect_identical(iterator@iTime, 1L)
    expect_identical(iterator@iAge, NA_integer_)
    expect_identical(iterator@iTriangle, NA_integer_)
    expect_identical(iterator@iVec, c(10L, 13L, 16L, 28L, 31L, 34L))
    expect_false(iterator@finished)
    ## i = 48L
    iterator <- resetCODPCP(iterator, i = 48L)
    expect_identical(iterator@iTime, 2L)
    expect_identical(iterator@iAge, NA_integer_)
    expect_identical(iterator@iTriangle, NA_integer_)
    expect_identical(iterator@iVec, c(48L, 51L, 54L, 66L, 69L, 72L))
    expect_true(iterator@finished)
})

test_that("R and C versions of resetCODPCP give same answer", {
    resetCODPCP <- demest:::resetCODPCP
    CohortIterator <- demest:::CohortIterator
    InternalMovements <- dembase:::InternalMovements
    ## with age
    internal <- Counts(array(1:108,
                             dim = c(3, 3, 3, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 reg_orig = 1:3,
                                 reg_dest = 1:3,
                                 time = c("2001-2005", "2006-2010"),
                                 triangle = c("Lower", "Upper"))))
    template <- Counts(array(0L,
                             dim = c(3, 3, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 reg = 1:3,
                                 time = c("2001-2005", "2006-2010"),
                                 triangle = c("Lower", "Upper"))))
    internal <- InternalMovements(internal = internal,
                                  template = template)
    iterator <- CohortIterator(internal)
    ## i = 2
    ans.R <- resetCODPCP(iterator, i = 2L, useC = FALSE)
    ans.C <- resetCODPCP(iterator, i = 2L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## i = 85L
    ans.R <- resetCODPCP(iterator, i = 85L, useC = FALSE)
    ans.C <- resetCODPCP(iterator, i = 85L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## i = 57L
    ans.R <- resetCODPCP(iterator, i = 57L, useC = FALSE)
    ans.C <- resetCODPCP(iterator, i = 57L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## without age
    internal <- Counts(array(1:72,
                             dim = c(3, 3, 2, 2, 2),
                             dimnames = list(reg_orig = 1:3,
                                 reg_dest = 1:3,
                                 eth_orig = 1:2,
                                 eth_dest = 1:2,
                                 time = c("2001-2005", "2006-2010"))))
    template <- Counts(array(0L,
                             dim = c(3, 2, 2),
                             dimnames = list(reg = 1:3,
                                 eth = 1:2,
                                 time = c("2001-2005", "2006-2010"))))
    internal <- InternalMovements(internal = internal,
                                  template = template)
    iterator <- CohortIterator(internal)
    ## i = 10L
    ans.R <- resetCODPCP(iterator, i = 10L, useC = FALSE)
    ans.C <- resetCODPCP(iterator, i = 10L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## i = 48L
    ans.R <- resetCODPCP(iterator, i = 48L, useC = FALSE)
    ans.C <- resetCODPCP(iterator, i = 48L, useC = TRUE)
    expect_identical(ans.R, ans.C)
})



## DimIterator

test_that("advanceD gives valid answer", {
    advanceD <- demest:::advanceD
    DimIterator <- demest:::DimIterator
    ## i is first index
    x <- DimIterator(dim = 3:2, i = 1L)
    expect_identical(x@nStrides, -2L)
    for (i in 1:10) {
        for (j in 1:2) {
            x <- advanceD(x)
            expect_identical(x@nStrides, 1L)
        }
        x <- advanceD(x)
        expect_identical(x@nStrides, -2L)
    }
    ## i is last index
    x <- DimIterator(dim = 3:2, i = 2L)
    expect_identical(x@nStrides, -1L)
    for (i in 1:10) {
        for (j in 1:2) {
            x <- advanceD(x)
            expect_identical(x@nStrides, 0L)
        }
        x <- advanceD(x)
        expect_identical(x@nStrides, 1L)
        for (j in 1:2) {
            x <- advanceD(x)
            expect_identical(x@nStrides, 0L)
        }
        x <- advanceD(x)
        expect_identical(x@nStrides, -1L)
    }
    ## i is middle index
    x <- DimIterator(dim = c(3L, 2L, 2L), i = 2L)
    expect_identical(x@nStrides, -1L)
    for (i in 1:10) {
        for (j in 1:2) {
            x <- advanceD(x)
            expect_identical(x@nStrides, 0L)
        }
        x <- advanceD(x)
        expect_identical(x@nStrides, 1L)
        for (j in 1:2) {
            x <- advanceD(x)
            expect_identical(x@nStrides, 0L)
        }
        x <- advanceD(x)
        expect_identical(x@nStrides, -1L)
    }
    ## iterator should return to initial state after completely traversing object
    x <- DimIterator(dim = 3:2, i = 1L)
    x0 <- x
    for (i in 1:6)
        x <- advanceD(x)
    expect_identical(x, x0)
})

test_that("R and C versions of advanceD give same answer", {
    advanceD <- demest:::advanceD
    DimIterator <- demest:::DimIterator
    ## i is first index
    x.R <- DimIterator(dim = 3:2, i = 1L)
    x.C <- x.R
    for (i in 1:20) {
        x.R <- advanceD(x.R, useC = FALSE)
        x.C <- advanceD(x.C, useC = TRUE)
        expect_identical(x.R, x.C)
    }
    ## i is last index
    x.R <- DimIterator(dim = 3:2, i = 2L)
    x.C <- x.R
    for (i in 1:20) {
        x.R <- advanceD(x.R, useC = FALSE)
        x.C <- advanceD(x.C, useC = TRUE)
        expect_identical(x.R, x.C)
    }
    ## i is middle index
    x.R <- DimIterator(dim = c(3L, 2L, 2L), i = 2L)
    x.C <- x.R
    for (i in 1:20) {
        x.R <- advanceD(x.R, useC = FALSE)
        x.C <- advanceD(x.C, useC = TRUE)
        expect_identical(x.R, x.C)
    }
    ## both return to initial state after completely traversing object
    x0 <- DimIterator(dim = 3:2, i = 1L)
    x.R <- x0
    x.C <- x0
    for (i in 1:6) {
        x.R <- advanceD(x.R)
        x.C <- advanceD(x.C)
    }
    expect_identical(x.R, x0)
    expect_identical(x.C, x0)
})

test_that("resetD gives valid answer", {
    resetD <- demest:::resetD
    DimIterator <- demest:::DimIterator
    advanceD <- demest:::advanceD
    ## i is first index
    x <- DimIterator(dim = 3:2, i = 1L)
    x0 <- x
    for (i in 1:20)
        x <- advanceD(x)
    x <- resetD(x)
    expect_identical(x, x0)
    ## i is last index
    x <- DimIterator(dim = 3:2, i = 2L)
    x0 <- x
    for (i in 1:20)
        x <- advanceD(x)
    x <- resetD(x)
    expect_identical(x, x0)
    ## i is middle index
    x <- DimIterator(dim = c(3L, 2L, 2L), i = 2L)
    x0 <- x
    for (i in 1:20)
        x <- advanceD(x)
    x <- resetD(x)
    expect_identical(x, x0)
})

test_that("R and C versions of resetD gives same answer", {
    resetD <- demest:::resetD
    DimIterator <- demest:::DimIterator
    advanceD <- demest:::advanceD
    ## i is first index
    x <- DimIterator(dim = 3:2, i = 1L)
    for (i in 1:20)
        x <- advanceD(x)
    x.R <- resetD(x, useC = FALSE)
    x.C <- resetD(x, useC = TRUE)
    expect_identical(x.R, x.C)
    ## i is last index
    x <- DimIterator(dim = 3:2, i = 2L)
    for (i in 1:20)
        x <- advanceD(x)
    x.R <- resetD(x, useC = FALSE)
    x.C <- resetD(x, useC = TRUE)
    expect_identical(x.R, x.C)
    ## i is middle index
    x <- DimIterator(dim = c(3L, 2L, 2L), i = 2L)
    for (i in 1:20)
        x <- advanceD(x)
    x.R <- resetD(x, useC = FALSE)
    x.C <- resetD(x, useC = TRUE)
    expect_identical(x.R, x.C)
})

test_that("advanceM gives valid answer", {
    advanceM <- demest:::advanceM
    MarginIterator <- demest:::MarginIterator
    ## two dimensions
    x <- MarginIterator(dim = 3:4)
    expect_identical(x@indices, c(1L, 1L))
    x <- advanceM(x)
    expect_identical(x@indices, c(2L, 1L))
    x <- advanceM(x)
    expect_identical(x@indices, c(3L, 1L))
    x <- advanceM(x)
    expect_identical(x@indices, c(1L, 2L))
    x <- advanceM(x)
    expect_identical(x@indices, c(2L, 2L))
    x <- advanceM(x)
    expect_identical(x@indices, c(3L, 2L))
    x <- advanceM(x)
    expect_identical(x@indices, c(1L, 3L))
    x <- advanceM(x)
    expect_identical(x@indices, c(2L, 3L))
    x <- advanceM(x)
    expect_identical(x@indices, c(3L, 3L))
    x <- advanceM(x)
    expect_identical(x@indices, c(1L, 4L))
    x <- advanceM(x)
    expect_identical(x@indices, c(2L, 4L))
    x <- advanceM(x)
    expect_identical(x@indices, c(3L, 4L))
    x <- advanceM(x)
    expect_identical(x@indices, c(1L, 1L))
    ## three dimensions
    x <- MarginIterator(dim = 2:4)
    expect_identical(x@indices, c(1L, 1L, 1L))
    x <- advanceM(x)
    expect_identical(x@indices, c(2L, 1L, 1L))
    x <- advanceM(x)
    expect_identical(x@indices, c(1L, 2L, 1L))
    x <- advanceM(x)
    expect_identical(x@indices, c(2L, 2L, 1L))
    x <- advanceM(x)
    expect_identical(x@indices, c(1L, 3L, 1L))
    x <- advanceM(x)
    expect_identical(x@indices, c(2L, 3L, 1L))
    x <- advanceM(x)
    expect_identical(x@indices, c(1L, 1L, 2L))
    x <- advanceM(x)
    expect_identical(x@indices, c(2L, 1L, 2L))
    x <- advanceM(x)
    expect_identical(x@indices, c(1L, 2L, 2L))
    x <- advanceM(x)
    expect_identical(x@indices, c(2L, 2L, 2L))
    x <- advanceM(x)
    expect_identical(x@indices, c(1L, 3L, 2L))
    x <- advanceM(x)
    expect_identical(x@indices, c(2L, 3L, 2L))
    x <- advanceM(x)
    expect_identical(x@indices, c(1L, 1L, 3L))
    x <- advanceM(x)
    expect_identical(x@indices, c(2L, 1L, 3L))
    x <- advanceM(x)
    expect_identical(x@indices, c(1L, 2L, 3L))
    x <- advanceM(x)
    expect_identical(x@indices, c(2L, 2L, 3L))
    x <- advanceM(x)
    expect_identical(x@indices, c(1L, 3L, 3L))
    x <- advanceM(x)
    expect_identical(x@indices, c(2L, 3L, 3L))
    x <- advanceM(x)
    expect_identical(x@indices, c(1L, 1L, 4L))
    x <- advanceM(x)
    expect_identical(x@indices, c(2L, 1L, 4L))
    x <- advanceM(x)
    expect_identical(x@indices, c(1L, 2L, 4L))
    x <- advanceM(x)
    expect_identical(x@indices, c(2L, 2L, 4L))
    x <- advanceM(x)
    expect_identical(x@indices, c(1L, 3L, 4L))
    x <- advanceM(x)
    expect_identical(x@indices, c(2L, 3L, 4L))
    x <- advanceM(x)
    expect_identical(x@indices, c(1L, 1L, 1L))
})

test_that("R and C versions of advanceM give same answer", {
    advanceM <- demest:::advanceM
    MarginIterator <- demest:::MarginIterator
    ## two dimensions
    x.R <- MarginIterator(dim = 3:4)
    x.C <- x.R
    for (i in 1:20) {
        x.R <- advanceM(x.R, useC = FALSE)
        x.C <- advanceM(x.C, useC = TRUE)
        expect_identical(x.R, x.C)
    }
    ## four dimensions
    x.R <- MarginIterator(dim = 5:2)
    x.C <- x.R
    for (i in 1:200) {
        x.R <- advanceM(x.R, useC = FALSE)
        x.C <- advanceM(x.C, useC = TRUE)
        expect_identical(x.R, x.C)
    }
})

test_that("resetM gives valid answer", {
    resetM <- demest:::resetM
    advanceM <- demest:::advanceM
    MarginIterator <- demest:::MarginIterator
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ## two dimensions
        x0 <- MarginIterator(dim = 3:4)
        x1 <- x0
        n <- rpois(n = 1L, lambda = 10) + 1L
        for (i in seq_len(n))
            x1 <- advanceM(x1)
        x1 <- resetM(x1)
        expect_identical(x1, x0)
        ## two dimensions
        x0 <- MarginIterator(dim = 4:2)
        x1 <- x0
        n <- rpois(n = 1L, lambda = 10) + 1L
        for (i in seq_len(n))
            x1 <- advanceM(x1)
        x1 <- resetM(x1)
        expect_identical(x1, x0)
    }
})

test_that("R and C versions of resetM give same answer", {
    resetM <- demest:::resetM
    advanceM <- demest:::advanceM
    MarginIterator <- demest:::MarginIterator
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ## two dimensions
        x <- MarginIterator(dim = 3:4)
        n <- rpois(n = 1L, lambda = 10) + 1L
        for (i in seq_len(n))
            x <- advanceM(x)
        x.R <- resetM(x, useC = FALSE)
        x.C <- resetM(x, useC = TRUE)
        expect_identical(x.R, x.C)
        ## four dimensions
        x <- MarginIterator(dim = 3:6)
        n <- rpois(n = 1L, lambda = 30) + 1L
        for (i in seq_len(n))
            x <- advanceM(x)
        x.R <- resetM(x, useC = FALSE)
        x.C <- resetM(x, useC = TRUE)
        expect_identical(x.R, x.C)
    }
})

test_that("advanceS gives valid answer", {
    advanceS <- demest:::advanceS
    SliceIterator <- demest:::SliceIterator
    ## three dimensions
    x <- SliceIterator(dim = c(3L, 2L, 2L),
                       iAlong = 1L)
    expect_identical(x@indices, c(1L, 4L, 7L, 10L))
    x <- advanceS(x)
    expect_identical(x@indices, c(2L, 5L, 8L, 11L))
    x <- advanceS(x)
    expect_identical(x@indices, c(3L, 6L, 9L, 12L))
    x <- advanceS(x)
    expect_identical(x@indices, c(1L, 4L, 7L, 10L))
    x <- SliceIterator(dim = c(3L, 2L, 2L),
                       iAlong = 2L)
    expect_identical(x@indices, c(1L, 2L, 3L, 7L, 8L, 9L))
    x <- advanceS(x)
    expect_identical(x@indices, c(4L, 5L, 6L, 10L, 11L, 12L))
    x <- advanceS(x)
    expect_identical(x@indices, c(1L, 2L, 3L, 7L, 8L, 9L))
    x <- SliceIterator(dim = c(3L, 2L, 2L),
                       iAlong = 3L)
    expect_identical(x@indices, c(1L, 2L, 3L, 4L, 5L, 6L))
    x <- advanceS(x)
    expect_identical(x@indices, c(7L, 8L, 9L, 10L, 11L, 12L))
    x <- advanceS(x)
    expect_identical(x@indices, c(1L, 2L, 3L, 4L, 5L, 6L))
    ## one dimension
    x <- SliceIterator(dim = 3L,
                       iAlong = 1L)
    expect_identical(x@indices, 1L)
    x <- advanceS(x)
    expect_identical(x@indices, 2L)
    x <- advanceS(x)
    expect_identical(x@indices, 3L)
    x <- advanceS(x)
    expect_identical(x@indices, 1L)
})

test_that("R and C versions ofadvanceS give same answer", {
    advanceS <- demest:::advanceS
    SliceIterator <- demest:::SliceIterator
    ## three dimensions
    x <- SliceIterator(dim = c(3L, 2L, 2L),
                       iAlong = 1L)
    x.R <- x
    x.C <- x
    for (i in 1:4) {
        x.R <- advanceS(x.R, useC = FALSE)
        x.C <- advanceS(x.C, useC = TRUE)
        expect_identical(x.R, x.C)
    }
    x <- SliceIterator(dim = c(3L, 2L, 2L),
                       iAlong = 2L)
    x.R <- x
    x.C <- x
    for (i in 1:4) {
        x.R <- advanceS(x.R, useC = FALSE)
        x.C <- advanceS(x.C, useC = TRUE)
        expect_identical(x.R, x.C)
    }
    x <- SliceIterator(dim = c(3L, 2L, 2L),
                       iAlong = 3L)
    x.R <- x
    x.C <- x
    for (i in 1:4) {
        x.R <- advanceS(x.R, useC = FALSE)
        x.C <- advanceS(x.C, useC = TRUE)
        expect_identical(x.R, x.C)
    }
    ## one dimension
    x <- SliceIterator(dim = 3L,
                       iAlong = 1L)
    x.R <- x
    x.C <- x
    for (i in 1:4) {
        x.R <- advanceS(x.R, useC = FALSE)
        x.C <- advanceS(x.C, useC = TRUE)
        expect_identical(x.R, x.C)
    }
})

test_that("resetS gives valid answer", {
    resetS <- demest:::resetS
    advanceS <- demest:::advanceS
    SliceIterator <- demest:::SliceIterator
    ## three dimensions
    x <- SliceIterator(dim = c(3L, 2L, 2L),
                       iAlong = 1L)
    x0 <- x
    x <- advanceS(x)
    x <- advanceS(x)
    x <- resetS(x)
    expect_identical(x, x0)
    x <- SliceIterator(dim = c(3L, 2L, 2L),
                       iAlong = 2L)
    x0 <- x
    x <- advanceS(x)
    x <- resetS(x)
    expect_identical(x, x0)
    x <- SliceIterator(dim = c(3L, 2L, 2L),
                       iAlong = 3L)
    x0 <- x
    x <- advanceS(x)
    x <- advanceS(x)
    x <- advanceS(x)
    x <- resetS(x)
    expect_identical(x, x0)
    ## one dimension
    x <- SliceIterator(dim = 3L,
                       iAlong = 1L)
    x0 <- x
    x <- advanceS(x)
    x <- advanceS(x)
    x <- resetS(x)
    expect_identical(x, x0)
})

test_that("R and C versions of resetS give same answer", {
    resetS <- demest:::resetS
    advanceS <- demest:::advanceS
    SliceIterator <- demest:::SliceIterator
    ## three dimensions
    x <- SliceIterator(dim = c(3L, 2L, 2L),
                       iAlong = 1L)
    x <- advanceS(x)
    x <- advanceS(x)
    x.R <- resetS(x, useC = FALSE)
    x.C <- resetS(x, useC = TRUE)
    expect_identical(x.R, x.C)
    x <- SliceIterator(dim = c(3L, 2L, 2L),
                       iAlong = 2L)
    x <- advanceS(x)
    x.R <- resetS(x, useC = FALSE)
    x.C <- resetS(x, useC = TRUE)
    expect_identical(x.R, x.C)
    x <- SliceIterator(dim = c(3L, 2L, 2L),
                       iAlong = 3L)
    x <- advanceS(x)
    x <- advanceS(x)
    x <- advanceS(x)
    x.R <- resetS(x, useC = FALSE)
    x.C <- resetS(x, useC = TRUE)
    expect_identical(x.R, x.C)
    ## one dimension
    x <- SliceIterator(dim = 3L,
                       iAlong = 1L)
    x <- advanceS(x)
    x <- advanceS(x)
    x.R <- resetS(x, useC = FALSE)
    x.C <- resetS(x, useC = TRUE)
    expect_identical(x.R, x.C)
})











