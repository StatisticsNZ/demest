
context("update-nongeneric")

n.test <- 5
test.identity <- FALSE
test.extended <- FALSE


## UPDATING STANDARD DEVIATION (VIA SLICE SAMPLING) #################################

if (test.extended) {
    test_that("updateSDNorm works with no upper limit", {
        updateSDNorm <- demest:::updateSDNorm
        sigma <- runif(n = 1, 0.0001, max = 10)
        A <- runif(n = 1, min = 0.1, max = 10)
        nu <- 1.0 * max(rpois(n = 1, lambda = 5), 1)
        V <- runif(n = 1, 0.01, 10)
        n <- rpois(n = 1, lambda = 10)
        f <- function(sigma) {
            sigma^(-n) * exp(-V/(2*sigma^2)) * (sigma^2 + nu*A^2)^(-(nu+1)/2)
        }
        n.sample <- 100000
        ans <- numeric(length = n.sample)
        for (i in 1:n.sample) {
            sigma <- updateSDNorm(sigma = sigma,
                                  A = A,
                                  nu = nu,
                                  V = V,
                                  n = n,
                                  max = Inf,
                                  useC = TRUE)
            ans[i] <- sigma
        }
        d <- density(ans)
        y <- d$y
        x <- d$x
        non.zero <- y > 0
        y <- y[non.zero]
        x <- x[non.zero]
        f.sigma <- f(x)
        f.sigma <- f.sigma * max(y) / max(f.sigma)
        expect_true(cor(f.sigma, y) > 0.99)
        if (FALSE) {
            plot(y ~ x, type = "l")
            lines(y = f.sigma, x = x, col = "red")
        }
    })
}

if (test.extended) {
    test_that("updateSDNorm works with upper limit", {
        updateSDNorm <- demest:::updateSDNorm
        sigma <- runif(n = 1, 0.0001, max = 10)
        A <- runif(n = 1, min = 0.1, max = 10)
        nu <- 1.0 * max(rpois(n = 1, lambda = 5), 1)
        V <- runif(n = 1, 0.01, 10)
        n <- rpois(n = 1, lambda = 10)
        f <- function(sigma) {
            sigma^(-n) * exp(-V/(2*sigma^2)) * (sigma^2 + nu*A^2)^(-(nu+1)/2)
        }
        numerator <- V - n*nu*A^2 + sqrt((V - n*nu*A^2)^2 + 4*(n + nu + 1)*V*nu*A^2)
        denominator <- 2*(n + nu + 1)
        sigma.star <- sqrt(numerator / denominator)
        max <- sigma.star * 1.5
        n.sample <- 100000
        ans <- numeric(length = n.sample)
        for (i in 1:n.sample) {
            sigma <- updateSDNorm(sigma = sigma,
                                  A = A,
                                  nu = nu,
                                  V = V,
                                  n = n,
                                  max = max,
                                  useC = TRUE)
            ans[i] <- sigma
        }
        d <- density(ans, to = 0.98 * max)
        y <- d$y
        x <- d$x
        non.zero <- y > 0
        y <- y[non.zero]
        x <- x[non.zero]
        f.sigma <- f(x)
        f.sigma <- f.sigma * max(y) / max(f.sigma)
        expect_true(cor(f.sigma, y) > 0.99)
        expect_true(all(ans < max))
        if (FALSE) {
            plot(y ~ x, type = "l")
            lines(y = f.sigma, x = x, col = "red")
        }
    })
}

test_that("R and C versions of updateSDNorm give same answer with no upper limit", {
    updateSDNorm <- demest:::updateSDNorm
    set.seed(1)
    sigma <- runif(n = 1, 0.0001, max = 10)
    A <- runif(n = 1, min = 0.1, max = 10)
    nu <- 1.0 * max(rpois(n = 1, lambda = 5), 1)
    V <- runif(n = 1, 0.01, 10)
    n <- as.integer(rpois(n = 1, lambda = 10))
    n.sample <- 100
    ans.R <- numeric(length = n.sample)
    ans.C <- numeric(length = n.sample)
    sigma.R <- sigma
    sigma.C <- sigma
    for (i in 1:n.sample) {
        set.seed(i)
        sigma.R <- updateSDNorm(sigma = sigma.R,
                                A = A,
                                nu = nu,
                                V = V,
                                n = n,
                                max = Inf,
                                useC = FALSE)
        ans.R[i] <- sigma.R
        set.seed(i)
        sigma.C <- updateSDNorm(sigma = sigma.C,
                                A = A,
                                nu = nu,
                                V = V,
                                n = n,
                                max = Inf,
                                useC = TRUE)
        ans.C[i] <- sigma.C
    }
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
})

test_that("R and C versions of updateSDNorm give same answer with upper limit", {
    updateSDNorm <- demest:::updateSDNorm
    set.seed(1)
    sigma <- runif(n = 1, 0.0001, max = 10)
    A <- runif(n = 1, min = 0.1, max = 10)
    nu <- 1.0 * max(rpois(n = 1, lambda = 5), 1)
    V <- runif(n = 1, 0.01, 10)
    n <- as.integer(rpois(n = 1, lambda = 10))
    numerator <- V - n*nu*A^2 + sqrt((V - n*nu*A^2)^2 + 4*(n + nu + 1)*V*nu*A^2)
    denominator <- 2*(n + nu + 1)
    sigma.star <- sqrt(numerator / denominator)
    max <- sigma.star * 1.5
    n.sample <- 100
    ans.R <- numeric(length = n.sample)
    ans.C <- numeric(length = n.sample)
    sigma.R <- sigma
    sigma.C <- sigma
    for (i in 1:n.sample) {
        set.seed(i)
        sigma.R <- updateSDNorm(sigma = sigma.R,
                                A = A,
                                nu = nu,
                                V = V,
                                n = n,
                                max = sigma.star,
                                useC = FALSE)
        ans.R[i] <- sigma.R
        set.seed(i)
        sigma.C <- updateSDNorm(sigma = sigma.C,
                                A = A,
                                nu = nu,
                                V = V,
                                n = n,
                                max = sigma.star,
                                useC = TRUE)
        ans.C[i] <- sigma.C
    }
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
})

if (test.extended) {
    test_that("updateSDRobust works with no upper limit", {
        updateSDRobust <- demest:::updateSDRobust
        sigma <- runif(n = 1, 0.0001, max = 10)
        A <- runif(n = 1, min = 0.1, max = 10)
        nuBeta <- 1.0 * max(rpois(n = 1, lambda = 5), 1)
        nuTau <- 1.0 * max(rpois(n = 1, lambda = 5), 1)
        V <- runif(n = 1, 0.01, 10)
        n <- rpois(n = 1, lambda = 10)
        f <- function(sigma) {
            sigma^(n * nuBeta) * exp(-nuBeta*sigma^2*V/2) * (sigma^2 + nuTau*A^2)^(-(nuTau+1)/2)
        }
        n.sample <- 100000
        ans <- numeric(length = n.sample)
        for (i in 1:n.sample) {
            sigma <- updateSDRobust(sigma = sigma,
                                    A = A,
                                    nuBeta = nuBeta,
                                    nuTau = nuTau,
                                    V = V,
                                    n = n,
                                    max = Inf,
                                    useC = TRUE)
            ans[i] <- sigma
        }
        d <- density(ans)
        y <- d$y
        x <- d$x
        non.zero <- y > 0
        y <- y[non.zero]
        x <- x[non.zero]
        f.sigma <- f(x)
        f.sigma <- f.sigma * max(y) / max(f.sigma)
        expect_true(cor(f.sigma, y) > 0.99)
        if (FALSE) {
            plot(y ~ x, type = "l")
            lines(y = f.sigma, x = x, col = "red")
        }
    })
}

if (test.extended) {
    test_that("updateSDRobust works with upper limit", {
        updateSDRobust <- demest:::updateSDRobust
        sigma <- runif(n = 1, 0.0001, max = 10)
        A <- runif(n = 1, min = 0.1, max = 10)
        nuBeta <- 1.0 * max(rpois(n = 1, lambda = 5), 1)
        nuTau <- 1.0 * max(rpois(n = 1, lambda = 5), 1)
        V <- runif(n = 1, 0.01, 10)
        n <- rpois(n = 1, lambda = 10)
        H1 <- nuBeta * V
        H2 <- nuBeta * nuTau * V * A^2 + nuTau + 1 - n * nuBeta
        H3 <- -n * nuBeta * nuTau * A^2
        sigma.star <- sqrt((-H2 + sqrt(H2^2 - 4*H1*H3))/(2*H1))
        max <- sigma.star
        f <- function(sigma) {
            sigma^(n * nuBeta) * exp(-nuBeta*sigma^2*V/2) * (sigma^2 + nuTau*A^2)^(-(nuTau+1)/2)
        }
        n.sample <- 100000
        ans <- numeric(length = n.sample)
        for (i in 1:n.sample) {
            sigma <- updateSDRobust(sigma = sigma,
                                    A = A,
                                    nuBeta = nuBeta,
                                    nuTau = nuTau,
                                    V = V,
                                    n = n,
                                    max = max,
                                    useC = TRUE)
            ans[i] <- sigma
        }
        d <- density(ans, to = 0.98 * max)
        y <- d$y
        x <- d$x
        non.zero <- y > 0
        y <- y[non.zero]
        x <- x[non.zero]
        f.sigma <- f(x)
        f.sigma <- f.sigma * max(y) / max(f.sigma)
        expect_true(cor(f.sigma, y) > 0.99)
        expect_true(all(ans < max))
        if (FALSE) {
            plot(y ~ x, type = "l")
            lines(y = f.sigma, x = x, col = "red")
        }
    })
}

test_that("R and C versions of updateSDRobust give same answer with no upper limit", {
    updateSDRobust <- demest:::updateSDRobust
    set.seed(1)
    sigma <- runif(n = 1, 0.0001, max = 10)
    A <- runif(n = 1, min = 0.1, max = 10)
    nuBeta <- 1.0 * max(rpois(n = 1, lambda = 5), 1)
    nuTau <- 1.0 * max(rpois(n = 1, lambda = 5), 1)
    V <- runif(n = 1, 0.01, 10)
    n <- as.integer(rpois(n = 1, lambda = 10))
    n.sample <- 100
    ans.R <- numeric(length = n.sample)
    ans.C <- numeric(length = n.sample)
    sigma.R <- sigma
    sigma.C <- sigma
    for (i in 1:n.sample) {
        set.seed(i)
        sigma.R <- updateSDRobust(sigma = sigma.R,
                                  A = A,
                                  nuBeta = nuBeta,
                                  nuTau = nuTau,
                                  V = V,
                                  n = n,
                                  max = 3,
                                  useC = FALSE)
        ans.R[i] <- sigma.R
        set.seed(i)
        sigma.C <- updateSDRobust(sigma = sigma.C,
                                  A = A,
                                  nuBeta = nuBeta,
                                  nuTau = nuTau,
                                  V = V,
                                  n = n,
                                  max = 3,
                                  useC = TRUE)
        ans.C[i] <- sigma.C
    }
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
})

test_that("R and C versions of updateSDRobust give same answer with upper limit", {
    updateSDRobust <- demest:::updateSDRobust
    set.seed(1)
    sigma <- runif(n = 1, 0.0001, max = 10)
    A <- runif(n = 1, min = 0.1, max = 10)
    nuBeta <- 1.0 * max(rpois(n = 1, lambda = 5), 1)
    nuTau <- 1.0 * max(rpois(n = 1, lambda = 5), 1)
    V <- runif(n = 1, 0.01, 10)
    n <- as.integer(rpois(n = 1, lambda = 10))
    n.sample <- 100
    H1 <- nuBeta * V
    H2 <- nuBeta * nuTau * V * A^2 + nuTau + 1 - n * nuBeta
    H3 <- -n * nuBeta * nuTau * A^2
    sigma.star <- sqrt((-H2 + sqrt(H2^2 - 4*H1*H3))/(2*H1))
    max <- sigma.star
    ans.R <- numeric(length = n.sample)
    ans.C <- numeric(length = n.sample)
    sigma.R <- sigma
    sigma.C <- sigma
    for (i in 1:n.sample) {
        set.seed(i)
        sigma.R <- updateSDRobust(sigma = sigma.R,
                                  A = A,
                                  nuBeta = nuBeta,
                                  nuTau = nuTau,
                                  V = V,
                                  n = n,
                                  max = sigma.star,
                                  useC = FALSE)
        ans.R[i] <- sigma.R
        set.seed(i)
        sigma.C <- updateSDRobust(sigma = sigma.C,
                                  A = A,
                                  nuBeta = nuBeta,
                                  nuTau = nuTau,
                                  V = V,
                                  n = n,
                                  max = sigma.star,
                                  useC = TRUE)
        ans.C[i] <- sigma.C
    }
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
})




## UPDATING PRIORS ################################################################
if (test.extended) {
    test_that("updateAlphaDeltaDLMWithTrend gives valid answer", {
        ffbs <- function(beta, alphaDelta, m, C, phi, tau, omegaAlpha, omegaDelta) {
            K <- length(alphaDelta) - 1
            a <- replicate(n = K, c(0, 0), simplify = FALSE)
            R <- replicate(n = K, diag(2), simplify = FALSE)
            G <- matrix(c(1, 0, 1, phi), nr = 2)
            for (k in seq_len(K)) {
                a[[k]] <- drop(G %*% m[[k]])
                R[[k]] <- G %*% C[[k]] %*% t(G) + matrix(c(omegaAlpha^2, 0, 0, omegaDelta^2), nr = 2)
                q <- R[[k]][1] + tau^2
                e <- beta[k] - a[[k]][1]
                A <- R[[k]][1:2] / q
                m[[k+1]] <- a[[k]] + A * e
                C[[k+1]] <- R[[k]] - A %*% t(A) * q
            }
            s <- svd(C[[K+1]])
            C.sqrt <- s$u %*% diag(sqrt(s$d))
            z <- rnorm(2)
            alphaDelta[[K+1]] <- m[[K+1]] + drop(C.sqrt %*% z)
            for (k in seq(from = K-1, to = 0)) {
                B <- C[[k+1]] %*% t(G) %*% solve(R[[k+1]])
                m.star <- m[[k+1]] + B %*% (alphaDelta[[k+2]] - a[[k+1]])
                C.star <- C[[k+1]] - B %*% R[[k+1]] %*% t(B)
                s <- svd(C.star)
                C.star.sqrt <- s$u %*% diag(sqrt(s$d))
                z <- rnorm(2)
                alphaDelta[[k+1]] <- m.star + drop(C.star.sqrt %*% z)
            }
            alphaDelta
        }
        updateAlphaDeltaDLMWithTrend <- demest:::updateAlphaDeltaDLMWithTrend
        initialPrior <- demest:::initialPrior
        ## dim = c(4, 10); along = 2
        spec <- DLM()
        metadata <- new("MetaData",
                        nms = c("region", "time"),
                        dimtypes = c("state", "time"),
                        DimScales = list(new("Categories",
                            dimvalues = c("a", "b", "c", "d")),
                            new("Points", dimvalues = 1:10)))
        alpha.obtained <- array(dim = c(4, 11, 1000))
        delta.obtained <- array(dim = c(4, 11, 1000))
        alpha.expected <- array(dim = c(4, 11, 1000))
        delta.expected <- array(dim = c(4, 11, 1000))
        set.seed(1)
        for (sim in 1:1000) {
            beta <- rnorm(n = 40, mean = rep(1:10, each = 4))
            betaTilde <- rnorm(n = 40, mean = rep(1:10, each = 4))
            prior <- initialPrior(spec, beta = beta, metadata = metadata, sY = NULL)
            set.seed(1 + sim)
            ans.obtained <- updateAlphaDeltaDLMWithTrend(prior = prior,
                                                         betaTilde = betaTilde,
                                                         useC = TRUE)
            alpha.obtained[ , , sim] <- ans.obtained@alphaDLM@.Data
            delta.obtained[ , , sim] <- ans.obtained@deltaDLM@.Data
            set.seed(sim + 1)
            for (i in 1:4) {
                ans <- ffbs(beta = matrix(betaTilde, nr = 4)[i,],
                            alphaDelta = replicate(n = 11, c(0, 0), simplify = FALSE),
                            m = prior@mWithTrend@.Data,
                            C = prior@CWithTrend@.Data,
                            phi = prior@phi,
                            tau = prior@tau@.Data,
                            omegaAlpha = prior@omegaAlpha@.Data,
                            omegaDelta = prior@omegaDelta@.Data)
                alpha.expected[i,,sim] <- sapply(ans, function(x) x[1])
                delta.expected[i,,sim] <- sapply(ans, function(x) x[2])
            }
        }
        alpha.obtained <- apply(alpha.obtained, 1:2, sum)/1000
        delta.obtained <- apply(delta.obtained, 1:2, sum)/1000
        alpha.expected <- apply(alpha.expected, 1:2, sum)/1000
        delta.expected <- apply(delta.expected, 1:2, sum)/1000
        expect_equal(alpha.obtained[,-1], alpha.expected[,-1], tol = 0.03)
        expect_equal(delta.obtained[,-1], delta.expected[,-1], tol = 0.03)
    })
}

test_that("R and C versions of updateAlphaDeltaDLMWithTrend give same answer", {
    updateAlphaDeltaDLMWithTrend <- demest:::updateAlphaDeltaDLMWithTrend
    initialPrior <- demest:::initialPrior
    ## dim = c(4, 10); along = 2
    for (seed in seq_len(n.test)) {
        spec <- DLM()
        metadata <- new("MetaData",
                        nms = c("region", "time"),
                        dimtypes = c("state", "time"),
                        DimScales = list(new("Categories",
                            dimvalues = c("a", "b", "c", "d")),
                            new("Points", dimvalues = 1:10)))
        set.seed(seed)
        beta <- rnorm(40)
        betaTilde <- rnorm(40)
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              multScale = 1)
        set.seed(seed)
        ans.R <- updateAlphaDeltaDLMWithTrend(prior = prior,
                                              betaTilde = betaTilde,
                                              useC = FALSE)
        set.seed(seed)
        ans.C <- updateAlphaDeltaDLMWithTrend(prior = prior,
                                              betaTilde = betaTilde,
                                              useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
        ## dim = 5; along = 1
        spec <- DLM()
        metadata <- new("MetaData",
                        nms = "age",
                        dimtypes = "age",
                        DimScales = list(new("Intervals", dimvalues = 0:5)))
        set.seed(seed)
        beta <- rnorm(5)
        betaTilde <- rnorm(5)
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              multScale = 1)
        set.seed(seed)
        ans.R <- updateAlphaDeltaDLMWithTrend(prior = prior,
                                              betaTilde = betaTilde,
                                              useC = FALSE)
        set.seed(seed)
        ans.C <- updateAlphaDeltaDLMWithTrend(prior = prior,
                                              betaTilde = betaTilde,
                                              useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
        ## dim = c(6, 6, 10); along = 2
        spec <- DLM()
        metadata <- new("MetaData",
                        nms = c("region", "time", "age"),
                        dimtypes = c("state", "time", "age"),
                        DimScales = list(new("Categories",
                            dimvalues = c("a", "b", "c", "d", "e", "f")),
                            new("Points", dimvalues = 1:6),
                            new("Intervals", dimvalues = 0:10)))
        set.seed(seed)
        beta <- rnorm(360)
        betaTilde <- rnorm(360)
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              multScale = 1)
        set.seed(seed)
        ans.R <- updateAlphaDeltaDLMWithTrend(prior = prior,
                                              betaTilde = betaTilde,
                                              useC = FALSE)
        set.seed(seed)
        ans.C <- updateAlphaDeltaDLMWithTrend(prior = prior,
                                              betaTilde = betaTilde,
                                              useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updateAlphaDLMNoTrend gives valid answer", {
    ffbs <- function(beta, alpha, m, C, phi, tau, omega) {
        K <- length(alpha) - 1L
        a <- numeric(K)
        R <- numeric(K)
        for (k in seq_len(K)) {
            a[k] <- phi * m[[k]]
            R[k] <- phi^2 * C[[k]] + omega^2
            q <- R[k] + tau^2
            e <- beta[k] - a[k]
            A <- R[k] / q
            m[[k+1]] <- a[k] + A * e
            C[[k+1]] <- R[k] - A^2 * q
        }
        alpha[K+1] <- rnorm(n = 1, mean = m[[K+1]], sd = sqrt(C[[K+1]]))
        for (k in seq(from = K, to = 1)) {
            B <- C[[k]] * phi / R[k]
            mean <- m[[k]] + B * (alpha[k+1] - a[k])
            var <- C[[k]] - B^2 * R[k]
            alpha[k] <- rnorm(n = 1, mean = mean, sd = sqrt(var))
        }
        alpha
    }
    updateAlphaDLMNoTrend <- demest:::updateAlphaDLMNoTrend
    initialPrior <- demest:::initialPrior
    ## dim = c(4, 10); along = 2
    for (seed in seq_len(n.test)) {
        spec <- DLM(trend = NULL)
        metadata <- new("MetaData",
                        nms = c("region", "time"),
                        dimtypes = c("state", "time"),
                        DimScales = list(new("Categories",
                            dimvalues = c("a", "b", "c", "d")),
                            new("Points", dimvalues = 1:10)))
        set.seed(seed)
        beta <- rnorm(40)
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              multScale = 1)
        set.seed(seed)
        ans.obtained <- updateAlphaDLMNoTrend(prior = prior,
                                              betaTilde = beta)
        ans.expected <- prior
        alpha <- matrix(nr = 4, ncol = 11)
        set.seed(seed)
        for (i in 1:4) {
            ans <- ffbs(beta = matrix(beta, nr = 4)[i,],
                        alpha = matrix(prior@alphaDLM, nr = 4)[i,],
                        m = prior@mNoTrend@.Data,
                        C = prior@CNoTrend@.Data,
                        phi = prior@phi,
                        tau = prior@tau@.Data,
                        omega = prior@omegaAlpha@.Data)
            alpha[i,] <- ans
        }
        ans.expected@alphaDLM@.Data <- as.numeric(alpha)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
        ## dim = 5; along = 1
        spec <- DLM(trend = NULL)
        metadata <- new("MetaData",
                        nms = "age",
                        dimtypes = "age",
                        DimScales = list(new("Intervals", dimvalues = 0:5)))
        set.seed(seed)
        beta <- rnorm(5)
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              multScale = 1)
        set.seed(seed)
        ans.obtained <- updateAlphaDLMNoTrend(prior = prior,
                                              beta = beta)
        ans.expected <- prior
        set.seed(seed)
        ans <- ffbs(beta = beta,
                    alpha = prior@alphaDLM@.Data,
                    m = prior@mNoTrend@.Data,
                    C = prior@CNoTrend@.Data,
                    phi = prior@phi,
                    tau = prior@tau@.Data,
                    omega = prior@omegaAlpha@.Data)
        ans.expected@alphaDLM@.Data <- as.double(ans)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
        ## dim = c(6, 6, 10); along = 2
        spec <- DLM(trend = NULL)
        metadata <- new("MetaData",
                        nms = c("region", "time", "age"),
                        dimtypes = c("state", "time", "age"),
                        DimScales = list(new("Categories",
                            dimvalues = c("a", "b", "c", "d", "e", "f")),
                            new("Points", dimvalues = 1:6),
                            new("Intervals", dimvalues = 0:10)))
        set.seed(seed)
        beta <- rnorm(360)
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              multScale = 1)
        set.seed(seed)
        ans.obtained <- updateAlphaDLMNoTrend(prior = prior,
                                              beta = beta)
        ans.expected <- prior
        alpha <- array(dim = c(6, 7, 10))
        set.seed(seed)
        for (j in 1:10) {
            for (i in 1:6) {
                ans <- ffbs(beta = array(beta, dim = c(6, 6, 10))[i, , j],
                            alpha = array(prior@alphaDLM@.Data, dim = c(6, 7, 10))[i, , j],
                            m = prior@mNoTrend@.Data,
                            C = prior@CNoTrend@.Data,
                            phi = prior@phi,
                            tau = prior@tau@.Data,
                            omega = prior@omegaAlpha@.Data)
                alpha[i, , j] <- ans
            }
        }
        ans.expected@alphaDLM@.Data <- as.numeric(alpha)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of updateAlphaDLMNoTrend give same answer", {
    updateAlphaDLMNoTrend <- demest:::updateAlphaDLMNoTrend
    initialPrior <- demest:::initialPrior
    ## dim = c(4, 10); along = 2
    for (seed in seq_len(n.test)) {
        spec <- DLM(trend = NULL)
        metadata <- new("MetaData",
                        nms = c("region", "time"),
                        dimtypes = c("state", "time"),
                        DimScales = list(new("Categories",
                            dimvalues = c("a", "b", "c", "d")),
                            new("Points", dimvalues = 1:10)))
        set.seed(seed)
        beta <- rnorm(40)
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              multScale = 1)
        set.seed(seed)
        ans.R <- updateAlphaDLMNoTrend(prior = prior,
                                       betaTilde = beta,
                                       useC = FALSE)
        set.seed(seed)
        ans.C <- updateAlphaDLMNoTrend(prior = prior,
                                       betaTilde = beta,
                                       useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
        ## dim = 5; along = 1
        spec <- DLM(trend = NULL)
        metadata <- new("MetaData",
                        nms = "age",
                        dimtypes = "age",
                        DimScales = list(new("Intervals", dimvalues = 0:5)))
        set.seed(seed)
        beta <- rnorm(5)
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              multScale = 1)
        set.seed(seed)
        ans.R <- updateAlphaDLMNoTrend(prior = prior,
                                       betaTilde = beta,
                                       useC = FALSE)
        set.seed(seed)
        ans.C <- updateAlphaDLMNoTrend(prior = prior,
                                       betaTilde = beta,
                                       useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
        ## dim = c(6, 6, 10); along = 2
        spec <- DLM(trend = NULL)
        metadata <- new("MetaData",
                        nms = c("region", "time", "age"),
                        dimtypes = c("state", "time", "age"),
                        DimScales = list(new("Categories",
                            dimvalues = c("a", "b", "c", "d", "e", "f")),
                            new("Points", dimvalues = 1:6),
                            new("Intervals", dimvalues = 0:10)))
        set.seed(seed)
        beta <- rnorm(360)
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              multScale = 1)
        set.seed(seed)
        ans.R <- updateAlphaDLMNoTrend(prior = prior,
                                       betaTilde = beta,
                                       useC = FALSE)
        set.seed(seed)
        ans.C <- updateAlphaDLMNoTrend(prior = prior,
                                       betaTilde = beta,
                                       useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updateBeta gives valid answer", {
    updateBeta <- demest:::updateBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        spec <- DLM()
        beta <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "region",
                        dimtypes = "state",
                        DimScales = list(new("Categories", dimvalues = letters[1:10])))
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL, multScale = 1)
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        ans.obtained <- updateBeta(prior = prior,
                                   vbar = vbar,
                                   n = n,
                                   sigma = sigma)
        set.seed(seed)
        mean <- (((n/sigma^2) * vbar + (1/prior@tau@.Data^2) * prior@alphaDLM@.Data[-1]) 
                 / ((n/sigma^2) + (1/prior@tau@.Data^2)))
        sd <- 1 / sqrt((n/sigma^2) + (1/prior@tau@.Data^2))
        ans.expected <- rnorm(n = 10, mean = mean, sd = sd)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of updateBeta give same answer", {
    updateBeta <- demest:::updateBeta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        spec <- DLM()
        beta <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "region",
                        dimtypes = "state",
                        DimScales = list(new("Categories", dimvalues = letters[1:10])))
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL, multScale = 1)
        vbar <- rnorm(10)
        n <- 5L
        sigma <- runif(1)
        set.seed(seed)
        ans.R <- updateBeta(prior = prior,
                            vbar = vbar,
                            n = n,
                            sigma = sigma,
                            useC = FALSE)
        set.seed(seed)
        ans.C <- updateBeta(prior = prior,
                            vbar = vbar,
                            n = n,
                            sigma = sigma,
                            useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updateBetasAndPriorsBetas works", {
    initialModel <- demest:::initialModel
    updateBetasAndPriorsBetas <- demest:::updateBetasAndPriorsBetas
    updateBetaAndPriorBeta <- demest:::updateBetaAndPriorBeta
    makeVBar <- demest:::makeVBar
    exposure <- Counts(array(rpois(n = 40, lambda = 20),
                             dim = c(5, 2, 4),
                             dimnames = list(age = c("0-4", "5-9", "10-14", "15-19", "20+"),
                                 sex = c("f", "m"),
                                 region = letters[1:4])))
    y <- Counts(array(rpois(n = 40, lambda = 0.5 * exposure),
                      dim = dim(exposure),
                      dimnames = dimnames(exposure)))
    spec <- Model(y ~ Poisson(mean ~ age * sex + region))
    model <- initialModel(spec, y = y, exposure = exposure)
    set.seed(1)
    ans.obtained <- updateBetasAndPriorsBetas(model, g = log)
    set.seed(1)
    ans.expected <- model
    for (i in 1:5) {
        vbar <- makeVBar(ans.expected, iBeta = i, g = log)
        l <- updateBetaAndPriorBeta(prior = ans.expected@priorsBetas[[i]],
                                    vbar = vbar,
                                    n = length(y) %/% length(vbar),
                                    sigma = ans.expected@sigma@.Data)
        ans.expected@betas[[i]] <- l[[1]]
        ans.expected@priorsBetas[[i]] <- l[[2]]
    }
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})

test_that("R and C versions of updateBetasAndPriorsBetas give same answer", {
    initialModel <- demest:::initialModel
    updateBetasAndPriorsBetas <- demest:::updateBetasAndPriorsBetas
    exposure <- Counts(array(rpois(n = 40, lambda = 20),
                             dim = c(5, 2, 4),
                             dimnames = list(age = c("0-4", "5-9", "10-14", "15-19", "20+"),
                                 sex = c("f", "m"),
                                 region = letters[1:4])))
    y <- Counts(array(rpois(n = 40, lambda = 0.5 * exposure),
                      dim = dim(exposure),
                      dimnames = dimnames(exposure)))
    spec <- Model(y ~ Poisson(mean ~ age * sex + region))
    model <- initialModel(spec, y = y, exposure = exposure)
    set.seed(1)
    ans.R <- updateBetasAndPriorsBetas(model, g = log, useC = FALSE)
    set.seed(1)
    ans.C <- updateBetasAndPriorsBetas(model, g = log, useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
})

test_that("R and C versions of updateBetasAndPriorsBetas give same answer with BinomialVaryingAgCertain", {
    initialModel <- demest:::initialModel
    updateBetasAndPriorsBetas <- demest:::updateBetasAndPriorsBetas
    for (seed in seq_len(n.test)) {
		set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgCertain(value = value)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.integer(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rbinom(n = 20, size = exposure, prob = theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Binomial(mean ~ age + sex), aggregate = aggregate)
        model <- initialModel(spec, y = y, exposure = exposure)
        logit <- function(x) log(x/(1-x))
        set.seed(seed)
		ans.R <- updateBetasAndPriorsBetas(model, g = logit, useC = FALSE)
		set.seed(seed)
		ans.C <- updateBetasAndPriorsBetas(model, g = logit, useC = TRUE)
		if (test.identity)
			expect_identical(ans.R, ans.C)
		else
			expect_equal(ans.R, ans.C)
	}
})

test_that("updateComponentWeightMix gives valid answer", {
    updateComponentWeightMix <- demest:::updateComponentWeightMix
    set.seed(100)
    initialPrior <- demest:::initialPrior
    beta <- rnorm(200)
    metadata <- new("MetaData",
                    nms = c("reg", "time", "age"),
                    dimtypes = c("state", "time", "age"),
                    DimScales = list(new("Categories", dimvalues = c("a", "b")),
                                     new("Points", dimvalues = 2001:2010),
                                     new("Intervals", dimvalues = as.numeric(0:10))))
    spec <- Mix()
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1)
    set.seed(2)
    ans.obtained <- updateComponentWeightMix(prior)
    set.seed(2)
    z <- matrix(prior@latentComponentWeightMix@.Data,
                nrow = prior@J@.Data)
    k <- prior@indexClassMix
    i.along <- as.integer(slice.index(array(dim = dim(metadata)), 2))
    inv.omega.sq <- 1/prior@omegaComponentWeightMix@.Data^2
    lev <- matrix(prior@levelComponentWeightMix@.Data,
                  ncol = prior@indexClassMaxMix@.Data)
    s <- seq_len(prior@indexClassMaxMix@.Data)
    W <- matrix(nrow = 10, ncol = prior@indexClassMaxMix@.Data)
    for (i in 1:10) {
        indices <- which(i.along == i)
        for (i.class in s) {
            A <- 0
            B <- 0
            for (j in indices) {
                include <- i.class <= k[j]
                if (include) {
                    A <- A + 1
                    B <- B + z[j, i.class]
                }
            }
            var <- 1/(inv.omega.sq + A)
            mean <- var*(lev[i,i.class]*inv.omega.sq + B)
            W[i, i.class] <- rnorm(1, mean = mean, sd = sqrt(var))
        }
    }
    ans.expected <- prior
    ans.expected@componentWeightMix@.Data <- as.double(W)
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})

test_that("R and C versions of updateComponentWeightMix give same answer", {
    updateComponentWeightMix <- demest:::updateComponentWeightMix
    set.seed(100)
    initialPrior <- demest:::initialPrior
    beta <- rnorm(200)
    metadata <- new("MetaData",
                    nms = c("reg", "time", "age"),
                    dimtypes = c("state", "time", "age"),
                    DimScales = list(new("Categories", dimvalues = c("a", "b")),
                                     new("Points", dimvalues = 2001:2010),
                                     new("Intervals", dimvalues = as.numeric(0:10))))
    spec <- Mix()
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1)
    set.seed(1)
    ans.R <- updateComponentWeightMix(prior, useC = FALSE)
    set.seed(1)
    ans.C <- updateComponentWeightMix(prior, useC = TRUE)
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})

test_that("updateEta gives valid answer", {
    updateEta <- demest:::updateEta
    initialPrior <- demest:::initialPrior
    rinvchisq1 <- demest:::rinvchisq1
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        data <- data.frame(region = rep(letters[1:10], times = 2),
                           sex = rep(c("f", "m"), each = 10),
                           income = rnorm(20),
                           cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
        formula <- mean ~ income * cat
        contrastsArg = list(cat = diag(3))
        spec <- Exch(covariates = Covariates(formula = formula,
                                             data = data, contrastsArg = contrastsArg))
        beta <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "region",
                        dimtypes = "state",
                        DimScales = list(new("Categories", dimvalues = letters[1:10])))
        prior0 <- initialPrior(spec,
                               beta = beta,
                               metadata = metadata,
                               sY = NULL, multScale = 1)
        expect_is(prior0, "ExchNormCov")
        beta <- rnorm(10)
        set.seed(seed)
        ans.obtained <- updateEta(prior = prior0, beta = beta)
        set.seed(seed)
        ans.expected <- prior0
        V.inv <- crossprod(prior0@Z)/prior0@tau@.Data^2 + diag(1/c(prior0@AEtaIntercept^2, prior0@UEtaCoef))
        V <- solve(V.inv)
        R <- qr.R(qr(V.inv))
        epsilon <- drop(solve(R) %*% rnorm(8))
        eta.hat <- drop(V %*% crossprod(prior0@Z, beta)/prior0@tau@.Data^2)
        eta <- eta.hat + epsilon
        eta <- unname(eta)
        ans.expected@eta@.Data <- eta
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of updateEta give same answer", {
    updateEta <- demest:::updateEta
    initialPrior <- demest:::initialPrior
    rinvchisq1 <- demest:::rinvchisq1
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        data <- data.frame(region = rep(letters[1:10], times = 2),
                           sex = rep(c("f", "m"), each = 10),
                           income = rnorm(20),
                           cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
        formula <- mean ~ income * cat
        contrastsArg = list(cat = diag(3))
        spec <- Exch(covariates = Covariates(formula = formula,
                                             data = data, contrastsArg = contrastsArg))
        beta <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "region",
                        dimtypes = "state",
                        DimScales = list(new("Categories", dimvalues = letters[1:10])))
        prior0 <- initialPrior(spec,
                               beta = beta,
                               metadata = metadata,
                               sY = NULL, multScale = 1)
        expect_is(prior0, "ExchNormCov")
        beta <- rnorm(10)
        set.seed(seed)
        ans.R <- updateEta(prior = prior0, beta = beta, useC = FALSE)
        set.seed(seed)
        ans.C <- updateEta(prior = prior0, beta = beta, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updateGWithTrend works", {
    updateGWithTrend <- demest:::updateGWithTrend
    initialPrior <- demest:::initialPrior
    spec <- DLM()
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL, multScale = 1)
    expect_is(prior, "DLMWithTrendNormZeroNoSeason")
    prior@phi <- runif(1, 0.8, 0.98)
    ans.obtained <- updateGWithTrend(prior)
    ans.expected <- prior
    ans.expected@GWithTrend@.Data[4] <- ans.expected@phi
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of updateGWithTrend give same answer", {
    updateGWithTrend <- demest:::updateGWithTrend
    initialPrior <- demest:::initialPrior
    spec <- DLM()
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL, multScale = 1)
    expect_is(prior, "DLMWithTrendNormZeroNoSeason")
    prior@phi <- runif(1, 0.8, 0.98)
    ans.R <- updateGWithTrend(prior, useC = FALSE)
    ans.C <- updateGWithTrend(prior, useC = TRUE)
    expect_identical(ans.R, ans.C)
})

## test_that("updateLevelComponentWeightMix gives valid answer", {
##     ## updateLevelComponentWeightMix <- demest:::updateLevelComponentWeightMix
##     set.seed(100)
##     initialPrior <- demest:::initialPrior
##     beta <- rnorm(200)
##     metadata <- new("MetaData",
##                     nms = c("reg", "time"),
##                     dimtypes = c("state", "time"),
##                     DimScales = list(new("Categories", dimvalues = letters[1:20]),
##                                      new("Points", dimvalues = 2001:2010)))
##     spec <- Mix()
##     prior <- initialPrior(spec,
##                           beta = beta,
##                           metadata = metadata,
##                           sY = NULL,
##                           multScale = 1)
##     set.seed(2)
##     ans.obtained <- updateLevelComponentWeightMix(prior)
##     kalman_filter <- function(mu,phi,Q,R,X0,P0,Z,useC=FALSE)
##     {
##         ## 'mu'
##         stopifnot(identical(length(mu), 1L))
##         stopifnot(is.double(mu))
##         ## 'phi'
##         stopifnot(identical(length(phi), 1L))
##         stopifnot(is.double(phi))
##         stopifnot(abs(phi) <= 1)
##         ## 'Q'
##         stopifnot(identical(length(Q), 1L))
##         stopifnot(is.double(Q))
##         ## 'R'
##         stopifnot(identical(length(R), 1L))
##         stopifnot(is.double(R))
##         ## 'X0'
##         stopifnot(identical(length(X0), 1L))
##         stopifnot(is.double(X0))
##         ## 'P0'
##         stopifnot(identical(length(P0), 1L))
##         stopifnot(is.double(P0))
##         stopifnot(P0>0)
##         ## 'Z'
##         stopifnot(is.vector(Z))
##         stopifnot(sum(is.na(Z))<1)
##         if (useC) {
##             .Call(kalman_filter_R,mu,phi,Q,R,X0,P0,Z)
##         }
##         else {
##             X <- c(X0)
##             P <- c(P0)
##             X[1] <- X0
##             N <- length(Z)
##             for (i in 1:N) 
##             {
##                                         # predict error matrix
##                 Pp <- phi^2*P[i]+Q
##                                         # Kalman Gain
##                 K <- Pp/(Pp+R)
##                                         # predict State
##                 Xp <- mu+phi*X[i]
##                                         # state correction
##                 X[i+1] <- Xp+K*(Z[i]-Xp)
##                                         # filter error
##                 P[i+1] <- (1-K)^2*Pp+K^2*R            
##             }
##             res <- cbind(X[2:(N+1)],P[2:(N+1)])
##             return(res)
##         }
##     }
##     sample_alpha_using_forback <- function(Kstar,mu,phi,sigma2EPS,sigma2ETA,W,alpha,Lw,useC=FALSE)
##     {
##         ## 'mu'
##         stopifnot(identical(length(mu), 1L))
##         stopifnot(is.double(mu))
##         ## 'phi'
##         stopifnot(identical(length(phi), 1L))
##         stopifnot(is.double(phi))
##         stopifnot(abs(phi) < 1)
##         ## 'Kstar'
##         stopifnot(identical(length(Kstar), 1L))
##         stopifnot(is.integer(Kstar))
##         stopifnot(Kstar > 0)
##         ## 'sigma2EPS'
##         stopifnot(identical(length(sigma2EPS), 1L))
##         stopifnot(is.double(sigma2EPS))
##         stopifnot(sigma2EPS > 0)
##         ## 'sigma2ETA'
##         stopifnot(identical(length(sigma2ETA), 1L))
##         stopifnot(is.double(sigma2ETA))
##         stopifnot(sigma2ETA > 0)
##         ## 'W'
##         stopifnot(identical(dim(W)[1], Lw))
##         stopifnot(is.matrix(W))
##         stopifnot(sum(is.na(W))<1)
##         ## 'alpha'
##         stopifnot(identical(dim(alpha)[1], Lw))
##         stopifnot(is.matrix(alpha))
##         stopifnot(sum(is.na(alpha))<1)
##         if (useC) {
##             .Call(sample_alpha_using_forback_R,Kstar,mu,phi,sigma2EPS,sigma2ETA,W,alpha,Lw)
##         }
##         else {
##             tutaK <- dim(W)[2]
##             for(h in 1:Kstar)
##             {
##                 ##use Kalman filter first
##                 res <- kalman_filter(mu,phi,sigma2EPS,sigma2ETA,0,1,W[,h])
##                 x <- res[,1]
##                 P <- res[,2]
##                 ##use forward filtering backward sampling algorithm
##                 alpha[Lw,h] <- rnorm(1,x[Lw],sd=sqrt(P[Lw]))
##                 for(t in (Lw-1):1)
##                 {
##                     theV <- 1/(phi^2/sigma2ETA+1/P[t])
##                     theE <- theV*(phi*(alpha[t+1,h]-mu)/sigma2ETA+x[t]/P[t])
##                     alpha[t,h] <- rnorm(1,mean=theE,sd=sqrt(theV))
##                 }
##             }
##             if(tutaK>Kstar) #sampling from the priors
##             {
##                 num <- seq(Kstar+1,tutaK)
##                 alpha[1,num] <- rnorm(length(num),mean=mu/(1-phi),sd=sqrt(sigma2ETA/(1-phi^2)))
##                 alpha[2:Lw,num] <- rnorm(length(num)*(Lw-1),mu+phi*alpha[1:(Lw-1),num],sd=sqrt(sigma2ETA))
##             }
##             return(alpha)
##         }
##     }
##     set.seed(2)
##     alpha <- sample_alpha_using_forback(Kstar = prior@indexClassMaxMix@.Data,
##                                         mu = prior@priorMeanLevelComponentWeightMix@.Data,
##                                         phi = prior@phiMix,
##                                         sigma2EPS = prior@omegaComponentWeightMix@.Data^2,
##                                         sigma2ETA = prior@omegaLevelComponentWeightMix@.Data^2,
##                                         W = matrix(prior@componentWeightMix@.Data,
##                                                    ncol = prior@indexClassMaxMix@.Data),
##                                         alpha = matrix(prior@levelComponentWeightMix@.Data,
##                                                        ncol = prior@indexClassMaxMix@.Data),
##                                         Lw = prior@indexClassMaxMix@.Data,
##                                         useC=FALSE)
##     ans.expected <- prior
##     ans.expected@levelComponentWeightMix@.Data <- as.double(alpha)
##     if (test.identity)
##         expect_identical(ans.obtained, ans.expected)
##     else
##         expect_equal(ans.obtained, ans.expected)
## })




test_that("updateOmegaAlpha works", {
    updateOmegaAlpha <- demest:::updateOmegaAlpha
    initialPrior <- demest:::initialPrior
    updateSDNorm <- demest:::updateSDNorm
    for (seed in seq_len(n.test)) {
        ## withTrend = TRUE
        spec <- DLM()
        beta <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              multScale = 1)
        expect_is(prior, "DLMWithTrendNormZeroNoSeason")
        set.seed(seed)
        ans.obtained <- updateOmegaAlpha(prior, withTrend = TRUE)
        set.seed(seed)
        ans.expected <- prior
        V <- sum((prior@alphaDLM[-1] - prior@alphaDLM[-11] - prior@deltaDLM[-11])^2)
        omega <- updateSDNorm(sigma = prior@omegaAlpha,
                              A = prior@AAlpha,
                              nu = prior@nuAlpha,
                              V = V,
                              n = prior@J,
                              max = prior@omegaAlphaMax@.Data)
        if (omega > 0)
            ans.expected@omegaAlpha@.Data <- omega
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)        
        ## withTrend = FALSE, phi = 0.9
        spec <- DLM(trend = NULL, damp = Damp(coef = 0.9))
        beta <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL, multScale = 1)
        expect_is(prior, "DLMNoTrendNormZeroNoSeason")
        set.seed(seed)
        ans.obtained <- updateOmegaAlpha(prior, withTrend = FALSE)
        set.seed(seed)
        ans.expected <- prior
        V <- sum((prior@alphaDLM[-1] - prior@phi * prior@alphaDLM[-11])^2)
        omega <- updateSDNorm(sigma = prior@omegaAlpha,
                              A = prior@AAlpha,
                              nu = prior@nuAlpha,
                              V = V,
                              n = prior@J,
                              max = prior@omegaAlphaMax@.Data)
        if (omega > 0)
            ans.expected@omegaAlpha@.Data <- omega
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)        
    }
})

test_that("R and C versions of updateOmegaAlpha give same answer", {
    updateOmegaAlpha <- demest:::updateOmegaAlpha
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        ## withTrend = TRUE
        spec <- DLM()
        beta <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL, multScale = 1)
        expect_is(prior, "DLMWithTrendNormZeroNoSeason")
        set.seed(seed)
        ans.R <- updateOmegaAlpha(prior, withTrend = TRUE, useC = FALSE)
        set.seed(seed)
        ans.C <- updateOmegaAlpha(prior, withTrend = TRUE, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)        
        ## withTrend = FALSE, phi = 0.9
        spec <- DLM(trend = NULL, damp = Damp(coef = 0.9))
        beta <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL, multScale = 1)
        expect_is(prior, "DLMNoTrendNormZeroNoSeason")
        set.seed(seed)
        ans.R <- updateOmegaAlpha(prior, withTrend = FALSE, useC = FALSE)
        set.seed(seed)
        ans.C <- updateOmegaAlpha(prior, withTrend = FALSE, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)        
    }
})

test_that("updateOmegaDelta works", {
    updateOmegaDelta <- demest:::updateOmegaDelta
    initialPrior <- demest:::initialPrior
    updateSDNorm <- demest:::updateSDNorm
    for (seed in seq_len(n.test)) {
        spec <- DLM()
        beta <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              multScale = 1)
        expect_is(prior, "DLMWithTrendNormZeroNoSeason")
        set.seed(seed)
        ans.obtained <- updateOmegaDelta(prior)
        set.seed(seed)
        ans.expected <- prior
        V <- sum((prior@deltaDLM[-1] - prior@phi * prior@deltaDLM[-11])^2)
        omega <- updateSDNorm(sigma = prior@omegaDelta,
                              A = prior@ADelta,
                              nu = prior@nuDelta,
                              V = V,
                              n = prior@J,
                              max = prior@omegaAlphaMax@.Data)
        if (omega > 0)
            ans.expected@omegaDelta@.Data <- omega
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)        
    }
})

test_that("R and C versions of updateOmegaDelta give same answer", {
    updateOmegaDelta <- demest:::updateOmegaDelta
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        spec <- DLM()
        beta <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              multScale = 1)
        expect_is(prior, "DLMWithTrendNormZeroNoSeason")
        set.seed(seed)
        ans.R <- updateOmegaDelta(prior, useC = FALSE)
        set.seed(seed)
        ans.C <- updateOmegaDelta(prior, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updateOmegaSeason works", {
    updateOmegaSeason <- demest:::updateOmegaSeason
    initialPrior <- demest:::initialPrior
    updateSDNorm <- demest:::updateSDNorm
    for (seed in seq_len(n.test)) {
        spec <- DLM(season = Season(n = 4))
        beta <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              multScale = 1)
        expect_is(prior, "DLMWithTrendNormZeroWithSeason")
        set.seed(seed)
        ans.obtained <- updateOmegaSeason(prior)
        set.seed(seed)
        ans.expected <- prior
        V <- 0
        for (i in 1:10)
            V <- V + (prior@s[[i+1]][1] - prior@s[[i]][4])^2
        omega <- updateSDNorm(sigma = prior@omegaSeason,
                              A = prior@ASeason,
                              nu = prior@nuSeason,
                              V = V,
                              n = prior@J,
                              max = prior@omegaAlphaMax@.Data)
        if (omega > 0)
            ans.expected@omegaSeason@.Data <- omega
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)        
    }
})

test_that("R and C versions of updateOmegaSeason give same answer", {
    updateOmegaSeason <- demest:::updateOmegaSeason
    initialPrior <- demest:::initialPrior
    updateSDNorm <- demest:::updateSDNorm
    for (seed in seq_len(n.test)) {
        spec <- DLM(season = Season(n = 4))
        beta <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL, multScale = 1)
        expect_is(prior, "DLMWithTrendNormZeroWithSeason")
        set.seed(seed)
        ans.R <- updateOmegaSeason(prior, useC = FALSE)
        set.seed(seed)
        ans.C <- updateOmegaSeason(prior, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updatePhi works", {
    updatePhi <- demest:::updatePhi
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        ## withTrend = TRUE
        spec <- DLM()
        beta <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL, multScale = 1)
        expect_is(prior, "DLMWithTrendNormZeroNoSeason")
        set.seed(seed)
        ans.obtained <- updatePhi(prior, withTrend = TRUE)
        set.seed(seed)
        ans.expected <- prior
        mean <- sum(prior@deltaDLM[-1] * prior@deltaDLM[-11])/sum(prior@deltaDLM[-11]^2)
        sd <- prior@omegaDelta / sqrt(sum(prior@deltaDLM[-11]^2))
        found <- FALSE
        i <- 1
        while (!found && i < 1000) {
            prop <- rnorm(n = 1, mean = mean, sd = sd)
            found <- (prop >= prior@minPhi) && (prop <= prior@maxPhi)
            i <- i + 1
        }
        if (found)
            ans.expected@phi <- prop
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
        ## withTrend = FALSE, phi = 1
        spec <- DLM(trend = NULL, damp = NULL)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL, multScale = 1)
        expect_is(prior, "DLMNoTrendNormZeroNoSeason")
        set.seed(seed)
        ans.obtained <- updatePhi(prior, withTrend = FALSE)
        set.seed(seed)
        ans.expected <- prior
        expect_identical(ans.obtained, ans.expected)
        ## withTrend = FALSE
        spec <- DLM(trend = NULL, damp = Damp(min = 0.6, max = 0.9))
        beta <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              multScale = 1)
        expect_is(prior, "DLMNoTrendNormZeroNoSeason")
        set.seed(seed)
        ans.obtained <- updatePhi(prior, withTrend = FALSE)
        set.seed(seed)
        ans.expected <- prior
        mean <- sum(prior@alphaDLM[-1] * prior@alphaDLM[-11])/sum(prior@alphaDLM[-11]^2)
        sd <- prior@omegaAlpha / sqrt(sum(prior@alphaDLM[-11]^2))
        found <- FALSE
        i <- 1
        while (!found && i < 1000) {
            prop <- rnorm(n = 1, mean = mean, sd = sd)
            found <- (prop >= prior@minPhi) && (prop <= prior@maxPhi)
            i <- i + 1
        }
        if (found)
            ans.expected@phi <- prop
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)        
    }
})

test_that("R and C versions of updatePhi give same answer", {
    updatePhi <- demest:::updatePhi
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        ## withTrend = TRUE
        spec <- DLM()
        beta <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL, multScale = 1)
        expect_is(prior, "DLMWithTrendNormZeroNoSeason")
        set.seed(seed)
        ans.R <- updatePhi(prior, withTrend = TRUE, useC = FALSE)
        set.seed(seed)
        ans.C <- updatePhi(prior, withTrend = TRUE, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)        
        ## withTrend = FALSE, phi = 1
        spec <- DLM(trend = NULL, damp = NULL)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL, multScale = 1)
        expect_is(prior, "DLMNoTrendNormZeroNoSeason")
        set.seed(seed)
        ans.R <- updatePhi(prior, withTrend = FALSE, useC = FALSE)
        set.seed(seed)
        ans.C <- updatePhi(prior, withTrend = FALSE, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)        
        ## trend = NULL, damp(min = 0.6, max = 0.9)
        spec <- DLM(trend = NULL, damp = Damp(min = 0.6, max = 0.9))
        beta <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL, multScale = 1)
        expect_is(prior, "DLMNoTrendNormZeroNoSeason")
        set.seed(seed)
        ans.R <- updatePhi(prior, withTrend = FALSE, useC = FALSE)
        set.seed(seed)
        ans.C <- updatePhi(prior, withTrend = FALSE, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)        
    }
})

test_that("updateSeason gives valid answer", {
    ffbs <- function(beta, s, m, C, tau, omega) {
        K <- length(s) - 1L
        nSeason <- length(s[[1L]])
        a <- replicate(n = K, rep(0, times = nSeason), simplify = FALSE)
        R <- replicate(n = K, diag(nSeason), simplify = FALSE)
        G <- matrix(0, nr = nSeason, nc = nSeason)
        G[1, nSeason] <- 1
        G[row(G) == col(G) + 1] <- 1
        W <- matrix(0, nr = nSeason, nc = nSeason)
        W[1] <- omega^2
        for (k in seq_len(K)) {
            a[[k]] <- drop(G %*% m[[k]])
            R[[k]] <- G %*% C[[k]] %*% t(G) + W
            q <- R[[k]][1] + tau^2
            e <- beta[k] - a[[k]][1]
            A <- R[[k]][,1] / q
            m[[k+1]] <- a[[k]] + A * e
            C[[k+1]] <- R[[k]] - A %*% t(A) * q
        }
        s[[K+1]] <- rnorm(n = nSeason, mean = m[[K+1]], sd = sqrt(diag(C[[K+1]])))
        for (k in seq(from = K, to = 1)) {
            for (i.n in seq_len(nSeason-1))
                s[[k]][i.n] <- s[[k+1]][i.n + 1]
            cn <- diag(C[[k]])[nSeason]
            s[[k]][nSeason] <- rnorm(n = 1,
                                     mean = (cn/(cn+omega^2))*s[[k+1]][1] + ((omega^2)/(cn+omega^2))*m[[k]][nSeason],
                                     sd = omega*sqrt(cn/(cn+omega^2)))
        }
        s
    }
    updateSeason <- demest:::updateSeason
    initialPrior <- demest:::initialPrior
    ## dim = c(4, 10); along = 2
    for (seed in seq_len(n.test)) {
        spec <- DLM(season = Season(n = 4))
        metadata <- new("MetaData",
                        nms = c("region", "time"),
                        dimtypes = c("state", "time"),
                        DimScales = list(new("Categories",
                            dimvalues = c("a", "b", "c", "d")),
                            new("Points", dimvalues = 1:10)))
        set.seed(seed)
        beta <- rnorm(40)
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              multScale = 1)
        set.seed(seed)
        ans.obtained <- updateSeason(prior = prior,
                                     betaTilde = beta)
        ans.expected <- prior
        season <- matrix(vector(mode = "list", length = 44), nr = 4, nc = 11)
        set.seed(seed)
        for (i in 1:4) {
            ans <- ffbs(beta = matrix(beta, nr = 4)[i,],
                        s = matrix(prior@s, nr = 4)[i,],
                        m = prior@mSeason@.Data,
                        C = lapply(prior@CSeason@.Data, function(x) diag(x)),
                        tau = prior@tau@.Data,
                        omega = prior@omegaSeason@.Data)
            season[i,] <- ans
        }
        dim(season) <- NULL
        ans.expected@s@.Data <- season
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
        ## dim = 5; along = 1
        spec <- DLM(season = Season(n = 2))
        metadata <- new("MetaData",
                        nms = "age",
                        dimtypes = "age",
                        DimScales = list(new("Intervals", dimvalues = 0:5)))
        set.seed(seed)
        beta <- rnorm(5)
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              multScale = 1)
        set.seed(seed)
        ans.obtained <- updateSeason(prior = prior,
                                     beta = beta)
        ans.expected <- prior
        set.seed(seed)
        ans <- ffbs(beta = beta,
                    s = prior@s@.Data,
                    m = prior@mSeason@.Data,
                    C = lapply(prior@CSeason@.Data, function(x) diag(x)),
                    tau = prior@tau@.Data,
                    omega = prior@omegaSeason@.Data)
        ans.expected@s@.Data <- ans
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
        ## dim = c(6, 6, 10); along = 2
        spec <- DLM(season = Season(n = 3))
        metadata <- new("MetaData",
                        nms = c("region", "time", "age"),
                        dimtypes = c("state", "time", "age"),
                        DimScales = list(new("Categories",
                            dimvalues = c("a", "b", "c", "d", "e", "f")),
                            new("Points", dimvalues = 1:6),
                            new("Intervals", dimvalues = 0:10)))
        set.seed(seed)
        beta <- rnorm(360)
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              multScale = 1)
        set.seed(seed)
        ans.obtained <- updateSeason(prior = prior,
                                     beta = beta)
        ans.expected <- prior
        season <- array(vector(mode = "list", length = 420), dim = c(6, 7, 10))
        set.seed(seed)
        for (j in 1:10) {
            for (i in 1:6) {
                ans <- ffbs(beta = array(beta, dim = c(6, 6, 10))[i, , j],
                            s = array(prior@s@.Data, dim = c(6, 7, 10))[i, , j],
                            m = prior@mSeason@.Data,
                            C = lapply(prior@CSeason@.Data, function(x) diag(x)),
                            tau = prior@tau@.Data,
                            omega = prior@omegaSeason@.Data)
                season[i, , j] <- ans
            }
        }
        dim(season) <- NULL
        ans.expected@s@.Data <- season
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of updateSeason give same answer", {
    updateSeason <- demest:::updateSeason
    initialPrior <- demest:::initialPrior
    ## dim = c(4, 10); along = 2
    for (seed in seq_len(n.test)) {
        spec <- DLM(season = Season(n = 4))
        metadata <- new("MetaData",
                        nms = c("region", "time"),
                        dimtypes = c("state", "time"),
                        DimScales = list(new("Categories",
                            dimvalues = c("a", "b", "c", "d")),
                            new("Points", dimvalues = 1:10)))
        set.seed(seed)
        beta <- rnorm(40)
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              multScale = 1)
        set.seed(seed)
        ans.R <- updateSeason(prior = prior, betaTilde = beta, useC = FALSE)
        set.seed(seed)
        ans.C <- updateSeason(prior = prior, betaTilde = beta, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
        ## dim = 5; along = 1
        spec <- DLM(season = Season(n = 2))
        metadata <- new("MetaData",
                        nms = "age",
                        dimtypes = "age",
                        DimScales = list(new("Intervals", dimvalues = 0:5)))
        set.seed(seed)
        beta <- rnorm(5)
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              multScale = 1)
        set.seed(seed)
        ans.R <- updateSeason(prior = prior, betaTilde = beta, useC = FALSE)
        set.seed(seed)
        ans.C <- updateSeason(prior = prior, betaTilde = beta, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
        ## dim = c(6, 6, 10); along = 2
        spec <- DLM(season = Season(n = 3))
        metadata <- new("MetaData",
                        nms = c("region", "time", "age"),
                        dimtypes = c("state", "time", "age"),
                        DimScales = list(new("Categories",
                            dimvalues = c("a", "b", "c", "d", "e", "f")),
                            new("Points", dimvalues = 1:6),
                            new("Intervals", dimvalues = 0:10)))
        set.seed(seed)
        beta <- rnorm(360)
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              multScale = 1)
        set.seed(seed)
        ans.R <- updateSeason(prior = prior, betaTilde = beta, useC = FALSE)
        set.seed(seed)
        ans.C <- updateSeason(prior = prior, betaTilde = beta, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updateTauNorm gives valid answer", {
    updateTauNorm <- demest:::updateTauNorm
    initialPrior <- demest:::initialPrior
    rinvchisq1 <- demest:::rinvchisq1
    updateSDNorm <- demest:::updateSDNorm
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        data <- data.frame(region = rep(letters[1:10], times = 2),
                           sex = rep(c("f", "m"), each = 10),
                           income = rnorm(20),
                           cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
        formula <- mean ~ income * cat
        contrastsArg = list(cat = diag(3))
        spec <- Exch(covariates = Covariates(formula = formula,
                         data = data, contrastsArg = contrastsArg))
        beta <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "region",
                        dimtypes = "state",
                        DimScales = list(new("Categories", dimvalues = letters[1:10])))
        prior0 <- initialPrior(spec,
                               beta = beta,
                               metadata = metadata,
                               sY = NULL, multScale = 1)
        expect_is(prior0, "ExchNormCov")
        beta <- rnorm(10)
        set.seed(seed)
        ans.obtained <- updateTauNorm(prior = prior0, beta = beta)
        set.seed(seed)
        ans.expected <- prior0
        V <- sum((beta - prior0@Z %*% prior0@eta)^2)
        ans.expected@tau@.Data <- updateSDNorm(sigma = prior0@tau@.Data,
                                               A = prior0@ATau@.Data,
                                               nu = prior0@nuTau@.Data,
                                               V = V,
                                               n = 10L,
                                               max = prior0@tauMax@.Data)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of updateTauNorm give same answer", {
    updateTauNorm <- demest:::updateTauNorm
    initialPrior <- demest:::initialPrior
    rinvchisq1 <- demest:::rinvchisq1
    updateSDNorm <- demest:::updateSDNorm
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        data <- data.frame(region = rep(letters[1:10], times = 2),
                           sex = rep(c("f", "m"), each = 10),
                           income = rnorm(20),
                           cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
        formula <- mean ~ income * cat
        contrastsArg = list(cat = diag(3))
        spec <- Exch(covariates = Covariates(formula = formula,
                         data = data, contrastsArg = contrastsArg))
        beta <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "region",
                        dimtypes = "state",
                        DimScales = list(new("Categories", dimvalues = letters[1:10])))
        prior0 <- initialPrior(spec,
                               beta = beta,
                               metadata = metadata,
                               sY = NULL, multScale = 1)
        expect_is(prior0, "ExchNormCov")
        beta <- rnorm(10)
        set.seed(seed)
        ans.R <- updateTauNorm(prior = prior0, beta = beta, useC = FALSE)
        set.seed(seed)
        ans.C <- updateTauNorm(prior = prior0, beta = beta, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updateTauRobust gives valid answer", {
    updateTauRobust <- demest:::updateTauRobust
    initialPrior <- demest:::initialPrior
    rinvchisq1 <- demest:::rinvchisq1
    updateSDRobust <- demest:::updateSDRobust
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        data <- data.frame(region = rep(letters[1:10], times = 2),
                           sex = rep(c("f", "m"), each = 10),
                           income = rnorm(20),
                           cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
        formula <- mean ~ income * cat
        contrastsArg = list(cat = diag(3))
        covariates <- Covariates(formula = formula,
                                 data = data,
                                 contrastsArg = contrastsArg)
        error <- Error(robust = TRUE)
        spec <- Exch(covariates = covariates, error = error)
        beta <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "region",
                        dimtypes = "state",
                        DimScales = list(new("Categories", dimvalues = letters[1:10])))
        prior0 <- initialPrior(spec,
                               beta = beta,
                               metadata = metadata,
                               sY = NULL,
                               multScale = 1)
        expect_is(prior0, "ExchRobustCov")
        beta <- rnorm(10)
        set.seed(seed)
        ans.obtained <- updateTauRobust(prior = prior0)
        set.seed(seed)
        ans.expected <- prior0
        V <- sum(1/prior0@UBeta)
        ans.expected@tau@.Data <- updateSDRobust(sigma = prior0@tau@.Data,
                                                 A = prior0@ATau@.Data,
                                                 nuBeta = prior0@nuBeta@.Data,
                                                 nuTau = prior0@nuTau@.Data,
                                                 V = V,
                                                 n = 10L,
                                                 max = prior0@tauMax@.Data)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of updateTauRobust give same answer", {
    updateTauRobust <- demest:::updateTauRobust
    initialPrior <- demest:::initialPrior
    rinvchisq1 <- demest:::rinvchisq1
    updateSDRobust <- demest:::updateSDRobust
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        data <- data.frame(region = rep(letters[1:10], times = 2),
                           sex = rep(c("f", "m"), each = 10),
                           income = rnorm(20),
                           cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
        formula <- mean ~ income * cat
        contrastsArg = list(cat = diag(3))
        covariates <- Covariates(formula = formula,
                                 data = data,
                                 contrastsArg = contrastsArg)
        error <- Error(robust = TRUE)
        spec <- Exch(covariates = covariates, error = error)
        beta <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "region",
                        dimtypes = "state",
                        DimScales = list(new("Categories", dimvalues = letters[1:10])))
        prior0 <- initialPrior(spec,
                               beta = beta,
                               metadata = metadata,
                               sY = NULL, multScale = 1)
        expect_is(prior0, "ExchRobustCov")
        beta <- rnorm(10)
        set.seed(seed)
        ans.R <- updateTauRobust(prior = prior0, useC = FALSE)
        set.seed(seed)
        ans.C <- updateTauRobust(prior = prior0, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updateUBeta gives valid answer", {
    updateUBeta <- demest:::updateUBeta
    initialPrior <- demest:::initialPrior
    rinvchisq1 <- demest:::rinvchisq1
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        data <- data.frame(region = rep(letters[1:10], times = 2),
                           sex = rep(c("f", "m"), each = 10),
                           income = rnorm(20),
                           cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
        formula <- mean ~ income * cat
        contrastsArg = list(cat = diag(3))
        spec <- Exch(covariates = Covariates(formula = formula,
                                             data = data, contrastsArg = contrastsArg),
                     error = Error(robust = TRUE))
        beta <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "region",
                        dimtypes = "state",
                        DimScales = list(new("Categories", dimvalues = letters[1:10])))
        prior0 <- initialPrior(spec,
                               beta = beta,
                               metadata = metadata,
                               sY = NULL, multScale = 1)
        expect_is(prior0, "ExchRobustCov")
        beta <- rnorm(10)
        set.seed(seed)
        ans.obtained <- updateUBeta(prior = prior0, beta = beta)
        set.seed(seed)
        ans.expected <- prior0
        U <- numeric(10)
        beta.hat <- prior0@Z %*% prior0@eta
        for (i in 1:10) {
            U[i] <- rinvchisq1(df = prior0@nuBeta + 1,
                               scale = ((prior0@nuBeta * prior0@tau^2 + (beta[i] - beta.hat[i])^2)
                                        / (prior0@nuBeta + 1)))
        }
        ans.expected@UBeta@.Data <- U
        expect_identical(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of updateUBeta give same answer", {
    updateUBeta <- demest:::updateUBeta
    initialPrior <- demest:::initialPrior
    rinvchisq1 <- demest:::rinvchisq1
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        data <- data.frame(region = rep(letters[1:10], times = 2),
                           sex = rep(c("f", "m"), each = 10),
                           income = rnorm(20),
                           cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
        formula <- mean ~ income * cat
        contrastsArg = list(cat = diag(3))
        spec <- Exch(covariates = Covariates(formula = formula,
                                             data = data, contrastsArg = contrastsArg),
                     error = Error(robust = TRUE))
        beta <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "region",
                        dimtypes = "state",
                        DimScales = list(new("Categories", dimvalues = letters[1:10])))
        prior0 <- initialPrior(spec,
                               beta = beta,
                               metadata = metadata,
                               sY = NULL, multScale = 1)
        expect_is(prior0, "ExchRobustCov")
        beta <- rnorm(10)
        set.seed(seed)
        ans.R <- updateUBeta(prior = prior0, beta = beta, useC = FALSE)
        set.seed(seed)
        ans.C <- updateUBeta(prior = prior0, beta = beta, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updateUEtaCoef gives valid answer", {
    updateUEtaCoef <- demest:::updateUEtaCoef
    initialPrior <- demest:::initialPrior
    rinvchisq1 <- demest:::rinvchisq1
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        data <- data.frame(region = rep(letters[1:10], times = 2),
                           sex = rep(c("f", "m"), each = 10),
                           income = rnorm(20),
                           cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
        formula <- mean ~ income * cat
        contrastsArg = list(cat = diag(3))
        spec <- Exch(covariates = Covariates(formula = formula,
                                             data = data, contrastsArg = contrastsArg),
                     error = Error(robust = TRUE))
        beta <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "region",
                        dimtypes = "state",
                        DimScales = list(new("Categories", dimvalues = letters[1:10])))
        prior0 <- initialPrior(spec,
                               beta = beta,
                               metadata = metadata,
                               sY = NULL, multScale = 1)
        expect_is(prior0, "ExchRobustCov")
        beta <- rnorm(10)
        set.seed(seed)
        ans.obtained <- updateUEtaCoef(prior = prior0)
        set.seed(seed)
        ans.expected <- prior0
        U <- numeric(7)
        for (i in 1:7) {
            U[i] <- rinvchisq1(df = prior0@nuEtaCoef + 1,
                               scale = ((prior0@nuEtaCoef * prior0@AEtaCoef^2 + prior0@eta[i+1]^2)
                                        / (prior0@nuEtaCoef + 1)))
        }
        ans.expected@UEtaCoef@.Data <- U
        expect_identical(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of updateUEtaCoef give same answer", {
    updateUEtaCoef <- demest:::updateUEtaCoef
    initialPrior <- demest:::initialPrior
    rinvchisq1 <- demest:::rinvchisq1
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        data <- data.frame(region = rep(letters[1:10], times = 2),
                           sex = rep(c("f", "m"), each = 10),
                           income = rnorm(20),
                           cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
        formula <- mean ~ income * cat
        contrastsArg = list(cat = diag(3))
        spec <- Exch(covariates = Covariates(formula = formula,
                                             data = data, contrastsArg = contrastsArg),
                     error = Error(robust = TRUE))
        beta <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "region",
                        dimtypes = "state",
                        DimScales = list(new("Categories", dimvalues = letters[1:10])))
        prior0 <- initialPrior(spec,
                               beta = beta,
                               metadata = metadata,
                               sY = NULL, multScale = 1)
        expect_is(prior0, "ExchRobustCov")
        beta <- rnorm(10)
        set.seed(seed)
        ans.R <- updateUEtaCoef(prior = prior0, useC = FALSE)
        set.seed(seed)
        ans.C <- updateUEtaCoef(prior = prior0, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updateWSqrt works", {
    updateWSqrt <- demest:::updateWSqrt
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        spec <- DLM()
        beta <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL, multScale = 1)
        expect_is(prior, "DLMWithTrendNormZeroNoSeason")
        prior@omegaAlpha@.Data <- runif(1, 0.1, 1)
        prior@omegaDelta@.Data <- runif(1, 0.1, 1)
        ans.obtained <- updateWSqrt(prior)
        ans.expected <- prior
        ans.expected@WSqrt@.Data[c(1, 4)] <- c(ans.expected@omegaAlpha@.Data^2,
                                               ans.expected@omegaDelta@.Data^2)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)        
    }
})

test_that("R and C versions of updateWSqrt give same answer", {
    updateWSqrt <- demest:::updateWSqrt
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        spec <- DLM()
        beta <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL, multScale = 1)
        expect_is(prior, "DLMWithTrendNormZeroNoSeason")
        prior@omegaAlpha@.Data <- runif(1, 0.1, 1)
        prior@omegaDelta@.Data <- runif(1, 0.1, 1)
        ans.R <- updateWSqrt(prior, useC = FALSE)
        ans.C <- updateWSqrt(prior, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)        
    }
})

test_that("updateWSqrtInvG works", {
    updateWSqrtInvG <- demest:::updateWSqrtInvG
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        spec <- DLM()
        beta <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL, multScale = 1)
        expect_is(prior, "DLMWithTrendNormZeroNoSeason")
        prior@omegaAlpha@.Data <- runif(1, 0.1, 1)
        prior@omegaDelta@.Data <- runif(1, 0.1, 1)
        ans.obtained <- updateWSqrtInvG(prior)
        ans.expected <- prior
        ans.expected@WSqrtInvG@.Data[c(1, 3)] <- 1/ans.expected@omegaAlpha@.Data
        ans.expected@WSqrtInvG@.Data[4] <- ans.expected@phi/ans.expected@omegaDelta@.Data
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)        
    }
})

test_that("R and C versions of updateWSqrtInvG give same answer", {
    updateWSqrtInvG <- demest:::updateWSqrtInvG
    initialPrior <- demest:::initialPrior
    for (seed in seq_len(n.test)) {
        spec <- DLM()
        beta <- rnorm(10)
        metadata <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2001:2010)))
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL, multScale = 1)
        expect_is(prior, "DLMWithTrendNormZeroNoSeason")
        prior@omegaAlpha@.Data <- runif(1, 0.1, 1)
        prior@omegaDelta@.Data <- runif(1, 0.1, 1)
        ans.R <- updateWSqrtInvG(prior, useC = FALSE)
        ans.C <- updateWSqrtInvG(prior, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)        
    }
})


## UPDATING MODELS ################################################################

test_that("updateSigma_Varying gives valid answer", {
    updateSigma_Varying <- demest:::updateSigma_Varying
    updateSDNorm <- demest:::updateSDNorm
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        y <- Counts(array(rpois(n = 20, lambda = 30),
                          dim = 5:4,
                          dimnames = list(age = 0:4, region = letters[1:4])))
        spec <- Model(y ~ Poisson(mean ~ age + region),
                      age ~ Exch())
        x <- initialModel(spec, y = y, exposure = NULL)
        set.seed(seed + 1)
        ans.obtained <- updateSigma_Varying(x, g = log)
        set.seed(seed + 1)
        ans.expected <- x
        mu <- x@betas[[1]] + x@betas[[2]] + rep(x@betas[[3]], each = 5)
        I <- length(x@theta)
        V <- sum((log(x@theta) - mu)^2)
        ans.expected@sigma@.Data <- updateSDNorm(sigma = ans.expected@sigma@.Data,
                                                 A = ans.expected@ASigma@.Data,
                                                 nu = ans.expected@nuSigma@.Data,
                                                 V = V,
                                                 n = I,
                                                 max = ans.expected@sigmaMax@.Data)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
        expect_identical(ans.obtained@theta, x@theta)
        expect_identical(ans.obtained@betas, x@betas)
        expect_identical(ans.obtained@priorsBetas, x@priorsBetas)
        expect_identical(ans.obtained@iteratorBetas, x@iteratorBetas)
    }
})

test_that("R and C versions of updateSigma_Varying give same answer", {
    updateSigma_Varying <- demest:::updateSigma_Varying
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        ## Poisson
        y <- Counts(array(rpois(n = 20, lambda = 30),
                          dim = 5:4,
                          dimnames = list(age = 0:4, region = letters[1:4])))
        spec <- Model(y ~ Poisson(mean ~ age + region),
                      age ~ Exch())
        x <- initialModel(spec, y = y, exposure = NULL)
        set.seed(seed + 1)
        ans.R <- updateSigma_Varying(x, g = log, useC = FALSE)
        set.seed(seed + 1)
        ans.C <- updateSigma_Varying(x, g = log, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
        ## Normal
        y <- Counts(array(rnorm(n = 20),
                          dim = 5:4,
                          dimnames = list(age = 0:4, region = letters[1:4])))
        weights <- Counts(array(1,
                                dim = 5:4,
                                dimnames = list(age = 0:4, region = letters[1:4])))
        spec <- Model(y ~ Normal(mean ~ age + region),
                      age ~ Exch())
        x <- initialModel(spec, y = y, weights = weights)
        set.seed(seed + 1)
        ans.R <- updateSigma_Varying(x, g = function(x) x, useC = FALSE)
        set.seed(seed + 1)
        ans.C <- updateSigma_Varying(x, g = function(x) x, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
        ## Binomial
        exposure <- Counts(array(rpois(n = 20, lambda = 20),
                                 dim = 5:4,
                                 dimnames = list(age = 0:4, region = letters[1:4])))
        y <- Counts(array(rbinom(n = 20, prob = 0.5, size = exposure),
                          dim = 5:4,
                          dimnames = list(age = 0:4, region = letters[1:4])))
        spec <- Model(y ~ Binomial(mean ~ age + region),
                      age ~ Exch())
        x <- initialModel(spec, y = y, exposure = exposure)## weights = weights)
        logit <- function(x) log(x/(1-x))
        set.seed(seed + 1)
        ans.R <- updateSigma_Varying(x, g = logit, useC = FALSE)
        set.seed(seed + 1)
        ans.C <- updateSigma_Varying(x, g = logit, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updateTheta_BinomialVarying gives valid answer", {
    updateTheta_BinomialVarying <- demest:::updateTheta_BinomialVarying
    initialModel <- demest:::initialModel
    logit <- function(p) log(p / (1 - p))
    invlogit <- function(y) exp(y) / (1 + exp(y))
    for (seed in seq_len(n.test)) {
        ## no missing values
        set.seed(seed)
        exposure <- Counts(array(as.integer(rpois(20, lambda  = 10)),
                                 dim = c(2, 10),
                                 dimnames = list(sex = c("f", "m"), age = 0:9)))
        prob <- runif(n = 1, min = 0.1, max = 0.9)
        y <- Counts(array(as.integer(rbinom(n = 20, size = exposure, prob = prob)),
                                 dim = c(2, 10),
                                 dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Binomial(mean ~ sex + age))
        model <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        ans.obtained <- updateTheta_BinomialVarying(model, y = y, exposure = exposure)
        set.seed(seed + 1)
        ans.expected <- model
        mu <- (model@betas[[1]]
               + model@betas[[2]]
               + rep(model@betas[[3]], each = 2))
        sigma <- model@sigma
        for (i in seq_along(model@theta)) {
            theta.curr <- model@theta[i]
            theta.prop <- invlogit(rnorm(1, mean = logit(theta.curr),
                                         sd = model@scaleTheta * model@scaleThetaMultiplier /sqrt(1 + log(1+exposure[i]))))
            log.diff <- dbinom(y[i], size = exposure[i], prob = theta.prop, log = TRUE) -
                dbinom(y[i], size = exposure[i], prob = theta.curr, log = TRUE) +
                    dnorm(logit(theta.prop), mean = mu[i], sd = sigma, log = TRUE) -
                        dnorm(logit(theta.curr), mean = mu[i], sd = sigma, log = TRUE)
            if ((log.diff >= 0) || (runif(1) < exp(log.diff))) {
                ans.expected@nAcceptTheta <- ans.expected@nAcceptTheta + 1L
                ans.expected@theta[i] <- theta.prop
            }
        }
        if (ans.expected@nAcceptTheta == 0L)
            warning("no proposals accepted")
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
        expect_identical(ans.obtained@betas, model@betas)
        expect_identical(ans.obtained@priorsBetas, model@priorsBetas)
        expect_identical(ans.obtained@sigma, model@sigma)
        expect_identical(ans.obtained@iteratorBetas, model@iteratorBetas)
        ## all missing values
        set.seed(seed)
        exposure <- Counts(array(as.integer(rpois(20, lambda  = 10)),
                                 dim = c(2, 10),
                                 dimnames = list(sex = c("f", "m"), age = 0:9)))
        prob <- runif(n = 1, min = 0.1, max = 0.9)
        y <- Counts(array(as.integer(rbinom(n = 20, size = exposure, prob = prob)),
                                 dim = c(2, 10),
                                 dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Binomial(mean ~ sex + age))
        model <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        y[] <- NA
        exposure[] <- NA
        ans.obtained <- updateTheta_BinomialVarying(model, y = y, exposure = exposure)
        set.seed(seed + 1)
        ans.expected <- model
        mu <- (model@betas[[1]]
               + model@betas[[2]]
               + rep(model@betas[[3]], each = 2))
        sigma <- model@sigma
        ans.expected@theta <- invlogit(rnorm(n = 20, mean = mu, sd = sigma))
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
        expect_identical(ans.obtained@betas, model@betas)
        expect_identical(ans.obtained@priorsBetas, model@priorsBetas)
        expect_identical(ans.obtained@sigma, model@sigma)
        expect_identical(ans.obtained@iteratorBetas, model@iteratorBetas)
    }
})

test_that("updateTheta_BinomialVarying gives valid answer - with lower, upper bounds", {
    updateTheta_BinomialVarying <- demest:::updateTheta_BinomialVarying
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        exposure <- Counts(array(as.integer(rpois(20, lambda  = 10)),
                                 dim = c(2, 10),
                                 dimnames = list(sex = c("f", "m"), age = 0:9)))
        prob <- runif(n = 1, min = 0.1, max = 0.9)
        y <- Counts(array(as.integer(rbinom(n = 20, size = exposure, prob = prob)),
                                 dim = c(2, 10),
                                 dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Binomial(mean ~ sex + age), lower = 0.7, upper = 0.8)
        model <- initialModel(spec, y = y, exposure = exposure)
        y[1:2] <- NA
        exposure[2] <- NA
        for (i in 1:5) {
            model <- updateTheta_BinomialVarying(model, y = y, exposure = exposure)
        }
        expect_true(validObject(model))
        expect_true(all(model@theta >= 0.7))
        expect_true(all(model@theta <= 0.8))
    }
})

test_that("R and C versions of updateTheta_BinomialVarying give same answer", {
    updateTheta_BinomialVarying <- demest:::updateTheta_BinomialVarying
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        exposure <- Counts(array(as.integer(rpois(20, lambda  = 10)),
                                 dim = c(2, 10),
                                 dimnames = list(sex = c("f", "m"), age = 0:9)))
        prob <- runif(n = 1, min = 0.1, max = 0.9)
        y <- Counts(array(as.integer(rbinom(n = 20, size = exposure, prob = prob)),
                                 dim = c(2, 10),
                                 dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Binomial(mean ~ sex + age), upper = 0.75)
        model <- initialModel(spec, y = y, exposure = exposure)
        y[15:20] <- NA
        exposure[18:20] <- NA
        set.seed(seed + 1)
        ans.R <- updateTheta_BinomialVarying(model, y = y, exposure = exposure, useC = FALSE)
        set.seed(seed + 1)
        ans.C <- updateTheta_BinomialVarying(model, y = y, exposure = exposure, useC = TRUE)
        if (ans.R@nAcceptTheta == 0L)
            warning("no proposals accepted")
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})


## updateTheta_BinomialVaryingAgCertain

## I can't think of a way to test that updateTheta_BinomialVaryingAgCertain
## and updateThetaAndValueBench_Binomial give the right
## answer without essentially repeating all their calculations.

test_that("updateTheta_BinomialVaryingAgCertain gives valid answer - single aggregate value", {
    updateTheta_BinomialVaryingAgCertain <- demest:::updateTheta_BinomialVaryingAgCertain
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        was.updated <- FALSE
        set.seed(seed)
        aggregate <- AgCertain(value = 0.5)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.integer(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rbinom(n = 20, size = exposure, prob = theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1:2] <- NA
        spec <- Model(y ~ Binomial(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        x1 <- updateTheta_BinomialVaryingAgCertain(x0, y = y, exposure = exposure)
        expect_true(validObject(x1))
        if (x1@nAcceptTheta > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
        if (!was.updated)
            warning("theta was not updated")
    }
})

test_that("R and C versions of updateTheta_BinomialVaryingAgCertain same answer - single aggregate value", {
    updateTheta_BinomialVaryingAgCertain <- demest:::updateTheta_BinomialVaryingAgCertain
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        was.updated <- FALSE
        set.seed(seed)
        aggregate <- AgCertain(value = 0.5)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.integer(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rbinom(n = 20, size = exposure, prob = theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[10:11] <- NA
        spec <- Model(y ~ Binomial(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        x.R <- updateTheta_BinomialVaryingAgCertain(x0, y = y, exposure = exposure, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateTheta_BinomialVaryingAgCertain(x0, y = y, exposure = exposure, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptTheta > 0L)
            was.updated <- TRUE
        if (!was.updated)
            warning("theta was not updated")
    }
})

test_that("updateTheta_BinomialVaryingAgCertain gives valid answer - multiple aggregate values", {
    updateTheta_BinomialVaryingAgCertain <- demest:::updateTheta_BinomialVaryingAgCertain
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgNormal(value = value, sd = sqrt(value))
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.integer(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rbinom(n = 20, size = exposure, prob = theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[12] <- NA
        spec <- Model(y ~ Binomial(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        x1 <- updateTheta_BinomialVaryingAgCertain(x0, y = y, exposure = exposure)
        expect_true(validObject(x1))
        if (x1@nAcceptTheta > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
        if (!was.updated)
            warning("theta was not updated")
    }
})

test_that("R and C versions of updateTheta_BinomialVaryingAgCertain same answer - multiple aggregate values", {
    updateTheta_BinomialVaryingAgCertain <- demest:::updateTheta_BinomialVaryingAgCertain
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgNormal(value = value, sd = sqrt(value))
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.integer(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rbinom(n = 20, size = exposure, prob = theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Binomial(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        x.R <- updateTheta_BinomialVaryingAgCertain(x0, y = y, exposure = exposure, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateTheta_BinomialVaryingAgCertain(x0, y = y, exposure = exposure, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptTheta > 0L)
            was.updated <- TRUE
        if (!was.updated)
            warning("theta was not updated")
    }
})

test_that("updateTheta_BinomialVaryingAgCertain gives valid answer - aggregate values with 0 weights", {
    updateTheta_BinomialVaryingAgCertain <- demest:::updateTheta_BinomialVaryingAgCertain
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgNormal(value = value, sd = sqrt(value))
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.integer(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        exposure[sample(length(exposure), size = 5)] <- 0L
        y <- as.integer(rbinom(n = 20, size = exposure, prob = theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[13:16] <- NA
        spec <- Model(y ~ Binomial(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        x1 <- updateTheta_BinomialVaryingAgCertain(x0, y = y, exposure = exposure)
        expect_true(validObject(x1))
        if (x1@nAcceptTheta > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
        if (!was.updated)
            warning("theta was not updated")
    }
})

test_that("R and C versions of updateTheta_BinomialVaryingAgCertain same answer - aggregate values with 0 weights", {
    updateTheta_BinomialVaryingAgCertain <- demest:::updateTheta_BinomialVaryingAgCertain
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgNormal(value = value, sd = sqrt(value))
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.integer(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        exposure[sample(length(exposure), size = 5)] <- 0L
        y <- as.integer(rbinom(n = 20, size = exposure, prob = theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1] <- NA
        spec <- Model(y ~ Binomial(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        x.R <- updateTheta_BinomialVaryingAgCertain(x0, y = y, exposure = exposure, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateTheta_BinomialVaryingAgCertain(x0, y = y, exposure = exposure, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptTheta > 0L)
            was.updated <- TRUE
        if (!was.updated)
            warning("theta was not updated")
    }
})

test_that("updateThetaAndValueAgNormal_Binomial gives valid answer - single aggregate value", {
    updateThetaAndValueAgNormal_Binomial <- demest:::updateThetaAndValueAgNormal_Binomial
    initialModel <- demest:::initialModel
    was.updated <- FALSE ## only test if was ever updated, since only one update done per iteration
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        aggregate <- AgNormal(value = 0.5, sd = 0.01, jump = 0.001)
        theta <- rbeta(n = 20, shape1 = 5, shape2 = 5)
        exposure <- as.integer(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10),
                                 dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rbinom(n = 20, size = exposure, prob = theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1] <- NA
        spec <- Model(y ~ Binomial(mean ~ age + sex), jump = 0.01, aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        x1 <- updateThetaAndValueAgNormal_Binomial(x0, y = y, exposure = exposure)
        expect_true(validObject(x1))
        if (x1@nAcceptAg@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
    }
    if (!was.updated)
        warning("theta and bench not updated")
})

test_that("R and C versions of updateThetaAndValueAgNormal_Binomial same answer - single aggregate value", {
    updateThetaAndValueAgNormal_Binomial <- demest:::updateThetaAndValueAgNormal_Binomial
    initialModel <- demest:::initialModel
    was.updated <- FALSE  ## only test if was ever updated, since only one update done per iteration
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        aggregate <- AgNormal(value = 0.5, sd = 0.2)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.integer(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rbinom(n = 20, size = exposure, prob = theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[3] <- NA
        spec <- Model(y ~ Binomial(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgNormal_Binomial(x0, y = y, exposure = exposure, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgNormal_Binomial(x0, y = y, exposure = exposure, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptAg > 0L)
            was.updated <- TRUE
    }
    if (!was.updated)
        warning("theta was not updated")
})

test_that("updateThetaAndValueAgNormal_Binomial gives valid answer - multiple aggregate values", {
    updateThetaAndValueAgNormal_Binomial <- demest:::updateThetaAndValueAgNormal_Binomial
    initialModel <- demest:::initialModel
    was.updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgNormal(value = value, sd = sqrt(value), jump = 0.001)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.integer(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rbinom(n = 20, size = exposure, prob = theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[3] <- NA
        spec <- Model(y ~ Binomial(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        x1 <- updateThetaAndValueAgNormal_Binomial(x0, y = y, exposure = exposure)
        expect_true(validObject(x1))
        if (x1@nAcceptAg@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
    }
    if (!was.updated)
        warning("theta was not updated")
})

test_that("R and C versions of updateThetaAndValueAgNormal_Binomial same answer - multiple aggregate values", {
    updateThetaAndValueAgNormal_Binomial <- demest:::updateThetaAndValueAgNormal_Binomial
    initialModel <- demest:::initialModel
    was.updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgNormal(value = value, sd = sqrt(value))
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.integer(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rbinom(n = 20, size = exposure, prob = theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[4] <- NA
        spec <- Model(y ~ Binomial(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgNormal_Binomial(x0, y = y, exposure = exposure, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgNormal_Binomial(x0, y = y, exposure = exposure, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptAg@.Data > 0L)
            was.updated <- TRUE
    }
    if (!was.updated)
        warning("theta was not updated")
})


## updateThetaAndValueAgFun_Binomial

test_that("updateThetaAndValueAgFun_Binomial gives valid answer - single aggregate value", {
    updateThetaAndValueAgFun_Binomial <- demest:::updateThetaAndValueAgFun_Binomial
    initialModel <- demest:::initialModel
    ## no missing values
    was.updated <- FALSE ## only test if was ever updated, since only one update done per iteration
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        FUN <- function(x, weights) sum(x * sqrt(weights))
        aggregate <- AgFun(value = 0.5, sd = 0.01, FUN = FUN)
        theta <- rbeta(n = 20, shape1 = 5, shape2 = 5)
        exposure <- as.integer(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10),
                                 dimnames = list(sex = c("f", "m"), age = 0:9)),
                           dimscales = c(age = "Intervals"))
        y <- as.integer(rbinom(n = 20, prob = theta, size = exposure))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)),
                    dimscales = c(age = "Intervals"))
        spec <- Model(y ~ Binomial(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        expect_is(x0, "BinomialVaryingAgFun")
        x1 <- updateThetaAndValueAgFun_Binomial(x0, y = y, exposure = exposure)
        expect_true(validObject(x1))
        if (x1@nAcceptTheta@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
    }
    if (!was.updated)
        warning("theta and bench not updated")
    ## has missing values
    was.updated <- FALSE ## only test if was ever updated, since only one update done per iteration
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        FUN <- function(x, weights) sum(x * sqrt(weights))
        aggregate <- AgFun(value = 0.5, sd = 0.01, FUN = FUN)
        theta <- rbeta(n = 20, shape1 = 5, shape2 = 5)
        exposure <- as.integer(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10),
                                 dimnames = list(sex = c("f", "m"), age = 0:9)),
                           dimscales = c(age = "Intervals"))
        y <- as.integer(rbinom(n = 20, prob = theta, size = exposure))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)),
                    dimscales = c(age = "Intervals"))
        y[1:5] <- NA
        spec <- Model(y ~ Binomial(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        x1 <- updateThetaAndValueAgFun_Binomial(x0, y = y, exposure = exposure)
        expect_true(validObject(x1))
        if (x1@nAcceptTheta@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
    }
    if (!was.updated)
        warning("theta and bench not updated")
})

test_that("R and C versions of updateThetaAndValueAgFun_Binomial same answer - single aggregate value", {
    updateThetaAndValueAgFun_Binomial <- demest:::updateThetaAndValueAgFun_Binomial
    initialModel <- demest:::initialModel
    ## no missing values
    was.updated <- FALSE  ## only test if was ever updated, since only one update done per iteration
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        FUN <- function(x, weights) sum(x * sqrt(weights))
        aggregate <- AgFun(value = 0.5, sd = 0.2, FUN = FUN)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.integer(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)),
                           dimscales = c(age = "Intervals"))
        y <- as.integer(rbinom(n = 20, prob = theta, size = exposure))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)),
                    dimscales = c(age = "Intervals"))
        spec <- Model(y ~ Binomial(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgFun_Binomial(x0, y = y, exposure = exposure, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgFun_Binomial(x0, y = y, exposure = exposure, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptTheta@.Data > 0L)
            was.updated <- TRUE
    }
    if (!was.updated)
        warning("theta was not updated")
    ## has missing values
    was.updated <- FALSE  ## only test if was ever updated, since only one update done per iteration
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        FUN <- function(x, weights) sum(x * sqrt(weights))
        aggregate <- AgFun(value = 0.5, sd = 0.2, FUN = FUN)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.integer(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rbinom(n = 20, prob = theta, size = exposure))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1:5] <- NA
        spec <- Model(y ~ Binomial(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgFun_Binomial(x0, y = y, exposure = exposure, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgFun_Binomial(x0, y = y, exposure = exposure, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptTheta@.Data > 0L)
            was.updated <- TRUE
    }
    if (!was.updated)
        warning("theta was not updated")
})

test_that("updateThetaAndValueAgFun_Binomial gives valid answer - multiple aggregate values", {
    updateThetaAndValueAgFun_Binomial <- demest:::updateThetaAndValueAgFun_Binomial
    initialModel <- demest:::initialModel
    ## no missing values
    for (seed in seq_len(n.test)) {
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        FUN <- function(x, weights) sum(x * sqrt(weights))
        aggregate <- AgFun(value = value, sd = sqrt(value), FUN = FUN)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.integer(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)),
                           dimscales = c(age = "Intervals"))
        y <- as.integer(rbinom(n = 20, prob = theta, size = exposure))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)),
                    dimscales = c(age = "Intervals"))
        spec <- Model(y ~ Binomial(mean ~ age + sex), jump = 0.001, aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        x1 <- updateThetaAndValueAgFun_Binomial(x0, y = y, exposure = exposure)
        expect_true(validObject(x1))
        if (x1@nAcceptTheta@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
        if (!was.updated)
            warning("theta was not updated")
    }
    ## has missing values
    for (seed in seq_len(n.test)) {
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        FUN <- function(x, weights) sum(x * sqrt(weights))
        aggregate <- AgFun(value = value, sd = sqrt(value), FUN = FUN)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.integer(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)),
                           dimscales = c(age = "Intervals"))
        y <- as.integer(rbinom(n = 20, prob = theta, size = exposure))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)),
                    dimscales = c(age = "Intervals"))
        y[1:5] <- NA
        spec <- Model(y ~ Binomial(mean ~ age + sex), jump = 0.001, aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        x1 <- updateThetaAndValueAgFun_Binomial(x0, y = y, exposure = exposure)
        expect_true(validObject(x1))
        if (x1@nAcceptTheta@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
        if (!was.updated)
            warning("theta was not updated")
    }
})

test_that("R and C versions of updateThetaAndValueAgFun_Binomial same answer - multiple aggregate values", {
    updateThetaAndValueAgFun_Binomial <- demest:::updateThetaAndValueAgFun_Binomial
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        ## no missing values
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)),
                        dimscales = c(age = "Intervals"))
        FUN <- function(x, weights) sum(x * sqrt(weights))
        aggregate <- AgFun(value = value, sd = sqrt(value), FUN = FUN)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.integer(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)),
                           dimscales = c(age = "Intervals"))
        y <- as.integer(rbinom(n = 20, prob = theta, size = exposure))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Binomial(mean ~ age + sex), jump = 0.1, aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgFun_Binomial(x0, y = y, exposure = exposure, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgFun_Binomial(x0, y = y, exposure = exposure, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptTheta@.Data > 0L)
            was.updated <- TRUE
        if (!was.updated)
            warning("aggregate value was not updated")
        ## has missing values
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)),
                        dimscales = c(age = "Intervals"))
        FUN <- function(x, weights) sum(x * sqrt(weights))
        aggregate <- AgFun(value = value, sd = sqrt(value), FUN = FUN)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.integer(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rbinom(n = 20, prob = theta, size = exposure))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)),
                    dimscales = c(age = "Intervals"))
        spec <- Model(y ~ Binomial(mean ~ age + sex), jump = 0.1, aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgFun_Binomial(x0, y = y, exposure = exposure, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgFun_Binomial(x0, y = y, exposure = exposure, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptTheta@.Data > 0L)
            was.updated <- TRUE
        if (!was.updated)
            warning("aggregate value was not updated")
    }
})


## updateTheta_NormalVarying

test_that("updateTheta_NormalVarying, no limits, gives valid answer", {
    updateTheta_NormalVarying <- demest:::updateTheta_NormalVarying
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ## no missing values
        y <- Values(array(rnorm(n = 20),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        w <- Counts(array(runif(n = 20),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        spec <- Model(y ~ Normal(mean ~ age + region))
        model <- initialModel(spec, y = y, weights = w)
        set.seed(seed + 1)
        ans.obtained <- updateTheta_NormalVarying(model, y = y)
        set.seed(seed + 1)
        ans.expected <- model
        mu <- (model@betas[[1]]
               + model@betas[[2]]
               + rep(model@betas[[3]], each = 5))
        prec.prior <- 1 / model@sigma^2
        prec.data <- model@w / model@varsigma^2
        mean <- (prec.prior / (prec.prior + prec.data)) * mu +
            (prec.data / (prec.prior + prec.data)) * y
        var <- 1 / (prec.prior + prec.data)
        ans.expected@theta <- rnorm(20, mean = mean, sd = sqrt(var))
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
        expect_identical(ans.obtained@varsigma, model@varsigma)
        expect_identical(ans.obtained@w, model@w)
        expect_identical(ans.obtained@sigma, model@sigma)
        expect_identical(ans.obtained@betas, model@betas)
        expect_identical(ans.obtained@priorsBetas, model@priorsBetas)
        ## all missing values
        y <- Values(array(rnorm(n = 20),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        w <- Counts(array(runif(n = 20),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        spec <- Model(y ~ Normal(mean ~ age + region))
        model <- initialModel(spec, y = y, weights = w)
        y[] <- NA
        set.seed(seed + 1)
        ans.obtained <- updateTheta_NormalVarying(model, y = y)
        set.seed(seed + 1)
        ans.expected <- model
        mu <- (model@betas[[1]]
               + model@betas[[2]]
               + rep(model@betas[[3]], each = 5))
        ans.expected@theta <- rnorm(20, mean = mu, sd = model@sigma)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("updateTheta_NormalVarying, with limits, gives valid answer", {
    updateTheta_NormalVarying <- demest:::updateTheta_NormalVarying
    initialModel <- demest:::initialModel
    rnormTruncated <- demest:::rnormTruncated
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ## no missing values
        y <- Values(array(rnorm(n = 20),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        w <- Counts(array(runif(n = 20),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        spec <- Model(y ~ Normal(mean ~ age + region), lower = -2, upper = 2)
        model <- initialModel(spec, y = y, weights = w)
        set.seed(seed + 1)
        ans.obtained <- updateTheta_NormalVarying(model, y = y)
        set.seed(seed + 1)
        ans.expected <- model
        mu <- (model@betas[[1]]
               + model@betas[[2]]
               + rep(model@betas[[3]], each = 5))
        prec.prior <- 1 / model@sigma@.Data^2
        prec.data <- model@w / model@varsigma@.Data^2
        mean <- (prec.prior / (prec.prior + prec.data)) * mu +
            (prec.data / (prec.prior + prec.data)) * y
        var <- 1 / (prec.prior + prec.data)
        theta <- numeric(20)
        for (i in 1:20)
            theta[i] <- rnormTruncated(n = 1L,
                                       mean = mean[i],
                                       sd = sqrt(var)[i],
                                       lower = -2,
                                       upper = 2,
                                       tolerance = 1e-5,
                                       maxAttempt = 100L,
                                       uniform = FALSE,
                                       useC = TRUE)
        ans.expected@theta <- theta
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
        expect_identical(ans.obtained@varsigma, ans.expected@varsigma)
        expect_identical(ans.obtained@w, ans.expected@w)
        expect_identical(ans.obtained@sigma, ans.expected@sigma)
        expect_identical(ans.obtained@betas, ans.expected@betas)
        expect_identical(ans.obtained@priorsBetas, ans.expected@priorsBetas)
        expect_true(all(ans.obtained@theta > -2))
        expect_true(all(ans.obtained@theta < 2))
        ## has missing values
        y <- Values(array(rnorm(n = 20),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        w <- Counts(array(runif(n = 20),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        y[1:10] <- NA
        w[1:9] <- NA
        spec <- Model(y ~ Normal(mean ~ age + region), lower = -2, upper = 2)
        model <- initialModel(spec, y = y, weights = w)
        set.seed(seed + 1)
        ans.obtained <- updateTheta_NormalVarying(model, y = y)
        set.seed(seed + 1)
        ans.expected <- model
        mu <- (model@betas[[1]]
               + model@betas[[2]]
               + rep(model@betas[[3]], each = 5))
        prec.prior <- 1 / model@sigma@.Data^2
        prec.data <- model@w / model@varsigma@.Data^2
        mean <- (prec.prior / (prec.prior + prec.data)) * mu +
            (prec.data / (prec.prior + prec.data)) * y
        mean[1:10] <- mu[1:10]
        var <- 1 / (prec.prior + prec.data)
        sd <- sqrt(var)
        sd[1:10] <- model@sigma@.Data
        theta <- numeric(20)
        for (i in 1:20)
            theta[i] <- rnormTruncated(n = 1L,
                                       mean = mean[i],
                                       sd = sd[i],
                                       lower = -2,
                                       upper = 2,
                                       tolerance = 1e-5,
                                       maxAttempt = 100L,
                                       uniform = FALSE,
                                       useC = TRUE)
        ans.expected@theta <- theta
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
        expect_identical(ans.obtained@varsigma, ans.expected@varsigma)
        expect_identical(ans.obtained@w, ans.expected@w)
        expect_identical(ans.obtained@sigma, ans.expected@sigma)
        expect_identical(ans.obtained@betas, ans.expected@betas)
        expect_identical(ans.obtained@priorsBetas, ans.expected@priorsBetas)
        expect_true(all(ans.obtained@theta > -2))
        expect_true(all(ans.obtained@theta < 2))
    }
})

test_that("R and C versions of updateTheta_NormalVarying, no limits, give same answer", {
    updateTheta_NormalVarying <- demest:::updateTheta_NormalVarying
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        ## no missing values
        y <- Values(array(rnorm(n = 20),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        w <- Counts(array(runif(n = 20),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        spec <- Model(y ~ Normal(mean ~ age + region))
        model <- initialModel(spec, y = y, weights = w)
        set.seed(seed + 1)
        ans.R <- updateTheta_NormalVarying(model, y = y, useC = FALSE)
        set.seed(seed + 1)
        ans.C <- updateTheta_NormalVarying(model, y = y, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
        ## has missing values
        y <- Values(array(rnorm(n = 20),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        w <- Counts(array(runif(n = 20),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        y[1:5] <- NA
        spec <- Model(y ~ Normal(mean ~ age + region))
        model <- initialModel(spec, y = y, weights = w)
        set.seed(seed + 1)
        ans.R <- updateTheta_NormalVarying(model, y = y, useC = FALSE)
        set.seed(seed + 1)
        ans.C <- updateTheta_NormalVarying(model, y = y, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

## JAH cannot get this test to run correctly - complaints from R about the model etc.  
## replacement test hacked from the test of the R code only below
test_that("R and C versions of updateTheta_NormalVarying, with limits, give same answer", {
    updateTheta_NormalVarying <- demest:::updateTheta_NormalVarying
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        ## no missing values
        y <- Values(array(rnorm(n = 20),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        w <- Counts(array(runif(n = 20),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        spec <- Model(y ~ Normal(mean ~ age + region), lower = -2, upper = 3)
        model <- initialModel(spec, y = y, weights = w)
        set.seed(seed + 1)
        ans.obtained <- updateTheta_NormalVarying(model, y = y)
        set.seed(seed)
        set.seed(seed + 1)
        ans.R <- updateTheta_NormalVarying(model, y = y, useC = FALSE)
        set.seed(seed + 1)
        ans.C <- updateTheta_NormalVarying(model, y = y, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
        ## has missing values
        y <- Values(array(rnorm(n = 20),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        w <- Counts(array(runif(n = 20),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        y[1:5] <- NA
        spec <- Model(y ~ Normal(mean ~ age + region), lower = -1, upper = 3)
        model <- initialModel(spec, y = y, weights = w)
        set.seed(seed + 1)
        ans.obtained <- updateTheta_NormalVarying(model, y = y)
        set.seed(seed)
        set.seed(seed + 1)
        ans.R <- updateTheta_NormalVarying(model, y = y, useC = FALSE)
        set.seed(seed + 1)
        ans.C <- updateTheta_NormalVarying(model, y = y, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})


## new test - JAH 24/4/2016
test_that("updateTheta_NormalVarying, with limits, gives valid answer", {
    updateTheta_NormalVarying <- demest:::updateTheta_NormalVarying
    initialModel <- demest:::initialModel
    rnormTruncated <- demest:::rnormTruncated
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ## no missing values
        y <- Values(array(rnorm(n = 20),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        w <- Counts(array(runif(n = 20),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        spec <- Model(y ~ Normal(mean ~ age + region), lower = -2, upper = 2)
        model <- initialModel(spec, y = y, weights = w)
        set.seed(seed + 1)
        ans.R <- updateTheta_NormalVarying(model, y = y, useC = FALSE)
        set.seed(seed + 1)
        ans.C <- updateTheta_NormalVarying(model, y = y, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
        ## has missing values
        y <- Values(array(rnorm(n = 20),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        w <- Counts(array(runif(n = 20),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        y[1:10] <- NA
        w[1:9] <- NA
        spec <- Model(y ~ Normal(mean ~ age + region), lower = -2, upper = 2)
        model <- initialModel(spec, y = y, weights = w)
        set.seed(seed + 1)
        ans.R <- updateTheta_NormalVarying(model, y = y, useC = FALSE)
        set.seed(seed + 1)
        ans.C <- updateTheta_NormalVarying(model, y = y, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})


## updateTheta_NormalVaryingAgCertain

test_that("updateTheta_NormalVaryingAgCertain gives valid answer - single aggregate value", {
    updateTheta_NormalVaryingAgCertain <- demest:::updateTheta_NormalVaryingAgCertain
    initialModel <- demest:::initialModel
    was.updated <- FALSE
    ## no missing values
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        aggregate <- AgCertain(value = 2)
        y <- rnorm(n = 20)
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        weights <- 5 * rbeta(n = 20, shape1 = 1, shape2= 1)
        weights <- Counts(array(weights, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Normal(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, weights = weights)
        x1 <- updateTheta_NormalVaryingAgCertain(x0, y = y)
        expect_true(validObject(x1))
        if (x1@nAcceptTheta > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
    }
    if (!was.updated)
        warning("theta was not updated")
    ## has missing values
    was.updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        aggregate <- AgCertain(value = 2)
        y <- rnorm(n = 20)
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        weights <- 5 * rbeta(n = 20, shape1 = 1, shape2= 1)
        weights <- Counts(array(weights, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1:5] <- NA
        spec <- Model(y ~ Normal(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, weights = weights)
        x1 <- updateTheta_NormalVaryingAgCertain(x0, y = y)
        expect_true(validObject(x1))
        if (x1@nAcceptTheta > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
    }
    if (!was.updated)
        warning("theta was not updated")
})

test_that("R and C versions of updateTheta_NormalVaryingAgCertain same answer - single aggregate value", {
    updateTheta_NormalVaryingAgCertain <- demest:::updateTheta_NormalVaryingAgCertain
    initialModel <- demest:::initialModel
    ## no missing values
    was.updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        aggregate <- AgCertain(value = 0.5)
        y <- rnorm(n = 20)
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        weights <- 5 * rbeta(n = 20, shape1 = 1, shape2= 1)
        weights <- Counts(array(weights, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Normal(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, weights = weights)
        set.seed(seed + 1)
        x.R <- updateTheta_NormalVaryingAgCertain(x0, y = y, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateTheta_NormalVaryingAgCertain(x0, y = y, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptTheta > 0L)
            was.updated <- TRUE
    }
    if (!was.updated)
        warning("theta was not updated")
    ## has missing values
    was.updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        aggregate <- AgCertain(value = 0.5)
        y <- rnorm(n = 20)
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[11:15] <- NA
        weights <- 5 * rbeta(n = 20, shape1 = 1, shape2= 1)
        weights <- Counts(array(weights, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Normal(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, weights = weights)
        set.seed(seed + 1)
        x.R <- updateTheta_NormalVaryingAgCertain(x0, y = y, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateTheta_NormalVaryingAgCertain(x0, y = y, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptTheta > 0L)
            was.updated <- TRUE
    }
    if (!was.updated)
        warning("theta was not updated")
})

test_that("updateTheta_NormalVaryingAgCertain gives valid answer - multiple aggregate values", {
    updateTheta_NormalVaryingAgCertain <- demest:::updateTheta_NormalVaryingAgCertain
    initialModel <- demest:::initialModel
    ## no missing values
    was.updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgNormal(value = value, sd = sqrt(value))
        y <- rnorm(n = 20)
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        weights <- 5 * rbeta(n = 20, shape1 = 1, shape2= 1)
        weights <- Counts(array(weights, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Normal(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, weights = weights)
        x1 <- updateTheta_NormalVaryingAgCertain(x0, y = y)
        expect_true(validObject(x1))
        if (x1@nAcceptTheta > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
    }
    if (!was.updated)
        warning("theta was not updated")
    ## has missing values
    was.updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgNormal(value = value, sd = sqrt(value))
        y <- rnorm(n = 20)
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1:3] <- NA
        weights <- 5 * rbeta(n = 20, shape1 = 1, shape2= 1)
        weights <- Counts(array(weights, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Normal(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, weights = weights)
        x1 <- updateTheta_NormalVaryingAgCertain(x0, y = y)
        expect_true(validObject(x1))
        if (x1@nAcceptTheta > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
    }
    if (!was.updated)
        warning("theta was not updated")
})

test_that("R and C versions of updateTheta_NormalVaryingAgCertain same answer - multiple aggregate values", {
    updateTheta_NormalVaryingAgCertain <- demest:::updateTheta_NormalVaryingAgCertain
    initialModel <- demest:::initialModel
    ## no missing values
    was.updated <- FALSE
    for (seed in seq_len(n.test)) {
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgNormal(value = value, sd = sqrt(value))
        y <- rnorm(n = 20)
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        weights <- 5 * rbeta(n = 20, shape1 = 1, shape2= 1)
        weights <- Counts(array(weights, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Normal(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, weights = weights)
        set.seed(seed + 1)
        x.R <- updateTheta_NormalVaryingAgCertain(x0, y = y, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateTheta_NormalVaryingAgCertain(x0, y = y, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptTheta@.Data > 0L)
            was.updated <- TRUE
    }
    if (!was.updated)
        warning("theta was not updated")
    ## has missing values
    was.updated <- FALSE
    for (seed in seq_len(n.test)) {
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgNormal(value = value, sd = sqrt(value))
        y <- rnorm(n = 20)
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[15:20] <- NA
        weights <- 5 * rbeta(n = 20, shape1 = 1, shape2= 1)
        weights <- Counts(array(weights, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Normal(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, weights = weights)
        set.seed(seed + 1)
        x.R <- updateTheta_NormalVaryingAgCertain(x0, y = y, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateTheta_NormalVaryingAgCertain(x0, y = y, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptTheta@.Data > 0L)
            was.updated <- TRUE
    }
    if (!was.updated)
        warning("theta was not updated")
})

test_that("updateTheta_NormalVaryingAgCertain gives valid answer - some aggregate weights equal to 0", {
    updateTheta_NormalVaryingAgCertain <- demest:::updateTheta_NormalVaryingAgCertain
    initialModel <- demest:::initialModel
    was.updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        w <- 5 * rbeta(n = 20, shape1 = 1, shape2= 1)
        w <- Counts(array(w, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        w[sample(length(w), size = 5)] <- 0
        aggregate <- AgNormal(value = value, sd = sqrt(value), weights = w)
        y <- rnorm(n = 20)
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1] <- NA
        weights <- 5 * rbeta(n = 20, shape1 = 1, shape2= 1)
        weights <- Counts(array(weights, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Normal(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, weights = weights)
        x1 <- updateTheta_NormalVaryingAgCertain(x0, y = y)
        expect_true(validObject(x1))
        if (x1@nAcceptTheta > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
    }
    if (!was.updated)
        warning("theta was not updated")
})

test_that("R and C versions of updateTheta_NormalVaryingAgCertain same answer - some aggregate weights equal to 0", {
    updateTheta_NormalVaryingAgCertain <- demest:::updateTheta_NormalVaryingAgCertain
    initialModel <- demest:::initialModel
    was.updated <- FALSE
    for (seed in seq_len(n.test)) {
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        w <- 5 * rbeta(n = 20, shape1 = 1, shape2= 1)
        w <- Counts(array(w, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        w[sample(length(w), size = 5)] <- 0
        aggregate <- AgNormal(value = value, sd = sqrt(value), weights = w)
        y <- rnorm(n = 20)
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1] <- NA
        weights <- 5 * rbeta(n = 20, shape1 = 1, shape2= 1)
        weights <- Counts(array(weights, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Normal(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, weights = weights)
        set.seed(seed + 1)
        x.R <- updateTheta_NormalVaryingAgCertain(x0, y = y, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateTheta_NormalVaryingAgCertain(x0, y = y, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptTheta@.Data > 0L)
            was.updated <- TRUE
    }
    if (!was.updated)
        warning("theta was not updated")
})


## updateThetaAndValueAgNormal_Normal

test_that("updateThetaAndValueAgNormal_Normal gives valid answer - single aggregate value", {
    updateThetaAndValueAgNormal_Normal <- demest:::updateThetaAndValueAgNormal_Normal
    initialModel <- demest:::initialModel
    ## no missing values
    was.updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        aggregate <- AgNormal(value = 0.5, sd = 0.01, jump = 0.001)
        y <- rnorm(n = 20)
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        weights <- 5 * rbeta(n = 20, shape1 = 1, shape2= 1)
        weights <- Counts(array(weights, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Normal(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, weights = weights)
        x1 <- updateThetaAndValueAgNormal_Normal(x0, y = y)
        expect_true(validObject(x1))
        if (x1@nAcceptAg@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
    }
    if (!was.updated)
        warning("theta and bench not updated")
    ## has missing values
    was.updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        aggregate <- AgNormal(value = 0.5, sd = 0.01, jump = 0.001)
        y <- rnorm(n = 20)
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1:5] <- NA
        weights <- 5 * rbeta(n = 20, shape1 = 1, shape2= 1)
        weights <- Counts(array(weights, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Normal(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, weights = weights)
        x1 <- updateThetaAndValueAgNormal_Normal(x0, y = y)
        expect_true(validObject(x1))
        if (x1@nAcceptAg@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
    }
    if (!was.updated)
        warning("theta and bench not updated")
})

test_that("R and C versions of updateThetaAndValueAgNormal_Normal same answer - single aggregate value", {
    updateThetaAndValueAgNormal_Normal <- demest:::updateThetaAndValueAgNormal_Normal
    initialModel <- demest:::initialModel
    ## no missing values
    was.updated <- FALSE  ## only test if was ever updated, since only one update done per iteration
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        aggregate <- AgNormal(value = 0.5, sd = 0.2)
        y <- rnorm(n = 20)
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        weights <- 5 * rbeta(n = 20, shape1 = 1, shape2= 1)
        weights <- Counts(array(weights, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Normal(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, weights = weights)
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgNormal_Normal(x0, y = y, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgNormal_Normal(x0, y = y, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptAg@.Data > 0L)
            was.updated <- TRUE
    }
    if (!was.updated)
        warning("theta was not updated")
    ## has missing values
    was.updated <- FALSE  ## only test if was ever updated, since only one update done per iteration
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        aggregate <- AgNormal(value = 0.5, sd = 0.2)
        y <- rnorm(n = 20)
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[6:10] <- NA
        weights <- 5 * rbeta(n = 20, shape1 = 1, shape2= 1)
        weights <- Counts(array(weights, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Normal(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, weights = weights)
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgNormal_Normal(x0, y = y, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgNormal_Normal(x0, y = y, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptAg@.Data > 0L)
            was.updated <- TRUE
    }
    if (!was.updated)
        warning("theta was not updated")
})

test_that("updateThetaAndValueAgNormal_Normal gives valid answer - multiple aggregate values", {
    updateThetaAndValueAgNormal_Normal <- demest:::updateThetaAndValueAgNormal_Normal
    initialModel <- demest:::initialModel
    ## no missing values
    was.updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgNormal(value = value, sd = sqrt(value), jump = 0.001)
        y <- rnorm(n = 20)
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        weights <- 5 * rbeta(n = 20, shape1 = 1, shape2= 1)
        weights <- Counts(array(weights, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Normal(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, weights = weights)
        x1 <- updateThetaAndValueAgNormal_Normal(x0, y = y)
        expect_true(validObject(x1))
        if (x1@nAcceptAg@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
    }
    if (!was.updated)
        warning("theta was not updated")
    ## has missing values
    was.updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgNormal(value = value, sd = sqrt(value), jump = 0.001)
        y <- rnorm(n = 20)
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[3:4] <- NA
        weights <- 5 * rbeta(n = 20, shape1 = 1, shape2= 1)
        weights <- Counts(array(weights, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Normal(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, weights = weights)
        x1 <- updateThetaAndValueAgNormal_Normal(x0, y = y)
        expect_true(validObject(x1))
        if (x1@nAcceptAg@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
    }
    if (!was.updated)
        warning("theta was not updated")
})

test_that("R and C versions of updateThetaAndValueAgNormal_Normal same answer - multiple aggregate values", {
    updateThetaAndValueAgNormal_Normal <- demest:::updateThetaAndValueAgNormal_Normal
    initialModel <- demest:::initialModel
    ## no missing values
    was.updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgNormal(value = value, sd = sqrt(value))
        y <- rnorm(n = 20)
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        weights <- 5 * rbeta(n = 20, shape1 = 1, shape2= 1)
        weights <- Counts(array(weights, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Normal(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, weights = weights)
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgNormal_Normal(x0, y = y, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgNormal_Normal(x0, y = y, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptAg@.Data > 0L)
            was.updated <- TRUE
    }
    if (!was.updated)
        warning("theta was not updated")
    ## has missing values
    was.updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgNormal(value = value, sd = sqrt(value))
        y <- rnorm(n = 20)
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1:5] <- NA
        weights <- 5 * rbeta(n = 20, shape1 = 1, shape2= 1)
        weights <- Counts(array(weights, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Normal(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, weights = weights)
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgNormal_Normal(x0, y = y, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgNormal_Normal(x0, y = y, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptAg@.Data > 0L)
            was.updated <- TRUE
    }
    if (!was.updated)
        warning("theta was not updated")
})











## updateThetaAndValueAgFun_Normal

test_that("updateThetaAndValueAgFun_Normal gives valid answer - single aggregate value", {
    updateThetaAndValueAgFun_Normal <- demest:::updateThetaAndValueAgFun_Normal
    initialModel <- demest:::initialModel
    ## no missing values
    was.updated <- FALSE ## only test if was ever updated, since only one update done per iteration
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        FUN <- function(x, weights) sum(x * sqrt(weights))
        aggregate <- AgFun(value = 0.5, sd = 0.01, FUN = FUN)
        theta <- rnorm(n = 20)
        weights <- Counts(array(runif(n = 20), dim = c(2, 10),
                                 dimnames = list(sex = c("f", "m"), age = 0:9)),
                           dimscales = c(age = "Intervals"))
        y <- Counts(array(rnorm(20), dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)),
                    dimscales = c(age = "Intervals"))
        spec <- Model(y ~ Normal(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, weights = weights)
        expect_is(x0, "NormalVaryingVarsigmaUnknownAgFun")
        x1 <- updateThetaAndValueAgFun_Normal(x0, y = y)
        expect_true(validObject(x1))
        if (x1@nAcceptTheta@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
    }
    if (!was.updated)
        warning("theta and bench not updated")
    ## has missing values
    was.updated <- FALSE ## only test if was ever updated, since only one update done per iteration
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        FUN <- function(x, weights) sum(x * sqrt(weights))
        aggregate <- AgFun(value = 0.5, sd = 0.01, FUN = FUN)
        theta <- rnorm(n = 20)
        weights <- Counts(array(runif(n = 20), dim = c(2, 10),
                                 dimnames = list(sex = c("f", "m"), age = 0:9)),
                           dimscales = c(age = "Intervals"))
        y <- Counts(array(rnorm(20), dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)),
                    dimscales = c(age = "Intervals"))
        y[1:5] <- NA
        spec <- Model(y ~ Normal(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, weights = weights)
        x1 <- updateThetaAndValueAgFun_Normal(x0, y = y)
        expect_true(validObject(x1))
        if (x1@nAcceptTheta@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
    }
    if (!was.updated)
        warning("theta and ag value not updated")
})

test_that("R and C versions of updateThetaAndValueAgFun_Normal same answer - single aggregate value", {
    updateThetaAndValueAgFun_Normal <- demest:::updateThetaAndValueAgFun_Normal
    initialModel <- demest:::initialModel
    ## no missing values
    was.updated <- FALSE  ## only test if was ever updated, since only one update done per iteration
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        FUN <- function(x, weights) sum(x * sqrt(weights))
        aggregate <- AgFun(value = 0.5, sd = 0.2, FUN = FUN)
        theta <- rnorm(n = 20)
        weights <- Counts(array(runif(n = 20), dim = c(2, 10),
                                 dimnames = list(sex = c("f", "m"), age = 0:9)),
                           dimscales = c(age = "Intervals"))
        y <- Counts(array(rnorm(20), dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)),
                    dimscales = c(age = "Intervals"))
        spec <- Model(y ~ Normal(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, weights = weights)
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgFun_Normal(x0, y = y, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgFun_Normal(x0, y = y, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptTheta@.Data > 0L)
            was.updated <- TRUE
    }
    if (!was.updated)
        warning("theta was not updated")
    ## has missing values
    was.updated <- FALSE  ## only test if was ever updated, since only one update done per iteration
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        FUN <- function(x, weights) sum(x * sqrt(weights))
        aggregate <- AgFun(value = 0.5, sd = 0.2, FUN = FUN)
        theta <- rnorm(n = 20)
        weights <- Counts(array(runif(n = 20), dim = c(2, 10),
                                 dimnames = list(sex = c("f", "m"), age = 0:9)),
                           dimscales = c(age = "Intervals"))
        y <- Counts(array(rnorm(20), dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)),
                    dimscales = c(age = "Intervals"))
        y[1:5] <- NA
        spec <- Model(y ~ Normal(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, weights = weights)
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgFun_Normal(x0, y = y, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgFun_Normal(x0, y = y, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptTheta@.Data > 0L)
            was.updated <- TRUE
    }
    if (!was.updated)
        warning("theta was not updated")
})

test_that("updateThetaAndValueAgFun_Normal gives valid answer - multiple aggregate values", {
    updateThetaAndValueAgFun_Normal <- demest:::updateThetaAndValueAgFun_Normal
    initialModel <- demest:::initialModel
    ## no missing values
    for (seed in seq_len(n.test)) {
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        FUN <- function(x, weights) sum(x * sqrt(weights))
        aggregate <- AgFun(value = value, sd = sqrt(value), FUN = FUN)
        theta <- rnorm(n = 20)
        weights <- Counts(array(runif(n = 20), dim = c(2, 10),
                                 dimnames = list(sex = c("f", "m"), age = 0:9)),
                           dimscales = c(age = "Intervals"))
        y <- Counts(array(rnorm(20), dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)),
                    dimscales = c(age = "Intervals"))
        spec <- Model(y ~ Normal(mean ~ age + sex), jump = 0.001, aggregate = aggregate)
        x0 <- initialModel(spec, y = y, weights = weights)
        x1 <- updateThetaAndValueAgFun_Normal(x0, y = y)
        expect_true(validObject(x1))
        if (x1@nAcceptTheta@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
        if (!was.updated)
            warning("theta was not updated")
    }
    ## has missing values
    for (seed in seq_len(n.test)) {
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        FUN <- function(x, weights) sum(x * sqrt(weights))
        aggregate <- AgFun(value = value, sd = sqrt(value), FUN = FUN)
        theta <- rnorm(n = 20)
        weights <- Counts(array(runif(n = 20), dim = c(2, 10),
                                 dimnames = list(sex = c("f", "m"), age = 0:9)),
                           dimscales = c(age = "Intervals"))
        y <- Counts(array(rnorm(20), dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)),
                    dimscales = c(age = "Intervals"))
        y[1:5] <- NA
        spec <- Model(y ~ Normal(mean ~ age + sex), jump = 0.001, aggregate = aggregate)
        x0 <- initialModel(spec, y = y, weights = weights)
        x1 <- updateThetaAndValueAgFun_Normal(x0, y = y)
        expect_true(validObject(x1))
        if (x1@nAcceptTheta@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
        if (!was.updated)
            warning("theta was not updated")
    }
})

test_that("R and C versions of updateThetaAndValueAgFun_Normal same answer - multiple aggregate values", {
    updateThetaAndValueAgFun_Normal <- demest:::updateThetaAndValueAgFun_Normal
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        ## no missing values
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)),
                        dimscales = c(age = "Intervals"))
        FUN <- function(x, weights) sum(x * sqrt(weights))
        aggregate <- AgFun(value = value, sd = sqrt(value), FUN = FUN)
        theta <- rnorm(n = 20)
        weights <- Counts(array(runif(n = 20), dim = c(2, 10),
                                 dimnames = list(sex = c("f", "m"), age = 0:9)),
                           dimscales = c(age = "Intervals"))
        y <- Counts(array(rnorm(20), dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)),
                    dimscales = c(age = "Intervals"))
        spec <- Model(y ~ Normal(mean ~ age + sex), jump = 0.1, aggregate = aggregate)
        x0 <- initialModel(spec, y = y, weights = weights)
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgFun_Normal(x0, y = y, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgFun_Normal(x0, y = y, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptTheta@.Data > 0L)
            was.updated <- TRUE
        if (!was.updated)
            warning("aggregate value was not updated")
        ## has missing values
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)),
                        dimscales = c(age = "Intervals"))
        FUN <- function(x, weights) sum(x * sqrt(weights))
        aggregate <- AgFun(value = value, sd = sqrt(value), FUN = FUN)
        theta <- rnorm(n = 20)
        weights <- Counts(array(runif(n = 20), dim = c(2, 10),
                                 dimnames = list(sex = c("f", "m"), age = 0:9)),
                           dimscales = c(age = "Intervals"))
        y <- Counts(array(rnorm(20), dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)),
                    dimscales = c(age = "Intervals"))
        spec <- Model(y ~ Normal(mean ~ age + sex), jump = 0.1, aggregate = aggregate)
        x0 <- initialModel(spec, y = y, weights = weights)
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgFun_Normal(x0, y = y, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgFun_Normal(x0, y = y, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptTheta@.Data > 0L)
            was.updated <- TRUE
        if (!was.updated)
            warning("aggregate value was not updated")
    }
})


## updateTheta_PoissonVaryingNotUseExp

test_that("updateTheta_PoissonVaryingNotUseExp gives valid answer", {
    updateTheta_PoissonVaryingNotUseExp <- demest:::updateTheta_PoissonVaryingNotUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        ## no missing values
        y <- Counts(array(as.integer(rpois(n = 20, lambda = 30)),
                          dim = c(2, 10),
                          dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ sex + age))
        model <- initialModel(spec, y = y, exposure = NULL)
        set.seed(seed + 1)
        ans.obtained <- updateTheta_PoissonVaryingNotUseExp(model, y = y)
        set.seed(seed + 1)
        ans.expected <- model
        mu <- (model@betas[[1]]
               + model@betas[[2]]
               + rep(model@betas[[3]], each = 2))
        for (i in seq_along(model@theta)) {
            theta.curr <- model@theta[i]
            theta.prop <- exp(rnorm(1, mean = log(theta.curr), sd = model@scaleTheta))
            log.diff <- dpois(y[i], lambda = theta.prop, log = TRUE) -
                dpois(y[i], lambda = theta.curr, log = TRUE) +
                    dnorm(x = log(theta.prop), mean = mu[i], sd = model@sigma, log = TRUE) -
                        dnorm(x = log(theta.curr), mean = mu[i], sd = model@sigma, log = TRUE)
            if ((log.diff >= 0) || (runif(1) < exp(log.diff))) {
                ans.expected@nAcceptTheta <- ans.expected@nAcceptTheta + 1L
                ans.expected@theta[i] <- theta.prop
            }
        }
        if (ans.expected@nAcceptTheta == 0L)
            warning("no proposals accepted")
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
        expect_identical(ans.obtained@betas, model@betas)
        expect_identical(ans.obtained@priorsBetas, model@priorsBetas)
        expect_identical(ans.obtained@sigma, model@sigma)
        expect_identical(ans.obtained@iteratorBetas, model@iteratorBetas)
        ## has missing values
        y <- Counts(array(as.integer(rpois(n = 20, lambda = 30)),
                          dim = c(2, 10),
                          dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1:5] <- NA
        spec <- Model(y ~ Poisson(mean ~ sex + age))
        model <- initialModel(spec, y = y, exposure = NULL)
        set.seed(seed + 1)
        ans.obtained <- updateTheta_PoissonVaryingNotUseExp(model, y = y)
        set.seed(seed + 1)
        ans.expected <- model
        mu <- (model@betas[[1]]
               + model@betas[[2]]
               + rep(model@betas[[3]], each = 2))
        ans.expected@theta[1:5] <- exp(rnorm(n = 5, mean = mu[1:5], sd = model@sigma))
        for (i in 6:20) {
            theta.curr <- model@theta[i]
            theta.prop <- exp(rnorm(1, mean = log(theta.curr), sd = model@scaleTheta))
            log.diff <- dpois(y[i], lambda = theta.prop, log = TRUE) -
                dpois(y[i], lambda = theta.curr, log = TRUE) +
                    dnorm(x = log(theta.prop), mean = mu[i], sd = model@sigma, log = TRUE) -
                        dnorm(x = log(theta.curr), mean = mu[i], sd = model@sigma, log = TRUE)
            if ((log.diff >= 0) || (runif(1) < exp(log.diff))) {
                ans.expected@nAcceptTheta <- ans.expected@nAcceptTheta + 1L
                ans.expected@theta[i] <- theta.prop
            }
        }
        if (ans.expected@nAcceptTheta == 0L)
            warning("no proposals accepted")
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
        expect_identical(ans.obtained@betas, model@betas)
        expect_identical(ans.obtained@priorsBetas, model@priorsBetas)
        expect_identical(ans.obtained@sigma, model@sigma)
        expect_identical(ans.obtained@iteratorBetas, model@iteratorBetas)
        ## has subtotals - whole subarray missing
        y <- Counts(array(as.integer(rpois(n = 20, lambda = 30)),
                          dim = c(2, 10),
                          dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1:12] <- NA
        subtotals <- Counts(array(30L, dim = 1, dimnames = list(age = "0-4")))
        y <- attachSubtotals(y, subtotals = subtotals)
        spec <- Model(y ~ Poisson(mean ~ sex + age))
        model <- initialModel(spec, y = y, exposure = NULL)
        set.seed(seed + 1)
        ans.obtained <- updateTheta_PoissonVaryingNotUseExp(model, y = y)
        set.seed(seed + 1)
        ans.expected <- model
        mu <- (model@betas[[1]]
               + model@betas[[2]]
               + rep(model@betas[[3]], each = 2))
        for (i in 1:10) {
            theta.curr <- ans.expected@theta[i]
            theta.prop <- exp(rnorm(1, mean = log(theta.curr), sd = model@scaleTheta))
            log.diff <- dpois(subtotals, lambda = sum(ans.expected@theta[1:10]) + theta.prop - theta.curr, log = TRUE) -
                dpois(subtotals, lambda = sum(ans.expected@theta[1:10]), log = TRUE) +
                    dnorm(x = log(theta.prop), mean = mu[i], sd = model@sigma, log = TRUE) -
                        dnorm(x = log(theta.curr), mean = mu[i], sd = model@sigma, log = TRUE)
            if ((log.diff >= 0) || (runif(1) < exp(log.diff))) {
                ans.expected@nAcceptTheta <- ans.expected@nAcceptTheta + 1L
                ans.expected@theta[i] <- theta.prop
            }
        }
        ans.expected@theta[11:12] <- exp(rnorm(n = 2, mean = mu[11:12], sd = model@sigma))
        for (i in 13:20) {
            theta.curr <- ans.expected@theta[i]
            theta.prop <- exp(rnorm(1, mean = log(theta.curr), sd = model@scaleTheta))
            log.diff <- dpois(y[i], lambda = theta.prop, log = TRUE) -
                dpois(y[i], lambda = theta.curr, log = TRUE) +
                    dnorm(x = log(theta.prop), mean = mu[i], sd = model@sigma, log = TRUE) -
                        dnorm(x = log(theta.curr), mean = mu[i], sd = model@sigma, log = TRUE)
            if ((log.diff >= 0) || (runif(1) < exp(log.diff))) {
                ans.expected@nAcceptTheta <- ans.expected@nAcceptTheta + 1L
                ans.expected@theta[i] <- theta.prop
            }
        }
        if (ans.expected@nAcceptTheta == 0L)
            warning("no proposals accepted")
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
        expect_identical(ans.obtained@betas, model@betas)
        expect_identical(ans.obtained@priorsBetas, model@priorsBetas)
        expect_identical(ans.obtained@sigma, model@sigma)
        expect_identical(ans.obtained@iteratorBetas, model@iteratorBetas)
        ## has subtotals - part subarray missing
        y <- Counts(array(as.integer(rpois(n = 20, lambda = 30)),
                          dim = c(2, 10),
                          dimnames = list(sex = c("f", "m"), age = 0:9)),
                    dimscales = c(age = "Intervals"))
        subtotals <- subarray(y, age < 5)
        subtotals <- collapseIntervals(subtotals,
                                       dimension = "age",
                                       breaks = c(0, 5))
        subtotals <- collapseDimension(subtotals,
                                       dimension = "sex")
        y[c(1:8, 11:12)] <- NA
        y <- attachSubtotals(y, subtotals = subtotals)
        spec <- Model(y ~ Poisson(mean ~ sex + age))
        model <- initialModel(spec, y = y, exposure = NULL)
        set.seed(seed + 1)
        ans.obtained <- updateTheta_PoissonVaryingNotUseExp(model, y = y)
        set.seed(seed + 1)
        ans.expected <- model
        mu <- (model@betas[[1]]
               + model@betas[[2]]
               + rep(model@betas[[3]], each = 2))
        for (i in 1:8) {
            theta.curr <- ans.expected@theta[i]
            theta.prop <- exp(rnorm(1, mean = log(theta.curr), sd = model@scaleTheta))
            log.diff <- dpois(y@subtotalsNet, lambda = sum(ans.expected@theta[1:8]) + theta.prop - theta.curr, log = TRUE) -
                dpois(y@subtotalsNet, lambda = sum(ans.expected@theta[1:8]), log = TRUE) +
                    dnorm(x = log(theta.prop), mean = mu[i], sd = model@sigma, log = TRUE) -
                        dnorm(x = log(theta.curr), mean = mu[i], sd = model@sigma, log = TRUE)
            if ((log.diff >= 0) || (runif(1) < exp(log.diff))) {
                ans.expected@nAcceptTheta <- ans.expected@nAcceptTheta + 1L
                ans.expected@theta[i] <- theta.prop
            }
        }
        for (i in 9:10) {
            theta.curr <- ans.expected@theta[i]
            theta.prop <- exp(rnorm(1, mean = log(theta.curr), sd = model@scaleTheta))
            log.diff <- dpois(y[i], lambda = theta.prop, log = TRUE) -
                dpois(y[i], lambda = theta.curr, log = TRUE) +
                    dnorm(x = log(theta.prop), mean = mu[i], sd = model@sigma, log = TRUE) -
                        dnorm(x = log(theta.curr), mean = mu[i], sd = model@sigma, log = TRUE)
            if ((log.diff >= 0) || (runif(1) < exp(log.diff))) {
                ans.expected@nAcceptTheta <- ans.expected@nAcceptTheta + 1L
                ans.expected@theta[i] <- theta.prop
            }
        }
        ans.expected@theta[11:12] <- exp(rnorm(n = 2, mean = mu[11:12], sd = model@sigma))
        for (i in 13:20) {
            theta.curr <- ans.expected@theta[i]
            theta.prop <- exp(rnorm(1, mean = log(theta.curr), sd = model@scaleTheta))
            log.diff <- dpois(y[i], lambda = theta.prop, log = TRUE) -
                dpois(y[i], lambda = theta.curr, log = TRUE) +
                    dnorm(x = log(theta.prop), mean = mu[i], sd = model@sigma, log = TRUE) -
                        dnorm(x = log(theta.curr), mean = mu[i], sd = model@sigma, log = TRUE)
            if ((log.diff >= 0) || (runif(1) < exp(log.diff))) {
                ans.expected@nAcceptTheta <- ans.expected@nAcceptTheta + 1L
                ans.expected@theta[i] <- theta.prop
            }
        }
        if (ans.expected@nAcceptTheta == 0L)
            warning("no proposals accepted")
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
        expect_identical(ans.obtained@betas, model@betas)
        expect_identical(ans.obtained@priorsBetas, model@priorsBetas)
        expect_identical(ans.obtained@sigma, model@sigma)
        expect_identical(ans.obtained@iteratorBetas, model@iteratorBetas)
        ## has lower, upper
        y <- Counts(array(as.integer(rpois(n = 20, lambda = 30)),
                          dim = c(2, 10),
                          dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1:12] <- NA
        subtotals <- Counts(array(30L, dim = 1, dimnames = list(age = "0-4")))
        y <- attachSubtotals(y, subtotals = subtotals)
        spec <- Model(y ~ Poisson(mean ~ sex + age), lower = 10, upper = 30)
        model <- initialModel(spec, y = y, exposure = NULL)
        set.seed(seed + 1)
        ans.obtained <- updateTheta_PoissonVaryingNotUseExp(model, y = y)
        expect_true(all((ans.obtained@theta > 10) & ans.obtained@theta < 30))
    }
})

test_that("R and C versions of updateTheta_PoissonVaryingNotUseExp give same answer", {
    updateTheta_PoissonVaryingNotUseExp <- demest:::updateTheta_PoissonVaryingNotUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        ## no missing values
        y <- Counts(array(as.integer(rpois(n = 20, lambda = 30)),
                          dim = c(2, 10),
                          dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ sex + age))
        model <- initialModel(spec, y = y, exposure = NULL)
        set.seed(seed + 1)
        ans.R <- updateTheta_PoissonVaryingNotUseExp(model, y = y, useC = FALSE)
        set.seed(seed + 1)
        ans.C <- updateTheta_PoissonVaryingNotUseExp(model, y = y, useC = TRUE)
        if (ans.R@nAcceptTheta == 0L)
            warning("no proposals accepted")
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
        ## has missing values
        y <- Counts(array(as.integer(rpois(n = 20, lambda = 30)),
                          dim = c(2, 10),
                          dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1:5] <- NA
        spec <- Model(y ~ Poisson(mean ~ sex + age))
        model <- initialModel(spec, y = y, exposure = NULL)
        set.seed(seed + 1)
        ans.R <- updateTheta_PoissonVaryingNotUseExp(model, y = y, useC = FALSE)
        set.seed(seed + 1)
        ans.C <- updateTheta_PoissonVaryingNotUseExp(model, y = y, useC = TRUE)
        if (ans.R@nAcceptTheta == 0L)
            warning("no proposals accepted")
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
        ## has subtotals - whole subarray missing
        y <- Counts(array(as.integer(rpois(n = 20, lambda = 30)),
                          dim = c(2, 10),
                          dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1:12] <- NA
        subtotals <- Counts(array(30L, dim = 1, dimnames = list(age = "0-4")))
        y <- attachSubtotals(y, subtotals = subtotals)
        spec <- Model(y ~ Poisson(mean ~ sex + age))
        model <- initialModel(spec, y = y, exposure = NULL)
        set.seed(seed + 1)
        ans.R <- updateTheta_PoissonVaryingNotUseExp(model, y = y, useC = FALSE)
        set.seed(seed + 1)
        ans.C <- updateTheta_PoissonVaryingNotUseExp(model, y = y, useC = TRUE)
        if (ans.R@nAcceptTheta == 0L)
            warning("no proposals accepted")
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
        ## has subtotals - part subarray missing
        y <- Counts(array(as.integer(rpois(n = 20, lambda = 30)),
                          dim = c(2, 10),
                          dimnames = list(sex = c("f", "m"), age = 0:9)),
                    dimscales = c(age = "Intervals"))
        subtotals <- subarray(y, age < 5)
        subtotals <- collapseIntervals(subtotals,
                                       dimension = "age",
                                       breaks = c(0, 5))
        subtotals <- collapseDimension(subtotals,
                                       dimension = "sex")
        y[c(1:8, 11:12)] <- NA
        y <- attachSubtotals(y, subtotals = subtotals)
        spec <- Model(y ~ Poisson(mean ~ sex + age))
        model <- initialModel(spec, y = y, exposure = NULL)
        set.seed(seed + 1)
        ans.R <- updateTheta_PoissonVaryingNotUseExp(model, y = y, useC = FALSE)
        set.seed(seed + 1)
        ans.C <- updateTheta_PoissonVaryingNotUseExp(model, y = y, useC = TRUE)
        if (ans.R@nAcceptTheta == 0L)
            warning("no proposals accepted")
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
        ## has lower, upper
        y <- Counts(array(as.integer(rpois(n = 20, lambda = 30)),
                          dim = c(2, 10),
                          dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1:12] <- NA
        subtotals <- Counts(array(30L, dim = 1, dimnames = list(age = "0-4")))
        y <- attachSubtotals(y, subtotals = subtotals)
        spec <- Model(y ~ Poisson(mean ~ sex + age), lower = 10, upper = 30)
        model <- initialModel(spec, y = y, exposure = NULL)
        set.seed(seed + 1)
        ans.R <- updateTheta_PoissonVaryingNotUseExp(model, y = y, useC = FALSE)
        set.seed(seed + 1)
        ans.C <- updateTheta_PoissonVaryingNotUseExp(model, y = y, useC = TRUE)
        if (ans.R@nAcceptTheta == 0L)
            warning("no proposals accepted")
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})


## updateTheta_PoissonVaryingUseExp

test_that("updateTheta_PoissonVaryingUseExp gives valid answer", {
    updateTheta_PoissonVaryingUseExp <- demest:::updateTheta_PoissonVaryingUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        ## no missing values
        exposure <- Counts(array(10 * rbeta(n = 20, shape1 = 20, shape2 = 5),
                                 dim = c(5, 4),
                                 dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        y <- Counts(array(as.integer(rpois(n = 20, lambda = 0.5 * exposure)),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        spec <- Model(y ~ Poisson(mean ~ age + region))
        model <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        ans.obtained <- updateTheta_PoissonVaryingUseExp(model, y = y, exposure = exposure)
        set.seed(seed + 1)
        ans.expected <- model
        mu <- (model@betas[[1]]
               + model@betas[[2]]
               + rep(model@betas[[3]], each = 5))
        for (i in seq_along(ans.expected@theta)) {
            theta.curr <- ans.expected@theta[i]
            theta.prop <- exp(rnorm(1, mean = log(theta.curr),
                                    sd = model@scaleTheta*model@scaleThetaMultiplier/sqrt(1+log(1+exposure[i]))))
            log.diff <- dpois(y[i], lambda = theta.prop * exposure[i], log = TRUE) -
                dpois(y[i], lambda = theta.curr * exposure[i], log = TRUE) +
                    dnorm(x = log(theta.prop), mean = mu[i], sd = model@sigma, log = TRUE) -
                        dnorm(x = log(theta.curr), mean = mu[i], sd = model@sigma, log = TRUE)
            if ((log.diff >= 0) || (runif(1) < exp(log.diff))) {
                ans.expected@nAcceptTheta <- ans.expected@nAcceptTheta + 1L
                ans.expected@theta[i] <- theta.prop
            }
        }
        if (ans.expected@nAcceptTheta == 0L)
            warning("no proposals accepted")
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
        expect_identical(ans.obtained@betas, model@betas)
        expect_identical(ans.obtained@priorsBetas, model@priorsBetas)
        expect_identical(ans.obtained@sigma, model@sigma)
        expect_identical(ans.obtained@iteratorBetas, model@iteratorBetas)
        ## has missing values
        exposure <- Counts(array(10 * rbeta(n = 20, shape1 = 20, shape2 = 5),
                                 dim = c(5, 4),
                                 dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        y <- Counts(array(as.integer(rpois(n = 20, lambda = 0.5 * exposure)),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        y[1:5] <- NA
        spec <- Model(y ~ Poisson(mean ~ age + region))
        model <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        ans.obtained <- updateTheta_PoissonVaryingUseExp(model, y = y, exposure = exposure)
        set.seed(seed + 1)
        ans.expected <- model
        mu <- (model@betas[[1]]
               + model@betas[[2]]
               + rep(model@betas[[3]], each = 5))
        ans.expected@theta[1:5] <- exp(rnorm(n = 5, mean = mu[1:5], sd = model@sigma))
        for (i in 6:20) {
            theta.curr <- ans.expected@theta[i]
            theta.prop <- exp(rnorm(1, mean = log(theta.curr),
                                    sd = model@scaleTheta*model@scaleThetaMultiplier/sqrt(1+log(1+exposure[i]))))
            log.diff <- dpois(y[i], lambda = theta.prop * exposure[i], log = TRUE) -
                dpois(y[i], lambda = theta.curr * exposure[i], log = TRUE) +
                    dnorm(x = log(theta.prop), mean = mu[i], sd = model@sigma, log = TRUE) -
                        dnorm(x = log(theta.curr), mean = mu[i], sd = model@sigma, log = TRUE)
            if ((log.diff >= 0) || (runif(1) < exp(log.diff))) {
                ans.expected@nAcceptTheta <- ans.expected@nAcceptTheta + 1L
                ans.expected@theta[i] <- theta.prop
            }
        }
        if (ans.expected@nAcceptTheta == 0L)
            warning("no proposals accepted")
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
        expect_identical(ans.obtained@betas, model@betas)
        expect_identical(ans.obtained@priorsBetas, model@priorsBetas)
        expect_identical(ans.obtained@sigma, model@sigma)
        expect_identical(ans.obtained@iteratorBetas, model@iteratorBetas)
        ## has subtotals - whole array missing
        exposure <- Counts(array(10 * rbeta(n = 20, shape1 = 20, shape2 = 5),
                                 dim = c(5, 4),
                                 dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        y <- Counts(array(as.integer(rpois(n = 20, lambda = 0.5 * exposure)),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        y[1:12] <- NA
        subtotals <- Counts(array(30:31, dim = 2, dimnames = list(region = c("a", "b"))))
        y <- attachSubtotals(y, subtotals = subtotals)
        spec <- Model(y ~ Poisson(mean ~ age + region))
        model <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        ans.obtained <- updateTheta_PoissonVaryingUseExp(model, y = y, exposure = exposure)
        set.seed(seed + 1)
        ans.expected <- model
        mu <- (model@betas[[1]]
               + model@betas[[2]]
               + rep(model@betas[[3]], each = 5))
        for (i in 1:10) {
            theta.curr <- ans.expected@theta[i]
            theta.prop <- exp(rnorm(1, mean = log(theta.curr),
                                    sd = model@scaleTheta*model@scaleThetaMultiplier/sqrt(1+log(1+exposure[i]))))
            j <- if (i <= 5) 1 else 2
            ind <- (1:5) + (j - 1) * 5
            log.diff <- dpois(subtotals[j],
                              lambda = sum(ans.expected@theta[ind] * exposure[ind]) + (theta.prop - theta.curr) * exposure[i],
                              log = TRUE) -
                dpois(subtotals[j], lambda = sum(ans.expected@theta[ind] * exposure[ind]), log = TRUE) +
                    dnorm(x = log(theta.prop), mean = mu[i], sd = ans.expected@sigma, log = TRUE) -
                        dnorm(x = log(theta.curr), mean = mu[i], sd = ans.expected@sigma, log = TRUE)
            if ((log.diff >= 0) || (runif(1) < exp(log.diff))) {
                ans.expected@nAcceptTheta <- ans.expected@nAcceptTheta + 1L
                ans.expected@theta[i] <- theta.prop
            }
        }
        ans.expected@theta[11:12] <- exp(rnorm(n = 2, mean = mu[11:12], sd = ans.expected@sigma))
        for (i in 13:20) {
            theta.curr <- ans.expected@theta[i]
            theta.prop <- exp(rnorm(1, mean = log(theta.curr),
                                    sd = ans.expected@scaleTheta*model@scaleThetaMultiplier/sqrt(1+log(1+exposure[i]))))
            log.diff <- dpois(y[i], lambda = theta.prop * exposure[i], log = TRUE) -
                dpois(y[i], lambda = theta.curr * exposure[i], log = TRUE) +
                    dnorm(x = log(theta.prop), mean = mu[i], sd = ans.expected@sigma, log = TRUE) -
                        dnorm(x = log(theta.curr), mean = mu[i], sd = ans.expected@sigma, log = TRUE)
            if ((log.diff >= 0) || (runif(1) < exp(log.diff))) {
                ans.expected@nAcceptTheta <- ans.expected@nAcceptTheta + 1L
                ans.expected@theta[i] <- theta.prop
            }
        }
        if (ans.expected@nAcceptTheta == 0L)
            warning("no proposals accepted")
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
        expect_identical(ans.obtained@betas, ans.expected@betas)
        expect_identical(ans.obtained@priorsBetas, ans.expected@priorsBetas)
        expect_identical(ans.obtained@sigma, ans.expected@sigma)
        expect_identical(ans.obtained@iteratorBetas, ans.expected@iteratorBetas)
        ## has subtotals - part subarray missing
        exposure <- Counts(array(10 * rbeta(n = 20, shape1 = 20, shape2 = 5),
                                 dim = c(5, 4),
                                 dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        y <- Counts(array(as.integer(rpois(n = 20, lambda = 0.5 * exposure)),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        y[c(1:8, 11:12)] <- NA
        subtotals <- Counts(array(30:31, dim = 2, dimnames = list(region = c("a", "b"))))
        y <- attachSubtotals(y, subtotals = subtotals)
        spec <- Model(y ~ Poisson(mean ~ age + region))
        model <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        ans.obtained <- updateTheta_PoissonVaryingUseExp(model, y = y, exposure = exposure)
        set.seed(seed + 1)
        ans.expected <- model
        mu <- (model@betas[[1]]
               + model@betas[[2]]
               + rep(model@betas[[3]], each = 5))
        for (i in 1:8) {
            theta.curr <- ans.expected@theta[i]
            theta.prop <- exp(rnorm(1, mean = log(theta.curr),
                                    sd = model@scaleTheta*model@scaleThetaMultiplier/sqrt(1+log(1+exposure[i]))))
            if (i <= 5) {
                j <- 1
                ind <- 1:5
            }
            else {
                j <- 2
                ind <- 6:8
            }
            log.diff <- dpois(y@subtotalsNet[j],
                              lambda = sum(ans.expected@theta[ind] * exposure[ind]) + (theta.prop - theta.curr) * exposure[i],
                              log = TRUE) -
                dpois(y@subtotalsNet[j], lambda = sum(ans.expected@theta[ind] * exposure[ind]), log = TRUE) +
                    dnorm(x = log(theta.prop), mean = mu[i], sd = ans.expected@sigma, log = TRUE) -
                        dnorm(x = log(theta.curr), mean = mu[i], sd = ans.expected@sigma, log = TRUE)
            if ((log.diff >= 0) || (runif(1) < exp(log.diff))) {
                ans.expected@nAcceptTheta <- ans.expected@nAcceptTheta + 1L
                ans.expected@theta[i] <- theta.prop
            }
        }
        for (i in 9:10) {
            theta.curr <- ans.expected@theta[i]
            theta.prop <- exp(rnorm(1, mean = log(theta.curr),
                                    sd = ans.expected@scaleTheta*model@scaleThetaMultiplier/sqrt(1+log(1+exposure[i]))))
            log.diff <- dpois(y[i], lambda = theta.prop * exposure[i], log = TRUE) -
                dpois(y[i], lambda = theta.curr * exposure[i], log = TRUE) +
                    dnorm(x = log(theta.prop), mean = mu[i], sd = ans.expected@sigma, log = TRUE) -
                        dnorm(x = log(theta.curr), mean = mu[i], sd = ans.expected@sigma, log = TRUE)
            if ((log.diff >= 0) || (runif(1) < exp(log.diff))) {
                ans.expected@nAcceptTheta <- ans.expected@nAcceptTheta + 1L
                ans.expected@theta[i] <- theta.prop
            }
        }
        ans.expected@theta[11:12] <- exp(rnorm(n = 2, mean = mu[11:12], sd = ans.expected@sigma))
        for (i in 13:20) {
            theta.curr <- ans.expected@theta[i]
            theta.prop <- exp(rnorm(1, mean = log(theta.curr),
                                    sd = ans.expected@scaleTheta*model@scaleThetaMultiplier/sqrt(1+log(1+exposure[i]))))
            log.diff <- dpois(y[i], lambda = theta.prop * exposure[i], log = TRUE) -
                dpois(y[i], lambda = theta.curr * exposure[i], log = TRUE) +
                    dnorm(x = log(theta.prop), mean = mu[i], sd = ans.expected@sigma, log = TRUE) -
                        dnorm(x = log(theta.curr), mean = mu[i], sd = ans.expected@sigma, log = TRUE)
            if ((log.diff >= 0) || (runif(1) < exp(log.diff))) {
                ans.expected@nAcceptTheta <- ans.expected@nAcceptTheta + 1L
                ans.expected@theta[i] <- theta.prop
            }
        }
        if (ans.expected@nAcceptTheta == 0L)
            warning("no proposals accepted")
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
        expect_identical(ans.obtained@betas, ans.expected@betas)
        expect_identical(ans.obtained@priorsBetas, ans.expected@priorsBetas)
        expect_identical(ans.obtained@sigma, ans.expected@sigma)
        expect_identical(ans.obtained@iteratorBetas, ans.expected@iteratorBetas)
        ## has lower, upper
        exposure <- Counts(array(10 * rbeta(n = 20, shape1 = 20, shape2 = 5),
                                 dim = c(5, 4),
                                 dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        y <- Counts(array(as.integer(rpois(n = 20, lambda = 0.5 * exposure)),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        y[1:12] <- NA
        subtotals <- Counts(array(30:31, dim = 2, dimnames = list(region = c("a", "b"))))
        y <- attachSubtotals(y, subtotals = subtotals)
        spec <- Model(y ~ Poisson(mean ~ age + region), lower = 0.3, upper = 0.6)
        model <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        ans.obtained <- updateTheta_PoissonVaryingUseExp(model, y = y, exposure = exposure)
        expect_true(all((ans.obtained@theta > 0.3) & ans.obtained@theta < 0.6))
    }
})

test_that("R and C versions of updateTheta_PoissonVaryingUseExp give same answer", {
    updateTheta_PoissonVaryingUseExp <- demest:::updateTheta_PoissonVaryingUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        ## no missing values
        exposure <- Counts(array(10 * rbeta(n = 20, shape1 = 20, shape2 = 5),
                                 dim = c(5, 4),
                                 dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        y <- Counts(array(as.integer(rpois(n = 20, lambda = 0.5 * exposure)),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        spec <- Model(y ~ Poisson(mean ~ age + region))
        model <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        ans.R <- updateTheta_PoissonVaryingUseExp(model, y = y, exposure = exposure, useC = FALSE)
        set.seed(seed + 1)
        ans.C <- updateTheta_PoissonVaryingUseExp(model, y = y, exposure = exposure, useC = TRUE)
        if (ans.R@nAcceptTheta == 0L)
            warning("no proposals accepted")
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
        ## has missing values
        exposure <- Counts(array(10 * rbeta(n = 20, shape1 = 20, shape2 = 5),
                                 dim = c(5, 4),
                                 dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        y <- Counts(array(as.integer(rpois(n = 20, lambda = 0.5 * exposure)),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        y[1:5] <- NA
        spec <- Model(y ~ Poisson(mean ~ age + region))
        model <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        ans.R <- updateTheta_PoissonVaryingUseExp(model, y = y, exposure = exposure, useC = FALSE)
        set.seed(seed + 1)
        ans.C <- updateTheta_PoissonVaryingUseExp(model, y = y, exposure = exposure, useC = TRUE)
        if (ans.R@nAcceptTheta == 0L)
            warning("no proposals accepted")
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
        ## has subtotals - whole subarray missing
        exposure <- Counts(array(10 * rbeta(n = 20, shape1 = 20, shape2 = 5),
                                 dim = c(5, 4),
                                 dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        y <- Counts(array(as.integer(rpois(n = 20, lambda = 0.5 * exposure)),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        y[1:12] <- NA
        subtotals <- Counts(array(30:31, dim = 2, dimnames = list(region = c("a", "b"))))
        y <- attachSubtotals(y, subtotals = subtotals)
        spec <- Model(y ~ Poisson(mean ~ age + region))
        model <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        ans.R <- updateTheta_PoissonVaryingUseExp(model, y = y, exposure = exposure, useC = FALSE)
        set.seed(seed + 1)
        ans.C <- updateTheta_PoissonVaryingUseExp(model, y = y, exposure = exposure, useC = TRUE)
        if (ans.R@nAcceptTheta == 0L)
            warning("no proposals accepted")
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
        ## has subtotals - part subarray missing
        exposure <- Counts(array(10 * rbeta(n = 20, shape1 = 20, shape2 = 5),
                                 dim = c(5, 4),
                                 dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        y <- Counts(array(as.integer(rpois(n = 20, lambda = 0.5 * exposure)),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        y[c(1:8, 11:12)] <- NA
        subtotals <- Counts(array(30:31, dim = 2, dimnames = list(region = c("a", "b"))))
        y <- attachSubtotals(y, subtotals = subtotals)
        spec <- Model(y ~ Poisson(mean ~ age + region))
        model <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        ans.R <- updateTheta_PoissonVaryingUseExp(model, y = y, exposure = exposure, useC = FALSE)
        set.seed(seed + 1)
        ans.C <- updateTheta_PoissonVaryingUseExp(model, y = y, exposure = exposure, useC = TRUE)
        if (ans.R@nAcceptTheta == 0L)
            warning("no proposals accepted")
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
        ## has lower, upper
        exposure <- Counts(array(10 * rbeta(n = 20, shape1 = 20, shape2 = 5),
                                 dim = c(5, 4),
                                 dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        y <- Counts(array(as.integer(rpois(n = 20, lambda = 0.5 * exposure)),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        y[1:12] <- NA
        subtotals <- Counts(array(30:31, dim = 2, dimnames = list(region = c("a", "b"))))
        y <- attachSubtotals(y, subtotals = subtotals)
        spec <- Model(y ~ Poisson(mean ~ age + region), lower = 0.3, upper = 0.6)
        model <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        ans.R <- updateTheta_PoissonVaryingUseExp(model, y = y, exposure = exposure, useC = FALSE)
        set.seed(seed + 1)
        ans.C <- updateTheta_PoissonVaryingUseExp(model, y = y, exposure = exposure, useC = TRUE)
        if (ans.R@nAcceptTheta == 0L)
            warning("no proposals accepted")
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})


## updateTheta_PoissonVaryingNotUseExpAgCertain

test_that("updateTheta_PoissonVaryingNotUseExpAgCertain gives valid answer - single aggregate value", {
    updateTheta_PoissonVaryingNotUseExpAgCertain <- demest:::updateTheta_PoissonVaryingNotUseExpAgCertain
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        ## no missing values
        was.updated <- FALSE
        set.seed(seed)
        aggregate <- AgCertain(value = 400)
        y <- as.integer(rpois(n = 20, lambda = 20))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = NULL)
        x1 <- updateTheta_PoissonVaryingNotUseExpAgCertain(x0, y = y)
        expect_true(validObject(x1))
        if (x1@nAcceptTheta > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
        if (!was.updated)
            warning("theta was not updated")
        ## has missing values
        was.updated <- FALSE
        set.seed(seed)
        aggregate <- AgCertain(value = 400)
        y <- as.integer(rpois(n = 20, lambda = 20))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1:5] <- NA
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = NULL)
        x1 <- updateTheta_PoissonVaryingNotUseExpAgCertain(x0, y = y)
        expect_true(validObject(x1))
        if (x1@nAcceptTheta > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
        if (!was.updated)
            warning("theta was not updated")
    }
})

test_that("R and C versions of updateTheta_PoissonVaryingNotUseExpAgCertain same answer - single aggregate value", {
    updateTheta_PoissonVaryingNotUseExpAgCertain <- demest:::updateTheta_PoissonVaryingNotUseExpAgCertain
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        ## no missing values
        was.updated <- FALSE
        set.seed(seed)
        aggregate <- AgCertain(value = 400)
        y <- as.integer(rpois(n = 20, lambda = 20))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = NULL)
        set.seed(seed + 1)
        x.R <- updateTheta_PoissonVaryingNotUseExpAgCertain(x0, y = y, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateTheta_PoissonVaryingNotUseExpAgCertain(x0, y = y, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptTheta@.Data > 0L)
            was.updated <- TRUE
        if (!was.updated)
            warning("theta was not updated")
        ## has missing values
        was.updated <- FALSE
        set.seed(seed)
        aggregate <- AgCertain(value = 400)
        y <- as.integer(rpois(n = 20, lambda = 20))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1:5] <- NA
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = NULL)
        set.seed(seed + 1)
        x.R <- updateTheta_PoissonVaryingNotUseExpAgCertain(x0, y = y, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateTheta_PoissonVaryingNotUseExpAgCertain(x0, y = y, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptTheta@.Data > 0L)
            was.updated <- TRUE
        if (!was.updated)
            warning("theta was not updated")
    }
})

test_that("updateTheta_PoissonVaryingNotUseExpAgCertain gives valid answer - multiple aggregate values", {
    updateTheta_PoissonVaryingNotUseExpAgCertain <- demest:::updateTheta_PoissonVaryingNotUseExpAgCertain
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgNormal(value = value, sd = sqrt(value))
        y <- as.integer(rpois(n = 20, lambda = 20))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1:3] <- NA
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = NULL)
        x1 <- updateTheta_PoissonVaryingNotUseExpAgCertain(x0, y = y)
        expect_true(validObject(x1))
        if (x1@nAcceptTheta > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
        if (!was.updated)
            warning("theta was not updated")
    }
})

test_that("R and C versions of updateTheta_PoissonVaryingNotUseExpAgCertain same answer - multiple aggregate values", {
    updateTheta_PoissonVaryingNotUseExpAgCertain <- demest:::updateTheta_PoissonVaryingNotUseExpAgCertain
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgNormal(value = value, sd = sqrt(value))
        y <- as.integer(rpois(n = 20, lambda = 20))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1:3] <- NA
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = NULL)
        set.seed(seed + 1)
        x.R <- updateTheta_PoissonVaryingNotUseExpAgCertain(x0, y = y, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateTheta_PoissonVaryingNotUseExpAgCertain(x0, y = y, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptTheta@.Data > 0L)
            was.updated <- TRUE
        if (!was.updated)
            warning("theta was not updated")
    }
})

test_that("updateTheta_PoissonVaryingNotUseExpAgCertain gives valid answer - some benchmarks weights = 0", {
    updateTheta_PoissonVaryingNotUseExpAgCertain <- demest:::updateTheta_PoissonVaryingNotUseExpAgCertain
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        w <- Counts(array(rpois(n = 20, lambda = 4),
                          dim = c(2, 10),
                          dimnames = list(sex = c("f", "m"), age = 0:9)))
        w[sample(20, size = 8)] <- 0
        aggregate <- AgNormal(value = value, sd = sqrt(value), weights = w)
        y <- as.integer(rpois(n = 20, lambda = 20))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1:3] <- NA
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = NULL)
        x1 <- updateTheta_PoissonVaryingNotUseExpAgCertain(x0, y = y)
        expect_true(validObject(x1))
        if (x1@nAcceptTheta > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
        if (!was.updated)
            warning("theta was not updated")
    }
})

test_that("R and C versions of updateTheta_PoissonVaryingNotUseExpAgCertain same answer - some aggregate weights = 0", {
    updateTheta_PoissonVaryingNotUseExpAgCertain <- demest:::updateTheta_PoissonVaryingNotUseExpAgCertain
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        w <- Counts(array(rpois(n = 20, lambda = 4),
                          dim = c(2, 10),
                          dimnames = list(sex = c("f", "m"), age = 0:9)))
        w[sample(20, size = 8)] <- 0
        aggregate <- AgNormal(value = value, sd = sqrt(value), weights = w)
        y <- as.integer(rpois(n = 20, lambda = 20))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1:3] <- NA
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = NULL)
        set.seed(seed + 1)
        x.R <- updateTheta_PoissonVaryingNotUseExpAgCertain(x0, y = y, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateTheta_PoissonVaryingNotUseExpAgCertain(x0, y = y, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptTheta@.Data > 0L)
            was.updated <- TRUE
        if (!was.updated)
            warning("theta was not updated")
    }
})


## updateTheta_PoissonVaryingUseExpAgCertain 

test_that("updateTheta_PoissonVaryingUseExpAgCertain gives valid answer - single aggregate value", {
    updateTheta_PoissonVaryingUseExpAgCertain <- demest:::updateTheta_PoissonVaryingUseExpAgCertain
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        ## no missing values
        was.updated <- FALSE
        set.seed(seed)
        aggregate <- AgCertain(value = 2)
        theta <- 5 * rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.double(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rpois(n = 20, lambda = theta * exposure))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        x1 <- updateTheta_PoissonVaryingUseExpAgCertain(x0, y = y, exposure = exposure)
        expect_true(validObject(x1))
        if (x1@nAcceptTheta > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
        if (!was.updated)
            warning("theta was not updated")
        ## has missing values
        was.updated <- FALSE
        set.seed(seed)
        aggregate <- AgCertain(value = 2)
        theta <- 5 * rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.double(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rpois(n = 20, lambda = theta * exposure))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1:5] <- NA
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        x1 <- updateTheta_PoissonVaryingUseExpAgCertain(x0, y = y, exposure = exposure)
        expect_true(validObject(x1))
        if (x1@nAcceptTheta > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
        if (!was.updated)
            warning("theta was not updated")
    }
})

test_that("R and C versions of updateTheta_PoissonVaryingUseExpAgCertain same answer - single aggregate value", {
    updateTheta_PoissonVaryingUseExpAgCertain <- demest:::updateTheta_PoissonVaryingUseExpAgCertain
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        ## no missing values
        was.updated <- FALSE
        set.seed(seed)
        aggregate <- AgCertain(value = 0.5)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.double(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rpois(n = 20, lambda = exposure * theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        x.R <- updateTheta_PoissonVaryingUseExpAgCertain(x0, y = y, exposure = exposure, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateTheta_PoissonVaryingUseExpAgCertain(x0, y = y, exposure = exposure, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptTheta@.Data > 0L)
            was.updated <- TRUE
        if (!was.updated)
            warning("theta was not updated")
        ## has missing values
        was.updated <- FALSE
        set.seed(seed)
        aggregate <- AgCertain(value = 0.5)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.double(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rpois(n = 20, lambda = exposure * theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1:5] <- NA
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        x.R <- updateTheta_PoissonVaryingUseExpAgCertain(x0, y = y, exposure = exposure, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateTheta_PoissonVaryingUseExpAgCertain(x0, y = y, exposure = exposure, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptTheta@.Data > 0L)
            was.updated <- TRUE
        if (!was.updated)
            warning("theta was not updated")
    }
})

test_that("updateTheta_PoissonVaryingUseExpAgCertain gives valid answer - multiple aggregate values", {
    updateTheta_PoissonVaryingUseExpAgCertain <- demest:::updateTheta_PoissonVaryingUseExpAgCertain
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgNormal(value = value, sd = sqrt(value))
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.double(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rpois(n = 20, lambda = exposure * theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1:3] <- NA
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        x1 <- updateTheta_PoissonVaryingUseExpAgCertain(x0, y = y, exposure = exposure)
        expect_true(validObject(x1))
        if (x1@nAcceptTheta > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
        if (!was.updated)
            warning("theta was not updated")
    }
})

test_that("R and C versions of updateTheta_PoissonVaryingUseExpAgCertain same answer - multiple aggregate values", {
    updateTheta_PoissonVaryingUseExpAgCertain <- demest:::updateTheta_PoissonVaryingUseExpAgCertain
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgNormal(value = value, sd = sqrt(value))
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.double(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rpois(n = 20, lambda = exposure * theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1:3] <- NA
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        x.R <- updateTheta_PoissonVaryingUseExpAgCertain(x0, y = y, exposure = exposure, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateTheta_PoissonVaryingUseExpAgCertain(x0, y = y, exposure = exposure, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptTheta@.Data > 0L)
            was.updated <- TRUE
        if (!was.updated)
            warning("theta was not updated")
    }
})

test_that("updateTheta_PoissonVaryingUseExpAgCertain gives valid answer - some aggregate weights = 0", {
    updateTheta_PoissonVaryingUseExpAgCertain <- demest:::updateTheta_PoissonVaryingUseExpAgCertain
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        w <- Counts(array(rpois(n = 20, lambda = 4),
                          dim = c(2, 10),
                          dimnames = list(sex = c("f", "m"), age = 0:9)))
        w[sample(20, size = 8)] <- 0
        aggregate <- AgNormal(value = value, sd = sqrt(value), weights = w)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.double(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rpois(n = 20, lambda = exposure * theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1:3] <- NA
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        x1 <- updateTheta_PoissonVaryingUseExpAgCertain(x0, y = y, exposure = exposure)
        expect_true(validObject(x1))
        if (x1@nAcceptTheta > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
        if (!was.updated)
            warning("theta was not updated")
    }
})

test_that("R and C versions of updateTheta_PoissonVaryingUseExpAgCertain same answer - some aggregate weights = 0", {
    updateTheta_PoissonVaryingUseExpAgCertain <- demest:::updateTheta_PoissonVaryingUseExpAgCertain
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        w <- Counts(array(rpois(n = 20, lambda = 4),
                          dim = c(2, 10),
                          dimnames = list(sex = c("f", "m"), age = 0:9)))
        w[sample(20, size = 8)] <- 0
        aggregate <- AgNormal(value = value, sd = sqrt(value), weights = w)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.double(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rpois(n = 20, lambda = exposure * theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1:3] <- NA
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        x.R <- updateTheta_PoissonVaryingUseExpAgCertain(x0, y = y, exposure = exposure, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateTheta_PoissonVaryingUseExpAgCertain(x0, y = y, exposure = exposure, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptTheta@.Data > 0L)
            was.updated <- TRUE
        if (!was.updated)
            warning("theta was not updated")
    }
})


## updateThetaAndValueAgNormal_PoissonNotUseExp

test_that("updateThetaAndValueAgNormal_PoissonNotUseExp gives valid answer - single aggregate value", {
    updateThetaAndValueAgNormal_PoissonNotUseExp <- demest:::updateThetaAndValueAgNormal_PoissonNotUseExp
    initialModel <- demest:::initialModel
    ## no missing values
    was.updated <- FALSE ## only test if was ever updated, since only one update done per iteration
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        aggregate <- AgNormal(value = 20, sd = 1, jump = 0.001)
        y <- as.integer(rpois(n = 20, lambda = 20))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = NULL)
        x1 <- updateThetaAndValueAgNormal_PoissonNotUseExp(x0, y = y)
        expect_true(validObject(x1))
        if (x1@nAcceptAg@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
    }
    if (!was.updated)
        warning("theta and bench not updated")
    ## has missing values
    was.updated <- FALSE ## only test if was ever updated, since only one update done per iteration
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        aggregate <- AgNormal(value = 20, sd = 1, jump = 0.001)
        y <- as.integer(rpois(n = 20, lambda = 20))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1:5] <- NA
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = NULL)
        x1 <- updateThetaAndValueAgNormal_PoissonNotUseExp(x0, y = y)
        expect_true(validObject(x1))
        if (x1@nAcceptAg@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
    }
    if (!was.updated)
        warning("theta and bench not updated")
})

test_that("R and C versions of updateThetaAndValueAgNormal_PoissonNotUseExp same answer - single aggregate value", {
    updateThetaAndValueAgNormal_PoissonNotUseExp <- demest:::updateThetaAndValueAgNormal_PoissonNotUseExp
    initialModel <- demest:::initialModel
    ## no missing values
    was.updated <- FALSE  ## only test if was ever updated, since only one update done per iteration
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        aggregate <- AgNormal(value = 20, sd = 2)
        y <- as.integer(rpois(n = 20, lambda = 20))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = NULL)
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgNormal_PoissonNotUseExp(x0, y = y, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgNormal_PoissonNotUseExp(x0, y = y, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptAg@.Data > 0L)
            was.updated <- TRUE
    }
    if (!was.updated)
        warning("theta was not updated")
    ## has missing values
    was.updated <- FALSE  ## only test if was ever updated, since only one update done per iteration
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        aggregate <- AgNormal(value = 20, sd = 2)
        y <- as.integer(rpois(n = 20, lambda = 20))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1:5] <- NA
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = NULL)
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgNormal_PoissonNotUseExp(x0, y = y, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgNormal_PoissonNotUseExp(x0, y = y, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptAg@.Data > 0L)
            was.updated <- TRUE
    }
    if (!was.updated)
        warning("theta was not updated")
})

test_that("updateThetaAndValueAgNormal_PoissonNotUseExp gives valid answer - multiple aggregate values", {
    updateThetaAndValueAgNormal_PoissonNotUseExp <- demest:::updateThetaAndValueAgNormal_PoissonNotUseExp
    initialModel <- demest:::initialModel
    ## no missing values
    for (seed in seq_len(n.test)) {
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(runif(n = 3, max = 30), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgNormal(value = value, sd = sqrt(value), jump = 0.001)
        y <- as.integer(rpois(n = 20, lambda = 20))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), jump = 0.001, aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = NULL)
        x1 <- updateThetaAndValueAgNormal_PoissonNotUseExp(x0, y = y)
        expect_true(validObject(x1))
        if (x1@nAcceptAg@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
        if (!was.updated)
            warning("theta was not updated")
    }
    ## has missing values
    for (seed in seq_len(n.test)) {
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(runif(n = 3, max = 30), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgNormal(value = value, sd = sqrt(value), jump = 0.001)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        y <- as.integer(rpois(n = 20, lambda = 20))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1:5] <- NA
        spec <- Model(y ~ Poisson(mean ~ age + sex), jump = 0.001, aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = NULL)
        x1 <- updateThetaAndValueAgNormal_PoissonNotUseExp(x0, y = y)
        expect_true(validObject(x1))
        if (x1@nAcceptAg@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
        if (!was.updated)
            warning("theta was not updated")
        validObject(x1)
    }
})

test_that("R and C versions of updateThetaAndValueAgNormal_PoissonNotUseExp same answer - multiple aggregate values", {
    updateThetaAndValueAgNormal_PoissonNotUseExp <- demest:::updateThetaAndValueAgNormal_PoissonNotUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        ## no missing values
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgNormal(value = value, sd = sqrt(value), jump = 0.01)
        y <- as.integer(rpois(n = 20, lambda = 20))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), jump = 0.1, aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = NULL)
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgNormal_PoissonNotUseExp(x0, y = y, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgNormal_PoissonNotUseExp(x0, y = y, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptAg@.Data > 0L)
            was.updated <- TRUE
        if (!was.updated)
            warning("aggregate value was not updated")
        ## has missing values
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgNormal(value = value, sd = sqrt(value), jump = 0.01)
        y <- as.integer(rpois(n = 20, lambda = 20))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), jump = 0.1, aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = NULL)
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgNormal_PoissonNotUseExp(x0, y = y, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgNormal_PoissonNotUseExp(x0, y = y, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptAg@.Data > 0L)
            was.updated <- TRUE
        if (!was.updated)
            warning("aggregate value was not updated")
    }
})

## updateThetaAndValueAgPoisson_PoissonNotUseExp

test_that("updateThetaAndValueAgPoisson_PoissonNotUseExp gives valid answer - single aggregate value", {
    updateThetaAndValueAgPoisson_PoissonNotUseExp <- demest:::updateThetaAndValueAgPoisson_PoissonNotUseExp
    initialModel <- demest:::initialModel
    ## no missing values
    was.updated <- FALSE ## only test if was ever updated, since only one update done per iteration
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        aggregate <- AgPoisson(value = 20, jump = 0.001)
        y <- as.integer(rpois(n = 20, lambda = 20))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = NULL)
        x1 <- updateThetaAndValueAgPoisson_PoissonNotUseExp(x0, y = y)
        expect_true(validObject(x1))
        if (x1@nAcceptAg@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
    }
    if (!was.updated)
        warning("theta and bench not updated")
    ## has missing values
    was.updated <- FALSE ## only test if was ever updated, since only one update done per iteration
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        aggregate <- AgPoisson(value = 20, jump = 0.001)
        y <- as.integer(rpois(n = 20, lambda = 20))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1:5] <- NA
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = NULL)
        x1 <- updateThetaAndValueAgPoisson_PoissonNotUseExp(x0, y = y)
        expect_true(validObject(x1))
        if (x1@nAcceptAg@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
    }
    if (!was.updated)
        warning("theta and bench not updated")
})

test_that("R and C versions of updateThetaAndValueAgPoisson_PoissonNotUseExp same answer - single aggregate value", {
    updateThetaAndValueAgPoisson_PoissonNotUseExp <- demest:::updateThetaAndValueAgPoisson_PoissonNotUseExp
    initialModel <- demest:::initialModel
    ## no missing values
    was.updated <- FALSE  ## only test if was ever updated, since only one update done per iteration
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        aggregate <- AgPoisson(value = 20)
        y <- as.integer(rpois(n = 20, lambda = 20))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = NULL)
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgPoisson_PoissonNotUseExp(x0, y = y, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgPoisson_PoissonNotUseExp(x0, y = y, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptAg@.Data > 0L)
            was.updated <- TRUE
    }
    if (!was.updated)
        warning("theta was not updated")
    ## has missing values
    was.updated <- FALSE  ## only test if was ever updated, since only one update done per iteration
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        aggregate <- AgPoisson(value = 20)
        y <- as.integer(rpois(n = 20, lambda = 20))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1:5] <- NA
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = NULL)
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgPoisson_PoissonNotUseExp(x0, y = y, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgPoisson_PoissonNotUseExp(x0, y = y, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptAg@.Data > 0L)
            was.updated <- TRUE
    }
    if (!was.updated)
        warning("theta was not updated")
})

test_that("updateThetaAndValueAgPoisson_PoissonNotUseExp gives valid answer - multiple aggregate values", {
    updateThetaAndValueAgPoisson_PoissonNotUseExp <- demest:::updateThetaAndValueAgPoisson_PoissonNotUseExp
    initialModel <- demest:::initialModel
    ## no missing values
    for (seed in seq_len(n.test)) {
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(runif(n = 3, max = 30), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgPoisson(value = value, jump = 0.001)
        y <- as.integer(rpois(n = 20, lambda = 20))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), jump = 0.001, aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = NULL)
        x1 <- updateThetaAndValueAgPoisson_PoissonNotUseExp(x0, y = y)
        expect_true(validObject(x1))
        if (x1@nAcceptAg@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
        if (!was.updated)
            warning("theta was not updated")
    }
    ## has missing values
    for (seed in seq_len(n.test)) {
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(runif(n = 3, max = 30), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgPoisson(value = value, jump = 0.001)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        y <- as.integer(rpois(n = 20, lambda = 20))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1:5] <- NA
        spec <- Model(y ~ Poisson(mean ~ age + sex), jump = 0.001, aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = NULL)
        x1 <- updateThetaAndValueAgPoisson_PoissonNotUseExp(x0, y = y)
        expect_true(validObject(x1))
        if (x1@nAcceptAg@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
        if (!was.updated)
            warning("theta was not updated")
        validObject(x1)
    }
})

test_that("R and C versions of updateThetaAndValueAgPoisson_PoissonNotUseExp same answer - multiple aggregate values", {
    updateThetaAndValueAgPoisson_PoissonNotUseExp <- demest:::updateThetaAndValueAgPoisson_PoissonNotUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        ## no missing values
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgPoisson(value = value, jump = 0.01)
        y <- as.integer(rpois(n = 20, lambda = 20))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), jump = 0.1, aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = NULL)
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgPoisson_PoissonNotUseExp(x0, y = y, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgPoisson_PoissonNotUseExp(x0, y = y, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptAg@.Data > 0L)
            was.updated <- TRUE
        if (!was.updated)
            warning("aggregate value was not updated")
        ## has missing values
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgPoisson(value = value, jump = 0.01)
        y <- as.integer(rpois(n = 20, lambda = 20))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), jump = 0.1, aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = NULL)
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgPoisson_PoissonNotUseExp(x0, y = y, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgPoisson_PoissonNotUseExp(x0, y = y, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptAg@.Data > 0L)
            was.updated <- TRUE
        if (!was.updated)
            warning("aggregate value was not updated")
    }
})



## updateThetaAndValueAgFun_PoissonNotUseExp

test_that("updateThetaAndValueAgFun_PoissonNotUseExp gives valid answer - single aggregate value", {
    updateThetaAndValueAgFun_PoissonNotUseExp <- demest:::updateThetaAndValueAgFun_PoissonNotUseExp
    initialModel <- demest:::initialModel
    ## no missing values
    was.updated <- FALSE ## only test if was ever updated, since only one update done per iteration
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        FUN <- function(x, weights) sum(x * sqrt(weights))
        aggregate <- AgFun(value = 0.5, sd = 0.01, FUN = FUN)
        theta <- rbeta(n = 20, shape1 = 5, shape2 = 5)
        y <- as.integer(rpois(n = 20, lambda = theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)),
                    dimscales = c(age = "Intervals"))
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = NULL)
        expect_is(x0, "PoissonVaryingNotUseExpAgFun")
        x1 <- updateThetaAndValueAgFun_PoissonNotUseExp(x0, y = y)
        expect_true(validObject(x1))
        if (x1@nAcceptTheta@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
    }
    if (!was.updated)
        warning("theta and bench not updated")
    ## has missing values
    was.updated <- FALSE ## only test if was ever updated, since only one update done per iteration
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        FUN <- function(x, weights) sum(x * sqrt(weights))
        aggregate <- AgFun(value = 0.5, sd = 0.01, FUN = FUN)
        theta <- rbeta(n = 20, shape1 = 5, shape2 = 5)
        y <- as.integer(rpois(n = 20, lambda = theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)),
                    dimscales = c(age = "Intervals"))
        y[1:5] <- NA
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = NULL)
        x1 <- updateThetaAndValueAgFun_PoissonNotUseExp(x0, y = y)
        expect_true(validObject(x1))
        if (x1@nAcceptTheta@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
    }
    if (!was.updated)
        warning("theta and bench not updated")
})

test_that("R and C versions of updateThetaAndValueAgFun_PoissonNotUseExp same answer - single aggregate value", {
    updateThetaAndValueAgFun_PoissonNotUseExp <- demest:::updateThetaAndValueAgFun_PoissonNotUseExp
    initialModel <- demest:::initialModel
    ## no missing values
    was.updated <- FALSE  ## only test if was ever updated, since only one update done per iteration
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        FUN <- function(x, weights) sum(x * sqrt(weights))
        aggregate <- AgFun(value = 0.5, sd = 0.2, FUN = FUN)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        y <- as.integer(rpois(n = 20, lambda = theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)),
                    dimscales = c(age = "Intervals"))
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = NULL)
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgFun_PoissonNotUseExp(x0, y = y, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgFun_PoissonNotUseExp(x0, y = y, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptTheta@.Data > 0L)
            was.updated <- TRUE
    }
    if (!was.updated)
        warning("theta was not updated")
    ## has missing values
    was.updated <- FALSE  ## only test if was ever updated, since only one update done per iteration
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        FUN <- function(x, weights) sum(x * sqrt(weights))
        aggregate <- AgFun(value = 0.5, sd = 0.2, FUN = FUN)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        y <- as.integer(rpois(n = 20, lambda = theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1:5] <- NA
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = NULL)
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgFun_PoissonNotUseExp(x0, y = y, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgFun_PoissonNotUseExp(x0, y = y, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptTheta@.Data > 0L)
            was.updated <- TRUE
    }
    if (!was.updated)
        warning("theta was not updated")
})

test_that("updateThetaAndValueAgFun_PoissonNotUseExp gives valid answer - multiple aggregate values", {
    updateThetaAndValueAgFun_PoissonNotUseExp <- demest:::updateThetaAndValueAgFun_PoissonNotUseExp
    initialModel <- demest:::initialModel
    ## no missing values
    for (seed in seq_len(n.test)) {
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        FUN <- function(x, weights) sum(x * sqrt(weights))
        aggregate <- AgFun(value = value, sd = sqrt(value), FUN = FUN)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        y <- as.integer(rpois(n = 20, lambda = theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)),
                    dimscales = c(age = "Intervals"))
        spec <- Model(y ~ Poisson(mean ~ age + sex), jump = 0.001, aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = NULL)
        x1 <- updateThetaAndValueAgFun_PoissonNotUseExp(x0, y = y)
        expect_true(validObject(x1))
        if (x1@nAcceptTheta@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
        if (!was.updated)
            warning("theta was not updated")
    }
    ## has missing values
    for (seed in seq_len(n.test)) {
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        FUN <- function(x, weights) sum(x * sqrt(weights))
        aggregate <- AgFun(value = value, sd = sqrt(value), FUN = FUN)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        y <- as.integer(rpois(n = 20, lambda = theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)),
                    dimscales = c(age = "Intervals"))
        y[1:5] <- NA
        spec <- Model(y ~ Poisson(mean ~ age + sex), jump = 0.001, aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = NULL)
        x1 <- updateThetaAndValueAgFun_PoissonNotUseExp(x0, y = y)
        expect_true(validObject(x1))
        if (x1@nAcceptTheta@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
        if (!was.updated)
            warning("theta was not updated")
    }
})

test_that("R and C versions of updateThetaAndValueAgFun_PoissonNotUseExp same answer - multiple aggregate values", {
    updateThetaAndValueAgFun_PoissonNotUseExp <- demest:::updateThetaAndValueAgFun_PoissonNotUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        ## no missing values
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)),
                        dimscales = c(age = "Intervals"))
        FUN <- function(x, weights) sum(x * sqrt(weights))
        aggregate <- AgFun(value = value, sd = sqrt(value), FUN = FUN)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        y <- as.integer(rpois(n = 20, lambda = theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), jump = 0.1, aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = NULL)
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgFun_PoissonNotUseExp(x0, y = y, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgFun_PoissonNotUseExp(x0, y = y, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptTheta@.Data > 0L)
            was.updated <- TRUE
        if (!was.updated)
            warning("aggregate value was not updated")
        ## has missing values
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)),
                        dimscales = c(age = "Intervals"))
        FUN <- function(x, weights) sum(x * sqrt(weights))
        aggregate <- AgFun(value = value, sd = sqrt(value), FUN = FUN)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        y <- as.integer(rpois(n = 20, lambda = theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)),
                    dimscales = c(age = "Intervals"))
        spec <- Model(y ~ Poisson(mean ~ age + sex), jump = 0.1, aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = NULL)
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgFun_PoissonNotUseExp(x0, y = y, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgFun_PoissonNotUseExp(x0, y = y, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptTheta@.Data > 0L)
            was.updated <- TRUE
        if (!was.updated)
            warning("aggregate value was not updated")
    }
})

## updateThetaAndValueAgNormal_PoissonUseExp

test_that("updateThetaAndValueAgNormal_PoissonUseExp gives valid answer - single aggregate value", {
    updateThetaAndValueAgNormal_PoissonUseExp <- demest:::updateThetaAndValueAgNormal_PoissonUseExp
    initialModel <- demest:::initialModel
    ## no missing values
    was.updated <- FALSE ## only test if was ever updated, since only one update done per iteration
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        aggregate <- AgNormal(value = 0.5, sd = 0.01, jump = 0.001)
        theta <- rbeta(n = 20, shape1 = 5, shape2 = 5)
        exposure <- as.double(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10),
                                 dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rpois(n = 20, lambda = exposure * theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        x1 <- updateThetaAndValueAgNormal_PoissonUseExp(x0, y = y, exposure = exposure)
        expect_true(validObject(x1))
        if (x1@nAcceptAg@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
    }
    if (!was.updated)
        warning("theta and bench not updated")
    ## has missing values
    was.updated <- FALSE ## only test if was ever updated, since only one update done per iteration
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        aggregate <- AgNormal(value = 0.5, sd = 0.01, jump = 0.001)
        theta <- rbeta(n = 20, shape1 = 5, shape2 = 5)
        exposure <- as.double(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10),
                                 dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rpois(n = 20, lambda = exposure * theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1:5] <- NA
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        x1 <- updateThetaAndValueAgNormal_PoissonUseExp(x0, y = y, exposure = exposure)
        expect_true(validObject(x1))
        if (x1@nAcceptAg@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
    }
    if (!was.updated)
        warning("theta and bench not updated")
})

test_that("R and C versions of updateThetaAndValueAgNormal_PoissonUseExp same answer - single aggregate value", {
    updateThetaAndValueAgNormal_PoissonUseExp <- demest:::updateThetaAndValueAgNormal_PoissonUseExp
    initialModel <- demest:::initialModel
    ## no missing values
    was.updated <- FALSE  ## only test if was ever updated, since only one update done per iteration
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        aggregate <- AgNormal(value = 0.5, sd = 0.2)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.double(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rpois(n = 20, lambda = exposure * theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgNormal_PoissonUseExp(x0, y = y, exposure = exposure, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgNormal_PoissonUseExp(x0, y = y, exposure = exposure, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptAg@.Data > 0L)
            was.updated <- TRUE
    }
    if (!was.updated)
        warning("theta was not updated")
    ## has missing values
    was.updated <- FALSE  ## only test if was ever updated, since only one update done per iteration
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        aggregate <- AgNormal(value = 0.5, sd = 0.2)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.double(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rpois(n = 20, lambda = exposure * theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1:5] <- NA
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgNormal_PoissonUseExp(x0, y = y, exposure = exposure, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgNormal_PoissonUseExp(x0, y = y, exposure = exposure, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptAg@.Data > 0L)
            was.updated <- TRUE
    }
    if (!was.updated)
        warning("theta was not updated")
})

test_that("updateThetaAndValueAgNormal_PoissonUseExp gives valid answer - multiple aggregate values", {
    updateThetaAndValueAgNormal_PoissonUseExp <- demest:::updateThetaAndValueAgNormal_PoissonUseExp
    initialModel <- demest:::initialModel
    ## no missing values
    for (seed in seq_len(n.test)) {
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgNormal(value = value, sd = sqrt(value), jump = 0.001)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.double(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rpois(n = 20, lambda = exposure * theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), jump = 0.001, aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        x1 <- updateThetaAndValueAgNormal_PoissonUseExp(x0, y = y, exposure = exposure)
        expect_true(validObject(x1))
        if (x1@nAcceptAg@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
        if (!was.updated)
            warning("theta was not updated")
    }
    ## has missing values
    for (seed in seq_len(n.test)) {
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgNormal(value = value, sd = sqrt(value), jump = 0.001)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.double(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rpois(n = 20, lambda = exposure * theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1:5] <- NA
        spec <- Model(y ~ Poisson(mean ~ age + sex), jump = 0.001, aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        x1 <- updateThetaAndValueAgNormal_PoissonUseExp(x0, y = y, exposure = exposure)
        expect_true(validObject(x1))
        if (x1@nAcceptAg@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
        if (!was.updated)
            warning("theta was not updated")
    }
})

test_that("R and C versions of updateThetaAndValueAgNormal_PoissonUseExp same answer - multiple aggregate values", {
    updateThetaAndValueAgNormal_PoissonUseExp <- demest:::updateThetaAndValueAgNormal_PoissonUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        ## no missing values
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgNormal(value = value, sd = sqrt(value), jump = 0.01)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.double(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rpois(n = 20, lambda = exposure * theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), jump = 0.1, aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgNormal_PoissonUseExp(x0, y = y, exposure = exposure, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgNormal_PoissonUseExp(x0, y = y, exposure = exposure, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptAg@.Data > 0L)
            was.updated <- TRUE
        if (!was.updated)
            warning("aggregate value was not updated")
        ## has missing values
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgNormal(value = value, sd = sqrt(value), jump = 0.01)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.double(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rpois(n = 20, lambda = exposure * theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), jump = 0.1, aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgNormal_PoissonUseExp(x0, y = y, exposure = exposure, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgNormal_PoissonUseExp(x0, y = y, exposure = exposure, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptAg@.Data > 0L)
            was.updated <- TRUE
        if (!was.updated)
            warning("aggregate value was not updated")
    }
})


## updateThetaAndValueAgLife_PoissonUseExp

test_that("updateThetaAndValueAgLife_PoissonUseExp gives valid answer - single aggregate value", {
    updateThetaAndValueAgLife_PoissonUseExp <- demest:::updateThetaAndValueAgLife_PoissonUseExp
    initialModel <- demest:::initialModel
    ## no missing values
    was.updated <- FALSE ## only test if was ever updated, since only one update done per iteration
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        theta <- rgamma(n = 20, shape = 2, rate = 0.5) / 10
        expose <- as.double(rpois(n = 20, lambda = 20)) + 1
        expose <- Counts(array(expose, dim = c(2, 10),
                                 dimnames = list(sex = c("f", "m"), age = c(0:8, "9+"))))
        aggregate <- AgLife(value = 3, sd = 0.3)
        y <- as.integer(rpois(n = 20, lambda = expose * theta))
        y <- Counts(array(y, dim = c(2, 10),
                          dimnames = list(sex = c("f", "m"), age = c(0:8, "9+"))))
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = expose)
        expect_is(x0, "PoissonVaryingUseExpAgLife")
        x1 <- updateThetaAndValueAgLife_PoissonUseExp(x0, y = y, exposure = expose)
        expect_true(validObject(x1))
        if (x1@nAcceptTheta@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
    }
    if (!was.updated)
        warning("theta and bench not updated")
    ## has missing values
    was.updated <- FALSE ## only test if was ever updated, since only one update done per iteration
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        theta <- rgamma(n = 20, shape = 2, rate = 0.5) / 10
        expose <- as.double(rpois(n = 20, lambda = 20)) + 1
        expose <- Counts(array(expose, dim = c(2, 10),
                                 dimnames = list(sex = c("f", "m"), age = c(0:8, "9+"))))
        aggregate <- AgLife(value = 3, sd = 0.3)
        y <- as.integer(rpois(n = 20, lambda = expose * theta))
        y <- Counts(array(y, dim = c(2, 10),
                          dimnames = list(sex = c("f", "m"), age = c(0:8, "9+"))))
        y[1:5] <- NA
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = expose)
        expect_is(x0, "PoissonVaryingUseExpAgLife")
        x1 <- updateThetaAndValueAgLife_PoissonUseExp(x0, y = y, exposure = expose)
        expect_true(validObject(x1))
        if (x1@nAcceptTheta@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
    }
    if (!was.updated)
        warning("theta and bench not updated")
})

test_that("R and C versions of updateThetaAndValueAgLife_PoissonUseExp same answer - single aggregate value", {
    updateThetaAndValueAgLife_PoissonUseExp <- demest:::updateThetaAndValueAgLife_PoissonUseExp
    initialModel <- demest:::initialModel
    ## no missing values
    was.updated <- FALSE  ## only test if was ever updated, since only one update done per iteration
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        theta <- rgamma(n = 20, shape = 2, rate = 0.5) / 10
        expose <- as.double(rpois(n = 20, lambda = 20)) + 1
        expose <- Counts(array(expose, dim = c(2, 10),
                                 dimnames = list(sex = c("f", "m"), age = c(0:8, "9+"))))
        aggregate <- AgLife(value = 3, sd = 0.3)
        y <- as.integer(rpois(n = 20, lambda = expose * theta))
        y <- Counts(array(y, dim = c(2, 10),
                          dimnames = list(sex = c("f", "m"), age = c(0:8, "9+"))))
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = expose)
        expect_is(x0, "PoissonVaryingUseExpAgLife")
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgLife_PoissonUseExp(x0, y = y, exposure = expose, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgLife_PoissonUseExp(x0, y = y, exposure = expose, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptTheta@.Data > 0L)
            was.updated <- TRUE
    }
    if (!was.updated)
        warning("theta was not updated")
    ## has missing values
    was.updated <- FALSE  ## only test if was ever updated, since only one update done per iteration
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        theta <- rgamma(n = 20, shape = 2, rate = 0.5) / 10
        expose <- as.double(rpois(n = 20, lambda = 20)) + 1
        expose <- Counts(array(expose, dim = c(2, 10),
                                 dimnames = list(sex = c("f", "m"), age = c(0:8, "9+"))))
        aggregate <- AgLife(value = 3, sd = 0.3)
        y <- as.integer(rpois(n = 20, lambda = expose * theta))
        y <- Counts(array(y, dim = c(2, 10),
                          dimnames = list(sex = c("f", "m"), age = c(0:8, "9+"))))
        y[1:5] <- NA
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = expose)
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgLife_PoissonUseExp(x0, y = y, exposure = expose, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgLife_PoissonUseExp(x0, y = y, exposure = expose, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptTheta@.Data > 0L)
            was.updated <- TRUE
    }
    if (!was.updated)
        warning("theta was not updated")
})

test_that("updateThetaAndValueAgLife_PoissonUseExp gives valid answer - multiple aggregate values", {
    updateThetaAndValueAgLife_PoissonUseExp <- demest:::updateThetaAndValueAgLife_PoissonUseExp
    initialModel <- demest:::initialModel
    ## no missing values
    for (seed in seq_len(n.test)) {
        was.updated <- FALSE
        set.seed(seed)
        theta <- rgamma(n = 20, shape = 2, rate = 0.5) / 10
        expose <- as.double(rpois(n = 20, lambda = 20)) + 1
        expose <- Counts(array(expose, dim = c(2, 10),
                                 dimnames = list(sex = c("f", "m"), age = c(0:8, "9+"))))
        value <- Values(array(c(3, 4), dim = 2, dimnames = list(sex = c("f", "m"))))
        aggregate <- AgLife(value = value, sd = 0.3)
        y <- as.integer(rpois(n = 20, lambda = expose * theta))
        y <- Counts(array(y, dim = c(2, 10),
                          dimnames = list(sex = c("f", "m"), age = c(0:8, "9+"))))
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = expose)
        expect_is(x0, "PoissonVaryingUseExpAgLife")
        x1 <- updateThetaAndValueAgLife_PoissonUseExp(x0, y = y, exposure = expose)
        expect_true(validObject(x1))
        if (x1@nAcceptTheta@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
        if (!was.updated)
            warning("theta was not updated")
    }
    ## has missing values
    for (seed in seq_len(n.test)) {
        was.updated <- FALSE
        set.seed(seed)
        theta <- rgamma(n = 20, shape = 2, rate = 0.5) / 10
        expose <- as.double(rpois(n = 20, lambda = 20)) + 1
        expose <- Counts(array(expose, dim = c(2, 10),
                                 dimnames = list(sex = c("f", "m"), age = c(0:8, "9+"))))
        value <- Values(array(c(3, 4), dim = 2, dimnames = list(sex = c("f", "m"))))
        aggregate <- AgLife(value = value, sd = 0.3)
        y <- as.integer(rpois(n = 20, lambda = expose * theta))
        y <- Counts(array(y, dim = c(2, 10),
                          dimnames = list(sex = c("f", "m"), age = c(0:8, "9+"))))
        y[1:5] <- NA
        spec <- Model(y ~ Poisson(mean ~ age + sex), jump = 0.001, aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = expose)
        x1 <- updateThetaAndValueAgLife_PoissonUseExp(x0, y = y, exposure = expose)
        expect_true(validObject(x1))
        if (x1@nAcceptTheta@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
        if (!was.updated)
            warning("theta was not updated")
    }
})

test_that("R and C versions of updateThetaAndValueAgLife_PoissonUseExp same answer - multiple aggregate values", {
    updateThetaAndValueAgLife_PoissonUseExp <- demest:::updateThetaAndValueAgLife_PoissonUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        ## no missing values
        was.updated <- FALSE
        set.seed(seed)
        theta <- rgamma(n = 20, shape = 2, rate = 0.5) / 10
        expose <- as.double(rpois(n = 20, lambda = 20)) + 1
        expose <- Counts(array(expose, dim = c(2, 10),
                                 dimnames = list(sex = c("f", "m"), age = c(0:8, "9+"))))
        value <- Values(array(c(3, 4), dim = 2, dimnames = list(sex = c("f", "m"))))
        aggregate <- AgLife(value = value, sd = 0.3)
        y <- as.integer(rpois(n = 20, lambda = expose * theta))
        y <- Counts(array(y, dim = c(2, 10),
                          dimnames = list(sex = c("f", "m"), age = c(0:8, "9+"))))
        spec <- Model(y ~ Poisson(mean ~ age + sex), jump = 0.1, aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = expose)
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgLife_PoissonUseExp(x0, y = y, exposure = expose, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgLife_PoissonUseExp(x0, y = y, exposure = expose, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptTheta@.Data > 0L)
            was.updated <- TRUE
        if (!was.updated)
            warning("aggregate value was not updated")
        ## has missing values
        was.updated <- FALSE
        set.seed(seed)
        theta <- rgamma(n = 20, shape = 2, rate = 0.5) / 10
        expose <- as.double(rpois(n = 20, lambda = 20)) + 1
        expose <- Counts(array(expose, dim = c(2, 10),
                                 dimnames = list(sex = c("f", "m"), age = c(0:8, "9+"))))
        value <- Values(array(c(3, 4), dim = 2, dimnames = list(sex = c("f", "m"))))
        aggregate <- AgLife(value = value, sd = 0.3)
        y <- as.integer(rpois(n = 20, lambda = expose * theta))
        y <- Counts(array(y, dim = c(2, 10),
                          dimnames = list(sex = c("f", "m"), age = c(0:8, "9+"))))
        spec <- Model(y ~ Poisson(mean ~ age + sex), jump = 0.1, aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = expose)
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgLife_PoissonUseExp(x0, y = y, exposure = expose, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgLife_PoissonUseExp(x0, y = y, exposure = expose, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptTheta@.Data > 0L)
            was.updated <- TRUE
        if (!was.updated)
            warning("aggregate value was not updated")
    }
})


## updateThetaAndValueAgPoisson_PoissonUseExp

test_that("updateThetaAndValueAgPoisson_PoissonUseExp gives valid answer - single aggregate value", {
    updateThetaAndValueAgPoisson_PoissonUseExp <- demest:::updateThetaAndValueAgPoisson_PoissonUseExp
    initialModel <- demest:::initialModel
    ## no missing values
    was.updated <- FALSE ## only test if was ever updated, since only one update done per iteration
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        aggregate <- AgPoisson(value = 0.5, jump = 0.001)
        theta <- rbeta(n = 20, shape1 = 5, shape2 = 5)
        exposure <- as.double(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10),
                                 dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rpois(n = 20, lambda = exposure * theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        x1 <- updateThetaAndValueAgPoisson_PoissonUseExp(x0, y = y, exposure = exposure)
        expect_true(validObject(x1))
        if (x1@nAcceptAg@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
    }
    if (!was.updated)
        warning("theta and bench not updated")
    ## has missing values
    was.updated <- FALSE ## only test if was ever updated, since only one update done per iteration
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        aggregate <- AgPoisson(value = 0.5, jump = 0.001)
        theta <- rbeta(n = 20, shape1 = 5, shape2 = 5)
        exposure <- as.double(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10),
                                 dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rpois(n = 20, lambda = exposure * theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1:5] <- NA
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        x1 <- updateThetaAndValueAgPoisson_PoissonUseExp(x0, y = y, exposure = exposure)
        expect_true(validObject(x1))
        if (x1@nAcceptAg@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
    }
    if (!was.updated)
        warning("theta and bench not updated")
})

test_that("R and C versions of updateThetaAndValueAgPoisson_PoissonUseExp same answer - single aggregate value", {
    updateThetaAndValueAgPoisson_PoissonUseExp <- demest:::updateThetaAndValueAgPoisson_PoissonUseExp
    initialModel <- demest:::initialModel
    ## no missing values
    was.updated <- FALSE  ## only test if was ever updated, since only one update done per iteration
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        aggregate <- AgPoisson(value = 0.5)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.double(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rpois(n = 20, lambda = exposure * theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgPoisson_PoissonUseExp(x0, y = y, exposure = exposure, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgPoisson_PoissonUseExp(x0, y = y, exposure = exposure, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptAg@.Data > 0L)
            was.updated <- TRUE
    }
    if (!was.updated)
        warning("theta was not updated")
    ## has missing values
    was.updated <- FALSE  ## only test if was ever updated, since only one update done per iteration
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        aggregate <- AgPoisson(value = 0.5)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.double(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rpois(n = 20, lambda = exposure * theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1:5] <- NA
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgPoisson_PoissonUseExp(x0, y = y, exposure = exposure, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgPoisson_PoissonUseExp(x0, y = y, exposure = exposure, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptAg@.Data > 0L)
            was.updated <- TRUE
    }
    if (!was.updated)
        warning("theta was not updated")
})

test_that("updateThetaAndValueAgPoisson_PoissonUseExp gives valid answer - multiple aggregate values", {
    updateThetaAndValueAgPoisson_PoissonUseExp <- demest:::updateThetaAndValueAgPoisson_PoissonUseExp
    initialModel <- demest:::initialModel
    ## no missing values
    for (seed in seq_len(n.test)) {
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgPoisson(value = value, jump = 0.001)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.double(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rpois(n = 20, lambda = exposure * theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), jump = 0.001, aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        x1 <- updateThetaAndValueAgPoisson_PoissonUseExp(x0, y = y, exposure = exposure)
        expect_true(validObject(x1))
        if (x1@nAcceptAg@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
        if (!was.updated)
            warning("theta was not updated")
    }
    ## has missing values
    for (seed in seq_len(n.test)) {
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgPoisson(value = value, jump = 0.001)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.double(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rpois(n = 20, lambda = exposure * theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1:5] <- NA
        spec <- Model(y ~ Poisson(mean ~ age + sex), jump = 0.001, aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        x1 <- updateThetaAndValueAgPoisson_PoissonUseExp(x0, y = y, exposure = exposure)
        expect_true(validObject(x1))
        if (x1@nAcceptAg@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
        if (!was.updated)
            warning("theta was not updated")
    }
})

test_that("R and C versions of updateThetaAndValueAgPoisson_PoissonUseExp same answer - multiple aggregate values", {
    updateThetaAndValueAgPoisson_PoissonUseExp <- demest:::updateThetaAndValueAgPoisson_PoissonUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        ## no missing values
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgPoisson(value = value, jump = 0.01)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.double(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rpois(n = 20, lambda = exposure * theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), jump = 0.1, aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgPoisson_PoissonUseExp(x0, y = y, exposure = exposure, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgPoisson_PoissonUseExp(x0, y = y, exposure = exposure, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptAg@.Data > 0L)
            was.updated <- TRUE
        if (!was.updated)
            warning("aggregate value was not updated")
        ## has missing values
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        aggregate <- AgPoisson(value = value, jump = 0.01)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.double(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rpois(n = 20, lambda = exposure * theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), jump = 0.1, aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgPoisson_PoissonUseExp(x0, y = y, exposure = exposure, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgPoisson_PoissonUseExp(x0, y = y, exposure = exposure, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptAg@.Data > 0L)
            was.updated <- TRUE
        if (!was.updated)
            warning("aggregate value was not updated")
    }
})


## updateThetaAndValueAgFun_PoissonUseExp

test_that("updateThetaAndValueAgFun_PoissonUseExp gives valid answer - single aggregate value", {
    updateThetaAndValueAgFun_PoissonUseExp <- demest:::updateThetaAndValueAgFun_PoissonUseExp
    initialModel <- demest:::initialModel
    ## no missing values
    was.updated <- FALSE ## only test if was ever updated, since only one update done per iteration
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        FUN <- function(x, weights) sum(x * sqrt(weights))
        aggregate <- AgFun(value = 0.5, sd = 0.01, FUN = FUN)
        theta <- rbeta(n = 20, shape1 = 5, shape2 = 5)
        exposure <- as.double(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10),
                                 dimnames = list(sex = c("f", "m"), age = 0:9)),
                           dimscales = c(age = "Intervals"))
        y <- as.integer(rpois(n = 20, lambda = exposure * theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)),
                    dimscales = c(age = "Intervals"))
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        expect_is(x0, "PoissonVaryingUseExpAgFun")
        x1 <- updateThetaAndValueAgFun_PoissonUseExp(x0, y = y, exposure = exposure)
        expect_true(validObject(x1))
        if (x1@nAcceptTheta@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
    }
    if (!was.updated)
        warning("theta and bench not updated")
    ## has missing values
    was.updated <- FALSE ## only test if was ever updated, since only one update done per iteration
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        FUN <- function(x, weights) sum(x * sqrt(weights))
        aggregate <- AgFun(value = 0.5, sd = 0.01, FUN = FUN)
        theta <- rbeta(n = 20, shape1 = 5, shape2 = 5)
        exposure <- as.double(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10),
                                 dimnames = list(sex = c("f", "m"), age = 0:9)),
                           dimscales = c(age = "Intervals"))
        y <- as.integer(rpois(n = 20, lambda = exposure * theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)),
                    dimscales = c(age = "Intervals"))
        y[1:5] <- NA
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        x1 <- updateThetaAndValueAgFun_PoissonUseExp(x0, y = y, exposure = exposure)
        expect_true(validObject(x1))
        if (x1@nAcceptTheta@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
    }
    if (!was.updated)
        warning("theta and bench not updated")
})

test_that("R and C versions of updateThetaAndValueAgFun_PoissonUseExp same answer - single aggregate value", {
    updateThetaAndValueAgFun_PoissonUseExp <- demest:::updateThetaAndValueAgFun_PoissonUseExp
    initialModel <- demest:::initialModel
    ## no missing values
    was.updated <- FALSE  ## only test if was ever updated, since only one update done per iteration
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        FUN <- function(x, weights) sum(x * sqrt(weights))
        aggregate <- AgFun(value = 0.5, sd = 0.2, FUN = FUN)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.double(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)),
                           dimscales = c(age = "Intervals"))
        y <- as.integer(rpois(n = 20, lambda = exposure * theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)),
                    dimscales = c(age = "Intervals"))
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgFun_PoissonUseExp(x0, y = y, exposure = exposure, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgFun_PoissonUseExp(x0, y = y, exposure = exposure, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptTheta@.Data > 0L)
            was.updated <- TRUE
    }
    if (!was.updated)
        warning("theta was not updated")
    ## has missing values
    was.updated <- FALSE  ## only test if was ever updated, since only one update done per iteration
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        FUN <- function(x, weights) sum(x * sqrt(weights))
        aggregate <- AgFun(value = 0.5, sd = 0.2, FUN = FUN)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.double(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rpois(n = 20, lambda = exposure * theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y[1:5] <- NA
        spec <- Model(y ~ Poisson(mean ~ age + sex), aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgFun_PoissonUseExp(x0, y = y, exposure = exposure, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgFun_PoissonUseExp(x0, y = y, exposure = exposure, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptTheta@.Data > 0L)
            was.updated <- TRUE
    }
    if (!was.updated)
        warning("theta was not updated")
})

test_that("updateThetaAndValueAgFun_PoissonUseExp gives valid answer - multiple aggregate values", {
    updateThetaAndValueAgFun_PoissonUseExp <- demest:::updateThetaAndValueAgFun_PoissonUseExp
    initialModel <- demest:::initialModel
    ## no missing values
    for (seed in seq_len(n.test)) {
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        FUN <- function(x, weights) sum(x * sqrt(weights))
        aggregate <- AgFun(value = value, sd = sqrt(value), FUN = FUN)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.double(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)),
                           dimscales = c(age = "Intervals"))
        y <- as.integer(rpois(n = 20, lambda = exposure * theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)),
                    dimscales = c(age = "Intervals"))
        spec <- Model(y ~ Poisson(mean ~ age + sex), jump = 0.001, aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        x1 <- updateThetaAndValueAgFun_PoissonUseExp(x0, y = y, exposure = exposure)
        expect_true(validObject(x1))
        if (x1@nAcceptTheta@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
        if (!was.updated)
            warning("theta was not updated")
    }
    ## has missing values
    for (seed in seq_len(n.test)) {
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)))
        FUN <- function(x, weights) sum(x * sqrt(weights))
        aggregate <- AgFun(value = value, sd = sqrt(value), FUN = FUN)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.double(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)),
                           dimscales = c(age = "Intervals"))
        y <- as.integer(rpois(n = 20, lambda = exposure * theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)),
                    dimscales = c(age = "Intervals"))
        y[1:5] <- NA
        spec <- Model(y ~ Poisson(mean ~ age + sex), jump = 0.001, aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        x1 <- updateThetaAndValueAgFun_PoissonUseExp(x0, y = y, exposure = exposure)
        expect_true(validObject(x1))
        if (x1@nAcceptTheta@.Data > 0L) {
            expect_false(identical(x0@theta, x1@theta))
            was.updated <- TRUE
        }
        else
            expect_identical(x0@theta, x1@theta)
        if (!was.updated)
            warning("theta was not updated")
    }
})

test_that("R and C versions of updateThetaAndValueAgFun_PoissonUseExp same answer - multiple aggregate values", {
    updateThetaAndValueAgFun_PoissonUseExp <- demest:::updateThetaAndValueAgFun_PoissonUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        ## no missing values
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)),
                        dimscales = c(age = "Intervals"))
        FUN <- function(x, weights) sum(x * sqrt(weights))
        aggregate <- AgFun(value = value, sd = sqrt(value), FUN = FUN)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.double(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)),
                           dimscales = c(age = "Intervals"))
        y <- as.integer(rpois(n = 20, lambda = exposure * theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ Poisson(mean ~ age + sex), jump = 0.1, aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgFun_PoissonUseExp(x0, y = y, exposure = exposure, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgFun_PoissonUseExp(x0, y = y, exposure = exposure, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptTheta@.Data > 0L)
            was.updated <- TRUE
        if (!was.updated)
            warning("aggregate value was not updated")
        ## has missing values
        was.updated <- FALSE
        set.seed(seed)
        value <- Values(array(rbeta(n = 3, shape1 = 20, shape2 = 5), dim = 3, dimnames = list(age = 0:2)),
                        dimscales = c(age = "Intervals"))
        FUN <- function(x, weights) sum(x * sqrt(weights))
        aggregate <- AgFun(value = value, sd = sqrt(value), FUN = FUN)
        theta <- rbeta(n = 20, shape1 = 20, shape2 = 5)
        exposure <- as.double(rpois(n = 20, lambda = 20))
        exposure <- Counts(array(exposure, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)))
        y <- as.integer(rpois(n = 20, lambda = exposure * theta))
        y <- Counts(array(y, dim = c(2, 10), dimnames = list(sex = c("f", "m"), age = 0:9)),
                    dimscales = c(age = "Intervals"))
        spec <- Model(y ~ Poisson(mean ~ age + sex), jump = 0.1, aggregate = aggregate)
        x0 <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed + 1)
        x.R <- updateThetaAndValueAgFun_PoissonUseExp(x0, y = y, exposure = exposure, useC = FALSE)
        set.seed(seed + 1)
        x.C <- updateThetaAndValueAgFun_PoissonUseExp(x0, y = y, exposure = exposure, useC = TRUE)
        if (test.identity)
            expect_identical(x.R, x.C)
        else
            expect_equal(x.R, x.C)
        if (x.R@nAcceptTheta@.Data > 0L)
            was.updated <- TRUE
        if (!was.updated)
            warning("aggregate value was not updated")
    }
})




## updateVarsigma

test_that("updateVarsigma gives valid answer", {
    updateVarsigma <- demest:::updateVarsigma
    initialModel <- demest:::initialModel
    updateSDNorm <- demest:::updateSDNorm
    I <- 20L
    for (seed in seq_len(n.test)) {
        ## no missing values
        set.seed(seed)
        varsigma <- runif(1, 1, 20)
        w <- rbeta(n = I, shape1 = 5, shape2 = 5)
        weights <- Counts(array(w,
                                dim = c(I/2, 2),
                                dimnames = list(age = seq(from = 0, to = I/2-1), sex = c("f", "m"))))
        mu <- runif(1, -10, 10)
        sigma <- runif(1, 0.1, 20)
        y <- Counts(array(rnorm(n = I, mean = mu, sd = sqrt(w) * varsigma),
                          dim = c(I/2, 2),
                          dimnames = list(age = seq(from = 0, to = I/2-1), sex = c("f", "m"))))
        spec <- Model(y ~ Normal(mean ~ age))
        model <- initialModel(spec, y = y, weights = weights)
        set.seed(seed + 1)
        ans.obtained <- updateVarsigma(model, y = y)
        set.seed(seed + 1)
        ans.expected <- model
        V <- sum(w * (y - model@theta)^2)
        ans.expected@varsigma@.Data <- updateSDNorm(sigma = ans.expected@varsigma@.Data,
                                                    A = ans.expected@AVarsigma@.Data,
                                                    nu = ans.expected@nuVarsigma@.Data,
                                                    V = V,
                                                    n = I,
                                                    max = ans.expected@varsigmaMax@.Data)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
        expect_identical(ans.obtained@theta, model@theta)
        expect_identical(ans.obtained@w, model@w)
        expect_identical(ans.obtained@betas, model@betas)
        expect_identical(ans.obtained@sigma, model@sigma)
        ## has missing values
        set.seed(seed)
        varsigma <- runif(1, 1, 20)
        w <- rbeta(n = I, shape1 = 5, shape2 = 5)
        weights <- Counts(array(w,
                                dim = c(I/2, 2),
                                dimnames = list(age = seq(from = 0, to = I/2-1), sex = c("f", "m"))))
        mu <- runif(1, -10, 10)
        sigma <- runif(1, 0.1, 20)
        y <- Counts(array(rnorm(n = I, mean = mu, sd = sqrt(w) * varsigma),
                          dim = c(I/2, 2),
                          dimnames = list(age = seq(from = 0, to = I/2-1), sex = c("f", "m"))))
        y[1:5] <- NA
        spec <- Model(y ~ Normal(mean ~ age))
        model <- initialModel(spec, y = y, weights = weights)
        set.seed(seed + 1)
        ans.obtained <- updateVarsigma(model, y = y)
        set.seed(seed + 1)
        ans.expected <- model
        V <- sum(w[6:20] * (y[6:20] - model@theta[6:20])^2)
        ans.expected@varsigma@.Data <- updateSDNorm(sigma = ans.expected@varsigma@.Data,
                                                    A = ans.expected@AVarsigma@.Data,
                                                    nu = ans.expected@nuVarsigma@.Data,
                                                    V = V,
                                                    n = I - 5L,
                                                    max = ans.expected@varsigmaMax@.Data)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
        expect_identical(ans.obtained@theta, model@theta)
        expect_identical(ans.obtained@w, model@w)
        expect_identical(ans.obtained@betas, model@betas)
        expect_identical(ans.obtained@sigma, model@sigma)
    }
})

test_that("R and C versions of updateVarsigma give same answer", {
    updateVarsigma <- demest:::updateVarsigma
    initialModel <- demest:::initialModel
    I <- 20L
    for (seed in seq_len(n.test)) {
        ## no missing values
        set.seed(seed)
        varsigma <- runif(1, 1, 20)
        w <- rbeta(n = I, shape1 = 5, shape2 = 5)
        weights <- Counts(array(w,
                          dim = c(I/2, 2),
                          dimnames = list(age = seq(from = 0, to = I/2-1), sex = c("f", "m"))))
        mu <- runif(1, -10, 10)
        sigma <- runif(1, 0.1, 20)
        y <- Counts(array(rnorm(n = I, mean = mu, sd = sqrt(w) * varsigma),
                          dim = c(I/2, 2),
                          dimnames = list(age = seq(from = 0, to = I/2-1), sex = c("f", "m"))))
        spec <- Model(y ~ Normal(mean ~ age))
        model <- initialModel(spec, y = y, weights = weights)
        set.seed(seed + 1)
        ans.R <- updateVarsigma(model, y = y, useC = FALSE)
        set.seed(seed + 1)
        ans.C <- updateVarsigma(model, y = y, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
        ## has missing values
        set.seed(seed)
        varsigma <- runif(1, 1, 20)
        w <- rbeta(n = I, shape1 = 5, shape2 = 5)
        weights <- Counts(array(w,
                          dim = c(I/2, 2),
                          dimnames = list(age = seq(from = 0, to = I/2-1), sex = c("f", "m"))))
        mu <- runif(1, -10, 10)
        sigma <- runif(1, 0.1, 20)
        y <- Counts(array(rnorm(n = I, mean = mu, sd = sqrt(w) * varsigma),
                          dim = c(I/2, 2),
                          dimnames = list(age = seq(from = 0, to = I/2-1), sex = c("f", "m"))))
        y[1:5] <- NA
        spec <- Model(y ~ Normal(mean ~ age))
        model <- initialModel(spec, y = y, weights = weights)
        set.seed(seed + 1)
        ans.R <- updateVarsigma(model, y = y, useC = FALSE)
        set.seed(seed + 1)
        ans.C <- updateVarsigma(model, y = y, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})


## UPDATING COUNTS ####################################################################

## updateCounts - PoissonNotUseExp

test_that("R version of updateCountsPoissonNotUseExp works with no subtotals", {
    updateCountsPoissonNotUseExp <- demest:::updateCountsPoissonNotUseExp
    diffLogLik <- demest:::diffLogLik
    initialModel <- demest:::initialModel
    getIAfter <- dembase::getIAfter
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    logLikelihood <- demest:::logLikelihood
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        yProp <- as.integer(rpois(n = 1, lambda = 10))
        y <- Counts(array(as.integer(rpois(24, lambda = 10)),
                          dim = c(6, 4),
                          dimnames = list(age = 0:5, reg = letters[1:4])))
        model <- initialModel(Model(y ~ Poisson(mean ~ reg + age)),
                              y = y, exposure = NULL)
        datasets <- list(Counts(array(as.integer(rpois(3, lambda = 20)),
                                      dim = 3,
                                      dimnames = list(reg = letters[1:3]))),
                         Counts(array(as.integer(rpois(18, lambda = 10)),
                                      dim = c(6, 3),
                                      dimnames = list(age = 0:5, reg = letters[1:3]))))
        observation <- vector("list", 2)
        transforms <- vector("list", 2)
        for (i in 1:2) {
            transforms[[i]] <- makeCollapseTransformExtra(makeTransform(x = y,
                                                                        y = datasets[[i]],
                                                                        subset = TRUE))
            observation[[i]] <- initialModel(Model(y ~ Poisson(mean ~ 1)),
                                             y = datasets[[i]],
                                             exposure = collapse(y, transforms[[i]]))
        }
        set.seed(seed)
        ans.obtained <- updateCountsPoissonNotUseExp(y = y,
                                                     model = model,
                                                     observation = observation,
                                                     datasets = datasets,
                                                     transforms = transforms)
        set.seed(seed)
        ans.expected <- y
        for (i in seq_along(y)) {
            y.prop <- as.integer(rpois(n = 1, lambda = model@theta[i]))
            diff.log.lik <- diffLogLik(yProp = y.prop,
                                       y = ans.expected,
                                       indicesY = i,
                                       observation = observation,
                                       datasets = datasets,
                                       transforms = transforms)
            if ((diff.log.lik >= 0) || (runif(1) < exp(diff.log.lik)))
                ans.expected[i] <- y.prop
        }
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
    set.seed(100)
    ans.obtained <- updateCountsPoissonNotUseExp(y = y,
                                                 model = model,
                                                 observation = observation,
                                                 datasets = datasets,
                                                 transforms = transforms)
    set.seed(100)
    ans.expected <- y
    for (i in seq_along(y)) {
        y.prop <- as.integer(rpois(n = 1, lambda = model@theta[i]))
        diff.log.lik <- diffLogLik(yProp = y.prop,
                                   y = ans.expected,
                                   indicesY = i,
                                   observation = observation,
                                   datasets = datasets,
                                   transforms = transforms)
        if ((diff.log.lik >= 0) || (runif(1) < exp(diff.log.lik)))
            ans.expected[i] <- y.prop
    }
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})

test_that("R and C versions of updateCountsPoissonNotUseExp with no subtotals give same answer", {
    updateCountsPoissonNotUseExp <- demest:::updateCountsPoissonNotUseExp
    initialModel <- demest:::initialModel
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    set.seed(100)
    yProp <- as.integer(rpois(n = 1, lambda = 10))
    y <- Counts(array(as.integer(rpois(24, lambda = 10)),
                      dim = c(6, 4),
                      dimnames = list(age = 0:5, reg = letters[1:4])))
    model <- initialModel(Model(y ~ Poisson(mean ~ reg + age)),
                          y = y, exposure = NULL)
    datasets <- list(Counts(array(as.integer(rpois(3, lambda = 20)),
                                  dim = 3,
                                  dimnames = list(reg = letters[1:3]))),
                     Counts(array(as.integer(rpois(18, lambda = 10)),
                                  dim = c(6, 3),
                                  dimnames = list(age = 0:5, reg = letters[1:3]))))
    observation <- vector("list", 2)
    transforms <- vector("list", 2)
    for (i in 1:2) {
        transforms[[i]] <- makeCollapseTransformExtra(makeTransform(x = y,
                                                                    y = datasets[[i]],
                                                                    subset = TRUE))
        observation[[i]] <- initialModel(Model(y ~ Poisson(mean ~ 1)),
                                         y = datasets[[i]],
                                         exposure = collapse(y, transforms[[i]]))
    }
    set.seed(100)
    ans.R <- updateCountsPoissonNotUseExp(y = y,
                                          model = model,
                                          observation = observation,
                                          datasets = datasets,
                                          transforms = transforms,
                                          useC = FALSE)
    set.seed(100)
    ans.C <- updateCountsPoissonNotUseExp(y = y,
                                          model = model,
                                          observation = observation,
                                          datasets = datasets,
                                          transforms = transforms,
                                          useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
})

test_that("R version of updateCountsPoissonNotUseExp works with subtotals made from collapsed y", {
    updateCountsPoissonNotUseExp <- demest:::updateCountsPoissonNotUseExp
    diffLogLik <- demest:::diffLogLik
    initialModel <- demest:::initialModel
    getIAfter <- dembase::getIAfter
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    logLikelihood <- demest:::logLikelihood
    makeIOther <- demest:::makeIOther
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        y <- Counts(array(as.integer(rpois(24, lambda = 10)),
                          dim = c(6, 4),
                          dimnames = list(age = 0:5, reg = letters[1:4])))
        transformSubtotals <- new("CollapseTransformExtra",
                                  indices = list(rep(1L, 6L), c(1:3, 0L)),
                                  dims = c(0L, 1L),
                                  dimBefore = c(6L, 4L),
                                  dimAfter = 3L,
                                  multiplierBefore = c(1L, 6L),
                                  multiplierAfter = 1L,
                                  invIndices = list(list(1:6), list(1L, 2L, 3L)))
        subtotals <- collapse(y, transform = transformSubtotals)
        y <- new("CountsWithSubtotalsInternal",
                 y,
                 subtotals = as.integer(subtotals),
                 metadataSubtotals = subtotals@metadata,
                 transformSubtotals = transformSubtotals)
        model <- initialModel(Model(y ~ Poisson(mean ~ reg + age)),
                              y = y, exposure = NULL)
        datasets <- list(Counts(array(as.integer(rpois(3, lambda = 20)),
                                      dim = 3,
                                      dimnames = list(reg = letters[1:3]))),
                         Counts(array(as.integer(rpois(18, lambda = 10)),
                                      dim = c(6, 3),
                                      dimnames = list(age = 0:5, reg = letters[1:3]))))
        observation <- vector("list", 2)
        transforms <- vector("list", 2)
        for (i in 1:2) {
            transforms[[i]] <- makeCollapseTransformExtra(makeTransform(x = y,
                                                                        y = datasets[[i]],
                                                                        subset = TRUE))
            observation[[i]] <- initialModel(Model(y ~ Poisson(mean ~ 1)),
                                             y = datasets[[i]],
                                             exposure = collapse(y, transforms[[i]]))
        }
        set.seed(seed)
        ans.obtained <- updateCountsPoissonNotUseExp(y = y,
                                                     model = model,
                                                     observation = observation,
                                                     datasets = datasets,
                                                     transforms = transforms)
        set.seed(seed)
        y.tmp <- y
        for (i in 1:18) {
            i.other <- makeIOther(i = i, transform = transformSubtotals)
            y.prop <- as.integer(rmultinom(n = 1L,
                                           size = sum(y.tmp[c(i, i.other)]),
                                           prob = model@theta[c(i, i.other)]))
            diff.log.lik <- diffLogLik(yProp = y.prop,
                                       y = y.tmp,
                                       indicesY = c(i, i.other),
                                       observation = observation,
                                       datasets = datasets,
                                       transforms = transforms)
            if ((diff.log.lik >= 0) || (runif(1) < exp(diff.log.lik)))
                y.tmp[c(i, i.other)] <- y.prop
        }
        for (i in 19:24) {
            y.prop <- as.integer(rpois(n = 1, lambda = model@theta[i]))
            diff.log.lik <- diffLogLik(yProp = y.prop,
                                       y = y.tmp,
                                       indicesY = i,
                                       observation = observation,
                                       datasets = datasets,
                                       transforms = transforms)
            if ((diff.log.lik >= 0) || (runif(1) < exp(diff.log.lik)))
                y.tmp[i] <- y.prop
        }
        ans.expected <- y.tmp
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of updateCountsPoissonNotUseExp give same answer with subtotals made from collapsed y", {
    updateCountsPoissonNotUseExp <- demest:::updateCountsPoissonNotUseExp
    diffLogLik <- demest:::diffLogLik
    initialModel <- demest:::initialModel
    getIAfter <- dembase::getIAfter
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    logLikelihood <- demest:::logLikelihood
    makeIOther <- demest:::makeIOther
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        y <- Counts(array(as.integer(rpois(24, lambda = 10)),
                          dim = c(6, 4),
                          dimnames = list(age = 0:5, reg = letters[1:4])))
        transformSubtotals <- new("CollapseTransformExtra",
                                  indices = list(rep(1L, 6L), c(1:3, 0L)),
                                  dims = c(0L, 1L),
                                  dimBefore = c(6L, 4L),
                                  dimAfter = 3L,
                                  multiplierBefore = c(1L, 6L),
                                  multiplierAfter = 1L,
                                  invIndices = list(list(1:6), list(1L, 2L, 3L)))
        subtotals <- collapse(y, transform = transformSubtotals)
        y <- new("CountsWithSubtotalsInternal",
                 y,
                 subtotals = as.integer(subtotals),
                 metadataSubtotals = subtotals@metadata,
                 transformSubtotals = transformSubtotals)
        model <- initialModel(Model(y ~ Poisson(mean ~ reg + age)),
                              y = y, exposure = NULL)
        datasets <- list(Counts(array(as.integer(rpois(3, lambda = 20)),
                                      dim = 3,
                                      dimnames = list(reg = letters[1:3]))),
                         Counts(array(as.integer(rpois(18, lambda = 10)),
                                      dim = c(6, 3),
                                      dimnames = list(age = 0:5, reg = letters[1:3]))))
        observation <- vector("list", 2)
        transforms <- vector("list", 2)
        for (i in 1:2) {
            transforms[[i]] <- makeCollapseTransformExtra(makeTransform(x = y,
                                                                        y = datasets[[i]],
                                                                        subset = TRUE))
            observation[[i]] <- initialModel(Model(y ~ Poisson(mean ~ 1)),
                                             y = datasets[[i]],
                                             exposure = collapse(y, transforms[[i]]))
        }
        set.seed(seed)
        ans.R <- updateCountsPoissonNotUseExp(y = y,
                                              model = model,
                                              observation = observation,
                                              datasets = datasets,
                                              transforms = transforms,
                                              useC = FALSE)
        set.seed(seed)
        ans.C <- updateCountsPoissonNotUseExp(y = y,
                                              model = model,
                                              observation = observation,
                                              datasets = datasets,
                                              transforms = transforms,
                                              useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})



## updateCounts - PoissonUseExp

test_that("R version of updateCountsPoissonUseExp works with no subtotals", {
    updateCountsPoissonUseExp <- demest:::updateCountsPoissonUseExp
    diffLogLik <- demest:::diffLogLik
    initialModel <- demest:::initialModel
    getIAfter <- dembase::getIAfter
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    logLikelihood <- demest:::logLikelihood
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        exposure <- Counts(array(rgamma(n = 48, shape = 2, rate = 0.2),
                                 dim = c(6, 2, 4),
                                 dimnames = list(age = 0:5, sex = c("f", "m"), reg = letters[1:4])))
        y <- Counts(array(as.integer(rpois(48, lambda = exposure * 0.3)),
                          dim = c(6, 2, 4),
                          dimnames = list(age = 0:5, sex = c("f", "m"), reg = letters[1:4])))
        model <- initialModel(Model(y ~ Poisson(mean ~ reg + sex + age)),
                              y = y,
                              exposure = exposure)
        datasets <- list(Counts(array(as.integer(rpois(n = 24, lambda = collapseDimension(y, dimension = "sex"))),
                                      dim = c(6, 4),
                                      dimnames = list(age = 0:5, reg = letters[1:4]))),
                         Counts(array(as.integer(rpois(n = 36, lambda = y[,,1:3])),
                                      dim = c(6, 2, 3),
                                      dimnames = list(age = 0:5, sex = c("f", "m"), reg = letters[1:3]))))
        observation <- vector("list", 2)
        transforms <- vector("list", 2)
        for (i in 1:2) {
            transforms[[i]] <- makeCollapseTransformExtra(makeTransform(x = y,
                                                                        y = datasets[[i]],
                                                                        subset = TRUE))
            observation[[i]] <- initialModel(Model(y ~ Poisson(mean ~ 1)),
                                             y = datasets[[i]],
                                             exposure = collapse(y, transforms[[i]]))
        }
        set.seed(seed)
        ans.obtained <- updateCountsPoissonUseExp(y = y,
                                                  model = model,
                                                  exposure = exposure,
                                                  observation = observation,
                                                  datasets = datasets,
                                                  transforms = transforms)
        set.seed(seed)
        ans.expected <- y
        for (i in seq_along(y)) {
            y.prop <- as.integer(rpois(n = 1, lambda = model@theta[i] * exposure[i]))
            diff.log.lik <- diffLogLik(yProp = y.prop,
                                       y = ans.expected,
                                       indicesY = i,
                                       observation = observation,
                                       datasets = datasets,
                                       transforms = transforms)
            if ((diff.log.lik >= 0) || (runif(1) < exp(diff.log.lik)))
                ans.expected[i] <- y.prop
        }
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of updateCountsPoissonUseExp give same answer with no subtotals", {
    updateCountsPoissonUseExp <- demest:::updateCountsPoissonUseExp
    diffLogLik <- demest:::diffLogLik
    initialModel <- demest:::initialModel
    getIAfter <- dembase::getIAfter
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    logLikelihood <- demest:::logLikelihood
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        exposure <- Counts(array(rgamma(n = 48, shape = 2, rate = 0.2),
                                 dim = c(6, 2, 4),
                                 dimnames = list(age = 0:5, sex = c("f", "m"), reg = letters[1:4])))
        y <- Counts(array(as.integer(rpois(48, lambda = exposure * 0.3)),
                          dim = c(6, 2, 4),
                          dimnames = list(age = 0:5, sex = c("f", "m"), reg = letters[1:4])))
        model <- initialModel(Model(y ~ Poisson(mean ~ reg + sex + age)),
                              y = y,
                              exposure = exposure)
        datasets <- list(Counts(array(as.integer(rpois(n = 24, lambda = collapseDimension(y, dimension = "sex"))),
                                      dim = c(6, 4),
                                      dimnames = list(age = 0:5, reg = letters[1:4]))),
                         Counts(array(as.integer(rpois(n = 36, lambda = y[,,1:3])),
                                      dim = c(6, 2, 3),
                                      dimnames = list(age = 0:5, sex = c("f", "m"), reg = letters[1:3]))))
        observation <- vector("list", 2)
        transforms <- vector("list", 2)
        for (i in 1:2) {
            transforms[[i]] <- makeCollapseTransformExtra(makeTransform(x = y,
                                                                        y = datasets[[i]],
                                                                        subset = TRUE))
            observation[[i]] <- initialModel(Model(y ~ Poisson(mean ~ 1)),
                                             y = datasets[[i]],
                                             exposure = collapse(y, transforms[[i]]))
        }
        set.seed(seed)
        ans.R <- updateCountsPoissonUseExp(y = y,
                                           model = model,
                                           exposure = exposure,
                                           observation = observation,
                                           datasets = datasets,
                                           transforms = transforms,
                                           useC = FALSE)
        set.seed(seed)
        ans.C <- updateCountsPoissonUseExp(y = y,
                                           model = model,
                                           exposure = exposure,
                                           observation = observation,
                                           datasets = datasets,
                                           transforms = transforms,
                                           useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("R version of updateCountsPoissonUseExp works with subtotals made from collapsed y", {
    updateCountsPoissonUseExp <- demest:::updateCountsPoissonUseExp
    diffLogLik <- demest:::diffLogLik
    initialModel <- demest:::initialModel
    getIAfter <- dembase::getIAfter
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    logLikelihood <- demest:::logLikelihood
    makeIOther <- demest:::makeIOther
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        y <- Counts(array(as.integer(rpois(24, lambda = 10)),
                          dim = c(6, 4),
                          dimnames = list(age = 0:5, reg = letters[1:4])))
        exposure <- Counts(array(runif(24, max = 20),
                                 dim = c(6, 4),
                                 dimnames = list(age = 0:5, reg = letters[1:4])))
        transformSubtotals <- new("CollapseTransformExtra",
                                  indices = list(rep(1L, 6L), c(1:3, 0L)),
                                  dims = c(0L, 1L),
                                  dimBefore = c(6L, 4L),
                                  dimAfter = 3L,
                                  multiplierBefore = c(1L, 6L),
                                  multiplierAfter = 1L,
                                  invIndices = list(list(1:6), list(1L, 2L, 3L)))
        subtotals <- collapse(y, transform = transformSubtotals)
        y <- new("CountsWithSubtotalsInternal",
                 y,
                 subtotals = as.integer(subtotals),
                 metadataSubtotals = subtotals@metadata,
                 transformSubtotals = transformSubtotals)
        model <- initialModel(Model(y ~ Poisson(mean ~ reg + age)),
                              y = y,
                              exposure = exposure)
        datasets <- list(Counts(array(as.integer(rpois(3, lambda = 20)),
                                      dim = 3,
                                      dimnames = list(reg = letters[1:3]))),
                         Counts(array(as.integer(rpois(18, lambda = 10)),
                                      dim = c(6, 3),
                                      dimnames = list(age = 0:5, reg = letters[1:3]))))
        observation <- vector("list", 2)
        transforms <- vector("list", 2)
        for (i in 1:2) {
            transforms[[i]] <- makeCollapseTransformExtra(makeTransform(x = y,
                                                                        y = datasets[[i]],
                                                                        subset = TRUE))
            observation[[i]] <- initialModel(Model(y ~ Poisson(mean ~ 1)),
                                             y = datasets[[i]],
                                             exposure = collapse(y, transforms[[i]]))
        }
        set.seed(seed)
        ans.obtained <- updateCountsPoissonUseExp(y = y,
                                                  exposure = exposure,
                                                  model = model,
                                                  observation = observation,
                                                  datasets = datasets,
                                                  transforms = transforms)
        set.seed(seed)
        y.tmp <- y
        for (i in 1:18) {
            i.other <- makeIOther(i = i, transform = transformSubtotals)
            y.prop <- as.integer(rmultinom(n = 1L,
                                           size = sum(y.tmp[c(i, i.other)]),
                                           prob = model@theta[c(i, i.other)] * exposure[c(i, i.other)]))
            diff.log.lik <- diffLogLik(yProp = y.prop,
                                       y = y.tmp,
                                       indicesY = c(i, i.other),
                                       observation = observation,
                                       datasets = datasets,
                                       transforms = transforms)
            if ((diff.log.lik >= 0) || (runif(1) < exp(diff.log.lik)))
                y.tmp[c(i, i.other)] <- y.prop
        }
        for (i in 19:24) {
            y.prop <- as.integer(rpois(n = 1, lambda = model@theta[i] * exposure[i]))
            diff.log.lik <- diffLogLik(yProp = y.prop,
                                       y = y.tmp,
                                       indicesY = i,
                                       observation = observation,
                                       datasets = datasets,
                                       transforms = transforms)
            if ((diff.log.lik >= 0) || (runif(1) < exp(diff.log.lik)))
                y.tmp[i] <- y.prop
        }
        ans.expected <- y.tmp
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of updateCountsPoissonUseExp give same answer with subtotals made from collapsed y", {
    updateCountsPoissonUseExp <- demest:::updateCountsPoissonUseExp
    diffLogLik <- demest:::diffLogLik
    initialModel <- demest:::initialModel
    getIAfter <- dembase::getIAfter
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    logLikelihood <- demest:::logLikelihood
    makeIOther <- demest:::makeIOther
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        y <- Counts(array(as.integer(rpois(24, lambda = 10)),
                          dim = c(6, 4),
                          dimnames = list(age = 0:5, reg = letters[1:4])))
        exposure <- Counts(array(runif(24, max = 20),
                                 dim = c(6, 4),
                                 dimnames = list(age = 0:5, reg = letters[1:4])))
        transformSubtotals <- new("CollapseTransformExtra",
                                  indices = list(rep(1L, 6L), c(1:3, 0L)),
                                  dims = c(0L, 1L),
                                  dimBefore = c(6L, 4L),
                                  dimAfter = 3L,
                                  multiplierBefore = c(1L, 6L),
                                  multiplierAfter = 1L,
                                  invIndices = list(list(1:6), list(1L, 2L, 3L)))
        subtotals <- collapse(y, transform = transformSubtotals)
        y <- new("CountsWithSubtotalsInternal",
                 y,
                 subtotals = as.integer(subtotals),
                 metadataSubtotals = subtotals@metadata,
                 transformSubtotals = transformSubtotals)
        model <- initialModel(Model(y ~ Poisson(mean ~ reg + age)),
                              y = y,
                              exposure = exposure)
        datasets <- list(Counts(array(as.integer(rpois(3, lambda = 20)),
                                      dim = 3,
                                      dimnames = list(reg = letters[1:3]))),
                         Counts(array(as.integer(rpois(18, lambda = 10)),
                                      dim = c(6, 3),
                                      dimnames = list(age = 0:5, reg = letters[1:3]))))
        observation <- vector("list", 2)
        transforms <- vector("list", 2)
        for (i in 1:2) {
            transforms[[i]] <- makeCollapseTransformExtra(makeTransform(x = y,
                                                                        y = datasets[[i]],
                                                                        subset = TRUE))
            observation[[i]] <- initialModel(Model(y ~ Poisson(mean ~ 1)),
                                             y = datasets[[i]],
                                             exposure = collapse(y, transforms[[i]]))
        }
        set.seed(seed)
        ans.R <- updateCountsPoissonUseExp(y = y,
                                           exposure = exposure,
                                           model = model,
                                           observation = observation,
                                           datasets = datasets,
                                           transforms = transforms,
                                           useC = FALSE)
        set.seed(seed)
        ans.C <- updateCountsPoissonUseExp(y = y,
                                           exposure = exposure,
                                           model = model,
                                           observation = observation,
                                           datasets = datasets,
                                           transforms = transforms,
                                           useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("R version of updateCountsBinomial works", {
    updateCountsBinomial <- demest:::updateCountsBinomial
    diffLogLik <- demest:::diffLogLik
    initialModel <- demest:::initialModel
    getIAfter <- dembase::getIAfter
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    logLikelihood <- demest:::logLikelihood
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        exposure <- Counts(array(as.integer(rpois(n = 48, lambda = 20)),
                                 dim = c(6, 2, 4),
                                 dimnames = list(age = 0:5, sex = c("f", "m"), reg = letters[1:4])))
        y <- Counts(array(as.integer(rbinom(48, size = exposure, prob = 0.5)),
                          dim = c(6, 2, 4),
                          dimnames = list(age = 0:5, sex = c("f", "m"), reg = letters[1:4])))
        model <- initialModel(Model(y ~ Binomial(mean ~ reg + sex + age)),
                              y = y,
                              exposure = exposure)
        datasets <- list(Counts(array(as.integer(rpois(n = 24, lambda = collapseDimension(y, dimension = "sex"))),
                                      dim = c(6, 4),
                                      dimnames = list(age = 0:5, reg = letters[1:4]))),
                         Counts(array(as.integer(rpois(n = 36, lambda = y[,,1:3])),
                                      dim = c(6, 2, 3),
                                      dimnames = list(age = 0:5, sex = c("f", "m"), reg = letters[1:3]))))
        observation <- vector("list", 2)
        transforms <- vector("list", 2)
        for (i in 1:2) {
            transforms[[i]] <- makeCollapseTransformExtra(makeTransform(x = y,
                                                                        y = datasets[[i]],
                                                                        subset = TRUE))
            observation[[i]] <- initialModel(Model(y ~ Poisson(mean ~ 1)),
                                             y = datasets[[i]],
                                             exposure = collapse(y, transforms[[i]]))
        }
        set.seed(seed)
        ans.obtained <- updateCountsBinomial(y = y,
                                             model = model,
                                             exposure = exposure,
                                             observation = observation,
                                             datasets = datasets,
                                             transforms = transforms)
        set.seed(seed)
        ans.expected <- y
        for (i in seq_along(y)) {
            y.prop <- as.integer(rbinom(n = 1, size = exposure[i], prob = model@theta[i]))
            diff.log.lik <- diffLogLik(yProp = y.prop,
                                       y = ans.expected,
                                       indicesY = i,
                                       observation = observation,
                                       datasets = datasets,
                                       transforms = transforms)
            if ((diff.log.lik >= 0) || (runif(1) < exp(diff.log.lik)))
                ans.expected[i] <- y.prop
        }
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of updateCountsBinomial give same answer", {
    updateCountsBinomial <- demest:::updateCountsBinomial
    diffLogLik <- demest:::diffLogLik
    initialModel <- demest:::initialModel
    getIAfter <- dembase::getIAfter
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    logLikelihood <- demest:::logLikelihood
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        exposure <- Counts(array(as.integer(rpois(n = 48, lambda = 20)),
                                 dim = c(6, 2, 4),
                                 dimnames = list(age = 0:5, sex = c("f", "m"), reg = letters[1:4])))
        y <- Counts(array(as.integer(rbinom(48, size = exposure, prob = 0.5)),
                          dim = c(6, 2, 4),
                          dimnames = list(age = 0:5, sex = c("f", "m"), reg = letters[1:4])))
        model <- initialModel(Model(y ~ Binomial(mean ~ reg + sex + age)),
                              y = y,
                              exposure = exposure)
        datasets <- list(Counts(array(as.integer(rpois(n = 24, lambda = collapseDimension(y, dimension = "sex"))),
                                      dim = c(6, 4),
                                      dimnames = list(age = 0:5, reg = letters[1:4]))),
                         Counts(array(as.integer(rpois(n = 36, lambda = y[,,1:3])),
                                      dim = c(6, 2, 3),
                                      dimnames = list(age = 0:5, sex = c("f", "m"), reg = letters[1:3]))))
        observation <- vector("list", 2)
        transforms <- vector("list", 2)
        for (i in 1:2) {
            transforms[[i]] <- makeCollapseTransformExtra(makeTransform(x = y,
                                                                        y = datasets[[i]],
                                                                        subset = TRUE))
            observation[[i]] <- initialModel(Model(y ~ Poisson(mean ~ 1)),
                                             y = datasets[[i]],
                                             exposure = collapse(y, transforms[[i]]))
        }
        set.seed(seed)
        ans.R <- updateCountsBinomial(y = y,
                                      model = model,
                                      exposure = exposure,
                                      observation = observation,
                                      datasets = datasets,
                                      transforms = transforms,
                                      useC = FALSE)
        set.seed(seed)
        ans.C <- updateCountsBinomial(y = y,
                                      model = model,
                                      exposure = exposure,
                                      observation = observation,
                                      datasets = datasets,
                                      transforms = transforms,
                                      useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})


## updateObservation ######################################################

test_that("R version of updateObservationCounts works", {
    updateObservationCounts <- demest:::updateObservationCounts
    initialModel <- demest:::initialModel
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    updateModelUseExp <- demest:::updateModelUseExp
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        yProp <- rpois(n = 1, lambda = 10)
        y <- Counts(array(as.integer(rpois(30, lambda = 10)),
                          dim = c(6, 5),
                          dimnames = list(age = 0:5, reg = letters[1:5])))
        spec <- Model(y ~ Poisson(mean ~ reg + age))
        model <- initialModel(spec, y = y, exposure = NULL)
        datasets <- list(Counts(array(as.integer(rpois(4, lambda = 10)),
                                      dim = 4,
                                      dimnames = list(reg = letters[1:4]))),
                         toInteger(y[,1:3] / 2, force = TRUE))
        transforms <- list(makeTransform(x = y,
                                         y = datasets[[1]],
                                         subset = TRUE),
                           makeTransform(x = y,
                                         y = datasets[[2]],
                                         subset = TRUE))
        transforms <- lapply(transforms, makeCollapseTransformExtra)
        observation <- list(initialModel(Model(y ~ Poisson(mean ~ 1)),
                                         y = datasets[[1]],
                                         exposure = toDouble(collapse(y, transforms[[1]]))),
                            initialModel(Model(y ~ Binomial(mean ~ 1)),
                                         y = datasets[[2]],
                                         exposure = toDouble(collapse(y, transforms[[2]]))))
        set.seed(seed + 1)
        ans.obtained <- updateObservationCounts(y = y,
                                                observation = observation,
                                                datasets = datasets,
                                                transforms = transforms)
        set.seed(seed + 1)
        ans.expected <- observation
        ans.expected[[1]] <- updateModelUseExp(ans.expected[[1]],
                                               y = datasets[[1]],
                                               exposure = toDouble(collapse(y,
                                                   transform = transforms[[1]])))
        ans.expected[[2]] <- updateModelUseExp(ans.expected[[2]],
                                               y = datasets[[2]],
                                               exposure = collapse(y,
                                                   transform = transforms[[2]]))
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of updateObservationCounts give same answer", {
    updateObservationCounts <- demest:::updateObservationCounts
    initialModel <- demest:::initialModel
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        y <- Counts(array(as.integer(rpois(30, lambda = 10)),
                          dim = c(6, 5),
                          dimnames = list(age = 0:5, reg = letters[1:5])))
        spec <- Model(y ~ Poisson(mean ~ reg + age))
        model <- initialModel(spec, y = y, exposure = NULL)
        datasets <- list(Counts(array(as.integer(rpois(4, lambda = 10)),
                                      dim = 4,
                                      dimnames = list(reg = letters[1:4]))),
                         toInteger(y[,1:3] / 2, force = TRUE))
        transforms <- list(makeTransform(x = y,
                                         y = datasets[[1]],
                                         subset = TRUE),
                           makeTransform(x = y,
                                         y = datasets[[2]],
                                         subset = TRUE))
        transforms <- lapply(transforms, makeCollapseTransformExtra)
        observation <- list(initialModel(Model(y ~ Poisson(mean ~ 1)),
                                         y = datasets[[1]],
                                         exposure = toDouble(collapse(y, transforms[[1]]))),
                            initialModel(Model(y ~ Binomial(mean ~ 1)),
                                         y = datasets[[2]],
                                         exposure = toDouble(collapse(y, transforms[[2]]))))
        set.seed(seed + 1)
        ans.R <- updateObservationCounts(y = y,
                                         observation = observation,
                                         datasets = datasets,
                                         transforms = transforms,
                                         useC = FALSE)
        set.seed(seed + 1)
        ans.C <- updateObservationCounts(y = y,
                                         observation = observation,
                                         datasets = datasets,
                                         transforms = transforms,
                                         useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("R version of updateObservationCounts works with aggregate data model", {
    updateObservationCounts <- demest:::updateObservationCounts
    initialModel <- demest:::initialModel
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    updateModelUseExp <- demest:::updateModelUseExp
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        yProp <- rpois(n = 1, lambda = 10)
        y <- Counts(array(as.integer(rpois(30, lambda = 10)),
                          dim = c(6, 5),
                          dimnames = list(age = 0:5, reg = letters[1:5])))
        model <- initialModel(Model(y ~ Poisson(mean ~ reg + age)),
                              y = y, exposure = NULL)
        datasets <- list(Counts(array(as.integer(rpois(4, lambda = 20)),
                                      dim = 4,
                                      dimnames = list(reg = letters[1:4]))))
        aggregate <- AgNormal(value = mean(y) / mean(datasets[[1]]), sd = 0.01)
        transform <- makeCollapseTransformExtra(makeTransform(x = y,
                                                              y = datasets[[1]],
                                                              subset = TRUE))
        observation <- list(initialModel(Model(y ~ Poisson(mean ~ 1),
                                               aggregate = aggregate),
                                         y = datasets[[1]],
                                         exposure = toDouble(collapse(y, transform))))
        set.seed(seed + 1)
        ans.obtained <- updateObservationCounts(y = y,
                                                observation = observation,
                                                datasets = datasets,
                                                transforms = list(transform))
        set.seed(seed + 1)
        ans.expected <- list(updateModelUseExp(observation[[1]],
                                               y = datasets[[1]],
                                               exposure = toDouble(collapse(y,
                                                   transform))))
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of updateObservationCounts give same answer with aggregate data model", {
    updateObservationCounts <- demest:::updateObservationCounts
    initialModel <- demest:::initialModel
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    updateModelUseExp <- demest:::updateModelUseExp
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        yProp <- rpois(n = 1, lambda = 10)
        y <- Counts(array(as.integer(rpois(30, lambda = 10)),
                          dim = c(6, 5),
                          dimnames = list(age = 0:5, reg = letters[1:5])))
        model <- initialModel(Model(y ~ Poisson(mean ~ reg + age)),
                              y = y, exposure = NULL)
        datasets <- list(Counts(array(as.integer(rpois(4, lambda = 20)),
                                      dim = 4,
                                      dimnames = list(reg = letters[1:4]))))
        aggregate <- AgNormal(value = mean(y) / mean(datasets[[1]]), sd = 0.01)
        transform <- makeCollapseTransformExtra(makeTransform(x = y,
                                                              y = datasets[[1]],
                                                              subset = TRUE))
        observation <- list(initialModel(Model(y ~ Poisson(mean ~ 1),
                                                           aggregate = aggregate),
                                         y = datasets[[1]],
                                         exposure = toDouble(collapse(y, transform))))
        set.seed(seed + 1)
        ans.R <- updateObservationCounts(y = y,
                                         observation = observation,
                                         datasets = datasets,
                                         transforms = list(transform),
                                         useC = FALSE)
        set.seed(seed + 1)
        ans.C <- updateObservationCounts(y = y,
                                         observation = observation,
                                         datasets = datasets,
                                         transforms = list(transform),
                                         useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})


