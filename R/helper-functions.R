
## DEMOGRAPHIC OBJECTS #############################################################

## HAS_TESTS
checkAllTermsInFormulaSpecified <- function(formula, namesSpecPriors) {
    labels <- attr(stats::terms(formula), "term.labels")
    not.specified <- setdiff(labels, namesSpecPriors)
    n.not.specified <- length(not.specified)
    if (n.not.specified > 0L) {
        stop(sprintf(ngettext(n.not.specified,
                              "no prior specified for term %s in formula '%s'",
                              "no priors specified for terms %s in formula '%s'"),
                     paste(sQuote(not.specified), collapse = ", "),
                     deparse(formula)))
    }
    NULL
}



listAllSubsets <- function(n) {
    if (n == 0L)
        stop(gettext("no dimensions"))
    else if (n == 1L)
        list()
    else {
        s <- seq_len(n)
        makeCombnOrderI <- function(i) utils::combn(s, i, simplify = FALSE)
        ans <- lapply(seq_len(n - 1L), makeCombnOrderI)
        unlist(ans, recursive = FALSE)
    }
}



## makeFakeBetas

## HAS_TESTS
makeIteratorBetas <- function(betas, namesBetas, y) {
    n <- length(betas)
    names.y <- names(y)
    dim.y <- dim(y)
    margins <- vector(mode = "list", length = n)
    margins[[1L]] <- 0L
    if (n > 1L) {
        for (i in seq.int(from = 2L, to = n)) {
            name.split <- strsplit(namesBetas[i], split = ":", fixed = TRUE)[[1L]]
            margins[[i]] <- match(name.split, names.y)
        }
    }
    BetaIterator(dim = dim.y, margins = margins)
}

## TRANSLATED
## HAS_TESTS
makeMu <- function(n, betas, iterator, useC = FALSE) {
    ## n
    stopifnot(is.integer(n))
    stopifnot(!is.na(n))
    stopifnot(n > 0L)
    ## betas
    stopifnot(is.list(betas))
    stopifnot(all(sapply(betas, is.numeric)))
    stopifnot(all(sapply(betas, function(x) !any(is.na(x)))))
    ## iterator
    stopifnot(methods::is(iterator, "BetaIterator"))
    if (useC) {
        .Call(makeMu_R, n, betas, iterator)
    }
    else {
        iterator <- resetB(iterator)
        mu <- numeric(n)
        for (i in seq_len(n)) {
            indices <- iterator@indices
            mu[i] <- 0
            for (b in seq_along(betas))
                mu[i] <- mu[i] + betas[[b]][indices[b]]
            iterator <- advanceB(iterator)
        }
        mu
    }
}

## HAS_TESTS
## Redistribute values in 'x' to ensure that none
## exceeds the corresponding value in 'max',
## while respecting 'subtotal'.
maxWithSubtotal <- function(x, max, subtotal) {
    if (!is.integer(x))
        stop(gettextf("'%s' does not have type \"%s\"",
                      "x", "integer"))
    if (any(x < 0, na.rm = TRUE))
        stop(gettextf("'%s' has negative values",
                      "x"))
    if (!is.integer(max))
        stop(gettextf("'%s' does not have type \"%s\"",
                      "max", "integer"))
    if (any(is.na(max)))
        stop(gettextf("'%s' has missing values",
                      "max"))
    if (!identical(length(x), length(max)))
        stop(gettextf("'%s' and '%s' have different lengths",
                      "x", "max"))
    if (!identical(length(subtotal), 1L))
        stop(gettextf("'%s' does not have length %d",
                      "subtotal", 1L))
    if (!is.integer(subtotal))
        stop(gettextf("'%s' does not have type \"%s\"",
                      "subtotal", "integer"))
    if (is.na(subtotal))
        stop(gettextf("'%s' is missing",
                      "subtotal"))
    if (sum(max) < subtotal)
        stop(gettextf("'%s' and '%s' incompatible",
                      "max", "subtotal"))
    has.missing <- any(is.na(x))
    if (has.missing) {
        if (sum(x, na.rm = TRUE) > subtotal)
            stop(gettextf("'%s' and '%s' incompatible",
                          "x", "subtotal"))
        is.more.than.max <- !is.na(x) & (x > max)
        x[is.more.than.max] <- max[is.more.than.max]
        x
    }
    else {
        if (sum(x) != subtotal)
            stop(gettextf("'%s' and '%s' incompatible",
                          "x", "subtotal"))
        else if (sum(max) == subtotal)
            max
        else {
            is.more.than.max <- x > max
            total.redistribute <- sum(x[is.more.than.max]) - sum(max[is.more.than.max])
            x[is.more.than.max] <- max[is.more.than.max]
            for (i in seq_len(total.redistribute)) {
                is.less.than.max <- x < max  ## recalculate each iteration
                ## the following two lines are slightly awkward, but are necessary
                ## because 'sample' assumes that if the first argument has length 1, it
                ## is the length of the vector to choose values from
                prob <- ifelse(is.less.than.max, max - x, 0)
                i.add <- sample(seq_along(x), size = 1L, prob = prob)
                x[i.add] <- x[i.add] + 1L
            }
            x
        }
    }
}


## RANDOM VARIATES #################################################################

## TRANSLATED
## HAS_TESTS
dpoibin1 <- function(x, size, prob, log = FALSE, useC = FALSE) {
    for (name in c("x", "size")) {
        value <- get(name)
        if (!identical(length(value), 1L))
            stop(gettextf("'%s' does not have length %d",
                          name, 1L))
        if (!is.integer(value))
            stop(gettextf("'%s' does not have type \"%s\"",
                          name, "integer"))
        if (is.na(value))
            stop(gettextf("'%s' is missing",
                          name))
        if (value < 0L)
            stop(gettextf("'%s' is negative", name))
    }
    if (!identical(length(prob), 1L))
        stop(gettextf("'%s' does not have length %d",
                      "prob", 1L))
    if (!is.double(prob))
        stop(gettextf("'%s' does not have type \"%s\"",
                      "prob", "double"))
    if (is.na(prob))
        stop(gettextf("'%s' is missing",
                      "prob"))
    if (prob < 0)
        stop(gettextf("'%s' is negative", "prob"))
    if (prob > 1)
        stop(gettextf("'%s' is greater than %d",
                      "prob", 1L))
    if (!is.logical(log))
        stop(gettextf("'%s' does not have type \"%s\"", "log", "logical"))
    if (!identical(length(log), 1L))
        stop(gettextf("'%s' does not have length %d", "log", 1L))
    if (is.na(log))
        stop(gettextf("'%s' is missing", "log"))
    if (useC) {
        .Call(dpoibin1_R, x, size, prob, log)
    }
    else {
        kThreshold <- 50
        lambda <- (1 - prob) * size
        if (x > kThreshold) {
            mean.binom <- prob * size
            var.binom <- prob * (1 - prob) * size
            mean.pois <- lambda
            var.pois <- lambda
            mean <- mean.binom + mean.pois
            var <- var.binom + var.pois
            sd <- sqrt(var)
            ans <- stats::dnorm(x, mean = mean, sd = sd, log = log)
        }
        else {
            limit <- min(x, size)
            ans <- 0
            for (i in seq.int(from = 0L, to = limit)) {
                prob.binom <- stats::dbinom(i, size = size, prob = prob)
                prob.pois <- stats::dpois(x - i, lambda = lambda)
                ans <- ans + prob.binom * prob.pois
            }
            if (log)
                ans <- log(ans)
        }
        ans
    }
}

## TRANSLATED
## HAS_TESTS
invlogit1 <- function(x, useC = FALSE) {
    if (!identical(length(x), 1L))
        stop(gettextf("'%s' does not have length %d",
                      "x", 1L))
    if (!is.numeric(x))
        stop(gettextf("'%s' is non-numeric",
                      "x"))
    if (is.na(x))
        stop(gettextf("'%s' is missing",
                      "x"))
    if (useC) {
        .Call(invlogit1_R, x)
    }
    else {
        if (x > 0)
            1 / (1 + exp(-x))
        else
            exp(x) / (1 + exp(x))
    }
}

## TRANSLATED
## HAS_TESTS
## Generate one random deviate from a categorical distribution
## with cumulative probability given by 'cumProb'.
## Algorithm from Ripley (1987) Stochastic Simulation, p71
rcateg1 <- function(cumProb, useC = FALSE) {
    stopifnot(is.double(cumProb))
    stopifnot(length(cumProb) > 0L)
    stopifnot(!any(is.na(cumProb)))
    stopifnot(all(cumProb > 0))
    if (length(cumProb) > 1L)
        stopifnot(all(diff(cumProb) >= 0))
    stopifnot(all.equal(cumProb[length(cumProb)], 1))
    if (useC) {
        .Call(rcateg1_R, cumProb)
    }
    else {
        u <- stats::runif(n = 1L)
        i <- 1L
        while (cumProb[i] <= u)
            i <- i + 1L
        i
    }
}

#' The half-t distribution.
#'
#' Density, distribution function, quantile function and random
#' generation for the halt-t distribution.
#'
#' The half-t distribution is also known as the folded-t distribution.
#'
#' If \eqn{X} has a \emph{t} distribution with degrees of freedom
#' \eqn{v}, location 0, and scale \code{s}, then \eqn{|X|}
#' has a half-\emph{t} distribution with degrees of freedom \eqn{v}
#' and scale \code{s}.
#'
#' Internally, the functions all call the corresponding functions
#' for the \code{\link[=TDist]{t distribution}}.
#' 
#' @param x Vector of quantiles.
#' @param p Vector of quantiles.
#' @param q Vector of probabilities.
#' @param n Number of observations.
#' @param df Degrees of freedom. Positive. Can be non-integer,
#'     and can be \code{Inf}.
#' @param scale Dispersion parameter.
#'
#' @return \code{dhalft} gives the density, \code{phalft} gives
#'     the distribution function, \code{qhalft} gives the
#'     quantile function, and \code{rhalft} generates random
#'     deviates.
#'
#' @seealso Function \code{\link{plotHalfT}} plots density and distribution
#' functions for half-\emph{t} distributions.
#'
#' @references
#' Based on Brazauskas, V., and Kleefeld, A. (2011) Folded and log-folded-t
#' distributions as models for insurance loss data.
#' \emph{Scandinavian Actuarial Journal} 59-74.
#'
#' @examples
#' dhalft(x = 0.5, df = 7, scale = 0.5)
#' qhalft(p = 0.9, df = 4)
#' phalft(q = 0.5, df = 7, scale = 2)
#' rhalft(n = 5, df = 30)
#' @name halft-distn
#' @aliases rhalft, qhalft, phalft, dhalft
NULL

## HAS_TESTS
#' @rdname halft-distn
#' @export
rhalft <- function(n, df, scale = 1) {
    if (!is.numeric(scale))
        stop(gettextf("'%s' is non-numeric",
                      "scale"))
    if (any(scale[!is.na(scale)] <= 0))
        stop(gettextf("'%s' is non-positive",
                      "scale"))
    ans <- stats::rt(n = n, df = df)
    scale * abs(ans)
}

## HAS_TESTS        
#' @rdname halft-distn
#' @export
qhalft <- function(p, df, scale = 1) {
    if (!is.numeric(scale))
        stop(gettextf("'%s' is non-numeric",
                      "scale"))
    if (any(scale[!is.na(scale)] <= 0))
        stop(gettextf("'%s' is non-positive",
                      "scale"))
    p <- (p + 1) / 2
    ans <- stats::qt(p = p,
                     df = df)
    scale * ans
}

## HAS_TESTS
#' @rdname halft-distn
#' @export
phalft <- function(q, df, scale = 1) {
    if (!is.numeric(scale))
        stop(gettextf("'%s' is non-numeric",
                      "scale"))
    if (any(scale[!is.na(scale)] <= 0))
        stop(gettextf("'%s' is non-positive",
                      "scale"))
    q <- q / scale
    ans <- stats::pt(q = q,
                     df = df)
    2 * (ans - 0.5)
}

## HAS_TESTS
#' @rdname halft-distn
#' @export
dhalft <- function(x, df, scale = 1) {
    if (!is.numeric(scale))
        stop(gettextf("'%s' is non-numeric",
                      "scale"))
    if (any(scale[!is.na(scale)] <= 0))
        stop(gettextf("'%s' is non-positive",
                      "scale"))
    x <- x / scale
    (2/scale) * stats::dt(x = x, df = df, log = FALSE)
}

#' Plot the half-t distribution.
#'
#' Plot the density or distribution function of a
#' \code{\link[=halft-distn]{half-t}} distribution.
#'
#' @inheritParams halft-distn
#' @param max A quantile, defaulting to 0.999.  The x-axis for the plot
#' extends from 0 to this quantile.
#' @param density Whether to plot the density function (the default) or the
#' distribution function.
#' @param add Whether to add to the current plot.
#' @param \dots Other arguments, passed to functions \code{\link[graphics]{plot}}
#' and \code{\link[graphics]{lines}}.
#'
#' @examples
#' plotHalfT()
#' plotHalfT(df = 4, add = TRUE, col = "red")
#' plotHalfT(df = 4, scale = 1.1, add = TRUE, col = "blue")
#' @export
plotHalfT <- function(df = 7, scale = 1, max = 0.999,
                      density = TRUE, add = FALSE, ...) {
    xmax <- qhalft(p = max, df = df, scale = scale)
    x <- seq(from = 0, to = xmax, length.out = 500)
    if (density) {
        density <- dhalft(x = x, df = df, scale = scale)
        if (add)
            graphics::lines(x = x, y = density, ...)
        else
            graphics::plot(x = x, y = density, type = "l", ...)
    }
    else {
        prob <- phalft(q = x, df = df, scale = scale)
        if (add)
            graphics::lines(x = x, y = prob, ...)
        else
            graphics::plot(x = x, y = prob, type = "l", ...)
    }
}

## TRANSLATED
## HAS_TESTS
rhalftTrunc1 <- function(df, scale, max, useC = FALSE) {
    ## df
    stopifnot(is.double(df))
    stopifnot(identical(length(df), 1L))
    stopifnot(!is.na(df))
    stopifnot(df > 0)
    ## scale
    stopifnot(is.double(scale))
    stopifnot(identical(length(scale), 1L))
    stopifnot(!is.na(scale))
    stopifnot(scale > 0)
    ## max
    stopifnot(is.double(max))
    stopifnot(identical(length(max), 1L))
    stopifnot(!is.na(max))
    stopifnot(max > 0)
    if (useC) {
        .Call(rhalftTrunc1_R, df, scale, max)
    }
    else {
        kMaxAttempt <- 1000L
        for (i in seq_len(kMaxAttempt)) {
            ans <- stats::rt(n = 1L, df = df)
            ans <- scale * abs(ans)
            if (ans < max)
                return(ans)
        }
        stop("unable to generate value for truncated half-t (consider using higher maximum value)")
    }
}


## HAS_TESTS
rinvchisq1 <- function(df, scaleSq, useC = FALSE) {
    stopifnot(is.double(df))
    stopifnot(identical(length(df), 1L))
    stopifnot(!is.na(df))
    stopifnot(df > 0)
    stopifnot(is.double(scaleSq))
    stopifnot(identical(length(scaleSq), 1L))
    stopifnot(!is.na(scaleSq))
    stopifnot(scaleSq > 0)
    if (useC) {
        .Call(rinvchisq1_R, df, scaleSq)
    }
    else {
        X <- stats::rchisq(n = 1, df = df)
        df * scaleSq / X
    }
}

## TRANSLATED
## HAS_TESTS
rnormTruncated <- function(n, mean, sd, lower, upper, tolerance = 1e-5, maxAttempt,
                           uniform = TRUE, useC = FALSE) {
    ## n
    stopifnot(identical(length(n), 1L))
    stopifnot(is.integer(n))
    stopifnot(n > 0L)
    ## mean
    stopifnot(identical(length(mean), n))
    stopifnot(is.double(mean))
    stopifnot(!any(is.na(mean)))
    ## sd
    stopifnot(identical(length(sd), n))
    stopifnot(is.double(sd))
    stopifnot(!any(is.na(sd)))
    stopifnot(all(sd >= 0))
    ## lower
    stopifnot(is.double(lower))
    stopifnot(identical(length(lower), 1L))
    stopifnot(!is.na(lower))
    ## upper
    stopifnot(is.double(upper))
    stopifnot(identical(length(upper), 1L))
    stopifnot(!is.na(upper))
    ## tolerance
    stopifnot(is.double(tolerance))
    stopifnot(identical(length(tolerance), 1L))
    stopifnot(!is.na(tolerance))
    stopifnot(tolerance >= 0)
    ## maxAttempt
    stopifnot(identical(length(maxAttempt), 1L))
    stopifnot(is.integer(maxAttempt))
    stopifnot(!is.na(maxAttempt))
    stopifnot(maxAttempt > 0L)
    ## uniform
    stopifnot(identical(length(uniform), 1L))
    stopifnot(is.logical(uniform))
    stopifnot(!is.na(uniform))
    ## lower, upper
    stopifnot((lower + tolerance) < (upper - tolerance))
    if (useC) {
        .Call(rnormTruncated_R, n, mean, sd, lower, upper, tolerance, maxAttempt, uniform)
    }
    else {
        ans <- double(n)
        for (i in seq_len(n)) {
            found <- FALSE
            n.attempt <- 0L
            while (!found && (n.attempt < maxAttempt)) {
                n.attempt <- n.attempt + 1L
                prop.value <- stats::rnorm(n = 1L, mean = mean[i], sd = sd[i])
                found <- (prop.value > (lower + tolerance)) && (prop.value < (upper - tolerance))
            }
            if (found)
                ans[i] <- prop.value
            else {
                if (uniform) {
                    if (lower + tolerance > mean[i]) {
                        lower.unif <- lower + tolerance
                        upper.unif <- min(lower + tolerance + sd[i], upper - tolerance)
                    }
                    else {
                        upper.unif <- upper - tolerance
                        lower.unif <- max(upper - tolerance - sd[i], lower + tolerance)
                    }
                    ans[i] <- stats::runif(n = 1L,
                                           min = lower.unif,
                                           max = upper.unif)
                }
                else
                    stop("failed to generate value within specified range")
            }
        }
        ans
    }
}

## TRANSLATED
## HAS_TESTS
## Returns draw from truncated integer-only normal distribution (achieved by rounding).
rnormIntTrunc1 <- function(mean = 0, sd = 1, lower = NA_integer_, upper = NA_integer_, useC = FALSE) {
    ## mean
    stopifnot(is.double(mean))
    stopifnot(identical(length(mean), 1L))
    stopifnot(!is.na(mean))
    ## sd
    stopifnot(is.double(sd))
    stopifnot(identical(length(sd), 1L))
    stopifnot(!is.na(sd))
    stopifnot(sd > 0)
    ## lower
    stopifnot(is.integer(lower))
    stopifnot(identical(length(lower), 1L))
    ## upper
    stopifnot(is.integer(upper))
    stopifnot(identical(length(upper), 1L))
    ## lower and upper
    stopifnot(is.na(lower) || is.na(upper) || (lower <= upper))
    if (useC) {
        .Call(rnormIntTrunc1_R, mean, sd, lower, upper)
    }
    else {
        if (!is.na(lower) && !is.na(upper) && (lower == upper))
            return(lower)
        lower <- if (is.na(lower)) -Inf else as.double(lower)
        upper <- if (is.na(upper)) Inf else as.double(upper)
        ans <- rtnorm1(mean = mean,
                       sd = sd,
                       lower = lower,
                       upper = upper)
        if (ans > 0)
            as.integer(ans + 0.5)
        else
            as.integer(ans - 0.5)
    }
}


## TRANSLATED
## HAS_TESTS
## modified from code in package 'TruncatedNormal'.
## which uses alorithm from Z. I. Botev (2015),
## "The Normal Law Under Linear Restrictions:
##  Simulation and Estimation via Minimax Tilting", submitted to JRSS(B)
rtnorm1 <- function(mean = 0, sd = 1, lower = -Inf, upper = Inf,
                    useC = FALSE) {
    ## mean
    stopifnot(identical(length(mean), 1L))
    stopifnot(is.double(mean))
    stopifnot(!is.na(mean))
    ## sd
    stopifnot(identical(length(sd), 1L))
    stopifnot(is.double(sd))
    stopifnot(!is.na(sd))
    stopifnot(sd >= 0)
    ## lower
    stopifnot(identical(length(lower), 1L))
    stopifnot(is.double(lower))
    stopifnot(!is.na(lower))
    ## upper
    stopifnot(identical(length(upper), 1L))
    stopifnot(is.double(upper))
    stopifnot(!is.na(upper))
    ## lower, upper
    stopifnot(lower < upper)
    if (useC) {
        .Call(rtnorm1_R, mean, sd, lower, upper)
    }
    else {
        ## set threshold for switching between methods - in C this can be done via macro
        a <- 0.4
        tol <- 2.05
        ## standardized variables
        l <- (lower - mean)/sd
        u <- (upper - mean)/sd
        if (l > a) {
            c <- l^2 / 2
            f <- expm1(c - u^2 / 2)
            x <- c - log(1 + stats::runif(n = 1L) * f); # sample using Rayleigh
            ## keep list of rejected
            while (stats::runif(n = 1L)^2 * x > c) { # while there are rejections
                x <- c - log(1 + stats::runif(n = 1L) * f)
            }
            ans <- sqrt(2*x) # this Rayleigh transform can be delayed till the end
        }
        else if (u < (-a)) {
            c <- (-u)^2 / 2
            f <- expm1(c - (-l)^2 / 2)
            x <- c-log(1+stats::runif(n = 1L)*f); # sample using Rayleigh
            ## keep list of rejected
            while (stats::runif(n = 1L)^2 * x > c) { # while there are rejections
                x <- c - log(1 + stats::runif(n = 1L) * f)
            }
            ans <- (-1)*sqrt(2*x) # this Rayleigh transform can be delayed till the end
        }
        else {
            if (abs(u-l) > tol) { # abs(u-l)>tol, uses accept-reject
                x <- stats::rnorm(n = 1L) # sample from standard normal
                while (x < l | x > u) { # while there are rejections
                    x <- stats::rnorm(n = 1L)
                } 
                ans <- x         
            }
            else { # abs(u-l)<tol, uses inverse-transform
                pl <- stats::pnorm(q = l)
                pu <- stats::pnorm(q = u)
                u <- stats::runif(n = 1L)
                trans <- pl + (pu - pl) * u
                ans <- stats::qnorm(p = trans)
            }      
        }
        ans * sd + mean
    }
}

## TRANSLATED
## HAS_TESTS
## If no upper limit, 'upper' is NA_integer_
rpoisTrunc1 <- function(lambda, lower, upper, maxAttempt, useC = FALSE) {
    ## lambda
    stopifnot(is.double(lambda))
    stopifnot(identical(length(lambda), 1L))
    stopifnot(!is.na(lambda))
    stopifnot(lambda >= 0)
    ## lower
    stopifnot(is.integer(lower))
    stopifnot(identical(length(lower), 1L))
    ## upper
    stopifnot(is.integer(upper))
    stopifnot(identical(length(upper), 1L))
    ## maxAttempt
    stopifnot(is.integer(maxAttempt))
    stopifnot(identical(length(maxAttempt), 1L))
    stopifnot(!is.na(maxAttempt))
    stopifnot(maxAttempt > 0L)
    ## lower, upper
    stopifnot(is.na(lower) || is.na(upper) || (lower <= upper))
    if (useC) {
        .Call(rpoisTrunc1_R, lambda, lower, upper, maxAttempt)
    }
    else {
        if (is.na(lower))
            lower <- 0L
        if (lower < 0L)
            lower <- 0L
        finite.upper <- !is.na(upper)
        if (finite.upper && (lower == upper))
            return(lower)
        n.attempt <- 0L
        found <- FALSE
        if (finite.upper) {
            while (!found && (n.attempt < maxAttempt)) {
                n.attempt <- n.attempt + 1L
                prop.value <- stats::rpois(n = 1L, lambda = lambda)
                found <- (prop.value >= lower) && (prop.value <= upper)
            }
        }
        else {
            while (!found && (n.attempt < maxAttempt)) {
                n.attempt <- n.attempt + 1L
                prop.value <- stats::rpois(n = 1L, lambda = lambda)
                found <- prop.value >= lower
            }
        }
        if (found)
            as.integer(prop.value)
        else
            NA_integer_
    }
}

## ALONG ITERATOR ################################################################

## TRANSLATED
## HAS_TESTS
centerA <- function(vec, iterator, useC = FALSE) {
    stopifnot(is.double(vec))
    stopifnot(methods::is(iterator, "AlongIterator"))
    methods::validObject(iterator)
    indices <- iterator@indices
    nWithin <- iterator@nWithin
    nBetween <- iterator@nBetween
    stopifnot(identical(length(vec), as.integer(length(indices) * nWithin * nBetween)))
    if (useC) {
        .Call(centerA_R, vec, iterator)
    }
    else {
        indices <- iterator@indices
        nWithin <- iterator@nWithin
        nBetween <- iterator@nBetween
        length.ans <- length(indices) * nWithin * nBetween
        iterator <- resetA(iterator)
        ans <- numeric(length = length.ans)
        n.classifying <- length.ans / length(indices)
        for (i in seq_len(n.classifying)) {
            indices <- iterator@indices
            ans[indices] <- vec[indices] - mean(vec[indices])
            iterator <- advanceA(iterator)
        }
        ans
    }
}




## UPDATING ###########################################################################

## DOES_NOT_NEED_TESTS
checkUpdateBetaAndPriorBeta <- function(prior, vbar, n, sigma) {
    ## prior
    stopifnot(methods::validObject(prior))
    ## vbar
    stopifnot(is.double(vbar))
    stopifnot(!any(is.na(vbar)))
    ## n
    stopifnot(is.integer(n))
    stopifnot(identical(length(n), length(vbar)))
    stopifnot(!any(is.na(n)))
    stopifnot(all(n >= 0L))
    ## sigma
    stopifnot(is.double(sigma))
    stopifnot(identical(length(sigma), 1L))
    stopifnot(!is.na(sigma))
    stopifnot(sigma > 0)
    NULL
}

## TRANSLATED
## HAS_TESTS (INCLUDING FOR MIX)
## ADD TESTS FOR ICAR WHEN CLASSES FINISHED
betaHat <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "Prior") || methods::is(prior, "FakePrior"))
    stopifnot(methods::is(prior, "ComponentFlags"))
    if (useC) {
        .Call(betaHat_R, prior)
    }
    else {
        J <- prior@J@.Data
        has.alpha.dlm <- prior@hasAlphaDLM@.Data
        has.alpha.icar <- prior@hasAlphaICAR@.Data
        has.alpha.mix <- prior@hasAlphaMix@.Data
        has.covariates <- prior@hasCovariates@.Data
        has.season <- prior@hasSeason@.Data
        ans <- rep(0, times = J)
        if (has.alpha.dlm) {
            alpha.dlm <- prior@alphaDLM@.Data
            K <- prior@K@.Data
            L <- prior@L@.Data
            along.all.struc.zero <- prior@alongAllStrucZero
            iterator.alpha <- prior@iteratorState
            iterator.v <- prior@iteratorV
            iterator.alpha <- resetA(iterator.alpha)
            iterator.v <- resetA(iterator.v)
            for (l in seq_len(L)) {
                if (!along.all.struc.zero[l]) {
                    indices.alpha <- iterator.alpha@indices
                    indices.v <- iterator.v@indices
                    for (k in seq_len(K)) {
                        i.alpha <- indices.alpha[k + 1L]
                        i.ans <- indices.v[k]
                        ans[i.ans] <- ans[i.ans] + alpha.dlm[i.alpha]
                    }
                }
                iterator.alpha <- advanceA(iterator.alpha)
                iterator.v <- advanceA(iterator.v)
            }
        }
        if (has.alpha.icar) {
            alpha.icar <- prior@alphaICAR@.Data
            ans <- ans + alpha.icar
        }
        if (has.alpha.mix) { 
            alpha.mix <- prior@alphaMix@.Data 
            ans <- ans + alpha.mix 
        } 
        if (has.covariates) {
            Z <- unname(prior@Z)
            eta <- prior@eta@.Data
            ans <- ans + drop(Z %*% eta)
        }
        if (has.season) {
            s <- prior@s@.Data
            K <- prior@K@.Data
            L <- prior@L@.Data
            along.all.struc.zero <- prior@alongAllStrucZero
            iterator.s <- prior@iteratorState
            iterator.v <- prior@iteratorV
            iterator.s <- resetA(iterator.s)
            iterator.v <- resetA(iterator.v)
            for (l in seq_len(L)) {
                if (!along.all.struc.zero[l]) {
                    indices.s <- iterator.s@indices
                    indices.v <- iterator.v@indices
                    for (k in seq_len(K)) {
                        i.s <- indices.s[k + 1L]
                        i.ans <- indices.v[k]
                        ans[i.ans] <- ans[i.ans] + s[[i.s]][1L]
                    }
                }
                iterator.s <- advanceA(iterator.s)
                iterator.v <- advanceA(iterator.v)
            }
        }
        ans
    }
}

## TRANSLATED
## HAS TESTS
betaHatAlphaDLM <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "Prior"))
    stopifnot(methods::is(prior, "ComponentFlags"))
    stopifnot(prior@hasAlphaDLM)
    if (useC) {
        .Call(betaHatAlphaDLM_R, prior)
    }
    else {
        J <- prior@J@.Data
        ans <- rep(0, times = J)
        alpha.dlm <- prior@alphaDLM@.Data
        K <- prior@K@.Data
        L <- prior@L@.Data
        along.all.struc.zero <- prior@alongAllStrucZero
        iterator.alpha <- prior@iteratorState
        iterator.v <- prior@iteratorV
        iterator.alpha <- resetA(iterator.alpha)
        iterator.v <- resetA(iterator.v)
        for (l in seq_len(L)) {
            if (!along.all.struc.zero[l]) {
                indices.alpha <- iterator.alpha@indices
                indices.v <- iterator.v@indices
                for (k in seq_len(K)) {
                    i.alpha <- indices.alpha[k + 1L]
                    i.ans <- indices.v[k]
                    ans[i.ans] <- ans[i.ans] + alpha.dlm[i.alpha]
                }
            }
            iterator.alpha <- advanceA(iterator.alpha)
            iterator.v <- advanceA(iterator.v)
        }
        ans
    }
}

## ## NO_TESTS
## betaHatAlphaICAR <- function(prior, useC = FALSE) {
##     stopifnot(methods::is(prior, "Prior"))
##     stopifnot(methods::is(prior, "ComponentFlags"))
##     stopifnot(prior@hasAlphaICAR)
##     if (useC) {
##         .Call(betaHatICAR_R, prior)
##     }
##     else {
##         prior@alphaICAR@.Data
##     }
## }


## TRANSLATED
## HAS_TESTS
betaHatCovariates <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "Prior"))
    stopifnot(methods::is(prior, "ComponentFlags"))
    stopifnot(prior@hasCovariates)
    if (useC) {
        .Call(betaHatCovariates_R, prior)
    }
    else {
        Z <- unname(prior@Z)
        eta <- prior@eta@.Data
        drop(Z %*% eta)
    }
}

## TRANSLATED
## HAS_TESTS
betaHatSeason <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "Prior"))
    stopifnot(methods::is(prior, "ComponentFlags"))
    stopifnot(prior@hasSeason)
    if (useC) {
        .Call(betaHatSeason_R, prior)
    }
    else {
        J <- prior@J@.Data
        ans <- rep(0, times = J)
        s <- prior@s@.Data
        K <- prior@K@.Data
        L <- prior@L@.Data
        along.all.struc.zero <- prior@alongAllStrucZero
        iterator.s <- prior@iteratorState
        iterator.v <- prior@iteratorV
        iterator.s <- resetA(iterator.s)
        iterator.v <- resetA(iterator.v)
        for (l in seq_len(L)) {
            if (!along.all.struc.zero[l]) {
                indices.s <- iterator.s@indices
                indices.v <- iterator.v@indices
                for (k in seq_len(K)) {
                    i.s <- indices.s[k + 1L]
                    i.ans <- indices.v[k]
                    ans[i.ans] <- ans[i.ans] + s[[i.s]][1L]
                }
            }
            iterator.s <- advanceA(iterator.s)
            iterator.v <- advanceA(iterator.v)
        }
        ans
    }
}

## TRANSLATED
## NOTE: C version of this function can give different results due
## to effect of kEpsilon test for deriv near zero.
##
## If the function finds the root within 'kMaxIter' iterations, it
## returns this root. If the function fails to find a root, it returns -99.0.
## The root must be between 'min' and 'max'.
findOneRootLogPostSigmaNorm <- function(sigma0, z, A, nu, V, n, min, max,
                                        useC = FALSE) {
    ## 'sigma0'
    stopifnot(identical(length(sigma0), 1L))
    stopifnot(is.double(sigma0))
    stopifnot(!is.na(sigma0))
    stopifnot(sigma0 > 0)
    ## 'z'
    stopifnot(identical(length(z), 1L))
    stopifnot(is.double(z))
    stopifnot(!is.na(z))
    ## 'A'
    stopifnot(identical(length(A), 1L))
    stopifnot(is.double(A))
    stopifnot(!is.na(A))
    stopifnot(A > 0)
    ## 'nu'
    stopifnot(identical(length(nu), 1L))
    stopifnot(is.double(nu))
    stopifnot(!is.na(nu))
    stopifnot(nu > 0)
    ## 'V'
    stopifnot(identical(length(V), 1L))
    stopifnot(is.double(V))
    stopifnot(!is.na(V))
    stopifnot(V > 0)
    ## 'n'
    stopifnot(identical(length(n), 1L))
    stopifnot(is.integer(n))
    stopifnot(!is.na(n))
    stopifnot(n > 0L)
    ## 'min'
    stopifnot(identical(length(min), 1L))
    stopifnot(is.double(min))
    stopifnot(!is.na(min))
    ## 'max'
    stopifnot(identical(length(max), 1L))
    stopifnot(is.double(max))
    stopifnot(!is.na(max))
    ## 'min' and 'max'
    stopifnot(max > min)
    if (useC) {
        .Call(findOneRootLogPostSigmaNorm_R, sigma0, z, A, nu, V, n, min, max)
    }
    else {
        kTolerance <- 1e-20           ## C version can use macros to set these
        kEpsilon <- 1e-15     ## NEW
        kMaxIter <- 1000L
        ## check that a value can be found
        min.tol <- min + kTolerance
        fmin <- -n*log(min.tol) - V/(2*(min.tol)^2) - ((nu + 1)/2) * log((min.tol)^2 + nu*A^2)
        if (is.finite(max)) {
            max.tol <- max - kTolerance
            fmax <- -n*log(max.tol) - V/(2*(max.tol)^2) - ((nu + 1)/2) * log((max.tol)^2 + nu*A^2)
        }
        else
            fmax <- -Inf
        if (((fmin < z) && (fmax < z)) || ((fmin > z) && (fmax > z)))
            return(-99.0)
        ## find root
        f0 <- -n*log(sigma0) - V/(2*sigma0^2) - ((nu + 1)/2) * log(sigma0^2 + nu*A^2)
        g0 <- (f0 - z)^2
        for (i in seq_len(kMaxIter)) {
            f0prime <- -n/sigma0 + V/(sigma0^3) - ((nu + 1)*sigma0) / (sigma0^2 + nu*A^2)
            deriv.near.zero <- abs(f0prime) < kEpsilon ## NEW
            if (deriv.near.zero) ## NEW
                return(-1.0) ## NEW
            rho <- 1
            repeat {
                sigma1 <- sigma0 - rho * (f0 - z) / f0prime
                if ((min <= sigma1) && (sigma1 <= max))
                    break
                rho <- rho / 2
            }
            repeat {
                f1 <- -n*log(sigma1) - V/(2*sigma1^2) - ((nu + 1)/2) * log(sigma1^2 + nu*A^2)
                g1 <- (f1 - z)^2
                if (g1 <= g0 || abs(g1 - g0) < kTolerance || (rho < kTolerance))
                    break
                rho <- rho / 2 
                sigma1 <- sigma0 - rho * (f0 - z) / f0prime
            }
            if ((abs(g1 - g0) < kTolerance) || (rho < kTolerance))
                return(sigma0)
            else {
                sigma0 <- sigma1
                f0 <- f1
                g0 <- g1
            }
        }
        ## reached maximum iterations without finding root
        return(-99.0)
    }
}

## TRANSLATED
## NOTE: C version of this function can give different results due
## to effect of kEpsilon test for deriv near zero.
##
## modified from pseudocode from https://en.wikipedia.org/wiki/Newton%27s_method
## If the function finds the root within 'kMaxIter' iterations, it
## returns this root.  If the derivative of 'f' is near 0, the function
## returns -1.0.  If the function fails to find a root, it returns -99.0.
## The root must be between 'min' and 'max'.  Note that this function
## uses a different 'f' and 'fprime' from function 'findOneRootLogPosSigmaNorm'.
findOneRootLogPostSigmaRobust <- function(sigma0, z, A, nuBeta, nuTau, V, n, min, max,
                                          useC = FALSE) {
    ## 'sigma0'
    stopifnot(identical(length(sigma0), 1L))
    stopifnot(is.double(sigma0))
    stopifnot(!is.na(sigma0))
    stopifnot(sigma0 > 0)
    ## 'z'
    stopifnot(identical(length(z), 1L))
    stopifnot(is.double(z))
    stopifnot(!is.na(z))
    ## 'A'
    stopifnot(identical(length(A), 1L))
    stopifnot(is.double(A))
    stopifnot(!is.na(A))
    stopifnot(A > 0)
    ## 'nuBeta'
    stopifnot(identical(length(nuBeta), 1L))
    stopifnot(is.double(nuBeta))
    stopifnot(!is.na(nuBeta))
    stopifnot(nuBeta > 0)
    ## 'nuTau'
    stopifnot(identical(length(nuTau), 1L))
    stopifnot(is.double(nuTau))
    stopifnot(!is.na(nuTau))
    stopifnot(nuTau > 0)
    ## 'V'
    stopifnot(identical(length(V), 1L))
    stopifnot(is.double(V))
    stopifnot(!is.na(V))
    stopifnot(V > 0)
    ## 'n'
    stopifnot(identical(length(n), 1L))
    stopifnot(is.integer(n))
    stopifnot(!is.na(n))
    stopifnot(n > 0L)
    ## 'min'
    stopifnot(identical(length(min), 1L))
    stopifnot(is.double(min))
    stopifnot(!is.na(min))
    ## 'max'
    stopifnot(identical(length(max), 1L))
    stopifnot(is.double(max))
    stopifnot(!is.na(max))
    ## 'min' and 'max'
    stopifnot(max > min)
    if (useC) {
        .Call(findOneRootLogPostSigmaRobust_R, sigma0, z, A, nuBeta, nuTau, V, n, min, max)
    }
    else {
        kTolerance <- 1e-20           ## C version can use macros to set these
        kEpsilon <- 1e-15     ## NEW
        kMaxIter <- 1000L
        min.tol <- min + kTolerance
        fmin <- n*nuBeta*log(min.tol) - (nuBeta/2)*(min.tol^2)*V - ((nuTau+1)/2)*log(min.tol^2 + nuTau*A^2)
        if (is.finite(max)) {
            max.tol <- max - kTolerance
            fmax <- n*nuBeta*log(max.tol) - (nuBeta/2)*(max.tol^2)*V - ((nuTau+1)/2)*log(max.tol^2 + nuTau*A^2)
        }
        else
            fmax <- -Inf
        if ((fmin < z && fmax < z) || (fmin>z && fmax>z))
            return(-99.0)
        ## find root
        f0 <- n*nuBeta*log(sigma0) - (nuBeta/2)*(sigma0^2)*V - ((nuTau+1)/2)*log(sigma0^2 + nuTau*A^2)
        g0 <- (f0 - z)^2
        for (i in seq_len(kMaxIter)) {
            f0prime <- n*nuBeta/sigma0 - nuBeta*sigma0*V - ((nuTau + 1)*sigma0)/(sigma0^2 + nuTau*A^2)
            deriv.near.zero <- abs(f0prime) < kEpsilon ## NEW
            if (deriv.near.zero) ## NEW
                return(-1.0) ## NEW
            rho <- 1
            repeat {
                sigma1 <- sigma0 - rho * (f0 - z) / f0prime
                if ((min <= sigma1) && (sigma1 <= max))
                    break
                rho <- rho / 2
            }
            repeat {
                f1 <- n*nuBeta*log(sigma1) - (nuBeta/2)*(sigma1^2)*V - ((nuTau+1)/2)*log(sigma1^2 + nuTau*A^2)
                g1 <- (f1 - z)^2
                if (g1 <= g0 || abs(g1 - g0) < kTolerance || (rho < kTolerance))
                    break
                rho <- rho / 2 
                sigma1 <- sigma0 - rho * (f0 - z) / f0prime
            }
            if ((abs(g1 - g0) < kTolerance) || (rho < kTolerance))
                return(sigma0)
            else {
                sigma0 <- sigma1
                f0 <- f1
                g0 <- g1
            }
        }
        ## reached maximum iterations without finding root
        return(-99.0)
        ## reached maximum iterations without finding root
        -99.0
    }
}

## TRANSLATED
## HAS_TESTS
getV <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "Prior"))
    stopifnot(methods::is(prior, "IsRobustMixin"))
    stopifnot(methods::validObject(prior))
    if (useC) {
        .Call(getV_R, prior)
    }
    else {
        is.robust <- prior@isRobust@.Data
        if (is.robust)
            prior@UBeta@.Data
        else {
            J <- prior@J@.Data
            tau <- prior@tau@.Data
            rep(tau^2, times = J)
        }
    }
}

## TRANSLATED
## HAS_TESTS
logPostPhiMix <- function(phi, level, meanLevel, nAlong, indexClassMaxMix, omega,
                          useC = FALSE) {
    ## 'phi'
    stopifnot(identical(length(phi), 1L))
    stopifnot(is.double(phi))
    stopifnot(!is.na(phi))
    ## 'level'
    stopifnot(is.double(level))
    stopifnot(!any(is.na(level)))
    ## 'meanLevel'
    stopifnot(identical(length(meanLevel), 1L))
    stopifnot(is.double(meanLevel))
    stopifnot(!is.na(meanLevel))
    ## 'nAlong'
    stopifnot(identical(length(nAlong), 1L))
    stopifnot(is.integer(nAlong))
    stopifnot(!is.na(nAlong))
    stopifnot(nAlong >= 2L)
    ## 'indexClassMaxMix'
    stopifnot(identical(length(indexClassMaxMix), 1L))
    stopifnot(is.integer(indexClassMaxMix))
    stopifnot(!is.na(indexClassMaxMix))
    stopifnot(indexClassMaxMix > 0L)
    ## 'omega'
    stopifnot(identical(length(omega), 1L))
    stopifnot(is.double(omega))
    stopifnot(!is.na(omega))
    stopifnot(omega > 0)
    ## 'level', 'nAlong', 'indexClassMaxMix'
    stopifnot(length(level) >= nAlong * indexClassMaxMix)
    if (useC) {
        .Call(logPostPhiMix_R, phi, level, meanLevel, nAlong, indexClassMaxMix, omega)
    }
    else {
        if (abs(phi) < 1) {
            ratio <- meanLevel / (1 - phi)
            ans.first <- 0
            for (i.class in seq_len(indexClassMaxMix)) {
                i.wt.first <- (i.class - 1L) * nAlong + 1L
                level.first <- level[i.wt.first]
                ans.first <- ans.first + (level.first - ratio)^2
            }
            ans.first <- (1 - phi^2) * ans.first
            ans.rest <- 0
            for (i.class in seq_len(indexClassMaxMix)) {
                for (i.along in seq.int(from = 2L, to = nAlong)) {
                    i.wt.curr <- (i.class - 1L) * nAlong + i.along
                    i.wt.prev <- i.wt.curr - 1L
                    level.curr <- level[i.wt.curr]
                    level.prev <- level[i.wt.prev]
                    ans.rest <-  ans.rest + (level.curr - meanLevel - phi * level.prev)^2
                }
            }
            (ans.first + ans.rest) / (-2 * omega^2)
        }
        else {
            0.0001
        }
    }
}


## TRANSLATED
## HAS_TESTS
logPostPhiFirstOrderMix <- function(phi, level, meanLevel, nAlong, indexClassMaxMix, omega,
                                    useC = FALSE) {
    ## 'phi'
    stopifnot(identical(length(phi), 1L))
    stopifnot(is.double(phi))
    stopifnot(!is.na(phi))
    ## 'level'
    stopifnot(is.double(level))
    stopifnot(!any(is.na(level)))
    ## 'meanLevel'
    stopifnot(identical(length(meanLevel), 1L))
    stopifnot(is.double(meanLevel))
    stopifnot(!is.na(meanLevel))
    ## 'nAlong'
    stopifnot(identical(length(nAlong), 1L))
    stopifnot(is.integer(nAlong))
    stopifnot(!is.na(nAlong))
    stopifnot(nAlong >= 2L)
    ## 'indexClassMaxMix'
    stopifnot(identical(length(indexClassMaxMix), 1L))
    stopifnot(is.integer(indexClassMaxMix))
    stopifnot(!is.na(indexClassMaxMix))
    stopifnot(indexClassMaxMix > 0)
    ## 'omega'
    stopifnot(identical(length(omega), 1L))
    stopifnot(is.double(omega))
    stopifnot(!is.na(omega))
    stopifnot(omega > 0)
    ## 'level', 'nAlong', 'indexClassMaxMix'
    stopifnot(length(level) >= nAlong * indexClassMaxMix)
    if (useC) {
        .Call(logPostPhiFirstOrderMix_R, phi, level, meanLevel, nAlong, indexClassMaxMix, omega)
    }
    else {
        if(abs(phi) < 1) {
            ans.first <- 0
            ratio <- meanLevel / (1 - phi)
            for (i.class in seq_len(indexClassMaxMix)) {
                i.wt.first <- (i.class - 1L) * nAlong + 1L
                level.first <- level[i.wt.first]
                ans.first <- ans.first + (level.first - ratio) * (phi * level.first + ratio)
            }
            ans.rest <- 0
            for (i.class in seq_len(indexClassMaxMix)) {
                for (i.along in seq.int(from = 2L, to = nAlong)) {
                    i.wt.curr <- (i.class - 1L) * nAlong + i.along
                    i.wt.prev <- i.wt.curr - 1L
                    level.curr <- level[i.wt.curr]
                    level.prev <- level[i.wt.prev]
                    ans.rest <- ans.rest + level.prev * (level.curr - meanLevel - phi * level.prev)
                }
            }
            (ans.first + ans.rest) / omega^2
        }
        else {
            0.0001
        }
    }
}

## TRANSLATED
## HAS_TESTS
logPostPhiSecondOrderMix <- function(phi, level, meanLevel, nAlong, indexClassMaxMix,
                                     omega, useC = FALSE) {
    ## 'phi'
    stopifnot(identical(length(phi), 1L))
    stopifnot(is.double(phi))
    stopifnot(!is.na(phi))
    ## 'level'
    stopifnot(is.double(level))
    stopifnot(!any(is.na(level)))
    ## 'meanLevel'
    stopifnot(identical(length(meanLevel), 1L))
    stopifnot(is.double(meanLevel))
    stopifnot(!is.na(meanLevel))
    ## 'nAlong'
    stopifnot(identical(length(nAlong), 1L))
    stopifnot(is.integer(nAlong))
    stopifnot(!is.na(nAlong))
    stopifnot(nAlong >= 2L)
    ## 'indexClassMaxMix'
    stopifnot(identical(length(indexClassMaxMix), 1L))
    stopifnot(is.integer(indexClassMaxMix))
    stopifnot(!is.na(indexClassMaxMix))
    stopifnot(indexClassMaxMix > 0)
    ## 'omega'
    stopifnot(identical(length(omega), 1L))
    stopifnot(is.double(omega))
    stopifnot(!is.na(omega))
    stopifnot(omega > 0)
    ## 'level', 'nAlong', 'indexClassMaxMix'
    stopifnot(length(level) >= nAlong * indexClassMaxMix)
    if (useC) {
        .Call(logPostPhiSecondOrderMix_R, phi, level, meanLevel, nAlong,
              indexClassMaxMix, omega)
    }
    else {
        if(abs(phi) < 1) {
            ans.first <- - 2 * indexClassMaxMix * meanLevel^2 / (1 - phi)^3
            ans.rest <- 0
            if (nAlong > 2L) {
                for (i.class in seq_len(indexClassMaxMix)) {
                    for (i.along in seq.int(from = 2L, to = nAlong - 1L)) {
                        i.wt <- (i.class - 1L) * nAlong + i.along
                        level.i.wt <- level[i.wt]
                        ans.rest <-  ans.rest - level.i.wt^2
                    }
                }
            }
            (ans.first + ans.rest) / omega^2
        }
        else {
            0.0001
        }
    }
}

## TRANSLATED
## HAS_TESTS
## assume first dimension of array that
## mx is obtained from is age
makeLifeExpBirth <- function(mx, nx, ax, iAge0, nAge,
                             useC = FALSE) {
    ## mx
    stopifnot(is.double(mx))
    stopifnot(!any(is.na(mx)))
    stopifnot(all(mx >= 0))
    ## nx
    stopifnot(is.double(nx))
    stopifnot(!any(is.na(nx)))
    stopifnot(all(nx > 0))
    ## ax
    stopifnot(is.double(ax))
    stopifnot(!any(is.na(ax)))
    stopifnot(all(ax >= 0))
    ## iAge0
    stopifnot(is.integer(iAge0))
    stopifnot(length(iAge0) == 1L)
    stopifnot(iAge0 >= 1L)
    ## nAge
    stopifnot(is.integer(nAge))
    stopifnot(length(nAge) == 1L)
    stopifnot(nAge >= 1L)
    ## mx and ax
    stopifnot(length(ax) == length(mx))
    ## ax and nx
    stopifnot(all(ax <= rep(nx, length.out = length(ax))))
    ## mx and iAge0
    stopifnot(iAge0 <= length(mx))
    ## mx and nAge
    stopifnot(nAge <= length(mx))    
    if (useC) {
        .Call(makeLifeExpBirth_R, mx, nx, ax, iAge0, nAge)
    }
    else {
        ans <- 0
        lx.i <- 1
        for (i in seq_len(nAge - 1L)) {
            mx.i <- mx[iAge0 + i - 1L]
            nx.i <- nx[i]
            ax.i <- ax[iAge0 + i - 1L]
            qx.i <- nx.i * mx.i / (1 + (nx.i - ax.i) * mx.i)
            lx.iplus1 <- lx.i * (1 - qx.i)
            Lx.i <- lx.iplus1 * nx.i + (lx.i - lx.iplus1) * ax.i
            ans <- ans + Lx.i
            lx.i <- lx.iplus1
        }
        mx.i <- mx[iAge0 + nAge - 1L]
        Lx.i <- lx.i / mx.i
        ans <- ans + Lx.i
        ans
    }
}

## TRANSLATED
## HAS_TESTS
makeVBarAndN <- function(object, iBeta, g, useC = FALSE) {
    ## object
    stopifnot(methods::is(object, "Varying"))
    stopifnot(methods::validObject(object))
    ## iBeta
    stopifnot(is.integer(iBeta))
    stopifnot(identical(length(iBeta), 1L))
    stopifnot(!is.na(iBeta))
    stopifnot(iBeta %in% seq_along(object@betas))
    ## g
    stopifnot(is.function(g))
    if (useC) {
        .Call(makeVBarAndN_R, object, iBeta) ## g not passed
    }
    else {
        theta <- object@theta
        cell.in.lik <- object@cellInLik
        betas <- object@betas
        iterator <- object@iteratorBetas
        if (identical(g, log)) { 
            box.cox.param <- object@boxCoxParam 
            uses.box.cox.transform <- box.cox.param > 0 
        } 
        else 
            uses.box.cox.transform <- FALSE
        beta <- betas[[iBeta]]
        iterator <- resetB(iterator)
        vbar <- rep(0, times = length(beta))
        n <- rep(0L, times = length(beta))
        i.other.betas <- seq_along(betas)[-iBeta]
        for (i.mu in seq_along(theta)) {
            include.cell <- cell.in.lik[i.mu]
            if (include.cell) {
                indices <- iterator@indices
                pos.ans <- indices[iBeta]
                if (uses.box.cox.transform) 
                    vbar[pos.ans] <- vbar[pos.ans] + (theta[i.mu] ^ box.cox.param - 1) / box.cox.param 
                else 
                    vbar[pos.ans] <- vbar[pos.ans] + g(theta[i.mu])
                for (i.other.beta in i.other.betas) {
                    other.beta <- betas[[i.other.beta]]
                    pos.other.beta <- indices[i.other.beta]
                    vbar[pos.ans] <- vbar[pos.ans] - other.beta[pos.other.beta]
                }
                n[pos.ans] <- n[pos.ans] + 1L
            }
            iterator <- advanceB(iterator)
        }
        for (i in seq_along(vbar))
            if (n[i] > 0L)
                vbar[i] <- vbar[i] / n[i]
        list(vbar, n)
    }
}


## TRANSLATED
## HAS_TESTS
modePhiMix <- function(level, meanLevel, nAlong,
                       indexClassMaxMix, omega,
                       tolerance, useC = FALSE) {
    ## 'level'
    stopifnot(is.double(level))
    stopifnot(!any(is.na(level)))
    ## 'meanLevel'
    stopifnot(identical(length(meanLevel), 1L))
    stopifnot(is.double(meanLevel))
    stopifnot(!is.na(meanLevel))
    ## 'nAlong'
    stopifnot(identical(length(nAlong), 1L))
    stopifnot(is.integer(nAlong))
    stopifnot(!is.na(nAlong))
    stopifnot(nAlong >= 2L)
    ## 'indexClassMaxMix'
    stopifnot(identical(length(indexClassMaxMix), 1L))
    stopifnot(is.integer(indexClassMaxMix))
    stopifnot(!is.na(indexClassMaxMix))
    stopifnot(indexClassMaxMix > 0)
    ## 'omega'
    stopifnot(identical(length(omega), 1L))
    stopifnot(is.double(omega))
    stopifnot(!is.na(omega))
    stopifnot(omega > 0)
    ## 'tolerance'
    stopifnot(identical(length(tolerance), 1L))
    stopifnot(is.double(tolerance))
    stopifnot(!is.na(tolerance))
    stopifnot(tolerance > 0)
    ## 'level', 'nAlong', 'indexClassMaxMix'
    stopifnot(length(level) >= nAlong * indexClassMaxMix)
    if (useC) {
        .Call(modePhiMix_R,
              level, meanLevel, nAlong,
              indexClassMaxMix, omega, tolerance)
    }
    else {
        kCutoffConvergenceModePhi <- 0.0001 # in C can use macro
        phi.curr <- 0.9 # typical value for mode
        diff.outer <- 1
        while (diff.outer > kCutoffConvergenceModePhi) {
            length.step <- 0.1
            log.post.curr <- logPostPhiMix(phi = phi.curr,
                                           level = level,
                                           meanLevel = meanLevel,
                                           nAlong = nAlong,
                                           indexClassMaxMix = indexClassMaxMix,
                                           omega = omega,
                                           useC = TRUE)
            diff.inner <- 0
            while ((diff.inner <= 0) & (length.step > 0.001)) {
                log.post.first <- logPostPhiFirstOrderMix(phi = phi.curr,
                                                          level = level,
                                                          meanLevel = meanLevel,
                                                          nAlong = nAlong,
                                                          indexClassMaxMix = indexClassMaxMix,
                                                          omega = omega,
                                                          useC = TRUE)
                log.post.second <- logPostPhiSecondOrderMix(phi = phi.curr,
                                                            level = level,
                                                            meanLevel = meanLevel,
                                                            nAlong = nAlong,
                                                            indexClassMaxMix = indexClassMaxMix,
                                                            omega = omega,
                                                            useC = TRUE)
                phi.new <- phi.curr - length.step * log.post.first / log.post.second
                if (phi.new > 1 - tolerance)
                    phi.new <- 1 - tolerance
                if (phi.new < -1 + tolerance)
                    phi.new <- -1 + tolerance                    
                log.post.new <- logPostPhiMix(phi = phi.new,
                                              level = level,
                                              meanLevel = meanLevel,
                                              nAlong = nAlong,
                                              indexClassMaxMix = indexClassMaxMix,
                                              omega = omega,
                                              useC = TRUE)
                diff.inner <- log.post.new - log.post.curr
                length.step <- length.step - 0.001
            }
            diff.outer <- abs(phi.new - phi.curr)
            phi.curr <- phi.new
        }
        phi.new
    }
}

## TRANSLATED
## HAS_TESTS
safeLogProp_Binomial <- function(logit.th.new, logit.th.other.new,
                                 logit.th.old, logit.th.other.old,
                                 scale, weight, weight.other,
                                 useC = FALSE) {
    for (name in c("logit.th.new", "logit.th.other.new",
                   "logit.th.old", "logit.th.other.old",
                   "scale", "weight", "weight.other")) {
        value <- get(name)
        stopifnot(identical(length(value), 1L))
        stopifnot(is.double(value))
        stopifnot(!is.na(value))
    }
    stopifnot(scale > 0)
    if (useC) {
        .Call(safeLogProp_Binomial_R, logit.th.new, logit.th.other.new,
              logit.th.old, logit.th.other.old, scale,
              weight, weight.other)
    }
    else {
        if (abs(logit.th.new) > abs(logit.th.other.new)) {
            if (logit.th.new > 0) {
                outside <- logit.th.new
                coef.first <- (exp(-2 * logit.th.new)
                               + 2 * exp(-logit.th.new)
                               + 1)
                coef.second <- (exp(-logit.th.other.new - logit.th.new)
                                + 2 * exp(-logit.th.new)
                                + exp(logit.th.other.new - logit.th.new))
            }
            else {
                outside <- -logit.th.new
                coef.first <- (1
                               + 2 * exp(logit.th.new)
                               + exp(2 * logit.th.new))
                coef.second <- (exp(-logit.th.other.new + logit.th.new)
                                + 2 * exp(logit.th.new)
                                + exp(logit.th.other.new + logit.th.new))
            }
        }
        else {
            if (logit.th.other.new > 0) {
                outside <- logit.th.other.new
                coef.first <- (exp(-logit.th.new - logit.th.other.new)
                               + 2 * exp(-logit.th.other.new)
                               + exp(logit.th.new - logit.th.other.new))
                coef.second <- (exp(-2 * logit.th.other.new)
                                + 2 * exp(-logit.th.other.new)
                                + 1)
            }
            else {
                outside <- -logit.th.other.new
                coef.first <- (exp(-logit.th.new + logit.th.other.new)
                               + 2 * exp(logit.th.other.new)
                               + exp(logit.th.new + logit.th.other.new))
                coef.second <- (1
                                + 2 * exp(logit.th.other.new)
                                + exp(2 * logit.th.other.new))
            }
        }
        dens.first <- stats::dnorm(x = logit.th.new,
                            mean = logit.th.old,
                            sd = scale)
        dens.second <- stats::dnorm(x = logit.th.other.new,
                             mean = logit.th.other.old,
                             sd = scale)
        weight.ratio <- abs(weight / weight.other)
        outside + log(coef.first * dens.first
                      + weight.ratio * coef.second * dens.second)
    }
}

## TRANSLATED
## HAS_TESTS
## This function only protect against overflow due to 'theta' being too small.
## It does not protect against inaccuracies due to 'theta' being too large.
## This doesn't seem worthwhile when the chance of 'theta' being too large
## is small (typically 'theta' will be a rate), and the consequences of the
## errors are not too bad (a coefficient being 0 rather than just above 0.)
safeLogProp_Poisson <- function(log.th.new, log.th.other.new,
                                log.th.old, log.th.other.old,
                                scale, weight, weight.other,
                                useC = FALSE) {
    for (name in c("log.th.new", "log.th.other.new",
                   "log.th.old", "log.th.other.old",
                   "scale", "weight", "weight.other")) {
        value <- get(name)
        stopifnot(identical(length(value), 1L))
        stopifnot(is.double(value))
        stopifnot(!is.na(value))
    }
    stopifnot(scale > 0)
    if (useC) {
        .Call(safeLogProp_Poisson_R, log.th.new, log.th.other.new,
              log.th.old, log.th.other.old, scale,
              weight, weight.other)
    }
    else {
        if ((log.th.new < log.th.other.new) && (log.th.new < 0)) {
            outside <- -log.th.new
            coef.first <- 1
            coef.second <- exp(-log.th.other.new + log.th.new)
        }
        else if ((log.th.other.new < log.th.new) && (log.th.other.new < 0)) {
            outside <- -log.th.other.new
            coef.first <- exp(-log.th.new + log.th.other.new)
            coef.second <- 1
        }
        else {
            outside <- 0
            coef.first <- exp(-log.th.new)
            coef.second <- exp(-log.th.other.new)
        }
        dens.first <- stats::dnorm(x = log.th.new, mean = log.th.old, sd = scale)
        dens.second <- stats::dnorm(x = log.th.other.new, mean = log.th.other.old, sd = scale)
        weight.ratio <- abs(weight / weight.other)
        outside + log(coef.first * dens.first
                      + weight.ratio * coef.second * dens.second)
    }
}



## MONITORING ######################################################################

## JAH note: I am not sure if we translate this.  At present the
## write_to_file function in C combines extracting and writing and
## I think is much more efficient like that for C purposes even though
## it is a bit ugly from a design point of view.  What I'd really like
## is a more efficient way of getting at the slots to extract in C.
## HAS_TESTS
extractValues <- function(object) {
    if (is.numeric(object)) {
        as.numeric(object)
    }
    else if (is.logical(object)) {
        as.numeric(object)
    }
    else if (is.list(object)) {
        unlist(lapply(object, extractValues))
    }
    else {
        slots.to.extract <- object@slotsToExtract
        n <- length(slots.to.extract)
        if (n > 0L) {
            ans <- vector(mode = "list", length = n)
            for (i in seq_along(ans)) {
                obj <- methods::slot(object, slots.to.extract[i])
                ans[[i]] <- extractValues(obj)
            }
            unlist(ans)
        }
        else
            numeric()
    }
}

## ## JAH note: I am not sure if we translate this.  At present the
## ## write_to_file function in C combines extracting and writing and
## ## I think is much more efficient like that for C purposes even though
## ## it is a bit ugly from a design point of view.  What I'd really like
## ## is a more efficient way of getting at the slots to extract in C.
## ## HAS_TESTS
## extractValues <- function(object) {
##     if (is.numeric(object)) {
##         object
##     }
##     else if (is.list(object)) {
##         unlist(lapply(object, extractValues))
##     }
##     else {
##         slots.to.extract <- object@slotsToExtract
##         n <- length(slots.to.extract)
##         if (n > 0L) {
##             ans <- vector(mode = "list", length = n)
##             for (i in seq_along(ans)) {
##                 obj <- methods::slot(object, slots.to.extract[i])
##                 ans[[i]] <- extractValues(obj)
##             }
##             unlist(ans)
##         }
##         else
##             numeric()
##     }
## }

## HAS_TESTS
## Helper function for 'sweepAllMargins'.
sweepMargins <- function(x, margins) {
    dim <- dim(x)
    s <- seq_along(dim)
    seqs <- lapply(dim, seq_len)
    ones <- lapply(dim, function(times) rep(1L, times = times))
    for (margin in margins) {
        along <- s[-margin]
        indices <- replace(seqs, list = along,  values = ones[along])
        dims <- match(s, margin, nomatch = 0L)
        dim.margin <- dim[margin]
        collapse <- methods::new("CollapseTransform",
                        indices = indices,
                        dims = dims,
                        dimBefore = dim,
                        dimAfter = dim.margin)
        extend <- methods::new("ExtendTransform",
                      indices = indices,
                      dims = dims,
                      dimBefore = dim.margin,
                      dimAfter = dim)
        collapsed <- dembase::collapse(x, transform = collapse)
        means <- collapsed / prod(dim[along])
        means <- extend(means, transform = extend)
        x <- x - means
    }
    x
}



## UPDATING COUNTS ##################################################################

## 'diffLogLik' has special provisions for infinite values.
## log-likelihood of -Inf can occur fairly frequently with
## sparse data.  When the data model is binomial, it occurs
## whenever the cell in 'dataset' has a higher value than the
## sum of the corresponding cell(s) from 'y'.  When the
## data model is Poisson or a Poisson-binomial mixture,
## it occurs whenever the cell in 'dataset' has a value > 0
## but the corresponding cell(s)
## in 'y' are all 0. Whether testing and allowing for an
## early exit from the function speeds things up depends on the
## sparseness of the data and the speed of testing
## vs calculating likelihoods.  However, I quite like the
## idea of explicitly testing to emphasize that we need
## to deal with -Inf.

## TRANSLATED
## HAS_TESTS
diffLogLik <- function(yProp, y, indicesY, dataModels,
                       datasets, transforms, useC = FALSE) {
    ## yProp
    stopifnot(is.integer(yProp))
    stopifnot(!any(is.na(yProp)))
    stopifnot(all(yProp >= 0))
    ## y
    stopifnot(methods::is(y, "Counts"))
    stopifnot(is.integer(y))
    stopifnot(!any(is.na(y)))
    stopifnot(all(y >= 0))
    ## indicesY
    stopifnot(is.integer(indicesY))
    stopifnot(!any(is.na(indicesY)))
    stopifnot(all(indicesY >= 1L))
    ## dataModels
    stopifnot(is.list(dataModels))
    stopifnot(all(sapply(dataModels, methods::is, "Model")))
    stopifnot(all(sapply(dataModels, methods::is, "UseExposure")))
    ## datasets
    stopifnot(is.list(datasets))
    stopifnot(all(sapply(datasets, methods::is, "Counts")))
    stopifnot(all(sapply(datasets, is.integer)))
    stopifnot(all(sapply(datasets, function(x) all(x[!is.na(x)] >= 0))))
    ## transforms
    stopifnot(is.list(transforms))
    stopifnot(all(sapply(transforms, methods::is, "CollapseTransformExtra")))
    ## yProp and indicesY
    stopifnot(identical(length(yProp), length(indicesY)))
    ## y and indicesY
    stopifnot(all(indicesY <= length(y)))
    ## y and transforms
    for (i in seq_along(transforms))
        stopifnot(identical(transforms[[i]]@dimBefore, dim(y)))
    ## dataModels and datasets
    stopifnot(identical(length(dataModels), length(datasets)))
    ## dataModels and transforms
    stopifnot(identical(length(dataModels), length(transforms)))
    ## datasets and transforms
    for (i in seq_along(datasets))
        stopifnot(identical(transforms[[i]]@dimAfter, dim(datasets[[i]])))
    if (useC) {
        .Call(diffLogLik_R, yProp, y, indicesY, dataModels,
              datasets, transforms)
    }
    else {
        ans <- 0
        n.element.indices.y <- length(indicesY)
        n.dataset <- length(datasets)
        i.element.indices.y <- 1L
        ans.infinite <- FALSE
        while ((i.element.indices.y <= n.element.indices.y) && !ans.infinite) {
            i.cell.y <- indicesY[i.element.indices.y]
            if (yProp[i.element.indices.y] != y[i.cell.y]) {
                i.dataset <- 1L
                while ((i.dataset <= n.dataset) && !ans.infinite) {
                    transform <- transforms[[i.dataset]]
                    i.cell.dataset <- dembase::getIAfter(i.cell.y, transform = transform)
                    if (i.cell.dataset > 0L) {
                        dataset <- datasets[[i.dataset]]
                        cell.observed <- !is.na(dataset[i.cell.dataset])
                        if (cell.observed) {
                            model <- dataModels[[i.dataset]]
                            i.contrib.to.cell <- dembase::getIShared(i = i.cell.y, transform = transform)
                            collapsed.y.curr <- sum(y[i.contrib.to.cell])
                            diff.prop.curr <- yProp[i.element.indices.y] - y[i.cell.y]
                            collapsed.y.prop <- collapsed.y.curr + diff.prop.curr
                            log.lik.prop <- logLikelihood(model = model,
                                                          count = collapsed.y.prop,
                                                          dataset = dataset,
                                                          i = i.cell.dataset)
                            if (is.infinite(log.lik.prop)) {
                                ans <- -Inf
                                ans.infinite <- TRUE
                                break
                            }
                            log.lik.curr <- logLikelihood(model = model,
                                                          count = collapsed.y.curr,
                                                          dataset = dataset,
                                                          i = i.cell.dataset)
                            ans <- ans + log.lik.prop - log.lik.curr
                        }
                    }
                    i.dataset <- i.dataset + 1L
                }
            }
            i.element.indices.y <- i.element.indices.y + 1L
        }
        ans
    }
}

## TRANSLATED
## HAS_TESTS
## Calling function should test that dataset[i] is not missing
logLikelihood_Binomial <- function(model, count, dataset, i, useC = FALSE) {
    ## model
    stopifnot(methods::is(model, "Model"))
    stopifnot(methods::is(model, "Binomial"))
    ## count
    stopifnot(identical(length(count), 1L))
    stopifnot(is.integer(count))
    stopifnot(!is.na(count))
    stopifnot(count >= 0L)
    ## dataset
    stopifnot(is.integer(dataset))
    stopifnot(all(dataset[!is.na(dataset)] >= 0L))
    ## i
    stopifnot(identical(length(i), 1L))
    stopifnot(is.integer(i))
    stopifnot(!is.na(i))
    stopifnot(i >= 1L)
    ## dataset and i
    stopifnot(i <= length(dataset))
    stopifnot(!is.na(dataset@.Data[i]))
    ## model and dataset
    stopifnot(identical(length(model@theta), length(dataset)))
    ## model and i
    stopifnot(i <= length(model@theta))
    if (useC) {
        .Call(logLikelihood_Binomial_R, model, count, dataset, i)
    }
    else {
        x <- dataset[[i]]
        prob <- model@theta[i]
        stats::dbinom(x = x, size = count, prob = prob, log = TRUE)
    }
}


## TRANSLATED
## HAS_TESTS
## *************************************************************
## NOTE THAT THIS FUNCTION RETURNS THE UNNORMALISED LIKELIHOOD.
## THIS IS FINE WHEN THE FUNCTION IS BEING USED TO DECIDE WHETHER
## TO ACCEPT A PROPOSED VALUE FOR 'count' BUT WILL NOT WORK WHEN
## DECIDING TO ACCEPT A PROPOSED VALUE FOR 'theta', OR FOR CALCULATING
## LIKELIHOODS MORE GENERALLY.
## *************************************************************
## Calling function should test that dataset[i] is not missing
logLikelihood_CMP <- function(model, count, dataset, i, useC = FALSE) {
    ## model
    stopifnot(methods::is(model, "Model"))
    stopifnot(methods::is(model, "UseExposure"))
    ## count
    stopifnot(identical(length(count), 1L))
    stopifnot(is.integer(count))
    stopifnot(!is.na(count))
    stopifnot(count >= 0L)
    ## dataset
    stopifnot(is.integer(dataset))
    stopifnot(all(dataset[!is.na(dataset)] >= 0L))
    ## i
    stopifnot(identical(length(i), 1L))
    stopifnot(is.integer(i))
    stopifnot(!is.na(i))
    stopifnot(i >= 1L)
    ## dataset and i
    stopifnot(i <= length(dataset))
    stopifnot(!is.na(dataset@.Data[i]))
    ## model and dataset
    stopifnot(identical(length(model@theta), length(dataset)))
    ## model and i
    stopifnot(i <= length(model@theta))
    if (useC) {
        .Call(logLikelihood_CMP_R, model, count, dataset, i)
    }
    else {
        x <- dataset[[i]]
        gamma <- model@theta[i] * count
        nu <- model@nuCMP[i]
        nu * (x * log(gamma) - lgamma(x + 1))
    }
}

## TRANSLATED
## HAS_TESTS
## Calling function should test that dataset[i] is not missing
logLikelihood_Poisson <- function(model, count, dataset, i, useC = FALSE) {
    ## model
    stopifnot(methods::is(model, "Model"))
    stopifnot(methods::is(model, "UseExposure"))
    ## count
    stopifnot(identical(length(count), 1L))
    stopifnot(is.integer(count))
    stopifnot(!is.na(count))
    stopifnot(count >= 0L)
    ## dataset
    stopifnot(is.integer(dataset))
    stopifnot(all(dataset[!is.na(dataset)] >= 0L))
    ## i
    stopifnot(identical(length(i), 1L))
    stopifnot(is.integer(i))
    stopifnot(!is.na(i))
    stopifnot(i >= 1L)
    ## dataset and i
    stopifnot(i <= length(dataset))
    stopifnot(!is.na(dataset@.Data[i]))
    ## model and dataset
    stopifnot(identical(length(model@theta), length(dataset)))
    ## model and i
    stopifnot(i <= length(model@theta))
    if (useC) {
        .Call(logLikelihood_Poisson_R, model, count, dataset, i)
    }
    else {
        x <- dataset[[i]]
        rate <- model@theta[i]
        lambda <- rate * count
        stats::dpois(x = x, lambda = lambda, log = TRUE)
    }
}

## TRANSLATED
## HAS_TESTS
## Calling function should test that dataset[i] is not missing
logLikelihood_PoissonBinomialMixture <- function(model, count, dataset, i, useC = FALSE) {
    ## model
    stopifnot(methods::is(model, "Model"))
    stopifnot(methods::is(model, "PoissonBinomialMixture"))
    ## count
    stopifnot(identical(length(count), 1L))
    stopifnot(is.integer(count))
    stopifnot(!is.na(count))
    stopifnot(count >= 0L)
    ## dataset
    stopifnot(is.integer(dataset))
    stopifnot(all(dataset[!is.na(dataset)] >= 0L))
    ## i
    stopifnot(identical(length(i), 1L))
    stopifnot(is.integer(i))
    stopifnot(!is.na(i))
    stopifnot(i >= 1L)
    ## dataset and i
    stopifnot(i <= length(dataset))
    stopifnot(!is.na(dataset@.Data[i]))
    if (useC) {
        .Call(logLikelihood_PoissonBinomialMixture_R, model, count, dataset, i)
    }
    else {
        x <- dataset[[i]]
        prob <- model@prob
        dpoibin1(x = x, size = count, prob = prob, log = TRUE)
    }
}

## TRANSLATED
## HAS_TESTS
## Calling function should test that dataset[i] is not missing
logLikelihood_NormalFixedUseExp <- function(model, count, dataset, i, useC = FALSE) {
    ## model
    stopifnot(methods::is(model, "Model"))
    stopifnot(methods::is(model, "NormalFixedUseExp"))
    ## count
    stopifnot(identical(length(count), 1L))
    stopifnot(is.integer(count))
    stopifnot(!is.na(count))
    ## dataset
    stopifnot(is.integer(dataset))
    ## i
    stopifnot(identical(length(i), 1L))
    stopifnot(is.integer(i))
    stopifnot(!is.na(i))
    stopifnot(i >= 1L)
    ## dataset and i
    stopifnot(i <= length(dataset))
    stopifnot(!is.na(dataset@.Data[i]))
    if (useC) {
        .Call(logLikelihood_NormalFixedUseExp_R, model, count, dataset, i)
    }
    else {
        x <- dataset[[i]]
        mean <- model@mean@.Data[i]
        sd <- model@sd@.Data[i]
        mean <- count * mean
        stats::dnorm(x = x, mean = mean, sd = sd, log = TRUE)
    }
}


## Calling function should test that dataset[i] is not missing.
## Assume that 'dataset' is composed entirely of values divisible by 3
logLikelihood_Round3 <- function(model, count, dataset, i, useC = FALSE) {
    ## model
    stopifnot(methods::is(model, "Model"))
    stopifnot(methods::is(model, "Round3"))
    ## count
    stopifnot(identical(length(count), 1L))
    stopifnot(is.integer(count))
    stopifnot(!is.na(count))
    ## dataset
    stopifnot(is.integer(dataset))
    stopifnot(all(dataset[!is.na(dataset)] %% 3L == 0L))
    ## i
    stopifnot(identical(length(i), 1L))
    stopifnot(is.integer(i))
    stopifnot(!is.na(i))
    stopifnot(i >= 1L)
    ## dataset and i
    stopifnot(i <= length(dataset))
    stopifnot(!is.na(dataset@.Data[i]))
    if (useC) {
        .Call(logLikelihood_Round3_R, model, count, dataset, i)
    }
    else {
        x <- dataset[[i]]
        diff <- abs(x - count)
        if (diff > 2L)
            -Inf
        else if (diff == 2L)
            -log(3)
        else if (diff == 1L)
            log(2) - log(3)
        else
            0
    }
}

## TRANSLATEd
## HAS_TESTS
## Calling function should test that dataset[i] is not missing
logLikelihood_TFixedUseExp <- function(model, count, dataset, i, useC = FALSE) {
    ## model
    stopifnot(methods::is(model, "Model"))
    stopifnot(methods::is(model, "TFixedUseExp"))
    ## count
    stopifnot(identical(length(count), 1L))
    stopifnot(is.integer(count))
    stopifnot(!is.na(count))
    ## dataset
    stopifnot(is.integer(dataset))
    ## i
    stopifnot(identical(length(i), 1L))
    stopifnot(is.integer(i))
    stopifnot(!is.na(i))
    stopifnot(i >= 1L)
    ## dataset and i
    stopifnot(i <= length(dataset))
    stopifnot(!is.na(dataset@.Data[i]))
    if (useC) {
        .Call(logLikelihood_TFixedUseExp_R, model, count, dataset, i)
    }
    else {
        x <- dataset[[i]]
        mean <- model@mean@.Data[i]
        sd <- model@sd@.Data[i]
        nu <- model@nu@.Data
        mean <- count * mean
        x.rescaled <- (x - mean) / sd
        stats::dt(x = x.rescaled, df = nu, log = TRUE) - log(sd) # Jacobian
    }
}



## TRANSLATED
## HAS_TESTS
## Given the position of a cell, and the transform from the object
## to the subtotals, 'makeIOther' returns the position of another cell
## that belongs to the same subtotal.  If the cell is the only cell
## included in the subtotal, then a value of 0L is returned. If the
## cell does not belong to any subtotal, a value of -1L is returned.
makeIOther <- function(i, transform, useC = FALSE) {
    ## i
    stopifnot(identical(length(i), 1L))
    stopifnot(is.integer(i))
    stopifnot(!is.na(i))
    stopifnot(i >= 1L)
    ## transform
    stopifnot(methods::is(transform, "CollapseTransformExtra"))
    ## i and transform
    stopifnot(i <= prod(transform@dimBefore))
    if (useC) {
        .Call(makeIOther_R, i, transform)
    }
    else {
        i.shared <- dembase::getIShared(i = i, transform = transform)
        n.shared <- length(i.shared)
        if (n.shared == 0L)
            -1L
        else if (n.shared == 1L)
            0L
        else {
            ## draw pair in way that can be replicated in C
            which.self <- which(i.shared == i)
            ## select 'which.shared' from 1, ..., n.shared-1
            which.shared <- as.integer(stats::runif(n = 1L) * (n.shared - 1L)) + 1L
            if (which.shared == n.shared) ## in case stats::runif(n = 1L) == 1.0
                which.shared = n.shared - 1L;
            ## if which.shared < which.self, leave as is; otherwise add 1
            if (which.shared >= which.self)
                which.shared <- which.shared + 1L
            i.shared[which.shared]
        }
    }
}



## ESTIMATION #######################################################################


## HAS_TESTS
joinFiles <- function(filenamesFirst, filenamesLast) {
    kLength <- 10000
    if (length(filenamesFirst) != length(filenamesLast))
        stop(gettextf("'%s' and '%s' have different lengths",
                      "filenamesFirst", "filenamesLast"))
    for (i in seq_along(filenamesFirst)) {
        con.append <- file(filenamesFirst[i], "ab")
        con.read <- file(filenamesLast[i], "rb")
        finished <- FALSE
        while (!finished) {
            object <- readBin(con = con.read, what = "double", n = kLength)
            writeBin(object, con = con.append)
            finished <- length(object) < kLength
        }
        close(con.append)
        close(con.read)
        unlink(filenamesLast[i])
    }
    NULL
}

## ## NO_TESTS
## estimateOneChain <- function(combined, seed, tempfile, nBurnin, nSim, nThin,
##                              continuing, ...) {
##     ## set seed if continuing
##     if (!is.null(seed))
##         assign(".Random.seed", seed, envir = .GlobalEnv)
##     ## burnin
##     combined <- updateCombined(combined, nUpdate = nBurnin, useC = TRUE)
##     ## production
##     con <- file(tempfile, open = "wb")
##     n.prod <- nSim %/% nThin
##     for (i in seq_len(n.prod)) {
##         combined <- updateCombined(combined, nUpdate = nThin, useC = TRUE)
##         values <- extractValues(combined)
##         writeBin(values, con = con)
##     }
##     close(con)
##     ## return final state
##     combined
## }

## We limit the number of updates in any one call to .Call, because R does
## not release memory until the end of the call.
estimateOneChain <- function(combined, seed, tempfile, nBurnin, nSim, nThin,
                             nUpdateMax, continuing, useC, ...) {
    ## set seed if continuing
    if (!is.null(seed))
        assign(".Random.seed", seed, envir = .GlobalEnv)
    ## burnin
    nLoops <- nBurnin %/% nUpdateMax
    for (i in seq_len(nLoops)) {
        combined <- updateCombined(combined, nUpdate = nUpdateMax, useC = useC)
    }
    ## and any final ones
    nLeftOver <- nBurnin - nLoops * nUpdateMax
    combined <- updateCombined(combined, nUpdate = nLeftOver, useC = useC)
    ## production
    con <- file(tempfile, open = "wb")
    n.prod <- nSim %/% nThin
    for (i in seq_len(n.prod)) {
        nLoops <- nThin %/% nUpdateMax
        for (i in seq_len(nLoops)) {
           combined <- updateCombined(combined, nUpdate = nUpdateMax, useC = useC)
        }
        ## and any final ones
        nLeftOver <- nThin - nLoops * nUpdateMax
        combined <- updateCombined(combined, nUpdate = nLeftOver, useC = useC)
        values <- extractValues(combined)
        writeBin(values, con = con)
    }
    close(con)
    ## return final state
    combined
}

## HAS_TESTS
finalMessage <- function(filename, verbose) {
    if (verbose)
        message(gettextf("results contained in file \"%s\"", filename))
    else
        invisible(NULL)
}

## HAS_TESTS
makeControlArgs <- function(call, parallel, nUpdateMax) {
    ## call is 'call'
    if (!is.call(call))
        stop(gettextf("'%s' does not have class \"%s\"",
                      "call", "call"))
    ## parallel is logical
    if (!is.logical(parallel))
        stop(gettextf("'%s' does not have type \"%s\"",
                      "parallel", "logical"))
    ## 'parallel' has length 1
    if (!identical(length(parallel), 1L))
        stop(gettextf("'%s' does not have length %d",
                      "parallel", 1L))
    ## 'parallel' is not missing
    if (is.na(parallel))
        stop(gettextf("'%s' is missing",
                      "parallel"))
    ## 'nUpdateMax' is length 1
    if (!identical(length(nUpdateMax), 1L))
        stop(gettextf("'%s' does not have length %d",
                      "nUpdateMax", 1L))
    ## 'nUpdateMax' is not missing
    if (is.na(nUpdateMax))
        stop(gettextf("'%s' is missing",
                      "nUpdateMax"))
    ## 'nUpdateMax' is numeric
    if (!is.numeric(nUpdateMax))
        stop(gettextf("'%s' is non-numeric",
                      "nUpdateMax"))
    ## 'nUpdateMax' is integer
    if (round(nUpdateMax) != nUpdateMax)
        stop(gettextf("'%s' has non-integer value",
                      "nUpdateMax"))
    nUpdateMax <- as.integer(nUpdateMax)
    ## 'nUpdateMax' positive
    if (nUpdateMax < 1L)
        stop(gettextf("'%s' is less than %d",
                      "nUpdateMax", 1L))
    list(call = call,
         parallel = parallel,
         lengthIter = NULL,
         nUpdateMax = nUpdateMax)
}

## HAS_TESTS
makeMCMCArgs <- function(nBurnin, nSim, nChain, nThin) {
    for (name in c("nBurnin", "nSim", "nChain", "nThin")) {
        value <- get(name)
        ## length 1
        if (!identical(length(value), 1L))
            stop(gettextf("'%s' does not have length %d",
                          name, 1L))
        ## is not missing
        if (is.na(value))
            stop(gettextf("'%s' is missing",
                          name))
        ## is numeric
        if (!is.numeric(value))
            stop(gettextf("'%s' is non-numeric",
                          name))
        ## is integer
        if (round(value) != value)
            stop(gettextf("'%s' has non-integer value",
                          name))
        assign(name, value = as.integer(value))
    }
    ## 'nBurnin', nSim non-negative
    for (name in c("nBurnin", "nSim")) {
        value <- get(name)
        if (value < 0L)
            stop(gettextf("'%s' is negative",
                          name))
    }
    ## 'nChain', 'nThin' positive
    for (name in c("nChain", "nThin")) {
        value <- get(name)
        if (value < 1L)
            stop(gettextf("'%s' is less than %d",
                          name, 1L))
    }
    ## nThin <= nSim if nSim positive
    if ((nSim > 0L) && (nThin > nSim))
        stop(gettextf("'%s' is greater than '%s'",
                      "nThin", "nSim"))
    list(nBurnin = nBurnin,
         nSim = nSim,
         nChain = nChain,
         nThin = nThin)
}



## PRINTING ##########################################################################

expandTermsMod <- function(names) {
    ans <- "(Intercept)"
    if (length(names) > 1L) {
        other.terms <- names[-1L]
        other.terms <- paste0(other.terms, "[j[i]]")
        other.terms <- paste(other.terms, collapse = " + ")
        ans <- paste(ans, other.terms, sep = " + ")
    }
    ans
}

expandTermsSpec <- function(f) {
    terms <- terms(f)
    term.labels <- attr(terms, "term.labels")
    ans <- paste0(term.labels, "[j[i]]")
    ans <- paste(ans, collapse = " + ")
    ans
}

printAggregateEqns <- function(object) {
    if (methods::extends(class(object), "Aggregate")) {
        printAgValEqns(object)
        printAgAccuracyEqns(object)
    }
    else
        invisible()
}

printAggregateSpecEqns <- function(object) {
    aggregate <- object@aggregate
    printSpecAggregateEqns(aggregate)
}

printBinomialLikEqns <- function(object) {
    formulaMu <- object@formulaMu
    terms <- expandTermsSpec(formulaMu)
    cat("            y[i] ~ binomial(exposure[i], prob[i])\n")
    cat("  logit(prob[i]) ~ N(", terms, ", sd^2)\n", sep = "")
}

printBinomialModEqns <- function(object) {
    call <- object@call
    lower <- object@lower
    upper <- object@upper
    names <- object@namesBetas
    series <- call$series
    name.y <- deparse(call$formula[[2L]])
    if (is.null(series)) {
        if (identical(name.y, "y"))
            exposure <- "exposure"
        else
            exposure <- "y"
    }
    else
        exposure <- series
    name.y <- sprintf("%13s", name.y)
    lower <- invlogit1(lower)
    upper <- invlogit1(upper)
    terms <- expandTermsMod(names)
    cat(name.y, "[i] ~ binomial(", exposure, "[i], prob[i])", sep = "")
    if ((0 < lower) || (upper < 1))
        cat(",  ", format(lower, digits = 4), "< prob[i] <", format(upper, digits = 4))
    cat("\n")
    cat("  logit(prob[i]) ~ N(", terms, ", sd^2)\n", sep = "")
}

printBinomialSpecEqns <- function(object) {
    formulaMu <- object@formulaMu
    nameY <- object@nameY
    series <- object@series@.Data
    lower <- object@lower
    upper <- object@upper
    has.series <- !is.na(series)
    name.y <- sprintf("%13s", nameY)
    if (has.series)
        exposure <- series        
    else
        exposure <- "exposure"
    terms <- expandTermsSpec(formulaMu)
    cat(name.y, "[i] ~ binomial(", exposure, "[i], prob[i])", sep = "")
    if ((0 < lower) || (upper < 1))
        cat(",  ", format(lower, digits = 4), "< prob[i] <", format(upper, digits = 4))
    cat("\n")
    cat("  logit(prob[i]) ~ N(", terms, ", sd^2)\n", sep = "")
}

printCMPLikEqns <- function(object) {
    formulaMu <- object@formulaMu
    useExpose <- object@useExpose@.Data
    mean <- object@meanLogNuCMP@.Data
    sd <- object@sdLogNuCMP@.Data
    terms <- expandTermsSpec(formulaMu)
    if (useExpose) {
        cat("              y[i] ~ CMP(rate[i] * exposure[i], dispersion[i])\n")
        cat("      log(rate[i]) ~ N(", terms, ", sd^2)\n", sep = "")
    }
    else {
        cat("              y[i] ~ CMP(count[i], dispersion[i])\n")
        cat("     log(count[i]) ~ N(", terms, ", sd^2)\n", sep = "")
    }
    cat("log(dispersion[i]) ~ N(", mean, ", ", squaredOrNA(sd), ")\n", sep = "")
}

printCMPModEqns <- function(object) {
    call <- object@call
    mean <- object@meanLogNuCMP@.Data
    sd <- object@sdLogNuCMP@.Data
    lower <- object@lower
    upper <- object@upper
    names <- object@namesBetas
    uses.exposure <- methods::is(object, "UseExposure")
    series <- call$series
    name.y <- deparse(call$formula[[2L]])
    if (is.null(series)) {
        if (identical(name.y, "y"))
            exposure <- "exposure"
        else
            exposure <- "y"
    }
    else
        exposure <- series
    name.y <- sprintf("%15s", name.y)
    lower <- exp(lower)
    upper <- exp(upper)
    terms <- expandTermsMod(names)
    if (uses.exposure) {
        cat(name.y, "[i] ~ CMP(rate[i] * ", exposure, "[i])", sep = "")
        if (lower > 0 || is.finite(upper))
            cat(",  ", format(lower, digits = 4), "< rate[i] <", format(upper, digits = 4))
        cat("\n")
        cat("      log(rate[i]) ~ N(", terms, ", sd^2)\n", sep = "")
    }
    else {
        cat(name.y, "[i] ~ CMP(count[i])", sep = "")
        if ((lower > 0) || (is.finite(upper)))
            cat(",  ", format(lower, digits = 4), "< count[i] <", format(upper, digits = 4))
        cat("\n")
        cat("     log(count[i]) ~ N(", terms, ", sd^2)\n", sep = "")
    }
    cat("log(dispersion[i]) ~ N(", mean, ", ", squaredOrNA(sd), ")\n", sep = "")
}

printCMPSpecEqns <- function(object) {
    formulaMu <- object@formulaMu
    nameY <- object@nameY
    series <- object@series@.Data
    lower <- object@lower
    upper <- object@upper
    useExpose <- object@useExpose@.Data
    mean <- object@meanLogNuCMP@.Data
    sd <- object@sdLogNuCMP@.Data
    has.series <- !is.na(series)
    name.y <- sprintf("%15s", nameY)
    terms <- expandTermsSpec(formulaMu)
    if (useExpose) {
        if (has.series)
            exposure <- series
        else
            exposure <- "exposure"
        cat(name.y, "[i] ~ CMP(rate[i] * ", exposure, "[i], dispersion[i])", sep = "")
        if (lower > 0 || is.finite(upper))
            cat(",  ", format(lower, digits = 4), "< rate[i] <", format(upper, digits = 4))
        cat("\n")
        cat("      log(rate[i]) ~ N(", terms, ", sd^2)\n", sep = "")
    }
    else {
        cat("              y[i] ~ CMP(count[i], dispersion[i])")
        if (lower > 0 || is.finite(upper))
            cat(",  ", format(lower, digits = 4), "< count[i] <", format(upper, digits = 4))
        cat("\n")
        cat("     log(count[i]) ~ N(", terms, ", sd^2)  \n", sep = "")
    }
    cat("log(dispersion[i]) ~ N(", mean, ", ", squaredOrNA(sd), ")\n", sep = "")
}


printCovariatesEqns <- function(object) {
    AEtaIntercept <- object@AEtaIntercept@.Data
    AEtaCoef <- object@AEtaCoef@.Data
    meanEtaCoef <- object@meanEtaCoef@.Data
    nuEtaCoef <- object@nuEtaCoef@.Data
    n <- length(nuEtaCoef)
    cat("    covariate[j] ~ (Intercept) + data[j,] * coef\n")
    cat("     (Intercept) ~ N(0, ", squaredOrNA(AEtaIntercept), ")\n", sep = "")
    if (n == 1L)
        cat("            coef ~ t(", nuEtaCoef, ", ", meanEtaCoef, ", ", squaredOrNA(AEtaCoef), ")\n",
            sep = "")
    else
        cat("            coef ~ t([", paste(nuEtaCoef, collapse = ","), "], [",
            paste(meanEtaCoef, collapse = ","), "], [",
            paste(sapply(AEtaCoef, squaredOrNA), collapse = ","), "])\n",
            sep = "")
}

printCovariatesDLMEqns <- function(object, isMain) {
    AEtaIntercept <- object@AEtaIntercept@.Data
    AEtaCoef <- object@AEtaCoef@.Data
    meanEtaCoef <- object@meanEtaCoef@.Data
    nuEtaCoef <- object@nuEtaCoef@.Data
        n <- length(nuEtaCoef)
    if (isMain)
        cat("    covariate[j] ~ (Intercept) + data[j,] * coef\n")
    else
        cat("  covariate[k,l] ~ (Intercept) + data[k,l,] * coef\n")
    cat("     (Intercept) ~ N(0, ", squaredOrNA(AEtaIntercept), ")\n", sep = "")
    if (n == 1L)
        cat("            coef ~ t(", nuEtaCoef, ", ", meanEtaCoef, ", ", squaredOrNA(AEtaCoef), ")\n",
            sep = "")
    else
        cat("            coef ~ t([", paste(nuEtaCoef, collapse = ","), "], [",
            paste(meanEtaCoef, collapse = ","), "], [",
            paste(sapply(AEtaCoef, squaredOrNA), collapse = ","), "])\n",
            sep = "")
}

printDLMEqns <- function(object, name, order, hasTrend, hasSeason, hasCovariates) {
    is.main <- order == 1L
    if (is.null(name))
        name <- "parameter"
    if (is.main) {
        name <- sprintf("%13s", name, sep = "")
        cat(name, "[j] = level[j] + ", sep = "")
    }
    else {
        name <- sprintf("%11s", name, sep = "")
        cat(name, "[k,l] = level[k,l] + ", sep = "")
    }                  
    if (hasSeason) {
        if (is.main)
            cat("season[j] + ")
        else
            cat("season[k,l] + ")
    }
    if (hasCovariates) {
        if (is.main)
            cat("covariates[j] + ")
        else
            cat("covariates[k,l] + ")
    }
    if (is.main)
        cat("error[j]\n")
    else
        cat("error[k,l]\n")
    printLevelTrendEqns(object = object,
                        isMain = is.main,
                        hasTrend = hasTrend)
    if (hasSeason)
        printSeasonEqns(object = object,
                        isMain = is.main)
    if (hasCovariates)
        printCovariatesDLMEqns(object = object,
                               isMain = is.main)
    printErrorDLMEqns(object = object,
                      isMain = is.main)
}

printErrorDLMEqns <- function(object, isMain) {
    nuTau <- object@nuTau
    A <- object@ATau@.Data
    max <- object@tauMax
    is.robust <- methods::is(object, "SpecRobustMixin")
    if (is.robust) {
        nuBeta <- object@nuBeta
        if (isMain)
            cat("        error[j] ~ t(", nuBeta, ", 0, scaleError^2)\n", sep = "")
        else
            cat("      error[k,l] ~ t(", nuBeta, ", 0, scaleError^2)\n", sep = "")
    }
    else {
        if (isMain)
            cat("        error[j] ~ N(0, scaleError^2)\n")
        else
            cat("      error[k,l] ~ N(0, scaleError^2)\n")
    }
    cat("      scaleError ~ trunc-half-t(", nuTau, ", ", sep = "")
    cat(squaredOrNA(A), ", ", format(max, digits = 4), ")\n", sep = "")
}

printErrorEqns <- function(object) {
    nuTau <- object@nuTau
    A <- object@ATau@.Data
    max <- object@tauMax
    is.robust <- methods::is(object, "SpecRobustMixin")
    if (is.robust) {
        nuBeta <- object@nuBeta
        cat("        error[j] ~ t(", nuBeta, ", 0, scaleError^2)\n", sep = "")
    }
    else
        cat("        error[j] ~ N(0, scaleError^2)\n")
    cat("      scaleError ~ trunc-half-t(", nuTau, ", ", sep = "")
    cat(squaredOrNA(A), ", ", format(max, digits = 4), ")\n", sep = "")
}

printExchEqns <- function(object, name, hasCovariates) {
    if (is.null(name))
        name <- "parameter"
    name <- sprintf("%13s", name) 
    cat(name, "[j] = ", sep = "")
    if (hasCovariates)
        cat("covariate[j] + ")
    cat("error[j]\n")
    if (hasCovariates)
        printCovariatesEqns(object)
    printErrorEqns(object)
}

printExchFixedEqns <- function(object, name) {
    mean <- object@mean@.Data
    sd <- object@tau@.Data
    if (is.null(name))
        name <- "parameter"
    name <- sprintf("%13s", name) 
    cat(name, "[j] ~ N(", mean, ", ", squaredOrNA(sd), ")\n", sep = "")
}

printJump <- function(object) {
    aggregate <- object@aggregate
    scale.theta <- stringScaleTheta(object)
    scale.ag <- stringScaleAg(aggregate)
    print.scale.theta <- nzchar(scale.theta)
    print.scale.ag <- nzchar(scale.ag)
    if (print.scale.theta || print.scale.ag) {
        cat("\njump:\n")
        if (print.scale.theta)
            cat(scale.theta)
        if (print.scale.ag)
            cat(scale.ag)
    }
    else
        invisible()
}

printJumpAg <- function(object) {
    jump <- object@scaleAg
    value <- object@valueAg
    if (methods::.hasSlot(object, "weightAg"))
        has.weight <- !is.null(object@weightAg)
    else
        has.weight <- FALSE
    value.is.scalar <- identical(length(value), 1L)
    if (!value.is.scalar || has.weight)
        cat("\n")
    cat("jump:", jump, "\n")
}

printKnownEqns <- function(object, name) {
    is.uncertain <- (methods::is(object, "SpecKnownUncertain")
        || methods::is(object, "KnownUncertain"))
    if (is.null(name))
        name <- "parameter"
    name <- sprintf("%13s", name)
    if (is.uncertain)
        cat(name, "[j] ~ N(mean[j], sd[j]^2)\n", sep = "")
    else
        cat(name, "[j] = mean[j]\n", sep = "")
}

printLevelTrendEqns <- function(object, isMain, hasTrend) {
    AAlpha <- object@AAlpha@.Data
    omegaAlphaMax <- object@omegaAlphaMax
    nuAlpha <- object@nuAlpha
    phi <- object@phi
    phi.known <- object@phiKnown
    min.phi <- object@minPhi
    max.phi <- object@maxPhi
    shape1 <- object@shape1Phi@.Data
    shape2 <- object@shape2Phi@.Data
    is.spec <- methods::is(object, "SpecPrior")
    if (hasTrend) {
        has.level <- object@hasLevel@.Data
        ADelta0 <- object@ADelta0@.Data
        ADelta <- object@ADelta@.Data
        meanDelta0 <- object@meanDelta0@.Data
        nuDelta <- object@nuDelta@.Data
        omegaDeltaMax <- object@omegaDeltaMax@.Data
        if (is.spec)
            AAlpha0 <- NA
        else {
            DC <- object@DC@.Data
            AAlpha0 <- DC[[1L]][1L]
        }
    }
    else {
        has.level <- FALSE
        if (is.spec)
            AAlpha0 <- NA
        else {
            C <- object@CNoTrend@.Data
            AAlpha0 <- sqrt(C[[1L]])
        }
    }
    show.damp <- !phi.known || (phi < 1)
    if (hasTrend) {
        if (isMain) {
            cat("        level[j] = level[j-1] + trend[j-1]")
            if (has.level)
                cat(" + errorLevel[j]\n")
            else
                cat("\n")
            cat("        trend[j] = ")
        }
        else {
            cat("      level[k,l] = level[k-1,l] + trend[k-1,l]")
            if (has.level)
                cat("+ errorLevel[k,l]\n")
            else
                cat("\n")
            cat("      trend[k,l] = ")
        }
        if (show.damp)
            cat("damp * ")
        if (isMain)
            cat("trend[j-1] + errorTrend[j]\n")
        else
            cat("trend[k-1,l] + errorTrend[k,l]\n")
    }
    else {
        if (isMain)
            cat("        level[j] = ")
        else
            cat("      level[k,l] = ")
        if (show.damp)
            cat("damp * ")
        if (isMain)
            cat("level[j-1] + errorLevel[j]\n")
        else
            cat("level[k-1,l] + errorLevel[k,l]\n")
    }
    if (isMain) {
        if (!hasTrend || (hasTrend && has.level))
            cat("        level[0] ~ N(0, ", squaredOrNA(AAlpha0), ")\n", sep = "")
        else
            cat("        level[0] = 0\n")
        if (hasTrend) {
            cat("        trend[0] ~ N(", meanDelta0, ", ", sep = "")
            cat(squaredOrNA(ADelta0), ")\n", sep = "")
        }
    }
    else {
        if (!hasTrend || (hasTrend && has.level))
            cat("      level[0,l] ~ N(0, ", squaredOrNA(AAlpha0), ")\n", sep = "")
        else
            cat("      level[0,l] = 0\n")
        if (hasTrend) {
            cat("      trend[0,l] ~ N(", meanDelta0, ", ", sep = "")
            cat(squaredOrNA(ADelta0), ")\n", sep = "")
        }
    }
    if (show.damp) {
        if (phi.known)
            cat("            damp =",
                format(phi, digits = 4),
                "\n")
        else {
            cat("   dampTransform = (damp-",
                format(min.phi, digits = 4),
                ")/(",
                format(max.phi, digits = 4),
                "-",
                format(min.phi, digits = 4),
                ")\n",
                sep = "")
            cat("   dampTransform ~ Beta(",
                format(shape1, digits = 4),
                ",",
                format(shape2, digits = 4),
                ")\n",
                sep = "")
        }
    }
    if (isMain) {
        if (!hasTrend || (hasTrend && has.level))
            cat("   errorLevel[j] ~ N(0, scaleLevel^2)\n")
    }
    else {
        if (!hasTrend || (hasTrend && has.level))
            cat(" errorLevel[k,l] ~ N(0, scaleLevel^2)\n")
    }
    if (hasTrend) {
        if (isMain)
            cat("   errorTrend[j] ~ N(0, scaleTrend^2)\n")
        else
            cat(" errorTrend[k,l] ~ N(0, scaleTrend^2)\n")
    }
    if (!hasTrend || (hasTrend && has.level)) {
        cat("      scaleLevel ~ trunc-half-t(", nuAlpha, ", ", sep = "")
        cat(squaredOrNA(AAlpha),
            ", ",
            format(omegaAlphaMax, digits = 4),
            ")\n",
            sep = "")
    }
    if (hasTrend) {
        cat("      scaleTrend ~ trunc-half-t(", nuDelta, ", ", sep = "")
        cat(squaredOrNA(ADelta),
            ", ",
            format(omegaDeltaMax, digits = 4),
            ")\n",
            sep = "")
    }
}

printMixEqns <- function(object, name, hasCovariates) {
    AVectors <- object@AVectorsMix@.Data
    nuVectors <- object@nuVectorsMix@.Data
    omegaVectorsMax <- object@omegaVectorsMaxMix@.Data
    priorMean <- object@priorMeanLevelComponentWeightMix@.Data
    priorSD <- object@priorSDLevelComponentWeightMix@.Data
    AComponentWeight <- object@AComponentWeightMix@.Data
    nuComponentWeight <- object@nuComponentWeightMix@.Data
    omegaComponentWeightMax <- object@omegaComponentWeightMaxMix@.Data
    ALevelComponentWeight <- object@ALevelComponentWeightMix@.Data
    nuLevelComponentWeight <- object@nuLevelComponentWeightMix@.Data
    omegaLevelComponentWeightMax <- object@omegaLevelComponentWeightMaxMix@.Data
    phi <- object@phiMix
    phi.known <- object@phiKnown
    min.phi <- object@minPhi
    max.phi <- object@maxPhi
    shape1 <- object@shape1Phi@.Data
    shape2 <- object@shape2Phi@.Data
    if (is.null(name))
        name <- "parameter"
    name <- sprintf("%11s", name, sep = "")
    cat(name, "[k,l] ~ Mix(components, weights, ", sep = "")
    if (hasCovariates)
        cat("covariates, ")
    cat("error)\n")
    cat("       component ~ N(0, scaleComponent^2)\n")
    cat("  scaleComponent ~ trunc-half-t(", nuVectors, ", ", sep = "")
    cat(squaredOrNA(AVectors),
        ", ",
        format(omegaVectorsMax, digits = 4),
        ")\n",
        sep = "")
    cat("     weight[k,h] = g(level1[k,1], ..., level1[k,h])\n")
    cat("     level1[k,h] = level2[k,h] + error1[k,h]\n")
    cat("     level2[k,h] = mean + damp * level2[k-1,h] + error2[k,h]\n")
    cat("     level2[1,h] ~ N(", priorMean, "/(1-damp), ", sep = "")
    cat(priorSD, "/sqrt(1-damp^2))\n", sep = "")
    cat("            mean ~ N(", priorMean, ", ", squaredOrNA(priorSD), ")\n", sep = "")
    if (phi.known)
        cat("            damp =",
            format(phi, digits = 4),
            "\n")
    else {
        cat("   dampTransform = (damp-",
            format(min.phi, digits = 4),
            ")/(",
            format(max.phi, digits = 4),
            "-",
            format(min.phi, digits = 4),
            ")\n",
            sep = "")
        cat("   dampTransform ~ Beta(",
            format(shape1, digits = 4),
            ",",
            format(shape2, digits = 4),
            ")\n",
            sep = "")
    }
    cat("          error1 ~ N(0, scale1^2)\n")
    cat("          error2 ~ N(0, scale2^2)\n")
    cat("          scale1 ~ trunc-half-t(", nuComponentWeight, ", ", sep = "")
    cat(squaredOrNA(AComponentWeight),
        ", ",
        format(omegaComponentWeightMax, digits = 4),
        ")\n",
        sep = "")
    cat("          scale2 ~ trunc-half-t(", nuLevelComponentWeight, ", ", sep = "")
    cat(squaredOrNA(ALevelComponentWeight),
        ", ",
        format(omegaLevelComponentWeightMax, digits = 4),
        ")\n",
        sep = "")
    if (hasCovariates)
        printCovariatesDLMEqns(object = object,
                               isMain = FALSE)
    printErrorDLMEqns(object,
                      isMain = FALSE)
}

printNormalFixedLikEqns <- function(object) {
    useExpose <- object@useExpose@.Data
    if (useExpose)
        cat("            y[i] ~ Normal(exposure[i] * mean[i], sd[i]^2)\n")
    else
        cat("            y[i] ~ Normal(mean[i], sd[i]^2)\n")
}

printNormalFixedModEqns <- function(object) {
    call <- object@call
    uses.exposure <- methods::is(object, "UseExposure")
    series <- call$series
    name.y <- deparse(call$formula[[2L]])
    if (is.null(series)) {
        if (identical(name.y, "y"))
            exposure <- "exposure"
        else
            exposure <- "y"
    }
    else
        exposure <- series
    name.y <- sprintf("%13s", name.y)
    if (uses.exposure)
        cat(name.y, "[i] ~ Normal(", exposure, "[i] * mean[i], sd[i]^2)\n", sep = "")
    else
        cat(name.y, "Normal(mean[i], sd[i]^2)\n", sep = "")
}

printNormalFixedSpecEqns <- function(object) {
    series <- object@series@.Data
    call <- object@call
    nameY <- object@nameY
    useExpose <- object@useExpose@.Data
    has.series <- !is.na(series)
    name.y <- deparse(call$formula[[2L]])
    name.y <- sprintf("%13s", nameY)
    if (useExpose) {
        if (has.series)
            exposure <- series        
        else
            exposure <- "exposure"
        cat(name.y, "[i] ~ Normal(", exposure, "[i] * mean[i], sd[i]^2)\n", sep = "")
    }
    else
        cat("            y[i] ~ Normal(mean[i], sd[i]^2)\n")
}

printNormalVarsigmaKnownLikEqns <- function(object) {
    formulaMu <- object@formulaMu
    varsigma <- object@varsigma
    varsigmaSetToZero <- object@varsigmaSetToZero@.Data
    terms <- expandTermsSpec(formulaMu)
    if (varsigmaSetToZero)
        cat("            y[i] = mean[i]\n", sep = "")
    else
        cat("            y[i] ~ N(mean[i], ", varsigma, "^2 / weights[i])\n", sep = "")
    cat("         mean[i] ~ N(", terms, ", sd^2)\n", sep = "")
}

printNormalVarsigmaUnknownLikEqns <- function(object) {
    formulaMu <- object@formulaMu
    nu <- object@nuVarsigma@.Data
    A <- object@AVarsigma@.Data
    max <- object@varsigmaMax@.Data
    terms <- expandTermsSpec(formulaMu)
    cat("            y[i] ~ N(mean[i], sdData^2 / weights[i])\n", sep = "")
    cat("         mean[i] ~ N(", terms, ", sd^2)\n", sep = "")
    cat("          sdData ~ trunc-half-t(", nu, ", ", sep = "")
    cat(squaredOrNA(A), ", ", format(max, digits = 4), ")\n", sep = "")
}

printNormalVarsigmaKnownModEqns <- function(object) {
    call <- object@call
    lower <- object@lower
    upper <- object@upper
    names <- object@namesBetas
    varsigma <- object@varsigma@.Data
    varsigmaSetToZero <- object@varsigmaSetToZero@.Data
    series <- call$series
    has.series <- !is.null(series)
    name.y <- deparse(call$formula[[2L]])
    name.y <- sprintf("%13s", name.y)
    terms <- expandTermsMod(names)
    if (varsigmaSetToZero)
        cat(name.y, "[i] = mean[i]", sep = "")
    else
        cat(name.y, "[i] ~ N(mean[i], ", varsigma, "^2 / weight[i])", sep = "")
    if (is.finite(lower) || is.finite(upper))
        cat(",  ", format(lower, digits = 4), "< mean[i] <", format(upper, digits = 4))
    cat("\n")
    cat("         mean[i] ~ N(", terms, ", sd^2)\n", sep = "")
}

printNormalVarsigmaKnownSpecEqns <- function(object) {
    formulaMu <- object@formulaMu
    varsigma <- object@varsigma
    varsigmaSetToZero <- object@varsigmaSetToZero@.Data
    nameY <- object@nameY
    lower <- object@lower
    upper <- object@upper
    name.y <- sprintf("%13s", nameY)
    terms <- expandTermsSpec(formulaMu)
    if (varsigmaSetToZero)
        cat(name.y, "[i] = mean[i]", sep = "")
    else
        cat(name.y, "[i] ~ N(mean[i], ", varsigma, "^2 / weight[i])", sep = "")
    if (is.finite(lower) || is.finite(upper))
        cat(",  ", format(lower, digits = 4), "< mean[i] <", format(upper, digits = 4))
    cat("\n")
    cat("         mean[i] ~ N(", terms, ", sd^2)\n", sep = "")
}

printNormalVarsigmaUnknownModEqns <- function(object) {
    call <- object@call
    lower <- object@lower
    upper <- object@upper
    names <- object@namesBetas
    nu <- object@nuVarsigma@.Data
    A <- object@AVarsigma@.Data
    max <- object@varsigmaMax@.Data
    series <- call$series
    has.series <- !is.null(series)
    name.y <- deparse(call$formula[[2L]])
    name.y <- sprintf("%13s", name.y)
    terms <- expandTermsMod(names)
    cat("            y[i] ~ N(mean[i], sdData^2 / weights[i])", sep = "")
    if (is.finite(lower) || is.finite(upper))
        cat(",  ", format(lower, digits = 4), "< mean[i] <", format(upper, digits = 4))
    cat("\n")
    cat("         mean[i] ~ N(", terms, ", sd^2)\n", sep = "")
    cat("          sdData ~ trunc-half-t(", nu, ", ", sep = "")
    cat(squaredOrNA(A), ", ", format(max, digits = 4), ")\n", sep = "")
}

printNormalVarsigmaUnknownSpecEqns <- function(object) {
    formulaMu <- object@formulaMu
    nu <- object@nuVarsigma@.Data
    A <- object@AVarsigma@.Data
    max <- object@varsigmaMax@.Data
    nameY <- object@nameY
    lower <- object@lower
    upper <- object@upper
    name.y <- sprintf("%13s", nameY)
    terms <- expandTermsSpec(formulaMu)
    cat("            y[i] ~ N(mean[i], sdData^2 / weights[i])", sep = "")
    if (is.finite(lower) || is.finite(upper))
        cat(",  ", format(lower, digits = 4), "< mean[i] <", format(upper, digits = 4))
    cat("\n")
    cat("         mean[i] ~ N(", terms, ", sd^2)\n", sep = "")
    cat("          sdData ~ trunc-half-t(", nu, ", ", sep = "")
    cat(squaredOrNA(A), ", ", format(max, digits = 4), ")\n", sep = "")
}

printPoissonBinomialLikEqns <- function(object) {
    prob <- object@prob
    cat("y[i] ~ Poisson-binomial(exposure[i], prob[i])\n")
}

printPoissonBinomialModEqns <- function(object) {
    call <- object@call
    prob <- object@prob
    series <- call$series
    name.y <- deparse(call$formula[[2L]])
    if (is.null(series)) {
        if (identical(name.y, "y"))
            exposure <- "exposure"
        else
            exposure <- "y"
    }
    else
        exposure <- series
    name.y <- sprintf("%13s", name.y)
    cat(name.y, "[i] ~ Poisson-binomial(", exposure, "[i], prob[i])\n", sep = "")
}

printPoissonBinomialSpecEqns <- function(object) {
    prob <- object@prob
    nameY <- object@nameY
    series <- object@series@.Data
    has.series <- !is.na(series)
    name.y <- sprintf("%13s", nameY)
    if (has.series)
        exposure <- series        
    else
        exposure <- "exposure"
    cat(name.y, "[i] ~ Poisson-binomial(", exposure, "[i], prob[i])\n", sep = "")
}

printPoissonLikEqns <- function(object) {
    formulaMu <- object@formulaMu
    useExpose <- object@useExpose@.Data
    structuralZeros <- object@structuralZeros
    terms <- expandTermsSpec(formulaMu)
    if (useExpose) {
        cat("            y[i] ~ Poisson(rate[i] * exposure[i])\n")
        cat("    log(rate[i]) ~ N(", terms, ", sd^2)\n", sep = "")
    }
    else {
        cat("            y[i] ~ Poisson(count[i])\n")
        cat("   log(count[i]) ~ N(", terms, ", sd^2)\n", sep = "")
    }
    cat("\nhas structural zeros: ", !is.null(structuralZeros), "\n", sep = "")
}

printPoissonModEqns <- function(object) {
    call <- object@call
    lower <- object@lower
    upper <- object@upper
    names <- object@namesBetas
    uses.exposure <- methods::is(object, "UseExposure")
    series <- call$series
    name.y <- deparse(call$formula[[2L]])
    if (is.null(series)) {
        if (identical(name.y, "y"))
            exposure <- "exposure"
        else
            exposure <- "y"
    }
    else
        exposure <- series
    name.y <- sprintf("%13s", name.y)
    lower <- exp(lower)
    upper <- exp(upper)
    terms <- expandTermsMod(names)
    if (uses.exposure) {
        cat(name.y, "[i] ~ Poisson(rate[i] * ", exposure, "[i])", sep = "")
        if (lower > 0 || is.finite(upper))
            cat(",  ", format(lower, digits = 4), "< rate[i] <", format(upper, digits = 4))
        cat("\n")
        cat("    log(rate[i]) ~ N(", terms, ", sd^2)\n", sep = "")
    }
    else {
        cat(name.y, "[i] ~ Poisson(count[i])", sep = "")
        if ((lower > 0) || (is.finite(upper)))
            cat(",  ", format(lower, digits = 4), "< count[i] <", format(upper, digits = 4))
        cat("\n")
        cat("   log(count[i]) ~ N(", terms, ", sd^2)\n", sep = "")
    }
}

printPoissonSpecEqns <- function(object) {
    formulaMu <- object@formulaMu
    nameY <- object@nameY
    series <- object@series@.Data
    lower <- object@lower
    upper <- object@upper
    useExpose <- object@useExpose@.Data
    structuralZeros <- object@structuralZeros
    has.series <- !is.na(series)
    name.y <- sprintf("%13s", nameY)
    terms <- expandTermsSpec(formulaMu)
    if (useExpose) {
        if (has.series)
            exposure <- series
        else
            exposure <- "exposure"
        cat(name.y, "[i] ~ Poisson(rate[i] * ", exposure, "[i])", sep = "")
        if (lower > 0 || is.finite(upper))
            cat(",  ", format(lower, digits = 4), "< rate[i] <", format(upper, digits = 4))
        cat("\n")
        cat("    log(rate[i]) ~ N(", terms, ", sd^2)\n", sep = "")
    }
    else {
        cat("            y[i] ~ Poisson(count[i])")
        if (lower > 0 || is.finite(upper))
            cat(",  ", format(lower, digits = 4), "< count[i] <", format(upper, digits = 4))
        cat("\n")
        cat("   log(count[i]) ~ N(", terms, ", sd^2)  \n", sep = "")
    }
    cat("\nhas structural zeros: ", !is.null(structuralZeros), "\n", sep = "")
}

printRound3LikEqns <- function(object) {
    cat("            y[i] ~ round3(exposure[i])\n")
}


printRound3ModEqns <- function(object) {
    call <- object@call
    series <- call$series
    name.y <- deparse(call$formula[[2L]])
    if (is.null(series)) {
        if (identical(name.y, "y"))
            exposure <- "exposure"
        else
            exposure <- "y"
    }
    else
        exposure <- series
    name.y <- sprintf("%13s", name.y)
    cat(name.y, "[i] ~ round3(", exposure, "[i])\n", sep = "")
}

printRound3SpecEqns <- function(object) {
    nameY <- object@nameY
    series <- object@series@.Data
    has.series <- !is.na(series)
    name.y <- sprintf("%13s", nameY)
    if (has.series)
        exposure <- series        
    else
        exposure <- "exposure"
    cat(name.y, "[i] ~ round3(", exposure, "[i])\n", sep = "")
}

printPriorsEqns <- function(object) {
    stopifnot(methods::is(object, "Varying"))
    priors <- object@priorsBetas
    names <- object@namesBetas
    margins <- object@margins
    metadata.y <- object@metadataY
    prior.intercept <- priors[[1L]]
    printPriorIntercept(prior.intercept)
    n <- length(priors)
    if (n >= 2L) {
        cat("\n")
        for (i in seq.int(from = 2L, to = n)) {
            prior <- priors[[i]]
            name <- names[[i]]
            mar <- margins[[i]]
            order <- length(mar)
            printPriorEqns(object = prior,
                           name = name,
                           order = order)
            if (i < n)
                cat("\n")
        }
    }
}

printSDAg <- function(object) {
    sd <- object@sdAg@.Data
    metadata <- object@metadataAg
    sd.is.scalar <- identical(length(sd), 1L)
    if (sd.is.scalar)
        cat("sd:", format(sd, digits = 4), "\n")
    else {
        sd <- array(sd,
                    dim = dim(metadata),
                    dimnames = dimnames(metadata))
        cat("\nsd:\n")
        print(sd, digits = 4)
    }
}

printSDEqns <- function(object) {
    if (methods::.hasSlot(object, "priorsBetas")) {
        priors <- object@priorsBetas
        is.saturated <- any(sapply(priors, function(x) x@isSaturated@.Data))
    }
    else
        is.saturated <- FALSE
    if (is.saturated)
        invisible()
    else {
        nu <- object@nuSigma@.Data
        A <- object@ASigma@.Data
        max <- object@sigmaMax@.Data
        cat("              sd ~ trunc-half-t(", nu, ", ", sep = "")
        cat(squaredOrNA(A), ", ", format(max, digits = 4), ")\n", sep = "")
    }
}

printSeasonEqns <- function(object, isMain) {
    n <- object@nSeason@.Data
    nu <- object@nuSeason@.Data
    A <- object@ASeason@.Data
    max <- object@omegaSeasonMax@.Data
    if (isMain) {
        cat("       season[j] ~ season[j-", n, "] + errorSeason[j]\n", sep = "")
        cat("  errorSeason[j] ~ N(0, scaleSeason^2)\n")
    }
    else {
        cat("     season[k,l] ~ season[k-", n, ",l] + errorSeason[k,l]\n", sep = "")
        cat("errorSeason[k,l] ~ N(0, scaleSeason^2)\n")
    }
    cat("     scaleSeason ~ trunc-half-t(", nu, ", ", sep = "")
    cat(squaredOrNA(A), ", ", format(max, digits = 4), ")\n", sep = "")
}

printSpecAggregateEqns <- function(object) {
    aggregate <- object@aggregate
    printSpecAgValEqns(object = object,
                       aggregate = aggregate)
    printSpecAgAccuracyEqns(aggregate)
}

printSpecsPriorsEqns <- function(object) {
    formulaMu <- object@formulaMu
    terms <- terms(formulaMu)
    term.labels <- attr(terms, "term.labels")
    term.orders <- attr(terms, "order")
    specs <- object@specsPriors
    names <- object@namesSpecsPriors
    i.name <- 0L
    n.name <- length(names)
    for (i.term in seq_along(term.labels)) {
        label <- term.labels[i.term]
        i.spec <- match(label, names, nomatch = 0L)
        has.label <- i.spec > 0L
        if (has.label) {
            i.name <- i.name + 1L
            spec <- specs[[i.spec]]
            name <- term.labels[i.term]
            order <- term.orders[i.term]
            printPriorEqns(object = specs[[i.spec]],
                           name = name,
                           order = order)
            cat("\n")
        }
    }
}

printTFixedLikEqns <- function(object) {
    df <- object@nu@.Data
    useExpose <- object@useExpose@.Data
    if (useExpose)
        cat("            y[i] ~ t(", df, ", exposure[i] * location[i], scale[i]^2)\n", sep = "")
    else
        cat("            y[i] ~ t(", df, ", location[i], scale[i]^2)\n", sep = "")
}


printTFixedModEqns <- function(object) {
    call <- object@call
    df <- object@nu@.Data
    uses.exposure <- methods::is(object, "UseExposure")
    series <- call$series
    name.y <- deparse(call$formula[[2L]])
    if (is.null(series)) {
        if (identical(name.y, "y"))
            exposure <- "exposure"
        else
            exposure <- "y"
    }
    else
        exposure <- series
    name.y <- sprintf("%13s", name.y)
    if (uses.exposure)
        cat(name.y, "[i] ~ t(", df, ", ", exposure, "[i] * location[i], scale[i]^2)\n", sep = "")
    else
        cat(name.y, "t(", df, ", location[i], scale[i]^2)\n", sep = "")
}

printTFixedSpecEqns <- function(object) {
    series <- object@series@.Data
    df <- object@nu@.Data
    call <- object@call
    nameY <- object@nameY
    useExpose <- object@useExpose@.Data
    has.series <- !is.na(series)
    name.y <- deparse(call$formula[[2L]])
    name.y <- sprintf("%13s", nameY)
    if (useExpose) {
        if (has.series)
            exposure <- series        
        else
            exposure <- "exposure"
        cat(name.y, "[i] ~ t(", df, ", ", exposure, "[i] * location[i], scale[i]^2)\n", sep = "")
    }
    else
        cat("            y[i] ~ t(", df, ", location[i], scale[i]^2)\n", sep = "")
} 

printWeightAg <- function(object) {
    weight <- object@weightAg
    metadata <- object@metadataAg
    if (!is.null(weight)) {
        cat("\nweights:\n")
        print(weight@.Data)
    }              
}

printValueAg <- function(object) {
    value <- object@valueAg@.Data
    metadata <- object@metadataAg
    cat("\n")
    if (is.null(metadata))
        cat("value:", value, "\n")
    else {
        value <- array(value,
                       dim = dim(metadata),
                       dimnames = dimnames(metadata))
        cat("value:\n")
        print(value, digits = 4)
    }
}

printZeroEqns <- function(name) {
    if (is.null(name))
        name <- "parameter"
    name <- sprintf("%13s", name)
    cat(name, "[j] = 0\n", sep = "")
}


squaredOrNA <- function(x) {
    if (is.na(x))
        x
    else {
        if (isTRUE(all.equal(x, 1.0)))
            format(x, digits = 4)
        else
            paste0(format(x, digits = 4), "^2")
    }
}



## INSPECT RESULTS ###################################################################


## HAS_TESTS
MCMCDemographic <- function(object, sample = NULL, nSample = 25,
                            nChain, nThin = 1L, skeleton = NULL) {
    if (!methods::is(object, "DemographicArray"))
        stop(gettextf("'%s' has class \"%s\"",
                      "object", class(object)))
    .Data <- object@.Data
    dim <- dim(object)
    n.dim <- length(dim)
    i.iter <- match("iteration", dembase::dimtypes(object), nomatch = 0L)
    if (identical(i.iter, 0L))
        stop(gettextf("no dimension with dimtype \"%s\"", "iteration"))
    n.data <- length(.Data)
    n.iter <- dim[i.iter]
    n.slice <- n.data / n.iter  ## number of values in one iteration
    s <- seq_len(n.slice)
    if (is.null(sample)) {
        checkPositiveInteger(x = nSample,
                             name = "nSample")
        indices.struc.zero <- getIndicesStrucZero(skeleton)
        s <- setdiff(s, indices.struc.zero)
        n.s <- length(s)
        if ((n.s == 1L) || (nSample >= n.s))
            sample <- s
        else
            sample <- sample(s, size = nSample, replace = FALSE)
        sample <- sort(sample)
    }
    else {
        if (!all(sample %in% s))
            stop(gettextf("'%s' has values outside the valid range", "sample"))
    }
    for (name in c("nChain", "nThin")) {
        value <- get(name)
        if (!is.numeric(value))
            stop(gettextf("'%s' does not have type \"%s\"", name, "numeric"))
        if (!identical(length(value), 1L))
            stop(gettextf("'%s' does not have length %d", name, 1L))
        if (is.na(value))
            stop(gettextf("'%s' is missing", name))
        if (value < 1L)
            stop(gettextf("'%s' is less than %d", name, 1L))
        assign(name, as.integer(value))
    }
    if (!identical(n.iter %% nChain, 0L))
        stop(gettextf("number of iterations is not divisible by '%s'", "nChain"))
    if (!identical(i.iter, n.dim)) {
        s <- seq_len(n.dim)
        .Data <- aperm(.Data, perm = c(s[-i.iter], i.iter))
    }
    .Data <- array(.Data, dim = c(n.slice, n.iter / nChain, nChain))
    .Data <- .Data[sample, , , drop = FALSE]
    makeColnames <- function(x, y) outer(x, y, FUN = paste, sep = ".")
    colnames <- Reduce(makeColnames, dimnames(object)[-i.iter])
    colnames <- colnames[sample]
    makeChainIntoMCMC <- function(i) {
        chain <- matrix(.Data[ , , i], nrow = nrow(.Data))
        chain <- t(chain)
        colnames(chain) <- colnames
        coda::mcmc(chain, thin = nThin)
    }
    l <- lapply(seq_len(nChain), makeChainIntoMCMC)
    coda::mcmc.list(l)
}

## HAS_TESTS
addIterationsToTransform <- function(transform, nIter) {
    ## transform
    stopifnot(methods::is(transform, "CollapseTransform"))
    ## nIteration
    stopifnot(is.integer(nIter))
    stopifnot(identical(length(nIter), 1L))
    stopifnot(!is.na(nIter))
    stopifnot(nIter > 0L)
    indices <- transform@indices
    dims <- transform@dims
    dimBefore <- transform@dimBefore
    dimAfter <- transform@dimAfter
    indices.ans <- c(indices, list(seq_len(nIter)))
    dims.ans <- c(dims, length(dimAfter) + 1L)
    dimBefore.ans <- c(dimBefore, nIter)
    dimAfter.ans <- c(dimAfter, nIter)
    methods::new("CollapseTransform",
        indices = indices.ans,
        dims = dims.ans,
        dimBefore = dimBefore.ans,
        dimAfter = dimAfter.ans)
}

## HAS_TESTS
calculateDF <- function(dim) {
    if (!is.integer(dim))
        stop(gettextf("'%s' does not have type \"%s\"",
                      "dim", "integer"))
    n <- length(dim)
    if (n == 0L)
        stop(gettextf("'%s' has length %d",
                      "dim", 0L))
    if (any(is.na(dim)))
        stop(gettextf("'%s' has missing values",
                      "dim"))
    if (any(dim < 2L))
        stop(gettextf("'%s' has values less than %d",
                      "dim", 2L))
    if (n == 1L)
        dim - 1L
    else {
        s <- seq_len(n)
        margins <- lapply(s[-n], function(m) utils::combn(s, m, simplify = FALSE))
        margins <- unlist(margins, recursive = FALSE)
        ans <- as.integer(prod(dim)) - 1L
        for (i in seq_along(margins))
            ans <- ans - Recall(dim[margins[[i]]])
        ans
    }
}


## HAS_TESTS
centerPolyGammaTrend <- function(object) {
    .Data <- object@.Data
    metadata <- object@metadata
    means <- apply(.Data[1L, , ], MARGIN = 2L, mean)
    .Data[1L, , ] <- sweep(.Data[1L, , ], MARGIN = 2L, means)
    methods::new("Values",
        .Data = .Data,
        metadata = metadata)
}

## HAS_TESTS
checkProbs <- function(probs) {
    if (!is.numeric(probs))
        stop(gettextf("'%s' is non-numeric",
                      "probs"))
    if (length(probs) == 0L)
        stop(gettextf("'%s' has length %d",
                      "probs", 0L))
    if (any(is.na(probs)))
        stop(gettextf("'%s' has missing values",
                      "probs"))
    if (any(duplicated(probs)))
        stop(gettextf("'%s' has duplicates",
                      "probs"))
    if (any(probs < 0))
        stop(gettextf("'%s' has negative values",
                      "probs"))
    if (any(probs > 1))
        stop(gettextf("'%s' has values greater than %d",
                      "probs", 1L))
    NULL
}

## HAS_TESTS
checkAndTidyTotalOrSampled <- function(x, model, ySampled, name) {
    if (!methods::is(x, "Counts"))
        stop(gettextf("'%s' has class \"%s\"",
                      name, class(x)))
    if (any(is.na(x)))
        stop(gettextf("'%s' has missing values",
                      name))
    if (any(x < 0L))
        stop(gettextf("'%s' has negative values",
                      name))
    x <- castPopnOrSampled(x = x, model = model, name = name)
    x <- tryCatch(dembase::makeCompatible(x = x, y = ySampled),
                  error = function(e) e)
    if (methods::is(x, "error"))
        stop(gettextf("'%s' and '%s' incompatible : %s",
                      name, "y", x$message))
    x
}

## HAS_TESTS
checkMax <- function(max) {
    if (!is.null(max)) {
        if (!identical(length(max), 1L))
            stop(gettextf("'%s' does not have length %d",
                          "max", 1L))
        if (!is.numeric(max))
            stop(gettextf("'%s' does not have type \"%s\"",
                          "max", "numeric"))
        if (is.na(max))
            stop(gettextf("'%s' is missing",
                          "max"))
        if (!isTRUE(all.equal(round(max), max)))
            stop(gettextf("'%s' is not an integer",
                          "max"))
        if (max < 1L)
            stop(gettextf("'%s' is less than %d",
                          "max", 1L))
    }
    NULL
}

## HAS_TESTS
excludeFromList <- function(object, exclude = character()) {
    if (!is.list(object))
        stop(gettextf("'%s' has class \"%s\"",
                      "object", class(object)))
    for (i in seq_along(object)) {
        if (is.list(object[[i]]))
            object[[i]] <- Recall(object[[i]], exclude = exclude)
        else {
            if (names(object)[i] %in% exclude)
                object[i] <- list(NULL)
        }
    }
    object
}

## HAS_TESTS
fetchResultsObject <- function(filename) {
    con <- file(filename, open = "rb")
    on.exit(close(con))
    size.results <- readBin(con = con, what = "integer", n = 1L)
    size.adjustments <- readBin(con = con, what = "integer", n = 1L)
    results <- readBin(con = con, what = "raw", n = size.results)
    unserialize(results)
}

## HAS_TESTS
fetchInner <- function(object, nameObject, where, iterations,
                       filename, lengthIter, nIteration,
                       listsAsSingleItems, 
                       impute = FALSE) {
    n.where <- length(where)
    if (n.where == 0L) {
        if (is.list(object)) {
            if (nameObject %in% listsAsSingleItems)
                object
            else {
                choices <- names(object)
                raiseMultipleChoicesError(choices)
            }
        }
        else
            fetchResults(object = object,
                         nameObject = nameObject,
                         filename = filename,
                         iterations = iterations,
                         nIteration = nIteration,
                         lengthIter = lengthIter,
                         impute = impute)
    }
    else {
        if (is.list(object) && !(nameObject %in% listsAsSingleItems)) {
            choices <- names(object)
            name <- where[1L]
            i <- charmatch(name, choices, nomatch = -1L)
            if (i > 0L) {
                name <- choices[i]
                Recall(object = object[[i]],
                       nameObject = name,
                       where = where[-1L],
                       iterations = iterations,
                       filename = filename,
                       lengthIter = lengthIter,
                       nIteration = nIteration,
                       listsAsSingleItems = listsAsSingleItems,
                       impute = impute)
            }
            else if (i == 0L)
                raiseMultipleMatchesError(target = name, choices = choices)
            else
                raiseNotFoundError(target = name, choices = choices)
        }
        else
            raiseOvershotError(nameObject = nameObject, where = where)
    }
}

## HAS_TESTS
## assume that 'where' valid
fetchSkeleton <- function(object, where) {
    choices <- methods::slotNames(object)
    name <- where[1L]
    i <- charmatch(name, choices)
    name <- choices[i]
    fetchSkeletonInner(methods::slot(object, name), where = where[-1L])
}

## HAS_TESTS
fetchSkeletonInner <- function(object, where) {
    n.where <- length(where)
    if (n.where == 0L)
        object
    else {
        choices <- names(object)
        name <- where[1L]
        i <- charmatch(name, choices)
        name <- choices[i]
        Recall(object = object[[i]], where = where[-1L])
    }
}

## HAS_TESTS
finiteSDInner <- function(filename, model, where, probs, iterations) {
    if (!methods::is(model, "Betas"))
        return(NULL)
    checkProbs(probs)
    names.betas <- model@namesBetas
    dims <- model@dims
    n.beta <- length(names.betas)
    if (n.beta == 1L)
        return(NULL)
    ## calculate SS and df for betas (other than intercept)
    SS <- vector(mode = "list", length = n.beta - 1L)
    df <- vector(mode = "list", length = n.beta - 1L)
    for (i in seq_len(n.beta - 1L)) {
        name.beta <- names.betas[i + 1L]
        where.beta <- c(where, "prior", name.beta)
        beta <- fetch(filename,
                      where = where.beta,
                      iterations = iterations)
        n.iter <- dim(beta)[length(dim(beta))]
        beta <- matrix(beta@.Data, ncol = n.iter)
        SS[[i]] <- colSums(beta^2)
        df[[i]] <- calculateDF(dims[[i + 1L]])
    }
    ## calculate sd
    sd <- vector(mode = "list", length = n.beta - 1L)
    for (i in seq_along(sd))
        sd[[i]] <- sqrt(SS[[i]] / df[[i]])
    ## calculate quantiles
    .Data = lapply(sd, stats::quantile, probs = probs)
    ## assemble answer
    .Data <- do.call(rbind, .Data)
    dn <- list(term = names.betas[-1L],
               quantile = paste0(100 * probs, "%"))
    dimnames(.Data) <- dn
    metadata <- methods::new("MetaData",
                    nms = names(dn),
                    dimtypes = c("state", "quantile"),
                    DimScales = list(methods::new("Categories", dimvalues = dn[[1L]]),
                        methods::new("Quantiles", dimvalues = probs)))
    df <- unlist(df)
    methods::new("FiniteSD",
        .Data = .Data,
        metadata = metadata,
        df = df)
}

## HAS_TESTS
foldMCMCList <- function(l) {
    if (!coda::is.mcmc.list(l))
        stop(gettextf("'%s' has class \"%s\"",
                      "l", class(l)))
    nrow.old <- nrow(l[[1L]])
    if (nrow.old < 2L)
        stop(gettextf("'%s' has fewer than %d rows",
                      "l", 2L))
    nrow.new <- trunc(nrow.old / 2)
    n <- length(l)
    ans <- vector(mode = "list", length = 2L * n)
    rows.first <- seq_len(nrow.new)
    rows.second <- seq(to = nrow.old, length = nrow.new)
    thin <- coda::thin(l[[1L]])
    for (i.old in seq_len(n)) {
        i.new.first <- (i.old - 1L) * 2L + 1L
        i.new.second <- i.new.first + 1L
        old <- l[[i.old]]
        first <- old[rows.first, , drop = FALSE]
        second <- old[rows.second, , drop = FALSE]
        first <- coda::mcmc(first, thin = thin)
        second <- coda::mcmc(second, thin = thin)
        ans[[i.new.first]] <- first
        ans[[i.new.second]] <- second
    }
    coda::mcmc.list(ans)
}    

#' Obtain potential scale reduction factors (Rhats).
#'
#' Extract potential scale reduction factors (Rhats) from an object of
#' class \code{\linkS4class{SummaryResults}}. See the documentation for
#' \code{\link{fetchSummary}} for details.
#' 
#' @param object An object of class \code{\linkS4class{SummaryResults}}.
#' 
#' @return A named numeric vector.
#'
#' @seealso  \code{\link{fetchSummary}}
#'
#' @examples
#' library(demdata)
#' deaths <- Counts(round(VADeaths2))
#' popn <- Counts(VAPopn)
#' filename <- tempfile()
#' estimateModel(Model(y ~ Poisson(mean ~ sex)),
#'               y = deaths,
#'               exposure = popn,
#'               filename = filename,
#'               nBurnin = 2,
#'               nSim = 20,
#'               nChain = 2,
#'               parallel = FALSE)
#' summary.deaths <- fetchSummary(filename)
#' gelmanDiag(summary.deaths)
#' @export
gelmanDiag <- function(object) {
    if (!methods::is(object, "SummaryResults"))
        stop(gettextf("'%s' has class \"%s\"",
                      "object", class(object)))
    if (methods::.hasSlot(object, "gelmanDiag"))
        object@gelmanDiag
    else
        NULL
}

## TRANSLATED
## HAS_TESTS
## Assume all arguments have been checked
## - 'filename' is a string;
## - 'first', 'last', and 'lengthIter' are all positive integer scalars,
##    such that 'first' <= 'last' <= 'lengthIter';
## - 'iterations' is vector of integers of length >= 1, in order, with no duplicates
## Note that the function defaults to useC is TRUE (rather than FALSE like most
## functions) since it meant to be called from within R.
getDataFromFile <- function(filename, first, last, lengthIter,
                            iterations, useC = TRUE) {
    if (useC) {
        .Call(getDataFromFile_R, filename, first, last, lengthIter, iterations)
    }
    else {
        n.iter <- length(iterations)
        length.data <- last - first + 1L
        length.gap <- lengthIter - length.data
        ans <- double(length = n.iter * length.data)
        con <- file(filename, open = "rb")
        on.exit(close(con))
        ## find out size of results object - stored in first position
        size.results <- readBin(con = con, what = "integer", n = 1L)
        ## skip over size of adjustments
        readBin(con = con, what = "integer", n = 1L)
        ## skip over results object
        for (j in seq_len(size.results))
            readBin(con = con, what = "raw", n = 1L)
        ## go to start of first iteration
        n.skip <- iterations[1L] - 1L
        for (i in seq_len(n.skip))
            for (j in seq_len(lengthIter))
                readBin(con = con, what = "double", n = 1L)
        ## go along iteration to start of data
        for (j in seq_len(first - 1L))
            readBin(con = con, what = "double", n = 1L)
        ## read data
        pos <- 1L
        for (j in seq_len(length.data)) {
            ans[pos] <- readBin(con = con, what = "double", n = 1L)
            pos <- pos + 1L
        }
        ## read remaining lines, if any
        if (n.iter > 1L) {
            for (i in seq.int(from = 2L, to = n.iter)) {
                ## move to start of next block of data
                n.skip <- iterations[i] - iterations[i - 1L] - 1L
                for (j in seq_len(n.skip * lengthIter + length.gap))
                    readBin(con = con, what = "double", n = 1L)
                ## read data
                for (j in seq_len(length.data)) {
                    ans[pos] <- readBin(con = con, what = "double", n = 1L)
                    pos <- pos + 1L
                }
            }
        }
        ans
    }
}

## TRANSLATED
## HAS_TESTS
## Single-iteration version of 'getDataFromFile'.
## Assume all arguments have been checked.
## - 'filename' is a string;
## - 'first', 'last', and 'lengthIter' are all positive integer scalars,
##    such that 'first' <= 'last' <= 'lengthIter';
getOneIterFromFile <- function(filename, first, last, lengthIter, iteration,
                               useC = FALSE) {
    if (useC) {
        .Call(getOneIterFromFile_R, filename, first, last, lengthIter,
              iteration)
    }
    else {
        length.data <- last - first + 1L
        ans <- double(length = length.data)
        con <- file(filename, open = "rb")
        on.exit(close(con))
        ## go to start of first iteration
        n.skip <- iteration - 1L
        ## go to start of first iteration
        for (i in seq_len(n.skip))
            for (j in seq_len(lengthIter))
                readBin(con = con, what = "double", n = 1L)
        ## go along iteration to start of data
        for (j in seq_len(first - 1L))
            readBin(con = con, what = "double", n = 1L)
        ## read data
        for (j in seq_len(length.data))
            ans[j] <- readBin(con = con, what = "double", n = 1L)
        ans
    }
}

## HAS_TESTS
giveListElementsLongNames <- function(object, names = NULL) {
    if (!is.list(object))
        stop(gettextf("'%s' has class \"%s\"",
                      "object", class(object)))
    for (i in seq_along(object)) {
        name <- names(object)[i]
        combined.name <- paste(c(names, name), collapse = ".")
        if (is.list(object[[i]]))
            object[[i]] <- Recall(object[[i]], names = combined.name)
        else
            names(object)[i] <- combined.name
    }
    object
}


isSaturated <- function(object) {
    if (!methods::is(object, "Varying"))
        stop(gettextf("'%s' has class \"%s\"",
                      "object", class(object)))
    priors <- object@priorsBetas
    isSat <- function(x) x@isSaturated@.Data
    is.sat <- sapply(priors, isSat)
    any(is.sat)
}

## HAS_TESTS
isTimeVarying <- function(filenameEst, filenamePred, where) {
    obj.est <- fetch(filenameEst,
                     where = where,
                     iterations = 1L,
                     impute = FALSE)
    obj.pred <- fetch(filenamePred,
                      where = where,
                      iterations = 1L,
                      impute = FALSE)
    metadata.est <- obj.est@metadata
    metadata.pred <- obj.pred@metadata
    !identical(metadata.est, metadata.pred)
}

## HAS_TESTS
jump <- function(object) {
    where.jump <- whereJump(object)
    if (is.null(where.jump))
        NULL
    else {
        ans <- sapply(where.jump,
                      function(w) fetch(object, where = w))
        ans <- matrix(ans, ncol = 1L)
        colnames(ans) <- "value"
        rownames(ans) <- sapply(where.jump, paste, collapse = ".")
        ans
    }
}

## HAS_TESTS
listsAsSingleItems <- function()
    c("control", "final", "seed")

## HAS_TESTS
makeAutocorr <- function(object) {
    if (!methods::is(object, "mcmc.list"))
        stop(gettextf("'%s' has class \"%s\"",
                      class(object)))
    cor <- coda::autocorr(object, lags = 1, relative = FALSE)
    several.var <- ncol(object[[1L]]) > 1L
    if (several.var) {
        cor <- lapply(cor, drop)
        cor <- lapply(cor, diag)
    }
    cor <- unlist(cor)
    mean(abs(cor))
}


## NO_TESTS
makeContentsList <- function(object, where, max) {
    listsAsSingleItems <- listsAsSingleItems()
    n.where <- length(where)
    choices <- methods::slotNames(object)
    depth <- 1L
    if (n.where == 0L) {
        ans <- vector(mode = "list", length = length(choices))
        for (i in seq_along(ans)) {
            name <- choices[i]
            ans[i] <- list(makeContentsListInner(object = methods::slot(object, name),
                                                 nameObject = name,
                                                 where = character(),
                                                 max = max,
                                                 depth = depth,
                                                 listsAsSingleItems = listsAsSingleItems))
        }
        names(ans) <- choices
        ans
    }
    else {
        name <- where[1L]
        i <- charmatch(name, choices, nomatch = -1L)
        if (i > 0L) {
            name <- choices[i]
            makeContentsListInner(object = methods::slot(object, name),
                                  nameObject = name,
                                  where = where[-1L],
                                  max = max,
                                  depth = depth,
                                  listsAsSingleItems = listsAsSingleItems)
        }
        else if (i == 0L)
            raiseMultipleMatchesError(target = name, choices = choices)
        else
            raiseNotFoundError(target = name, choices = choices)
    }
}

## NO_TESTS
makeContentsListInner <- function(object, nameObject, where, max, depth, listsAsSingleItems) {
    n.where <- length(where)
    depth.ok <- is.null(max) || (depth < max)
    normal.list <- !(nameObject %in% listsAsSingleItems)
    if (n.where == 0L) {
        if (is.list(object) && depth.ok && normal.list) {
            ans <- vector(mode = "list", length = length(object))
            names <- names(object)
            depth <- depth + 1L
            for (i in seq_along(ans))
                ans[i] <- list(Recall(object = object[[i]],
                                      nameObject = names[i],
                                      where = character(),
                                      max = max,
                                      depth = depth,
                                      listsAsSingleItems = listsAsSingleItems))
            names(ans) <- names
            ans
        }
        else
            nameObject
    }
    else {
        if (is.list(object) && normal.list) {
            choices <- names(object)
            name <- where[1L]
            i <- charmatch(name, choices, nomatch = -1L)
            if (i > 0L) {
                if (depth.ok) {
                    name <- choices[i]
                    depth <- depth + 1L
                    Recall(object = object[[i]],
                           nameObject = name,
                           where = where[-1L],
                           max = max,
                           depth = depth,
                           listsAsSingleItems = listsAsSingleItems)
                }
                else
                    name
            }
            else if (i == 0L)
                raiseMultipleMatchesError(target = name, choices = choices)
            else
                raiseNotFoundError(target = name, choices = choices)
        }
        else
            raiseOvershotError(nameObject = nameObject, where = where)
    }
}

## HAS_TESTS
makeGelmanDiag <- function(object, filename, nSample) {
    if (!methods::is(object, "Results"))
        stop(gettextf("'%s' has class \"%s\"",
                      "object", class(object)))
    if (identical(dembase::nIteration(object), 0L))
        numeric()
    l <- fetchMCMC(filename = filename,
                   nSample = nSample)
    if (is.null(l))
        numeric()
    n.param <- length(l)
    med <- rep(NA, times = n.param)
    max <- rep(NA, times = n.param)
    n <- integer(length = n.param)
    where.mcmc <- whereMetropStat(object, whereEstimated)
    for (i in seq_len(n.param)) {
        one.iter <- fetch(filename,
                          where = where.mcmc[[i]],
                          iterations = 1L,
                          impute = FALSE)
        skeleton <- fetchSkeleton(object,
                                  where = where.mcmc[[i]])
        indices.struc.zero <- getIndicesStrucZero(skeleton)
        N <- length(one.iter) - length(indices.struc.zero)
        n[i] <- min(N, nSample)
        mcmc.list.i <- l[[i]]
        mcmc.list.i <- foldMCMCList(mcmc.list.i)
        rhat.i <- coda::gelman.diag(mcmc.list.i,
                                    autoburnin = FALSE,
                                    multivariate = FALSE)
        rhat.i <- rhat.i$psrf[, "Point est."]
        med[i] <- median(rhat.i, na.rm = TRUE)
        if (n[i] > 1L)
            max[i] <- max(rhat.i, na.rm = TRUE)
        has.na <- any(is.na(rhat.i))
        if (has.na)
            max[i] <- Inf
    }
    ans <- data.frame(med, max, n,
                      stringsAsFactors = FALSE)
    row.names(ans) <- names(l)
    ans
}

## HAS_TESTS
makeMCMCBetas <- function(priors, names) {
    is.estimated <- sapply(priors, betaIsEstimated)
    names <- names[is.estimated]
    lapply(names, function(name) c("prior", name))
}

## HAS_TESTS
makeMCMCPriorsBetas <- function(priors, names) {
    ans <- lapply(priors, whereEstimated)
    times <- sapply(ans, length)
    names <- rep(names, times = times)
    ans <- unlist(ans)
    if (length(ans) > 0L)
        mapply(c,
               "hyper",
               names,
               ans,
               SIMPLIFY = FALSE,
               USE.NAMES = FALSE)
    else
        NULL        
}

## HAS_TESTS
makeMetropolis <- function(object, filename, nSample) {
    if (!methods::is(object, "Results"))
        stop(gettextf("'%s' has class \"%s\"",
                      "object", class(object)))
    if (identical(dembase::nIteration(object), 0L))
        return(NULL)
    where.jump <- whereMetropStat(object, FUN = whereJump)
    if (identical(where.jump, list(NULL)))
        return(NULL)
    where.acceptance <- whereMetropStat(object, FUN = whereAcceptance)
    where.autocorr <- whereMetropStat(object, FUN = whereAutocorr)
    jump <- sapply(where.jump, function(where) fetch(filename, where))
    acceptance <- lapply(where.acceptance, function(where) fetch(filename, where))
    acceptance <- sapply(acceptance, mean)
    autocorr <- lapply(where.autocorr, function(where) fetchMCMC(filename, where, nSample = nSample))
    autocorr <- sapply(autocorr, makeAutocorr)
    ans <- data.frame(jump, acceptance, autocorr)
    rownames <- sapply(where.autocorr, function(x) paste(x, collapse = "."))
    rownames(ans) <- rownames
    ans
}

## HAS_TESTS
makeParameters <- function(object, filename) {
    n.iter.sample <- 100L
    if (!methods::is(object, "Results"))
        stop(gettextf("'%s' has class \"%s\"",
                      "object", class(object)))
    n.iter <- dembase::nIteration(object)
    if (n.iter == 0L)
        return(NULL)
    else if (n.iter <= n.iter.sample)
        iterations <- seq_len(n.iter)
    else
        iterations <- sample(n.iter, size = n.iter.sample)
    where.est <- whereMetropStat(object, whereEstimated)
    n.where <- length(where.est)
    if (n.where == 0L)
        return(NULL)
    ans <- vector(mode = "list", length = n.where)
    length <- integer(length = n.where)
    for (i in seq_len(n.where)) {
        where <- where.est[[i]]
        posterior.sample <- fetch(filename,
                                  where = where,
                                  iterations = iterations,
                                  impute = FALSE)
        point.estimates <- collapseIterations(posterior.sample,
                                              FUN = median,
                                              na.rm = TRUE)
        point.estimates <- as.numeric(point.estimates)
        n.point.estimates <- length(point.estimates)
        skeleton <- fetchSkeleton(object = object,
                                  where = where)
        indices.struc.zero <- getIndicesStrucZero(skeleton)
        N <- n.point.estimates - length(indices.struc.zero)
        if (N == 1L)
            ans[[i]] <- c(NA,
                          point.estimates,
                          NA,
                          N)
        else
            ans[[i]] <- c(min(point.estimates),
                          median(point.estimates),
                          max(point.estimates),
                          N)
    }
    colnames <- c(c("min", "med", "max", "N"))
    rownames <- sapply(where.est, paste, collapse = ".")
    ans <- do.call(rbind, ans)
    colnames(ans) <- colnames
    rownames(ans) <- rownames
    as.data.frame(ans)
}

#' Extract information on Metropolis-Hastings updates.
#' 
#' Given an object of class \code{\linkS4class{SummaryResults}}, extract
#' the standard deviation of the proposal density, the proportion of
#' proposals accepted, and autocorrelation, for Metropolis-Hastings updates.
#' See the documentation for \code{\link{fetchSummary}} for details.
#'
#' @param object An object of class \code{\linkS4class{SummaryResults}}.
#' 
#' @return If Metropolis-Hastings updates where carried out, a matrix;
#' otherwise \code{NULL}.
#'
#' @seealso \code{\link{fetchSummary}}
#' 
#' @examples
#' library(demdata)
#' deaths <- Counts(round(VADeaths2))
#' popn <- Counts(VAPopn)
#' filename <- tempfile()
#' estimateModel(Model(y ~ Poisson(mean ~ age)),
#'               y = deaths,
#'               exposure = popn,
#'               filename = filename,
#'               nBurnin = 2,
#'               nSim = 5,
#'               nChain = 2)
#' summary.est <- fetchSummary(filename)
#' metropolis(summary.est)
#' @export
metropolis <- function(object) {
    if (!methods::is(object, "SummaryResults"))
        stop(gettextf("'%s' has class \"%s\"",
                      "object", class(object)))
    object@metropolis
}


#' Extract summaries of parameter estimates from a SummaryResults object.
#' 
#' Given an object of class \code{\linkS4class{SummaryResults}}, extract
#' a data.frame containing summaries of parameter estimates.
#' See the documentation for \code{\link{fetchSummary}} for details
#' of the summaries.
#' 
#' @param object An object of class \code{\linkS4class{SummaryResults}}.
#' 
#' @return A data.frame.
#'
#' @seealso \code{\link{fetchSummary}}
#'
#' @examples
#' deaths <- demdata::VADeaths2
#' popn <- demdata::VAPopn
#' deaths <- round(deaths)
#' deaths <- Counts(deaths)
#' popn <- Counts(popn)
#' filename <- tempfile()
#' model <- Model(y ~ Poisson(mean ~ age + sex),
#'                jump = 0.5)
#' estimateModel(model = model,
#'               y = deaths,
#'               exposure = popn,
#'               filename = filename,
#'               nBurnin = 50,
#'               nSim = 50,
#'               nChain = 2,
#'               parallel = FALSE)
#' summary.est <- fetchSummary(filename)
#' parameters(summary.est)
#' @export
parameters <- function(object) {
    if (!methods::is(object, "SummaryResults"))
        stop(gettextf("'%s' has class \"%s\"",
                      "object", class(object)))
    object@parameters
}

## HAS_TESTS
raiseMultipleChoicesError <- function(choices) {
    stop(sprintf(ngettext(length(choices),
                          "'%s' stops before end of hierarchy : remaining choice is %s",
                          "'%s' stops before end of hierarchy : remaining choices are %s"),
                  "where", paste(sQuote(choices), collapse = ", ")))
}

## HAS_TESTS
raiseMultipleMatchesError <- function(target, choices) {
    stop(gettextf("'%s' partially matches two or more of %s",
                  target,
                  paste(sQuote(choices), collapse = ", ")))
}


## HAS_TESTS
raiseNotFoundError <- function(target, choices) {
    n.choices <- length(choices)
    stop(sprintf(ngettext(n.choices,
                          "'%s' not found : only choice is %s",
                          "'%s' not found : choices are %s"),
                 target, paste(sQuote(choices), collapse = ", ")))
}

## HAS_TESTS
raiseOvershotError <- function(nameObject, where) {
    stop(sprintf(ngettext(length(where),
                          "hierarchy only extends to '%s' : '%s' has additional term %s",
                          "hierarchy only extends to '%s' : '%s' has additional terms %s"),
                 nameObject,
                 "where",
                 paste(dQuote(where), collapse = ", ")))
}

## HAS_TESTS
seasonalNormalizingFactor <- function(season, nSeason, iAlong, nIteration, metadata) {
    n <- length(season)
    n.other <- n / (nSeason * nIteration)
    dim.season <- c(nSeason, n.other, nIteration)
    season <- array(season, dim = dim.season)
    A <- colMeans(season)
    dim.A <- c(dim(metadata), nIteration)
    n.along <- dim.A[iAlong]
    dim.A <- replace(dim.A,
                     list = iAlong,
                     values = n.along + 1L)
    A <- array(A, dim = dim.A)
    A[slice.index(A, MARGIN = iAlong) > 1L]
}


## NO_TESTS
showContentsList <- function(l, nIndent = 2L) {
    kIndent <- 2L
    for (i in seq_along(l)) {
        item <- l[[i]]
        spaces <- paste(rep(" ", times = nIndent), collapse = "")
        if (is.list(item)) {
            name <- names(l)[i]
            cat(spaces, name, "\n", sep = "")
            Recall(item, nIndent = nIndent + kIndent)
        }
        else
            cat(spaces, item, "\n", sep = "")
    }
}

## HAS_TESTS
centerAlong <- function(object, iAlong) {
    if (!methods::is(object, "Values"))
        stop(gettextf("'%s' has class \"%s\"",
                      "object", class(object)))
    if (!is.integer(iAlong))
        stop(gettextf("'%s' does not have type \"%s\"",
                      "iAlong", "integer"))
    if (!identical(length(iAlong), 1L))
        stop(gettextf("'%s' does not have length %d",
                      "iAlong", 1L))
    if (is.na(iAlong))
        stop(gettextf("'%s' is missing",
                      "iAlong"))
    .Data <- object@.Data
    metadata <- object@metadata
    dim <- dim(object)
    n <- length(dim)
    s <- seq_len(n)
    if (!(iAlong %in% s))
        stop(gettextf("'%s' does not specify a dimension of '%s'",
                      "iAlong", "object"))
    if (n > 1L) {
        margin <- s[-iAlong]
        means <- apply(.Data, MARGIN = margin, FUN = mean)
        .Data <- sweep(.Data, MARGIN = margin, STATS = means)
    }
    else
        .Data <- .Data - mean(.Data)
    methods::new("Values", .Data = .Data, metadata = metadata)
}

## HAS_TESTS
## assumes that 'est' and 'pred' are time-varying
combineEstPredHelper <- function(est, pred) {
    .Data.est <- est@.Data
    .Data.pred <- pred@.Data
    metadata.est <- est@metadata
    metadata.pred <- pred@metadata
    dim.est <- dim(metadata.est)
    dim.pred <- dim(metadata.pred)
    names.est <- names(metadata.est)
    names.pred <- names(metadata.pred)
    dimtypes.est <- dembase::dimtypes(metadata.est, use.names = FALSE)
    dimtypes.pred <- dembase::dimtypes(metadata.pred, use.names = FALSE)
    DimScales.est <- dembase::DimScales(metadata.est, use.names = FALSE)
    DimScales.pred <- dembase::DimScales(metadata.pred, use.names = FALSE)
    if (!identical(names.est, names.pred))
        stop(gettextf("results from '%s' and '%s' have different '%s'",
                      "est", "pred", "names"))
    if (!identical(dimtypes.est, dimtypes.pred))
        stop(gettextf("results from '%s' and '%s' have different '%s'",
                      "est", "pred", "dimtypes"))
    DimScales.different <- !mapply(identical,
                                   x = DimScales.est,
                                   y = DimScales.pred)
    if (sum(DimScales.different) != 1L)
        stop(gettextf("results from '%s' and '%s' have incompatible dimensions or '%s'",
                      "est", "pred", "dimscales"))
    i.along <- which(DimScales.different)
    DimScale.est <- DimScales.est[[i.along]]
    DimScale.pred <- DimScales.pred[[i.along]]
    DimScale <- concatDimScaleFirstSecond(first = DimScale.est,
                                          second = DimScale.pred,
                                          name = names.est[i.along])
    DimScales <- replace(DimScales.est,
                         list = i.along,
                         values = list(DimScale))
    metadata <- methods::new("MetaData",
                    nms = names.est,
                    dimtypes = dimtypes.est,
                    DimScales = DimScales)
    s <- seq_along(dim.est)
    perm <- c(s[-i.along], i.along)
    .Data.est <- aperm(.Data.est, perm = perm)
    .Data.pred <- aperm(.Data.pred, perm = perm)
    .Data <- array(c(.Data.est, .Data.pred),
                   dim = dim(metadata)[perm],
                   dimnames = dimnames(metadata)[perm])
    .Data <- aperm(.Data, perm = match(s, perm))
    list(.Data = .Data, metadata = metadata)
}

## HAS_TESTS
flattenList <- function(object) {
  if (!is.list(object))
    stop(gettextf("'%s' has class \"%s\"",
                  "object", class(object)))
  ans <- list()
  for (i in seq_along(object)) {
    if (is.list(object[[i]]))
      ans <- c(ans, Recall(object[[i]]))
    else
      ans <- c(ans, object[i])
  }
  ans
}

## HAS_TESTS
trimNULLsFromList <- function(object) {
  if (!is.list(object))
    stop(gettextf("'%s' has class \"%s\"",
                  "object", class(object)))
  onlyNULL <- function(x) all(is.null(unlist(x)))
  i <- 1L
  while (i <= length(object)) {
    if (onlyNULL(object[[i]]))
      object[[i]] <- NULL
    else {
      if (is.list(object[[i]]))
        object[[i]] <- Recall(object[[i]])
      i <- i + 1L
    }
  }
  object
}


## DEMOGRAPHIC ACCOUNTS ###################################################

## preparing inputs

## HAS_TESTS
alignDataModelsToDatasets <- function(dataModels, datasets, namesDatasets) {
    names.obs.mod <- sapply(dataModels, methods::slot, "nameY")
    obs.not.in.data <- setdiff(names.obs.mod, namesDatasets)
    data.not.in.obs <- setdiff(namesDatasets, names.obs.mod)
    ## no data models without datasets
    if (length(obs.not.in.data) > 0L)
        stop(gettextf("'%s' contains a model for '%s', but there is no dataset called '%s' in '%s'",
                      "dataModels", obs.not.in.data[1L], obs.not.in.data[1L], "datasets"))
    ## no datasets without data models
    if (length(data.not.in.obs) > 0L)
        stop(gettextf("'%s' contains a dataset called '%s', but '%s' does not contain a model for '%s'",
                      "datasets", data.not.in.obs[1L], "dataModels", data.not.in.obs[1L]))
    ans <- vector(mode = "list", length = length(datasets))
    for (i in seq_along(datasets)) {
        name.dataset <- namesDatasets[i]
        i.obs <- match(name.dataset, names.obs.mod)
        ans[[i]] <- dataModels[[i.obs]]
    }
    ans
}

## HAS_TESTS
alignSystemModelsToAccount <- function(systemModels, account) {
    names.sys.mod <- sapply(systemModels, methods::slot, "nameY")
    names.components <- account@namesComponents
    names.series <- c("population", names.components)
    sys.not.in.series <- setdiff(names.sys.mod, names.series)
    series.not.in.sys <- setdiff(names.series, names.sys.mod)
    ## no system models without series
    if (length(sys.not.in.series) > 0L)
        stop(gettextf("'%s' contains a system model for '%s', but there is no series called '%s' in '%s'",
                      "systemModels", sys.not.in.series[1L], sys.not.in.series[1L], "account"))
    ## no series without system models
    if (length(series.not.in.sys) > 0L)
        stop(gettextf("'%s' does not contain a model for series '%s' in '%s'",
                      "systemModels", series.not.in.sys[1L], "account"))
    ans <- vector(mode = "list", length = length(names.series))
    for (i in seq_along(names.series)) {
        name.series <- names.series[i]
        i.sys <- match(name.series, names.sys.mod)
        ans[[i]] <- systemModels[[i.sys]]
    }
    ## system model for 'population' does not have exposure term
    popn.uses.exposure <- ans[[1L]]@useExpose@.Data
    if (popn.uses.exposure)
        stop(gettextf("system model for '%s' uses exposure",
                      "population"))
    ans
}

## HAS_TESTS
checkAndTidySystemWeights <- function(weights, systemModels) {
    if (identical(weights, list())) {
        ans <- vector(mode = "list", length = length(systemModels))
        return(ans)
    }
    names.weights <- names(weights)
    checkListNames(names = names.weights,
                   listName = "weights")
    names.sys.mod <- sapply(systemModels, methods::slot, "nameY")
    weights.not.in.sys <- setdiff(names.weights, names.sys.mod)
    ## no weights without system models (system models without weights are OK)
    if (length(weights.not.in.sys) > 0L)
        stop(gettextf("'%s' contains weights for '%s', but '%s' does not contain a model for '%s'",
                      "weights", weights.not.in.sys[1L], "systemModels", weights.not.in.sys[1L]))
    ans <- vector(mode = "list", length = length(systemModels))
    for (i in seq_along(ans)) {
        spec <- systemModels[[i]]
        name.y <- spec@nameY
        i.weights <- match(name.y, names.weights, nomatch = 0L)
        has.weights <- i.weights > 0L
        if (has.weights) {
            uses.weights <- modelUsesWeights(spec)
            if (uses.weights)
                ans[[i]] <- weights[[i.weights]]
            else ## weights supplied only if system model needs them
                stop(gettextf("'%s' contains weights for '%s', but system model for '%s' does not use weights",
                              "weights", name.y, name.y))
        }
        else
            ans[[i]] <- NULL
    }
    ans
}

## HAS_TESTS
checkSystemModels <- function(systemModels) {
    ## 'systemModels' is a list
    if (!is.list(systemModels))
        stop(gettextf("'%s' has class \"%s\"",
                      "systemModels", class(systemModels)))
    for (i in seq_along(systemModels)) {
        spec <- systemModels[[i]]
        ## element has class "SpecModel"
        if (!methods::is(spec, "SpecModel"))
            stop(gettextf("element %d of '%s' has class \"%s\"",
                          i, "systemModels", class(spec)))
        ## specification is valid
        return.value <- tryCatch(methods::validObject(spec),
                                 error = function(e) e)
        if (methods::is(return.value, "error"))
            stop(gettextf("element %d of '%s' is invalid : %s",
                          i, "systemModels", return.value$message))
        ## no 'series' argument supplied
        if (!identical(spec@series@.Data, "y"))
            stop(gettextf("element %d of '%s' has value for '%s' [\"%s\"] : in system models, series should instead be specified via response variable",
                          i, "systemModels", "series", spec@series@.Data))
    }
    NULL
}

## HAS_TESTS
## Note that every data model has to relate to one series,
## but every series does not have to have a data model.
makeSeriesIndices <- function(dataModels, account) {
    names.obs.mod <- sapply(dataModels, methods::slot, "series")
    names.components <- account@namesComponents
    names.series <- c("population", names.components)
    obs.not.in.series <- setdiff(names.obs.mod, names.series)
    if (length(obs.not.in.series) > 0L)
        stop(gettextf("'%s' contains a model for '%s', but '%s' does not have a series called '%s'",
                      "dataModels", obs.not.in.series[1L], "account", obs.not.in.series[1L]))
    ans <- integer(length = length(dataModels))
    for (i in seq_along(dataModels)) {
        name.obs <- names.obs.mod[i]
        i.series <- match(name.obs, names.series)
        ans[i] <- i.series - 1L # 'population' has index 0; first component has index 1
    }
    ans
}

## HAS_TESTS
makeTransformsAccountToDatasets <- function(account, datasets, concordances,
                                            namesDatasets, seriesIndices) {
    population <- account@population
    components <- account@components
    series <- c(list(population), components)
    names.components <- account@namesComponents
    names.series <- c("population", names.components)
    names.concordances <- names(concordances)
    ans <- vector(mode = "list", length = length(datasets))
    for (i in seq_along(ans)) {
        dataset <- datasets[[i]]
        index <- seriesIndices[i] + 1L
        name.dataset <- namesDatasets[i]
        i.concordances <- match(name.dataset, names.concordances, nomatch = 0L)
        has.concordances <- i.concordances > 0L
        if (has.concordances)
            concordances.dataset <- concordances[[i.concordances]]
        else
            concordances.dataset <- list()
        transform <- tryCatch(dembase::makeTransform(x = series[[index]],
                                                     y = dataset,
                                                     concordances = concordances.dataset,
                                                     subset = TRUE,
                                                     check = TRUE),
                              error = function(e) e)
        if (methods::is(transform, "error"))
            stop(gettextf("unable to collapse series '%s' to make it compatible with dataset '%s' : %s",
                          names.series[index], namesDatasets[i], transform$message))
        transform <- dembase::makeCollapseTransformExtra(transform)
        ans[[i]] <- transform
    }
    ans
}



## functions for getting cell positions in other components,
## given a mapping, in file 'mapping-functions.R'

## TRANSLATED
## HAS_TESTS
chooseICellComp <- function(description, useC = FALSE) {
    stopifnot(methods::is(description, "DescriptionComp"))
    if (useC) {
        .Call(chooseICellComp_R, description)
    }
    else {
        length <- description@length
        i <- as.integer(stats::runif(n = 1L) * length) # C-style
        if (i == length) # just in case
            i <- length - 1L
        i <- i + 1L # R-style
        i
    }
}

## TRANSLATED
## HAS_TESTS
chooseICellOutInPool <- function(description, useC = FALSE) { 
    stopifnot(methods::is(description, "DescriptionPool"))
    if (useC) {
        .Call(chooseICellOutInPool_R, description) 
    }
    else {
        step.direction <- description@stepDirection
        n.between.vec <- description@nBetweenVec
        step.between.vec <- description@stepBetweenVec
        n.within.vec <- description@nWithinVec
        step.within.vec <- description@stepWithinVec
        n.dim.between <- length(n.between.vec)
        n.dim.within <- length(n.within.vec)
        i.out <- 1L  # assume 'outs' come before 'ins' 
        i.in <- 1L + step.direction
        for (d in seq_len(n.dim.between)) {
            n.between <- n.between.vec[d]  # guaranteed > 1
            step.between <- step.between.vec[d]
            i.between.out <- as.integer(stats::runif(n = 1L) * n.between) # C-style  
            if (i.between.out == n.between) # just in case
                i.between.out <- n.between - 1L
            i.between.in <- as.integer(stats::runif(n = 1L) * (n.between - 1L)) # C-style
            if (i.between.in == n.between - 1L) # just in case
                i.between.in <- n.between - 2L
            if (i.between.in >= i.between.out)
                i.between.in <- i.between.in + 1L
            i.out <- i.out + i.between.out * step.between
            i.in <- i.in + i.between.in * step.between
        }
        for (d in seq_len(n.dim.within)) {
            n.within <- n.within.vec[d]
            step.within <- step.within.vec[d]
            i.within <- as.integer(stats::runif(n = 1L) * n.within) # C-style
            if (i.within == n.within) # just in case
                i.within <- n.within - 1L
            i.out <- i.out + i.within * step.within
            i.in <- i.in + i.within * step.within
        }
        c(i.out, i.in)
    }
}

## TRANSLATED
## HAS_TESTS
chooseICellPopn <- function(description, useC = FALSE) {
    stopifnot(methods::is(description, "DescriptionPopn"))
    if (useC) {
        .Call(chooseICellPopn_R, description)
    }
    else {
        length <- description@length
        n.time <- description@nTime
        step.time <- description@stepTime
        n.initial <- length %/% n.time
        i <- as.integer(stats::runif(n = 1L) * n.initial) # C-style
        if (i == n.initial) # just in case
            i <- n.initial - 1L
        i <- i %% step.time + (i %/% step.time) * (n.time * step.time) # C-style
        i <- i + 1L  # R-style
        i
    }
}

## TRANSLATED
## HAS_TESTS
## This function is almost identical to 'chooseICellOutInPool', but
## it seems clearest to keep them separate, even at the cost
## of some cut-and-paste.
chooseICellSubAddNet <- function(description, useC = FALSE) { 
    stopifnot(methods::is(description, "DescriptionNet"))
    if (useC) {
        .Call(chooseICellSubAddNet_R, description) 
    }
    else { # different from 'chooseICellOutInPool', because do not extract 'step.direction'
        n.between.vec <- description@nBetweenVec
        step.between.vec <- description@stepBetweenVec
        n.within.vec <- description@nWithinVec
        step.within.vec <- description@stepWithinVec
        n.dim.between <- length(n.between.vec)
        n.dim.within <- length(n.within.vec)
        i.sub <- 1L
        i.add <- 1L # different from 'chooseICellOutInPool', because do not add 'step.direction'
        for (d in seq_len(n.dim.between)) {
            n.between <- n.between.vec[d]  # guaranteed > 1
            step.between <- step.between.vec[d]
            i.between.sub <- as.integer(stats::runif(n = 1L) * n.between) # C-style  
            if (i.between.sub == n.between) # just in case
                i.between.sub <- n.between - 1L
            i.between.add <- as.integer(stats::runif(n = 1L) * (n.between - 1L)) # C-style
            if (i.between.add == n.between - 1L) # just in case
                i.between.add <- n.between - 2L
            if (i.between.add >= i.between.sub)
                i.between.add <- i.between.add + 1L
            i.sub <- i.sub + i.between.sub * step.between
            i.add <- i.add + i.between.add * step.between
        }
        for (d in seq_len(n.dim.within)) {
            n.within <- n.within.vec[d]
            step.within <- step.within.vec[d]
            i.within <- as.integer(stats::runif(n = 1L) * n.within) # C-style
            if (i.within == n.within) # just in case
                i.within <- n.within - 1L
            i.sub <- i.sub + i.within * step.within
            i.add <- i.add + i.within * step.within
        }
        c(i.sub, i.add)
    }
}



## TRANSLATED
## HAS_TESTS
isLowerTriangle <- function(i, description, useC = FALSE) {
    stopifnot(is(description, "DescriptionComp"))
    stopifnot(description@hasAge)
    if (useC) {
        .Call(isLowerTriangle_R, i, description)
    }
    else {
        step.triangle <- description@stepTriangle
        i.triangle <- ((i - 1L) %/% step.triangle) %% 2L ## C-style
        i.triangle == 0L
    }
}

## TRANSLATED
## HAS_TESTS
## Assumes that population and accession have identical dimensions,
## except that time dimension for accession is one shorter than
## time dimension for population
getIAccNextFromPopn <- function(i, description, useC = FALSE) {
    ## 'i'
    stopifnot(is.integer(i))
    stopifnot(identical(length(i), 1L))
    stopifnot(!is.na(i))
    stopifnot(i >= 1L)
    ## 'description'
    stopifnot(methods::is(description, "DescriptionPopn"))
    stopifnot(description@hasAge)
    ## 'i' and 'description'
    stopifnot(i <= description@length)
    if (useC) {
        .Call(getIAccNextFromPopn_R, i, description)
    }
    else {            
        n.time.popn <- description@nTime
        n.age.popn <- description@nAge
        step.time.popn <- description@stepTime
        step.age.popn <- description@stepAge
        n.time.acc <- n.time.popn - 1L
        n.age.acc <- n.age.popn - 1L
        i.time.popn <- (((i - 1L) %/% step.time.popn) %% n.time.popn) + 1L ## R-style
        if (i.time.popn < n.time.popn) {
            i.age.popn <- (((i - 1L) %/% step.age.popn) %% n.age.popn) + 1L ## R-style
            if (i.age.popn < n.age.popn) {
                ## adjust for loss of one time period
                i.acc <- (((i - 1L) %/% (step.time.popn * n.time.popn)) * (step.time.popn * n.time.acc)
                    + ((i - 1L) %% (step.time.popn * n.time.popn))) + 1L
                ## adjust for loss of one age group
                if (step.time.popn > step.age.popn)
                    step.age.acc <- step.age.popn
                else
                    step.age.acc <- (step.age.popn %/% n.time.popn) * n.time.acc
                i.acc <- (((i.acc - 1L) %/% (step.age.acc * n.age.popn)) * (step.age.acc * n.age.acc)
                    + ((i.acc - 1L) %% (step.age.acc * n.age.popn))) + 1L
                i.acc
            }
            else
                0L
        }
        else
            0L
    }
}

## TRANSLATED
## HAS_TESTS
## Assumes that the Lexis triangle dimension is the
## last dimension in 'exposure'.
## We only ever update population values for the beginning
## of the first period.
getIExpFirstFromPopn <- function(i, description, useC = FALSE) {
    ## 'i'
    stopifnot(is.integer(i))
    stopifnot(identical(length(i), 1L))
    stopifnot(!is.na(i))
    stopifnot(i >= 1L)
    ## 'description'
    stopifnot(methods::is(description, "DescriptionPopn"))
    ## 'i' and 'description'
    stopifnot(i <= description@length)
    stopifnot((((i - 1L) %/% description@stepTime) %% description@nTime) == 0L) # first time point
    if (useC) {
        .Call(getIExpFirstFromPopn_R, i, description)
    }
    else {            
        n.time.popn <- description@nTime
        step.time <- description@stepTime
        length.popn <- description@length
        has.age <- description@hasAge
        i.nontime <- (i - 1L) %/% (n.time.popn * step.time)
        remainder <- (i - 1L) %% (n.time.popn * step.time) + 1L
        n.time.exp <- n.time.popn - 1L
        index.exp <- i.nontime * n.time.exp * step.time + remainder
        if (has.age) {
            length.lower.tri <- (length.popn %/% n.time.popn) * n.time.exp
            length.lower.tri + index.exp
        }
        else
            index.exp
    }
}

## TRANSLATED
## HAS_TESTS
getIPopnNextFromPopn <- function(i, description, useC = FALSE) {
    ## 'i'
    stopifnot(is.integer(i))
    stopifnot(identical(length(i), 1L))
    stopifnot(!is.na(i))
    stopifnot(i >= 1L)
    ## 'description'
    stopifnot(methods::is(description, "DescriptionPopn"))
    ## 'i' and 'description'
    stopifnot(i <= description@length)
    if (useC) {
        .Call(getIPopnNextFromPopn_R, i, description)
    }
    else {
        step.time <- description@stepTime
        n.time <- description@nTime
        i.time <- (((i - 1L) %/% step.time) %% n.time) + 1L # R-style
        if (i.time < n.time) {
            ans <- i + step.time
            has.age <- description@hasAge
            if (has.age) {
                step.age <- description@stepAge
                n.age <- description@nAge
                i.age <- (((i - 1L) %/% step.age) %% n.age) + 1L # R-style
                if (i.age < n.age)
                    ans <- ans + step.age
            }
            ans
        }
        else
            0L
    }
}

## TRANSLATED
## HAS_TESTS
getMinValCohortAccession <- function(i, series, iterator, useC = FALSE) {  
    ## 'i'
    stopifnot(is.integer(i))
    stopifnot(identical(length(i), 1L))
    stopifnot(!is.na(i))
    stopifnot(i >= 1L)
    ## 'series'
    stopifnot(is.integer(series))
    stopifnot(!any(is.na(series)))
    ## 'iterator'
    stopifnot(methods::is(iterator, "CohortIteratorAccession"))
    ## 'i' and 'series'
    stopifnot(i <= length(series))
    if (useC) {
        .Call(getMinValCohortAccession_R, i, series, iterator)
    }
    else {              
        ans <- series[i]
        iterator <- resetCA(iterator, i = i)  
        while (!iterator@finished) {
            iterator <- advanceCA(iterator)
            i <- iterator@i
            ans <- min(series[i], ans)
        }
        ans
    }
}


## TRANSLATED
## HAS_TESTS
getMinValCohortPopulation <- function(i, series, iterator, useC = FALSE) {  
    ## 'i'
    stopifnot(is.integer(i))
    stopifnot(identical(length(i), 1L))
    stopifnot(!is.na(i))
    stopifnot(i >= 1L)
    ## 'series'
    stopifnot(is.integer(series))
    stopifnot(!any(is.na(series)))
    ## 'iterator'
    stopifnot(methods::is(iterator, "CohortIteratorPopulation"))
    ## 'i' and 'series'
    stopifnot(i <= length(series))
    if (useC) {
        .Call(getMinValCohortPopulation_R, i, series, iterator)
    }
    else {              
        ans <- series[i]
        iterator <- resetCP(iterator, i = i)  
        while (!iterator@finished) {
            iterator <- advanceCP(iterator)
            i <- iterator@i
            ans <- min(series[i], ans)
        }
        ans
    }
}

## HAS_TESTS
makeTransformExpToBirths <- function(exposure, births,
                                     dominant = c("Female", "Male")) {
    dominant <- match.arg(dominant)
    names.exp <- names(exposure)
    dimtypes.exp <- dimtypes(exposure, use.names = FALSE)
    dimtypes.births <- dimtypes(births, use.names = FALSE)
    DimScales.exp <- DimScales(exposure, use.names = FALSE)
    DimScales.births <- DimScales(births, use.names = FALSE)
    i.sex.exp <- match("sex", dimtypes.exp, nomatch = 0L)
    i.age.exp <- match("age", dimtypes.exp, nomatch = 0L)
    has.sex.exp <- i.sex.exp > 0L
    has.age.exp <- i.age.exp > 0L
    dimBefore <- dim(exposure)
    dimAfter <- dim(exposure)
    indices <- lapply(dimBefore, seq_len)
    if (has.sex.exp) {
        DimScale <- DimScales.exp[[i.sex.exp]]
        if (dominant == "Female") 
            i.dominant <- dembase::iFemale(DimScale)
        else
            i.dominant <- dembase::iMale(DimScale)
        indices[[i.sex.exp]] <- ifelse(indices[[i.sex.exp]] == i.dominant, 1L, 0L)
        dims <- match(names.exp, names.exp[-i.sex.exp], nomatch = 0L)
    }
    else
        dims <- seq_along(names.exp)
    if (has.age.exp) {
        i.age.births <- match("age", dimtypes.births)
        DimScale.exp <- DimScales.exp[[i.age.exp]]
        DimScale.births <- DimScales.births[[i.age.births]]
        labels.exp <- labels(DimScale.exp)
        labels.births <- labels(DimScale.births)
        indices[[i.age.exp]] <- match(labels.exp, labels.births, nomatch = 0L)
        dimAfter[i.age.exp] <- length(labels.births)
    }
    if (has.sex.exp)
        dimAfter <- dimAfter[-i.sex.exp]
    methods::new("CollapseTransform",
                 dims = dims,
                 indices = indices,
                 dimBefore = dimBefore,
                 dimAfter = dimAfter)
}


## HAS_TESTS
makeIteratorCAP <- function(dim, iTime, iAge, accession) {
    n.time <- dim[iTime]
    step.time <- 1L
    for (d in seq_len(iTime - 1L))
        step.time <- step.time * dim[d]
    has.age <- iAge > 0L
    if (has.age) {
        n.age <- dim[iAge]
        step.age <- 1L
        for (d in seq_len(iAge - 1L))
            step.age <- step.age * dim[d]
        i.age <- 1L
    }
    else {
        n.age <- as.integer(NA)
        step.age <- as.integer(NA)
        i.age <- as.integer(NA)
    }
    finished <- n.time == 1L
    class <- if (accession) "CohortIteratorAccession" else "CohortIteratorPopulation"
    methods::new(class,
                 i = 1L,
                 nTime = n.time,
                 stepTime = step.time,
                 iTime = 1L,
                 hasAge = has.age,
                 nAge = n.age,
                 stepAge = step.age,
                 iAge = i.age,
                 finished = finished)
}

## HAS_TESTS
makeIteratorCC <- function(dim, iTime, iAge, iTriangle, lastAgeGroupOpen) {
    n.time <- dim[iTime]
    step.time <- 1L
    for (d in seq_len(iTime - 1L))
        step.time <- step.time * dim[d]
    has.age <- iAge > 0L
    if (has.age) {
        n.age <- dim[iAge]
        step.age <- 1L
        for (d in seq_len(iAge - 1L))
            step.age <- step.age * dim[d]
        i.age <- 1L
        step.triangle <- 1L
        for (d in seq_len(iTriangle - 1L))
            step.triangle <- step.triangle * dim[d]
        i.triangle <- 1L
    }
    else {
        n.age <- as.integer(NA)
        step.age <- as.integer(NA)
        i.age <- as.integer(NA)
        step.triangle <- as.integer(NA)
        i.triangle <- as.integer(NA)
    }
    finished <- n.time == 1L
    methods::new("CohortIteratorComponent",
        i = 1L,
        nTime = n.time,
        stepTime = step.time,
        iTime = 1L,
        hasAge = has.age,
        nAge = n.age,
        stepAge = step.age,
        iAge = i.age,
        stepTriangle = step.triangle,
        iTriangle = i.triangle,
        finished = finished,
        lastAgeGroupOpen = lastAgeGroupOpen)
}

## HAS_TESTS
makeIteratorCODPCP <- function(dim, iTime, iAge, iTriangle, iMultiple, lastAgeGroupOpen) {
    n.time <- dim[iTime]
    step.time <- 1L
    for (d in seq_len(iTime - 1L))
        step.time <- step.time * dim[d]
    has.age <- iAge > 0L
    if (has.age) {
        n.age <- dim[iAge]
        step.age <- 1L
        for (d in seq_len(iAge - 1L))
            step.age <- step.age * dim[d]
        i.age <- 1L
        step.triangle <- 1L
        for (d in seq_len(iTriangle - 1L))
            step.triangle <- step.triangle * dim[d]
        i.triangle <- 1L
    }
    else {
        n.age <- as.integer(NA)
        step.age <- as.integer(NA)
        i.age <- as.integer(NA)
        step.triangle <- as.integer(NA)
        i.triangle <- as.integer(NA)
    }
    increment <- vector(mode = "list", length = length(iMultiple))
    for (j in seq_along(iMultiple)) {
        i.m <- iMultiple[j]
        step <- 1L
        for (d in seq_len(i.m - 1L))
            step <- step * dim[d]
        increment[[j]] <- seq.int(from = 0L, by = step, length.out = dim[i.m])
    }
    increment <- expand.grid(increment)
    increment <- Reduce(f = "+", x = increment)
    i <- 1L
    length.vec <- length(increment)
    i.vec <- i + increment
    finished <- n.time == 1L
    methods::new("CohortIteratorOrigDestParChPool",
                 i = i,
                 nTime = n.time,
                 stepTime = step.time,
                 iTime = 1L,
                 hasAge = has.age,
                 nAge = n.age,
                 stepAge = step.age,
                 iAge = i.age,
                 stepTriangle = step.triangle,
                 iTriangle = i.triangle,
                 iVec = i.vec,
                 lengthVec = length.vec,
                 increment = increment,
                 finished = finished,
                 lastAgeGroupOpen = lastAgeGroupOpen)
}


makeOutputAccount <- function(account, systemModels, pos) {
    population.obj <- account@population
    components.obj <- account@components
    names.components <- account@namesComponents
    first <- pos
    pos <- first + length(population.obj)
    s <- seq_along(dim(population.obj))
    sys.mod <- systemModels[[1L]]
    struc.zero.array <- sys.mod@strucZeroArray
    population.out <- Skeleton(population.obj,
                               first = first,
                               strucZeroArray = struc.zero.array,
                               margin = 2)
    components.out <- vector(mode = "list",
                             length = length(components.obj))
    for (i in seq_along(components.obj)) {
        component <- components.obj[[i]]
        sys.mod <- systemModels[[i + 1L]]
        first <- pos
        pos <- first + length(component)
        if (methods::is(sys.mod, "StrucZeroArrayMixin")) {
            s <- seq_along(dim(component))
            struc.zero.array = sys.mod@strucZeroArray
            components.out[[i]] <- Skeleton(component,
                                            first = first,
                                            strucZeroArray = struc.zero.array,
                                            margin = s)
        }
        else
            components.out[[i]] <- Skeleton(component,
                                            first = first)
    }
    names(components.out) <- names.components
    c(list(population = population.out),
      components.out)
}



## CMP ###############################################################


## TRANSLATED
## HAS_TESTS
# The CMP desity function is f(y|theta,nu)= (theta^y/y!)^nu*1/Z(theta,nu), 
# Z(theta,nu) is the intractable normalising constant
# So f is split in two parts: Z and q(y|theta,nu)=(theta^y/y!)^nu
logDensCMPUnnormalised1 <- function(x, gamma, nu, useC = FALSE) {
    ## 'x'
    stopifnot(is.integer(x))
    stopifnot(identical(length(x), 1L))
    stopifnot(!is.na(x))
    stopifnot(x >= 0L)
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
        .Call(logDensCMPUnnormalised1_R, x, gamma, nu)
    }
    else {
        nu * (x * log(gamma) - lgamma(x + 1))
    }
}

## TRANSLATED
## HAS_TESTS
rcmpUnder <- function(mu, nu, maxAttempt, useC = FALSE) {
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
    ## 'maxAttempt'
    stopifnot(is.integer(maxAttempt))
    stopifnot(identical(length(maxAttempt), 1L))
    stopifnot(!is.na(maxAttempt))
    stopifnot(maxAttempt >= 1L)
    if (useC) {
        .Call(rcmpUnder_R, mu, nu, maxAttempt)
    }
    else {
        fl <- floor(mu)
        logm <- lgamma(fl + 1)
        for (i in seq_len(maxAttempt)) {
            ynew <- rpois(n = 1L, lambda = mu)
            logy <- lgamma(ynew + 1)
            log_a <- (nu - 1) * (log(mu) * (ynew - fl) - logy + logm)
            u <- log(runif(n = 1L))
            if(u < log_a) 
                return(ynew)
        }
        return(-Inf)
    }
}

## TRANSLATED
## HAS_TESTS
rcmpOver <- function(mu, nu, maxAttempt, useC = FALSE) {
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
    ## 'maxAttempt'
    stopifnot(is.integer(maxAttempt))
    stopifnot(identical(length(maxAttempt), 1L))
    stopifnot(!is.na(maxAttempt))
    stopifnot(maxAttempt >= 1L)
    if (useC) {
        .Call(rcmpOver_R, mu, nu, maxAttempt)
    }
    else {
        p <- 2 * nu / (2 * mu * nu + 1 + nu)
        fl <- floor(mu / ((1 - p)^(1 / nu)))
        logfl <- lgamma(fl + 1)
        for (i in seq_len(maxAttempt)) {
            ynew <- rgeom(n = 1L, prob = p)
            logy <- lgamma(ynew + 1)
            log_a <- (ynew - fl) * (nu * log(mu) - log1p(-p)) + nu * (logfl - logy)
            u <- log(runif(n = 1L))
            if(u < log_a) 
                return(ynew)
        }
        return(-Inf)
    }
}

## TRANSLATED
## HAS_TESTS
## Random generation function for CMP data
rcmp1 <- function(mu, nu, maxAttempt, useC = FALSE){
    ## 'mu'
    stopifnot(is.double(mu))
    stopifnot(identical(length(mu), 1L))
    stopifnot(!is.na(mu))
    stopifnot(mu > 0L)
    ## 'nu'
    stopifnot(is.double(nu))
    stopifnot(identical(length(nu), 1L))
    stopifnot(!is.na(nu))
    stopifnot(nu > 0L)
    ## 'maxAttempt'
    stopifnot(is.integer(maxAttempt))
    stopifnot(identical(length(maxAttempt), 1L))
    stopifnot(!is.na(maxAttempt))
    stopifnot(maxAttempt >= 1L)
    if (useC) {
        .Call(rcmp1_R, mu, nu, maxAttempt)
    }
    else {
        if( nu < 1)
            rcmpOver(mu = mu,
                     nu = nu,
                     maxAttempt = maxAttempt)
        else
            rcmpUnder(mu = mu,
                      nu = nu,
                      maxAttempt = maxAttempt)
    }
}
