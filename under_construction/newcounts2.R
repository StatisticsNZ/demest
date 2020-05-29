## This is modified from 'updateCountsPoissonNotUseExp',
## including keeping the idea that 'i' etc can have length 1 or 2.
## Having the variable length no longer seems like a good idea,
## but we've written helper functions etc that expect it,
## so not worth changing.
updateYThetaCountsPoissonNotUseExp <- function(combined) {
    if (useC) {
        .Call(updateYThetaCountsPoissonNotUseExp_R, combined)
    }
    else {
        y <- combined@y
        model <- combined@model
        mu <- model@mu
        sigma <- model@sigma
        theta <- model@theta
        struc.zero.array <- model@strucZeroArray
        has.subtotals <- methods::is(y, "HasSubtotals")
        box.cox.param <- object@boxCoxParam
        update.theta.with.beta <- object@updateThetaWithBeta@.Data
        if (has.subtotals) {
            transform.subtotals <- y@transformSubtotals
        }
        for (i in seq_along(y)) {
            if (struc.zero.array[i] != 0L) {
                if (has.subtotals) {
                    i.other <- makeIOther(i = i, transform = transform.subtotals)
                    i <- c(i, i.other)
                    if (i.other > 0L) { ## found other cell with same subtotal
                        if (update.theta.with.beta)
                            th.prop <- theta[i]
                        else {
                            tr.th.prop <- rnorm(n = 2L, mean = mu[i], sd = sigma)
                            if (box.cox.param > 0)
                                th.prop <- (box.cox.param * tr.th.prop + 1) ^ (1 / box.cox.param)
                            else
                                th.prop <- exp(tr.th.prop)
                        }
                        sum.y <- sum(y[i])
                        y.prop <- as.integer(stats::rmultinom(n = 1L,
                                                              size = size,
                                                              prob = th.prop))
                    }
                    else if (i.other == 0L) { ## subtotal refers to single cell
                        next
                    }
                    else { ## cell not included in any subtotal
                        ## as.integer needed for R < 3.0
                        if (update.theta.with.beta)
                            th.prop <- theta[i]
                        else {
                            tr.th.prop <- rnorm(n = 1L, mean = mu[i], sd = sigma)
                            if (box.cox.param > 0)
                                th.prop <- (box.cox.param * tr.th.prop + 1) ^ (1 / box.cox.param)
                            else
                                th.prop <- exp(tr.th.prop)
                        }
                        y.prop <- as.integer(stats::rpois(n = 1L, lambda = th.prop))
                    }
                }
                else {
                    ## as.integer needed for R < 3.0
                    if (update.theta.with.beta)
                        th.prop <- theta[i]
                    else {
                        tr.th.prop <- rnorm(n = 1L, mean = mu[i], sd = sigma)
                        if (box.cox.param > 0)
                            th.prop <- (box.cox.param * tr.th.prop + 1) ^ (1 / box.cox.param)
                        else
                            th.prop <- exp(tr.th.prop)
                    }
                    y.prop <- as.integer(stats::rpois(n = 1L, lambda = th.prop))
                }
                diff.log.lik <- diffLogLik(yProp = y.prop,
                                           y = y,
                                           indicesY = i,
                                           dataModels = dataModels,
                                           datasets = datasets,
                                           transforms = transforms)
                accept <- (diff.log.lik >= 0) || (stats::runif(n = 1L) < exp(diff.log.lik))
                if (accept) {
                    y[i] <- y.prop
                    if (!update.theta.with.beta)
                        theta[i] <- th.prop
                }
            }
        }
        combind@y <- y
        combined@model@theta <- theta
        combined
    }
}
