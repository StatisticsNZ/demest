
updateAccountMove <- function(combined, useC = FALSE) {
    stopifnot(is(combined, "CombinedAccountMove"))
    if (useC) {
        .Call(updateAccountMove_R, combined)
    }
    else {
        account <- combined@account
        n.cell <- combined@nCellAccount
        for (i in seq_len(n.cell)) {
            combined <- updateProposalAccountMove(combined)
            generated.new.proposal <- combined@generatedNewProposal
            if (generated.new.proposal) {
                diff.log.lik <- diffLogLikAccountMove(combined)
                if (is.finite(diff.log.lik)) {
                    diff.log.dens <- diffLogDensAccountMove(combined)
                    log.r <- diff.log.lik + diff.log.dens
                    accept <- (log.r > 0) || (runif(n = 1L) < exp(log.r))
                    if (accept)
                        combined <- updateValuesAccountMove(combined)
                }
            }
        }
        combined
    }
}

updateProposalAccountMove <- function(combined, useC = FALSE) {
    stopifnot(is(combined, "CombinedAccountMove"))
    if (useC) {
        .Call(updateProposalAccountMove_R, combined)
    }
    else {
        prob.popn <- combined@probPopn
        update.popn <- runif(n = 1L) < prob.popn
        if (update.popn)
            updateProposalAccountMovePopn(combined)
        else {
            cum.prob <- combined@cumProbComp
            i.orig.dest <- combined@iOrigDest
            i.pool <- combined@iPool
            i.net <- combined@iNet
            i.comp <- rcateg1(cum.prob)
            combined@iComp <- i.comp
            if (i.comp == i.orig.dest)
                updateProposalAccountMoveOrigDest(combined)
            else if (i.comp == i.pool)
                updateProposalAccountMovePool(combined)
            else if (i.comp == i.net)
                updateProposalAccountMoveNet(combined)
            else
                updateProposalAccountMoveComp(combined = combined)
        }
    }
}




updateProposalAccountMoveOrigDest <- function(combined, useC = FALSE) {
    stopifnot(is(combined, "CombinedAccountMove"))
    if (useC) {
        .Call(updateProposalAccountMoveOrigDest_R, combined)
    }
    else {
        account <- combined@account
        population <- account@population
        components <- account@components
        uses.exposure <- combined@usesExposure
        i.comp <- combined@iComp
        component <- components[[i.comp]]
        if (uses.exposure[i.comp])
            expected.exposure <- combined@expectedExposure
        has.age <- combined@hasAge
        if (has.age) {
            accession <- combined@accession
            mapping.acc <- combined@mappingsToAcc[[i.comp]]
            iterator.acc <- combined@iteratorAcc
        }
        mapping.popn <- combined@mappingsToPopn[[i.comp]]
        description.comp <- combined@descriptionsComp[[i.comp]]
        mapping.exposure <- combined@mappingsToExposure[[i.comp]]
        iterator.popn <- combined@iteratorPopn                     
        sys.mod.popn <- combined@system[[1L]]
        sys.mod.comp <- combined@system[[i.comp + 1L]]
        theta.popn <- sys.mod.popn@theta
        theta.comp <- sys.mod.comp@theta
        max.attempt <- combined@maxAttempt
        i.cell <- chooseICellComp(description.comp)
        if (uses.exposure)
            i.exposure <- getIExposure(i = i.cell,
                                       mapping = mapping.exposure)
        pair.exp.first <- getIExpFirstPairFromOrigDest(i = i.cell,
                                                                mapping = mapping.exposure)
        i.exp.first.orig <- pair.exp.first[1L]
        i.exp.first.dest <- pair.exp.first[2L]
        is.lower.triangle <- isLowerTriangle(i = i.cell,
                                             description = description.comp)
        pair.popn.next <- getIPopnNextFromOrigDest(i = i.cell,
                                                   mapping = mapping.popn)
        i.popn.next.orig <- pair.popn.next[1L]
        i.popn.next.dest <- pair.popn.next[2L]
        min.val.orig <- getMinValCohort(i = i.popn.next.orig,
                                        series = population,
                                        iter = iterator.popn)
        min.val.dest <- getMinValCohort(i = i.popn.next.dest,
                                        series = population,
                                        iter = iterator.popn)
        if (has.age) {
            pair.acc.next <- getIAccNextFromOrigDest(i = i.cell,
                                                     mapping = mapping.acc)
            i.acc.next.orig <- pair.acc.next[1L]
            has.later.accession <- i.acc.next.orig > 0L
            if (has.later.accession) {
                i.acc.next.dest <- pair.acc.next[2L]
                min.acc.orig <- getMinValCohort(i = i.acc.next.orig,
                                                series = accession,
                                                iter = iterator.acc)
                min.acc.dest <- getMinValCohort(i = i.acc.next.dest,
                                                series = accession,
                                                iter = iterator.acc)
                min.val.orig <- min(min.val.orig, min.acc.orig)
                min.val.dest <- min(min.val.dest, min.acc.dest)
            }
        }
        theta.cell <- theta.comp[i.cell]
        if (uses.exposure[i.comp]) {
            expected.exposure.cell <- expected.exposure[i.exposure]
            lambda <- theta.cell * expected.exposure.cell
        }
        else
            lambda <- theta.cell
        val.curr <- component[i.cell]
        lower <- val.curr - min.val.dest
        upper <- val.curr + min.val.orig
        if (lower > upper)
            found.value <- FALSE
        else {
            val.prop <- rpoisTrunc1(lambda = lambda,
                                    lower = lower,
                                    upper = upper,
                                    maxAttempt = max.attempt)
            found.value <- !is.na(val.prop)
        }
        if (found.value) {
            diff.prop <- val.prop - val.curr
            generated.new.proposal <- diff.prop != 0L
        }
        else
            generated.new.proposal <- FALSE
        if (generated.new.proposal) {
            combined@iCell <- i.cell
            combined@iCellOther <- NA_integer_
            combined@isLowerTriangle <- is.lower.triangle
            combined@iPopnNext <- i.popn.next.orig
            combined@iPopnNextOther <- i.popn.next.dest
            if (uses.age) {
                combined@iAccNext <- i.acc.next.orig
                combined@iAccNextOther <- i.acc.next.dest
            }
            else {
                combined@iAccNext <- NA_integer_
                combined@iAccNextOther <- NA_integer_
            }
            if (uses.exposure[i.comp]) {
                combined@iExposure <- i.exposure
                combined@iExposureOther <- NA_integer_
            }
            else {
                combined@iExposure <- NA_integer_
                combined@iExposureOther <- NA_integer_
            }
            combined@iExpFirst <- i.exp.first.orig
            combined@iExpFirstOther <- i.exp.first.dest
            combined@diffProp <- diff.prop
        }
        else {
            combined@iCell <- NA_integer_
            combined@iCellOther <- NA_integer_
            combined@isLowerTriangle <- NA
            combined@iPopnNext <- NA_integer_
            combined@iPopnNextOther <- NA_integer_
            combined@iAccNext <- NA_integer_
            combined@iAccNextOther <- NA_integer_
            combined@iExposure <- NA_integer_
            combined@iExposureOther <- NA_integer_
            combined@iExpFirst <- NA_integer_
            combined@iExpFirstOther <- NA_integer_
            combined@diffProp <- NA_integer_
        }
        combined
    }
}

updateProposalAccountMovePool <- function(combined) {
    stopifnot(is(combined, "CombinedAccountMove"))
    if (useC) {
        .Call(updateProposalAccountMovePool_R, combined)
    }
    else {
        account <- combined@account
        population <- account@population
        i.comp <- combined@iComp
        component <- account@components[[i.comp]]
        uses.exposure <- combined@usesExposure[i.comp]
        if (uses.exposure)
            expected.exposure <- combined@expectedExposure
        has.age <- combined@hasAge
        if (has.age) {
            accession <- combined@accession
            iterator.acc <- combined@iteratorAcc
            mapping.acc <- combined@mappingsToAcc[[i.comp]]
        }
        mapping.popn <- combined@mappingsToPopn[[i.comp]]
        mapping.exposure <- combined@mappingsToExposure[[i.comp]]
        description.comp <- combined@descriptionsComp[[i.comp]]
        iterator.popn <- combined@iteratorPopn
        sys.mod.popn <- combined@system[[1L]]
        sys.mod.comp <- combined@system[[i.comp + 1L]]
        theta.popn <- sys.mod.popn@theta
        theta.comp <- sys.mod.comp@theta
        max.attempt <- combined@maxAttempt
        pair.cell <- chooseICellOutInPool(description.comp)
        i.cell.out <- pair.cell[1L]
        i.cell.in <- pair.cell[2L]
        i.exp.first.out <- getIExpFirstFromPool(i = i.cell.out,
                                                         mapping = mapping.exposure)
        i.exp.first.in <- getIExpFirstFromPool(i = i.cell.in,
                                                        mapping = mapping.exposure)
        is.lower.triangle <- isLowerTriangle(i = i.cell,
                                             description = description.comp)
        if (uses.exposure) {
            i.exposure.out <- getIExposure(i = i.cell.out,
                                           mapping = mapping.exposure)
            i.exposure.in <- getIExposure(i = i.cell.in,
                                          mapping = mapping.exposure)
        }
        i.popn.next.out <- getIPopnNextFromComp(i = i.cell.out,
                                                mapping = mapping.popn)
        i.popn.next.in <- getIPopnNextFromComp(i = i.cell.in,
                                               mapping = mapping.popn)
        min.val.out <- getMinValCohort(i = i.popn.next.out,
                                       series = population,
                                       iter = iterator.popn)
        min.val.in <- getMinValCohort(i = i.popn.next.in,
                                      series = population,
                                      iter = iterator.popn)
        if (has.age) {
            i.acc.next.out <- getIAccNextFromComp(i = i.cell.out,
                                                  mapping = mapping.acc)
            has.later.accession <- i.acc.next.out > 0L
            if (has.later.accession) {
                i.acc.next.in <- getIAccNextFromComp(i = i.cell.in,
                                                     mapping = mapping.acc)
                min.acc.out <- getMinValCohort(i = i.acc.next.out,
                                               series = accession,
                                               iter = iterator.acc)
                min.acc.in <- getMinValCohort(i = i.acc.next.in,
                                              series = accession,
                                              iter = iterator.acc)
                min.val.out <- min(min.val.out, min.acc.out)
                min.val.in <- min(min.val.in, min.acc.in)
            }
        }
        theta.out <- theta.comp[i.cell.out]
        if (uses.exposure) {
            expected.exposure.out <- expected.exposure[i.exposure.out]
            lambda.out <- theta.out * expected.exposure.out
        }
        else
            lambda.out <- theta.out
        val.curr.out <- component[i.cell.out]
        val.curr.in <- component[i.cell.in]
        lower <- val.curr.in - min.val.in
        upper <- val.curr.out + min.val.out
        if (lower > upper)
            found.value <- FALSE
        else {
            val.prop.out <- rpoisTrunc1(lambda = lambda.out,
                                        lower = lower,
                                        upper = upper,
                                        maxAttempt = max.attempt)
            found.value <- !is.na(val.prop.out)
        }
        if (found.value) {
            diff.prop <- val.prop.out - val.curr.out
            generated.new.proposal <- diff.prop != 0L
        }
        else
            generated.new.proposal <- FALSE
        if (generated.new.proposal) {
            combined@iCell <- i.cell.out
            combined@iCellOther <- i.cell.in
            combined@isLowerTriangle <- is.lower.triangle
            combined@iPopnNext <- i.popn.next.out
            combined@iPopnNextOther <- i.popn.next.in
            if (uses.age) {
                combined@iAccNext <- i.acc.next.out
                combined@iAccNextOther <- i.acc.next.in
            }
            else {
                combined@iAccNext <- NA_integer_
                combined@iAccNextOther <- NA_integer_
            }
            if (uses.exposure) {
                combined@iExposure <- i.exposure.out
                combined@iExposureOther <- i.exposure.in
            }
            else {
                combined@iExposure <- NA_integer_
                combined@iExposureOther <- NA_integer_
            }
            combined@iExpFirst <- i.exp.first.out
            combined@iExpFirstOther <- i.exp.first.in
            combined@diffProp <- diff.prop
        }
        else {
            combined@iCell <- NA_integer_
            combined@iCellOther <- NA_integer_
            combined@isLowerTriangle <- NA
            combined@iPopnNext <- NA_integer_
            combined@iPopnNextOther <- NA_integer_
            combined@iAccNext <- NA_integer_
            combined@iAccNextOther <- NA_integer_
            combined@iExposure <- NA_integer_
            combined@iExposureOther <- NA_integer_
            combined@iExpFirst <- NA_integer_
            combined@iExpFirstOther <- NA_integer_
            combined@diffProp <- NA_integer_
        }
        combined
    }
}

updateProposalAccountMoveNet <- function(combined) {
    stopifnot(is(combined, "CombinedAccountMove"))
    if (useC) {
        .Call(updateProposalAccountMoveNet_R, combined)
    }
    else {
        account <- combined@account
        population <- account@population
        i.comp <- combined@iComp
        component <- account@components[[i.comp]]
        has.age <- combined@hasAge
        if (has.age) {
            accession <- combined@accession
            iterator.acc <- combined@iteratorAcc
            mapping.to.acc <- combined@mappingsToAcc[[i.comp]]
        }        
        mapping.to.popn <- combined@mappingsToPopn[[i.comp]]
        description.comp <- combined@descriptionsComp[[i.comp]]
        iterator.popn <- combined@iteratorPopn
        sys.mod.popn <- combined@system[[1L]]
        sys.mod.comp <- combined@system[[i.comp + 1L]]
        theta.popn <- sys.mod.popn@theta
        theta.comp <- sys.mod.comp@theta
        varsigma.comp <- sys.mod.comp@varsigma
        w.comp <- sys.mod.comp@w
        max.attempt <- combined@maxAttempt
        pair.cell <- chooseICellOutInNet(description.comp)
        i.cell.1 <- pair.cell[1L]
        i.cell.2 <- pair.cell[2L]
        is.lower.triangle <- isLowerTriangle(i = i.cell,
                                             description = description.comp)
        i.exp.first.1 <- getIExpFirstFromComp(i = i.cell.1, # needed??
                                                       mapping = mapping.exposure)
        i.exp.first.2 <- getIExpFirstFromComp(i = i.cell.2,
                                                       mapping = mapping.exposure)
        i.popn.next.1 <- getIPopnNextFromComp(i = i.cell.1,
                                              mapping = mapping.to.popn)
        i.popn.next.2 <- getIPopnNextFromComp(i = i.cell.2,
                                              mapping = mapping.to.popn)
        min.val.1 <- getMinValCohort(i = i.popn.next.1,
                                     series = population,
                                     iter = iterator.popn)
        min.val.2 <- getMinValCohort(i = i.popn.next.2,
                                     series = population,
                                     iter = iterator.popn)
        if (has.age) {
            i.acc.next.1 <- getIAccNextFromComp(i = i.cell.1,
                                                mapping = mapping.to.acc)
            has.later.accession <- i.acc.next.1 > 0L
            if (has.later.accession) {
                i.acc.next.2 <- getIAccNextFromComp(i = i.cell.2,
                                                    mapping = mapping.to.acc)
                min.acc.1 <- getMinValCohort(i = i.acc.next.1,
                                             series = accession,
                                             iter = iterator.acc)
                min.acc.2 <- getMinValCohort(i = i.acc.next.2,
                                             series = accession,
                                             iter = iterator.acc)
                min.val.1 <- min(min.val.1, min.acc.1)
                min.val.2 <- min(min.val.2, min.acc.2)
            }
        }
        mean.1 <- theta.comp[i.cell.1]
        val.curr.1 <- component[i.cell.1]
        val.curr.2 <- component[i.cell.2]
        lower <- val.curr.1 - min.val.1
        upper <- val.curr.2 + min.val.2
        if (lower > upper)
            found.value <- FALSE
        else {
            w.1 <- w.comp[i.cell.1]
            sd.1 <- varsigma.comp / w.1
            val.prop.1 <- rnormIntTrunc1(mean = mean.1,
                                         sd = sd.1,
                                         lower = lower,
                                         upper = upper,
                                         maxAttempt = max.attempt)
            found.value <- !is.na(val.prop)
        }
        if (found.value) {
            diff.prop <- val.prop - val.curr
            generated.new.proposal <- diff.prop != 0L
        }
        else
            generated.new.propoal <- FALSE
        if (generated.new.proposal) {
            combined@iCell <- i.cell.1
            combined@iCellOther <- i.cell.2
            combined@isLowerTriangle <- is.lower.triangle
            combined@iPopnNext <- i.popn.next.1
            combined@iPopnNextOther <- i.popn.next.2
            if (uses.age) {
                combined@iAccNext <- i.acc.next.1
                combined@iAccNextOther <- i.acc.next.2
            }
            else {
                combined@iAccNext <- NA_integer_
                combined@iAccNextOther <- NA_integer_
            }
            combined@iExposure <- NA_integer_
            combined@iExposureOther <- NA_integer_
            combined@iExpFirst <- i.exp.first.1
            combined@iExpFirstOther <- i.exp.first.2
            combined@diffProp <- diff.prop
        }
        else {
            combined@iCell <- NA_integer_
            combined@iCellOther <- NA_integer_
            combined@isLowerTriangle <- NA
            combined@iPopnNext <- NA_integer_
            combined@iPopnNextOther <- NA_integer_
            combined@iAccNext <- NA_integer_
            combined@iAccNextOther <- NA_integer_
            combined@iExposure <- NA_integer_
            combined@iExposureOther <- NA_integer_
            combined@iExpFirst <- NA_integer_
            combined@iExpFirstOther <- NA_integer_
            combined@diffProp <- NA_integer_
        }
        combined
    }
}

updateProposalAccountMoveComp <- function(combined) {
    stopifnot(is(combined, "CombinedAccountMove"))
    if (useC) {
        .Call(updateProposalAccountMoveComp_R, combined)
    }
    else {
        account <- combined@account
        population <- account@population
        i.comp <- combined@iComp
        component <- account@components[[i.comp]]
        is.increment <- combined@isIncrement[[i.comp]]
        has.age <- combined@hasAge
        if (has.age) {
            accession <- combined@accession
            iterator.acc <- combined@iteratorAcc
            mapping.acc <- combined@mappingsToAcc[[i.comp]]
        }        
        mapping.to.popn <- combined@mappingsToPopn[[i.comp]]
        iterator.popn <- combined@iteratorPopn
        uses.exposure <- combined@usesExposure[[i.comp]]
        if (uses.exposure) {
            exposure <- combined@exposure
            mapping.to.exposure <- combined@mappingsToExposure[[i.comp]]
        }
        description.comp <- combined@descriptionsComp[[i.comp]]
        sys.mod.popn <- combined@system[[1L]]
        sys.mod.comp <- combined@system[[i.comp + 1L]]
        theta.popn <- sys.mod.popn@theta
        theta.comp <- sys.mod.comp@theta
        is.net.entries <- combined@isNetEntries[[i.comp]]
        if (is.net.entries) {
            varsigma.comp <- sys.mod.comp@varsigma
            w.comp <- sys.mod.comp@w
        }
        max.attempt <- combined@maxAttempt
        i.cell <- chooseICellComp(description.comp)
        i.exp.first <- getIExpFirstFromComp(i = i.cell,
                                                     mapping = mapping.exposure)
        is.lower.triangle <- isLowerTriangle(i = i.cell,
                                             description = description.comp)
        if (uses.exposure)
            i.exposure <- getIExposure(i = i.cell,
                                       mapping = mapping.to.exposure)
        i.popn.next <- getIPopnNextFromComp(i = i.cell,
                                            mapping = mapping.to.popn)
        min.val <- getMinValCohort(i = i.popn.next,
                                   series = population,
                                   iter = iterator.popn)
        if (has.age) {
            i.acc.next <- getIAccNextFromComp(i = i.cell,
                                              mapping = mapping.to.acc)
            has.later.accession <- i.acc.next > 0L
            if (has.later.accession) {
                min.acc <- getMinValCohort(i = i.acc.next,
                                           series = accession,
                                           iter = iterator.acc)
                min.val <- min(min.val, min.acc)
            }
        }
        val.curr <- component[i.cell]
        if (is.increment) {
            lower <- val.curr - min.val
            upper <- NA_integer_
        }
        else {
            lower <- NA_integer_
            upper <- val.curr + min.val
        }
        if (is.net.entries) {
            mean <- theta.comp[i.cell]
            w.cell <- w.comp[i.cell]
            sd <- varsigma.comp / w.cell
            val.prop <- rnormIntTrunc1(mean = mean,
                                       sd = sd,
                                       lower = lower,
                                       upper = upper,
                                       maxAttempt = max.attempt)
        }
        else {
            theta.cell <- theta.comp[i.cell]
            if (uses.exposure) {
                exposure.cell <- exposure[i.exposure]
                lambda <- theta.cell * exposure.cell
            }
            else
                lambda <- theta.cell
            val.prop <- rpoisTrunc1(lambda = lambda,
                                    lower = lower,
                                    upper = upper,
                                    maxAttempt = max.attempt)
        }
        found.value <- !is.na(val.prop)
        if (found.value) {
            diff.prop <- val.prop - val.curr
            generated.new.proposal <- diff.prop != 0L
        }
        else
            generated.new.proposal <- FALSE
        if (generated.new.proposal) {
            combined@iCell <- i.cell
            combined@iCellOther <- NA_integer_
            combined@isLowerTriangle <- is.lower.triangle
            combined@iPopnNext <- i.popn.next
            combined@iPopnNextOther <- NA_integer_
            if (uses.age) {
                combined@iAccNext <- i.acc.next
                combined@iAccNextOther <- NA_integer_
            }
            else {
                combined@iAccNext <- NA_integer_
                combined@iAccNextOther <- NA_integer_
            }
            if (uses.exposure) {
                combined@iExposure <- i.exposure
                combined@iExposureOther <- NA_integer_
            }
            else {
                combined@iExposure <- NA_integer_
                combined@iExposureOther <- NA_integer_
            }
            combined@iExpFirst <- i.exp.first
            combined@iExpFirstOther <- NA_integer_
            combined@diffProp <- diff.prop
        }
        else {
            combined@iCell <- NA_integer_
            combined@iCellOther <- NA_integer_
            combined@isLowerTriangle <- NA
            combined@iPopnNext <- NA_integer_
            combined@iPopnNextOther <- NA_integer_
            combined@iAccNext <- NA_integer_
            combined@iAccNextOther <- NA_integer_
            combined@iExposure <- NA_integer_
            combined@iExposureOther <- NA_integer_
            combined@iExpFirst <- NA_integer_
            combined@iExpFirstOther <- NA_integer_
            combined@diffProp <- NA_integer_
        }
        combined
    }
}

## LOG LIKELIHOOOD #########################################################

diffLogLikAccountMove <- function(combined) {
    stopifnot(is(combined, "CombinedAccountMove"))
    if (useC) {
        .Call(diffLogLikAccountMove_R, combined)
    }
    else {
        i.comp <- combined@iComp
        i.orig.dest <- combined@iOrigDest
        i.pool <- combined@iPool
        i.net <- combined@iNet
        if (i.comp == 0L)
            diffLogLikAccountMovePopn(combined)
        else if (i.comp == i.orig.dest)
            diffLogLikAccountMoveOrigDest(combined)
        else if (i.comp == i.pool) 
            diffLogLikAccountMovePool(combined)
        else if (i.comp == i.net) 
            diffLogLikAccountMoveNet(combined)
        else
            diffLogLikAccountMoveComp(combined)
    }
}

diffLogLikAccountMovePopn <- function(combined, useC = FALSE) {
    stopifnot(is(combined, "CombinedAccountMove"))
    if (useC) {
        .Call(diffLogLikAccountMovePopn_R, combined)
    }
    else {
        account <- combined@account
        population <- account@population
        iterator <- combined@iteratorPopn
        observation <- combined@observation
        datasets <- combined@datasets
        series.indices <- combined@seriesIndices
        transforms <- combined@transforms
        i.cell <- combined@iCell
        diff <- combined@diffProp
        diffLogLikPopn(diff = diff,
                       iFirst = i.cell,
                       iterator = iterator,
                       population = population,
                       observation = observation,
                       datasets = datasets,
                       seriesIndices = series.indices,
                       transforms = transforms)
    }
}

diffLogLikAccountMoveOrigDest <- function(combined, useC = TRUE) {
    stopifnot(is(combined, "CombinedAccountMove"))
    if (useC) {
        .Call(diffLogLikAccountMoveOrigDest_R, combined)
    }
    else {
        account <- combined@account
        i.comp <- combined@iComp
        component <- account@components[[i.comp]]
        population <- account@population
        iterator <- combined@iteratorPopn
        observation <- combined@observation
        datasets <- combined@datasets
        series.indices <- combined@seriesIndices
        transforms <- combined@transforms
        i.cell <- combined@iCell
        i.popn.orig <- combined@iPopnNext
        i.popn.dest <- combined@iPopnNextOther
        diff <- combined@diffProp
        diff.cell <- diffLogLikCellComp(diff = diff,
                                        iCell = i.cell,
                                        iComp = i.comp,
                                        component = component,
                                        observation = observation,
                                        datasets = datasets,
                                        seriesIndices = series.indices,
                                        transforms = transforms)
        if (is.infinite(diff.cell))
            return(diff.cell)
        diff.popn <- diffLogLikPopnPair(diff = diff,
                                        iPopnOrig = i.popn.orig,
                                        iPopnDest = i.popn.dest,
                                        iterator = iterator,
                                        population = population,
                                        observation = observation,
                                        datasets = datasets,
                                        seriesIndices = series.indices,
                                        transforms = transforms)
        diff.cell + diff.popn
    }
}


diffLogLikAccountMovePool <- function(combined, useC = FALSE) {
    stopifnot(is(combined, "CombinedAccountMove"))
    if (useC) {
        .Call(diffLogLikAccountMovePool_R, combined)
    }
    else {
        account <- combined@account
        i.comp <- combined@iComp
        component <- account@components[[i.comp]]
        population <- account@population
        iterator <- combined@iteratorPopn
        observation <- combined@observation
        datasets <- combined@datasets
        series.indices <- combined@seriesIndices
        transforms <- combined@transforms
        i.cell.out <- combined@iCell
        i.cell.in <- combined@iCellOther
        i.popn.out <- combined@iPopnNext
        i.popn.in <- combined@iPopnNextOther
        diff <- combined@diffProp
        diff.cells <- diffLogLikCellsPoolNet(diff = diff,
                                             iCellOut = i.cell.out,
                                             iCellIn = i.cell.in,
                                             isPool = TRUE,
                                             iComp = i.comp,
                                             component = component,
                                             observation = observation,
                                             datasets = datasets,
                                             seriesIndices = series.indices,
                                             transforms = transforms)
        if (is.infinite(diff.log.lik.cell))
            return(diff.log.lik.cell)
        diff.popn <- diffLogLikPopnPair(diff = diff,
                                        iPopnOrig = i.popn.out,
                                        iPopnDest = i.popn.in,
                                        iterator = iterator,
                                        population = population,
                                        observation = observation,
                                        datasets = datasets,
                                        seriesIndices = series.indices,
                                        transforms = transforms)
        diff.cell + diff.popn
    }
}


diffLogLikAccountMoveNet <- function(combined, useC = FALSE) {
    stopifnot(is(combined, "CombinedAccountMove"))
    if (useC) {
        .Call(diffLogLikAccountMoveNet_R, combined)
    }
    else {
        account <- combined@account
        i.comp <- combined@iComp
        component <- account@components[[i.comp]]
        population <- account@population
        iterator <- combined@iteratorPopn
        observation <- combined@observation
        datasets <- combined@datasets
        series.indices <- combined@seriesIndices
        transforms <- combined@transforms
        i.cell.orig <- combined@iCell
        i.cell.dest <- combined@iCellOther
        i.popn.orig <- combined@iPopnNext
        i.popn.dest <- combined@iPopnNextOther
        diff <- combined@diffProp
        diff.cells <- diffLogLikCellsPoolNet(diff = diff,
                                             iCellOrig = i.cell.orig,
                                             iCellDest = i.cell.dest,
                                             isPool = FALSE,
                                             iComp = i.comp,
                                             component = component,
                                             observation = observation,
                                             datasets = datasets,
                                             seriesIndices = series.indices,
                                             transforms = transforms)
        if (is.infinite(diff.log.lik.cell))
            return(diff.log.lik.cell)
        diff.popn <- diffLogLikPopnPair(diff = diff,
                                        iPopnOrig = i.popn.out,
                                        iPopnDest = i.popn.in,
                                        iterator = iterator,
                                        population = population,
                                        observation = observation,
                                        datasets = datasets,
                                        seriesIndices = series.indices,
                                        transforms = transforms)
        diff.cell + diff.popn
    }
}


diffLogLikAccountMoveComp <- function(combined, useC = FALSE) {
    stopifnot(is(combined, "CombinedAccountMove"))
    if (useC) {
        .Call(diffLogLikAccountMoveComp_R, combined)
    }
    else {
        account <- combined@account
        i.comp <- combined@iComp
        component <- account@components[[i.comp]]
        population <- account@population
        iterator <- combined@iteratorPopn
        observation <- combined@observation
        datasets <- combined@datasets
        series.indices <- combined@seriesIndices
        transforms <- combined@transforms
        i.cell <- combined@iCell
        i.popn.next <- combined@iPopnNext
        diff <- combined@diffProp
        is.increment <- combined@isIncrement[i.comp]
        diff.log.lik.cell <- diffLogLikCellComp(diff = diff,
                                                iComp = i.comp,
                                                iCell = i.cell,
                                                component = component,
                                                observation = observation,
                                                datasets = datasets,
                                                seriesIndices = series.indices,
                                                transforms = transforms)
        if (is.infinite(diff.log.lik.cell))
            return(diff.log.lik.cell)
        diff.popn <- if (is.increment) diff else -diff
        diff.log.lik.popn <- diffLogLikPopn(diff = diff.popn,
                                            iFirst = i.popn.next,
                                            iterator = iterator,
                                            population = population,
                                            observation = observation,
                                            datasets = datasets,
                                            seriesIndices = series.indices,
                                            transforms = transforms)
        if (is.infinite(diff.log.lik.popn))
            return(diff.log.lik.popn)
        diff.log.lik.cell + diff.log.lik.popn
    }
}

diffLogLikPopn <- function(diff, iFirst, iterator, population,
                           observation, datasets,
                           seriesIndices, transforms,
                           useC = FALSE) {
    ## diff
    stopifnot(identical(length(diff), 1L))
    stopifnot(is.integer(diff))
    stopifnot(!is.na(diff))
    ## iFirst
    stopifnot(identical(length(iFirst), 1L))
    stopifnot(is.integer(iFirst))
    stopifnot(!is.na(iFirst))
    stopifnot(iFirst > 0L)
    ## iterator
    stopifnot(is(iterator, "CohortIteratorAccessionPopulation"))
    ## population
    stopifnot(is(population, "Population"))
    ## observation
    stopifnot(is.list(observation))
    stopifnot(all(sapply(observation, is, "Model")))
    ## datasets
    stopifnot(is.list(datasets))
    stopifnot(all(sapply(datasets, is, "Counts")))
    ## seriesIndices
    stopifnot(is.integer(seriesIndices))
    stopifnot(!any(is.na(seriesIndices)))
    stopifnot(all(seriesIndices >= 0L))
    ## transforms
    stopifnot(is.list(transforms))
    stopifnot(all(sapply(transforms, is, "CollapseTransformExtra")))
    ## observation and datasets
    stopifnot(identical(length(observation), length(datasets)))
    ## observation and seriesIndices
    stopifnot(identical(length(observation), length(seriesIndices)))
    ## observation and transforms
    stopifnot(identical(length(observation), length(transforms)))
    if (useC) {
        .Call(diffLogLikPopn_R,
              diff, iFirst, iterator, population, observation,
              datasets, seriesIndices)
    }
    else {        
        ans <- 0
        for (i.dataset in seq_along(datasets)) {
            assoc.with.popn <- seriesIndices[i.dataset] == 0L
            if (assoc.with.popn) {
                model <- observation[[i.dataset]]
                dataset <- datasets[[i.dataset]]
                transform <- transforms[[i.dataset]]
                diff.dataset <- diffLogLikPopnOneDataset(diff = diff,
                                                         iFirst = iFirst,
                                                         iterator = iterator,
                                                         population = population,
                                                         model = model,
                                                         dataset = dataset,
                                                         transform = transform)
                if (is.infinite(diff.dataset))
                    return(diff.dataset)
                ans <- ans + diff.dataset
            }
        }
        ans
    }
}

diffLogLikPopnPair <- function(diff, iPopnOrig, iPopnDest,
                               iterator, population,
                               observation, datasets,
                               seriesIndices, transforms,
                               useC = FALSE) {
    ## diff
    stopifnot(identical(length(diff), 1L))
    stopifnot(is.integer(diff))
    stopifnot(!is.na(diff))
    ## iFirst
    stopifnot(identical(length(iFirst), 1L))
    stopifnot(is.integer(iFirst))
    stopifnot(!is.na(iFirst))
    stopifnot(iFirst > 0L)
    ## iterator
    stopifnot(is(iterator, "CohortIteratorAccessionPopulation"))
    ## population
    stopifnot(is(population, "Population"))
    ## observation
    stopifnot(is.list(observation))
    stopifnot(all(sapply(observation, is, "Model")))
    ## datasets
    stopifnot(is.list(datasets))
    stopifnot(all(sapply(datasets, is, "Counts")))
    ## seriesIndices
    stopifnot(is.integer(seriesIndices))
    stopifnot(!any(is.na(seriesIndices)))
    stopifnot(all(seriesIndices >= 0L))
    ## transforms
    stopifnot(is.list(transforms))
    stopifnot(all(sapply(transforms, is, "CollapseTransformExtra")))
    ## observation and datasets
    stopifnot(identical(length(observation), length(datasets)))
    ## observation and seriesIndices
    stopifnot(identical(length(observation), length(seriesIndices)))
    ## observation and transforms
    stopifnot(identical(length(observation), length(transforms)))
    if (useC) {
        .Call(diffLogLikPopnPair_R,
              diff, iPopnOrig, iPopnDest, iterator,
              population, observation, datasets,
              seriesIndices, transforms)
    }
    else {        
        ans <- 0
        for (i.dataset in seq_along(datasets)) {
            assoc.with.popn <- seriesIndices[i.dataset] == 0L
            if (assoc.with.popn) {
                transform <- transforms[[i.dataset]]
                i.after.orig <- getIAfter(i = iPopnOrig, transform = transform)
                i.after.dest <- getIAfter(i = iPopnDest, transform = transform)
                if (i.after.orig != i.after.dest) {
                    model <- observation[[i.dataset]]
                    dataset <- datasets[[i.dataset]]
                    transform <- transforms[[i.dataset]]
                    diff.orig <- diffLogLikPopnOneDataset(diff = -diff,
                                                          iFirst = iPopnOrig,
                                                          iterator = iterator,
                                                          population = population,
                                                          model = model,
                                                          dataset = dataset,
                                                          transform = transform)
                    if (is.infinite(diff.orig))
                        return(diff.orig)
                    diff.dest <- diffLogLikPopnOneDataset(diff = diff,
                                                          iFirst = iPopnDest,
                                                          iterator = iterator,
                                                          population = population,
                                                          model = model,
                                                          dataset = dataset,
                                                          transform = transform)
                    if (is.infinite(diff.dest))
                        return(diff.dest)
                    ans <- ans + diff.orig + diff.dest
                }
            }
        }
        ans
    }
}    

diffLogLikPopnOneDataset <- function(diff, iFirst, iterator, population,
                                     model, dataset, transform,
                                     useC = FALSE) {
    ## diff
    stopifnot(identical(length(diff), 1L))
    stopifnot(is.integer(diff))
    stopifnot(!is.na(diff))
    ## iFirst
    stopifnot(identical(length(iFirst), 1L))
    stopifnot(is.integer(iFirst))
    stopifnot(!is.na(iFirst))
    stopifnot(iFirst > 0L)
    ## iterator
    stopifnot(is(iterator, "CohortIteratorAccessionPopulation"))
    ## model
    stopifnot(is(model, "Model"))
    ## dataset
    stopifnot(is(dataset, "Counts"))
    ## transform
    stopifnot(is(transform, "CollapseTransformExtra"))
    if (useC) {
        .Call(diffLogLikPopnOneDataset_R, diff, iFirst, iterator,
              model, dataset, transform)
    }
    else {
        iterator <- resetCAP(iterator, i = iFirst)
        i.after <- getIAfter(i = i.before, transform = transform)
        if (iterator@finished) {
            diffLogLikPopnOneCell(iAfter = i.after,
                                  nContribute = 1L,
                                  diff = diff,
                                  transform = transform,
                                  population = population,
                                  model = model,
                                  dataset = dataset)
        }
        else {
            i.after.old <- i.after
            n.contribute <- 1L
            ans <- 0
            while (!iterator@finished) {
                iterator <- advanceCAP(iterator)
                i.before <- iterator@i
                i.after <- getIAfter(i = i.before, transform = transform)
                if (i.after == i.after.old) {
                    n.contribute <- n.contribute + 1L
                }
                else {
                    ans <- ans + diffLogLikPopnOneCell(iAfter = i.after.old,
                                                       nContribute = nContribute,
                                                       diff = diff,
                                                       transform = transform,
                                                       population = population,
                                                       model = model,
                                                       dataset = dataset)
                    i.after.old <- i.after
                }
            }
            ans
        }
    }
}

diffLogLikPopnOneCell <- function(iAfter, nContribute, diff, population
                                  model, dataset, transform, useC = FALSE) {
    ## iAfter
    stopifnot(identical(length(iAfter), 1L))
    stopifnot(is.integer(iAfter))
    stopifnot(!is.na(iAfter))
    stopifnot(iAfter > 0L)
    ## nContribute
    stopifnot(identical(length(nContribute), 1L))
    stopifnot(is.integer(nContribute))
    stopifnot(!is.na(nContribute))
    stopifnot(nContribute > 0L)
    ## diff
    stopifnot(identical(length(diff), 1L))
    stopifnot(is.integer(diff))
    stopifnot(!is.na(diff))
    ## population
    stopifnot(is(population, "Population"))
    ## model
    stopifnot(is(model, "Model"))
    ## dataset
    stopifnot(is(dataset, "Counts"))
    ## transform
    stopifnot(is(transform, "CollapseTransformExtra"))
    if (useC) {
        .Call(diffLogLikPopnOneCell_R,
              iAfter, nContribute, diff, population,
              model, dataset, transform)
    }
    else {
        cell.is.observed <- !is.na(dataset@.Data[iAfter])
        if (!cell.is.observed)
            return(0)
        vec.i.shared <- getIShared(i = iAfter, transform = transform)
        total.popn.curr <- 0L
        for (i.shared in  vec.i.shared)
            total.popn.curr <- total.popn.curr + population[i.shared]
        total.popn.prop <- total.popn.curr + nContribute * diff
        log.lik.prop <- likeLikelihood(model = model,
                                       count = total.popn.prop,
                                       dataset = dataset,
                                       i = i.after)
        if (is.infinite(log.lik.prop))
            return(log.lik.prop)
        log.lik.curr <- logLikelihood(model = model,
                                      count = total.popn.curr,
                                      dataset = dataset,
                                      i = i.after)
        log.lik.prop - log.lik.curr
    }
}

diffLogLikCellComp <- function(diff, iComp, iCell, component,
                               observation, datasets,
                               seriesIndices, transforms,
                               useC = FALSE) {
    ## diff
    stopifnot(identical(length(diff), 1L))
    stopifnot(is.integer(diff))
    stopifnot(!is.na(diff))
    ## iComp
    stopifnot(identical(length(iComp), 1L))
    stopifnot(is.integer(iComp))
    stopifnot(!is.na(iComp))
    stopifnot(iComp >= 0L)
    ## iCell
    stopifnot(identical(length(iCell), 1L))
    stopifnot(is.integer(iCell))
    stopifnot(!is.na(iCell))
    stopifnot(iCell >= 0L)
    ## component
    stopifnot(is(component, "Component"))
    ## observation
    stopifnot(is.list(observation))
    stopifnot(all(sapply(observation, is, "Model")))
    ## datasets
    stopifnot(is.list(datasets))
    stopifnot(all(sapply(datasets, is, "Counts")))
    ## seriesIndices
    stopifnot(is.integer(seriesIndices))
    stopifnot(!any(is.na(seriesIndices)))
    stopifnot(all(seriesIndices >= 0L))
    ## transforms
    stopifnot(is.list(transforms))
    stopifnot(all(sapply(transforms, is, "CollapseTransformExtra")))
    ## observation and datasets
    stopifnot(identical(length(observation), length(datasets)))
    ## observation and seriesIndices
    stopifnot(identical(length(observation), length(seriesIndices)))
    ## observation and transforms
    stopifnot(identical(length(observation), length(transforms)))
    if (useC) {
        .Call(diffLogLikCellComp_R,
              diff, iComp, iCell, component, observation,
              datasets, seriesIndices, transforms)
    }
    else {
        ans <- 0
        for (i.dataset in seq_along(datasets)) {
            assoc.with.comp <- seriesIndices[i.dataset] == iComp
            if (assoc.with.comp) {
                dataset <- datasets[[i.dataset]]
                model <- observation[[i.dataset]]
                transform <- transforms[[i.dataset]]
                diff.dataset <- diffLogLikCellOneDataset(diff = diff,
                                                         iCell = iCell,
                                                         component = component,
                                                         model = model,
                                                         dataset = dataset,
                                                         transform = transform)
                if (is.infinite(diff.dataset))
                    return(diff.dataset)
                ans <- ans + diff.dataset
            }
        }
        ans
    }
}

diffLogLikCellsPoolNet <- function(diff, iComp, iCellOut, iCellIn,
                                   isPool, component, observation, datasets,
                                   seriesIndices, transforms,
                                   useC = FALSE) {
    ## diff
    stopifnot(identical(length(diff), 1L))
    stopifnot(is.integer(diff))
    stopifnot(!is.na(diff))
    ## iComp
    stopifnot(identical(length(iComp), 1L))
    stopifnot(is.integer(iComp))
    stopifnot(!is.na(iComp))
    stopifnot(iComp > 0L)
    ## iCellOut
    stopifnot(identical(length(iCellOut), 1L))
    stopifnot(is.integer(iCellOut))
    stopifnot(!is.na(iCellOut))
    stopifnot(iCellOut > 0L)
    ## iCellIn
    stopifnot(identical(length(iCellIn), 1L))
    stopifnot(is.integer(iCellIn))
    stopifnot(!is.na(iCellIn))
    stopifnot(iCellIn > 0L)
    ## isPool
    stopifnot(identical(length(isPool), 1L))
    stopifnot(is.logical(isPool))
    stopifnot(!is.na(isPool))
    ## component
    stopifnot(is(component, "Component"))
    ## observation
    stopifnot(is.list(observation))
    stopifnot(all(sapply(observation, is, "Model")))
    ## datasets
    stopifnot(is.list(datasets))
    stopifnot(all(sapply(datasets, is, "Counts")))
    ## seriesIndices
    stopifnot(is.integer(seriesIndices))
    stopifnot(!any(is.na(seriesIndices)))
    stopifnot(all(seriesIndices >= 0L))
    ## transforms
    stopifnot(is.list(transforms))
    stopifnot(all(sapply(transforms, is, "CollapseTransformExtra")))
    ## observation and datasets
    stopifnot(identical(length(observation), length(datasets)))
    ## observation and seriesIndices
    stopifnot(identical(length(observation), length(seriesIndices)))
    ## observation and transforms
    stopifnot(identical(length(observation), length(transforms)))
    if (useC) {
        .Call(diffLogLikCellsPool_R,
              diff, iComp, iCellOut, iCellIn, component, observation,
              datasets, seriesIndices, transforms)
    }
    else {
        ans <- 0
        for (i.dataset in seq_along(datasets)) {
            assoc.with.comp <- seriesIndices[i.dataset] == iComp
            if (assoc.with.comp) {
                transform <- transforms[[i.dataset]]
                i.after.out <- getIAfter(iCellOut, transform = transform)
                i.after.in <- getIAfter(iCellIn, transform = transform)
                if (i.after.out != i.after.in) {
                    model <- observation[[i.dataset]]
                    dataset <- datasets[[i.dataset]]
                    diff.out <- diffLogLikCellOneDataset(diff = diff,
                                                         iCell = iCellOut,
                                                         component = component,
                                                         model = model,
                                                         dataset = dataset,
                                                         transform = transform)
                    if (is.infinite(diff.out))
                        return(diff.out)
                    if (!isPool)
                        diff <- -diff
                    diff.in <- diffLogLikCellOneDataset(diff = diff,
                                                        iCell = iCellIn,
                                                        component = component,
                                                        model = model,
                                                        dataset = dataset,
                                                        transform = transform)
                    if (is.infinite(diff.in))
                        return(diff.in)
                    ans <- ans + diff.out + diff.in
                }
            }
        }
        ans
    }
}

diffLogLikCellOneDataset <- function(diff, iCell, component,
                                     model, dataset, transform) {
    ## diff
    stopifnot(identical(length(diff), 1L))
    stopifnot(is.integer(diff))
    stopifnot(!is.na(diff))
    ## iCell
    stopifnot(identical(length(iCell), 1L))
    stopifnot(is.integer(iCell))
    stopifnot(!is.na(iCell))
    stopifnot(iCell >= 0L)
    ## component
    stopifnot(is(component, "Component"))
    ## iterator
    stopifnot(is(iterator, "CohortIteratorAccessionPopulation"))
    ## model
    stopifnot(is(model, "Model"))
    ## dataset
    stopifnot(is(dataset, "Counts"))
    ## transform
    stopifnot(is(transform, "CollapseTransformExtra"))
    if (useC) {
        .Call(diffLogLikCellOneDataset_R, diff, iCell, component,
              model, dataset, transform)
    }
    else {
        i.after <- getIAfter(iCell, transform = transform)
        cell.is.observed <- !is.na(dataset@.Data[i.after])
        if (!cell.is.observed)
            return(0)
        i.shared <- getIShared(iCell, transform = transform)
        total.comp.curr <- sum(component[i.shared])
        total.comp.prop <- total.comp.curr + diff
        log.lik.prop <- logLikelihood(model = model,
                                      count = total.comp.prop,
                                      dataset = dataset,
                                      i = i.after)
        if (is.infinite(log.lik.prop))
            return(log.lik.prop)
        log.lik.curr <- logLikelihood(model = model,
                                      count = total.comp.curr,
                                      dataset = dataset,
                                      i = i.after)
        log.lik.prop - log.lik.curr
    }
}
        



## LOG DENSITY ################################################################

diffLogDensAccountMove <- function(combined, useC = FALSE) {
    stopifnot(is(combined, "CombinedAccountMove"))
    if (useC) {
        .Call(diffLogDensAccountMove_R, combined)
    }
    else {
        i.comp <- combined@iComp
        i.orig.dest <- combined@iOrigDest
        i.pool <- combined@iPool
        i.net <- combined@iNet
        uses.exposure <- combined@usesExposure
        ans <- diffLogDensPopn(combined)
        if (i.comp == i.orig.dest) {
            if (uses.exposure[i.comp])
                ans <- ans + diffLogDensJumpOrigDest(combined)
            ans <- ans + diffLogDensExpOrigDestPool(combined)
        }
        else if (i.comp == i.pool) {
            if (uses.exposure[i.comp])
                ans <- ans + diffLogDensJumpPoolWithExpose(combined)
            else
                ans <- ans + diffLogDensJumpPoolNoExpose(combined)
            ans <- ans + diffLogDensExpOrigDestPool(combined)
        }
        else if (i.comp == i.net) {
            ans <- ans + diffLogDensJumpNet(combined)
            ans <- ans + diffLogDensExpNet(combined)
        }
        else {
            if (uses.exposure[i.comp])
                ans <- ans + diffLogDensJumpComp(combined)
            ans <- ans + diffLogDensExpComp(combined)
        }
        ans
    }
}

diffLogDensPopn <- function(combined, useC = FALSE) {
    stopifnot(is(combined, "CombinedAccountMove"))
    if (useC) {
        .Call(diffLogDensAccountMove_R, combined)
    }
    else {
        account <- combined@account
        population <- account@population
        iterator <- combined@iteratorPopn
        theta <- combined@system@model@theta
        i.popn <- combined@iPopn
        i.popn.oth <- combined@iPopnOth
        diff <- combined@diffProp
        i.comp <- combined@iComp
        i.orig.dest <- combined@iOrigDest
        i.pool <- combined@iPool
        i.net <- combined@iNet
        is.increment <- combined@isIncrement
        update.two.cohorts <- ((i.comp == i.orig.dest)
                               || (i.comp == i.pool)
                               || (i.comp == i.net))
        if (update.two.cohorts) {
            ans.orig <- diffLogDensPopnOneCohort(diff = -diff,
                                                 population = population,
                                                 i = i.popn,
                                                 iterator = iterator,
                                                 theta = theta)
            ans.dest <- diffLogDensPopnOneCohort(diff = diff,
                                                 population = population,
                                                 i = i.popn.oth,
                                                 iterator = iterator,
                                                 theta = theta)
            ans.orig + ans.dest
        }
        else {
            if (!is.increment[i.comp])
                diff <- -diff
            diffLogDensPopnOneCohort(diff = diff,
                                     population = population,
                                     i = i.popn,
                                     iterator = iterator,
                                     theta = theta)
        }
    }
}

diffLogDensPopnOneCohort <- function(diff, population, i, iterator, theta) {
    ## diff
    stopifnot(identical(length(diff), 1L))
    stopifnot(is.integer(diff))
    stopifnot(!is.na(diff))
    ## population
    stopifnot(is(population, "Population"))
    stopifnot(is.integer(population))
    stopifnot(!any(is.na(population)))
    stopifnot(all(population >= 0L))
    ## i
    stopifnot(identical(length(i), 1L))
    stopifnot(is.integer(i))
    stopifnot(!is.na(i))
    stopifnot(i >= 0L)
    ## iterator
    stopifnot(is(iterator, "CohortIteratorAccessionPopulation"))
    ## theta
    stopifnot(is(theta, "double"))
    stopifnot(!any(is.na(theta)))
    stopifnot(all(theta >= 0))
    ## population and theta
    stopifnot(identical(length(population), length(theta)))
    if (useC) {
        .Call(diffLogDensAccountMove_R, combined)
    }
    iterator <- resetCAP(iterator, i = i)
    ans <- 0
    repeat {
        i <- iterator@i
        val.curr <- population[i]
        val.prop <- val.curr + diff
        lambda <- theta[i]
        log.dens.prop <- dpois(x = val.prop, lambda = lambda, log = TRUE)
        log.dens.curr <- dpois(x = val.curr, lambda = lambda, log = TRUE)
        ans <- ans + log.dens.prop - log.dens.curr
        if (iterator@finished)
            break
        iterator <- advanceCAP(iterator)
    }
    ans
}

diffLogDensJumpOrigDest <- function(combined, useC = FALSE) {
    stopifnot(is(combined, "CombinedAccountMove"))
    if (useC) {
        .Call(diffLogDensJumpOrigDest_R, combined)
    }
    else {
        i.comp <- combined@iComp
        account <- combined@account
        component <- account@components[[i.comp]]
        system <- combined@system
        sys.mod <- system[[i.comp + 1L]]
        theta <- sys.mod@theta
        expose <- combined@exposure
        expected.expose <- combined@expectedExposure
        i.cell <- combined@iCell
        i.expose <- combined@iExposure
        is.lower.triangle <- combined@isLowerTriangle
        diff <- combined@diffProp
        theta.cell <- theta[i.cell]
        expose.cell.curr <- expose[i.expose]
        expose.cell.oth.curr <- expose[i.expose.oth]
        if (is.lower.triangle)
            expose.cell.prop <- expose.cell.curr - 0.5 * diff
        else
            expose.cell.prop <- expose.cell.curr
        expose.cell.jump <- expected.expose[i.expose]
        lambda.dens.prop <- theta.cell * expose.cell.prop
        lambda.dens.curr <- theta.cell * expose.cell.curr
        lambda.jump <- theta.cell * expose.cell.jump
        val.curr <- component[i.cell]
        val.prop <- val.curr + diff
        diff.log.dens <- (dpois(x = val.prop, lambda = lambda.dens.prop, log = TRUE)
                          - dpois(x = val.curr, lambda = lambda.dens.curr, log = TRUE))
        diff.log.jump <- (dpois(x = val.curr, lambda = lambda.jump, log = TRUE)
                          - dpois(x = val.prop, lambda = lambda.jump, log = TRUE))
        diff.log.dens + diff.log.jump
    }
}

diffLogDensJumpPoolWithExpose <- function(combined, useC = FALSE) {
    stopifnot(is(combined, "CombinedAccountMove"))
    if (useC) {
        .Call(diffLogDensJumpPoolWithExpose_R, combined)
    }
    else {
        i.comp <- combined@iComp
        account <- combined@account
        component <- account@components[[i.comp]]
        system <- combined@system
        sys.mod <- system[[i.comp + 1L]]
        theta <- sys.mod@theta
        expose <- combined@exposure
        expected.expose <- combined@expectedExposure
        i.cell.out <- combined@iCell
        i.cell.in <- combined@iCellOther
        i.expose.out <- combined@iExposure
        i.expose.in <- combined@iExposureOther
        is.lower.triangle <- combined@isLowerTriangle
        diff <- combined@diffProp
        theta.out <- theta[i.cell.out]
        theta.in <- theta[i.cell.in]
        expose.out.curr <- expose[i.expose.out]
        expose.in.curr <- expose[i.expose.in]
        if (is.lower.triangle) {
            expose.out.prop <- expose.out.curr - 0.5 * diff
            expose.in.prop <- expose.in.curr + 0.5 * diff
        }
        else {
            expose.out.prop <- expose.out.curr
            expose.in.prop <- expose.in.curr
        }
        expose.jump <- expected.expose[i.expose.out]
        lambda.dens.out.prop <- theta.out * expose.out.prop
        lambda.dens.in.prop <- theta.in * expose.in.prop
        lambda.dens.out.curr <- theta.out * expose.out.curr
        lambda.dens.in.curr <- theta.in * expose.in.curr
        lambda.jump <- theta.out * expose.jump
        val.out.curr <- component[i.cell.out]
        val.in.curr <- component[i.cell.in]
        val.out.prop <- val.out.curr + diff
        val.in.prop <- val.in.curr - diff
        diff.log.dens <- (dpois(x = val.out.prop, lambda = lambda.dens.out.prop, log = TRUE)
                          - dpois(x = val.curr, lambda = lambda.dens.out.curr, log = TRUE)
                          + dpois(x = val.in.prop, lambda = lambda.dens.in.prop, log = TRUE)
                          - dpois(x = val.in.curr, lambda = lambda.dens.in.curr, log = TRUE))
        diff.log.jump <- (dpois(x = val.out.curr, lambda = lambda.jump, log = TRUE)
                          - dpois(x = val.out.prop, lambda = lambda.jump, log = TRUE))
        diff.log.dens + diff.log.jump
    }
}

diffLogDensJumpPoolNoExpose <- function(combined, useC = FALSE) {
    stopifnot(is(combined, "CombinedAccountMove"))
    if (useC) {
        .Call(diffLogDensJumpPoolNoExpose_R, combined)
    }
    else {
        i.comp <- combined@iComp
        account <- combined@account
        component <- account@components[[i.comp]]
        system <- combined@system
        sys.mod <- system[[i.comp + 1L]]
        theta <- sys.mod@theta
        expose <- combined@exposure
        i.cell.in <- combined@iCellOther
        i.expose.in <- combined@iExposureOther
        is.lower.triangle <- combined@isLowerTriangle
        diff <- combined@diffProp
        theta.in <- theta[i.cell.in]
        expose.in.curr <- expose[i.expose.in]
        if (is.lower.triangle)
            expose.in.prop <- expose.in.curr + 0.5 * diff
        else
            expose.in.prop <- expose.in.curr
        lambda.dens.in.prop <- theta.in * expose.in.prop
        lambda.dens.in.curr <- theta.in * expose.in.curr
        val.in.curr <- component[i.cell.in]
        val.in.prop <- val.in.curr - diff
        dpois(x = val.in.prop, lambda = lambda.dens.in.prop, log = TRUE)
        - dpois(x = val.in.curr, lambda = lambda.dens.in.curr, log = TRUE)
    }
}

diffLogDensJumpNet <- function(combined, useC = FALSE) {
    stopifnot(is(combined, "CombinedAccountMove"))
    if (useC) {
        .Call(diffLogDensJumpNet_R, combined)
    }
    else {
        i.comp <- combined@iComp
        account <- combined@account
        component <- account@components[[i.comp]]
        system <- combined@system
        sys.mod <- system[[i.comp + 1L]]
        theta <- sys.mod@theta
        varsigma <- sys.mod@varsigma
        w <- sys.mod@w
        i.cell.in <- combined@iCellOther
        diff <- combined@diffProp
        mean.in <- theta[i.cell.in]
        w.in <- w[i.cell.in]
        sd.in <- varsigma / w.in
        val.in.curr <- component[i.cell.in]
        val.in.prop <- val.in.curr - diff
        dnorm(x = val.in.prop, mean = mean.in, sd = sd.in, log = TRUE)
        - dnorm(x = val.in.curr, mean = mean.in, sd = sd.in, log = TRUE)
    }
}


diffLogDensJumpComp <- function(combined, useC = FALSE) {
    stopifnot(is(combined, "CombinedAccountMove"))
    if (useC) {
        .Call(diffLogDensJumpAccountMoveComp_R, combined)
    }
    else {
        i.comp <- combined@iComp
        account <- combined@account
        component <- account@components[[i.comp]]
        system <- combined@system
        sys.mod <- system[[i.comp + 1L]]
        theta <- sys.mod@theta
        expose <- combined@exposure
        expected.expose <- combined@expectedExposure
        i.cell <- combined@iCell
        i.expose <- combined@iExposure
        is.lower.triangle <- combined@isLowerTriangle
        diff <- combined@diffProp
        is.increment <- combined@isIncrement
        theta.cell <- theta[i.cell]
        expose.cell.curr <- expose[i.expose]
        if (is.lower.triangle) {
            if (is.increment[i.comp])
                expose.cell.prop <- expose.cell.curr + 0.5 * diff
            else
                expose.cell.prop <- expose.cell.curr - 0.5 * diff
        }
        else
            expose.cell.prop <- expose.cell.curr
        expose.cell.jump <- expected.expose[i.expose]
        lambda.dens.prop <- theta.cell * expose.cell.prop
        lambda.dens.curr <- theta.cell * expose.cell.curr
        lambda.jump <- theta.cell * expose.cell.jump
        val.curr <- component[i.cell]
        val.prop <- val.curr + diff
        diff.log.dens <- (dpois(x = val.prop, lambda = lambda.dens.prop, log = TRUE)
                          - dpois(x = val.curr, lambda = lambda.dens.curr, log = TRUE))
        diff.log.jump <- (dpois(x = val.curr, lambda = lambda.jump, log = TRUE)
                          - dpois(x = val.prop, lambda = lambda.jump, log = TRUE))
        diff.log.dens + diff.log.jump
    }
}

diffLogDensExpOrigDestPool <- function(combined, useC = FALSE) {
    stopifnot(is(combined, "CombinedAccountMove"))
    if (useC) {
        .Call(diffLogDensExpOrigDestPool_R, combined)
    }
    else {
        account <- combined@account
        components <- account@components
        iterators.comp <- combined@iteratorsComp
        exposure <- combined@exposure
        uses.exposure <- combined@usesExposure
        mappings <- combined@mappingsFromExposure
        i.exp.orig <- combined@iExpFirst
        i.exp.dest <- combined@iExpFirstOth
        iterator.exposure <- combined@iteratorExposure
        diff <- combined@diffProp
        system <- combined@system
        ans <- 0
        for (i in seq_along(components)) {
            if (uses.exposure[i]) {
                component <- components[[i]]
                theta <- system[[i + 1L]]@theta
                iterator.comp <- iterators.comp[[i]]
                mapping <- mappings[[i]]
                i.cell.orig <- getICellCompFromExp(mapping = mapping,
                                                   i = i.exp.orig)
                i.cell.dest <- getICellCompFromExp(mapping = mapping,
                                                   i = i.exp.dest)
                ans.orig <- diffLogDensExpComp(iCell = i.cell.orig,
                                               component = component,
                                               theta = theta,
                                               iteratorComp = iterator.comp,
                                               iExpFirst = i.exp.orig,
                                               exposure = exposure,
                                               iteratorExposure = iterator.exposure,
                                               diff = -diff)
                if (is.infinite(ans.orig))
                    return(ans.orig)
                ans.dest <- diffLogDensExpComp(iCell = i.cell.dest,
                                               component = component,
                                               theta = theta,
                                               iteratorComp = iterator.comp,
                                               iExpFirst = i.exp.dest,
                                               exposure = exposure,
                                               iteratorExposure = iterator.exposure,
                                               diff = diff)
                if (is.infinite(ans.dest))
                    return(ans.dest)
                ans <- ans.orig + ans.dest
            }
        }
        ans
    }
}

diffLogDensExpNet <- function(combined, useC = FALSE) {
    stopifnot(is(combined, "CombinedAccountMove"))
    if (useC) {
        .Call(diffLogDensExpNet_R, combined)
    }
    else {
        account <- combined@account
        components <- account@components
        iterators.comp <- combined@iteratorsComp
        exposure <- combined@exposure
        uses.exposure <- combined@usesExposure
        mappings <- combined@mappingsFromExposure
        i.exp.1 <- combined@iExpFirst
        i.exp.2 <- combined@iExpFirstOth
        iterator.exposure <- combined@iteratorExposure
        diff <- combined@diffProp
        system <- combined@system
        ans <- 0
        for (i in seq_along(components)) {
            if (uses.exposure[i]) {
                component <- components[[i]]
                theta <- system[[i + 1L]]@theta
                iterator.comp <- iterators.comp[[i]]
                mapping <- mappings[[i]]
                i.cell.1 <- getICellCompFromExp(mapping = mapping, i = i.exp.1)
                i.cell.2 <- getICellCompFromExp(mapping = mapping, i = i.exp.2)
                ans.1 <- diffLogDensExpComp(iCell = i.cell.1,
                                            component = component,
                                            theta = theta,
                                            iteratorComp = iterator.comp,
                                            iExpFirst = i.exp.1,
                                            exposure = exposure,
                                            iteratorExposure = iterator.exposure,
                                            diff = diff)
                if (is.infinite(ans.1))
                    return(ans.1)
                ans.2 <- diffLogDensExpComp(iCell = i.cell.2,
                                            component = component,
                                            theta = theta,
                                            iteratorComp = iterator.comp,
                                            iExpFirst = i.exp.2,
                                            exposure = exposure,
                                            iteratorExposure = iterator.exposure,
                                            diff = -diff)
                if (is.infinite(ans.2))
                    return(ans.2)
                ans <- ans.1 + ans.2
            }
        }
        ans
    }
}

diffLogDensExpComp <- function(combined, useC = FALSE) {
    stopifnot(is(combined, "CombinedAccountMove"))
    if (useC) {
        .Call(diffLogDensExpComp_R, combined)
    }
    else {
        account <- combined@account
        components <- account@components
        iterators.comp <- combined@iteratorsComp
        uses.exposure <- combined@usesExposure
        mappings <- combined@mappingsFromExposure
        i.exp.first <- combined@iExpFirst
        iterator.exposure <- combined@iteratorExposure
        is.increment <- combined@isIncrement
        diff <- combined@diffProp
        i.comp <- combined@iComp
        i.orig.dest <- combined@iOrigDest
        i.pool <- combined@iPool
        system <- combined@system
        ans <- 0
        if (is.increment[i.comp])
            diff <- -diff
        for (i in seq_along(components)) {
            if (uses.exposure[i]) {
                component <- components[[i]]
                theta <- system[[i + 1L]]@theta
                iterator.comp <- iterators.comp[[i]]
                mapping <- mappings[[i]]
                if (i.comp == i.orig.dest) {
                    i.cell <- getICellOrigDestFromExp(i = i.exp.first,
                                                      mapping = mapping)
                    diff.log <- diffLogDensExpOneOrigDestPool(iCell = i.cell,
                                                              component = component,
                                                              theta = theta,
                                                              iteratorComp = iterator.comp,
                                                              iExpFirst = i.exp.first,
                                                              exposure = exposure,
                                                              iteratorExposure = iterator.exposure,
                                                              diff = diff)
                    if (is.infinite(diff.log))
                        return(diff.log)
                    ans <- ans + diff.log
                }
                if (i.comp == i.pool) {
                    i.cell <- getICellPoolFromExp(i = i.exp.first,
                                                  mapping = mapping)
                    ans.one <- diffLogDensExpOneOrigDestPool(iCell = i.cell,
                                                             component = component,
                                                             theta = theta,
                                                             iteratorComp = iterator.comp,
                                                             iExpFirst = i.exp.first,
                                                             exposure = exposure,
                                                             iteratorExposure = iterator.exposure,
                                                             diff = diff)
                    if (is.infinite(ans.one))
                        return(diff.log)
                    ans <- ans + ans.one
                }
                else {
                    i.cell <- getICellCompFromExpose(mapping = mapping,
                                                     i = i.exp.first)
                    ans.one <- diffLogDensExpOneComp(iCell = i.cell,
                                                     component = component,
                                                     theta = theta,
                                                     iteratorComp = iterator.comp,
                                                     iExpFirst = i.exp.first,
                                                     exposure = exposure,
                                                     iteratorExposure = iterator.exposure,
                                                     diff = diff)
                    if (is.infinite(ans.one))
                        return(diff.log)
                    ans <- ans + ans.one
                }
            }
        }
        ans
    }
}




diffLogDensExpOneOrigDestParChPool <- function(iCell, component, theta, iteratorComp, 
                                          iExpFirst, exposure, iteratorExposure,
                                          diff, useC = FALSE) {
    ## iCell
    stopifnot(identical(length(iCell), 1L))
    stopifnot(is.integer(iCell))
    stopifnot(!is.na(iCell))
    stopifnot(iCell > 0L)
    ## component
    stopifnot(is(component, "InternalMovementsOrigDest")
              || is(component, "InternalMovementsPool"))
    ## theta
    stopifnot(is(theta, "Values"))
    stopifnot(is.double(theta))
    stopifnot(!any(is.na(theta)))
    stopifnot(all(theta >= 0))
    ## iteratorComp
    stopifnot(is(iterator, "CohortIteratorOrigDestParChPool"))
    ## iExpFirst
    stopifnot(identical(length(iExpFirst), 1L))
    stopifnot(is.integer(iExpFirst))
    stopifnot(!is.na(iExpFirst))
    stopifnot(iExpFirst > 0L)
    ## theta
    stopifnot(is(exposure, "Values"))
    stopifnot(is.double(exposure))
    stopifnot(!any(is.na(exposure)))
    stopifnot(all(exposure >= 0))
    ## iteratorExposure
    stopifnot(is(iterator, "CohortIteratorComponent"))
    ## diff
    stopifnot(identical(length(diff), 1L))
    stopifnot(is.integer(diff))
    stopifnot(!is.na(diff))
    ## iCell and component
    stopifnot(iCell <= length(component))
    ## iExpFirst and exposure
    stopifnot(iExpFirst <= length(exposure))
    ## component and theta
    stopifnot(identical(length(component), length(theta)))
    if (useC) {
        .Call(diffLogDensExpOneOrigDestParChPool_R,
              iCell, component, theta, iteratorComp,
              iExpFirst, exposure, iteratorExposure,
              diff)
    }
    else {    
        kToleranceExposure <- 1e-5 ## in C can use a macro
        iteratorComp <- resetCODP(iteratorComp, i = iCell)
        iteratorExposure <- resetCC(iteratorExposure, i = iExpFirst)
        ans <- 0
        diff.expose <- 0.5 * diff
        lengthVec <- iterator@lengthVec
        if (iteratorComp@iTriangle == 1L) {
            i.e <- iteratorExposure@i
            i.c.vec <- iteratorComp@iVec
            expose.curr <- exposure[i.e]
            expose.prop <- expose.curr + diff.expose
            diff.log.expose <- log(expose.prop) - log(expose.curr)
            for (j in seq_len(lengthVec)) {
                i.c <- i.c.vec[j]
                comp.curr <- component[i.c]
                if ((expose.prop < kToleranceExposure) && (comp.corr > 0L))
                    return(-Inf)
                theta.curr <- theta[i.c]
                ans <- ans - theta.curr * diff.expose + comp.curr * diff.log.expose
            }
        }
        while (!iteratorComp@finished) {
            iteratorComp <- advanceCODP(iteratorComp)
            iteratorExposure <- advanceCC(iteratorExposure)
            i.e <- iteratorExposure@i
            i.c.vec <- iteratorComp@iVec
            expose.curr <- exposure[i.e]
            expose.prop <- expose.curr + diff.expose
            diff.log.expose <- log(expose.prop) - log(expose.curr)
            for (j in seq_len(lengthVec)) {
                i.c <- i.c.vec[j]
                comp.curr <- component[i.c]
                if ((expose.prop < kToleranceExposure) && (comp.corr > 0L))
                    return(-Inf)
                theta.curr <- theta[i.c]
                ans <- ans - theta.curr * diff.expose + comp.curr * diff.log.expose
            }
        }
        ans
    }
}

diffLogDensExpOneComp <- function(iCell, component, theta, iteratorComp, 
                                  iExpFirst, exposure, iteratorExposure,
                                  diff, useC = FALSE) {
    ## iCell
    stopifnot(identical(length(iCell), 1L))
    stopifnot(is.integer(iCell))
    stopifnot(!is.na(iCell))
    stopifnot(iCell > 0L)
    ## component
    stopifnot(is(component, "Component"))
    ## theta
    stopifnot(is(theta, "Values"))
    stopifnot(is.double(theta))
    stopifnot(!any(is.na(theta)))
    stopifnot(all(theta >= 0))
    ## iteratorComp
    stopifnot(is(iterator, "CohortIteratorComponent"))
    ## iExpFirst
    stopifnot(identical(length(iExpFirst), 1L))
    stopifnot(is.integer(iExpFirst))
    stopifnot(!is.na(iExpFirst))
    stopifnot(iExpFirst > 0L)
    ## theta
    stopifnot(is(exposure, "Values"))
    stopifnot(is.double(exposure))
    stopifnot(!any(is.na(exposure)))
    stopifnot(all(exposure >= 0))
    ## iteratorExposure
    stopifnot(is(iterator, "CohortIteratorComponent"))
    ## diff
    stopifnot(identical(length(diff), 1L))
    stopifnot(is.integer(diff))
    stopifnot(!is.na(diff))
    ## iCell and component
    stopifnot(iCell <= length(component))
    ## iExpFirst and exposure
    stopifnot(iExpFirst <= length(exposure))
    ## component and theta
    stopifnot(identical(length(component), length(theta)))
    if (useC) {
        .Call(diffLogDensExpOneComp_R,
              iCell, component, theta, iteratorComp,
              iExpFirst, exposure, iteratorExposure,
              diff)
    }
    else {    
        kToleranceExposure <- 1e-5 ## in C can use a macro
        iteratorComp <- resetCC(iteratorComp, i = iCell)
        iteratorExposure <- resetCC(iteratorExposure, i = iExpFirst)
        ans <- 0
        diff.expose <- 0.5 * diff
        if (iteratorComp@iTriangle == 1L) {
            i.e <- iteratorExposure@i
            i.c <- iteratorComp@i
            comp.curr <- component[i.c]
            expose.curr <- exposure[i.e]
            expose.prop <- expose.curr + diff.expose
            if ((expose.prop < kToleranceExposure) && (comp.corr > 0L))
                return(-Inf)
            diff.log.expose <- log(expose.prop) - log(expose.curr)
            theta.curr <- theta[i.c]
            ans <- ans - theta.curr * diff.expose + comp.curr * diff.log.expose
        }
        while (!iteratorComp@finished) {
            iteratorComp <- advanceCC(iteratorComp)
            iteratorExposure <- advanceCC(iteratorExposure)
            i.c <- iteratorComp@i
            i.e <- iteratorExposure@i
            comp.curr <- component[i.c]
            expose.curr <- exposure[i.e]
            expose.prop <- expose.curr + diff.expose
            if ((expose.prop < kToleranceExposure) && (comp.curr > 0L))
                return(-Inf)
            diff.log.expose <- log(expose.prop) - log(expose.curr)
            theta.curr <- theta[i.c]
            ans <- ans - theta.curr * diff.expose + comp.curr * diff.log.expose
        }
        ans
    }
}
    

## UPDATE VALUES ################################################################

updateValuesAccountMove <- function(combined) {
    combined <- updateCellAccountMove(combined)
    combined <- updateSubsequentPopnAccountMove(combined)
    combined <- updateExposure(combined)
    combined <- updateSubsequentAccession(combined)
    combined
}

updateCellAccountMove <- function(combined) {
    i.comp <- combined@iComp
    i.cell <- combined@iCell
    diff <- combined@diffProp
    is.popn <- i.comp == 0L
    if (is.popn)
        combined@account@population[i.cell] <-
            (combined@account@population[i.cell] 
             + diff)
    else {
        combined@account@components[[i.comp]][i.cell] <-
            (combined@account@components[[i.comp]][i.cell]
             + diff)
        i.pool <- combined@iPool
        i.net <- combined@iNet
        update.two.cells <- (i.comp == i.pool) || (i.comp == i.net)
        if (update.two.cells) {
            i.cell.other <- combined@iCellOther
            if (i.comp == i.pool)
                combined@account@components[[i.comp]][i.cell.other] <-
                    (combined@account@components[[i.comp]][i.cell.other]
                     + diff)
            if (i.comp == i.net)
                combined@account@components[[i.comp]][i.cell.other] <-
                    (combined@account@components[[i.comp]][i.cell.other]
                     - diff)
        }
    }
    combined
}

## there is always at least one subsequent population value to update
updateSubsequentPopnAccountMove <- function(combined) {
    i.comp <- combined@iComp
    i.orig.dest <- combined@iOrigDest
    i.pool <- combined@iPool
    i.net <- combined@iNet
    i.popn.next <- combined@iPopnNext
    i.popn.next.other <- combined@iPopnNextOther
    iterator <- combined@iteratorPopn
    is.popn <- i.comp == 0L
    if (is.popn) {
        iterator <- resetCAP(iterator, i = i.popn.next)
        repeat {
            i <- iterator@i
            combined@account@population[i] <- 
                (combined@account@population[i]
                 + diff)
            if (iterator@finished)
                break
            iterator <- advanceCAP(iterator)
        }
    }
    else {
        is.orig.dest <- i.comp == i.orig.dest
        is.pool <- i.comp == i.pool
        is.net <- i.comp == i.net
        update.two.cohorts <- (is.orig.dest || is.pool || is.net)
        if (update.two.cohorts) {
            if (is.orig.dest || is.pool) {
                diff.orig <- -diff
                diff.dest <- diff
            }
            else {
                diff.orig <- diff
                diff.dest <- -diff
            }
            iterator.orig <- resetCAP(iterator, i = i.popn.next)
            iterator.dest <- resetCAP(iterator, i = i.popn.next.other)
            repeat {
                i.orig <- iterator.orig@i
                i.dest <- iterator.dest@i
                combined@account@population[i.orig] <- 
                    (combined@account@population[i.orig]
                     + diff.orig)
                combined@account@population[i.dest] <- 
                    (combined@account@population[i.dest]
                     + diff.dest)
                if (iterator.orig@finished)
                    break
                iterator.orig <- advanceCAP(iterator.orig)
                iterator.dest <- advanceCAP(iterator.dest)
            }
        }
        else {
            if (!is.increment[i.comp])
                diff <- -diff
            iterator <- resetCAP(iterator, i = i.popn.next)
            repeat {
                i <- iterator@i
                combined@account@population[i] <- 
                    (combined@account@population[i]
                     + diff)
                if (iterator@finished)
                    break
                iterator <- advanceCAP(iterator)
            }
        }
    }
    combined
}


## updateExposure <- function(combined) {
##     i.comp <- combined@iComp
##     i.orig.dest <- combined@iOrigDest
##     i.pool <- combined@iPool
##     i.net <- combined@iNet
##     i.cell <- combined@iCell
##     i.cell.other <- combined@iCellOther
##     is.popn <- i.comp == 0L {
##         if (is.popn) {
            
    
##     if (is
##     i.exposure <- getIExposure(i = i.cell, mapping = mapping[[i.comp]])
    
##     is.orig.dest <- i.comp == i.orig.dest
##     is.pool <- i.comp == i.pool
##     is.net <- i.comp == i.net
##     update.two.cohorts <- (is.orig.dest || is.pool || is.net)
##     if (update.two.cohorts) {
        
##     update.two.cohorts <- 
##         mappings <- combined@mappingsToExposure
    
##     is.lower.triangle <- combined@isLowerTriangle
##     i
##     has.age <- combined@has.age
##     if (!has.age)
##         return(combined)
##     i.acc.next <- combined@iAccNext
##     no.values.to.update <- i.acc.next == 0L # final period, upper triangle
##     if (no.values.to.update)
##         return(combined)
##     i.comp <- combined@iComp
##     i.orig.dest <- combined@iOrigDest
##     i.pool <- combined@iPool
##     i.net <- combined@iNet
##     i.acc.next.other <- combined@iAccNextOther
##     iterator <- combined@iteratorAcc
##     is.popn <- i.comp == 0L
##     if (is.popn) {
##         iterator <- resetCAP(iterator, i = i.acc.next)
##         repeat {
##             i <- iterator@i
##             combined@account@population[i] <- 
##                 (combined@account@population[i]
##                  + diff)
##             if (iterator@finished)
##                 break
##             iterator <- advanceCAP(iterator)
##         }
##     }
##     ## if final period and lower triangle, there are no
##     ## subsequent accession values to update
##     else { 
##         is.orig.dest <- i.comp == i.orig.dest
##         is.pool <- i.comp == i.pool
##         is.net <- i.comp == i.net
##         update.two.cohorts <- (is.orig.dest || is.pool || is.net)
##         if (update.two.cohorts) {
##             if (is.orig.dest || is.pool) {
##                 diff.orig <- -diff
##                 diff.dest <- diff
##             }
##             else {
##                 diff.orig <- diff
##                 diff.dest <- -diff
##             }
##             iterator.orig <- resetCAP(iterator, i = i.acc.next)
##             iterator.dest <- resetCAP(iterator, i = i.acc.next.other)
##             repeat {
##                 i.orig <- iterator.orig@i
##                 i.dest <- iterator.dest@i
##                 combined@account@population[i.orig] <- 
##                     (combined@account@population[i.orig]
##                      + diff.orig)
##                 combined@account@population[i.dest] <- 
##                     (combined@account@population[i.dest]
##                      + diff.dest)
##                 if (iterator.orig@finished)
##                     break
##                 iterator.orig <- advanceCAP(iterator.orig)
##                 iterator.dest <- advanceCAP(iterator.dest)
##             }
##         }
##         else {
##             if (!is.increment[i.comp])
##                 diff <- -diff
##             iterator <- resetCAP(iterator, i = i.acc.next)
##             repeat {
##                 i <- iterator@i
##                 combined@account@population[i] <- 
##                     (combined@account@population[i]
##                      + diff)
##                 if (iterator@finished)
##                     break
##                 iterator <- advanceCAP(iterator)
##             }
##         }
##     }
##     combined
## }

updateSubsequentAccession <- function(combined) {
    has.age <- combined@hasAage
    if (!has.age)
        return(combined)
    i.acc.next <- combined@iAccNext
    no.values.to.update <- i.acc.next == 0L # final period, upper triangle
    if (no.values.to.update)
        return(combined)
    i.comp <- combined@iComp
    i.orig.dest <- combined@iOrigDest
    i.pool <- combined@iPool
    i.net <- combined@iNet
    i.acc.next.other <- combined@iAccNextOther
    iterator <- combined@iteratorAcc
    is.popn <- i.comp == 0L
    if (is.popn) {
        iterator <- resetCAP(iterator, i = i.acc.next)
        repeat {
            i <- iterator@i
            combined@account@population[i] <- 
                (combined@account@population[i]
                 + diff)
            if (iterator@finished)
                break
            iterator <- advanceCAP(iterator)
        }
    }
    else { 
        is.orig.dest <- i.comp == i.orig.dest
        is.pool <- i.comp == i.pool
        is.net <- i.comp == i.net
        update.two.cohorts <- (is.orig.dest || is.pool || is.net)
        if (update.two.cohorts) {
            if (is.orig.dest || is.pool) {
                diff.orig <- -diff
                diff.dest <- diff
            }
            else {
                diff.orig <- diff
                diff.dest <- -diff
            }
            iterator.orig <- resetCAP(iterator, i = i.acc.next)
            iterator.dest <- resetCAP(iterator, i = i.acc.next.other)
            repeat {
                i.orig <- iterator.orig@i
                i.dest <- iterator.dest@i
                combined@account@population[i.orig] <- 
                    (combined@account@population[i.orig]
                     + diff.orig)
                combined@account@population[i.dest] <- 
                    (combined@account@population[i.dest]
                     + diff.dest)
                if (iterator.orig@finished)
                    break
                iterator.orig <- advanceCAP(iterator.orig)
                iterator.dest <- advanceCAP(iterator.dest)
            }
        }
        else {
            if (!is.increment[i.comp])
                diff <- -diff
            iterator <- resetCAP(iterator, i = i.acc.next)
            repeat {
                i <- iterator@i
                combined@account@population[i] <- 
                    (combined@account@population[i]
                     + diff)
                if (iterator@finished)
                    break
                iterator <- advanceCAP(iterator)
            }
        }
    }
    combined
}




updateSystem <- function(combined) {
    system <- combined@system
    account <- combined@account
    population <- account@population
    components <- account@components
    mappings <- combined@mappingsToPopn
    has.age <- combined@hasAge
    model.popn <- system[[1L]]
    model.popn <- updateModelNotUseExp(model.popn, y = population)
    system[[1L]] <- model.popn
    for (i in seq_along(component)) {
        model.comp <- system[[i + 1L]]
        component <- components[[i]]
        mapping <- mappings[[i]]
        uses.expose <- !is.null(transform)
        if (uses.expose) {
            if (has.age)
                exposure <- makeExposureAge(population = population,
                                            mapping = mapping)
            else
                exposure <- makeExposureNoAge(population = population,
                                              mapping = mapping)
            model.comp <- updateModelUseExp(model = model.comp,
                                            y = component,
                                            exposure = exposure)
        }
        else
            model.comp <- updateModelNotUseExp(model = model.comp,
                                               y = component)
        system[[i + 1L]] <- model.comp
    }
    combined@system <- system
    combined
}

updateObservation <- function(combined) {
    observation <- combined@observation
    account <- combined@account
    population <- account@population
    components <- account@components
    series.indices <- combined@seriesIndices
    transforms <- combined@transformsDatasets
    for (i in seq_along(observation)) {
        model <- observation[[i]]
        dataset <- datasets[[i]]
        transform <- transforms[[i]]
        if (i == 1L)
            series <- population
        else
            series <- components[[i + 1L]]
        series.collapsed <- collapse(series, transform = transform)
        if (is(model, "Poisson"))
            series.collapsed <- toDouble(series.collapsed)
        model <- updateModelUseExp(model, y = dataset, exposure = series.collapsed)
        observation[[i]] <- model
    }
    combined@observation <- observation
    combined
}

## HELPER FUNCTIONS ################################################

getIExpFirstFromOrigDest <- function(i, mapping) {
    n.shared.vec <- mapping@nSharedVec
    step.shared.comp.vec <- mapping@stepSharedCurrentVec
    step.shared.popn.vec <- mapping@stepSharedTargetVec
    n.orig.dest.vec <- mapping@nOrigDestVec
    step.orig.vec <- mapping@stepOrigCurrentVec
    step.dest.vec <- mapping@stepDestCurrentVec
    step.orig.dest.vec <- mapping@stepOrigDestTargetVec    
    n.dim.shared <- length(n.shared.vec)
    n.dim.orig.dest <- length(n.orig.dest.vec)
    i.exp.orig <- 1L
    for (d in seq_len(n.dim.shared)) {
        n.shared <- n.shared.vec[d]
        step.shared.comp <- step.shared.comp.vec[d]
        step.shared.popn <- step.shared.popn.vec[d]
        i.shared <- ((i - 1L) %/% step.shared.comp) %% n.shared # C-style
        i.exp.orig <- i.exp.orig + i.shared * step.shared.popn
    }
    i.exp.dest <- i.exp.orig
    for (d in seq_len(n.dim.orig.dest)) {
        n.orig.dest <- n.orig.dest.vec[d]
        step.orig <- step.orig.vec[d]
        step.dest <- step.dest.vec[d]
        i.orig <- ((i - 1L) %/% step.orig) %% n.orig.dest # C-style
        i.dest <- ((i - 1L) %/% step.dest) %% n.orig.dest # C-style
        i.exp.orig <- i.exp.orig + i.orig * step.orig.dest
        i.exp.dest <- i.exp.dest + i.dest * step.orig.dest
    }
    c(i.exp.orig, i.exp.dest)
}


getIExpFirstFromComp <- function(i, mapping) {
    n.shared.vec <- mapping@nSharedVec
    step.shared.comp.vec <- mapping@stepSharedCurrentVec
    step.shared.exp.vec <- mapping@stepSharedTargetVec
    n.dim.shared <- length(n.shared.vec)
    i.exp <- 1L # R-style
    for (d in seq_len(n.dim.shared)) {
        n.shared <- n.shared.vec[d]
        step.shared.comp <- step.shared.comp.vec[d]
        step.shared.exp <- step.shared.exp.vec[d]
        i.shared <- ((i - 1L) %/% step.shared.comp) %% n.shared # C-style
        i.exp <- i.exp + i.shared * step.shared.exp # R-style
    }
    i.exp # R-style
}

isLowerTriangle <- function(i, description) {
    stopifnot(is(description, "DescriptionComp"))
}



## Returns draw from truncated integer-only normal distribution (achieved by rounding).
## Returns NA_integer_ if fails to find valid value.
rnormIntTrunc1 <- function(mean, sd, lower, upper, maxAttempt, useC = FALSE) {
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
    ## maxAttempt
    stopifnot(is.integer(maxAttempt))
    stopifnot(identical(length(maxAttempt), 1L))
    stopifnot(!is.na(maxAttempt))
    stopifnot(maxAttempt > 0L)
    ## lower and upper
    stopifnot(is.na(lower) || is.na(upper) || (lower <= upper))
    if (useC) {
        .Call(rnormIntTrunc1_R, mean, sd, lower, upper, maxAttempt)
    }
    else {
        finite.lower <- !is.na(lower)
        finite.upper <- !is.na(upper)
        if (finite.lower && finite.upper && (lower == upper))
            return(lower)
        n.attempt <- 0L
        found <- FALSE
        if (finite.lower && finite.upper) {
            while (!found && (n.attempt < maxAttempt)) {
                n.attempt <- n.attempt + 1L
                prop.value <- rnorm(n = 1L, mean = mean, sd = sd)
                prop.value <- as.integer(prop.value + 0.5)
                found <- (lower <= prop.value) && (prop.value <= upper)
            }
        }
        else if (!finite.lower && finite.upper) {
            while (!found && (n.attempt < maxAttempt)) {
                n.attempt <- n.attempt + 1L
                prop.value <- rnorm(n = 1L, mean = mean, sd = sd)
                prop.value <- as.integer(prop.value + 0.5)
                found <- prop.value <= upper
            }
        }
        else if (finite.lower && !finite.upper) {
            while (!found && (n.attempt < maxAttempt)) {
                n.attempt <- n.attempt + 1L
                prop.value <- rnorm(n = 1L, mean = mean, sd = sd)
                prop.value <- as.integer(prop.value + 0.5)
                found <- lower <= prop.value
            }
        }
        else {
            prop.value <- rnorm(n = 1L, mean = mean, sd = sd)
            prop.value <- as.integer(prop.value + 0.5)
            found <- TRUE
        }
        if (found)
            prop.value
        else
            NA_integer_
    }
}

## NEW CLASSES #################################################################

setClass("System",
         slots(system = "list"),
         contains = "VIRTUAL",
         validity = function(object) {
             system <- object@system
             ## all elements of "system" have class "Poisson" - except for net
             TRUE
         })


setClass("MappingsToPopnMixin",
         slots(mappingsToPopn = "list"),
         contains = "VIRTUAL",
         validity = function(object) {
             mappingsToPopn <- object@mappingsToPopn
             components <- object@components
             ## all elements have class "MappingToPopn"
             if (!all(sapply(mappingsToPopn, methods::is, "MappingToPopn")))
                 return(gettextf("'%s' has elements not of class \"%s\"",
                                 "mappingsToPopn", "MappingToPopn"))
             ## has same length as 'components'
             if (!identical(length(mappingsToPopn), length(components)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "mappingsToPopn", "components"))
             ## element has class "MappingOrigDestToPopn" iff corresponding
             ## element of 'components' has class "HasOrigDest",
             is.mapping.orig.dest <- sapply(components, methods::is, "MappingOrigDestToPopn")
             is.has.orig.dest <- sapply(components, methods::is, "HasOrigDest")
             if (!identical(is.mapping.orig.dest, is.has.orig.dest))
                 return(gettextf("elements of '%s' must have class \"%s\" iff correspondening element of '%s' has class \"%s\"",
                                 "mappingsToPopn", "MappingOrigDestToPopn", "components", "HasOrigDest"))             
             TRUE
         })

setClass("MappingsToAccMixin",
         slots(mappingsToAcc = "list"),
         contains = "VIRTUAL",
         validity = function(object) {
             mappingsToAcc <- object@mappingsToAcc
             components <- object@components
             ## all elements have class "MappingToAcc"
             if (!all(sapply(mappingsToAcc, methods::is, "MappingToAcc")))
                 return(gettextf("'%s' has elements not of class \"%s\"",
                                 "mappingsToAcc", "MappingToAcc"))
             ## has same length as 'components'
             if (!identical(length(mappingsToAcc), length(components)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "mappingsToAcc", "components"))
             ## element has class "MappingOrigDestToAcc" iff corresponding
             ## element of 'components' has class "HasOrigDest",
             is.mapping.orig.dest <- sapply(components, methods::is, "MappingOrigDestToAcc")
             is.has.orig.dest <- sapply(components, methods::is, "HasOrigDest")
             if (!identical(is.mapping.orig.dest, is.has.orig.dest))
                 return(gettextf("elements of '%s' must have class \"%s\" iff correspondening element of '%s' has class \"%s\"",
                                 "mappingsToAcc", "MappingOrigDestToAcc", "components", "HasOrigDest"))             
             TRUE
         })


setClass("ICellMixin",
         slots(iCell = "integer",
               iCellOther = "integer"),
         contains = "VIRTUAL",
         validity = function(object) {
             for (name in c("iCell", "iCellOther")) {
                 value <- slot(object, name)
                 ## 'iCell', 'iCellOther' have length 1
                 if (!identical(length(value), 1L))
                     return(gettextf("'%s' does not have length %d",
                                     name, 1L))
                 ## if 'iCell', 'iCellOther' not missing, they are greater than or equal to 1
                 if (!is.na(value) && value < 1L)
                     return(gettextf("'%s' is less than %d",
                                     name, 1L))
             }
             TRUE
         })

setClass("IsLowerTriangle",
         slots(isLowerTriangle = "logical"),
         contains = "VIRTUAL",
         validity = function(object) {
             isLowerTriangle <- object@isLowerTriangle
             ## 'isLowerTriangle' has length 1
             if (!identical(length(isLowerTriangle), 1L))
                 return(gettextf("'%s' does not have length %d",
                                 "lowerTriangle", 1L))
             TRUE
         })


setClass("IPopnNextMixin",
         slots(iPopnNext = "integer",
               iPopnNextOther = "integer"),
         contains = "VIRTUAL",
         validity = function(object) {
             iPopnNext <- object@iPopnNext
             iPopnNextOther <- object@iPopnNextOther
             population <- object@population
             n.population <- length(population)
             for (name in c("iPopnNext", "iPopnNextOther")) {
                 value <- slot(object, name)
                 ## 'iPopnNext', 'iPopnNextOther' have length 1
                 if (!identical(length(value), 1L))
                     return(gettextf("'%s' does not have length %d",
                                     name, 1L))
                 ## if 'iPopnNext', 'iPopnNextOther' not missing, they are greater than or equal to 0L
                 if (!is.na(value) && (value < 0L))
                     return(gettextf("'%s' is less than %d",
                                     name, 1L))
                 ## if 'iPopnNext', 'iPopnNextOther' not missing, they are less than or
                 ## equal to length of 'population'
                 if (!is.na(value) && (value > n.population))
                     return(gettextf("'%s' is greater than the length of '%s'",
                                     name, "population"))
             }
             ## if 'iPopnNext' and 'iPopnNextOther' not missing, they have different values
             if (!is.na(iPopnNext) && !is.na(iPopnNextOther) && (iPopnNext == iPopnNextOther))
                 return(gettextf("'%s' equals '%s'",
                                 "iPopnNext", "iPopnNextOther"))
             TRUE
         })


setClass("IAccNextMixin",
         slots(iAccNext = "integer",
               iAccNextOther = "integer"),
         contains = "VIRTUAL",
         validity = function(object) {
             iAccNext <- object@iAccNext
             iAccNextOther <- object@iAccNextOther
             for (name in c("iAccNext", "iAccNextOther")) {
                 value <- slot(object, name)
                 ## 'iAccNext', 'iAccNextOther' have length 1
                 if (!identical(length(value), 1L))
                     return(gettextf("'%s' does not have length %d",
                                     name, 1L))
                 ## if 'iAccNext', 'iAccNextOther' not missing, they are greater than or equal to 0L
                 if (!is.na(value) && (value < 0L))
                     return(gettextf("'%s' is less than %d",
                                     name, 1L))
                 ## if 'iAccNext', 'iAccNextOther' not missing, they are less than or
                 ## equal to length of 'accession'
                 if (!is.na(value)) {
                     accession <- object@accession
                     n.accession <- length(accession)
                     return(gettextf("'%s' is greater than the length of '%s'",
                                     name, "accession"))
                 }
             }
             ## if 'iAccNext' and 'iAccNextOther' not missing, they have different values
             if (!is.na(iAccNext) && !is.na(iAccNextOther) && (iAccNext == iAccNextOther))
                 return(gettextf("'%s' equals '%s'",
                                 "iAccNext", "iAccNextOther"))
             TRUE
         })

## The index of the cell in 'exposure' that appears in the likelihood for
## the cell being updated.  iExposure is 0L if the model for the cell being
## updated does not include exposure.
setClass("IExposureMixin",
         slots(iExposure = "integer",
               iExposureOther = "integer"),
         contains = "VIRTUAL",
         validity = function(object) {
             iExposure <- object@iExposure
             iExposureOther <- object@iExposureOther
             for (name in c("iExposure", "iExposureOther")) {
                 value <- slot(object, name)
                 ## 'iExposure', 'iExposureOther' have length 1
                 if (!identical(length(value), 1L))
                     return(gettextf("'%s' does not have length %d",
                                     name, 1L))
                 ## if 'iExposure', 'iExposureOther' not missing, they are greater than or equal to 1L
                 if (!is.na(value) && (value < 1L))
                     return(gettextf("'%s' is less than %d",
                                     name, 1L))
                 ## if 'iExposure', 'iExposureOther' not missing, they are less than or
                 ## equal to length of 'exposure'
                 if (!is.na(value)) {
                     exposure <- object@exposure
                     n.exposure <- length(exposure)
                     if (value > n.exposure)
                         return(gettextf("'%s' is greater than the length of '%s'",
                                         name, "exposure"))
                 }
             }
             ## if 'iExposure' and 'iExposureOther' not missing, they have different values
             if (!is.na(iExposure) && !is.na(iExposureOther) && (iExposure == iExposureOther))
                 return(gettextf("'%s' equals '%s'",
                                 "iExposure", "iExposureOther"))
             TRUE
         })

## the index of the first cell in 'exposure' that
## will change if the cell being updated is changed
setClass("IExpFirstMixin",
         slots(iExpFirst = "integer",
               iExpFirstOther = "integer"),
         contains = "VIRTUAL",
         validity = function(object) {
             iExpFirst <- object@iExpFirst
             iExpFirstOther <- object@iExpFirstOther
             for (name in c("iExpFirst", "iExpFirstOther")) {
                 value <- slot(object, name)
                 ## 'iExpFirst', 'iExpFirstOther' have length 1
                 if (!identical(length(value), 1L))
                     return(gettextf("'%s' does not have length %d",
                                     name, 1L))
                 ## if 'iExpFirst', 'iExpFirstOther' not missing, they are greater than or equal to 1L
                 if (!is.na(value) && (value < 1L))
                     return(gettextf("'%s' is less than %d",
                                     name, 1L))
                 if (!is.na(value)) {
                     ## if 'iExpFirst', 'iExpFirstOther' not missing, they are less than or
                     ## equal to length of 'exposure'
                     exposure <- object@exposure
                     n.exposure <- length(exposure)
                     if (value > n.exposure)
                         return(gettextf("'%s' is greater than the length of '%s'",
                                         name, "exposure"))
                     ## if 'iExpFirst', 'iExpFirstOther' not missing,
                     ## iExposure and iExposureOther not missing
                     name.i.exp <- if (name == "iExpFirstFirst") "iExposure" else "iExposureOther"
                     val.i.exp <- slot(object, name.i.exp)
                     if (value <= val.i.exp)
                         return(gettextf("'%s' less than or equal to '%s'",
                                         name, name.i.exp))
                 }
             }
             ## if 'iExpFirst' and 'iExpFirstOther' not missing, they have different values
             if (!is.na(iExpFirst) && !is.na(iExpFirstOther) && (iExpFirst == iExpFirstOther))
                 return(gettextf("'%s' equals '%s'",
                                 "iExpFirst", "iExpFirstOther"))
             TRUE
         })

setClass("MovementsAccountInternal",
         slots(accession = "Accession",
               descriptionPopn = "DescriptionPopn",
               descriptionsComp = "list",
               exposure = "Exposure",
               mappingsToExposure = "list",
               mappingsFromExposure = "list"),
         prototype(slotsToExtract = c("account", "system", "observation")),
         contains = c("MovementsAccount",
                      "MappingsToPopnMixin",
                      "MappingsToAccMixin",


                      "MappingsToExpMixin",
                      "MappingsFromExpMixin",


                      "MappingPopn", "MappingComp", "IsIncrement",
                      "ProbPopn", "CumProbComp", "NCellAccount",
                      "ICell", "DiffVal", "IAffectedPopn", "IAffectedAcc",
                      "SlotsToExtract"),
         validity = function(object) {
             ## 'accession' and 'population' have identical dimensions apart from time and age
             ## accession does not start at age 0 (otherwise have to have different mappings
             ## to accession, depending on whether account includes births)
             TRUE
         })

setClass("CombinedAccount",
         slots(account = "DemographicAccount"),
         contains = c("VIRTUAL", "AddToExpose", "MaxAttempt"))

setClass("CombinedAccountMove",
         contains = c("VIRTUAL", "CombinedAccount"),
         validity = function(object) {
             ## account has class "MovementsAccountExtra"
             TRUE
         })

setClass("diffProp",
         slots(diffProp = "integer"),
         contains = "VIRTUAL",
         validity = function(object) {
             diffProp <- object@diffProp
             ## 'diffProp' has length 1
             if (!identical(length(diffProp), 1L))
                 return(gettextf("'%s' does not have length %d",
                                 "diffProp", 1L))
             ## if 'diffProp' not missing, is greater than or equal to 1
             if (!is.na(diffProp) && value < 1L)
                 return(gettextf("'%s' is less than %d",
                                 name, 1L))
             TRUE
         })

## NO_TESTS
setMethod("updateCombined",
          signature(object = "CombinedAccountMove"),
          function(object, nUpdate = 1L, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(validObject(object))
              ## nUpdate
              stopifnot(identical(length(nUpdate), 1L))
              stopifnot(is.integer(nUpdate))
              stopifnot(!is.na(nUpdate))
              stopifnot(nUpdate >= 0L)
              if (useC) {
                  if (useSpecific)
                      .Call(updateCombined_CombinedAccountMoveEst_R,
                            object, nUpdate)
                  else
                      .Call(updateCombined_R, object, nUpdate)
              }
              else {
                  for (i in seq_len(nUpdate)) {
                      object <- updateAccountMove(object)
                      object <- updateSystem(object)
                      object <- updateObservation(object)
                  }
                  object
              }
          })



## ## NO_TESTS
## setClass("CombinedAccount",
##          slots(population = "Population",
##                         mappingPopn = "Mapping",
##                         accession = "Accession",
##                         components = "list",
##                         mappingsComp = "list",
##                         isIncrement = "logical",
##                         popnCompIndex = "integer",
##                         system = "list",
##                         addToExpose = "numeric",
##                         maxAttempt = "numeric"),
##          prototype = prototype(iMethodCombined = 10L,
##              slotsToExtract = c("population", "components",
##                  "system", "observation")),
##          contains = c("VIRTUAL", "Combined", "Observation"),
##          validity = function(object) {
##              population <- object@population
##              components <- object@components
##              ## 'probPopulation' has length 1
##              ## 'probPopulation' is not missing
##              ## 'probPopulation' between 0 and 1
##              ## 'cumProbComponents' has no missing values
##              ## all elements of 'cumProbComponents' greater than 0
##              ## 'cumProbComponents' nondecreasing
##              ## last element 'cumProbComponents' equals 1
##              ## all elements of 'system' have class "Model"
##              if (!all(sapply(system, is, "Model")))
##                  return(gettextf("'%s' has elements not of class \"%s\"",
##                                  "system", "Model"))
##              ## 'observation' does not have names
##              if (!is.null(names(observation)))
##                  return(gettextf("'%s' has names",
##                                  "observation"))
##              ## length of 'system' equals length of 'components' plus one
##              if (!identical(length(systems), length(components) + 1L))
##                  return(gettextf("'%s' and '%s' have inconsistent lengths",
##                                  "system", "components"))
##              TRUE
##          })

## setClass("CombinedAccountMovements",
##          slots(population = "Counts",
##                         components = "list",
##                         namesComponents = "character",
##                         system = "list"),
##          prototype = prototype(iMethodCombined = 999L),
##          contains = "CombinedAccount",
##          validity = function(object) {
##          })

## setClass("CombinedAccountTransitions",
##          prototype = prototype(iMethodCombined = 999L),
##          contains = "CombinedAccount",
##          validity = function(object) {
##              NULL
##          })













