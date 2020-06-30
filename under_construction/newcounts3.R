


updateProposalAccountMovePopn <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(updateProposalAccountMovePopn_R, combined)
    }
    else {
        account <- combined@account
        population <- account@population
        max.attempt <- combined@maxAttempt
        has.age <- combined@hasAge@.Data
        if (has.age) {
            accession <- combined@accession
            iterator.acc <- combined@iteratorAcc
        }
        description <- combined@descriptions[[1L]]
        iterator.popn <- combined@iteratorPopn
        sys.mod <- combined@systemModels[[1L]]
        theta <- sys.mod@theta
        struc.zero.array <- sys.mod@strucZeroArray
        joint.update <- combined@jointUpdate
        if (joint.update) {
            mu <- sys.mod@mu
            sigma <- sys.mod@sigma
            scale.theta <- sys.mod@scaleTheta@.Data
            lower <- sys.mod@lower
            upper <- sys.mod@upper
            tolerance <- model@tolerance
            box.cox.param <- model@boxCoxParam
        }
        generated.new.proposal  <- FALSE
        for (i in seq_len(max.attempt)) {
            i.cell <- chooseICellPopn(description)
            is.struc.zero <- struc.zero.array[i.cell] == 0L
            if (!is.struc.zero) {
                generated.new.proposal <- TRUE
                break
            }
        }
        if (generated.new.proposal && joint.update) {
            for (i in seq_len(max.attempt)) {
                tr.th.prop <- rnorm(n = 1L, mean = mu[i], sd = scale.theta * sigma)
                generated.new.proposal <- ((tr.th.prop > lower + tolerance)
                    && (tr.th.prop < upper - tolerance))
                if (generated.new.proposal) {
                    if (box.cox.param > 0)
                        th.prop <- (box.cox.param * tr.th.prop + 1) ^ (1 / box.cox.param)
                    else
                        th.prop <- exp(tr.th.prop)
                    break
                }
            }
        }        
        if (generated.new.proposal) {
            i.exposure <- 0L
            i.exp.first <- getIExpFirstFromPopn(i = i.cell,
                                                description = description)
            i.popn.next <- getIPopnNextFromPopn(i = i.cell,
                                                description = description)
            if (has.age) {
                is.oldest.age.group <- isOldestAgeGroup(i = i.cell,
                                                        description = description)
                if (is.oldest.age.group)
                    min.val <- NA_integer_
                else
                    min.val <- getMinValCohortPopulationHasAge(i = i.popn.next,
                                                               population = population,
                                                               accession = accession,
                                                               iterator = iterator.popn)
            }
            else
                min.val <- getMinValCohortPopulationNoAge(i = i.popn.next,
                                                          series = population,
                                                          iterator = iterator.popn)
            if (has.age) {
                i.acc.next <- getIAccNextFromPopn(i = i.cell,
                                                  description = description)
                has.later.accession <- i.acc.next > 0L
                if (has.later.accession) {
                    min.acc <- getMinValCohortAccession(i = i.acc.next,
                                                        series = accession,
                                                        iterator = iterator.acc)
                    if (is.na(min.val) || (min.acc < min.val))
                        min.val <- min.acc
                }
            }
            val.curr <- population[i.cell]
            lower <- val.curr - min.val
            upper <- NA_integer_
            th.curr <- theta[i]
            lambda <- if (joint.update) th.prop else th.curr
            val.prop <- rpoisTrunc1(lambda = lambda,
                                    lower = lower,
                                    upper = upper,
                                    maxAttempt = max.attempt)
            found.value <- !is.na(val.prop)
            if (found.value) {
                diff.prop <- unname(val.prop - val.curr)
                diff.theta <- if (joint.update) th.prop - th.curr else 0
                generated.new.proposal <- (diff.prop != 0L) || (diff.theta != 0)
            }
            else
                generated.new.proposal <- FALSE
        }
        if (generated.new.proposal) {
            combined@generatedNewProposal@.Data <- TRUE
            combined@isSmallUpdate@.Data <- FALSE
            combined@iCell <- i.cell
            combined@iCellOther <- NA_integer_
            combined@iPopnNext <- i.popn.next
            combined@iPopnNextOther <- NA_integer_
            if (has.age) {
                combined@iAccNext <- i.acc.next
                combined@iAccNextOther <- NA_integer_
                combined@isLowerTriangle@.Data <- NA
            }
            combined@iExposure <- i.exposure
            combined@iExposureOther <- NA_integer_
            combined@iExpFirst <- i.exp.first
            combined@iExpFirstOther <- NA_integer_
            combined@diffProp <- diff.prop
            combined@diffTheta <- diff.theta
        }
        else {
            combined@generatedNewProposal@.Data <- FALSE
            combined@isSmallUpdate@.Data <- FALSE
            combined@iCell <- NA_integer_
            combined@iCellOther <- NA_integer_
            combined@iPopnNext <- NA_integer_
            combined@iPopnNextOther <- NA_integer_
            if (has.age) {
                combined@iAccNext <- NA_integer_
                combined@iAccNextOther <- NA_integer_
                combined@isLowerTriangle@.Data <- NA
            }
            combined@iExposure <- NA_integer_
            combined@iExposureOther <- NA_integer_
            combined@iExpFirst <- NA_integer_
            combined@iExpFirstOther <- NA_integer_
            combined@diffProp <- NA_integer_
            combined@diffTheta <- NA_real_
        }
        combined
    }
}


## TRANSLATED
## HAS_TESTS
updateProposalAccountMoveBirths <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(updateProposalAccountMoveBirths_R, combined)
    }
    else {
        account <- combined@account
        population <- account@population
        i.comp <- combined@iComp
        component <- account@components[[i.comp]]
        max.attempt <- combined@maxAttempt
        has.age <- combined@hasAge
        if (has.age) {
            accession <- combined@accession
            iterator.acc <- combined@iteratorAcc
            mapping.to.acc <- combined@mappingsToAcc[[i.comp]]
        }
        mapping.to.popn <- combined@mappingsToPopn[[i.comp]]
        iterator.popn <- combined@iteratorPopn
        uses.exposure <- combined@modelUsesExposure[i.comp + 1L]
        mapping.to.exp <- combined@mappingsToExp[[i.comp]]
        description <- combined@descriptions[[i.comp + 1L]]
        sys.mod <-combined@systemModels[[i.comp + 1L]]
        theta <- sys.mod@theta
        struc.zero.array <- sys.mod@strucZeroArray
        joint.update <- combined@jointUpdate
        if (joint.update) {
            mu <- sys.mod@mu
            sigma <- sys.mod@sigma
            scale.theta <- sys.mod@scaleTheta@.Data
            lower <- sys.mod@lower
            upper <- sys.mod@upper
            tolerance <- model@tolerance
            box.cox.param <- model@boxCoxParam
        }
        generated.new.proposal <- FALSE
        for (i in seq_len(max.attempt)) {
            i.cell <- chooseICellComp(description)
            is.struc.zero <- struc.zero.array[i.cell] == 0L
            if (!is.struc.zero) {
                generated.new.proposal <- TRUE
                break
            }
        }
        if (generated.new.proposal && joint.update) {
            for (i in seq_len(max.attempt)) {
                tr.th.prop <- rnorm(n = 1L, mean = mu[i], sd = scale.theta * sigma)
                generated.new.proposal <- ((tr.th.prop > lower + tolerance)
                    && (tr.th.prop < upper - tolerance))
                if (generated.new.proposal) {
                    if (box.cox.param > 0)
                        th.prop <- (box.cox.param * tr.th.prop + 1) ^ (1 / box.cox.param)
                    else
                        th.prop <- exp(tr.th.prop)
                    break
                }
            }
        }        
        if (generated.new.proposal) {
            if (has.age)
                is.lower.triangle <- isLowerTriangle(i = i.cell,
                                                     description = description)
            if (uses.exposure) {
                expected.exposure <- combined@expectedExposure
                i.exposure <- getIExposureFromBirths(i = i.cell,
                                                     mapping = mapping.to.exp)
            }
            i.exp.first <- getIExpFirstFromBirths(i = i.cell,
                                                  mapping = mapping.to.exp)
            i.popn.next <- getIPopnNextFromComp(i = i.cell,
                                                mapping = mapping.to.popn)
            if (has.age)
                min.val <- getMinValCohortPopulationHasAge(i = i.popn.next,
                                                           population = population,
                                                           accession = accession,
                                                           iterator = iterator.popn)
            else
                min.val <- getMinValCohortPopulationNoAge(i = i.popn.next,
                                                          series = population,
                                                          iterator = iterator.popn)
            if (has.age) {
                i.acc.next <- getIAccNextFromComp(i = i.cell,
                                                  mapping = mapping.to.acc)
                has.later.accession <- i.acc.next > 0L
                if (has.later.accession) {
                    min.acc <- getMinValCohortAccession(i = i.acc.next,
                                                        series = accession,
                                                        iterator = iterator.acc)
                    min.val <- min(min.val, min.acc)
                }
            }
            val.curr <- component[i.cell]
            lower <- val.curr - min.val
            upper <- NA_integer_
            th.curr <- theta[i.cell]
            th.cell <- if (joint.update) th.prop else th.curr
            if (uses.exposure) {
                expected.exposure.cell <- expected.exposure[i.exposure]
                lambda <- th.cell * expected.exposure.cell
            }
            else
                lambda <- th.cell
            val.prop <- rpoisTrunc1(lambda = lambda,
                                    lower = lower,
                                    upper = upper,
                                    maxAttempt = max.attempt)
            found.value <- !is.na(val.prop)
            if (found.value) {
                diff.prop <- unname(val.prop - val.curr)
                diff.theta <- if (joint.update) th.prop - th.curr else 0
                generated.new.proposal <- (diff.prop != 0L) || (diff.theta != 0)
            }
            else
                generated.new.proposal <- FALSE
        }
        if (generated.new.proposal) {
            combined@generatedNewProposal@.Data <- TRUE
            combined@isSmallUpdate@.Data <- FALSE
            combined@iCell <- i.cell
            combined@iCellOther <- NA_integer_
            combined@iPopnNext <- i.popn.next
            combined@iPopnNextOther <- NA_integer_
            if (has.age) {
                combined@iAccNext <- i.acc.next
                combined@iAccNextOther <- NA_integer_
                combined@isLowerTriangle@.Data <- is.lower.triangle
            }
            if (uses.exposure) {
                combined@iExposure <- i.exposure
                combined@iExposureOther <- NA_integer_
            }
            else {
                combined@iExposure <- 0L
                combined@iExposureOther <- NA_integer_
            }
            combined@iExpFirst <- i.exp.first
            combined@iExpFirstOther <- NA_integer_
            combined@diffProp <- diff.prop
            combined@diffTheta <- diff.theta
        }
        else {
            combined@generatedNewProposal@.Data <- FALSE
            combined@isSmallUpdate@.Data <- FALSE
            combined@iCell <- NA_integer_
            combined@iCellOther <- NA_integer_
            combined@iPopnNext <- NA_integer_
            combined@iPopnNextOther <- NA_integer_
            if (has.age) {
                combined@iAccNext <- NA_integer_
                combined@iAccNextOther <- NA_integer_
                combined@isLowerTriangle@.Data <- NA
            }
            combined@iExposure <- NA_integer_
            combined@iExposureOther <- NA_integer_
            combined@iExpFirst <- NA_integer_
            combined@iExpFirstOther <- NA_integer_
            combined@diffProp <- NA_integer_
            combined@diffTheta <- NA_real_
        }
        combined
    }
}




## TRANSLATED
## HAS_TESTS
updateProposalAccountMoveOrigDest <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(updateProposalAccountMoveOrigDest_R, combined)
    }
    else {
        account <- combined@account
        population <- account@population
        i.comp <- combined@iComp
        component <- account@components[[i.comp]]
        max.attempt <- combined@maxAttempt
        has.age <- combined@hasAge@.Data
        if (has.age) {
            accession <- combined@accession
            iterator.acc <- combined@iteratorAcc
            mapping.to.acc <- combined@mappingsToAcc[[i.comp]]
        }
        mapping.to.popn <- combined@mappingsToPopn[[i.comp]]
        iterator.popn <- combined@iteratorPopn
        mapping.to.exp <- combined@mappingsToExp[[i.comp]]
        uses.exposure <- combined@modelUsesExposure[i.comp + 1L]
        description <- combined@descriptions[[i.comp + 1L]]
        sys.mod <-combined@systemModels[[i.comp + 1L]]
        theta <- sys.mod@theta
        struc.zero.array <- sys.mod@strucZeroArray
        joint.update <- combined@jointUpdate
        if (joint.update) {
            mu <- sys.mod@mu
            sigma <- sys.mod@sigma
            scale.theta <- sys.mod@scaleTheta@.Data
            lower <- sys.mod@lower
            upper <- sys.mod@upper
            tolerance <- model@tolerance
            box.cox.param <- model@boxCoxParam
        }
        generated.new.proposal  <- FALSE
        for (i in seq_len(max.attempt)) {
            i.cell <- chooseICellComp(description)
            is.struc.zero <- struc.zero.array[i.cell] == 0L
            if (!is.struc.zero) {
                generated.new.proposal <- TRUE
                break
            }
        }
        if (generated.new.proposal && joint.update) {
            for (i in seq_len(max.attempt)) {
                tr.th.prop <- rnorm(n = 1L, mean = mu[i], sd = scale.theta * sigma)
                generated.new.proposal <- ((tr.th.prop > lower + tolerance)
                    && (tr.th.prop < upper - tolerance))
                if (generated.new.proposal) {
                    if (box.cox.param > 0)
                        th.prop <- (box.cox.param * tr.th.prop + 1) ^ (1 / box.cox.param)
                    else
                        th.prop <- exp(tr.th.prop)
                    break
                }
            }
        }        
        if (generated.new.proposal) {
            if (has.age)
                is.lower.triangle <- isLowerTriangle(i = i.cell,
                                                     description = description)
            if (uses.exposure) {
                expected.exposure <- combined@expectedExposure
                i.exposure <- getIExposureFromOrigDest(i = i.cell,
                                                       mapping = mapping.to.exp)
            }
            pair.exp.first <- getIExpFirstPairFromOrigDest(i = i.cell,
                                                           mapping = mapping.to.exp)
            i.exp.first.orig <- pair.exp.first[1L]
            i.exp.first.dest <- pair.exp.first[2L]
            pair.popn.next <- getIPopnNextFromOrigDest(i = i.cell,
                                                       mapping = mapping.to.popn)
            i.popn.next.orig <- pair.popn.next[1L]
            i.popn.next.dest <- pair.popn.next[2L]
            if (has.age) {
                is.oldest.age.group <- isOldestAgeGroup(i = i.cell,
                                                        description = description)
                if (is.oldest.age.group && !is.lower.triangle) {
                    min.val.orig <- NA_integer_
                    min.val.dest <- NA_integer_
                }
                else {
                    min.val.orig <- getMinValCohortPopulationHasAge(i = i.popn.next.orig,
                                                                    population = population,
                                                                    accession = accession,
                                                                    iterator = iterator.popn)
                    min.val.dest <- getMinValCohortPopulationHasAge(i = i.popn.next.dest,
                                                                    population = population,
                                                                    accession = accession,
                                                                    iterator = iterator.popn)
                }
            }
            else {
                min.val.orig <- getMinValCohortPopulationNoAge(i = i.popn.next.orig,
                                                               series = population,
                                                               iterator = iterator.popn)
                min.val.dest <- getMinValCohortPopulationNoAge(i = i.popn.next.dest,
                                                               series = population,
                                                               iterator = iterator.popn)
            }
            if (has.age) {
                pair.acc.next <- getIAccNextFromOrigDest(i = i.cell,
                                                         mapping = mapping.to.acc)
                i.acc.next.orig <- pair.acc.next[1L]
                i.acc.next.dest <- pair.acc.next[2L]
                has.later.accession <- i.acc.next.orig > 0L
                if (has.later.accession) {
                    min.acc.orig <- getMinValCohortAccession(i = i.acc.next.orig,
                                                             series = accession,
                                                             iterator = iterator.acc)
                    min.acc.dest <- getMinValCohortAccession(i = i.acc.next.dest,
                                                             series = accession,
                                                             iterator = iterator.acc)
                    if (is.na(min.val.orig) || (min.acc.orig < min.val.orig))
                        min.val.orig <- min.acc.orig
                    if (is.na(min.val.dest) || (min.acc.dest < min.val.dest))
                        min.val.dest <- min.acc.dest
                }
            }
            val.curr <- component[i.cell]
            lower <- val.curr - min.val.dest
            upper <- val.curr + min.val.orig
            th.curr <- theta[i.cell]
            theta.cell <- if (joint.update) th.prop else th.curr
            if (uses.exposure) {
                expected.exposure.cell <- expected.exposure[i.exposure]
                lambda <- theta.cell * expected.exposure.cell
            }
            else
                lambda <- theta.cell
            if (lower > upper)
                found.value <- FALSE
            else {
                if (is.infinite(lower))
                    lower <- NA_integer_
                if (is.infinite(upper))
                    upper <- NA_integer_
                val.prop <- rpoisTrunc1(lambda = lambda,
                                        lower = lower,
                                        upper = upper,
                                        maxAttempt = max.attempt)
                found.value <- !is.na(val.prop)
            }
            if (found.value) {
                diff.prop <- unname(val.prop - val.curr)
                diff.theta <- if (joint.update) th.prop - th.curr else 0
                generated.new.proposal <- (diff.prop != 0L) || (diff.theta != 0)
            }
            else
                generated.new.proposal <- FALSE
        }
        if (generated.new.proposal) {
            combined@generatedNewProposal@.Data <- TRUE
            combined@isSmallUpdate@.Data <- FALSE
            combined@iCell <- i.cell
            combined@iCellOther <- NA_integer_
            combined@iPopnNext <- i.popn.next.orig
            combined@iPopnNextOther <- i.popn.next.dest
            if (has.age) {
                combined@iAccNext <- i.acc.next.orig
                combined@iAccNextOther <- i.acc.next.dest
                combined@isLowerTriangle@.Data <- is.lower.triangle
            }
            if (uses.exposure) {
                combined@iExposure <- i.exposure
                combined@iExposureOther <- NA_integer_
            }
            else {
                combined@iExposure <- 0L
                combined@iExposureOther <- NA_integer_
            }
            combined@iExpFirst <- i.exp.first.orig
            combined@iExpFirstOther <- i.exp.first.dest
            combined@diffProp <- diff.prop
            combined@diffTheta <- diff.theta
        }
        else {
            combined@generatedNewProposal@.Data <- FALSE
            combined@isSmallUpdate@.Data <- FALSE
            combined@iCell <- NA_integer_
            combined@iCellOther <- NA_integer_
            combined@iPopnNext <- NA_integer_
            combined@iPopnNextOther <- NA_integer_
            if (has.age) {
                combined@iAccNext <- NA_integer_
                combined@iAccNextOther <- NA_integer_
                combined@isLowerTriangle@.Data <- NA ## changed JAH 22/12/2017
            }
            combined@iExposure <- NA_integer_
            combined@iExposureOther <- NA_integer_
            combined@iExpFirst <- NA_integer_
            combined@iExpFirstOther <- NA_integer_
            combined@diffProp <- NA_integer_
            combined@diffTheta <- NA_real_
       }
        combined
    }
}



## TRANSLATED
## HAS_TESTS
updateProposalAccountMovePool <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(updateProposalAccountMovePool_R, combined)
    }
    else {
        account <- combined@account
        population <- account@population
        i.comp <- combined@iComp
        component <- account@components[[i.comp]]
        max.attempt <- combined@maxAttempt
        has.age <- combined@hasAge@.Data
        if (has.age) {
            accession <- combined@accession
            iterator.acc <- combined@iteratorAcc
            mapping.to.acc <- combined@mappingsToAcc[[i.comp]]
        }
        mapping.to.popn <- combined@mappingsToPopn[[i.comp]]
        iterator.popn <- combined@iteratorPopn
        mapping.to.exp <- combined@mappingsToExp[[i.comp]]
        uses.exposure <- combined@modelUsesExposure[i.comp + 1L]
        description <- combined@descriptions[[i.comp + 1L]]
        sys.mod <-combined@systemModels[[i.comp + 1L]]
        theta <- sys.mod@theta
        struc.zero.array <- sys.mod@strucZeroArray
        joint.update <- combined@jointUpdate
        if (joint.update) {
            mu <- sys.mod@mu
            sigma <- sys.mod@sigma
            scale.theta <- sys.mod@scaleTheta@.Data
            lower <- sys.mod@lower
            upper <- sys.mod@upper
            tolerance <- model@tolerance
            box.cox.param <- model@boxCoxParam
        }
        generated.new.proposal <- FALSE
        for (i in seq_len(max.attempt)) {
            pair.cell <- chooseICellOutInPool(description)
            i.cell.out <- pair.cell[1L] # 'diffProp' added to this cell
            i.cell.in <- pair.cell[2L]  # 'diffProp' also added to this cell
            is.struc.zero <- ((struc.zero.array[i.cell.out] == 0L)
                || (struc.zero.array[i.cell.in] == 0L))
            if (!is.struc.zero) {
                generated.new.proposal <- TRUE
                break
            }
        }
        if (generated.new.proposal && joint.update) {
            for (i in seq_len(max.attempt)) {
                tr.th.prop.out <- rnorm(n = 1L, mean = mu[i.cell.out], sd = scale.theta * sigma)
                generated.new.proposal <- ((tr.th.prop.out > lower + tolerance)
                    && (tr.th.prop.out < upper - tolerance))
                if (generated.new.proposal) {
                    if (box.cox.param > 0)
                        th.prop.out <- (box.cox.param * tr.th.prop.out + 1) ^ (1 / box.cox.param)
                    else
                        th.prop.out <- exp(tr.th.prop.out)
                    break
                }
            }
        }
        if (generated.new.proposal) {
            if (has.age)
                is.lower.triangle <- isLowerTriangle(i = i.cell.out,
                                                     description = description)
            if (uses.exposure) {
                expected.exposure <- combined@expectedExposure
                i.exposure.out <- getIExposureFromComp(i = i.cell.out,
                                                       mapping = mapping.to.exp)
                i.exposure.in <- getIExposureFromComp(i = i.cell.in,
                                                      mapping = mapping.to.exp)
            }
            i.exp.first.out <- getIExpFirstFromComp(i = i.cell.out,
                                                    mapping = mapping.to.exp)
            i.exp.first.in <- getIExpFirstFromComp(i = i.cell.in,
                                                   mapping = mapping.to.exp)
            i.popn.next.out <- getIPopnNextFromComp(i = i.cell.out,
                                                    mapping = mapping.to.popn)
            i.popn.next.in <- getIPopnNextFromComp(i = i.cell.in,
                                                   mapping = mapping.to.popn)
            if (has.age) {
                is.oldest.age.group <- isOldestAgeGroup(i = i.cell.out,
                                                        description = description)
                if (is.oldest.age.group && !is.lower.triangle) {
                    min.val.out <- NA_integer_
                    min.val.in <- NA_integer_
                }
                else {
                    min.val.out <- getMinValCohortPopulationHasAge(i = i.popn.next.out,
                                                                   population = population,
                                                                   accession = accession,
                                                                   iterator = iterator.popn)
                    min.val.in <- getMinValCohortPopulationHasAge(i = i.popn.next.in,
                                                                  population = population,
                                                                  accession = accession,
                                                                  iterator = iterator.popn)
                }
            }
            else {
                min.val.out <- getMinValCohortPopulationNoAge(i = i.popn.next.out,
                                                              series = population,
                                                              iterator = iterator.popn)
                min.val.in <- getMinValCohortPopulationNoAge(i = i.popn.next.in,
                                                             series = population,
                                                             iterator = iterator.popn)
            }
            if (has.age) {
                i.acc.next.out <- getIAccNextFromComp(i = i.cell.out,
                                                      mapping = mapping.to.acc)
                i.acc.next.in <- getIAccNextFromComp(i = i.cell.in,
                                                     mapping = mapping.to.acc)
                has.later.accession <- i.acc.next.out > 0L
                if (has.later.accession) {
                    min.acc.out <- getMinValCohortAccession(i = i.acc.next.out,
                                                            series = accession,
                                                            iterator = iterator.acc)
                    min.acc.in <- getMinValCohortAccession(i = i.acc.next.in,
                                                           series = accession,
                                                           iterator = iterator.acc)
                    if (is.na(min.val.out) || (min.acc.out < min.val.out))
                        min.val.out <- min.acc.out
                    if (is.na(min.val.in) || (min.acc.in < min.val.in))
                        min.val.in <- min.acc.in
                }
            }
            val.curr.out <- component[i.cell.out]
            val.curr.in <- component[i.cell.in]
            lower <- val.curr.in - min.val.in
            upper <- val.curr.out + min.val.out
            th.out.curr <- theta[i.cell.out]
            theta.out <- if (joint.update) th.prop.out else th.out.curr
            if (uses.exposure) {
                expected.exposure.out <- expected.exposure[i.exposure.out]
                lambda.out <- theta.out * expected.exposure.out
            }
            else
                lambda.out <- theta.out
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
                diff.prop <- unname(val.prop.out - val.curr.out)
                diff.theta <- if (joint.update) th.prop.out - th.out.curr else 0
                generated.new.proposal <- (diff.prop != 0L) || (diff.theta != 0)
            }
            else
                generated.new.proposal <- FALSE
        }
        if (generated.new.proposal) {
            combined@generatedNewProposal@.Data <- TRUE
            combined@isSmallUpdate@.Data <- FALSE
            combined@iCell <- i.cell.out
            combined@iCellOther <- i.cell.in
            combined@iPopnNext <- i.popn.next.out
            combined@iPopnNextOther <- i.popn.next.in
            if (has.age) {
                combined@iAccNext <- i.acc.next.out
                combined@iAccNextOther <- i.acc.next.in
                combined@isLowerTriangle@.Data <- is.lower.triangle
            }
            if (uses.exposure) {
                combined@iExposure <- i.exposure.out
                combined@iExposureOther <- i.exposure.in
            }
            else {
                combined@iExposure <- 0L
                combined@iExposureOther <- NA_integer_
            }
            combined@iExpFirst <- i.exp.first.out
            combined@iExpFirstOther <- i.exp.first.in
            combined@diffProp <- diff.prop
            combined@diffTheta <- diff.theta
        }
        else {
            combined@generatedNewProposal@.Data <- FALSE
            combined@isSmallUpdate@.Data <- FALSE
            combined@iCell <- NA_integer_
            combined@iCellOther <- NA_integer_
            combined@iPopnNext <- NA_integer_
            combined@iPopnNextOther <- NA_integer_
            if (has.age) {
                combined@iAccNext <- NA_integer_
                combined@iAccNextOther <- NA_integer_
                combined@isLowerTriangle@.Data <- NA
            }
            combined@iExposure <- NA_integer_
            combined@iExposureOther <- NA_integer_
            combined@iExpFirst <- NA_integer_
            combined@iExpFirstOther <- NA_integer_
            combined@diffProp <- NA_integer_
            combined@diffTheta <- NA_real_
        }
        combined
    }
}




## TRANSLATED
## HAS_TESTS
updateProposalAccountMoveNet <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(updateProposalAccountMoveNet_R, combined)
    }
    else {
        account <- combined@account
        population <- account@population
        i.comp <- combined@iComp
        component <- account@components[[i.comp]]
        max.attempt <- combined@maxAttempt
        has.age <- combined@hasAge
        if (has.age) {
            accession <- combined@accession
            iterator.acc <- combined@iteratorAcc
            mapping.to.acc <- combined@mappingsToAcc[[i.comp]]
        }
        mapping.to.popn <- combined@mappingsToPopn[[i.comp]]
        iterator.popn <- combined@iteratorPopn
        mapping.to.exp <- combined@mappingsToExp[[i.comp]]
        description <- combined@descriptions[[i.comp + 1L]]
        system.model <- combined@systemModels[[i.comp + 1L]]
        sys.mod <-combined@systemModels[[i.comp + 1L]]
        theta <- sys.mod@theta
        varsigma <- sys.mod@varsigma
        w <- sys.mod@w
        joint.update <- combined@jointUpdate
        if (joint.update) {
            mu <- sys.mod@mu
            sigma <- sys.mod@sigma
            scale.theta <- sys.mod@scaleTheta@.Data
            lower <- sys.mod@lower
            upper <- sys.mod@upper
            tolerance <- model@tolerance
        }
        pair.cell <- chooseICellSubAddNet(description)
        i.cell.add <- pair.cell[1L] # 'diffProp' added to this cell
        i.cell.sub <- pair.cell[2L] # 'diffProp' subtracted from this cell
        if (joint.update) {
            generated.new.proposal <- FALSE
            for (i in seq_len(max.attempt)) {
                th.prop.add <- rnorm(n = 1L, mean = mu[i.cell.add], sd = scale.theta * sigma)
                generated.new.proposal <- ((th.prop.add > lower + tolerance)
                    && (th.prop.add < upper - tolerance))
                if (generated.new.proposal)
                    break
            }
        }
        else
            generated.new.proposal <- TRUE
        if (generated.new.proposal) {
            if (has.age) {
                is.lower.triangle <- isLowerTriangle(i = i.cell.add,
                                                     description = description)
            }
            i.exp.first.add <- getIExpFirstFromComp(i = i.cell.add,
                                                    mapping = mapping.to.exp)
            i.exp.first.sub <- getIExpFirstFromComp(i = i.cell.sub,
                                                    mapping = mapping.to.exp)
            i.popn.next.add <- getIPopnNextFromComp(i = i.cell.add,
                                                    mapping = mapping.to.popn)
            i.popn.next.sub <- getIPopnNextFromComp(i = i.cell.sub,
                                                    mapping = mapping.to.popn)
            if (has.age) {
                is.oldest.age.group <- isOldestAgeGroup(i = i.cell.add,
                                                        description = description)
                if (is.oldest.age.group & !is.lower.triangle) {
                    min.val.add <- NA_integer_
                    min.val.sub <- NA_integer_
                }
                else {
                    min.val.add <- getMinValCohortPopulationHasAge(i = i.popn.next.add,
                                                                   population = population,
                                                                   accession = accession,
                                                                   iterator = iterator.popn)
                    min.val.sub <- getMinValCohortPopulationHasAge(i = i.popn.next.sub,
                                                                   population = population,
                                                                   accession = accession,
                                                                   iterator = iterator.popn)
                }
            }
            else {
                min.val.add <- getMinValCohortPopulationNoAge(i = i.popn.next.add,
                                                              series = population,
                                                              iterator = iterator.popn)
                min.val.sub <- getMinValCohortPopulationNoAge(i = i.popn.next.sub,
                                                              series = population,
                                                              iterator = iterator.popn)
            }
            if (has.age) {
                i.acc.next.add <- getIAccNextFromComp(i = i.cell.add,
                                                      mapping = mapping.to.acc)
                i.acc.next.sub <- getIAccNextFromComp(i = i.cell.sub,
                                                      mapping = mapping.to.acc)
                has.later.accession <- i.acc.next.add > 0L
                if (has.later.accession) {
                    min.acc.add <- getMinValCohortAccession(i = i.acc.next.add,
                                                            series = accession,
                                                            iterator = iterator.acc)
                    min.acc.sub <- getMinValCohortAccession(i = i.acc.next.sub,
                                                            series = accession,
                                                            iterator = iterator.acc)
                    if (is.na(min.val.add) || (min.acc.add < min.val.add))
                        min.val.add <- min.acc.add
                    if (is.na(min.val.sub) || (min.acc.sub < min.val.sub))
                        min.val.sub <- min.acc.sub
                }
            }
            th.curr.add <- theta[i.cell.add]
            mean.add <- if (joint.update) th.prop.add else th.curr.add
            val.curr.add <- component[i.cell.add]
            val.curr.sub <- component[i.cell.sub]
            lower <- val.curr.sub - min.val.sub
            upper <- val.curr.add + min.val.add
            if (lower > upper)
                found.value <- FALSE
            else {
                w.add <- w[i.cell.add]
                sd.add <- varsigma / sqrt(w.add)
                val.prop.add <- rnormIntTrunc1(mean = mean.add,
                                               sd = sd.add,
                                               lower = lower,
                                               upper = upper)
                found.value <- !is.na(val.prop.add)
            }
            if (found.value) {
                diff.prop <- unname(val.prop.add - val.curr.add)
                diff.theta <- if (joint.update) th.prop.add - th.curr.add else 0
                generated.new.proposal <- diff.prop != 0L
            }
            else
                generated.new.proposal <- FALSE
        }
        if (generated.new.proposal) {
            combined@generatedNewProposal@.Data <- TRUE
            combined@isSmallUpdate@.Data <- FALSE
            combined@iCell <- i.cell.add
            combined@iCellOther <- i.cell.sub
            combined@iPopnNext <- i.popn.next.add
            combined@iPopnNextOther <- i.popn.next.sub
            if (has.age) {
                combined@iAccNext <- i.acc.next.add
                combined@iAccNextOther <- i.acc.next.sub
                combined@isLowerTriangle@.Data <- is.lower.triangle
            }
            combined@iExposure <- NA_integer_
            combined@iExposureOther <- NA_integer_
            combined@iExpFirst <- i.exp.first.add
            combined@iExpFirstOther <- i.exp.first.sub
            combined@diffProp <- diff.prop
            combined@diffTheta <- diff.theta
        }
        else {
            combined@generatedNewProposal@.Data <- FALSE
            combined@isSmallUpdate@.Data <- FALSE
            combined@iCell <- NA_integer_
            combined@iCellOther <- NA_integer_
            combined@iPopnNext <- NA_integer_
            combined@iPopnNextOther <- NA_integer_
            if (has.age) {
                combined@iAccNext <- NA_integer_
                combined@iAccNextOther <- NA_integer_
                combined@isLowerTriangle@.Data <- NA
            }
            combined@iExposure <- NA_integer_
            combined@iExposureOther <- NA_integer_
            combined@iExpFirst <- NA_integer_
            combined@iExpFirstOther <- NA_integer_
            combined@diffProp <- NA_integer_
            combined@diffTheta <- NA_real_
        }
        combined
    }
}

## TRANSLATED
## HAS_TESTS
updateProposalAccountMoveComp <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(updateProposalAccountMoveComp_R, combined)
    }
    else {
        account <- combined@account
        population <- account@population
        i.comp <- combined@iComp
        component <- account@components[[i.comp]]
        is.increment <- combined@isIncrement[[i.comp]]
        max.attempt <- combined@maxAttempt
        has.age <- combined@hasAge@.Data
        if (has.age) {
            accession <- combined@accession
            iterator.acc <- combined@iteratorAcc
            mapping.to.acc <- combined@mappingsToAcc[[i.comp]]
        }
        mapping.to.popn <- combined@mappingsToPopn[[i.comp]]
        iterator.popn <- combined@iteratorPopn
        uses.exposure <- combined@modelUsesExposure[i.comp + 1L]
        mapping.to.exp <- combined@mappingsToExp[[i.comp]]
        description <- combined@descriptions[[i.comp + 1L]]
        sys.mod <- combined@systemModels[[i.comp + 1L]]
        theta <- sys.mod@theta
        is.net <- combined@isNet[i.comp]
        joint.update <- combined@jointUpdate
        if (joint.update) {
            mu <- sys.mod@mu
            sigma <- sys.mod@sigma
            scale.theta <- sys.mod@scaleTheta@.Data
            lower <- sys.mod@lower
            upper <- sys.mod@upper
            tolerance <- sys.mod@tolerance
        }
        if (is.net) {
            varsigma.comp <- sys.mod@varsigma
            w.comp <- sys.mod@w
            i.cell <- chooseICellComp(description)
            generated.new.proposal <- TRUE
        }
        else {
            struc.zero.array <- sys.mod@strucZeroArray
            box.cox.param <- sys.mod@boxCoxParam
            generated.new.proposal <- FALSE
            for (i in seq_len(max.attempt)) {
                i.cell <- chooseICellComp(description)
                is.struc.zero <- struc.zero.array[i.cell] == 0L
                if (!is.struc.zero) {
                    generated.new.proposal <- TRUE
                    break
                }
            }
        }
        if (generated.new.proposal && joint.update) {
            for (i in seq_len(max.attempt)) {
                tr.th.prop <- rnorm(n = 1L, mean = mu[i], sd = scale.theta * sigma)
                generated.new.proposal <- ((tr.th.prop > lower + tolerance)
                    && (tr.th.prop < upper - tolerance))
                if (generated.new.proposal) {
                    if (!is.net) {
                        if (box.cox.param > 0)
                            th.prop <- (box.cox.param * tr.th.prop + 1) ^ (1 / box.cox.param)
                        else
                            th.prop <- exp(tr.th.prop)
                    }
                    break
                }
            }
        }        
        if (generated.new.proposal) {
            if (has.age)
                is.lower.triangle <- isLowerTriangle(i = i.cell,
                                                     description = description)
            if (uses.exposure) {
                expected.exposure <- combined@expectedExposure
                i.exposure <- getIExposureFromComp(i = i.cell,
                                                   mapping = mapping.to.exp)
            }
            i.exp.first <- getIExpFirstFromComp(i = i.cell,
                                                mapping = mapping.to.exp)
            i.popn.next <- getIPopnNextFromComp(i = i.cell,
                                                mapping = mapping.to.popn)
            if (has.age) {
                is.oldest.age.group <- isOldestAgeGroup(i = i.cell,
                                                        description = description)
                if (is.oldest.age.group && !is.lower.triangle)
                    min.val <- NA_integer_
                else
                    min.val <- getMinValCohortPopulationHasAge(i = i.popn.next,
                                                               population = population,
                                                               accession = accession,
                                                               iterator = iterator.popn)
            }
            else
                min.val <- getMinValCohortPopulationNoAge(i = i.popn.next,
                                                          series = population,
                                                          iterator = iterator.popn)
            if (has.age) {
                i.acc.next <- getIAccNextFromComp(i = i.cell,
                                                  mapping = mapping.to.acc)
                has.later.accession <- i.acc.next > 0L
                if (has.later.accession) {
                    min.acc <- getMinValCohortAccession(i = i.acc.next,
                                                        series = accession,
                                                        iterator = iterator.acc)
                    if (is.na(min.val) || (min.acc < min.val))
                        min.val <- min.acc
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
            th.curr <- theta[i.cell]
            theta.cell <- if (joint.update) th.prop else th.curr
            if (is.net) {
                w.cell <- w.comp[i.cell]
                sd <- varsigma.comp / sqrt(w.cell)
                val.prop <- rnormIntTrunc1(mean = theta.cell,
                                           sd = sd,
                                           lower = lower,
                                           upper = upper)
            }
            else {
                if (uses.exposure) {
                    expected.exposure.cell <- expected.exposure[i.exposure]
                    lambda <- theta.cell * expected.exposure.cell
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
                diff.prop <- unname(val.prop - val.curr)
                diff.theta <- if (joint.update) th.prop - th.curr else 0
                generated.new.proposal <- (diff.prop != 0L) || (diff.theta != 0)
            }
            else
                generated.new.proposal <- FALSE
        }
        if (generated.new.proposal) {
            combined@generatedNewProposal@.Data <- TRUE
            combined@isSmallUpdate@.Data <- FALSE
            combined@iCell <- i.cell
            combined@iCellOther <- NA_integer_
            combined@iPopnNext <- i.popn.next
            combined@iPopnNextOther <- NA_integer_
            if (has.age) {
                combined@iAccNext <- i.acc.next
                combined@iAccNextOther <- NA_integer_
                combined@isLowerTriangle@.Data <- is.lower.triangle
            }
            if (uses.exposure) {
                combined@iExposure <- i.exposure
                combined@iExposureOther <- NA_integer_
            }
            else {
                combined@iExposure <- 0L
                combined@iExposureOther <- NA_integer_
            }
            combined@iExpFirst <- i.exp.first
            combined@iExpFirstOther <- NA_integer_
            combined@diffProp <- diff.prop
            combined@diffTheta <- diff.theta
        }
        else {
            combined@generatedNewProposal@.Data <- FALSE
            combined@isSmallUpdate@.Data <- FALSE
            combined@iCell <- NA_integer_
            combined@iCellOther <- NA_integer_
            combined@iPopnNext <- NA_integer_
            combined@iPopnNextOther <- NA_integer_
            if (has.age) {
                combined@iAccNext <- NA_integer_
                combined@iAccNextOther <- NA_integer_
                combined@isLowerTriangle@.Data <- NA
            }
            combined@iExposure <- NA_integer_
            combined@iExposureOther <- NA_integer_
            combined@iExpFirst <- NA_integer_
            combined@iExpFirstOther <- NA_integer_
            combined@diffProp <- NA_integer_
            combined@diffTheta <- NA_real_
        }
        combined
    }
}




## EXPOSURE
## TRANSLATED
## HAS_TESTS
## function called only if component uses exposure
## (otherwise ratios cancel)
diffLogDensJumpComp <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogDensJumpComp_R, combined)
    }
    else {
        uses.exposure <- combined@modelUsesExposure[i.comp + 1L]
        joint.update <- combined@jointUpdate
        if (!uses.exposure && !joint.update)
            return(0)
        i.comp <- combined@iComp
        i.births <- combined@iBirths
        is.births <- i.comp == i.births
        component <- combined@account@components[[i.comp]]
        theta <- combined@systemModels[[i.comp + 1L]]@theta
        i.cell <- combined@iCell
        diff.prop <- combined@diffProp
        diff.theta <- combined@diffTheta
        has.age <- combined@hasAge@.Data
        age.time.step <- combined@ageTimeStep
        is.increment <- combined@isIncrement
        if (uses.exposure) {
            i.exposure <- combined@iExposure
            i.exp.first <- combined@iExpFirst
            tol.exposure <- 0.000001
            exposure <- combined@exposure
            expected.exposure <- combined@expectedExposure
            exposure.cell.curr <- exposure[i.exposure]
            exposure.cell.jump <- expected.exposure[i.exposure]
        }
        if (is.births)
            exp.changes <- (i.exp.first != 0L) && (i.exp.first == i.exposure)
        else
            exp.changes <- TRUE
        if (uses.exposure) {
            if (exp.changes) {
                if (has.age) {
                    iterator <- combined@iteratorsComp[[i.comp]]
                    iterator <- resetCC(iterator, i = i.cell)
                    n.age <- iterator@nAge
                    last.age.group.open <- iterator@lastAgeGroupOpen
                    i.age <- iterator@iAge
                    i.triangle <- iterator@iTriangle
                    is.final <- i.age == n.age
                    is.upper <- i.triangle == 2L
                    if (is.final && last.age.group.open) {
                        if (is.upper)
                            incr.exp <- 0.5 * age.time.step * diff
                        else
                            incr.exp <- (1/3) * age.time.step * diff
                    }
                    else {
                        if (is.upper)
                            incr.exp <- (1/6) * age.time.step * diff
                        else
                            incr.exp <- (1/3) * age.time.step * diff
                    }
                }
                else
                    incr.exp <- 0.5 * age.time.step * diff
                if (!is.increment[i.comp])
                    incr.exp <- -1 * incr.exp
            }
            else
                incr.exp <- 0
            exposure.cell.prop <- exposure.cell.curr + incr.exp
            if (exposure.cell.prop < tol.exposure) {
                if (exposure.cell.prop > -1 * tol.exposure)
                    exposure.cell.prop <- 0
                else {
                    if (exposure.cell.curr < 0)
                        return(-Inf)
                    stop(sprintf("negative value for 'exposure.cell.prop' : %f", exposure.cell.prop))
                }
            }
        }
        val.curr <- component[i.cell]
        val.prop <- val.curr + diff
        if (uses.exposure && (val.prop > 0) && !(exposure.cell.prop > 0))
            ans <- -Inf ## even if same is true for current value
        else {
            theta.curr <- theta[i.cell]
            if (joint.update) {
                theta.prop <- theta.curr + diff.theta
                if (uses.exposure) {
                    lambda.dens.prop <- theta.prop * exposure.cell.prop
                    lambda.dens.curr <- theta.curr * exposure.cell.curr
                }
                else {

                }
                

                
                diff.log.dens <- (stats::dpois(x = val.prop, lambda = lambda.dens.prop, log = TRUE)
                    - stats::dpois(x = val.curr, lambda = lambda.dens.prop, log = TRUE))
            }
            else {
                lambda.dens.prop <- theta.cell * exposure.cell.prop
                lambda.jump <- theta.cell * exposure.cell.jump
                diff.log.dens <- (stats::dpois(x = val.prop, lambda = lambda.dens.prop, log = TRUE)
                    - stats::dpois(x = val.curr, lambda = lambda.dens.prop, log = TRUE))
                diff.log.jump <- (stats::dpois(x = val.curr, lambda = lambda.jump, log = TRUE)
                    - stats::dpois(x = val.prop, lambda = lambda.jump, log = TRUE))
            }
            ans <- diff.log.dens + diff.log.jump
            ans <- unname(ans)
        }
        ans
    }
}

