
## updateAccount DONE
##     updateProposalAccount [method] DONE
##         updateProposalAccountMovePopn DONE
##         updateProposalAccountMoveBirths DONE
##         updateProposalAccountMoveOrigDest DONE
##         updateProposalAccountMovePool DONE
##         updateProposalAccountMoveNet DONE
##         updateProposalAccountMoveComp DONE
##     diffLogLikAccount [method] DONE
##         diffLogLikAccountMovePopn DONE
##             diffLogLikPopn DONE
##             diffLogLikPopnOneDataset DONE
##                 diffLogLikPopnOneCell DONE
##         diffLogLikAccountMoveOrigDest DONE
##             diffLogLikCellComp DONE
##                 diffLogLikCellOneDataset DONE
##             diffLogLikPopPair DONE
##                 diffLogLikPopnOneDataset [reused] DONE
##                     diffLogLikPopnOneCell [reused] DONE
##         diffLogLikAccountMovePool DONE
##             diffLogLikCellsPool DONE
##                 diffLogLikCellOneDataset [reused] DONE
##             diffLogLikPopPair [reused] DONE
##                 diffLogLikPopnOneDataset [reused] DONE
##                     diffLogLikPopnOneCell [reused] DONE
##         diffLogLikAccountMoveNet DONE
##             diffLogLikCellsNet DONE
##                 diffLogLikCellOneDataset [reused] DONE
##             diffLogLikPopPair [reused] DONE
##                 diffLogLikPopnOneDataset [reused] DONE
##                     diffLogLikPopnOneCell [reused] DONE
##         diffLogLikAccountMoveComp DONE
##             diffLogLikCellComp [reused] DONE
##                 diffLogLikCellOneDataset [reused] DONE
##     diffLogDensAccount [method] DONE
##         diffLogDensPopn DONE
##             diffLogDensPopnOneCohort DONE
##         diffLogDensExpPopn DONE
##             diffLogDensExpOneOrigDestParChPool DONE
##             diffLogDensExpOneComp DONE
##         diffLogDensJumpOrigDest DONE
##         diffLogDensExpOrigDestPoolNet DONE
##             diffLogDensExpOneOrigDestParChPool [reused] DONE
##             diffLogDensExpOneComp [reused] DONE
##         diffLogDensJumpPoolWithExpose DONE
##         diffLogDensJumpPoolNoExpose DONE
##         diffLogDensExpOrigDestPoolNet [reused] DONE
##             diffLogDensExpOneOrigDestParChPool [reused] DONE
##             diffLogDensExpOneComp [reused] DONE
##         diffLogDensJumpNet DONE
##         diffLogDensExpOrigDestPoolNet [reused] DONE
##             diffLogDensExpOneOrigDestParChPool [reused] DONE
##             diffLogDensExpOneComp [reused] DONE
##         diffLogDensJumpComp DONE
##         diffLogDensExpComp DONE
##             diffLogDensExpOneOrigDestParChPool [reused] DONE
##             diffLogDensExpOneComp [reused] DONE
##     updateValuesAccount [method] 
##         updateCellMove DONE
##         updateSubsequentPopnMove DONE
##         updateSubsequentAccMove DONE
##         updateSubsequentExpMove DONE

## Overall update ############################################################

## READY_TO_TRANSLATE
## HAS_TESTS
updateAccount <- function(object, useC = FALSE) {
    stopifnot(methods::is(object, "CombinedAccount"))
    stopifnot(methods::validObject(object))
    if (useC) {
        .Call(updateAccount_R, object)
    }
    else {
        n.cell.account <- object@nCellAccount@.Data
        for (i in seq_len(n.cell.account)) {
            object <- updateProposalAccount(object)
            generated.new.proposal <- object@generatedNewProposal@.Data
            if (generated.new.proposal) {
                diff.log.lik <- diffLogLikAccount(object)
                if (is.finite(diff.log.lik)) {
                    diff.log.dens <- diffLogDensAccount(object)
                    if (is.finite(diff.log.dens)) {
                        log.r <- diff.log.lik + diff.log.dens
                        accept <- (log.r > 0) || (runif(n = 1L) < exp(log.r))
                        if (accept)
                            object <- updateValuesAccount(object)
                    }
                }
            }
        }
        object
    }
}

## Updating proposals ############################################################

## READY_TO_TRANSLATE
## HAS_TESTS
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
        theta <- combined@systemModels[[1L]]@theta
        i.cell <- chooseICellPopn(description)
        i.exposure <- 0L
        i.exp.first <- getIExpFirstFromPopn(i = i.cell,
                                            description = description)
        i.popn.next <- getIPopnNextFromPopn(i = i.cell,
                                            description = description)
        min.val <- getMinValCohortPopulation(i = i.popn.next,
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
                min.val <- min(min.val, min.acc)
            }
        }
        val.curr <- population[i.cell]
        lower <- val.curr - min.val
        upper <- NA_integer_
        lambda <- theta[i.cell]
        val.prop <- rpoisTrunc1(lambda = lambda,
                                lower = lower,
                                upper = upper,
                                maxAttempt = max.attempt)
        found.value <- !is.na(val.prop)
        if (found.value) {
            diff.prop <- val.prop - val.curr
            generated.new.proposal <- diff.prop != 0L
        }
        else
            generated.new.proposal <- FALSE
        combined@generatedNewProposal@.Data <- generated.new.proposal
        if (generated.new.proposal) {
            combined@generatedNewProposal@.Data <- TRUE
            combined@iCell <- i.cell
            combined@iCellOther <- NA_integer_
            combined@iPopnNext <- i.popn.next
            combined@iPopnNextOther <- NA_integer_
            if (has.age) {
                combined@iAccNext <- i.acc.next
                combined@iAccNextOther <- NA_integer_
                combined@isLowerTriangle <- NA
            }
            combined@iExposure <- i.exposure
            combined@iExposureOther <- NA_integer_
            combined@iExpFirst <- i.exp.first
            combined@iExpFirstOther <- NA_integer_
            combined@diffProp <- diff.prop
        }
        else {
            combined@generatedNewProposal@.Data <- FALSE
            combined@iCell <- NA_integer_
            combined@iCellOther <- NA_integer_
            combined@iPopnNext <- NA_integer_
            combined@iPopnNextOther <- NA_integer_
            if (has.age) {
                combined@iAccNext <- NA_integer_
                combined@iAccNextOther <- NA_integer_
                combined@isLowerTriangle <- NA
            }
            combined@iExposure <- NA_integer_
            combined@iExposureOther <- NA_integer_
            combined@iExpFirst <- NA_integer_
            combined@iExpFirstOther <- NA_integer_
            combined@diffProp <- NA_integer_
        }
        combined
    }
}

## READY_TO_TRANSLATE
## HAS_TESTS
updateProposalAccountMoveBirths <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(updateProposalAccountBirths_R, combined)
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
        theta <- combined@systemModels[[i.comp + 1L]]@theta
        i.cell <- chooseICellComp(description)
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
        min.val <- getMinValCohortPopulation(i = i.popn.next,
                                             series = population,
                                             iterator = iterator.popn)        
        if (has.age) {
            i.acc.next <- getIAccNextFromComp(i = i.cell,
                                              mapping = mapping.to.acc)
            has.later.accession <- i.acc.next > 0L
            if (has.later.accession) {
                min.acc <- getMinValCohortAccession(i = i.acc.next,
                                                    series = accession,
                                                    iter = iterator.acc)
                min.val <- min(min.val, min.acc)
            }
        }
        val.curr <- component[i.cell]
        lower <- val.curr - min.val
        upper <- NA_integer_
        theta.cell <- theta[i.cell]
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
        found.value <- !is.na(val.prop)
        if (found.value) {
            diff.prop <- val.prop - val.curr
            generated.new.proposal <- diff.prop != 0L
        }
        else
            generated.new.proposal <- FALSE
        if (generated.new.proposal) {
            combined@generatedNewProposal@.Data <- TRUE
            combined@iCell <- i.cell
            combined@iCellOther <- NA_integer_
            combined@iPopnNext <- i.popn.next
            combined@iPopnNextOther <- NA_integer_
            if (has.age) {
                combined@iAccNext <- i.acc.next
                combined@iAccNextOther <- NA_integer_
                combined@isLowerTriangle <- is.lower.triangle
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
            combined@generatedNewProposal@.Data <- FALSE
            combined@iCell <- NA_integer_
            combined@iCellOther <- NA_integer_
            combined@iPopnNext <- NA_integer_
            combined@iPopnNextOther <- NA_integer_
            if (has.age) {
                combined@iAccNext <- NA_integer_
                combined@iAccNextOther <- NA_integer_
                combined@isLowerTriangle <- NA
            }
            combined@iExposure <- NA_integer_
            combined@iExposureOther <- NA_integer_
            combined@iExpFirst <- NA_integer_
            combined@iExpFirstOther <- NA_integer_
            combined@diffProp <- NA_integer_
        }
        combined
    }
}

## READY_TO_TRANSLATE
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
        theta <- combined@systemModels[[i.comp + 1L]]@theta
        i.cell <- chooseICellComp(description)
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
        min.val.orig <- getMinValCohortPopulation(i = i.popn.next.orig,
                                                  series = population,
                                                  iterator = iterator.popn)
        min.val.dest <- getMinValCohortPopulation(i = i.popn.next.dest,
                                                  series = population,
                                                  iterator = iterator.popn)
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
                min.val.orig <- min(min.val.orig, min.acc.orig)
                min.val.dest <- min(min.val.dest, min.acc.dest)
            }
        }
        val.curr <- component[i.cell]
        lower <- val.curr - min.val.dest
        upper <- val.curr + min.val.orig
        theta.cell <- theta[i.cell]
        if (uses.exposure) {
            expected.exposure.cell <- expected.exposure[i.exposure]
            lambda <- theta.cell * expected.exposure.cell
        }
        else
            lambda <- theta.cell
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
        combined@generatedNewProposal@.Data <- generated.new.proposal
        if (generated.new.proposal) {
            combined@iCell <- i.cell
            combined@iCellOther <- NA_integer_
            combined@iPopnNext <- i.popn.next.orig
            combined@iPopnNextOther <- i.popn.next.dest
            if (has.age) {
                combined@iAccNext <- i.acc.next.orig
                combined@iAccNextOther <- i.acc.next.dest
                combined@isLowerTriangle <- is.lower.triangle
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
        }
        else {
            combined@iCell <- NA_integer_
            combined@iCellOther <- NA_integer_
            combined@iPopnNext <- NA_integer_
            combined@iPopnNextOther <- NA_integer_
            if (has.age) {
                combined@iAccNext <- NA_integer_
                combined@iAccNextOther <- NA_integer_
                combined@isLowerTriangle <- NA
            }
            combined@iExposure <- NA_integer_
            combined@iExposureOther <- NA_integer_
            combined@iExpFirst <- NA_integer_
            combined@iExpFirstOther <- NA_integer_
            combined@diffProp <- NA_integer_
        }
        combined
    }
}

## READY_TO_TRANSLATE
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
        theta <- combined@systemModels[[i.comp + 1L]]@theta
        pair.cell <- chooseICellOutInPool(description)
        i.cell.out <- pair.cell[1L] # 'diffProp' added to this cell
        i.cell.in <- pair.cell[2L]  # 'diffProp' also added to this cell
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
        min.val.out <- getMinValCohortPopulation(i = i.popn.next.out,
                                                 series = population,
                                                 iter = iterator.popn)
        min.val.in <- getMinValCohortPopulation(i = i.popn.next.in,
                                                series = population,
                                                iter = iterator.popn)
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
                min.val.out <- min(min.val.out, min.acc.out)
                min.val.in <- min(min.val.in, min.acc.in)
            }
        }
        val.curr.out <- component[i.cell.out]
        val.curr.in <- component[i.cell.in]
        lower <- val.curr.in - min.val.in
        upper <- val.curr.out + min.val.out
        theta.out <- theta[i.cell.out]
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
            diff.prop <- val.prop.out - val.curr.out
            generated.new.proposal <- diff.prop != 0L
        }
        else
            generated.new.proposal <- FALSE
        combined@generatedNewProposal@.Data <- generated.new.proposal
        if (generated.new.proposal) {
            combined@iCell <- i.cell.out
            combined@iCellOther <- i.cell.in
            combined@iPopnNext <- i.popn.next.out
            combined@iPopnNextOther <- i.popn.next.in
            if (has.age) {
                combined@iAccNext <- i.acc.next.out
                combined@iAccNextOther <- i.acc.next.in
                combined@isLowerTriangle <- is.lower.triangle
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
        }
        else {
            combined@iCell <- NA_integer_
            combined@iCellOther <- NA_integer_
            combined@iPopnNext <- NA_integer_
            combined@iPopnNextOther <- NA_integer_
            if (has.age) {
                combined@iAccNext <- NA_integer_
                combined@iAccNextOther <- NA_integer_
                combined@isLowerTriangle <- NA
            }
            combined@iExposure <- NA_integer_
            combined@iExposureOther <- NA_integer_
            combined@iExpFirst <- NA_integer_
            combined@iExpFirstOther <- NA_integer_
            combined@diffProp <- NA_integer_
        }
        combined
    }
}

## READY_TO_TRANSLATE
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
        theta <- system.model@theta
        varsigma <- system.model@varsigma
        w <- system.model@w
        pair.cell <- chooseICellSubAddNet(description)
        i.cell.add <- pair.cell[1L] # 'diffProp' added to this cell
        i.cell.sub <- pair.cell[2L] # 'diffProp' subtracted from this cell
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
        min.val.add <- getMinValCohortPopulation(i = i.popn.next.add,
                                                 series = population,
                                                 iterator = iterator.popn)
        min.val.sub <- getMinValCohortPopulation(i = i.popn.next.sub,
                                                 series = population,
                                                 iterator = iterator.popn)
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
                min.val.add <- min(min.val.add, min.acc.add)
                min.val.sub <- min(min.val.sub, min.acc.sub)
            }
        }
        mean.add <- theta[i.cell.add]
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
            diff.prop <- val.prop.add - val.curr.add
            generated.new.proposal <- diff.prop != 0L
        }
        else
            generated.new.proposal <- FALSE
        combined@generatedNewProposal@.Data <- generated.new.proposal
        if (generated.new.proposal) {
            combined@iCell <- i.cell.add
            combined@iCellOther <- i.cell.sub
            combined@iPopnNext <- i.popn.next.add
            combined@iPopnNextOther <- i.popn.next.sub
            if (has.age) {
                combined@iAccNext <- i.acc.next.add
                combined@iAccNextOther <- i.acc.next.sub
                combined@isLowerTriangle <- is.lower.triangle
            }
            combined@iExposure <- NA_integer_
            combined@iExposureOther <- NA_integer_
            combined@iExpFirst <- i.exp.first.add
            combined@iExpFirstOther <- i.exp.first.sub
            combined@diffProp <- diff.prop
        }
        else {
            combined@iCell <- NA_integer_
            combined@iCellOther <- NA_integer_
            combined@iPopnNext <- NA_integer_
            combined@iPopnNextOther <- NA_integer_
            if (has.age) {
                combined@iAccNext <- NA_integer_
                combined@iAccNextOther <- NA_integer_
                combined@isLowerTriangle <- NA
            }
            combined@iExposure <- NA_integer_
            combined@iExposureOther <- NA_integer_
            combined@iExpFirst <- NA_integer_
            combined@iExpFirstOther <- NA_integer_
            combined@diffProp <- NA_integer_
        }
        combined
    }
}

## READY_TO_TRANSLATE
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
        theta <- combined@systemModels[[i.comp + 1L]]@theta
        is.net <- combined@isNet[i.comp]
        if (is.net) {
            varsigma.comp <- sys.mod.comp@varsigma
            w.comp <- sys.mod.comp@w
        }
        i.cell <- chooseICellComp(description)
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
        min.val <- getMinValCohortPopulation(i = i.popn.next,
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
        if (is.increment) {
            lower <- val.curr - min.val
            upper <- NA_integer_
        }
        else {
            lower <- NA_integer_
            upper <- val.curr + min.val
        }
        if (is.net) {
            mean <- theta[i.cell]
            w.cell <- w.comp[i.cell]
            sd <- varsigma.comp / sqrt(w.cell)
            val.prop <- rnormIntTrunc1(mean = mean,
                                       sd = sd,
                                       lower = lower,
                                       upper = upper)
        }
        else {
            theta.cell <- theta[i.cell]
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
            diff.prop <- val.prop - val.curr
            generated.new.proposal <- diff.prop != 0L
        }
        else
            generated.new.proposal <- FALSE
        if (generated.new.proposal) {
            combined@generatedNewProposal@.Data <- TRUE
            combined@iCell <- i.cell
            combined@iCellOther <- NA_integer_
            combined@iPopnNext <- i.popn.next
            combined@iPopnNextOther <- NA_integer_
            if (has.age) {
                combined@iAccNext <- i.acc.next
                combined@iAccNextOther <- NA_integer_
                combined@isLowerTriangle <- is.lower.triangle
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
        }
        else {
            combined@generatedNewProposal@.Data <- FALSE
            combined@iCell <- NA_integer_
            combined@iCellOther <- NA_integer_
            combined@iPopnNext <- NA_integer_
            combined@iPopnNextOther <- NA_integer_
            if (has.age) {
                combined@iAccNext <- NA_integer_
                combined@iAccNextOther <- NA_integer_
                combined@isLowerTriangle <- NA
            }
            combined@iExposure <- NA_integer_
            combined@iExposureOther <- NA_integer_
            combined@iExpFirst <- NA_integer_
            combined@iExpFirstOther <- NA_integer_
            combined@diffProp <- NA_integer_
        }
        combined
    }
}


## Log-Likelihood ##################################################################

## TRANSLATED
## HAS_TESTS
diffLogLikAccountMovePopn <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogLikAccountMovePopn_R, combined)
    }
    else {
        account <- combined@account
        population <- account@population
        iterator <- combined@iteratorPopn
        dataModels <- combined@dataModels
        datasets <- combined@datasets
        seriesIndices <- combined@seriesIndices
        transforms <- combined@transforms
        iCell <- combined@iCell
        diff <- combined@diffProp
        diffLogLikPopn(diff = diff,
                       iFirst = iCell,
                       iterator = iterator,
                       population = population,
                       dataModels = dataModels,
                       datasets = datasets,
                       seriesIndices = seriesIndices,
                       transforms = transforms)
    }
}

## TRANSLATED
## HAS_TESTS
diffLogLikPopn <- function(diff, iFirst, iterator, population,
                           dataModels, datasets,
                           seriesIndices, transforms,
                           useC = FALSE) {
    ## diff
    stopifnot(identical(length(diff), 1L))
    stopifnot(is.integer(diff))
    stopifnot(!is.na(diff))
    stopifnot(diff != 0L)
    ## iFirst
    stopifnot(identical(length(iFirst), 1L))
    stopifnot(is.integer(iFirst))
    stopifnot(!is.na(iFirst))
    stopifnot(iFirst > 0L)
    ## iterator
    stopifnot(is(iterator, "CohortIteratorPopulation"))
    ## population
    stopifnot(is(population, "Population"))
    ## dataModels
    stopifnot(is.list(dataModels))
    stopifnot(all(sapply(dataModels, methods::is, "UseExposure")))
    ## datasets
    stopifnot(is.list(datasets))
    stopifnot(all(sapply(datasets, methods::is, "Counts")))
    ## seriesIndices
    stopifnot(is.integer(seriesIndices))
    stopifnot(!any(is.na(seriesIndices)))
    stopifnot(all(seriesIndices >= 0L))
    ## transforms
    stopifnot(is.list(transforms))
    stopifnot(all(sapply(transforms, methods::is, "CollapseTransformExtra")))
    ## dataModels and datasets
    stopifnot(identical(length(dataModels), length(datasets)))
    ## dataModels and seriesIndices
    stopifnot(identical(length(dataModels), length(seriesIndices)))
    ## dataModels and transforms
    stopifnot(identical(length(dataModels), length(transforms)))
    if (useC) {
        .Call(diffLogLikPopn_R,
              diff, iFirst, iterator, population, dataModels,
              datasets, seriesIndices, transforms)
    }
    else {        
        ans <- 0
        for (i.dataset in seq_along(datasets)) {
            assoc.with.popn <- seriesIndices[i.dataset] == 0L
            if (assoc.with.popn) {
                model <- dataModels[[i.dataset]]
                dataset <- datasets[[i.dataset]]
                transform <- transforms[[i.dataset]]
                diff.log.lik <- diffLogLikPopnOneDataset(diff = diff,
                                                         iFirst = iFirst,
                                                         iterator = iterator,
                                                         population = population,
                                                         model = model,
                                                         dataset = dataset,
                                                         transform = transform)
                if (is.infinite(diff.log.lik))
                    return(diff.log.lik)
                ans <- ans + diff.log.lik
            }
        }
        ans
    }
}

## TRANSLATED
## HAS_TESTS
## Time dimension for population has dimscale "Points", so can't be collapsed.
## Each new value for a cohort (which has a new time) is therefore guaranteed
## to have a new value or no value in the corresponding dataset.
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
    stopifnot(is(iterator, "CohortIteratorPopulation"))
    ## model
    stopifnot(is(model, "Model"))
    ## dataset
    stopifnot(is(dataset, "Counts"))
    ## transform
    stopifnot(is(transform, "CollapseTransformExtra"))
    if (useC) {
        .Call(diffLogLikPopnOneDataset_R, diff, iFirst, iterator,
              population, model, dataset, transform)
    }
    else {
        ans <- 0
        i.after <- getIAfter(i = iFirst,
                             transform = transform)
        if (i.after > 0L) {
            ans <- diffLogLikPopnOneCell(iAfter = i.after,
                                         diff = diff,
                                         transform = transform,
                                         population = population,
                                         model = model,
                                         dataset = dataset)
        }
        iterator <- resetCP(iterator, i = iFirst)
        while (!iterator@finished) {
            iterator <- advanceCP(iterator)
            i <- iterator@i
            i.after <- getIAfter(i = i,
                                 transform = transform)
            if (i.after > 0L) {
                ans <- ans + diffLogLikPopnOneCell(iAfter = i.after,
                                                   diff = diff,
                                                   transform = transform,
                                                   population = population,
                                                   model = model,
                                                   dataset = dataset)
            }
        }
        ans
    }
}

## TRANSLATED
## HAS_TESTS
## Have already checked that iAfter > 0
diffLogLikPopnOneCell <- function(iAfter, diff, population, model,
                                  dataset, transform, useC = FALSE) {
    ## iAfter
    stopifnot(identical(length(iAfter), 1L))
    stopifnot(is.integer(iAfter))
    stopifnot(!is.na(iAfter))
    stopifnot(iAfter > 0L)
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
    stopifnot(is.integer(dataset))
    ## transform
    stopifnot(is(transform, "CollapseTransformExtra"))
    if (useC) {
        .Call(diffLogLikPopnOneCell_R,
              iAfter, diff, population,
              model, dataset, transform)
    }
    else {
        cell.has.no.data <- is.na(dataset@.Data[iAfter])
        if (cell.has.no.data)
            return(0)
        vec.i.before <- dembase::getIBefore(i = iAfter,
                                            transform = transform)
        total.popn.curr <- 0L
        for (i.before in  vec.i.before)
            total.popn.curr <- total.popn.curr + population[i.before]
        total.popn.prop <- total.popn.curr + diff
        log.lik.prop <- logLikelihood(model = model,
                                      count = total.popn.prop,
                                      dataset = dataset,
                                      i = iAfter)
        if (is.infinite(log.lik.prop))
            return(log.lik.prop)
        log.lik.curr <- logLikelihood(model = model,
                                      count = total.popn.curr,
                                      dataset = dataset,
                                      i = iAfter)
        log.lik.prop - log.lik.curr
    }
}

## TRANSLATED
## HAS_TESTS
diffLogLikAccountMoveOrigDest <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogLikAccountMoveOrigDest_R, combined)
    }
    else {
        account <- combined@account
        i.comp <- combined@iComp
        component <- account@components[[i.comp]]
        population <- account@population
        iterator <- combined@iteratorPopn
        data.models <- combined@dataModels
        datasets <- combined@datasets
        series.indices <- combined@seriesIndices
        transforms <- combined@transforms
        i.cell <- combined@iCell
        i.popn.orig <- combined@iPopnNext
        i.popn.dest <- combined@iPopnNextOther
        diff <- combined@diffProp
        diff.log.lik.cell <- diffLogLikCellComp(diff = diff,
                                                iComp = i.comp,
                                                iCell = i.cell,
                                                component = component,
                                                dataModels = data.models,
                                                datasets = datasets,
                                                seriesIndices = series.indices,
                                                transforms = transforms)
        if (is.infinite(diff.log.lik.cell))
            return(diff.log.lik.cell)
        diff.log.lik.popn <- diffLogLikPopnPair(diff = diff,
                                                iPopnOrig = i.popn.orig,
                                                iPopnDest = i.popn.dest,
                                                iterator = iterator,
                                                population = population,
                                                dataModels = data.models,
                                                datasets = datasets,
                                                seriesIndices = series.indices,
                                                transforms = transforms)
        diff.log.lik.cell + diff.log.lik.popn
    }
}

## TRANSLATED
## HAS_TESTS
diffLogLikCellComp <- function(diff, iComp, iCell, component,
                               dataModels, datasets,
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
    stopifnot(iComp >= 1L)
    ## iCell
    stopifnot(identical(length(iCell), 1L))
    stopifnot(is.integer(iCell))
    stopifnot(!is.na(iCell))
    stopifnot(iCell >= 1L)
    ## component
    stopifnot(is(component, "Component"))
    ## dataModels
    stopifnot(is.list(dataModels))
    stopifnot(all(sapply(dataModels, methods::is, "Model")))
    ## datasets
    stopifnot(is.list(datasets))
    stopifnot(all(sapply(datasets, methods::is, "Counts")))
    ## seriesIndices
    stopifnot(is.integer(seriesIndices))
    stopifnot(!any(is.na(seriesIndices)))
    stopifnot(all(seriesIndices >= 0L))
    ## transforms
    stopifnot(is.list(transforms))
    stopifnot(all(sapply(transforms, methods::is, "CollapseTransformExtra")))
    ## dataModels and datasets
    stopifnot(identical(length(dataModels), length(datasets)))
    ## dataModels and seriesIndices
    stopifnot(identical(length(dataModels), length(seriesIndices)))
    ## dataModels and transforms
    stopifnot(identical(length(dataModels), length(transforms)))
    if (useC) {
        .Call(diffLogLikCellComp_R,
              diff, iComp, iCell, component, dataModels,
              datasets, seriesIndices, transforms)
    }
    else {
        ans <- 0
        for (i.dataset in seq_along(datasets)) {
            assoc.with.comp <- seriesIndices[i.dataset] == iComp
            if (assoc.with.comp) {
                dataset <- datasets[[i.dataset]]
                model <- dataModels[[i.dataset]]
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

## TRANSLATED
## HAS_TESTS
diffLogLikCellOneDataset <- function(diff, iCell, component,
                                     model, dataset, transform,
                                     useC = FALSE) {
    ## diff
    stopifnot(identical(length(diff), 1L))
    stopifnot(is.integer(diff))
    stopifnot(!is.na(diff))
    ## iCell
    stopifnot(identical(length(iCell), 1L))
    stopifnot(is.integer(iCell))
    stopifnot(!is.na(iCell))
    stopifnot(iCell >= 1L)
    ## component
    stopifnot(is(component, "Component"))
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
        i.after <- getIAfter(i = iCell,
                             transform = transform)
        cell.has.no.data <- (i.after == 0L) || is.na(dataset@.Data[i.after])
        if (cell.has.no.data)
            return(0)
        i.before <- getIBefore(i = i.after,
                               transform = transform)
        total.comp.curr <- sum(component[i.before])
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

## TRANSLATED
## HAS_TESTS
diffLogLikPopnPair <- function(diff, iPopnOrig, iPopnDest,
                               iterator, population,
                               dataModels, datasets,
                               seriesIndices, transforms,
                               useC = FALSE) {
    ## diff
    stopifnot(identical(length(diff), 1L))
    stopifnot(is.integer(diff))
    stopifnot(!is.na(diff))
    stopifnot(diff != 0L)
    ## iPopnOrig
    stopifnot(identical(length(iPopnOrig), 1L))
    stopifnot(is.integer(iPopnOrig))
    stopifnot(!is.na(iPopnOrig))
    stopifnot(iPopnOrig > 0L)
    ## iPopnDest
    stopifnot(identical(length(iPopnDest), 1L))
    stopifnot(is.integer(iPopnDest))
    stopifnot(!is.na(iPopnDest))
    stopifnot(iPopnDest > 0L)
    ## iterator
    stopifnot(is(iterator, "CohortIteratorPopulation"))
    ## population
    stopifnot(is(population, "Population"))
    ## dataModels
    stopifnot(is.list(dataModels))
    stopifnot(all(sapply(dataModels, methods::is, "UseExposure")))
    ## datasets
    stopifnot(is.list(datasets))
    stopifnot(all(sapply(datasets, methods::is, "Counts")))
    ## seriesIndices
    stopifnot(is.integer(seriesIndices))
    stopifnot(!any(is.na(seriesIndices)))
    stopifnot(all(seriesIndices >= 0L))
    ## transforms
    stopifnot(is.list(transforms))
    stopifnot(all(sapply(transforms, methods::is, "CollapseTransformExtra")))
    ## dataModels and datasets
    stopifnot(identical(length(dataModels), length(datasets)))
    ## dataModels and seriesIndices
    stopifnot(identical(length(dataModels), length(seriesIndices)))
    ## dataModels and transforms
    stopifnot(identical(length(dataModels), length(transforms)))
    if (useC) {
        .Call(diffLogLikPopnPair_R,
              diff, iPopnOrig, iPopnDest, iterator,
              population, dataModels, datasets,
              seriesIndices, transforms)
    }
    else {
        if (iPopnOrig == iPopnDest)
            return(0)
        ans <- 0
        for (i.dataset in seq_along(datasets)) {
            assoc.with.popn <- seriesIndices[i.dataset] == 0L
            if (assoc.with.popn) {
                transform <- transforms[[i.dataset]]
                i.after.orig <- getIAfter(i = iPopnOrig, transform = transform)
                i.after.dest <- getIAfter(i = iPopnDest, transform = transform)
                if (i.after.orig != i.after.dest) {
                    model <- dataModels[[i.dataset]]
                    dataset <- datasets[[i.dataset]]
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

## READY_TO_TRANSLATE
## HAS_TESTS
diffLogLikAccountMovePool <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogLikAccountMovePool_R, combined)
    }
    else {
        account <- combined@account
        i.comp <- combined@iComp
        component <- account@components[[i.comp]]
        population <- account@population
        iterator <- combined@iteratorPopn
        data.models <- combined@dataModels
        datasets <- combined@datasets
        series.indices <- combined@seriesIndices
        transforms <- combined@transforms
        i.cell.out <- combined@iCell
        i.cell.in <- combined@iCellOther
        i.popn.out <- combined@iPopnNext
        i.popn.in <- combined@iPopnNextOther
        diff <- combined@diffProp
        diff.log.lik.cells <- diffLogLikCellsPool(diff = diff,
                                                  iCellOut = i.cell.out,
                                                  iCellIn = i.cell.in,
                                                  iComp = i.comp,
                                                  component = component,
                                                  dataModels = data.models,
                                                  datasets = datasets,
                                                  seriesIndices = series.indices,
                                                  transforms = transforms)
        if (is.infinite(diff.log.lik.cells))
            return(diff.log.lik.cells)
        diff.log.lik.popn <- diffLogLikPopnPair(diff = diff,
                                                iPopnOrig = i.popn.out,
                                                iPopnDest = i.popn.in,
                                                iterator = iterator,
                                                population = population,
                                                dataModels = data.models,
                                                datasets = datasets,
                                                seriesIndices = series.indices,
                                                transforms = transforms)
        diff.log.lik.cells + diff.log.lik.popn
    }
}

## READY_TO_TRANSLATE
## HAS_TESTS
diffLogLikCellsPool <- function(diff, iComp, iCellOut, iCellIn,
                               component, dataModels, datasets,
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
    ## component
    stopifnot(is(component, "Component"))
    ## dataModels
    stopifnot(is.list(dataModels))
    stopifnot(all(sapply(dataModels, is, "Model")))
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
    ## dataModels and datasets
    stopifnot(identical(length(dataModels), length(datasets)))
    ## dataModels and seriesIndices
    stopifnot(identical(length(dataModels), length(seriesIndices)))
    ## dataModels and transforms
    stopifnot(identical(length(dataModels), length(transforms)))
    if (useC) {
        .Call(diffLogLikCellsPool_R,
              diff, iComp, iCellOut, iCellIn, component, dataModels,
              datasets, seriesIndices, transforms)
    }
    else {
        ans <- 0
        for (i.dataset in seq_along(datasets)) {
            assoc.with.comp <- seriesIndices[i.dataset] == iComp
            if (assoc.with.comp) {
                dataset <- datasets[[i.dataset]]
                model <- dataModels[[i.dataset]]
                transform <- transforms[[i.dataset]]
                ## 'diff' added to 'out' and 'in' cells
                diff.log.lik.out <- diffLogLikCellOneDataset(diff = diff,
                                                             iCell = iCellOut,
                                                             component = component,
                                                             model = model,
                                                             dataset = dataset,
                                                             transform = transform)
                if (is.infinite(diff.log.lik.out))
                    return(diff.log.lik.out)
                diff.log.lik.in <- diffLogLikCellOneDataset(diff = diff,
                                                            iCell = iCellIn,
                                                            component = component,
                                                            model = model,
                                                            dataset = dataset,
                                                            transform = transform)
                if (is.infinite(diff.log.lik.in))
                    return(diff.log.lik.in)
                ans <- ans + diff.log.lik.out + diff.log.lik.in
            }
        }
        ans
    }
}

## READY_TO_TRANSLATE
## HAS_TESTS
diffLogLikAccountMoveNet <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogLikAccountMoveNet_R, combined)
    }
    else {
        account <- combined@account
        i.comp <- combined@iComp
        component <- account@components[[i.comp]]
        population <- account@population
        iterator <- combined@iteratorPopn
        data.models <- combined@dataModels
        datasets <- combined@datasets
        series.indices <- combined@seriesIndices
        transforms <- combined@transforms
        i.cell.add <- combined@iCell
        i.cell.sub <- combined@iCellOther
        i.popn.add <- combined@iPopnNext
        i.popn.sub <- combined@iPopnNextOther
        diff <- combined@diffProp
        diff.log.lik.cells <- diffLogLikCellsNet(diff = diff,
                                                 iCellAdd = i.cell.add,
                                                 iCellSub = i.cell.sub,
                                                 iComp = i.comp,
                                                 component = component,
                                                 dataModels = data.models,
                                                 datasets = datasets,
                                                 seriesIndices = series.indices,
                                                 transforms = transforms)
        if (is.infinite(diff.log.lik.cells))
            return(diff.log.lik.cells)
        ## 'diffLogLikPopnPair assumes 'diff' is subtracted from first cohort
        ## and added to second, which is what happens with orig-dest and pool.
        ## To instead add and subtract, we use -diff.
        diff.log.lik.popn <- diffLogLikPopnPair(diff = -diff, 
                                                iPopnOrig = i.popn.add,
                                                iPopnDest = i.popn.sub,
                                                iterator = iterator,
                                                population = population,
                                                dataModels = data.models,
                                                datasets = datasets,
                                                seriesIndices = series.indices,
                                                transforms = transforms)
        diff.log.lik.cells + diff.log.lik.popn
    }
}

## READY_TO_TRANSLATE
## HAS_TESTS
diffLogLikCellsNet <- function(diff, iComp, iCellAdd, iCellSub,
                               component, dataModels, datasets,
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
    ## iCellAdd
    stopifnot(identical(length(iCellAdd), 1L))
    stopifnot(is.integer(iCellAdd))
    stopifnot(!is.na(iCellAdd))
    stopifnot(iCellAdd > 0L)
    ## iCellSub
    stopifnot(identical(length(iCellSub), 1L))
    stopifnot(is.integer(iCellSub))
    stopifnot(!is.na(iCellSub))
    stopifnot(iCellSub > 0L)
    ## component
    stopifnot(is(component, "Component"))
    ## dataModels
    stopifnot(is.list(dataModels))
    stopifnot(all(sapply(dataModels, is, "Model")))
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
    ## dataModels and datasets
    stopifnot(identical(length(dataModels), length(datasets)))
    ## dataModels and seriesIndices
    stopifnot(identical(length(dataModels), length(seriesIndices)))
    ## dataModels and transforms
    stopifnot(identical(length(dataModels), length(transforms)))
    if (useC) {
        .Call(diffLogLikCellsNet_R,
              diff, iComp, iCellAdd, iCellSub, component, dataModels,
              datasets, seriesIndices, transforms)
    }
    else {
        ans <- 0
        for (i.dataset in seq_along(datasets)) {
            assoc.with.comp <- seriesIndices[i.dataset] == iComp
            if (assoc.with.comp) {
                dataset <- datasets[[i.dataset]]
                model <- dataModels[[i.dataset]]
                transform <- transforms[[i.dataset]]
                ## 'diff' added to 'add' cell and subtracted from 'sub' cell
                diff.log.lik.add <- diffLogLikCellOneDataset(diff = diff,
                                                             iCell = iCellAdd,
                                                             component = component,
                                                             model = model,
                                                             dataset = dataset,
                                                             transform = transform)
                if (is.infinite(diff.log.lik.add))
                    return(diff.log.lik.add)
                diff.log.lik.sub <- diffLogLikCellOneDataset(diff = -diff,
                                                             iCell = iCellSub,
                                                             component = component,
                                                             model = model,
                                                             dataset = dataset,
                                                             transform = transform)
                if (is.infinite(diff.log.lik.sub))
                    return(diff.log.lik.sub)
                ans <- ans + diff.log.lik.add + diff.log.lik.sub
            }
        }
        ans
    }
}        





## READY_TO_TRANSLATE
## HAS_TESTS
diffLogLikAccountMoveComp <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogLikAccountMoveComp_R, combined)
    }
    else {
        account <- combined@account
        i.comp <- combined@iComp
        component <- combined@account@components[[i.comp]]
        population <- combined@account@population
        iterator <- combined@iteratorPopn
        data.models <- combined@dataModels
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
                                                dataModels = data.models,
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
                                            dataModels = data.models,
                                            datasets = datasets,
                                            seriesIndices = series.indices,
                                            transforms = transforms)
        if (is.infinite(diff.log.lik.popn))
            return(diff.log.lik.popn)
        diff.log.lik.cell + diff.log.lik.popn
    }
}




## LOG DENSITY ################################################################

## READY_TO_TRANSLATE
## HAS_TESTS
diffLogDensPopn <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogDensPopn_R, combined)
    }
    else {
        account <- combined@account
        population <- account@population
        iterator <- combined@iteratorPopn
        theta <- combined@systemModels[[1L]]@theta
        i.popn.next <- combined@iPopnNext
        i.popn.next.other <- combined@iPopnNextOther
        diff <- combined@diffProp
        i.cell <- combined@iCell
        i.comp <- combined@iComp
        i.orig.dest <- combined@iOrigDest
        i.pool <- combined@iPool
        i.int.net <- combined@iIntNet
        is.increment <- combined@isIncrement
        is.popn <- i.comp == 0L
        is.orig.dest <- i.comp == i.orig.dest
        is.pool <- i.comp == i.pool
        is.int.net <- i.comp == i.int.net
        if (is.popn) {
            diffLogDensPopnOneCohort(diff = diff,
                                     population = population,
                                     i = i.cell,
                                     iterator = iterator,
                                     theta = theta)
        }
        else if (is.orig.dest || is.pool) {
            ans.orig <- diffLogDensPopnOneCohort(diff = -diff,
                                                 population = population,
                                                 i = i.popn.next,
                                                 iterator = iterator,
                                                 theta = theta)
            ans.dest <- diffLogDensPopnOneCohort(diff = diff,
                                                 population = population,
                                                 i = i.popn.next.other,
                                                 iterator = iterator,
                                                 theta = theta)
            ans.orig + ans.dest
        }
        else if (is.int.net) {
            ans.add <- diffLogDensPopnOneCohort(diff = diff,
                                                population = population,
                                                i = i.popn.next,
                                                iterator = iterator,
                                                theta = theta)
            ans.sub <- diffLogDensPopnOneCohort(diff = -diff,
                                                population = population,
                                                i = i.popn.next.other,
                                                iterator = iterator,
                                                theta = theta)
            ans.add + ans.sub
        }
        else {
            if (!is.increment[i.comp])
                diff <- -diff
            diffLogDensPopnOneCohort(diff = diff,
                                     population = population,
                                     i = i.popn.next,
                                     iterator = iterator,
                                     theta = theta)
        }
    }
}

## READY_TO_TRANSLATE
## HAS_TESTS
diffLogDensPopnOneCohort <- function(diff, population, i, iterator, theta,
                                     useC = FALSE) {
    ## diff
    stopifnot(identical(length(diff), 1L))
    stopifnot(is.integer(diff))
    stopifnot(!is.na(diff))
    stopifnot(diff != 0L)
    ## population
    stopifnot(methods::is(population, "Population"))
    stopifnot(is.integer(population))
    stopifnot(!any(is.na(population)))
    stopifnot(all(population >= 0L))
    ## i
    stopifnot(identical(length(i), 1L))
    stopifnot(is.integer(i))
    stopifnot(!is.na(i))
    stopifnot(i >= 0L)
    ## iterator
    stopifnot(methods::is(iterator, "CohortIteratorPopulation"))
    ## theta
    stopifnot(is.double(theta))
    stopifnot(!any(is.na(theta)))
    stopifnot(all(theta >= 0))
    ## population and theta
    stopifnot(identical(length(population), length(theta)))
    if (useC) {
        .Call(diffLogDensAccountMove_R, combined)
    }
    iterator <- resetCP(iterator, i = i)
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
        iterator <- advanceCP(iterator)
    }
    ans
}
## READY_TO_TRANSLATE
## HAS_TESTS
## Difference in log-density of current values for
## all components attributable to change in exposure,
## where change in exposure due to change in
## a cell in the initial population.
diffLogDensExpPopn <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogDensExpPopn_R, combined)
    }
    else {
        components <- combined@account@components
        iterators.comp <- combined@iteratorsComp
        model.uses.exposure <- combined@modelUsesExposure
        mappings.from.exp <- combined@mappingsFromExp
        i.exp.first <- combined@iExpFirst
        iterator.exposure <- combined@iteratorExposure
        diff <- combined@diffProp
        i.orig.dest <- combined@iOrigDest
        i.pool <- combined@iPool
        i.births <- combined@iBirths
        i.par.ch <- combined@iParCh
        exposure <- combined@exposure
        systemModels <- combined@systemModels
        has.age <- combined@hasAge
        age.time.step <- combined@ageTimeStep
        ans <- 0
        for (i in seq_along(components)) {
            if (model.uses.exposure[i + 1L]) { ## note that 'net' components never use exposure
                component <- components[[i]]
                theta <- systemModels[[i + 1L]]@theta
                iterator.comp <- iterators.comp[[i]]
                mapping.from.exp <- mappings.from.exp[[i]]
                is.orig.dest <- i == i.orig.dest
                is.pool <- i == i.pool
                is.births <- i == i.births
                is.par.ch <- i == i.par.ch
                if (is.orig.dest || is.pool) {
                    i.cell <- getICellCompFromExp(i = i.exp.first,
                                                  mapping = mapping.from.exp)
                    diff.log <- diffLogDensExpOneOrigDestParChPool(iCell = i.cell,
                                                                   hasAge = has.age,
                                                                   ageTimeStep = age.time.step,
                                                                   updatedPopn = TRUE,
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
                else if (is.births) {
                    i.cell <- getICellBirthsFromExp(i = i.exp.first,
                                                    mapping = mapping.from.exp)
                    cell.is.affected <- i.cell > 0L
                    if (cell.is.affected) {
                        if (is.par.ch)
                            diff.log <- diffLogDensExpOneOrigDestParChPool(iCell = i.cell,
                                                                           hasAge = has.age,
                                                                           ageTimeStep = age.time.step,
                                                                           updatedPopn = TRUE,
                                                                           component = component,
                                                                           theta = theta,
                                                                           iteratorComp = iterator.comp,
                                                                           iExpFirst = i.exp.first,
                                                                           exposure = exposure,
                                                                           iteratorExposure = iterator.exposure,
                                                                           diff = diff)
                        else
                            diff.log <- diffLogDensExpOneComp(iCell = i.cell,
                                                              hasAge = has.age,
                                                              ageTimeStep = age.time.step,
                                                              updatedPopn = TRUE,
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
                }
                else {
                    i.cell <- getICellCompFromExp(i = i.exp.first,
                                                  mapping = mapping.from.exp)
                    diff.log <- diffLogDensExpOneComp(iCell = i.cell,
                                                      hasAge = has.age,
                                                      ageTimeStep = age.time.step,
                                                      updatedPopn = TRUE,
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
            }
        }
        ans
    }
}

## READY_TO_TRANSLATE
## HAS_TESTS
diffLogDensExpOneOrigDestParChPool <- function(iCell, hasAge, ageTimeStep, updatedPopn,
                                               component, theta, iteratorComp, 
                                               iExpFirst, exposure, iteratorExposure,
                                               diff, useC = FALSE) {
    ## iCell
    stopifnot(identical(length(iCell), 1L))
    stopifnot(is.integer(iCell))
    stopifnot(!is.na(iCell))
    stopifnot(iCell > 0L)
    ## hasAge
    stopifnot(identical(length(hasAge), 1L))
    stopifnot(is.logical(hasAge))
    stopifnot(!is.na(hasAge))
    ## ageTimeStep
    stopifnot(is.double(ageTimeStep))
    stopifnot(identical(length(ageTimeStep), 1L))
    stopifnot(!is.na(ageTimeStep))
    stopifnot(ageTimeStep > 0)
    ## updatedPopn
    stopifnot(identical(length(updatedPopn), 1L))
    stopifnot(is.logical(updatedPopn))
    stopifnot(!is.na(updatedPopn))
    ## component
    stopifnot(methods::is(component, "InternalMovementsOrigDest")
              || methods::is(component, "InternalMovementsPool")
              || methods::is(component, "BirthsMovementsHasParentChild"))
    ## theta
    stopifnot(is.double(theta))
    stopifnot(!any(is.na(theta)))
    stopifnot(all(theta >= 0))
    ## iteratorComp
    stopifnot(methods::is(iteratorComp, "CohortIteratorOrigDestParChPool"))
    ## iExpFirst
    stopifnot(identical(length(iExpFirst), 1L))
    stopifnot(is.integer(iExpFirst))
    stopifnot(!is.na(iExpFirst))
    stopifnot(iExpFirst > 0L)
    ## theta
    stopifnot(methods::is(exposure, "Exposure"))
    stopifnot(is.double(exposure))
    stopifnot(!any(is.na(exposure)))
    stopifnot(all(exposure >= 0))
    ## iteratorExposure
    stopifnot(methods::is(iteratorExposure, "CohortIteratorComponent"))
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
              iCell, hasAge, ageTimeStep, updatedPopn,
              component, theta, iteratorComp,
              iExpFirst, exposure, iteratorExposure,
              diff)
    }
    else {    
        iteratorComp <- resetCODPCP(iteratorComp, i = iCell)
        iteratorExposure <- resetCC(iteratorExposure, i = iExpFirst)
        ans <- 0
        length.vec <- iteratorComp@lengthVec
        repeat {
            i.e <- iteratorExposure@i
            i.c.vec <- iteratorComp@iVec
            if (hasAge)
                diff.exposure <- 0.5 * diff * ageTimeStep
            else {
                is.first.cell <- i.e == iExpFirst
                if (is.first.cell && !updatedPopn)
                    diff.exposure <- 0.5 * diff * ageTimeStep
                else
                    diff.exposure <- diff * ageTimeStep
            }
            exposure.curr <- exposure[i.e]
            exposure.prop <- exposure.curr + diff.exposure
            for (j in seq_len(length.vec)) {
                i.c <- i.c.vec[j]
                comp.curr <- component[i.c]
                if ((comp.curr > 0L) && !(exposure.prop > 0))
                    return(-Inf)
                theta.curr <- theta[i.c]
                diff.log.lik <- (dpois(x = comp.curr,
                                       lambda = theta.curr * exposure.prop,
                                       log = TRUE)
                    - dpois(x = comp.curr,
                            lambda = theta.curr * exposure.curr,
                            log = TRUE))
                ans <- ans + diff.log.lik
            }
            if (iteratorComp@finished)
                break
            iteratorComp <- advanceCODPCP(iteratorComp)
            iteratorExposure <- advanceCC(iteratorExposure)
        }
        ans
    }
}

## READY_TO_TRANSLATE
## HAS_TESTS
diffLogDensExpOneComp <- function(iCell, hasAge, ageTimeStep, updatedPopn,
                                  component, theta, iteratorComp, 
                                  iExpFirst, exposure, iteratorExposure,
                                  diff, useC = FALSE) {
    ## iCell
    stopifnot(identical(length(iCell), 1L))
    stopifnot(is.integer(iCell))
    stopifnot(!is.na(iCell))
    stopifnot(iCell > 0L)
    ## hasAge
    stopifnot(identical(length(hasAge), 1L))
    stopifnot(is.logical(hasAge))
    stopifnot(!is.na(hasAge))
    ## ageTimeStep
    stopifnot(is.double(ageTimeStep))
    stopifnot(identical(length(ageTimeStep), 1L))
    stopifnot(!is.na(ageTimeStep))
    stopifnot(ageTimeStep > 0)
    ## updatedPopn
    stopifnot(identical(length(updatedPopn), 1L))
    stopifnot(is.logical(updatedPopn))
    stopifnot(!is.na(updatedPopn))
    ## component
    stopifnot(is(component, "Component"))
    ## theta
    stopifnot(is.double(theta))
    stopifnot(!any(is.na(theta)))
    stopifnot(all(theta >= 0))
    ## iteratorComp
    stopifnot(is(iteratorComp, "CohortIteratorComponent"))
    ## iExpFirst
    stopifnot(identical(length(iExpFirst), 1L))
    stopifnot(is.integer(iExpFirst))
    stopifnot(!is.na(iExpFirst))
    stopifnot(iExpFirst > 0L)
    ## exposure
    stopifnot(is(exposure, "Exposure"))
    stopifnot(is.double(exposure))
    stopifnot(!any(is.na(exposure)))
    stopifnot(all(exposure >= 0))
    ## iteratorExposure
    stopifnot(methods::is(iteratorExposure, "CohortIteratorComponent"))
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
              iCell, hasAge, ageTimeStep, updatedPopn,
              component, theta, iteratorComp,
              iExpFirst, exposure, iteratorExposure, diff)
    }
    else {
        iteratorComp <- resetCC(iteratorComp, i = iCell)
        iteratorExposure <- resetCC(iteratorExposure, i = iExpFirst)
        ans <- 0
        repeat {
            i.c <- iteratorComp@i
            i.e <- iteratorExposure@i
            comp.curr <- component[i.c]
            theta.curr <- theta[i.c]
            if (hasAge)
                diff.exposure <- 0.5 * diff * ageTimeStep
            else {
                is.first.cell <- i.c == iCell
                if (is.first.cell && !updatedPopn)
                    diff.exposure <- 0.5 * diff * ageTimeStep
                else
                    diff.exposure <- diff * ageTimeStep
            }
            exposure.curr <- exposure[i.e]
            exposure.prop <- exposure.curr + diff.exposure
            if ((comp.curr > 0L) && !(exposure.prop > 0)) ## testing for exposure.prop == 0, since exposure.prop should be >= 0
                return(-Inf)
            diff.log.lik <- (dpois(x = comp.curr,
                                   lambda = theta.curr * exposure.prop,
                                   log = TRUE)
                - dpois(x = comp.curr,
                        lambda = theta.curr * exposure.curr,
                        log = TRUE))
            ans <- ans + diff.log.lik
            if (iteratorComp@finished)
                break
            iteratorComp <- advanceCC(iteratorComp)
            iteratorExposure <- advanceCC(iteratorExposure)
        }
        ans
    }
}

## READY_TO_TRANSLATE
## HAS_TESTS
## function called only if component uses exposure
## (otherwise ratios cancel)
diffLogDensJumpOrigDest <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogDensJumpOrigDest_R, combined)
    }
    else {
        i.comp <- combined@iComp
        component <- combined@account@components[[i.comp]]
        theta <- combined@systemModels[[i.comp + 1L]]@theta
        exposure <- combined@exposure
        expected.exposure <- combined@expectedExposure
        i.cell <- combined@iCell
        i.exposure <- combined@iExposure
        diff <- combined@diffProp
        has.age <- combined@hasAge@.Data
        age.time.step <- combined@ageTimeStep
        theta.cell <- theta[i.cell]
        exposure.cell.curr <- exposure[i.exposure]
        exposure.cell.jump <- expected.exposure[i.exposure]
        if (has.age) {
            is.lower.triangle <- combined@isLowerTriangle
            if (is.lower.triangle)
                exposure.cell.prop <- exposure.cell.curr - 0.5 * diff * age.time.step
            else
                exposure.cell.prop <- exposure.cell.curr
        }
        else
            exposure.cell.prop <- exposure.cell.curr - 0.5 * diff * age.time.step
        lambda.dens.prop <- theta.cell * exposure.cell.prop
        lambda.jump <- theta.cell * exposure.cell.jump
        val.curr <- component[i.cell]
        val.prop <- val.curr + diff
        diff.log.dens <- (dpois(x = val.prop, lambda = lambda.dens.prop, log = TRUE)
            - dpois(x = val.curr, lambda = lambda.dens.prop, log = TRUE))
        diff.log.jump <- (dpois(x = val.curr, lambda = lambda.jump, log = TRUE)
            - dpois(x = val.prop, lambda = lambda.jump, log = TRUE))
        diff.log.dens + diff.log.jump
    }
}

## READY_TO_TRANSLATE
## HAS_TESTS
## Difference in log-density of current values for
## all components attributable to change in exposure,
## where the change in exposure is due to a change in
## a cell in an Orig-Dest, Pool, or Net component
## (ie a component that potentially affects two cohorts).
diffLogDensExpOrigDestPoolNet <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogDensExpOrigDestPoolNet_R, combined)
    }
    else {
        components <- combined@account@components
        iterators.comp <- combined@iteratorsComp
        model.uses.exposure <- combined@modelUsesExposure
        mappings.from.exp <- combined@mappingsFromExp
        i.exp.first.orig <- combined@iExpFirst
        i.exp.first.dest <- combined@iExpFirstOther
        iterator.exposure <- combined@iteratorExposure
        diff <- combined@diffProp
        i.comp <- combined@iComp
        i.orig.dest <- combined@iOrigDest
        i.pool <- combined@iPool
        i.int.net <- combined@iIntNet
        i.births <- combined@iBirths
        i.par.ch <- combined@iParCh
        exposure <- combined@exposure
        systemModels <- combined@systemModels
        has.age <- combined@hasAge
        age.time.step <- combined@ageTimeStep
        no.exposure.affected <- (i.exp.first.orig == 0L) || (i.exp.first.orig == i.exp.first.dest)
        if (no.exposure.affected)
            return(0)
        ans <- 0
        comp.is.int.net <- i.comp == i.int.net
        if (comp.is.int.net) {
            diff.orig <- diff
            diff.dest <- -diff
        }
        else {
            diff.orig <- -diff
            diff.dest <- diff
        }
        for (i in seq_along(components)) {
            if (model.uses.exposure[i + 1]) { ## note that 'net' components never use exposure
                component <- components[[i]]
                theta <- systemModels[[i + 1L]]@theta
                iterator.comp <- iterators.comp[[i]]
                mapping.from.exp <- mappings.from.exp[[i]]
                is.orig.dest <- i == i.orig.dest
                is.pool <- i == i.pool
                is.births <- i == i.births
                is.par.ch <- i == i.par.ch
                if (is.orig.dest || is.pool) {
                    i.cell.orig <- getICellCompFromExp(i = i.exp.first.orig,
                                                       mapping = mapping.from.exp)
                    i.cell.dest <- getICellCompFromExp(i = i.exp.first.dest,
                                                       mapping = mapping.from.exp)
                    diff.log.orig <- diffLogDensExpOneOrigDestParChPool(iCell = i.cell.orig,
                                                                        hasAge = has.age,
                                                                        ageTimeStep = age.time.step,
                                                                        updatedPopn = FALSE,
                                                                        component = component,
                                                                        theta = theta,
                                                                        iteratorComp = iterator.comp,
                                                                        iExpFirst = i.exp.first.orig,
                                                                        exposure = exposure,
                                                                        iteratorExposure = iterator.exposure,
                                                                        diff = diff.orig)
                    if (is.infinite(diff.log.orig))
                        return(diff.log.orig)
                    ans <- ans + diff.log.orig
                    diff.log.dest <- diffLogDensExpOneOrigDestParChPool(iCell = i.cell.dest,
                                                                        hasAge = has.age,
                                                                        ageTimeStep = age.time.step,
                                                                        updatedPopn = FALSE,
                                                                        component = component,
                                                                        theta = theta,
                                                                        iteratorComp = iterator.comp,
                                                                        iExpFirst = i.exp.first.dest,
                                                                        exposure = exposure,
                                                                        iteratorExposure = iterator.exposure,
                                                                        diff = diff.dest)
                    if (is.infinite(diff.log.dest))
                        return(diff.log.dest)
                    ans <- ans + diff.log.dest
                }
                else if (is.births) {
                    i.cell.orig <- getICellBirthsFromExp(i = i.exp.first.orig,
                                                         mapping = mapping.from.exp)
                    i.cell.dest <- getICellBirthsFromExp(i = i.exp.first.dest,
                                                         mapping = mapping.from.exp)
                    cell.is.affected <- i.cell.orig > 0L
                    if (cell.is.affected) {
                        if (is.par.ch) {
                            diff.log.orig <- diffLogDensExpOneOrigDestParChPool(iCell = i.cell.orig,
                                                                                hasAge = has.age,
                                                                                ageTimeStep = age.time.step,
                                                                                updatedPopn = FALSE,
                                                                                component = component,
                                                                                theta = theta,
                                                                                iteratorComp = iterator.comp,
                                                                                iExpFirst = i.exp.first.orig,
                                                                                exposure = exposure,
                                                                                iteratorExposure = iterator.exposure,
                                                                                diff = diff.orig)
                            if (is.infinite(diff.log.orig))
                                return(diff.log.orig)
                            ans <- ans + diff.log.orig
                            diff.log.dest <- diffLogDensExpOneOrigDestParChPool(iCell = i.cell.dest,
                                                                                hasAge = has.age,
                                                                                ageTimeStep = age.time.step,
                                                                                updatedPopn = FALSE,
                                                                                component = component,
                                                                                theta = theta,
                                                                                iteratorComp = iterator.comp,
                                                                                iExpFirst = i.exp.first.dest,
                                                                                exposure = exposure,
                                                                                iteratorExposure = iterator.exposure,
                                                                                diff = diff.dest)
                            if (is.infinite(diff.log.dest))
                                return(diff.log.dest)
                            ans <- ans + diff.log.dest
                        }
                        else {
                            diff.log.orig <- diffLogDensExpOneComp(iCell = i.cell.orig,
                                                                   hasAge = has.age,
                                                                   ageTimeStep = age.time.step,
                                                                   updatedPopn = FALSE,
                                                                   component = component,
                                                                   theta = theta,
                                                                   iteratorComp = iterator.comp,
                                                                   iExpFirst = i.exp.first.orig,
                                                                   exposure = exposure,
                                                                   iteratorExposure = iterator.exposure,
                                                                   diff = diff.orig)
                            if (is.infinite(diff.log.orig))
                                return(diff.log.orig)
                            ans <- ans + diff.log.orig
                            diff.log.dest <- diffLogDensExpOneComp(iCell = i.cell.dest,
                                                                   hasAge = has.age,
                                                                   ageTimeStep = age.time.step,
                                                                   updatedPopn = FALSE,
                                                                   component = component,
                                                                   theta = theta,
                                                                   iteratorComp = iterator.comp,
                                                                   iExpFirst = i.exp.first.dest,
                                                                   exposure = exposure,
                                                                   iteratorExposure = iterator.exposure,
                                                                   diff = diff.dest)
                            if (is.infinite(diff.log.dest))
                                return(diff.log.dest)
                            ans <- ans + diff.log.dest
                        }
                    }
                }
                else { ## is net
                    i.cell.orig <- getICellCompFromExp(i = i.exp.first.orig,
                                                       mapping = mapping.from.exp)
                    i.cell.dest <- getICellCompFromExp(i = i.exp.first.dest,
                                                       mapping = mapping.from.exp)
                    diff.log.orig <- diffLogDensExpOneComp(iCell = i.cell.orig,
                                                           hasAge = has.age,
                                                           ageTimeStep = age.time.step,
                                                           updatedPopn = FALSE,
                                                           component = component,
                                                           theta = theta,
                                                           iteratorComp = iterator.comp,
                                                           iExpFirst = i.exp.first.orig,
                                                           exposure = exposure,
                                                           iteratorExposure = iterator.exposure,
                                                           diff = diff.orig)
                    if (is.infinite(diff.log.orig))
                        return(diff.log.orig)
                    ans <- ans + diff.log.orig
                    diff.log.dest <- diffLogDensExpOneComp(iCell = i.cell.dest,
                                                           hasAge = has.age,
                                                           ageTimeStep = age.time.step,
                                                           updatedPopn = FALSE,
                                                           component = component,
                                                           theta = theta,
                                                           iteratorComp = iterator.comp,
                                                           iExpFirst = i.exp.first.dest,
                                                           exposure = exposure,
                                                           iteratorExposure = iterator.exposure,
                                                           diff = diff.dest)
                    if (is.infinite(diff.log.dest))
                        return(diff.log.dest)
                    ans <- ans + diff.log.dest
                }
            }
        }
        ans
    }
}

## READY_TO_TRANSLATE
## HAS_TESTS
diffLogDensJumpPoolWithExpose <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogDensJumpPoolWithExpose_R, combined)
    }
    else {
        i.comp <- combined@iComp
        component <- combined@account@components[[i.comp]]
        theta <- combined@systemModels[[i.comp + 1L]]@theta
        exposure <- combined@exposure
        expected.exposure <- combined@expectedExposure
        i.cell.out <- combined@iCell
        i.cell.in <- combined@iCellOther
        i.exposure.out <- combined@iExposure
        i.exposure.in <- combined@iExposureOther
        diff <- combined@diffProp
        has.age <- combined@hasAge
        age.time.step <- combined@ageTimeStep
        theta.out <- theta[i.cell.out]
        theta.in <- theta[i.cell.in]
        exposure.out.curr <- exposure[i.exposure.out]
        exposure.in.curr <- exposure[i.exposure.in]
        exposure.out.jump <- expected.exposure[i.exposure.out]
        if (has.age) {
            is.lower.triangle <- combined@isLowerTriangle
            if (is.lower.triangle) {
                exposure.out.prop <- exposure.out.curr - 0.5 * diff * age.time.step
                exposure.in.prop <- exposure.in.curr + 0.5 * diff * age.time.step
            }
            else {
                exposure.out.prop <- exposure.out.curr
                exposure.in.prop <- exposure.in.curr
            }
        }
        else {
            exposure.out.prop <- exposure.out.curr - 0.5 * diff * age.time.step
            exposure.in.prop <- exposure.in.curr + 0.5 * diff * age.time.step
        }
        lambda.dens.out.prop <- theta.out * exposure.out.prop
        lambda.dens.in.prop <- theta.in * exposure.in.prop
        lambda.dens.out.curr <- theta.out * exposure.out.curr
        lambda.dens.in.curr <- theta.in * exposure.in.curr
        lambda.jump <- theta.out * exposure.out.jump
        val.out.curr <- component[i.cell.out]
        val.in.curr <- component[i.cell.in]
        val.out.prop <- val.out.curr + diff
        val.in.prop <- val.in.curr + diff
        diff.log.dens <- (dpois(x = val.out.prop, lambda = lambda.dens.out.prop, log = TRUE)
                          - dpois(x = val.out.curr, lambda = lambda.dens.out.curr, log = TRUE)
                          + dpois(x = val.in.prop, lambda = lambda.dens.in.prop, log = TRUE)
                          - dpois(x = val.in.curr, lambda = lambda.dens.in.curr, log = TRUE))
        diff.log.jump <- (dpois(x = val.out.curr, lambda = lambda.jump, log = TRUE)
                          - dpois(x = val.out.prop, lambda = lambda.jump, log = TRUE))
        diff.log.dens + diff.log.jump
    }
}

## READY_TO_TRANSLATE
## HAS_TESTS
diffLogDensJumpPoolNoExpose <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogDensJumpPoolNoExpose_R, combined)
    }
    else {
        i.comp <- combined@iComp
        component <- combined@account@components[[i.comp]]
        theta <- combined@systemModels[[i.comp + 1L]]@theta
        i.cell.in <- combined@iCellOther
        diff <- combined@diffProp
        theta.in <- theta[i.cell.in]
        val.in.curr <- component[i.cell.in]
        val.in.prop <- val.in.curr + diff
        (dpois(x = val.in.prop, lambda = theta.in, log = TRUE)
            - dpois(x = val.in.curr, lambda = theta.in, log = TRUE))
    }
}

## READY_TO_TRANSLATE
## HAS_TESTS
diffLogDensJumpNet <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogDensJumpNet_R, combined)
    }
    else {
        i.comp <- combined@iComp
        component <- combined@account@components[[i.comp]]
        model <- combined@systemModels[[i.comp + 1L]]
        theta <- model@theta
        varsigma <- model@varsigma
        w <- model@w
        i.cell.sub <- combined@iCellOther
        diff <- combined@diffProp
        mean.sub <- theta[i.cell.sub]
        w.sub <- w[i.cell.sub]
        sd.sub <- varsigma / sqrt(w.sub)
        val.sub.curr <- component[i.cell.sub]
        val.sub.prop <- val.sub.curr - diff
        (dnorm(x = val.sub.prop,
               mean = mean.sub,
               sd = sd.sub,
               log = TRUE)
            - dnorm(x = val.sub.curr,
                    mean = mean.sub,
                    sd = sd.sub,
                    log = TRUE))
    }
}

## READY_TO_TRANSLATE
## HAS_TESTS
## function called only if component uses exposure
## (otherwise ratios cancel)
diffLogDensJumpComp <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogDensJumpComp_R, combined)
    }
    else {
        i.comp <- combined@iComp
        component <- combined@account@components[[i.comp]]
        theta <- combined@systemModels[[i.comp + 1L]]@theta
        exposure <- combined@exposure
        expected.exposure <- combined@expectedExposure
        i.cell <- combined@iCell
        i.exposure <- combined@iExposure
        diff <- combined@diffProp
        has.age <- combined@hasAge@.Data
        age.time.step <- combined@ageTimeStep
        is.increment <- combined@isIncrement
        theta.cell <- theta[i.cell]
        exposure.cell.curr <- exposure[i.exposure]
        exposure.cell.jump <- expected.exposure[i.exposure]
        if (has.age) {
            is.lower.triangle <- combined@isLowerTriangle
            if (is.lower.triangle) {
                if (is.increment[i.comp])
                    exposure.cell.prop <- exposure.cell.curr + 0.5 * diff * age.time.step
                else
                    exposure.cell.prop <- exposure.cell.curr - 0.5 * diff * age.time.step
            }
            else
                exposure.cell.prop <- exposure.cell.curr
        }
        else {
            if (is.increment[i.comp])
                exposure.cell.prop <- exposure.cell.curr + 0.5 * diff * age.time.step
            else
                exposure.cell.prop <- exposure.cell.curr - 0.5 * diff * age.time.step
        }
        lambda.dens.prop <- theta.cell * exposure.cell.prop
        lambda.jump <- theta.cell * exposure.cell.jump
        val.curr <- component[i.cell]
        val.prop <- val.curr + diff
        diff.log.dens <- (dpois(x = val.prop, lambda = lambda.dens.prop, log = TRUE)
            - dpois(x = val.curr, lambda = lambda.dens.prop, log = TRUE))
        diff.log.jump <- (dpois(x = val.curr, lambda = lambda.jump, log = TRUE)
            - dpois(x = val.prop, lambda = lambda.jump, log = TRUE))
        diff.log.dens + diff.log.jump
    }
}


## READY_TO_TRANSLATE
## HAS_TESTS
## Difference in log-density of current values for
## all components attributable to change in exposure,
## where change in exposure due to change in
## cell in an ordinary component (ie a component
## that only potentially affects one cohort)
diffLogDensExpComp <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogDensExpComp_R, combined)
    }
    else {
        components <- combined@account@components
        iterators.comp <- combined@iteratorsComp
        model.uses.exposure <- combined@modelUsesExposure
        mappings.from.exp <- combined@mappingsFromExp
        i.exp.first <- combined@iExpFirst
        iterator.exposure <- combined@iteratorExposure
        is.increment <- combined@isIncrement
        diff <- combined@diffProp
        i.comp <- combined@iComp
        i.orig.dest <- combined@iOrigDest
        i.pool <- combined@iPool
        i.births <- combined@iBirths
        i.par.ch <- combined@iParCh
        exposure <- combined@exposure
        systemModels <- combined@systemModels
        has.age <- combined@hasAge
        age.time.step <- combined@ageTimeStep
        no.exposure.affected <- i.exp.first == 0L
        if (no.exposure.affected)
            return(0)
        ans <- 0
        if (!is.increment[i.comp])
            diff <- -diff
        for (i in seq_along(components)) {
            if (model.uses.exposure[i + 1L]) { ## note that 'net' components never use exposure
                component <- components[[i]]
                theta <- systemModels[[i + 1L]]@theta
                iterator.comp <- iterators.comp[[i]]
                mapping.from.exp <- mappings.from.exp[[i]]
                is.orig.dest <- i == i.orig.dest
                is.pool <- i == i.pool
                is.births <- i == i.births
                is.par.ch <- i == i.par.ch
                if (is.orig.dest || is.pool) {
                    i.cell <- getICellCompFromExp(i = i.exp.first,
                                                  mapping = mapping.from.exp)
                    diff.log <- diffLogDensExpOneOrigDestParChPool(iCell = i.cell,
                                                                   hasAge = has.age,
                                                                   ageTimeStep = age.time.step,
                                                                   updatedPopn = FALSE,
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
                else if (is.births) {
                    i.cell <- getICellBirthsFromExp(i = i.exp.first,
                                                    mapping = mapping.from.exp)
                    cell.is.affected <- i.cell > 0L
                    if (cell.is.affected) {
                        if (is.par.ch)
                            diff.log <- diffLogDensExpOneOrigDestParChPool(iCell = i.cell,
                                                                           hasAge = has.age,
                                                                           ageTimeStep = age.time.step,
                                                                           updatedPopn = FALSE,
                                                                           component = component,
                                                                           theta = theta,
                                                                           iteratorComp = iterator.comp,
                                                                           iExpFirst = i.exp.first,
                                                                           exposure = exposure,
                                                                           iteratorExposure = iterator.exposure,
                                                                           diff = diff)
                        else
                            diff.log <- diffLogDensExpOneComp(iCell = i.cell,
                                                              hasAge = has.age,
                                                              ageTimeStep = age.time.step,
                                                              updatedPopn = FALSE,
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
                }
                else {
                    i.cell <- getICellCompFromExp(i = i.exp.first,
                                                  mapping = mapping.from.exp)
                    diff.log <- diffLogDensExpOneComp(iCell = i.cell,
                                                      hasAge = has.age,
                                                      ageTimeStep = age.time.step,
                                                      updatedPopn = FALSE,
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
            }
        }
        ans
    }
}






## UPDATE VALUES ################################################################


## READY_TO_TRANSLATE
## HAS_TESTS
updateCellMove <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(updateCellMove_R, combined)
    }
    else {
        i.comp <- combined@iComp
        i.cell <- combined@iCell
        i.cell.other <- combined@iCellOther
        i.pool <- combined@iPool
        i.int.net <- combined@iIntNet
        diff <- combined@diffProp
        is.popn <- i.comp == 0L
        is.pool <- i.comp == i.pool
        is.int.net <- i.comp == i.int.net
        if (is.popn) {
            combined@account@population[i.cell] <- combined@account@population[i.cell] + diff
        }
        else if (is.pool) {
            combined@account@components[[i.comp]][i.cell] <-
                combined@account@components[[i.comp]][i.cell] + diff
            combined@account@components[[i.comp]][i.cell.other] <-
                combined@account@components[[i.comp]][i.cell.other] + diff
        }
        else if (is.int.net) {
            combined@account@components[[i.comp]][i.cell] <-
                combined@account@components[[i.comp]][i.cell] + diff
        combined@account@components[[i.comp]][i.cell.other] <-
            combined@account@components[[i.comp]][i.cell.other] - diff
        }
        else {
            combined@account@components[[i.comp]][i.cell] <-
                combined@account@components[[i.comp]][i.cell] + diff
        }
        combined
    }
}            



## READY_TO_TRANSLATE
## HAS_TESTS
## there is always at least one subsequent population value to update
updateSubsequentPopnMove <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(updateSubsequentPopnMove_R, combined)
    }
    else {
        i.comp <- combined@iComp
        i.orig.dest <- combined@iOrigDest
        i.pool <- combined@iPool
        i.int.net <- combined@iIntNet
        i.popn.next <- combined@iPopnNext
        iterator <- combined@iteratorPopn
        diff <- combined@diffProp
        is.increment <- combined@isIncrement
        i.comp <- combined@iComp
        i.orig.dest <- combined@iOrigDest
        i.pool <- combined@iPool
        i.int.net <- combined@iIntNet
        is.popn <- i.comp == 0L
        is.orig.dest <- i.comp == i.orig.dest
        is.pool <- i.comp == i.pool
        is.int.net <- i.comp == i.int.net
        update.two.cohorts <- (is.orig.dest || is.pool || is.int.net)
        if (update.two.cohorts)
            i.popn.next.other <- combined@iPopnNextOther
        if (is.popn) {
            iterator <- resetCP(iterator, i = i.popn.next)
            repeat {
                i <- iterator@i
                combined@account@population[i] <- combined@account@population[i] + diff
                if (iterator@finished)
                    break
                iterator <- advanceCP(iterator)
            }
        }
        else if (update.two.cohorts) {
            if (is.orig.dest || is.pool) {
                diff.orig <- -diff
                diff.dest <- diff
            }
            else { # is.int.net
                diff.orig <- diff
                diff.dest <- -diff
            }
            iterator.orig <- resetCP(iterator, i = i.popn.next)
            iterator.dest <- resetCP(iterator, i = i.popn.next.other)
            repeat {
                i.orig <- iterator.orig@i
                i.dest <- iterator.dest@i
                combined@account@population[i.orig] <- combined@account@population[i.orig] + diff.orig
                combined@account@population[i.dest] <- combined@account@population[i.dest] + diff.dest
                if (iterator.orig@finished)
                    break
                iterator.orig <- advanceCP(iterator.orig)
                iterator.dest <- advanceCP(iterator.dest)
            }
        }
        else {
            if (!is.increment[i.comp])
                diff <- -diff
            iterator <- resetCP(iterator, i = i.popn.next)
            repeat {
                i <- iterator@i
                combined@account@population[i] <- combined@account@population[i] + diff
                if (iterator@finished)
                    break
                iterator <- advanceCP(iterator)
            }
        }
        combined
    }
}

## READY_TO_TRANSLATE
## HAS_TESTS
updateSubsequentAccMove <- function(combined, useC = FALSE) {
    stopifnot(is(combined, "CombinedAccountMovementsHasAge"))
    if (useC) {
        .Call(updateSubsequentAccMove_R, combined)
    }
    else {
        i.comp <- combined@iComp
        i.orig.dest <- combined@iOrigDest
        i.pool <- combined@iPool
        i.int.net <- combined@iIntNet
        i.acc.next <- combined@iAccNext
        iterator <- combined@iteratorAcc
        diff <- combined@diffProp
        is.increment <- combined@isIncrement
        i.comp <- combined@iComp
        i.orig.dest <- combined@iOrigDest
        i.pool <- combined@iPool
        i.int.net <- combined@iIntNet
        no.subsequent.accession <- i.acc.next == 0L
        if (no.subsequent.accession)
            return(combined)
        is.popn <- i.comp == 0L
        is.orig.dest <- i.comp == i.orig.dest
        is.pool <- i.comp == i.pool
        is.int.net <- i.comp == i.int.net
        update.two.cohorts <- (is.orig.dest || is.pool || is.int.net)
        if (update.two.cohorts)
            i.acc.next.other <- combined@iAccNextOther
        if (is.popn) {
            iterator <- resetCA(iterator, i = i.acc.next)
            repeat {
                i <- iterator@i
                combined@accession[i] <- combined@accession[i] + diff
                if (iterator@finished)
                    break
                iterator <- advanceCA(iterator)
            }
        }
        else if (update.two.cohorts) {
            if (is.orig.dest || is.pool) {
                diff.orig <- -diff
                diff.dest <- diff
            }
            else { # is.int.net
                diff.orig <- diff
                diff.dest <- -diff
            }
            iterator.orig <- resetCA(iterator, i = i.acc.next)
            iterator.dest <- resetCA(iterator, i = i.acc.next.other)
            repeat {
                i.orig <- iterator.orig@i
                i.dest <- iterator.dest@i
                combined@accession[i.orig] <- combined@accession[i.orig] + diff.orig
                combined@accession[i.dest] <- combined@accession[i.dest] + diff.dest
                if (iterator.orig@finished)
                    break
                iterator.orig <- advanceCA(iterator.orig)
                iterator.dest <- advanceCA(iterator.dest)
            }
        }
        else {
            if (!is.increment[i.comp])
                diff <- -diff
            iterator <- resetCA(iterator, i = i.acc.next)
            repeat {
                i <- iterator@i
                combined@accession[i] <- combined@accession[i] + diff
                if (iterator@finished)
                    break
                iterator <- advanceCA(iterator)
            }
        }
        combined
    }
}
## READY_TO_TRANSLATE
## HAS_TESTS
updateSubsequentExpMove <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(updateSubsequentExpMove_R, combined)
    }
    else {
        i.comp <- combined@iComp
        i.orig.dest <- combined@iOrigDest
        i.pool <- combined@iPool
        i.int.net <- combined@iIntNet
        i.exp.first <- combined@iExpFirst
        iterator <- combined@iteratorExposure
        diff <- combined@diffProp
        is.increment <- combined@isIncrement
        has.age <- combined@hasAge
        age.time.step <- combined@ageTimeStep
        no.subsequent.exposure <- i.exp.first == 0L
        if (no.subsequent.exposure)
            return(combined)
        is.popn <- i.comp == 0L
        is.orig.dest <- i.comp == i.orig.dest
        is.pool <- i.comp == i.pool
        is.int.net <- i.comp == i.int.net
        update.two.cohorts <- (is.orig.dest || is.pool || is.int.net)
        if (update.two.cohorts)
            i.exp.first.other <- combined@iExpFirstOther
        if (is.popn) {
            if (has.age)
                diff <- 0.5 * diff * age.time.step
            else
                diff <- diff * age.time.step
            iterator <- resetCC(iterator, i = i.exp.first)
            repeat {
                i <- iterator@i
                combined@exposure[i] <- combined@exposure[i] + diff
                if (iterator@finished)
                    break
                iterator <- advanceCC(iterator)
            }
        }
        else if (update.two.cohorts) {
            if (is.orig.dest || is.pool) {
                diff.orig <- -0.5 * diff * age.time.step
                diff.dest <- 0.5 * diff * age.time.step
            }
            else { # is.int.net
                diff.orig <- 0.5 * diff * age.time.step
                diff.dest <- -0.5 * diff * age.time.step
            }
            iterator.orig <- resetCC(iterator, i = i.exp.first)
            iterator.dest <- resetCC(iterator, i = i.exp.first.other)
            combined@exposure[i.exp.first] <- combined@exposure[i.exp.first] + diff.orig
            combined@exposure[i.exp.first.other] <- combined@exposure[i.exp.first.other] + diff.dest
            if (!has.age) {
                diff.orig <- 2 * diff.orig
                diff.dest <- 2 * diff.dest
            }
            while (!iterator.orig@finished) {
                iterator.orig <- advanceCC(iterator.orig)
                iterator.dest <- advanceCC(iterator.dest)
                i.orig <- iterator.orig@i
                i.dest <- iterator.dest@i
                combined@exposure[i.orig] <- combined@exposure[i.orig] + diff.orig
                combined@exposure[i.dest] <- combined@exposure[i.dest] + diff.dest
            }
        }
        else {
            if (is.increment[i.comp])
                diff <- 0.5 * diff * age.time.step
            else
                diff <- -0.5 * diff * age.time.step
            iterator <- resetCC(iterator, i = i.exp.first)
            combined@exposure[i.exp.first] <- combined@exposure[i.exp.first] + diff
            if (!has.age)
                diff <- 2 * diff
            while (!iterator@finished) {
                iterator <- advanceCC(iterator)
                i <- iterator@i
                combined@exposure[i] <- combined@exposure[i] + diff
            }
        }
        combined
    }
}
