

## identical for transition
setMethod("updateCombined",
          signature(object = "CombinedAccount"),
          function(object, nUpdate = 1L, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## nUpdate
              stopifnot(identical(length(nUpdate), 1L))
              stopifnot(is.integer(nUpdate))
              stopifnot(!is.na(nUpdate))
              stopifnot(nUpdate >= 0L)
              if (useC) {
                  if (useSpecific)
                      .Call(updateCombined_CombinedAccount_R, object, nUpdate)
                  else
                      .Call(updateCombined_R, object, nUpdate)
              }
              else {
                  for (i in seq_len(nUpdate)) {
                      object <- updateAccount(object)
                      object <- updateSystem(object)
                      object <- updateObservationAccount(object)
                  }
                  object
              }
          })

## identical for transition
setMethod("updateAccount",
          signature(object = "CombinedAccount"),
          function(object, useC = FALSE) {
              stopifnot(validObject(object))
              if (useC) {
                  if (useSpecific)
                      .Call(updateAccount_CombinedAccount_R, object)
                  else
                      .Call(updateAccount_R, object)
              }
              else {
                  n.cell.account <- object@account@nCellAccount@.Data
                  for (i in seq_len(n.cell.account)) {
                      object <- updateProposalAccount(object)
                      generated.new.proposal <- object@generatedNewProposal@.Data
                      if (generated.new.proposal) {
                          diff.log.lik <- diffLogLikAccount(object)
                          if (is.finite(diff.log.lik)) {
                              diff.log.dens <- diffLogDensAccount(object)
                              log.r <- diff.log.lik + diff.log.dens
                              accept <- (log.r > 0) || (runif(n = 1L) < exp(log.r))
                              if (accept)
                                  object <- updateValuesAccount(object)
                          }
                      }
                  }
                  object
              }
          })


setMethod("updateProposalAccount",
          signature(object = "CombinedAccountMovements"),
          function(object, useC = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(updateProposalAccount_CombinedAccountMovements_R, object)
                  else
                      .Call(updateProposalAccount_R, object)
              }
              else {
                  account <- object@account
                  prob.popn <- account@probPopn
                  update.popn <- runif(n = 1L) < prob.popn
                  if (update.popn) {
                      object@iComp <- 0L
                      updateProposalAccountMovePopn(object)
                  }
                  else {
                      cum.prob <- object@cumProbComp
                      i.births <- object@iBirths
                      i.orig.dest <- object@iOrigDest
                      i.pool <- object@iPool
                      i.int.net <- object@iIntNet
                      i.comp <- rcateg1(cum.prob)
                      object@iComp <- i.comp
                      if (i.comp = i.births)
                          updateProposalAccountMoveBirths(object)
                      else if (i.comp == i.orig.dest)
                          updateProposalAccountMoveOrigDest(object)
                      else if (i.comp == i.pool)
                          updateProposalAccountMovePool(object)
                      else if (i.comp == i.int.net)
                          updateProposalAccountMoveNet(object)
                      else
                          updateProposalAccountMoveComp(object)
                  }
              }
          })

updateProposalAccountBirths <- function(combined, useC = FALSE) {
    stopifnot(is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(updateProposalAccountMoveOrigDest_R, combined)
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
        sys.mod.popn <- combined@systemModels[[1L]]
        sys.mod.comp <- combined@systemModels[[i.comp + 1L]]
        theta.popn <- sys.mod.popn@theta
        theta.comp <- sys.mod.comp@theta
        max.attempt <- combined@maxAttempt
        i.cell <- chooseICellComp(description.comp)
        i.exp.first <- getIExpFirstFromBirths(i = i.cell,
                                              mapping = mapping.exposure)
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
            is.lower.triangle <- isLowerTriangle(i = i.cell,
                                                 description = description.comp)
        }
        val.curr <- component[i.cell]
        lower <- val.curr - min.val
        upper <- NA_integer_
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
            combined@iPopnNext <- i.popn.next
            combined@iPopnNextOther <- NA_integer_
            if (uses.age) {
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
            combined@iCell <- NA_integer_
            combined@iCellOther <- NA_integer_
            combined@iPopnNext <- NA_integer_
            combined@iPopnNextOther <- NA_integer_
            if (uses.age) {
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


updateProposalAccountMoveOrigDest <- function(combined, useC = FALSE) {
    stopifnot(is(combined, "CombinedAccountMovements"))
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
        sys.mod.popn <- combined@systemModels[[1L]]
        sys.mod.comp <- combined@systemModels[[i.comp + 1L]]
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
    stopifnot(is(combined, "CombinedAccountMovements"))
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
        sys.mod.popn <- combined@systemModels[[1L]]
        sys.mod.comp <- combined@systemModels[[i.comp + 1L]]
        theta.popn <- sys.mod.popn@theta
        theta.comp <- sys.mod.comp@theta
        max.attempt <- combined@maxAttempt
        pair.cell <- chooseICellOutInPool(description.comp)
        i.cell.out <- pair.cell[1L]
        i.cell.in <- pair.cell[2L]
        i.exp.first.out <- getIExpFirstFromComp(i = i.cell.out,
                                                mapping = mapping.exposure)
        i.exp.first.in <- getIExpFirstFromComp(i = i.cell.in,
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
    stopifnot(is(combined, "CombinedAccountMovements"))
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
        sys.mod.popn <- combined@systemModels[[1L]]
        sys.mod.comp <- combined@systemModels[[i.comp + 1L]]
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

## LOG LIKELIHOOOD #########################################################

diffLogLikAccountMove <- function(combined) {
    stopifnot(is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogLikAccountMove_R, combined)
    }
    else {
        i.comp <- combined@iComp
        i.orig.dest <- combined@iOrigDest
        i.pool <- combined@iPool
        i.int.net <- combined@iIntNet
        if (i.comp == 0L)
            diffLogLikAccountMovePopn(combined)
        else if (i.comp == i.orig.dest)
            diffLogLikAccountMoveOrigDest(combined)
        else if (i.comp == i.pool) 
            diffLogLikAccountMovePool(combined)
        else if (i.comp == i.int.net) 
            diffLogLikAccountMoveNet(combined)
        else
            diffLogLikAccountMoveComp(combined)
    }
}

diffLogLikAccountMoveOrigDest <- function(combined, useC = TRUE) {
    stopifnot(is(combined, "CombinedAccountMovements"))
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
    stopifnot(is(combined, "CombinedAccountMovements"))
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
    stopifnot(is(combined, "CombinedAccountMovements"))
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
  

diffLogLikPopnPair <- function(diff, iPopnOrig, iPopnDest,
                               iterator, population,
                               observationModels, datasets,
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
    ## observationModels
    stopifnot(is.list(observationModels))
    stopifnot(all(sapply(observationModels, is, "Model")))
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
    ## observationModels and datasets
    stopifnot(identical(length(observationModels), length(datasets)))
    ## observationModels and seriesIndices
    stopifnot(identical(length(observationModels), length(seriesIndices)))
    ## observationModels and transforms
    stopifnot(identical(length(observationModels), length(transforms)))
    if (useC) {
        .Call(diffLogLikPopnPair_R,
              diff, iPopnOrig, iPopnDest, iterator,
              population, observationModels, datasets,
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
                    model <- observationModels[[i.dataset]]
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

diffLogLikCellsPoolNet <- function(diff, iComp, iCellOut, iCellIn,
                                   isPool, component, observationModels, datasets,
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
    ## observationModels
    stopifnot(is.list(observationModels))
    stopifnot(all(sapply(observationModels, is, "Model")))
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
    ## observationModels and datasets
    stopifnot(identical(length(observationModels), length(datasets)))
    ## observationModels and seriesIndices
    stopifnot(identical(length(observationModels), length(seriesIndices)))
    ## observationModels and transforms
    stopifnot(identical(length(observationModels), length(transforms)))
    if (useC) {
        .Call(diffLogLikCellsPool_R,
              diff, iComp, iCellOut, iCellIn, component, observationModels,
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
                    model <- observationModels[[i.dataset]]
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
        



## LOG DENSITY ################################################################

diffLogDensAccountMove <- function(combined, useC = FALSE) {
    stopifnot(is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogDensAccountMove_R, combined)
    }
    else {
        i.comp <- combined@iComp
        i.orig.dest <- combined@iOrigDest
        i.pool <- combined@iPool
        i.int.net <- combined@iIntNet
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
        else if (i.comp == i.int.net) {
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
    stopifnot(is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogDensPopn_R, combined)
    }
    else {
        account <- combined@account
        population <- account@population
        iterator <- combined@iteratorPopn
        theta <- combined@systemModels@model@theta
        i.popn <- combined@iPopn
        i.popn.oth <- combined@iPopnOth
        diff <- combined@diffProp
        i.comp <- combined@iComp
        i.orig.dest <- combined@iOrigDest
        i.pool <- combined@iPool
        i.int.net <- combined@iIntNet
        is.increment <- combined@isIncrement
        update.two.cohorts <- ((i.comp == i.orig.dest)
                               || (i.comp == i.pool)
                               || (i.comp == i.int.net))
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

diffLogDensJumpOrigDest <- function(combined, useC = FALSE) {
    stopifnot(is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogDensJumpOrigDest_R, combined)
    }
    else {
        i.comp <- combined@iComp
        account <- combined@account
        component <- account@components[[i.comp]]
        systemModels <- combined@systemModels
        sys.mod <- systemModels[[i.comp + 1L]]
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
    stopifnot(is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogDensJumpPoolWithExpose_R, combined)
    }
    else {
        i.comp <- combined@iComp
        account <- combined@account
        component <- account@components[[i.comp]]
        systemModels <- combined@systemModels
        sys.mod <- systemModels[[i.comp + 1L]]
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
    stopifnot(is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogDensJumpPoolNoExpose_R, combined)
    }
    else {
        i.comp <- combined@iComp
        account <- combined@account
        component <- account@components[[i.comp]]
        systemModels <- combined@systemModels
        sys.mod <- systemModels[[i.comp + 1L]]
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
    stopifnot(is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogDensJumpNet_R, combined)
    }
    else {
        i.comp <- combined@iComp
        account <- combined@account
        component <- account@components[[i.comp]]
        systemModels <- combined@systemModels
        sys.mod <- systemModels[[i.comp + 1L]]
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
    stopifnot(is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogDensJumpAccountMoveComp_R, combined)
    }
    else {
        i.comp <- combined@iComp
        account <- combined@account
        component <- account@components[[i.comp]]
        systemModels <- combined@systemModels
        sys.mod <- systemModels[[i.comp + 1L]]
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
    stopifnot(is(combined, "CombinedAccountMovements"))
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
        systemModels <- combined@systemModels
        ans <- 0
        for (i in seq_along(components)) {
            if (uses.exposure[i]) {
                component <- components[[i]]
                theta <- systemModels[[i + 1L]]@theta
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
    stopifnot(is(combined, "CombinedAccountMovements"))
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
        systemModels <- combined@systemModels
        ans <- 0
        for (i in seq_along(components)) {
            if (uses.exposure[i]) {
                component <- components[[i]]
                theta <- systemModels[[i + 1L]]@theta
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
    stopifnot(is(combined, "CombinedAccountMovements"))
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
        systemModels <- combined@systemModels
        ans <- 0
        if (is.increment[i.comp])
            diff <- -diff
        for (i in seq_along(components)) {
            if (uses.exposure[i]) {
                component <- components[[i]]
                theta <- systemModels[[i + 1L]]@theta
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
        i.int.net <- combined@iIntNet
        update.two.cells <- (i.comp == i.pool) || (i.comp == i.int.net)
        if (update.two.cells) {
            i.cell.other <- combined@iCellOther
            if (i.comp == i.pool)
                combined@account@components[[i.comp]][i.cell.other] <-
                    (combined@account@components[[i.comp]][i.cell.other]
                     + diff)
            if (i.comp == i.int.net)
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
    i.int.net <- combined@iIntNet
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
        is.int.net <- i.comp == i.int.net
        update.two.cohorts <- (is.orig.dest || is.pool || is.int.net)
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
##     i.int.net <- combined@iIntNet
##     i.cell <- combined@iCell
##     i.cell.other <- combined@iCellOther
##     is.popn <- i.comp == 0L {
##         if (is.popn) {
            
    
##     if (is
##     i.exposure <- getIExposure(i = i.cell, mapping = mapping[[i.comp]])
    
##     is.orig.dest <- i.comp == i.orig.dest
##     is.pool <- i.comp == i.pool
##     is.int.net <- i.comp == i.int.net
##     update.two.cohorts <- (is.orig.dest || is.pool || is.int.net)
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
##     i.int.net <- combined@iIntNet
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
##         is.int.net <- i.comp == i.int.net
##         update.two.cohorts <- (is.orig.dest || is.pool || is.int.net)
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
    i.int.net <- combined@iIntNet
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
        is.int.net <- i.comp == i.int.net
        update.two.cohorts <- (is.orig.dest || is.pool || is.int.net)
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
    systemModels <- combined@systemModels
    account <- combined@account
    population <- account@population
    components <- account@components
    mappings <- combined@mappingsToPopn
    has.age <- combined@hasAge
    model.popn <- systemModels[[1L]]
    model.popn <- updateModelNotUseExp(model.popn, y = population)
    systemModels[[1L]] <- model.popn
    for (i in seq_along(component)) {
        model.comp <- systemModels[[i + 1L]]
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
        systemModels[[i + 1L]] <- model.comp
    }
    combined@systemModels <- systemModels
    combined
}

updateObservation <- function(combined) {
    observationModels <- combined@observationModels
    account <- combined@account
    population <- account@population
    components <- account@components
    series.indices <- combined@seriesIndices
    transforms <- combined@transformsDatasets
    for (i in seq_along(observationModels)) {
        model <- observationModels[[i]]
        dataset <- datasets[[i]]
        transform <- transforms[[i]]
        series.index <- series.indices[i]
        if (series.index == 0L)
            series <- population
        else
            series <- components[[series.index]]
        series.collapsed <- collapse(series, transform = transform)
        if (is(model, "Poisson"))
            series.collapsed <- toDouble(series.collapsed)
        model <- updateModelUseExp(model, y = dataset, exposure = series.collapsed)
        observationModels[[i]] <- model
    }
    combined@observationModels <- observationModels
    combined
}

## HELPER FUNCTIONS ################################################







estimateAccount <- function(y, systemModels, observationModels, datasets, filename = NULL,
                            nBurnin = 1000, nSim = 1000, nChain = 4, nThin = 1,
                            parallel = TRUE, nUpdateMax = 200,
                            verbose = FALSE, useC = TRUE) {
    call <- match.call()
    methods::validObject(y)
    checkSystem(systemModels)
    systemModels <- alignSystemToAccount(systemModels = systemModels,
                                   account = account)
    checkObservation(observationModels, needsNonDefaultSeriesArg = TRUE)
    checkNamesDatasets(datasets)
    datasets <- alignDatasetsToObservation(datasets = datasets,
                                           observationModels = observationModels)
    ## check datasets after aligning to avoid checking datasets that are not needed
    datasets <- checkAndTidyDatasets(datasets)
    transforms <- makeTransformsYToDatasets(y = y, nameY = "y", datasets = datasets)
    namesDatasets <- names(datasets)
    names(datasets) <- NULL
    mcmc.args <- makeMCMCArgs(nBurnin = nBurnin,
                              nSim = nSim,
                              nChain = nChain,
                              nThin = nThin)
    if (is.null(filename))
        filename <- tempfile()
    else
        checkFilename(filename)
    control.args <- makeControlArgs(call = call,
                                    parallel = parallel)
    combineds <- replicate(n = mcmc.args$nChain,
                           initialCombinedAccount(account = account,
                                                  systemModels = systemModels,
                                                  observationModels = observationModels,
                                                  datasets = datasets,
                                                  namesDatasets = namesDatasets,
                                                  transforms = transforms))
    parallel <- control.args$parallel
    tempfile <- paste(filename, seq_len(mcmc.args$nChain), sep = "_")
    MoreArgs <- c(list(seed = NULL),
                  mcmc.args,
                  control.args,
                  list(continuing = FALSE))
    if (parallel) {
        cl <- parallel::makeCluster(getOption("cl.cores", default = mcmc.args$nChain))
        parallel::clusterSetRNGStream(cl)
        final.combineds <- parallel::clusterMap(cl = cl,
                                      fun = estimateOneChain,
                                      tempfile = tempfiles,
                                      combined = combineds,
                                      MoreArgs = MoreArgs,
                                      SIMPLIFY = FALSE,
                                      USE.NAMES = FALSE)
        seed <- parallel::clusterCall(cl, function() .Random.seed)
        parallel::stopCluster(cl)
    }
    else {
        final.combineds <- mapply(estimateOneChain,
                                  tempfile = tempfiles,
                                  combined = combineds,
                                  MoreArgs = MoreArgs,
                                  SIMPLIFY = FALSE,
                                  USE.NAMES = FALSE)
        seed <- list(.Random.seed)
    }
    control.args$lengthIter <- length(extractValues(final.combineds[[1L]]))
    results <- makeResultsAccount(finalCombineds = final.combineds,
                                  mcmcArgs = mcmc.args,
                                  controlArgs = control.args,
                                  seed = seed)
    makeResultsFile(filename = control.args$filename,
                    results = results,
                    tempfiles = tempfiles)
    finalMessage(filename = filename, verbose = verbose)
}
