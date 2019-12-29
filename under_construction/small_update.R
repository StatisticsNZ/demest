

chooseICellCompUpperTri <- function(description) {

    
}

chooseICellBirthsUpperTri <- function(description) {

    
}



getICellLowerTriFromComp <- function(iCellUp, description) {

}

getICellLowerTriFromBirths <- function(iCellUp, description) {

}

getICellLowerTriFromOrigDest <- function(iCellUp, description) {

}


## getICompNextFromComp <- function(i, description) {
    
## }


rbinomTrunc1 <- function(size, prob, lower, upper) {

}

setMethod("updateProposalAccount",
          signature(object = "CombinedAccountMovements"),
          function(object, useC = FALSE, useSpecific = FALSE) {
              stopifnot(methods::validObject(object))
              if (useC) {
                  if (useSpecific)
                      .Call(updateProposalAccount_CombinedAccountMovements_R, object)
                  else
                      .Call(updateProposalAccount_R, object)
              }
              else {
                  account <- object@account
                  prob.popn <- object@probPopn
                  prob.small.update <- object@probSmallUpdate@.Data ## NEW
                  update.popn <- stats::runif(n = 1L) < prob.popn
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
                      is.net.vec <- object@isNet ## NEW
                      prob.small.update <- object@probSmallUpdate ## NEW
                      i.comp <- rcateg1(cum.prob)
                      object@iComp <- i.comp
                      if (i.comp == i.births) {
                          is.small.update <- stats::runif(n = 1L) < prob.small.update ## NEW
                          if (is.small.update) ## NEW
                              updateProposalAccountMoveBirthsSmall(object) ## NEW
                          else ## NEW
                              updateProposalAccountMoveBirths(object)
                      } ## NEW
                      else if (i.comp == i.orig.dest) {
                          is.small.update <- stats::runif(n = 1L) < prob.small.update ## NEW
                          if (is.small.update) ## NEW
                              updateProposalAccountMoveOrigDestSmall(object) ## NEW
                          else ## NEW
                              updateProposalAccountMoveOrigDest(object) ## NEW
                      }
                      else if (i.comp == i.pool)
                          updateProposalAccountMovePool(object)
                      else if (i.comp == i.int.net)
                          updateProposalAccountMoveNet(object)
                      else { # comp
                          is.net <- is.net.vec[i.comp] ## NEW
                          is.small.update <- !is.net && (stats::runif(n = 1L) < prob.small.update) ## NEW
                          if (is.small.update) ## NEW
                              updateProposalAccountMoveCompSmall(object) ## NEW
                          else ## NEW
                              updateProposalAccountMoveComp(object)
                      }
                  }
              }
          })

setMethod("diffLogDensAccount",
          signature(combined = "CombinedAccountMovements"),
          function(combined, useC = FALSE, useSpecific = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(diffLogDensAccount_CombinedAccountMovements_R, combined)
                  else
                      .Call(diffLogDensAccount_R, combined)
              }
              else {
                  i.comp <- combined@iComp
                  i.orig.dest <- combined@iOrigDest
                  i.pool <- combined@iPool
                  i.int.net <- combined@iIntNet
                  is.small.update <- combined@isSmallUpdate ## NEW
                  model.uses.exposure <- combined@modelUsesExposure
                  is.popn <- i.comp == 0L
                  is.orig.dest <- i.comp == i.orig.dest
                  is.pool <- i.comp == i.pool
                  is.int.net <- i.comp == i.int.net
                  use.prior.popn <- combined@usePriorPopn@.Data
                  ans <- 0
                  if (use.prior.popn)
                      ans <- ans + diffLogDensPopn(combined)
                  if (is.popn)
                      ans <- ans + diffLogDensExpPopn(combined)
                  else if (is.orig.dest) {
                      if (is.small.update) ## NEW
                          ans <- ans + diffLogDensCompSmall(combined) ## NEW
                      else { ## NEW
                          if (model.uses.exposure[i.comp])
                              ans <- ans + diffLogDensJumpOrigDest(combined)
                          ans <- ans + diffLogDensExpOrigDestPoolNet(combined)
                      }
                  }
                  else if (is.pool) {
                      if (model.uses.exposure[i.comp])
                          ans <- ans + diffLogDensJumpPoolWithExpose(combined)
                      else
                          ans <- ans + diffLogDensJumpPoolNoExpose(combined)
                      ans <- ans + diffLogDensExpOrigDestPoolNet(combined)
                  }
                  else if (is.int.net) {
                      ans <- ans + diffLogDensJumpNet(combined)
                      ans <- ans + diffLogDensExpOrigDestPoolNet(combined)
                  }
                  else {
                      if (is.small.update) ## NEW
                          ans <- ans + diffLogDensCompSmall(combined) ## NEW
                      else { ## NEW
                          if (model.uses.exposure[i.comp])
                              ans <- ans + diffLogDensJumpComp(combined)
                          ans <- ans + diffLogDensExpComp(combined)
                      } ## NEW
                  }
                  ans
              }
          })


setMethod("diffLogLikAccount",
          signature(object = "CombinedAccountMovements"),
          function(object, useC = FALSE, useSpecific = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(diffLogLikAccount_CombinedAccountMovements_R, object)
                  else
                      .Call(diffLogLikAccount_R, object)
              }
              else {
                  i.comp <- object@iComp
                  i.orig.dest <- object@iOrigDest
                  i.pool <- object@iPool
                  i.int.net <- object@iIntNet
                  is.small.update <- object@isSmallUpdate ## NEW
                  if (i.comp == 0L)
                      diffLogLikAccountMovePopn(object)
                  else if (i.comp == i.orig.dest) { ## NEW
                      if (is.small.update) ## NEW
                          diffLogLikAccountMoveCompSmall(object) ## NEW
                      else ## NEW
                          diffLogLikAccountMoveOrigDest(object)
                  } ## NEW
                  else if (i.comp == i.pool) 
                      diffLogLikAccountMovePool(object)
                  else if (i.comp == i.int.net) 
                      diffLogLikAccountMoveNet(object)
                  else { ## NEW
                      if (is.small.update) ## NEW
                          diffLogLikAccountMoveCompSmall(object) ## NEW
                      else ## NEW
                          diffLogLikAccountMoveComp(object)
                  } ## NEW
              }
          })



setMethod("updateValuesAccount",
          signature(combined = "CombinedAccountMovements"),
          function(combined, useC = FALSE, useSpecific = FALSE) {
              stopifnot(methods::validObject(combined))
              if (useC) {
                  if (useSpecific)
                      .Call(updateValuesAccount_CombinedAccountMovements_R, combined)
                  else
                      .Call(updateValuesAccount_R, combined)
              }
              else {
                  has.age <- combined@hasAge
                  is.small.update <- combined@isSmallUpdate ## NEW
                  combined <- updateCellMove(combined)
                  if (is.small.update) ## NEW
                      combined <- updateAccSmall(combined)
                  else { ## NEW
                      combined <- updateSubsequentPopnMove(combined)
                      combined <- updateSubsequentExpMove(combined)
                      if (has.age)
                          combined <- updateSubsequentAccMove(combined)
                  } ## NEW
                  combined
              }
          })




## Assume has age. Note that accession irrelevant,
## since accession applies to cohort being born,
## and total number of births does not change
updateProposalAccountMoveBirthsSmall <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(updateProposalAccountMoveBirthsSmall_R, combined)
    }
    else {
        account <- combined@account
        population <- account@population
        i.comp <- combined@iComp
        component <- account@components[[i.comp]]
        max.attempt <- combined@maxAttempt
        uses.exposure <- combined@modelUsesExposure[i.comp + 1L]
        mapping.to.exp <- combined@mappingsToExp[[i.comp]]
        description <- combined@descriptions[[i.comp + 1L]]
        sys.mod.comp <- combined@systemModels[[i.comp + 1L]]
        theta <- sys.mod.comp@theta
        struc.zero.array <- sys.mod.comp@strucZeroArray
        generated.new.proposal <- FALSE
        for (i in seq_len(max.attempt)) {
            i.cell <- chooseICellComp(description)
            is.struc.zero <- struc.zero.array[i.cell] == 0L
            if (!is.struc.zero) {
                generated.new.proposal <- TRUE
                break
            }
        }
        if (generated.new.proposal) {
            ## i.cell.low is lower Lexis triangle within same
            ## age-period square
            i.cell.low <- getICellLowerTriFromBirths(iCellUp = i.cell.up,
                                                     description = description)
            val.up.curr <- component[i.cell.up]
            val.low.curr <- component[i.cell.low]
            val.up.expected <- theta[i.cell.up]
            val.low.expected <- theta[i.cell.low]
            if (uses.exposure) {
                exposure <- combined@exposure
                i.expose.up <- getIExposureFromBirths(i = i.cell.up,
                                                      mapping = mapping.to.exp)
                i.expose.low <- getIExposureFromBirths(i = i.cell.up,
                                                       mapping = mapping.to.exp)
                expose.up <- exposure[i.expose.up]
                expose.low <- exposure[i.expose.low]
                val.up.expected <- val.up.expected * expose.up
                val.low.expected <- val.low.expected * expose.low
            }
            prob <- val.up.expected / (val.up.expected + val.low.expected)
            size <- val.up.curr + val.low.curr
            val.up.prop <- rbinom(n = 1L,
                                  size = size,
                                  prob = prob)
            val.low.prop <- size - val.up.prop
            diff.prop <- unname(val.up.prop - val.up.curr)
            generated.new.proposal  <- diff.prop != 0L
        }
        if (generated.new.proposal) {
            combined@generatedNewProposal@.Data <- TRUE
            combined@isSmallUpdate <- TRUE
            combined@iCell <- i.cell.up
            combined@iCellOther <- i.cell.low
            combined@iPopnNext <- NA_integer_
            combined@iPopnNextOther <- NA_integer_
            combined@iAccNext <- 0L
            combined@iAccNextOther <- NA_integer_
            combined@isLowerTriangle@.Data <- FALSE
            if (uses.exposure) {
                combined@iExposure <- i.expose.up
                combined@iExposureOther <- i.expose.low
            }
            else {
                combined@iExposure <- 0L
                combined@iExposureOther <- NA_integer_
            }
            combined@iExpFirst <- NA_integer_
            combined@iExpFirstOther <- NA_integer_
            combined@diffProp <- diff.prop
        }
        else {
            combined@generatedNewProposal@.Data <- FALSE
            combined@isSmallUpdate <- TRUE
-=            combined@iCell <- NA_integer_
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
        }
        combined
    }
}


## assume has age
updateProposalAccountMoveOrigDestSmall <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    stopifnot(combined@hasAge@.Data)
    if (useC) {
        .Call(updateProposalAccountMoveOrigDestSmall_R, combined)
    }
    else {
        account <- combined@account
        population <- account@population
        i.comp <- combined@iComp
        component <- account@components[[i.comp]]
        max.attempt <- combined@maxAttempt
        accession <- combined@accession
        mapping.to.acc <- combined@mappingsToAcc[[i.comp]]
        uses.exposure <- combined@modelUsesExposure[i.comp + 1L]
        mapping.to.exp <- combined@mappingsToExp[[i.comp]]
        description <- combined@descriptions[[i.comp + 1L]]
        sys.mod.comp <- combined@systemModels[[i.comp + 1L]]
        theta <- sys.mod.comp@theta
        struc.zero.array <- sys.mod.comp@strucZeroArray
        for (i in seq_len(max.attempt)) {
            i.cell.up <- chooseICellCompUpperTri(description)
            is.struc.zero <- struc.zero.array[i.cell.up] == 0L
            if (!is.struc.zero) {
                generated.new.proposal <- TRUE
                break
            }
        }
        if (generated.new.proposal) {
            ## i.cell.low is lower Lexis triangle within same
            ## period but next age group - except when i.cell.up
            ## is for oldest age group, in which case i.cell.low
            ## is from same age-period square
            i.cell.low <- getICellLowerTriFromOrigDest(iCellUp = i.cell.up,
                                                       description = description)
            pair.acc <- getIAccNextFromOrigDest(i = i.cell.up,
                                                mapping = mapping.to.acc)
            i.acc.orig <- pair.acc[1L]
            i.acc.dest <- pair.acc[2L]
            is.final.age.group <- i.acc == 0L
            ## Our existing accounting system ignores possible accession
            ## for cohort aged A+ at time t. Future version should
            ## include this.
            if (!is.final.age.group) {
                val.acc.orig <- accession[i.acc.orig]
                val.acc.dest <- accession[i.acc.dest]
            }
            val.up.curr <- component[i.cell.up]
            val.low.curr <- component[i.cell.low]
            val.up.expected <- theta[i.cell.up]
            val.low.expected <- theta[i.cell.low]
            if (uses.exposure) {
                exposure <- combined@exposure
                i.expose.up <- getIExposureFromOrigDest(i = i.cell.up,
                                                       mapping = mapping.to.exp)
                i.expose.low <- getIExposureFromOrigDest(i = i.cell.low,
                                                       mapping = mapping.to.exp)
                expose.up <- exposure[i.expose.up]
                expose.low <- exposure[i.expose.low]
                val.up.expected <- val.up.expected * expose.up
                val.low.expected <- val.low.expected * expose.low
            }
            prob <- val.up.expected / (val.up.expected + val.low.expected)
            size <- val.up.curr + val.low.curr
            if (is.final.age.group) { ## no accession constraint
                val.up.prop <- rbinom(n = 1L,
                                      size = size,
                                      prob = prob)
            }
            else {
                lower <- val.up.curr - val.acc
                upper <- val.up.curr + val.acc
                val.up.prop <- rbinomTrunc1(size = size,
                                            prob = prob,
                                            lower = lower,
                                            upper = upper)
            }
            found.value <- !is.na(val.prop)
            if (found.value) {
                val.low.prop <- size - val.up.prop
                diff.prop <- unname(val.up.prop - val.up.curr)
                generated.new.proposal  <- diff.prop != 0L
            }
            else
                generated.new.proposal  <- FALSE
        }
    }
    if (generated.new.proposal) {
        combined@generatedNewProposal@.Data <- TRUE
        combined@isSmallUpdate@.Data <- TRUE
        combined@iCell <- i.cell.up
        combined@iCellOther <- i.cell.low
        combined@iPopnNext <- NA_integer_
        combined@iPopnNextOther <- NA_integer_
        combined@iAccNext <- i.acc.orig
        combined@iAccNextOther <- i.acc.dest
        combined@isLowerTriangle@.Data <- FALSE
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
    }
    combined
}



## assume has age, and is not net
updateProposalAccountMoveCompSmall <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    stopifnot(combined@hasAge@.Data)
    stopifnot(!combined@isNet[combined@i.comp])
    if (useC) {
        .Call(updateProposalAccountMoveCompSmall_R, combined)
    }
    else {
        account <- combined@account
        population <- account@population
        i.comp <- combined@iComp
        component <- account@components[[i.comp]]
        is.increment <- combined@isIncrement[[i.comp]]
        max.attempt <- combined@maxAttempt
        accession <- combined@accession
        mapping.to.acc <- combined@mappingsToAcc[[i.comp]]
        uses.exposure <- combined@modelUsesExposure[i.comp + 1L]
        mapping.to.exp <- combined@mappingsToExp[[i.comp]]
        description <- combined@descriptions[[i.comp + 1L]]
        sys.mod.comp <- combined@systemModels[[i.comp + 1L]]
        theta <- sys.mod.comp@theta
        struc.zero.array <- sys.mod.comp@strucZeroArray
        for (i in seq_len(max.attempt)) {
            i.cell.up <- chooseICellCompUpperTri(description)
            is.struc.zero <- struc.zero.array[i.cell.up] == 0L
            if (!is.struc.zero) {
                generated.new.proposal <- TRUE
                break
            }
        }
        if (generated.new.proposal) {
            ## i.cell.low is lower Lexis triangle within same
            ## period but next age group - except when i.cell.up
            ## is for oldest age group, in which case i.cell.low
            ## is from same age-period square
            i.cell.low <- getICellLowerTriFromComp(iCellUp = i.cell.up,
                                                   description = description)
            i.acc <- getIAccNextFromComp(i = i.cell.up,
                                         mapping = mapping.to.acc)
            is.final.age.group <- i.acc == 0L
            ## Our existing accounting system ignores possible accession
            ## for cohort aged A+ at time t. Future version should
            ## include this.
            if (!is.final.age.group)
                val.acc <- accession[i.acc]
            val.up.curr <- component[i.cell.up]
            val.low.curr <- component[i.cell.low]
            val.up.expected <- theta[i.cell.up]
            val.low.expected <- theta[i.cell.low]
            if (uses.exposure) {
                exposure <- combined@exposure
                i.expose.up <- getIExposureFromComp(i = i.cell.up,
                                                    mapping = mapping.to.exp)
                i.expose.low <- getIExposureFromComp(i = i.cell.up,
                                                     mapping = mapping.to.exp)
                expose.up <- exposure[i.expose.up]
                expose.low <- exposure[i.expose.low]
                val.up.expected <- val.up.expected * expose.up
                val.low.expected <- val.low.expected * expose.low
            }
            prob <- val.up.expected / (val.up.expected + val.low.expected)
            size <- val.up.curr + val.low.curr
            if (is.final.age.group) { ## no accession constraint
                val.up.prop <- rbinom(n = 1L,
                                      size = size,
                                      prob = prob)
            }
            else {
                if (is.increment) {
                    lower <- val.up.curr - val.acc
                    upper <- NA_integer_
                }
                else {
                    lower <- NA_integer_
                    upper <- val.up.curr + val.acc
                }
                val.up.prop <- rbinomTrunc1(size = size,
                                            prob = prob,
                                            lower = lower,
                                            upper = upper)
            }
            found.value <- !is.na(val.prop)
            if (found.value) {
                val.low.prop <- size - val.up.prop
                diff.prop <- unname(val.up.prop - val.up.curr)
                generated.new.proposal  <- diff.prop != 0L
            }
            else
                generated.new.proposal  <- FALSE
        }
    }
    if (generated.new.proposal) {
        combined@generatedNewProposal@.Data <- TRUE
        combined@isSmallUpdate@.Data <- TRUE
        combined@iCell <- i.cell.up
        combined@iCellOther <- i.cell.low
        combined@iPopnNext <- NA_integer_
        combined@iPopnNextOther <- NA_integer_
        combined@iAccNext <- i.acc
        combined@iAccNextOther <- NA_integer_
        combined@isLowerTriangle@.Data <- FALSE
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
    }
    combined
}


diffLogLikAccountMoveCompSmall <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    stopifnot(combined@@.Data)
    if (useC) {
        .Call(diffLogLikAccountMoveCompSmall_R, combined)
    }
    else {
        account <- combined@account
        i.comp <- combined@iComp
        component <- combined@account@components[[i.comp]]
        data.models <- combined@dataModels
        datasets <- combined@datasets
        series.indices <- combined@seriesIndices
        transforms <- combined@transforms
        i.cell.up <- combined@iCell
        i.cell.low <- combined@iCellOther
        diff <- combined@diffProp
        is.increment <- combined@isIncrement[i.comp]
        diff.log.lik.up <- diffLogLikCellComp(diff = diff,
                                              iComp = i.comp,
                                              iCell = i.cell.up,
                                              component = component,
                                              dataModels = data.models,
                                              datasets = datasets,
                                              seriesIndices = series.indices,
                                              transforms = transforms)
        if (is.infinite(diff.log.lik.up))
            return(diff.log.lik.up)
        diff.log.lik.low <- diffLogLikCellComp(diff = -diff,
                                               iComp = i.comp,
                                               iCell = i.cell.low,
                                               component = component,
                                               dataModels = data.models,
                                               datasets = datasets,
                                               seriesIndices = series.indices,
                                               transforms = transforms)
        if (is.infinite(diff.log.lik.low))
            return(diff.log.lik.low)
        diff.log.lik.up + diff.log.lik.low
    }
}

diffLogDensCompSmall <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogDensCompSmall_R, combined)
    }
    else {
        i.comp <- combined@iComp
        component <- combined@account@components[[i.comp]]
        theta <- combined@systemModels[[i.comp + 1L]]@theta
        uses.exposure <- combined@modelUsesExposure[i.comp + 1L]
        i.cell.up <- combined@iCell
        i.cell.low <- combined@iCellOther
        uses.exposure <- combined@modelUsesExposure[i.comp + 1L]
        diff <- combined@diffProp
        val.up.curr <- component[i.cell.up]
        val.low.curr <- component[i.cell.low]
        val.up.prop <- val.up.curr + diff
        val.low.prop <- val.low.cur - diff
        val.up.expected <- theta[i.cell.up]
        val.low.expected <- theta[i.cell.low]
        if (uses.exposure) {
            exposure <- combined@exposure
            i.expose.up <- combined@iExposure
            i.expose.low <- combined@iExposureOther
            expose.up <- exposure[i.expose.up]
            expose.low <- exposure[i.expose.low]
            val.up.expected <- val.up.expected * expose.up
            val.low.expected <- val.low.expected * expose.low
        }
        ans <- (stats::dpois(x = val.up.prop, lambda = val.up.expected, log = TRUE) +
                stats::dpois(x = val.low.prop, lambda = val.low.expected, log = TRUE) -
                stats::dpois(x = val.up.curr, lambda = val.up.expected, log = TRUE) -
                stats::dpois(x = val.low.curr, lambda = val.low.expected, log = TRUE))
        ans
    }
}


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
        is.small.update <- combined@isSmallUpdate ## NEW
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
        else if (is.int.net || is.small.update) { ## NEW
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



updateAccSmall <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovementsHasAge"))
    if (useC) {
        .Call(updateAccSmall_R, combined)
    }
    else {
        i.comp <- combined@iComp
        diff <- combined@diffProp
        is.increment <- combined@isIncrement
        i.acc <- combined@iAccNext
        has.accession <- i.acc > 0L
        if (has.accession) {
            if (is.increment)
                combined@accession[i.acc] <- combined@accession[i.acc] + diff
            else
                combined@accession[i.acc] <- combined@accession[i.acc] - diff
        }
    }
    combined
}
