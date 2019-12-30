

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
