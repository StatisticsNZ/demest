

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
