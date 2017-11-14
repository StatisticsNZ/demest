

setMethod("makeTransformExposure",
          signature(series = "Births"),
          function(series, exposure) {
              



##################################################################################
## Top Level #####################################################################
##################################################################################

## updateCombined
##   updateAccount
##   updateSystemModels
##   updateObservationAccount

##################################################################################
## Updating Account ##############################################################
##################################################################################



##################################################################################
## Updating SystemModels ##############################################################
##################################################################################


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


updateAccount <- function(object) {
    stopifnot(methods::is(object, "CombinedAccount"))
    stopifnot(methods::validObject(object))
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

              
              updateSystemModels <- function(combined, useC = FALSE) {
                  stopifnot(methods::is(combined, "CombinedAccount"))
                  stopifnot(methods::validObject(combined))
                  if (useC) {
                      .Call(updateSystemModels_R, combined)
                  }
                  else {
                      system.models <- combined@systemModels
                      population <- combined@account@population
                      components <- combined@account@components
                      has.age <- combined@hasAge
                      model.uses.exposure <- combined@modelUsesExposure
                      transforms.exp.to.comp <- combined@transformsExpToComp
                      transform.exp.to.births <- combined@transformExpToBirths
                      i.births <- combined@iBirths
                      model <- system.models[[1L]]
                      model <- updateModelNotUseExp(model,
                                                    y = population)
                      system.models[[1L]] <- model.popn
                      for (i in seq_along(components)) {
                          model <- system.models[[i + 1L]]
                          component <- components[[i]]
                          uses.exposure <- model.uses.exposure[i + 1L]
                          if (uses.exposure) {
                              exposure <- combined@exposure@.Data
                              is.births <- i == i.births
                              if (is.births)
                                  exposure <- collapse(exposure,
                                                       transform = transform.exp.to.births)
                              transform <- transforms.exp.to.comp[[i]]
                              if (!is.null(transform))
                                  exposure <- extend(exposure,
                                                     transform = transforms.exp.to.comp[i])
                              model <- updateModelUseExp(model = model.comp,
                                                         y = component,
                                                         exposure = exposure)
                          }
                          else
                              model <- updateModelNotUseExp(model = model,
                                                            y = component)
                          system.models[[i + 1L]] <- model
                      }
                      combined@systemModels <- system.models
                      combined
                  }
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










### Updating Account ####################################################################


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
                                              mapping = mapping.to.exp)
        i.exp.first.2 <- getIExpFirstFromComp(i = i.cell.2,
                                              mapping = mapping.to.exp)
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

              
setMethod("diffLogLikAccount",
          signature(object = "CombinedAccountMovements"),
          function(object, useC = FALSE, useSpecific = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(diffLogLikAccount_CombineAccountMovements_R, object)
                  else
                      .Call(diffLogLikAccount_R, object)
              }
              else {
                  i.comp <- object@iComp
                  i.orig.dest <- object@iOrigDest
                  i.pool <- object@iPool
                  i.int.net <- object@iIntNet
                  if (i.comp == 0L)
                      diffLogLikAccountMovePopn(object)
                  else if (i.comp == i.orig.dest)
                      diffLogLikAccountMoveOrigDest(object)
                  else if (i.comp == i.pool) 
                      diffLogLikAccountMovePool(object)
                  else if (i.comp == i.int.net) 
                      diffLogLikAccountMoveNet(object)
                  else
                      diffLogLikAccountMoveComp(object)
              }
          })


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
                  uses.exposure <- combined@usesExposure
                  ans <- diffLogDensPopn(combined)
                  if (i.comp == i.orig.dest) {
                      if (uses.exposure[i.comp])
                          ans <- ans + diffLogDensJumpOrigDest(combined)
                      ans <- ans + diffLogDensExpOrigDestPoolNet(combined)
                  }
                  else if (i.comp == i.pool) {
                      if (uses.exposure[i.comp])
                          ans <- ans + diffLogDensJumpPoolWithExpose(combined)
                      else
                          ans <- ans + diffLogDensJumpPoolNoExpose(combined)
                      ans <- ans + diffLogDensExpOrigDestPoolNet(combined)
                  }
                  else if (i.comp == i.int.net) {
                      ans <- ans + diffLogDensJumpNet(combined)
                      ans <- ans + diffLogDensExpOrigDestPoolNet(combined)
                  }
                  else {
                      if (uses.exposure[i.comp])
                          ans <- ans + diffLogDensJumpComp(combined)
                      ans <- ans + diffLogDensExpComp(combined)
                  }
                  ans
              }
          })





          
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




    

## UPDATE VALUES ################################################################


setMethod("updateValuesAccount",
          signature(combined = "CombinedAccountsMovements"),
          function(combined, useC = FALSE, useSpecific = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(updateValuesAccount_CombinedAccountMovements_R, combined)
                  else
                      .Call(updateValuesAccount_R, combined)
              }
              else {
                  combined <- updateCellMove(combined)
                  combined <- updateSubsequentPopnMove(combined)
                  combined <- updateExposure(combined)
                  combined <- updateSubsequentAccession(combined)
                  combined
              }
          })




updateCellMove <- function(combined) {
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



## updateExposure <- function(combined) {
##     exposure <- combined@exposure
##     diff <- combined@diffProp
##     i.comp <- combined@iComp
##     i.orig.dest <- combined@iOrigDest
##     i.pool <- combined@iPool
##     i.int.net <- combined@iIntNet
##     i.cell <- combined@iCell
##     i.cell.other <- combined@iCellOther
##     i.exp.first <- combined@iExpFirst
##     i.exp.first.other <- combined@iExpFirstOther
##     is.popn <- i.comp == 0L
##     is.orig.dest <- i.comp == i.orig.dest
##     is.pool <- i.comp == i.pool
##     is.int.net <- i.comp == i.int.net
##     if (i.exp.first == 0L)
##         return(combined)
##     if (is.popn) {
        

##     }
    
##     update.two.cohorts <- (is.orig.dest || is.pool || is.int.net)
    
##     is.popn <- i.comp == 0L
##     if (is.popn) {
        
        
            
    
##     if (is
##     i.exposure <- getIExposure(i = i.cell, mapping = mapping[[i.comp]])
    
    
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


updateExpectedExposure <- function(combined) {
    NULL
}



## HELPER FUNCTIONS ################################################










estimateAccount <- function(y, systemModels, observationModels, datasets,
                            dominant = c("Female", "Male"),
                            filename = NULL,
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
    dominant <- match.arg(dominant)
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
                                                  systemWeights = systemWeights,
                                                  observationModels = observationModels,
                                                  seriesIndices = seriesIndices,
                                                  datasets = datasets,
                                                  namesDatasets = namesDatasets,
                                                  transforms = transforms,
                                                  dominant = dominant))
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


