

## updating proposals #############################################################

## HAS_TESTS
updateProposalAccountMovePopn <- function(combined, useC = FALSE) {
    stopifnot(is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(updateProposalAccountMovePopn_R, combined)
    }
    else {
        account <- combined@account
        population <- account@population
        has.age <- combined@hasAge@.Data
        if (has.age) {
            accession <- combined@accession
            iterator.acc <- combined@iteratorAcc
        }
        description <- combined@descriptions[[1L]]
        iterator.popn <- combined@iteratorPopn
        theta <- combined@systemModels[[1L]]@theta
        max.attempt <- combined@maxAttempt
        i.cell <- chooseICellPopn(description)
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
        if (generated.new.proposal) {
            combined@iCell <- i.cell
            combined@iCellOther <- NA_integer_
            combined@iPopnNext <- i.popn.next
            combined@iPopnNextOther <- NA_integer_
            if (has.age) {
                combined@iAccNext <- i.acc.next
                combined@iAccNextOther <- NA_integer_
                combined@isLowerTriangle <- NA
            }
            combined@iExposure <- NA_integer_
            combined@iExposureOther <- NA_integer_
            combined@iExpFirst <- i.exp.first
            combined@iExpFirstOther <- NA_integer_
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


## Calculating log-likelihood #################################################

## HAS_TESTS
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

## HAS_TESTS
diffLogLikPopn <- function(diff, iFirst, iterator, population,
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
    stopifnot(is(iterator, "CohortIteratorPopulation"))
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
        .Call(diffLogLikPopn_R,
              diff, iFirst, iterator, population, observationModels,
              datasets, seriesIndices)
    }
    else {        
        ans <- 0
        for (i.dataset in seq_along(datasets)) {
            assoc.with.popn <- seriesIndices[i.dataset] == 0L
            if (assoc.with.popn) {
                model <- observationModels[[i.dataset]]
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

## HAS_TESTS
diffLogLikAccountMovePopn <- function(combined, useC = FALSE) {
    stopifnot(is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogLikAccountMovePopn_R, combined)
    }
    else {
        account <- combined@account
        population <- account@population
        iterator <- combined@iteratorPopn
        observationModels <- combined@observationModels
        datasets <- combined@datasets
        seriesIndices <- combined@seriesIndices
        transforms <- combined@transforms
        iCell <- combined@iCell
        diff <- combined@diffProp
        diffLogLikPopn(diff = diff,
                       iFirst = iCell,
                       iterator = iterator,
                       population = population,
                       observationModels = observationModels,
                       datasets = datasets,
                       seriesIndices = seriesIndices,
                       transforms = transforms)
    }
}
