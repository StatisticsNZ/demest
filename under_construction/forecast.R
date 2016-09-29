## NOT DO YET ######################################################################

## DON'T NEED TO TRANSLATE

setmethod("initialCombinedModelPredict",
          signature(combined = "CombinedModelNormal"),
          function(combined, along, covariates, exposure, weights) {
              model <- combined@model
              metadata <- model@metadataTheta
              if (!is.null(exposure))
                  stop(gettextf("Normal model, but '%s' is not %s",
                                "model", "exposure", "NULL"))
              weights <- checkAndTidyWeights(weights = weights, y = y)
              model <- initialPredictModel(model = model,
                                           along = along,
                                           labels = labels,
                                           covariates = covariates,
                                           weights = weights,
                                           benchmarks = benchmarks)
              .Data <- array(0.0, dim = dim(metadata), dimnames = dimnames(metadata))
              class.y <- class(combined@y)
              y <- new(class.y, .Data = .Data, metadata = metadataNew)
              new("CombinedModelNormal",
                  model = model,
                  y = y,
                  yIsIncomplete = TRUE,
                  slotsToExtract = c("model", "y"))
          })


## NEED TO BREAK INTO SMALLER FUNCTIONS, PUT MOST INTO DEMOGRAPHIC, AND USE
## FOR EXTRAPOLATE AND GROWTH
## NO_TESTS
## assume 'along' has been checked and tidied, and is valid integer
makeMetadataPredict <- function(metadataOld, along, n, labels) {
    name <- names(metadataOld)[along]
    dimtype <- dimtypes(metadataOld)[[along]]
    dimscale <- dimscales(metadataOld)[[along]]
    DimScale <- DimScales(metadataOld)[[along]]
    if (is.null(n)) {
        if (is.null(labels)) {
            stop(gettextf("'%s' and '%s' both %s",
                          "n", "labels", "NULL"))
        }
        else {
            labels <- as.character(labels)
            DimScale.predict <- inferDimScale(dimtype = dimtype,
                                              dimscale = dimscale,
                                              labels = labels,
                                              name = name)
        }
    }
    else {
        if (is.null(labels)) {
            if (!identical(length(n), 1L))
                stop(gettextf("'%s' does not have length %d",
                              "n", 1L))
            if (!is.numeric(n))
                stop(gettextf("'%s' is not a number",
                              "n"))
            if (as.integer(n) != n)
                stop(gettextf("'%s' is not an integer",
                              "n"))
            n <- as.integer(n)
            if (!(is(DimScale, "Points") || is(DimScale, "Intervals")))
                stop(gettextf("cannot use '%s' argument when '%s' dimension [\"%s\"] has dimscale \"%s\"",
                              "n", "along", name, dimscale))
            dimvalues <- DimScale@dimvalues
            n.dimvalues <- length(dimvalues)
            if (n > 0L) {
                step <- dimvalues[n.dimvalues] - dimvalues[n.dimvalues - 1L]
                dimvalues.extra <- seq(from = dimvalues[n.dimvalues],
                                       by = step,
                                       length = n)
                dimvalues.new <- c(dimvalues, dimvalues.extra)
            }
            else if (n == 0L)
                stop(gettextf("'%s' is %d",
                              "n", 0L))
            else {
                step <- dimvalues[2L] - dimvalues[1L]
                dimvalues.extra <- seq(from = dimvalues[1L],
                                       by = -step,
                                       length = n)
                dimvalues.new <- c(dimvalues.extra, dimvalues)
            }
            ## create new object
            DimScale.predict <- new(class(DimScale), dimvalues = dimvalues.new)
        }
        else {
            stop(gettextf("'%s' and '%s' are both non-%s",
                          "n", "labels", "NULL"))
        }
    }
    DimScales[[along]] <- DimScale.predict
    new("MetaData",
        nms = names,
        dimtypes = dimtypes,
        DimScales = DimScales)
}


## NEED TO TRANSLATE

setMethod("predictCombined",
          signature(combined = "CombinedModelHasExp"),
          function(combined, offsets, filename, lengthIter, iteration, nUpdate) {
              model <- combined@model
              model <- transferParamModel(model = model,
                                         offsets = offsets,
                                         filename = filename,
                                         lengthIter = lengthIter,
                                         iteration = iteration)
              model <- predictModelInternal(model = model, nUpdate = nUpdate)
              y <- predictYUseExp(y = y, model = model, exposure = exposure)
              combined@model <- model
              combined@y <- y
              combined
          })


setMethod("predictModelInternal",
          signature(object = "PoissonVaryingNotUseExpPredict"),
          function(object, nUpdate) {
              predictModelInternal_PoissonVarying(object)
          })


setMethod("transferParamModel",
          signature(object = "PoissonBinomialMixture")
          function(object, priorsOld, firsts, lasts, filename,
                   lengthIter, iteration) {
              object
          })

predictModelBench <- function(object, nUpdate) {
    object <- predictPriorsBetas(object)
    object <- predictBetas(object)
    object <- predictTheta(object)
    for (i in seq_len(nUpdate)) {
        object <- updatePredictedPriorsBetas(object)
        object <- updatePredictedBetas(object)
        object <- predictTheta(object)
    }
    object
}

setMethod("predictModelInternal",
          signature(object = "PoissonVaryingUseExpBenchUncertainPredict"),
          function(object, nUpdate) {
              predictModelBench(object = object, nUpdate= nUpdate)
          })



