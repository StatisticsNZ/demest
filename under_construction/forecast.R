

forecast <- function(initial, births = NULL, internal = NULL,
                     entries = list(), exits = list(), net = list(),
                     dominant = NULL, filename = NULL,
                     nBurnin = 1000, nChain = 2,
                     parallel = TRUE, nCore = 4) {
    account.blank <- accountBlank(initial = initial,   # Can be single account,
                                  births = births,     # or list of accounts,
                                  internal = internal, # depending on whether
                                  entries = entries,   # 'initial' has iterations.
                                  exits = exits,
                                  net = net) 
    metadata <- metadata(account.blank)
    dominant <- checkAndTidyDominant(dominant = dominant,
                                     metadata = metadata)
    system.rates.list <- makeSystemRates(births = births,
                                         internal = internal,
                                         entries = entries,
                                         exits = exits,
                                         net = net)
    if (nChain > 1L)
        system.models <- rep(systemModels,
                             times = nChain)
    cl <- parallel::makeCluster(getOption("cl.cores",
                                          default = nCore))
    accounts <- parallel::clusterMap(cl = cl,
                                     fun = forecastInner,
                                     account = account.blank,
                                     systemRates = system.rates.list,
                                     nBurnin = nBurnin)
    ans <- dbind(args = accounts)
    ans
}

populationForecast <- function(initial, births, internal, entries, exits , net) {
    if (!methods::is(initial, "Counts"))
        stop(gettextf("'%s' has class \"%s\"",
                      "initial", class(initial)))
    initial <- tryCatch(toInteger(initial),
                        error = function(e) e)
    if (methods::is(initial, "error"))
        stop(gettextf("unable to coerce '%s' to integer : %s",
                      initial$message))
    if (!is.null(births))
        DS.time.popn <- getDimScaleTimePopn(births,
                                            name = "births")
    else if (!is.null(internal))
        DS.time.popn <- getDimScaleTimePopn(internal,
                                            name = "internal")
    else if (length(entries) != 0L) {
        names.entries <- names(entries)
        if (is.null(names.entries))
            stop(gettextf("'%s' does not have names",
                          "entries"))
        DS.time.popn <- getDimScaleTimePopn(entries[[1L]],
                                            name = names.entries[1L])
    }
    else if (length(exits) != 0L) {
        names.exits <- names(exits)
        if (is.null(names.exits))
            stop(gettextf("'%s' does not have names",
                          "exits"))
        DS.time.popn <- getDimScaleTimePopn(exits[[1L]],
                                            name = names.exits[1L])
    }
    else if (length(net) != 0L) {
        names.net <- names(net)
        if (is.null(names.net))
            stop(gettextf("'%s' does not have names",
                          "net"))
        DS.time.popn <- getDimScaleTimePopn(net[[1L]],
                                            name = names.net[1L])
    }
    else
        stop(gettextf("no values supplied for '%s', '%s', '%s', '%s', or '%s'",
                      "births", "internal", "entries", "exits", "net"))
    dim.initial <- dim(initial)
    names.initial <- names(initial)
    dimtypes.initial <- dimtypes(initial,
                                 use.names = FALSE)
    DimScales.initial <- DimScales(initial,
                                   use.names = FALSE)
    i.time.initial <- match("time", dimtypes.initial, nomatch = 0L)
    has.time.initial <- i.time.initial > 0L
    if (has.time.initial) {
        DS.time.initial <- DimScales.initial[[i.time.initial]]
        if (!methods::is(DS.time.initial, "Points"))
            stop(gettextf("dimension of '%s' with %s \"%s\" has dimscale \"%s\"",
                          "initial", "dimtype", "time", class(DS.time.initial)))
        dv.time.initial <- dimvalues(DS.time.initial)
        n.dv.time.initial <- length(dv.time.initial)
        if (n.dv.time.initial == 0L)
            NULL # ignore
        else if (n.dv.time.initial == 1L) {
            dv.time.popn <- dimvalues(DS.time.popn)
            if (!isTRUE(all.equal(dv.time.initial, dv.time.popn[1L])))
                stop(gettextf("time dimension of '%s' not consistent with time dimension of components",
                              "initial"))
        }            
        else
            stop(gettextf("time dimension of '%s' has more than one point",
                          "initial"))
        DimScales.population <- replace(DimScales.initial,
                                        list = i.time.initial,
                                        values = list(DS.time.popn))
        metadata.population <- methods::new("MetaData",
                                            nms = names.initial,
                                            dimtypes = dimtypes.initial,
                                            DimScales = DimScales.population)
    }
    else {
        names.population <- make.unique(c(names.initial, "time"))
        dimtypes.population <- c(dimtypes.initial, "time")
        DimScales.population <- c(DimScales.initial, list(DS.time.popn))
        metadata.population <- new("MetaData",
                                   nms = names.population,
                                   dimtypes = dimtypes.population,
                                   DimScales = DimScales.population)
    }
    .Data.population <- array(0L,
                              dim = dim(metadata.population),
                              dimnames = dimnames(metadata.population))
    dim <- dim(metadata.population)
    dimtypes <- dimtypes(metadata.population)
    i.time <- match("time", dimtypes)
    i.age <- match("age", dimtypes, nomatch = 0L)
    has.age <- i.age > 0L
    n.time <- dim[i.time]
    popn <- initial@.Data
    if (has.age)
        n.age <- dim(popn)[i.age]
    n.popn <- length(popn)
    index.time <- slice.index(.Data.population,
                              margin = i.time)
    index.age <- slice.index(popn,
                             margin = i.age)
    s0 <- seq.int(from = 1L, to = n.age - 2L)
    s1 <- seq.int(from = 2L, to = n.age - 1L)
    for (i in seq_len(n.time)) {
        .Data.population[index.time == i] <- popn
        if (has.age) {
            popn[index.age == n.age] <- popn[index.age == n.age] + popn[index.age == n.age - 1L]
            popn[index.age %in% s1] <- popn[index.age %in% s0]
            popn[index.age == 1L] <- 0L
        }
    }
    new("Counts",
        .Data = .Data.population,
        metadata = metadata.population)
}


                                      
forecastInner <- function(account, systemRates, nBurnin) {
    account <- fillInBlankAccount(account = account,
                                  systemRates = systemRates)
    combined <- initialCombinedForecast(account = account,
                                        systemRates = systemRates)
    for (i in seq_len(nBurnin))
        }

    

forecastInnerOne <- function(account, systemModel, nBurnin) {
    account <- fillInBlankAccount(account = account,
                                  systemModel = systemModel)
    

    

setMethod("initialInternalOrigDest",
          signature(internal = "DemographicArray",
                    population = "Population"),
          function(internal, population) {
              names.int <- names(internal)
              names.popn <- names(population)
              dimtypes <- dimtypes(internal, use.names = FALSE)
              i.orig <- grep("origin", dimtypes)
              has.orig <- length(i.orig) > 0L
              if (!has.orig)
                  stop(gettextf("'%s' does not have dimension with %s \"%s\"",
                                "internal", "dimtype", "origin"))
              orig.names <- names.int[i.orig]
              base.names <- sub("_orig$", "", orig.names)
              dest.names <- paste(base.names, "dest", sep = "_")
              i.orig.popn <- match(base.names, names.popn)
              perm <- replace(names.popn,
                              list = i.orig.popn,
                              values = orig.names),
              perm <- c(perm, dest.names)
              internal <- aperm(internal,
                                perm = perm)
              s <- seq_len(length(population))
              if (
              sapply(s, function(i) rmultinom(n = 1L,
                                              size = population[[i]],
                                              prob = 

}


setMethod("initialCombinedAccount",
          signature(account = "Movements",
                    systemModels = "list",
                    systemWeights = "list",
                    dataModels = "list",
                    seriesIndices = "integer",
                    datasets = "list",
                    namesDatasets = "character",
                    transforms = "list"),
          function(account, systemModels, systemWeights,
                   dataModels, seriesIndices, 
                   datasets, namesDatasets, transforms,
                   dominant = c("Female", "Male")) {
              population <- account@population
              components <- account@components
              names.components <- account@namesComponents
              has.age <- "age" %in% dimtypes(population, use.names = FALSE)
              age.time.step <- dembase::ageTimeStep(population)
              n.popn <- length(population)
              n.components <- sapply(components, length)
              n.cell.account <- n.popn + sum(n.components)
              prob.popn <- n.popn / (n.popn + sum(n.components))
              cum.prob.comp <- cumsum(n.components) / sum(n.components)
              is.births <- sapply(components, methods::is, "Births")
              is.orig.dest <- sapply(components, methods::is, "HasOrigDest")
              is.par.ch <- sapply(components, methods::is, "HasParentChild")
              is.pool <- sapply(components, methods::is, "InternalMovementsPool")
              is.int.net <- sapply(components, methods::is, "InternalMovementsNet")
              is.net.move <- sapply(components, methods::is, "NetMovements")
              i.births <- if (any(is.births)) which(is.births) else -1L
              i.orig.dest <- if (any(is.orig.dest)) which(is.orig.dest) else -1L
              i.par.ch <- if (any(is.par.ch)) which(is.par.ch) else -1L
              i.pool <- if (any(is.pool)) which(is.pool) else -1L
              i.int.net <- if (any(is.int.net)) which(is.int.net) else -1L
              is.net <- is.int.net | is.net.move
              exposure <- dembase::exposure(population,
                                            triangles = has.age)
              exposure <- new("Exposure",
                              .Data = exposure@.Data,
                              metadata = exposure@metadata)
              population <- methods::new("Population",
                                         .Data = population@.Data,
                                         metadata = population@metadata)
              is.increment <- sapply(components, dembase::isPositiveIncrement)
              iterator.popn <- CohortIterator(population)
              iterator.exposure <- CohortIterator(exposure)
              iterators.comp <- lapply(components, CohortIterator)
              descriptions <- lapply(c(list(population), components), Description)
              mappings.from.exp <- lapply(components, function(x) Mapping(exposure, x))
              mappings.to.exp <- lapply(components, function(x) Mapping(x, exposure))
              mappings.to.popn <- lapply(components, function(x) Mapping(x, population))
              model.uses.exposure <- sapply(systemModels, function(x) x@useExpose@.Data)
              if ((i.births > 0L) && model.uses.exposure[i.births + 1L])
                  transform.exp.to.births <- makeTransformExpToBirths(exposure = exposure,
                                                                      births = components[[i.births]],
                                                                      dominant = dominant)
              else
                  transform.exp.to.births <- new("CollapseTransform")
              transforms.exp.to.comp <- vector(mode = "list", length = length(components))
              for (i in seq_along(transforms.exp.to.comp)) {
                  if (model.uses.exposure[i + 1L]) {
                      if (i == i.births) {
                          exposure.births <- collapse(exposure,
                                                      transform = transform.exp.to.births)
                          transform <- makeTransformExpToComp(exposure = exposure.births,
                                                              component = components[[i]],
                                                              nameComponent = names.components[i])
                      }
                      else
                          transform <- makeTransformExpToComp(exposure = exposure,
                                                              component = components[[i]],
                                                              nameComponent = names.components[i])
                  }
                  else
                      transform <- NULL
                  if (is.null(transform))
                      transforms.exp.to.comp[i] <- list(NULL)
                  else
                      transforms.exp.to.comp[[i]] <- transform
              }
              for (i in seq_along(systemModels)) {
                  series <- if (i == 1L) population else components[[i - 1L]]
                  spec <- systemModels[[i]]
                  if (model.uses.exposure[i]) {
                      if (i - 1L == i.births)
                          expose <- collapse(exposure,
                                             transform = transform.exp.to.births)
                      else
                          expose <- exposure
                      transform <- transforms.exp.to.comp[[i - 1L]]
                      if (!is.null(transform))
                          expose <- extend(expose,
                                           transform = transform)
                      systemModels[[i]] <- initialModel(spec,
                                                        y = series,
                                                        exposure = expose)
                  }
                  else {
                      weights <- systemWeights[[i]]
                      if (is.null(weights)) {
                          uses.weights <- modelUsesWeights(spec)
                          if (uses.weights) {
                              .Data <- array(1, dim = dim(series), dimnames = dimnames(series))
                              metadata <- series@metadata
                              weights <- methods::new("Counts", .Data = .Data, metadata = metadata)
                              systemModels[[i]] <- initialModel(spec,
                                                                y = series,
                                                                weights = weights)
                          }
                          else {
                              systemModels[[i]] <- initialModel(spec,
                                                                y = series,
                                                                exposure = NULL)
                          }
                      }
                      else {
                          systemModels[[i]] <- initialModel(spec,
                                                            y = series,
                                                            weights = weights)
                      }
                  }
              }
              struc.zero.array <- systemModels[[1L]]@strucZeroArray@.Data
              if (any(struc.zero.array == 0L)) {
                  population@.Data[struc.zero.array == 0L] <- 0L
                  exposure@.Data <- dembase::exposure(population, triangles = has.age)@.Data
                  account@population <- population
              }
              for (i in seq_along(components)) {
                  sys.mod <- systemModels[[i + 1L]]
                  has.struc.zeros <- methods::is(sys.mod, "StrucZeroArrayMixin")
                  if (has.struc.zeros) {
                      struc.zero.array <- sys.mod@strucZeroArray@.Data
                      if (any(struc.zero.array == 0L))
                          account@components[[i]]@.Data[struc.zero.array == 0L] <- 0L
                  }
              }
              .Data.theta.popn <- array(systemModels[[1L]]@theta,
                                        dim = dim(population),
                                        dimnames = dimnames(population))
              metadata.theta.popn <- population@metadata
              theta.popn <- new("Counts",
                                .Data = .Data.theta.popn,
                                metadata = metadata.theta.popn)
              expected.exposure <- dembase::exposure(theta.popn,
                                                     triangles = has.age)
              expected.exposure <- new("Exposure",
                                       .Data = expected.exposure@.Data,
                                       metadata = expected.exposure@metadata)
              for (i in seq_along(dataModels)) {
                  series.index <- seriesIndices[i]
                  series <- if (series.index == 0L) population else components[[series.index]]
                  series.collapsed <- dembase::collapse(series, transform = transforms[[i]])
                  model <- dataModels[[i]]
                  if (methods::is(model, "Poisson"))
                      series.collapsed <- dembase::toDouble(series.collapsed)
                  dataset <- datasets[[i]]
                  dataModels[[i]] <- initialModel(model,
                                                  y = dataset,
                                                  exposure = series.collapsed)
                  has.struc.zeros <- methods::is(model, "StrucZeroArrayMixin")
                  if (has.struc.zeros) {
                      struc.zero.array <- model@strucZeroArray@.Data
                      dataset@.Data[struc.zero.array == 0L] <- 0L
                      datasets[[i]] <- dataset
                  }
              }
              if (has.age) {
                  accession <- dembase::accession(account,
                                                  births = FALSE)
                  accession <- new("Accession",
                                   .Data = accession@.Data,
                                   metadata = accession@metadata)
                  iterator.acc <- CohortIterator(accession)
                  mappings.to.acc <- lapply(components, function(x) Mapping(x, accession))
                  methods::new("CombinedAccountMovementsHasAge",
                               accession = accession,
                               account = account,
                               ageTimeStep = age.time.step,
                               cumProbComp = cum.prob.comp,
                               datasets = datasets,
                               descriptions = descriptions,
                               diffProp = NA_integer_,
                               exposure = exposure,
                               expectedExposure = expected.exposure,
                               generatedNewProposal = new("LogicalFlag", FALSE),
                               hasAge = new("LogicalFlag", TRUE),
                               iAccNext = NA_integer_,
                               iAccNextOther = NA_integer_,
                               iBirths = i.births,
                               iCell = NA_integer_,
                               iCellOther = NA_integer_,
                               iComp = -1L,
                               iExpFirst = NA_integer_,
                               iExpFirstOther = NA_integer_,
                               iExposure = NA_integer_,
                               iExposureOther = NA_integer_,
                               iIntNet = i.int.net,
                               iOrigDest = i.orig.dest,
                               iParCh = i.par.ch,
                               iPopnNext = NA_integer_,
                               iPopnNextOther = NA_integer_,
                               iPool = i.pool,
                               isIncrement = is.increment,
                               isLowerTriangle = new("LogicalFlag", FALSE),
                               isNet = is.net,
                               iteratorAcc = iterator.acc,
                               iteratorExposure = iterator.exposure,
                               iteratorPopn = iterator.popn,
                               iteratorsComp = iterators.comp,
                               mappingsFromExp = mappings.from.exp,
                               mappingsToAcc = mappings.to.acc,
                               mappingsToExp = mappings.to.exp,
                               mappingsToPopn = mappings.to.popn,
                               modelUsesExposure = model.uses.exposure,
                               namesDatasets = namesDatasets,                           
                               nCellAccount = n.cell.account,
                               dataModels = dataModels,
                               probPopn = prob.popn,
                               seriesIndices = seriesIndices,
                               systemModels = systemModels,
                               transformExpToBirths = transform.exp.to.births,
                               transforms = transforms,
                               transformsExpToComp = transforms.exp.to.comp)
              }
              else {
                  methods::new("CombinedAccountMovementsNoAge",
                               account = account,
                               ageTimeStep = age.time.step,
                               cumProbComp = cum.prob.comp,
                               datasets = datasets,
                               descriptions = descriptions,
                               diffProp = NA_integer_,
                               exposure = exposure,
                               expectedExposure = expected.exposure,
                               generatedNewProposal = new("LogicalFlag", FALSE),
                               hasAge = new("LogicalFlag", FALSE),
                               iBirths = i.births,
                               iCell = NA_integer_,
                               iCellOther = NA_integer_,
                               iComp = -1L,
                               iExpFirst = NA_integer_,
                               iExpFirstOther = NA_integer_,
                               iExposure = NA_integer_,
                               iExposureOther = NA_integer_,
                               iIntNet = i.int.net,
                               iOrigDest = i.orig.dest,
                               iParCh = i.par.ch,
                               iPopnNext = NA_integer_,
                               iPopnNextOther = NA_integer_,
                               iPool = i.pool,
                               isIncrement = is.increment,
                               isNet = is.net,
                               iteratorExposure = iterator.exposure,
                               iteratorPopn = iterator.popn,
                               iteratorsComp = iterators.comp,
                               mappingsFromExp = mappings.from.exp,
                               mappingsToExp = mappings.to.exp,
                               mappingsToPopn = mappings.to.popn,
                               modelUsesExposure = model.uses.exposure,
                               namesDatasets = namesDatasets,
                               nCellAccount = n.cell.account,
                               dataModels = dataModels,
                               probPopn = prob.popn,
                               seriesIndices = seriesIndices,
                               systemModels = systemModels,
                               transformExpToBirths = transform.exp.to.births,
                               transforms = transforms,
                               transformsExpToComp = transforms.exp.to.comp)
              }
          })




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



