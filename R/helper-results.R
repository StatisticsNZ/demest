
## HAS_TESTS
## Should perhaps be turned into methods,
## but it seems like overkill
changeInPos <- function(object) {
    if (methods::is(object, "SkeletonOne"))
        1L
    else if (methods::is(object, "SkeletonMany"))
        object@last - object@first + 1L
    else if (is.list(object)) {
        total <- 0L
        for (i in seq_along(object))
            total <- total + Recall(object[[i]])
        total
    }
    else
        0L
}

## HAS_TESTS
fetchAdjustments <- function(filename, nIteration, lengthIter) {
    con <- file(filename, open = "rb")
    on.exit(close(con))
    size.results <- readBin(con = con, what = "integer", n = 1L)
    size.adjustments <- readBin(con = con, what = "integer", n = 1L)
    readBin(con = con, what = "raw", n = size.results)
    size.data <- nIteration * lengthIter
    readBin(con = con, what = "double", n = size.data)
    ans <- readBin(con = con, what = "raw", n = size.adjustments)
    unserialize(ans)
}

## HAS_TESTS
## 'dim' includes first element of 'along' dimension,
## but does not include 'season' dimension
indices0 <- function(iterator, nSeason = NULL, dim, iAlong) {
    if (!methods::is(iterator, "AlongIterator"))
        stop(gettextf("'%s' has class \"%s\"",
                      "iterator", class(iterator)))
    n.within <- iterator@nWithin
    n.between <- iterator@nBetween
    n.indices <- length(iterator@indices)
    n.dim <- length(dim)
    n <- n.within * n.between
    ans <- vector(mode = "list", length = n)
    iterator <- resetA(iterator)
    for (i in seq_len(n)) {
        indices <- iterator@indices
        if (is.null(nSeason))
            indices <- indices[1L]
        else {
            from <- (indices[1L] - 1L) * nSeason + 1L
            indices <- seq.int(from = from, length.out = nSeason)
            indices <- as.integer(indices)
        }
        ans[[i]] <- indices
        iterator <- advanceA(iterator, useC = TRUE)
    }
    ans <- unlist(ans)
    ans <- sort(ans)
    ans
}


## HAS_TESTS
## 'dim' includes first element of 'along' dimension,
## but does not include 'season' dimension
indicesShow <- function(iterator, nSeason = NULL, dim, iAlong) {
    if (!methods::is(iterator, "AlongIterator"))
        stop(gettextf("'%s' has class \"%s\"",
                      "iterator", class(iterator)))
    n.within <- iterator@nWithin
    n.between <- iterator@nBetween
    n.indices <- length(iterator@indices)
    n.dim <- length(dim)
    n <- n.within * n.between
    ans <- vector(mode = "list", length = n)
    iterator <- resetA(iterator)
    for (i in seq_len(n)) {
        indices <- iterator@indices
        indices <- indices[-1L]
        if (!is.null(nSeason))
            indices <- (indices - 1L) * nSeason + 1L
        ans[[i]] <- indices
        iterator <- advanceA(iterator, useC = TRUE)
    }
    ans <- unlist(ans)
    ans <- sort(ans)
    ans
}

isDamped <- function(object) {
    phi.known <- object@phiKnown@.Data
    min.phi <- object@minPhi
    phi <- object@phi
    if (phi.known)
        phi < 1
    else
        min.phi < 1
}

## HAS_TESTS
## assume that no subsetting and no permuting of elements within dimensions occurs
makeIndicesStrucZero <- function(strucZeroArray, margin) {
    if (is.null(strucZeroArray))
        integer()
    else {
        dim.before <- dim(strucZeroArray)
        dim.after <- dim.before[margin]
        s <- seq_along(dim.before)
        dims <- match(s, margin, nomatch = 0L)
        indices <- lapply(dim.before, seq_len)
        for (i in seq_along(dims)) {
            if (dims[i] == 0L)
                indices[[i]] <- rep(1L, times = dim.before[i])
        }
        transform <- methods::new("CollapseTransform",
                                  indices = indices,
                                  dims = dims,
                                  dimBefore = dim.before,
                                  dimAfter = dim.after)
        .Data <- strucZeroArray@.Data
        indices.struc.zero <- dembase::collapse(.Data, transform = transform)
        unname(which(indices.struc.zero@.Data == 0L))
    }
}

## HAS_TESTS
makeMetadata0 <- function(metadata, iAlong, nSeason) {
    dim <- dim(metadata)
    names <- names(metadata)
    dimtypes <- dimtypes(metadata, use.names = FALSE)
    DimScales <- DimScales(metadata, use.names = FALSE)
    if (!is.null(nSeason)) {
        nm.season <- make.unique(c(names, "season"))[length(names) + 1L]
        dimtype.season <- "state"
        DimScale.season <- methods::new("Categories", dimvalues = as.character(seq_len(nSeason)))
    }
    if (length(metadata) == 1L) {
        if (is.null(nSeason))
            NULL
        else
            methods::new("MetaData",
                nms = nm.season,
                dimtypes = dimtype.season,
                DimScales = list(DimScale.season))
    }
    else {
        names <- names[-iAlong]
        dimtypes <- dimtypes[-iAlong]
        DimScales <- DimScales[-iAlong]
        if (!is.null(nSeason)) {
            names <- c(nm.season, names)
            dimtypes <- c(dimtype.season, dimtypes)
            DimScales <- c(list(DimScale.season), DimScales)
        }
        methods::new("MetaData",
            nms = names,
            dimtypes = dimtypes,
            DimScales = DimScales)
    }
}

## HAS_TESTS
makeMetadataIncl0 <- function(metadata, iAlong, nSeason) {
    dim <- dim(metadata)
    names <- names(metadata)
    dimtypes <- dimtypes(metadata, use.names = FALSE)
    DimScales <- DimScales(metadata, use.names = FALSE)
    dimtypes[iAlong] <- "state"
    dimvalues.along <- as.character(seq_len(dim[iAlong] + 1L))
    DimScale.along <- methods::new("Categories", dimvalues = dimvalues.along)
    DimScales[[iAlong]] <- DimScale.along
    if (!is.null(nSeason)) {
        nm.season <- make.unique(c(names, "season"))[length(names) + 1L]
        names <- c(nm.season, names)
        dimtypes <- c("state", dimtypes)
        DimScale.season <- methods::new("Categories", dimvalues = as.character(seq_len(nSeason)))
        DimScales <- c(list(DimScale.season), DimScales)
    }
    methods::new("MetaData",
        nms = names,
        dimtypes = dimtypes,
        DimScales = DimScales)
}


## HAS_TESTS
makeMetadataVectorsMix <- function(metadata, iAlong, indexClassMax) {
    nms.old <- names(metadata)
    dimtypes.old <- dembase::dimtypes(metadata, use.names = FALSE)
    DimScales.old <- dembase::DimScales(metadata, use.names = FALSE)
    name.class <- make.unique(c(nms.old[-iAlong], "component"))[length(nms.old)]
    dimvalues.class <- as.character(seq_len(indexClassMax))
    DimScale.class <- methods::new("Categories", dimvalues = dimvalues.class)
    nms.ans <- replace(nms.old,
                       list = iAlong,
                       values = name.class)
    dimtypes.ans <- replace(dimtypes.old,
                            list = iAlong,
                            values = "state")
    DimScales.ans <- replace(DimScales.old,
                             list = iAlong,
                             values = list(DimScale.class))
    methods::new("MetaData",
                 nms = nms.ans,
                 dimtypes = dimtypes.ans,
                 DimScales = DimScales.ans)
}

## HAS_TESTS
makeMetadataWeightsMix <- function(metadata, iAlong, indexClassMax) {
    nms.old <- names(metadata)
    dimtypes.old <- dembase::dimtypes(metadata, use.names = FALSE)
    DimScales.old <- dembase::DimScales(metadata, use.names = FALSE)
    dimvalues.class <- as.character(seq_len(indexClassMax))
    DimScale.class <- methods::new("Categories", dimvalues = dimvalues.class)
    nms.ans <- make.unique(c(nms.old[iAlong], "component"))
    dimtypes.ans <- c(dimtypes.old[iAlong], "state")
    DimScales.ans <- c(DimScales.old[iAlong], list(DimScale.class))
    methods::new("MetaData",
                 nms = nms.ans,
                 dimtypes = dimtypes.ans,
                 DimScales = DimScales.ans)
}

## HAS_TESTS
makeOutputMCMC <- function(mcmcArgs, finalCombineds) {
    if (!identical(length(finalCombineds), mcmcArgs$nChain))
        stop(gettextf("length of '%s' [%d] not equal to '%s' argument [%d]",
                      "finalCombineds", length(finalCombineds), "nChain", mcmcArgs$nChain))
    n.sim <- mcmcArgs$nSim
    n.thin <- mcmcArgs$nThin
    n.chain <- mcmcArgs$nChain
    n.iteration <- as.integer(n.chain * (n.sim %/% n.thin))
    c(nBurnin = mcmcArgs$nBurnin,
      nSim = n.sim,
      nChain = n.chain,
      nThin = n.thin,
      nIteration = n.iteration)
}

## HAS_TESTS
makeOutputPriorCoef <- function(Z, pos) {
    names.eta <- colnames(Z)
    first <- pos
    ## values written to file include intercept,
    ## even though metadata does not include
    last <- pos + length(names.eta) - 1L
    dimvalues <- names.eta[-1L] # omit intercept
    DimScales <- list(methods::new("Categories", dimvalues = dimvalues))
    metadata <- methods::new("MetaData",
                             nms = "coef",
                             dimtypes = "state",
                             DimScales = DimScales)
    methods::new("SkeletonCovariates",
                 first = first,
                 last = last,
                 metadata = metadata)
}

## NO_TESTS
makeOutputPriorDamp <- function(pos) {
    Skeleton(first = pos)
}

## HAS_TESTS
makeOutputPriorScale <- function(pos) {
    Skeleton(first = pos)
}

## HAS_TESTS
makeOutputStateDLM <- function(iterator, metadata, nSeason, iAlong, pos, strucZeroArray = NULL, margin = NULL) {
    indices <- iterator@indices
    n.within <- iterator@nWithin
    n.between <- iterator@nBetween
    if (is.null(nSeason))
        length <- length(indices) * n.within * n.between
    else
        length <- length(indices) * n.within * n.between * nSeason
    first <- pos
    last <- pos + length - 1L
    dim <- dim(metadata)
    dim[iAlong] <- dim[iAlong] + 1L
    indices.0 <- indices0(iterator = iterator,
                          nSeason = nSeason,
                          dim = dim,
                          iAlong = iAlong)
    indices.show <- indicesShow(iterator = iterator,
                                nSeason = nSeason,
                                dim = dim,
                                iAlong = iAlong)
    metadata0 <- makeMetadata0(metadata = metadata,
                               iAlong = iAlong,
                               nSeason = nSeason)
    metadata.incl.0 <- makeMetadataIncl0(metadata = metadata,
                                         iAlong = iAlong,
                                         nSeason = nSeason)
    indices.struc.zero <- makeIndicesStrucZero(strucZeroArray = strucZeroArray,
                                               margin = margin)
    methods::new("SkeletonStateDLM",
                 first = first,
                 last = last,
                 iAlong = iAlong,
                 indicesShow = indices.show,
                 indices0 = indices.0,
                 indicesStrucZero = indices.struc.zero,
                 metadata = metadata,
                 metadata0 = metadata0,
                 metadataIncl0 = metadata.incl.0)
}

## HAS_TESTS
makePairsTerms <- function(margins) {
    if (length(margins) < 3L)
        return(list())
    margins <- margins[-1L] # intercept
    n <- length(margins)
    ans <- vector(mode = "list", length = n * (n - 1L) / 2L)
    i <- 1L
    for (j in seq.int(from = n, to = 2L)) {
        for (k in seq.int(from = j - 1L, to = 1L)) {
            first <- margins[[j]]
            second <- margins[[k]]
            first.is.higher.order <- length(first) > length(second)
            first.and.second.share.term <- any(second %in% first)
            if (first.is.higher.order && first.and.second.share.term)
                ans[[i]] <- c(j, k)
            else
                ans[[i]] <- NULL
            i <- i + 1L
        }
    }
    is.null <- sapply(ans, is.null)
    ans <- ans[!is.null]
    ans <- lapply(ans, function(x) x + 1L) # add 1 to allow for intercept
    ans
}

## HAS_TESTS
makeResultsFile <- function(filename, results, tempfiles) {
    kLength <- 10000
    con.write <- file(filename, open = "wb")
    on.exit(close(con.write))
    results <- serialize(results, connection = NULL)
    size.results <- length(results)
    writeBin(size.results, con = con.write)
    writeBin(0L, con = con.write) # placeholder for size of adjustment
    writeBin(results, con = con.write)
    for (i in seq_along(tempfiles)) {
        con.read <- file(tempfiles[i], "rb")
        finished <- FALSE
        while (!finished) {
            object <- readBin(con = con.read, what = "double", n = kLength)
            writeBin(object, con = con.write)
            finished <- length(object) < kLength
        }
        close(con.read)
        unlink(tempfiles[i])
    }
    NULL
}

## HAS_TESTS
makeResultsModelEst <- function(finalCombineds, mcmcArgs, controlArgs, seed) {
    combined <- finalCombineds[[1L]]
    model <- combined@model
    y <- combined@y
    has.exposure <- methods::is(combined, "HasExposure")
    exposure <- if (has.exposure) combined@exposure else NULL
    mcmc <- makeOutputMCMC(mcmcArgs = mcmcArgs,
                           finalCombineds = finalCombineds)
    n.sim <- mcmc[["nSim"]]
    final <- finalCombineds
    names(final) <- paste0("chain", seq_along(final))
    pos <- 1L
    if (n.sim > 0L) {
        output.model <- makeOutputModel(model = model, pos = pos, mcmc = mcmc)
        pos <- pos + changeInPos(output.model)
        if (any(is.na(y)))
            output.y <- SkeletonMissingData(y,
                                            model = model,
                                            outputModel = output.model,
                                            exposure = exposure)
        else
            output.y <- y
    }
    else {
        output.model <- list()
        output.y <- y
    }
    if (has.exposure) {
        methods::new("ResultsModelExposureEst",
                     model = output.model,
                     y = output.y,
                     exposure = exposure,
                     mcmc = mcmc,
                     control = controlArgs,
                     seed = seed,
                     final = final)
    }
    else
        methods::new("ResultsModelEst",
                     model = output.model,
                     y = output.y,
                     mcmc = mcmc,
                     control = controlArgs,
                     seed = seed,
                     final = final)
}

## HAS_TESTS
makeResultsModelPred <- function(finalCombineds, exposure, mcmcArgs, controlArgs, seed) {
    combined <- finalCombineds[[1L]]
    model <- combined@model
    y <- combined@y
    has.exposure <- !is.null(exposure)
    mcmc <- makeOutputMCMC(mcmcArgs = mcmcArgs,
                           finalCombineds = finalCombineds)
    final <- finalCombineds
    names(final) <- paste0("chain", seq_along(final))
    output.model <- makeOutputModel(model = model, pos = 1L, mcmc = mcmc)
    output.y <- SkeletonMissingData(y,
                                    model = model,
                                    outputModel = output.model,
                                    exposure = exposure)
    mcmc <- mcmc["nIteration"]
    if (has.exposure)
        methods::new("ResultsModelExposurePred",
                     model = output.model,
                     y = output.y,
                     exposure = exposure,
                     mcmc = mcmc,
                     control = controlArgs,
                     seed = seed,
                     final = final)
    else
        methods::new("ResultsModelPred",
                     model = output.model,
                     y = output.y,
                     mcmc = mcmc,
                     control = controlArgs,
                     seed = seed,
                     final = final)
}

## HAS_TESTS
makeResultsCounts <- function(finalCombineds, mcmcArgs, controlArgs, seed) {
    combined <- finalCombineds[[1L]]
    model <- combined@model
    y <- combined@y
    dataModels <- combined@dataModels
    datasets <- combined@datasets
    names.datasets <- combined@namesDatasets
    transforms <- combined@transforms
    has.struc.zero.array <- methods::is(model, "StrucZeroArrayMixin")
    if (has.struc.zero.array)
        struc.zero.array <- model@strucZeroArray
    mcmc <- makeOutputMCMC(mcmcArgs = mcmcArgs,
                           finalCombineds = finalCombineds)
    n.sim <- mcmc[["nSim"]]
    pos <- 1L
    if (n.sim > 0L) {
        output.model <- makeOutputModel(model = model, pos = pos, mcmc = mcmc)
        pos <- pos + changeInPos(output.model)
    }
    else
        model <- list()
    if (has.struc.zero.array)
        output.y <- Skeleton(y,
                             first = pos,
                             strucZeroArray = struc.zero.array)
    else
        output.y <- Skeleton(y,
                             first = pos)
    pos <- pos + changeInPos(output.y)
    has.exposure <- methods::is(combined, "HasExposure")
    if (has.exposure)
        exposure <- combined@exposure
    output.data.models <- vector(mode = "list", length = length(dataModels))
    if (n.sim > 0L) {
        for (i in seq_along(dataModels)) {
            output.data.models[[i]] <- makeOutputModel(model = dataModels[[i]],
                                                       pos = pos,
                                                       mcmc = mcmc)
            pos <- pos + changeInPos(output.data.models[[i]])
        }
        for (i in seq_along(datasets)) {
            if (any(is.na(datasets[[i]])))
                datasets[[i]] <-
                    SkeletonMissingDataset(object = datasets[[i]],
                                           model = dataModels[[i]],
                                           outputModel = output.data.models[[i]],
                                           transformComponent = transforms[[i]],
                                           skeletonComponent = output.y)
        }
    }
    names(output.data.models) <- names.datasets
    names(datasets) <- names.datasets
    final <- finalCombineds
    names(final) <- paste("chain", seq_along(final), sep = "")
    if (has.exposure) {
        methods::new("ResultsCountsExposureEst",
                     model = output.model,
                     y = output.y,
                     exposure = exposure,
                     dataModels = output.data.models,
                     datasets = datasets,
                     mcmc = mcmc,
                     control = controlArgs,
                     seed = seed,
                     final = final)
    }
    else
        methods::new("ResultsCountsEst",
                     model = output.model,
                     y = output.y,
                     dataModels = output.data.models,
                     datasets = datasets,
                     mcmc = mcmc,
                     control = controlArgs,
                     seed = seed,
                     final = final)
}

## HAS_TESTS
makeResultsAccount <- function(finalCombineds, mcmcArgs, controlArgs, seed) {
    combined <- finalCombineds[[1L]]
    account <- combined@account
    names.components <- account@namesComponents
    system.models <- combined@systemModels
    data.models <- combined@dataModels
    datasets <- combined@datasets
    names.datasets <- combined@namesDatasets
    transforms <- combined@transforms
    series.indices <- combined@seriesIndices
    names.series <- c("population", names.components)
    ## mcmc
    mcmc <- makeOutputMCMC(mcmcArgs = mcmcArgs,
                           finalCombineds = finalCombineds)
    n.sim <- mcmc[["nSim"]]
    ## account
    pos <- 1L
    if (n.sim > 0) {
        output.account <- makeOutputAccount(account = account,
                                            systemModels = system.models,
                                            pos = pos)
        pos <- pos + changeInPos(output.account)
    }
    else {
        output.account <- vector(mode = "list", length = length(names.series))
        names(output.account) <- names.series
    }
    ## system models
    output.system.models <- vector(mode = "list",
                                   length = length(system.models))
    if (n.sim > 0L) {
        for (i in seq_along(system.models)) {
            output.system.models[[i]] <- makeOutputModel(model = system.models[[i]],
                                                         pos = pos,
                                                         mcmc = mcmc)
            pos <- pos + changeInPos(output.system.models[[i]])
        }
    }
    names(output.system.models) <- names.series
    ## data models
    output.data.models <- vector(mode = "list",
                                 length = length(data.models))
    if (n.sim > 0L) {
        for (i in seq_along(data.models)) {
            output.data.models[[i]] <- makeOutputModel(model = data.models[[i]],
                                                       pos = pos,
                                                       mcmc = mcmc)
            pos <- pos + changeInPos(output.data.models[[i]])
        }
    }
    names(output.data.models) <- names.datasets
    ## datasets
    output.datasets <- vector(mode = "list", length = length(datasets))
    for (i in seq_along(datasets)) {
        has.missing <- any(is.na(datasets[[i]]))
        if (has.missing && (n.sim > 0L)) {
            index <- series.indices[i] + 1L
            output.series <- output.account[[index]]
            output.datasets[[i]] <- SkeletonMissingDataset(object = datasets[[i]],
                                                           model = data.models[[i]],
                                                           outputModel = output.data.models[[i]],
                                                           transformComponent = transforms[[i]],
                                                           skeletonComponent = output.series)
        }
        else
            output.datasets[[i]] <- datasets[[i]]
    }
    names(datasets) <- names.datasets
    final <- finalCombineds
    names(final) <- paste("chain", seq_along(final), sep = "")
    methods::new("ResultsAccount",
                 account = output.account,
                 systemModels = output.system.models,
                 dataModels = output.data.models,
                 datasets = datasets,
                 mcmc = mcmc,
                 control = controlArgs,
                 seed = seed,
                 final = final)
}


## TRANSLATED
## HAS_TESTS
overwriteValuesOnFile <- function(object, skeleton, filename,
                                  nIteration, lengthIter,
                                  useC = TRUE) {
    ## object
    stopifnot(methods::is(object, "Values"))
    stopifnot(is.double(object))
    ## skeleton
    stopifnot(methods::is(skeleton, "Skeleton"))
    ## nIteration
    stopifnot(is.integer(nIteration))
    stopifnot(identical(length(nIteration), 1L))
    stopifnot(!is.na(nIteration))
    stopifnot(nIteration >= 1L)
    ## lengthIter
    stopifnot(is.integer(lengthIter))
    stopifnot(identical(length(lengthIter), 1L))
    stopifnot(!is.na(lengthIter))
    stopifnot(lengthIter >= 1L)
    if (useC) {
        .Call(overwriteValuesOnFile_R, object, skeleton,
              filename, nIteration, lengthIter)
    }
    else {
        ## Based on the warnings in the help for 'seek' about using
        ## 'seek' with Windows, we have avoided it, and
        ## used writeBin and readBin instead.
        ## The pointer used for reading and the pointer used for
        ## writing are independent of each other, so we have to
        ## move each separately via read or write operations
        first <- skeleton@first
        last <- skeleton@last
        con <- file(filename, open = "r+b")
        on.exit(close(con))
        size.results <- readBin(con = con, what = "integer", n = 1L)
        writeBin(size.results, con = con)
        size.adj <- readBin(con = con, what = "integer", n = 1L)
        writeBin(size.adj, con = con)
        results <- readBin(con = con, what = "raw", n = size.results)
        writeBin(results, con = con)
        pos <- 1L ## position within object
        for (i.iter in seq_len(nIteration)) {
            ## skip over values in file before start of data
            before.first <- readBin(con = con, what = "double", n = first - 1L)
            writeBin(before.first, con = con)
            ## write values
            for (i.col in seq.int(from = first, to = last)) {
                readBin(con = con, what = "double", n = 1L) # discard value
                writeBin(object[pos], con = con)
                pos <- pos + 1L
            }
            ## skip remaining positions in line of file, if any
            if (last < lengthIter) {
                after.last <- readBin(con = con, what = "double", n = lengthIter - last)
                writeBin(after.last, con = con)
            }
        }
    }
}

## HAS_TESTS
readCoefInterceptFromFile <- function(skeleton, filename,
                                      nIteration, lengthIter) {
    first <- skeleton@first
    iterations <- seq_len(nIteration)
    .Data <- getDataFromFile(filename = filename,
                             first = first,
                             last = first,
                             lengthIter = lengthIter,
                             iterations = iterations)
    metadata <- methods::new("MetaData",
                             nms = "iteration",
                             dimtypes = "iteration",
                             DimScales = list(methods::new("Iterations",
                                                           dimvalues = iterations)))
    .Data <- array(.Data,
                   dim = dim(metadata),
                   dimnames = dimnames(metadata))
    methods::new("Values",
                 .Data = .Data,
                 metadata = metadata)
}

## HAS_TESTS
rescaleAndWriteBetas <- function(high, low, adj, skeletonHigh, skeletonLow,
                                 filename, nIteration, lengthIter) {
    high <- high - adj
    low <- low + adj
    overwriteValuesOnFile(object = high,
                          skeleton = skeletonHigh,
                          filename = filename,
                          nIteration = nIteration,
                          lengthIter = lengthIter)
    overwriteValuesOnFile(object = low,
                          skeleton = skeletonLow,
                          filename = filename,
                          nIteration = nIteration,
                          lengthIter = lengthIter)
    NULL
}

## HAS_TESTS
rescaleBetasPredHelper <- function(priorsBetas, namesBetas, skeletonsBetas,
                                   adjustments, prefixAdjustments,
                                   filename, nIteration, lengthIter) {
    for (i in seq_along(priorsBetas)) {
        prior <- priorsBetas[[i]]
        name <- namesBetas[i]
        skeleton <- skeletonsBetas[[i]]
        name.adj <- paste(prefixAdjustments, "prior", name, sep = ".")
        adjustment <- adjustments[[name.adj]]
        rescalePred(prior = prior,
                    skeleton = skeleton,
                    adjustment = adjustment,
                    filename = filename,
                    nIteration = nIteration,
                    lengthIter = lengthIter)
    }
}

## HAS_TESTS
rescaleInFile <- function(filename) {
    results <- fetchResultsObject(filename)
    nIteration <- results@mcmc["nIteration"]
    lengthIter <- results@control$lengthIter
    adjustments <- new.env(hash = TRUE) # modified in-place
    if (nIteration > 0L) {
        rescalePriors(results = results,
                      adjustments = adjustments,
                      filename = filename,
                      nIteration = nIteration,
                      lengthIter = lengthIter)
    }
    adjustments.serialized <- serialize(adjustments,
                                        connection = NULL)
    size.adjustments <- length(adjustments.serialized)
    con <- file(filename, open = "r+b")
    on.exit(close(con))
    size.results <- readBin(con = con, what = "integer", n = 1L)
    writeBin(size.results, con = con)
    readBin(con, what = "integer", n = 1L)
    writeBin(size.adjustments, con = con)
    results <- readBin(con, what = "raw", n = size.results)
    writeBin(results, con = con)
    for (i in seq_len(nIteration)) {
        line <- readBin(con, what = "double", n = lengthIter)
        writeBin(line, con = con)
    }
    writeBin(adjustments.serialized, con = con)    
    NULL
}

## HAS_TESTS
rescaleInFilePred <- function(filenameEst, filenamePred) {
    ## get 'adjustments' from filenameEst
    results.est <- fetchResultsObject(filenameEst)
    results.pred <- fetchResultsObject(filenamePred)
    nIteration.est <- results.est@mcmc["nIteration"]
    nIteration.pred <- results.pred@mcmc["nIteration"]
    lengthIter.est <- results.est@control$lengthIter
    lengthIter.pred <- results.pred@control$lengthIter
    con.est <- file(filenameEst, open = "rb")
    size.results.est <- readBin(con = con.est, what = "integer", n = 1L)
    size.adjustments <- readBin(con = con.est, what = "integer", n = 1L)
    readBin(con = con.est, what = "raw", n = size.results.est)
    for (i in seq_len(nIteration.est))
        readBin(con = con.est, what = "double", n = lengthIter.est)
    adjustments.serialized <- readBin(con = con.est, what = "raw", n = size.adjustments)
    close(con.est)
    adjustments <- unserialize(adjustments.serialized)
    ## rescale
    rescaleBetasPred(results = results.pred,
                     adjustments = adjustments,
                     filename = filenamePred,
                     nIteration = nIteration.pred,
                     lengthIter = lengthIter.pred)
    ## add 'adjustments' to filenamePred
    con.pred <- file(filenamePred, open = "r+b")
    on.exit(close(con.pred))
    size.results.pred <- readBin(con = con.pred, what = "integer", n = 1L)
    writeBin(size.results.pred, con = con.pred)
    readBin(con.pred, what = "integer", n = 1L)
    writeBin(size.adjustments, con = con.pred)
    results.pred.serialized <- readBin(con.pred, what = "raw", n = size.results.pred)
    writeBin(results.pred.serialized, con = con.pred)
    for (i in seq_len(nIteration.pred)) {
        line <- readBin(con.pred, what = "double", n = lengthIter.pred)
        writeBin(line, con = con.pred)
    }
    writeBin(adjustments.serialized, con = con.pred)    
    NULL
}

## NO_TESTS
readStateDLMFromFile <- function(skeleton, filename, iterations,
                                 nIteration, lengthIter, only0) {
    iAlong <- skeleton@iAlong
    first <- skeleton@first
    last <- skeleton@last
    if (is.null(iterations))
        iterations <- seq_len(nIteration)
    n.iter <- length(iterations)
    .Data <- getDataFromFile(filename = filename,
                             first = first,
                             last = last,
                             lengthIter = lengthIter,
                             iterations = iterations)
    .Data <- matrix(.Data, ncol = n.iter)
    if (only0) {
        indices0 <- skeleton@indices0
        metadata <- skeleton@metadata0
        if (is.null(metadata)) {
            metadata <- methods::new("MetaData",
                            nms = "iteration",
                            dimtypes = "iteration",
                            DimScales = list(methods::new("Iterations", dimvalues = seq_len(nIteration))))
        }
        else
            metadata <- dembase::addIterationsToMetadata(metadata, iterations = iterations)        
        .Data <- .Data[indices0, ]
    }
    else {
        metadata <- skeleton@metadataIncl0
        metadata <- dembase::addIterationsToMetadata(metadata, iterations = iterations)        
    }
    .Data <- array(.Data, dim = dim(metadata), dimnames = dimnames(metadata))
    methods::new("Values", .Data = .Data, metadata = metadata)
}

## HAS_TESTS
recordAdjustments <- function(priorHigh, priorLow, namesHigh, namesLow,
                              adj, adjustments, prefixAdjustments) {
    prefix.adjustments <- paste(prefixAdjustments, "prior", sep = ".")
    if (methods::is(priorHigh, "Exchangeable")) {
        name.high <- paste(namesHigh, collapse = ":")
        name.high <- paste(prefix.adjustments, name.high, sep = ".")
        already.has.high <- !is.null(adjustments[[name.high]])
        if (already.has.high)
            adjustments[[name.high]] <- adjustments[[name.high]] - adj
        else
            adjustments[[name.high]] <- -1 * adj
    }
    if (methods::is(priorLow, "Exchangeable")) {
        name.low <- paste(namesLow, collapse = ":")
        name.low <- paste(prefix.adjustments, name.low, sep = ".")
        already.has.low <- !is.null(adjustments[[name.low]])
        if (already.has.low)
            adjustments[[name.low]] <- adjustments[[name.low]] + adj
        else
            adjustments[[name.low]] <- adj
    }
    NULL
}

## HAS_TESTS
rescalePriorsHelper <- function(priors, margins, skeletonsBetas, skeletonsPriors,
                                adjustments, prefixAdjustments,
                                filename, nIteration, lengthIter) {
    for (i in seq_along(priors)) {
        if (i != 1L) {
            rescaleSeason(prior = priors[[i]],
                          skeleton = skeletonsPriors[[i]],
                          filename = filename,
                          nIteration = nIteration,
                          lengthIter = lengthIter)
        }
    }
    pairs.terms <- makePairsTerms(margins)
    for (pair.terms in pairs.terms) {
        i.higher <- pair.terms[1L]
        i.lower <- pair.terms[2L]
        prior.higher <- priors[[i.higher]]
        prior.lower <- priors[[i.lower]]
        skeleton.beta.higher <- skeletonsBetas[[i.higher]]
        skeleton.beta.lower <- skeletonsBetas[[i.lower]]
        skeletons.prior.higher <- skeletonsPriors[[i.higher]]
        skeletons.prior.lower <- skeletonsPriors[[i.lower]]
        rescalePairPriors(priorHigh = prior.higher,
                          priorLow = prior.lower,
                          skeletonBetaHigh = skeleton.beta.higher,
                          skeletonBetaLow = skeleton.beta.lower,
                          skeletonsPriorHigh = skeletons.prior.higher,
                          skeletonsPriorLow = skeletons.prior.lower,
                          adjustments = adjustments,
                          prefixAdjustments = prefixAdjustments,
                          filename = filename,
                          nIteration = nIteration,
                          lengthIter = lengthIter)
    }
    prior.intercept <- priors[[1L]]
    skeleton.beta.intercept <- skeletonsBetas[[1L]]
    for (i in seq_along(priors)) {
        if (i != 1L) {
            prior.term <- priors[[i]]
            skeleton.beta.term <- skeletonsBetas[[i]]
            skeletons.prior.term <- skeletonsPriors[[i]]
            rescalePriorIntercept(priorTerm = prior.term,
                                  priorIntercept = prior.intercept,
                                  skeletonBetaTerm = skeleton.beta.term,
                                  skeletonBetaIntercept = skeleton.beta.intercept,
                                  skeletonsPriorTerm = skeletons.prior.term,
                                  adjustments = adjustments,
                                  prefixAdjustments = prefixAdjustments,
                                  filename = filename,
                                  nIteration = nIteration,
                                  lengthIter = lengthIter)
        }
    }
}

## HAS_TESTS
setCoefInterceptToZeroOnFile <- function(skeleton, filename,
                                         nIteration, lengthIter) {
    ## Based on the warnings in the help for 'seek' about using
    ## 'seek' with Windows, we have avoided it, and
    ## used writeBin and readBin instead.
    ## The pointer used for reading and the pointer used for
    ## writing are independent of each other, so we have to
    ## move each separately via read or write operations
    first <- skeleton@first
    con <- file(filename, open = "r+b")
    on.exit(close(con))
    size.results <- readBin(con = con, what = "integer", n = 1L)
    writeBin(size.results, con = con)
    size.adj <- readBin(con = con, what = "integer", n = 1L)
    writeBin(size.adj, con = con)
    results <- readBin(con = con, what = "raw", n = size.results)
    writeBin(results, con = con)
    for (i.iter in seq_len(nIteration)) {
        ## skip over values in line before start of data
        before.first <- readBin(con = con, what = "double", n = first - 1L)
        writeBin(before.first, con = con)
        ## write 0
        readBin(con = con, what = "double", n = 1L) # discard value
        writeBin(0, con = con)
        ## skip remaining positions in line of file, if any
        if (first < lengthIter) {
            after.first <- readBin(con = con, what = "double", n = lengthIter - first)
            writeBin(after.first, con = con)
        }
    }
}


