
## HAS_TESTS
DimScaleIsRegular <- function(x) {
    if (!methods::is(x, "Points") && !methods::is(x, "Intervals"))
        stop(gettextf("'%s' has class \"%s\"",
                      "x", class(x)))
    dimvalues <- x@dimvalues
    dimvalues <- dimvalues[is.finite(dimvalues)]
    n <- length(dimvalues)
    if (n < 3L)
        TRUE
    else {
        steps <- diff(dimvalues)
        all(steps[1L] == steps[-1L])
    }
}

## HAS_TESTS
addAgCertain <- function(object, aggregate, defaultWeights) {
    theta <- object@theta
    metadata.y <- object@metadataY
    value <- aggregate@valueAg
    weight <- aggregate@weightAg
    metadata.ag <- aggregate@metadataAg
    concordances.ag <- aggregate@concordancesAg
    .Data.theta.obj <- array(0,
                             dim = dim(metadata.y),
                             dimnames = dimnames(metadata.y))
    theta.obj <- methods::new("Counts",
                              .Data = .Data.theta.obj,
                              metadata = metadata.y)
    if (is.null(metadata.ag))
        ag.obj <- 0
    else {
        .Data.ag.obj <- array(0,
                              dim = dim(metadata.ag),
                              dimnames = dimnames(metadata.ag))
        ag.obj <- methods::new("Counts",
                               .Data = .Data.ag.obj,
                               metadata = metadata.ag)
    }
    transform <- dembase::makeTransform(x = theta.obj,
                                        y = ag.obj,
                                        subset = TRUE,
                                        concordances = concordances.ag)
    transform <- dembase::makeCollapseTransformExtra(transform)
    weight <- makeWeightAg(weight = weight,
                           default = defaultWeights,
                           model = object,
                           thetaObj = theta.obj,
                           transform = transform,
                           values = value)
    value <- as.double(value)
    mu <- rep(0, times = length(theta))
    for (k in seq_along(value)) {
        i.th <- dembase::getIBefore(k, transform = transform, useC = TRUE)
        sum.wt <- sum(abs(weight[i.th]))
        if (sum.wt > 0)
            theta[i.th] <- value[k] / sum.wt
    }
    value <- methods::new("ParameterVector", value)
    class <- paste0(class(object), "AgCertain")
    slotsToExtract <- methods::new(class)@slotsToExtract
    iMethodModel <- methods::new(class)@iMethodModel
    list(theta = theta,
         value = value,
         weight = weight,
         transform = transform,
         metadata = metadata.ag,
         mu = mu,
         slotsToExtract = slotsToExtract,
         iMethodModel = iMethodModel)
}

## HAS_TESTS
addAgNormal <- function(object, aggregate, defaultWeights) {
    theta <- object@theta
    metadata.y <- object@metadataY
    mean <- aggregate@valueAg # 'value' from Spec becomes 'mean' in object
    scale <- aggregate@scaleAg
    sd <- aggregate@sdAg
    weight <- aggregate@weightAg
    metadata.ag <- aggregate@metadataAg
    concordances.ag <- aggregate@concordancesAg
    .Data.theta.obj <- array(0,
                             dim = dim(metadata.y),
                             dimnames = dimnames(metadata.y))
    theta.obj <- methods::new("Counts", .Data = .Data.theta.obj, metadata = metadata.y)
    if (is.null(metadata.ag))
        ag.obj <- 0
    else {
        .Data.ag.obj <- array(0,
                              dim = dim(metadata.ag),
                              dimnames = dimnames(metadata.ag))
        ag.obj <- methods::new("Counts", .Data = .Data.ag.obj, metadata = metadata.ag)
    }
    transform <- dembase::makeTransform(x = theta.obj,
                                        y = ag.obj,
                                        subset = TRUE,
                                        concordances = concordances.ag)
    transform <- dembase::makeCollapseTransformExtra(transform)
    weight <- makeWeightAg(weight = weight,
                           default = defaultWeights,
                           model = object,
                           thetaObj = theta.obj,
                           transform = transform,
                           values = mean)
    value <- array(weight * theta, dim = dim(metadata.y))
    value <- dembase::collapse(value, transform = transform)
    value <- as.double(value)
    value <- methods::new("ParameterVector", value)
    mean <- as.double(mean)
    mean <- methods::new("ParameterVector", mean)
    sd <- as.double(sd)
    sd <- methods::new("ScaleVec", sd)
    mu <- rep(0, times = length(theta))
    class <- paste0(class(object), "AgNormal")
    slotsToExtract <- methods::new(class)@slotsToExtract
    iMethodModel <- methods::new(class)@iMethodModel
    list(value = value,
         mean = mean,
         scale = scale,
         sd = sd,
         weight = weight,
         transform = transform,
         metadata = metadata.ag,
         mu = mu,
         slotsToExtract = slotsToExtract,
         iMethodModel = iMethodModel)
}

## HAS_TESTS
addAgPoisson <- function(object, aggregate, defaultWeights) {
    theta <- object@theta
    metadata.y <- object@metadataY
    mean <- aggregate@valueAg # 'value' from Spec becomes 'mean' in object
    scale <- aggregate@scaleAg
    metadata.ag <- aggregate@metadataAg
    concordances.ag <- aggregate@concordancesAg
    .Data.theta.obj <- array(0,
                             dim = dim(metadata.y),
                             dimnames = dimnames(metadata.y))
    theta.obj <- methods::new("Counts", .Data = .Data.theta.obj, metadata = metadata.y)
    if (is.null(metadata.ag))
        ag.obj <- 0
    else {
        .Data.ag.obj <- array(0,
                              dim = dim(metadata.ag),
                              dimnames = dimnames(metadata.ag))
        ag.obj <- methods::new("Counts", .Data = .Data.ag.obj, metadata = metadata.ag)
    }
    transform <- dembase::makeTransform(x = theta.obj,
                                        y = ag.obj,
                                        subset = TRUE,
                                        concordances = concordances.ag)
    transform <- dembase::makeCollapseTransformExtra(transform)
    weight <- makeWeightAg(weight = NULL,
                           default = defaultWeights,
                           model = object,
                           thetaObj = theta.obj,
                           transform = transform,
                           values = mean)
    value <- array(weight * theta, dim = dim(metadata.y))
    value <- dembase::collapse(value, transform = transform)
    value <- as.double(value)
    value <- methods::new("ParameterVector", value)
    exposure <- dembase::collapse(defaultWeights@.Data, transform = transform)
    exposure <- as.double(exposure)
    if (!all(exposure > 0))
        stop(gettext("exposure term in Poisson aggregate model contains non-positive values"))
    exposure <- methods::new("ScaleVec", exposure)
    mean <- as.double(mean)
    mean <- methods::new("ParameterVector", mean)
    mu <- rep(0, times = length(theta))
    class <- paste0(class(object), "AgPoisson")
    slotsToExtract <- methods::new(class)@slotsToExtract
    iMethodModel <- methods::new(class)@iMethodModel
    list(value = value,
         mean = mean,
         scale = scale,
         weight = weight,
         exposure = exposure,
         transform = transform,
         metadata = metadata.ag,
         mu = mu,
         slotsToExtract = slotsToExtract,
         iMethodModel = iMethodModel)
}
        
## HAS_TESTS
addAgFun <- function(object, aggregate, defaultWeights) {
    theta <- object@theta
    metadata.y <- object@metadataY
    mean <- aggregate@valueAg # 'value' from Spec becomes 'mean' in object
    sd <- aggregate@sdAg
    weight <- aggregate@weightAg
    metadata.ag <- aggregate@metadataAg
    fun.ag <- aggregate@funAg
    concordances.ag <- aggregate@concordancesAg
    .Data.theta.obj <- array(0,
                             dim = dim(metadata.y),
                             dimnames = dimnames(metadata.y))
    theta.obj <- methods::new("Counts",
                              .Data = .Data.theta.obj,
                              metadata = metadata.y)
    if (is.null(metadata.ag))
        ag.obj <- 0
    else {
        .Data.ag.obj <- array(0,
                              dim = dim(metadata.ag),
                              dimnames = dimnames(metadata.ag))
        ag.obj <- methods::new("Counts",
                               .Data = .Data.ag.obj,
                               metadata = metadata.ag)
    }
    transform <- dembase::makeTransform(x = theta.obj,
                                        y = ag.obj,
                                        subset = TRUE,
                                        concordances = concordances.ag)
    transform <- dembase::makeCollapseTransformExtra(transform)
    weight <- makeWeightAg(weight = weight,
                           default = defaultWeights,
                           model = object,
                           thetaObj = theta.obj,
                           transform = transform,
                           values = mean)
    metadata.args <- dembase::makeMetaDataSubarraysBefore(metadata = metadata.y,
                                                          transform = transform)
    n.value <- length(mean)
    x.args <- vector(mode = "list", length = n.value)
    weights.args <- vector(mode = "list", length = n.value)
    value <- double(length = n.value)
    for (i in seq_len(n.value)) {
        i.y <- getIBefore(i = i,
                          transform = transform,
                          useC = TRUE)
        metadata.i <- metadata.args[[i]]
        .Data.x <- theta[i.y]
        .Data.weights <- weight[i.y]
        .Data.x <- array(.Data.x,
                         dim = dim(metadata.i),
                         dimnames = dimnames(metadata.i))
        .Data.weights <- array(.Data.weights,
                               dim = dim(metadata.i),
                               dimnames = dimnames(metadata.i))
        x.args[[i]] <- methods::new("Values",
                                    .Data = .Data.x,
                                    metadata = metadata.i)
        weights.args[[i]] <- methods::new("Counts",
                                          .Data = .Data.weights,
                                          metadata = metadata.i)
        val <- tryCatch(fun.ag(x = x.args[[i]], 
                               weights = weights.args[[i]]),
                        error = function(e) e)
        if (methods::is(val, "error"))
            stop(gettextf("error applying '%s' : %s",
                          "FUN", val$message))
        if (!is.numeric(val))
            stop(gettextf("return value from '%s' has class \"%s\"",
                          "FUN", class(val)))
        if (!identical(length(val), 1L))
            stop(gettextf("return value from '%s' has length %d",
                          "FUN", length(val)))
        value[i] <- val
    }
    value <- methods::new("ParameterVector", value)
    mean <- as.double(mean)
    mean <- methods::new("ParameterVector", mean)
    sd <- as.double(sd)
    sd <- methods::new("ScaleVec", sd)
    class <- paste0(class(object), "AgFun")
    slotsToExtract <- methods::new(class)@slotsToExtract
    iMethodModel <- methods::new(class)@iMethodModel
    list(value = value,
         mean = mean,
         sd = sd,
         metadata = metadata.ag,
         transform = transform,
         funAg = fun.ag,
         xArgs = x.args,
         weightsArgs = weights.args,
         slotsToExtract = slotsToExtract,
         iMethodModel = iMethodModel)
}

## HAS_TESTS
addAgLife <- function(object, aggregate, defaultWeights) {
    theta <- object@theta
    metadata.y <- object@metadataY
    mean <- aggregate@valueAg # 'value' from Spec becomes 'mean' in object
    sd <- aggregate@sdAg
    ax <- aggregate@axAg
    metadata.ag <- aggregate@metadataAg
    concordances.ag <- aggregate@concordancesAg
    names.y <- names(metadata.y)
    dimtypes.y <- dimtypes(metadata.y, use.names = FALSE)
    DimScales.y <- DimScales(metadata.y, use.names = FALSE)
    i.age.y <- match("age", dimtypes.y, nomatch = 0L)
    has.age.y <- i.age.y > 0L
    if (!has.age.y)
        stop(gettextf("'%s' does not have a dimension with %s \"%s\"",
                      "y", "dimtype", "age"))
    name.age <- names.y[i.age.y]
    DimScale.age <- DimScales.y[[i.age.y]]
    .Data.theta.obj <- array(0,
                             dim = dim(metadata.y),
                             dimnames = dimnames(metadata.y))
    theta.obj <- methods::new("Counts",
                              .Data = .Data.theta.obj,
                              metadata = metadata.y)
    if (is.null(metadata.ag)) {
        names.ag <- NULL
        dimtypes.ag <- NULL
        DimScales.ag <- NULL
    }
    else {
        names.ag <- names(metadata.ag)
        dimtypes.ag <- dimtypes(metadata.ag, use.names = FALSE)
        DimScales.ag <- DimScales(metadata.ag, use.names = FALSE)
    }    
    names.mx <- c(name.age, names.ag)
    dimtypes.mx <- c("age", dimtypes.ag)
    DimScales.mx <- c(list(DimScale.age), DimScales.ag)
    metadata.mx <- methods::new("MetaData",
                                nms = names.mx,
                                dimtypes = dimtypes.mx,
                                DimScales = DimScales.mx)
    .Data.mx.obj <- array(0,
                          dim = dim(metadata.mx),
                          dimnames = dimnames(metadata.mx))
    mx.obj <- methods::new("Values",
                           .Data = .Data.mx.obj,
                           metadata = metadata.mx)
    transform <- tryCatch(dembase::makeTransform(x = theta.obj,
                                                 y = mx.obj,
                                                 subset = TRUE,
                                                 concordances = concordances.ag,
                                                 check = TRUE),
                          error = function(e) e)
    if (methods::is(transform, "error"))
        stop(gettextf("'%s' not compatible with '%s' from '%s' : %s",
                      "y", "value", "AgLife", transform$message))
    numerator.mx <- theta * defaultWeights
    numerator.mx <- numerator.mx@.Data
    denominator.mx <- defaultWeights@.Data
    numerator.mx <- collapse(numerator.mx,
                             transform = transform)
    denominator.mx <- collapse(denominator.mx,
                               transform = transform)
    mx <- numerator.mx / denominator.mx
    mx <- array(mx,
                dim = dim(metadata.mx),
                dimnames = dimnames(metadata.mx))
    mx <- methods::new("Values",
                       .Data = mx,
                       metadata = metadata.mx)
    if (is.null(ax))
        ax <- makeAxStart(mx) # mx must have metadata
    ax <- expandAx(ax = ax,
                   object = mx) # mx must have metadata
    dv.age <- DimScale.age@dimvalues
    nx <- diff(dv.age)
    mx <- as.double(mx)
    ax <- as.double(ax)
    transform <- dembase::makeCollapseTransformExtra(transform)
    value <- double(length = length(mean))
    nAge <- length(nx)
    seq.iAge0 <- seq.int(from = 1L,
                         by = nAge,
                         to = length(mx))
    for (i in seq_along(seq.iAge0)) {
        iAge0 <- seq.iAge0[i]
        value[i] <- makeLifeExpBirth(mx = mx,
                                     nx = nx,
                                     ax = ax,
                                     iAge0 = iAge0,
                                     nAge= nAge)
    }
    value <- methods::new("ParameterVector", value)
    mean <- as.double(mean)
    mean <- methods::new("ParameterVector", mean)
    sd <- as.double(sd)
    sd <- methods::new("ScaleVec", sd)
    nAge <- methods::new("Length", nAge)
    class <- paste0(class(object), "AgLife")
    slotsToExtract <- methods::new(class)@slotsToExtract
    iMethodModel <- methods::new(class)@iMethodModel
    list(value = value,
         mean = mean,
         sd = sd,
         metadataAg = metadata.ag,
         transform = transform,
         metadataMx = metadata.mx,
         mx = mx,
         ax = ax,
         nx = nx,
         nAge = nAge,
         slotsToExtract = slotsToExtract,
         iMethodModel = iMethodModel)
}

## HAS_TESTS
checkAndTidyDatasets <- function(datasets) {
    if (!is.list(datasets))
        stop(gettextf("'%s' has class \"%s\"",
                      "datasets", class(datasets)))
    if (identical(length(datasets), 0L))
        stop(gettextf("'%s' has length %d",
                      "datasets", 0L))
    names.datasets <- names(datasets)
    checkListNames(names = names.datasets,
                   listName = "datasets")
    ans <- vector(mode = "list", length = length(datasets))
    for (i in seq_along(ans)) {
        dataset <- datasets[[i]]
        if (!methods::is(dataset, "Counts"))
            stop(gettextf("dataset '%s' has class \"%s\"",
                          names.datasets[i], class(dataset)))
        if (any(round(dataset) != dataset, na.rm = TRUE))
            stop(gettextf("dataset '%s' has non-integer values",
                          names.datasets[i]))
        if (any(dataset < 0, na.rm = TRUE))
            stop(gettextf("dataset '%s' has negative values",
                          names.datasets[i]))
        dataset <- dembase::toInteger(dataset)
        ans[[i]] <- dataset
        names(ans) <- names.datasets
    }
    ans
}

## HAS_TESTS
checkAndTidyExposure <- function(exposure, y) {
    if (is.null(exposure))
        return(NULL)
    if (!methods::is(exposure, "Counts"))
        stop(gettextf("'%s' has class \"%s\"",
                      "exposure", class(exposure)))
    exposure <- dembase::makeCompatible(x = exposure, y = y, subset = TRUE)
    if (any(exposure[!is.na(exposure)] < 0))
        stop(gettextf("'%s' has negative values",
                      "exposure"))
    if (any(is.na(exposure) > is.na(y)))
        stop(gettextf("'%s' has missing values in places where '%s' does not",
                      "exposure", "y"))
    exposure
}

## HAS_TESTS
checkAndTidyListArgForEstimateFun <- function(arg, name = c("data", "aggregate", "lower", "upper"),
                                              isCounts = TRUE) {
    name <- match.arg(name)
    if (isCounts)
        names.expected <- c("model", "dataModels")
    else
        names.expected <- c("systemModels", "dataModels")
    if (!is.list(arg))
        stop(gettextf("'%s' has class \"%s\"",
                      name))
    if (identical(length(arg), 0L))
        return(NULL)
    names <- names(arg)
    if (is.null(names))
        stop(gettextf("'%s' does not have names",
                      name))
    if (any(is.na(names)))
        stop(gettextf("names for '%s' have missing values",
                      name))
    if (any(!nzchar(names)))
        stop(gettextf("names for '%s' have blanks",
                      name))
    if (any(duplicated(names)))
        stop(gettextf("names for '%s' have duplicates",
                      name))
    is.valid.name <- names %in% names.expected
    if (any(!is.valid.name))
        stop(gettextf("invalid name for '%s' : \"%s\"",
                      name, names[!is.valid.name][1L]))
    if (!isCounts) {
        systemModels <- arg[["systemModels"]]
        if (!is.list(systemModels))
            stop(gettextf("element \"%s\" of '%s' does not have class \"%s\"",
                          "systemModels", name, "list"))
    }
    dataModels <- arg[["dataModels"]]
    if (!is.list(dataModels))
        stop(gettextf("element \"%s\" of '%s' does not have class \"%s\"",
                      "dataModels", name, "list"))
    if (name == "aggregate")
        if (!all(sapply(unlist(arg), methods::is, "SpecAggregate")))
            stop(gettextf("'%s' contains elements not of class \"%s\"",
                          name, "SpecAggregate"))
    if (name %in% c("lower", "upper"))
        if (!all(sapply(unlist(arg), is.numeric)))
            stop(gettextf("'%s' contains elements not of type \"%s\"",
                          name, "numeric"))
    arg
}


## HAS_TESTS
checkAndTidySDAg <- function(sd, value, metadata) {
    length.sd <- length(sd)
    length.value <- length(value)
    if (methods::is(sd, "DemographicArray")) {
        ## 'sd' and 'value' have same length
        if (!identical(length.sd, length.value))
            stop(gettextf("'%s' and '%s' have different lengths",
                          "sd", "value"))
        ## 'sd' has same metadata as 'value'
        metadata.sd <- sd@metadata
        if (!isTRUE(all.equal(metadata.sd, metadata)))
            stop(gettextf("'%s' and '%s' have different metadata",
                          "sd", "value"))
        ## 'sd' has no missing values"
        if (any(is.na(sd)))
            stop(gettextf("'%s' has missing values",
                          "sd"))
        ## 'sd' has no negative values
        if (any(sd < 0))
            stop(gettextf("'%s' has negative values",
                          "sd"))
        sd <- as.double(sd@.Data)
    }
    else if (methods::is(sd, "numeric")) {
        ## 'sd' has length 1
        if (!identical(length.sd, 1L))
            stop(gettextf("'%s' is numeric but does not have length %d",
                          "sd", 1L))
        ## 'sd' is not missing
        if (is.na(sd))
            stop(gettextf("'%s' is missing",
                          "sd"))
        ## 'sd' is non-negative
        if (sd < 0)
            stop(gettextf("'%s' is negative",
                          "sd"))
        sd <- as.double(sd)
        sd <- rep(sd, times = length.value)
    }
    else {
        stop(gettextf("'%s' has class \"%s\"",
                      "sd", class(sd)))
    }
    methods::new("ScaleVec", sd)
}

## HAS_TESTS
checkAndTidyWeights <- function(weights, y) {
    if (is.null(weights))
        return(NULL)
    if (!methods::is(weights, "Counts"))
        stop(gettextf("'%s' has class \"%s\"",
                      "weights", class(weights)))
    weights <- dembase::makeCompatible(x = weights, y = y, subset = TRUE)
    weights <- dembase::toDouble(weights)
    if (any(weights[!is.na(weights)] < 0))
        stop(gettextf("'%s' has negative values",
                      "weights"))
    if (any(is.na(weights) > is.na(y)))
        stop(gettextf("'%s' has missing values in places where '%s' does not",
                      "weights", "y"))
    weights
}

## HAS_TESTS
## Used with Normal, so does not check for Counts, negatives, or non-integer
checkAndTidyY <- function(y, impute = FALSE) {
    ## 'y' is DemographicArray
    if (!methods::is(y, "DemographicArray"))
        stop(gettextf("'%s' has class \"%s\"",
                      "y", class(y)))
    ## 'y' has no zero-length dimensions
    is.zero <- dim(y) == 0L
    if (any(is.zero))
        stop(gettextf("dimension \"%s\" of '%s' has length %d",
                      names(y)[is.zero][1L], "y", dim(y)[is.zero][1L]))
    ## 'y' does not have iteration dimension
    is.iter <- dembase::dimtypes(y) == "iteration"
    if (any(is.iter))
        stop(gettextf("dimension \"%s\" of '%s' has dimtype \"%s\"",
                      names(y)[is.iter], "y", "iteration"))
    ## 'y' does not have quantile dimension
    is.quantile <- dembase::dimtypes(y) == "quantile"
    if (any(is.quantile))
        stop(gettextf("dimension \"%s\" of '%s' has dimtype \"%s\"",
                      names(y)[is.quantile], "y", "quantile"))
    ## 'y' has at least 2 non-missing values
    if (sum(!is.na(y)) < 2L)
        stop(gettextf("'%s' has fewer than %d non-missing values",
                      "y", 2L))
    ## return 'y'
    y
}

## HAS_TESTS
checkAxAg <- function(ax, value) {
    if (is.null(ax))
        return(ax)
    if (length(ax) == 0L)
        stop(gettextf("'%s' has length %d",
                      "ax", 0L))
    if (!methods::is(ax, "Values"))
        stop(gettextf("'%s' has class \"%s\"",
                      "ax", class(ax)))
    names <- names(ax)
    dim <- dim(ax)
    dimtypes <- dimtypes(ax, use.names = FALSE)
    DimScales <- DimScales(ax, use.names = FALSE)
    i.age <- match("age", dimtypes, nomatch = 0L)
    has.age <- i.age > 0L
    if (!has.age)
        stop(gettextf("'%s' does not have a dimension with %s \"%s\"",
                      "ax", "dimtype", "age"))
    DimScale.age <- DimScales[[i.age]]
    if (!methods::is(DimScale.age, "Intervals"))
        stop(gettextf("dimension of '%s' with %s \"%s\" does not have %s \"%s\"",
                      "ax", "dimtype", "age", "dimscale", "Intervals"))
    n.dim <- length(dim)
    if (n.dim > 1L) {
        if (!methods::is(value, "DemographicArray"))
            stop(gettextf("'%s' is not a demographic array, but '%s' has more than one dimension",
                          "value", "ax"))
        metadata.ax.no.age <- methods::new("MetaData",
                                  nms = names[-i.age],
                                  dimtypes = dimtypes[-i.age],
                                  DimScales = DimScales[-i.age])
        .Data.ax.no.age <- array(0,
                                 dim = dim(metadata.ax.no.age),
                                 dimnames = dimnames(metadata.ax.no.age))
        ax.no.age <- methods::new("Values",
                                  .Data = .Data.ax.no.age,
                                  metadata = metadata.ax.no.age)
        return.value <- tryCatch(dembase::canMakeCompatible(x = ax.no.age,
                                                            y = value,
                                                            subset = TRUE),
                                 error = function(e) e)
        if (methods::is(return.value, "error"))
            stop(gettextf("'%s' and '%s' not compatible : %s",
                          "ax", "value", return.value$message))
    }
    NULL
}

## HAS_TESTS
checkConcordances <- function(concordances) {
    ## 'concordances' is a list
    if (!is.list(concordances))
        stop(gettextf("'%s' has class \"%s\"",
                      "concordances", class(concordances)))
    if (identical(length(concordances), 0L))
        return(NULL)
    names <- names(concordances)
    ## all elements of 'concordances' have class "ManyToOne"
    if (!all(sapply(concordances, methods::is, "ManyToOne")))
        return(gettextf("'%s' has elements not of class \"%s\"",
                        "concordances", "ManyToOne"))
    ## 'concordances' has names
    if (is.null(names))
        stop(gettextf("'%s' does not have names",
                      "concordances"))
    ## no duplicated names for 'concordances'
    if (any(duplicated(names)))
        stop(gettextf("'%s' has duplicate names",
                      "concordances"))
    NULL
}


## HAS_TESTS
## Assume that 'datasets' has valid names
checkConcordancesDatasets <- function(concordances, datasets, namesDatasets) {
    ## 'concordances' is a list
    if (!is.list(concordances))
        stop(gettextf("'%s' has class \"%s\"",
                      "concordances", class(concordances)))
    if (identical(length(concordances), 0L))
        return(NULL)
    names.conc <- names(concordances)
    ## 'concordances' has names
    if (is.null(names.conc))
        stop(gettextf("'%s' does not have names",
                      "concordances"))
    ## no duplicated names for 'concordances'
    if (any(duplicated(names.conc)))
        stop(gettextf("'%s' has duplicate names",
                      "concordances"))
    ## picks out a dataset
    for (name in names.conc) {
        if (!(name %in% namesDatasets))
            stop(gettextf("'%s' has an element called \"%s\", but \"%s\" is not the name of a dataset",
                          "concordances", name, name))
    }
    ## every element of 'concordances' is a list
    for (name in names.conc) {
        if (!is.list(concordances[[name]]))
            stop(gettextf("element \"%s\" of '%s' is not a list",
                          name, "concordances"))
    }
    ## each element of 'concordances' is a valid concordance list
    for (name in names.conc) {
        element <- concordances[[name]]
        ## all elements have class "ManyToOne"
        if (!all(sapply(element, methods::is, "ManyToOne")))
            stop(gettextf("element \"%s\" of '%s' has elements not of class \"%s\"",
                            name, "concordances", "ManyToOne"))
        ## 'concordances' has names
        if (is.null(names(element)))
            stop(gettextf("element \"%s\" of '%s' does not have names",
                          name, "concordances"))
        ## no duplicated names for 'concordances'
        if (any(duplicated(names(element))))
            stop(gettextf("element \"%s\" of '%s' has duplicate names",
                          name, "concordances"))
    }
    NULL
}

## HAS_TESTS
checkDataModels <- function(dataModels, needsNonDefaultSeriesArg = FALSE) {
    ## 'dataModels' is a list
    if (!is.list(dataModels))
        stop(gettextf("'%s' has class \"%s\"",
                      "dataModels", class(dataModels)))
    for (i in seq_along(dataModels)) {
        obs <- dataModels[[i]]
        ## all elements have class "SpecModel"
        if (!methods::is(obs, "SpecModel"))
            stop(gettextf("element %d of '%s' has class \"%s\"",
                          i, "dataModels", class(obs)))
        ## element uses exposure
        if (!obs@useExpose@.Data)
            stop(gettextf("model %d of '%s' does not use exposure",
                          i, "dataModels"))
        ## element has name
        if (is.na(obs@nameY@.Data) || !nzchar(obs@nameY@.Data))
            stop(gettextf("element %d of '%s' has no name for response variable",
                          i, "dataModels"))
        ## specification of model is valid
        return.value <- tryCatch(methods::validObject(obs),
                                 error = function(e) e)
        if (methods::is(return.value, "error"))
            stop(gettextf("error in data model for '%s' : %s",
                          obs@nameY@.Data, return.value$message))
        if (needsNonDefaultSeriesArg) {
            ## 'series' argument supplied if needed
            if (identical(obs@series@.Data, "y"))
                stop(gettextf("'%s' argument not supplied in data model for '%s'",
                              "series", obs@nameY))
        }
        else {
            ## no 'series' argument supplied if not needed
            if (!identical(obs@series@.Data, "y"))
                warning(gettextf("non-default argument for '%s' in data model for '%s' ignored",
                                 "series", obs@nameY))
        }
    }
    NULL
}

## HAS_TESTS
checkForMarginalTerms <- function(formula) {
    terms <- stats::terms(formula)
    intercept <- attr(terms, "intercept")
    if (!identical(intercept, 1L))
        stop(gettextf("formula '%s' does not include an intercept",
                      deparse(formula)))
    factor <- attr(terms, "factor")
    if (length(factor) > 0L) {
        col.has.2 <- apply(factor, 2, function(x) any(x == 2L)) ## at most one TRUE
        if (any(col.has.2)) {
            term.without.marginal <- colnames(factor)[col.has.2]
            is.1 <- factor[ , col.has.2] == 1L
            missing.marginal <- rownames(factor)[is.1]
            missing.marginal <- paste(missing.marginal, collapse = ":")
            stop(gettextf("term '%s' is marginal to term '%s' but is not included in formula '%s'",
                          missing.marginal, term.without.marginal, deparse(formula)))
        }
    }
    else
        NULL
}

## HAS_TESTS
checkFormulaMu <- function(formula) {
    correct.length <- identical(length(formula), 3L)
    if (!correct.length)
        stop(gettextf("'%s' is not a valid formula",
                      deparse(formula)))
    if (!identical(deparse(formula[[2L]]), "mean"))
        stop(gettextf("formula '%s' does not have response '%s'",
                      deparse(formula), "mean"))
    NULL
}

## HAS_TESTS
checkFilename <- function(filename, name = "filename") {
    ## filename is character
    if (!is.character(filename))
        stop(gettextf("'%s' does not have type \"%s\"",
                      name, "character"))
    ## 'filename' has length 1
    if (!identical(length(filename), 1L))
        stop(gettextf("'%s' does not have length %d",
                      name, 1L))
    ## 'filename' is not missing
    if (is.na(filename))
        stop(gettextf("'%s' is missing",
                      name))
    NULL
}

## HAS_TESTS
checkFunAg <- function(FUN) {
    if (!is.function(FUN))
        stop(gettextf("'%s' has class \"%s\"",
                      "FUN", class(FUN)))
    formals.obtained <- formals(FUN)
    formals.expected <- formals(function(x, weights) NULL)
    if (!identical(formals.obtained, formals.expected))
        stop(gettextf("'%s' does not have formal arguments '%s' and '%s'",
                      "FUN", "x", "weights"))
    NULL
}

## HAS_TESTS
checkLengthDimInFormula <- function(y, formula, minLength = 2L) {
    terms.without.response <- stats::terms(formula[-2L])
    names.terms <- rownames(attr(terms.without.response, "factors"))
    lengths <- dim(y)[match(names.terms, names(y))]
    too.short <- lengths < minLength
    if (any(too.short))
        stop(gettextf("dimension \"%s\" is used in formula '%s' but has length %d",
                      names.terms[too.short][1L],
                      deparse(formula),
                      dim(y)[too.short][1L]))
    NULL
}

## HAS_TESTS
checkListNames <- function(names, listName) {
    if (is.null(names))
        stop(gettextf("'%s' does not have names",
                      listName))
    if (any(is.na(names)))
        stop(gettextf("names for '%s' has missing values",
                      listName))
    if (!all(nzchar(names)))
        stop(gettextf("names for '%s' has blanks",
                      listName))
    if (any(duplicated(names)))
        stop(gettextf("names for '%s' has duplicates",
                      listName))
    NULL
}


## HAS_TESTS
checkSpecWeightAg <- function(weights, metadata) {
    if (is.null(weights))
        NULL
    else {
        if (!methods::is(weights, "Counts"))
            stop(gettextf("'%s' has class \"%s\"",
                          "weights", class(weights)))
        ## if (any(weights < 0, na.rm = TRUE))
        ##     stop(gettextf("'%s' has negative values",
        ##                   "weights"))
        if (!is.null(metadata)) {
            .Data <- array(1L,
                           dim = dim(metadata),
                           dimnames = dimnames(metadata))
            y <- methods::new("Values", .Data = .Data, metadata = metadata)
            return.value <- tryCatch(dembase::canMakeCompatible(x = weights,
                                                                y = y,
                                                                subset = TRUE),
                                     error = function(e) e)
            if (methods::is(return.value, "error"))
                stop(gettextf("'%s' and '%s' not compatible : %s",
                              "weights", "value", return.value$message))
        }
    }
    NULL
}

## HAS_TESTS
checkTermsFromFormulaFound <- function(y, formula) {
    terms.without.response <- stats::terms(formula[-2L])
    names.terms <- rownames(attr(terms.without.response, "factors"))
    not.in.y <- !(names.terms %in% names(y))
    n.not.in.y <- sum(not.in.y)
    if (n.not.in.y > 0L)
        stop(sprintf(ngettext(n.not.in.y,
                              "dimension %s from formula '%s' not found",
                              "dimensions %s from formula '%s' not found"),
                     paste(dQuote(names.terms[not.in.y]), collapse = ", "),
                     deparse(formula)))
    NULL
}

## HAS_TESTS
## Function 'loglm' arranges and names 'beta' according to order of
## dimensions used in data.  Normally functions using a formula
## arrange and name terms according to the order they appear in the formula.
## 'convertToFormulaOrder' converts from 'loglm' format to standard format.
convertToFormulaOrder <- function(betas, formulaMu) {
    n.beta <- length(betas)
    terms <- stats::terms(formulaMu)
    factors <- attr(terms, "factors")
    dims.in.formula <- rownames(factors)
    ## when a term is an array, permute the array and change the name
    ## so that dimensions occur in the same order that they do in 'formula'
    for (i in seq_along(betas)) {
        beta <- betas[[i]]
        if (is.array(beta)) {
            names.dims <- names(dimnames(beta))
            perm <- order(match(names.dims, dims.in.formula))
            beta <- aperm(beta, perm = perm)
            betas[[i]] <- beta
            names(betas)[i] <- paste(names(dimnames(beta)), collapse = ":")
        }
    }
    ## reorder terms so that they appear in right order
    term.labels <- attr(terms, "term.labels")
    term.labels <- c("(Intercept)", term.labels)
    betas <- betas[match(term.labels, names(betas))]
    betas
}




## need to have Cross default for orig-dest or parent-child
## HAS_TESTS
defaultPrior <- function(beta, metadata) {
    is.intercept <- is.null(metadata)
    if (is.intercept)
        return(ExchFixed())
    dim <- dim(metadata)
    names <- names(metadata)
    dimtypes <- dembase::dimtypes(metadata, use.names = FALSE)
    DimScales <- dembase::DimScales(metadata, use.names = FALSE)
    n.beta <- length(beta)
    name.term <- paste(names, collapse = ":")
    if (n.beta < 2L)
        stop(gettextf("'%s' for \"%s\" has length %d",
                      "beta", name.term, n.beta))
    if (n.beta != prod(dim))
        stop(gettextf("length of '%s' for \"%s\" [%d] not equal to product of dimensions [%d]",
                      "beta", name.term, n.beta, prod(dim)))
    is.main.effect <- identical(length(dim), 1L)
    i.time <- match("time", dimtypes, nomatch = 0L)
    i.age <- match("age", dimtypes, nomatch = 0L)
    has.time <- i.time > 0L
    has.age <- i.age > 0L
    n.beta <- length(beta)
    if (n.beta <= 2L) {
        ExchFixed()
    }
    else {
        if (is.main.effect) {
            if (has.time) {
                DLM()
            }
            else
                Exch()
        }
        else {
            if (has.time) {
                along <- names[i.time]
                DLM(along = along, trend = NULL)
            }
            else
                Exch()
        }
    }
}


## HAS_TESTS
formulaIsInterceptOnly <- function(formula) {
    terms <- stats::terms(formula)
    identical(attr(terms, "factors"), integer(0)) &&
        identical(attr(terms, "intercept"), 1L)
}

## HAS_TESTS
imputeCountsInternal <- function(object, max = NULL) {
    has.subtotals <- methods::is(object, "HasSubtotals")
    if (has.subtotals) {
        subtotals <- object@subtotals
        metadata.subtotals <- object@metadataSubtotals
        transform.subtotals <- object@transformSubtotals
    }
    if (any(is.na(object))) {
        object <- impute(object, max = max)
        if (has.subtotals)
            object <- methods::new("CountsWithSubtotalsInternal",
                                   .Data = object@.Data,
                                   metadata = object@metadata,
                                   subtotals = subtotals,
                                   metadataSubtotals = metadata.subtotals,
                                   transformSubtotals = transform.subtotals)
    }
    object
}

## HAS_TESTS
initialDataModels <- function(dataModels, datasets, y, transforms) {
    ans <- vector(mode = "list", length = length(dataModels))
    for (i in seq_along(ans)) {
        ans[[i]] <- initialModel(object = dataModels[[i]],
                                 y = datasets[[i]],
                                 exposure = dembase::collapse(y, transform = transforms[[i]]))
    }
    ans
}

## HAS_TESTS
jitterBetas <- function(betas, priorsBetas) {
    kIntercept <- 0.1
    kTerms <- 0.25
    betas[[1L]] <- stats::rnorm(n = 1L,
                                mean = betas[[1L]],
                                sd = kIntercept * abs(betas[[1L]]))
    if (length(betas) > 1L) {
        for (i in seq_along(betas)[-1L]) {
            val <- stats::rnorm(n = length(betas[[i]]),
                                mean = betas[[i]],
                                sd = kTerms * stats::sd(betas[[i]]))
            prior <- priorsBetas[[i]]
            all.struc.zero <- prior@allStrucZero
            val[all.struc.zero] <- NA
            betas[[i]] <- val
        }
    }
    betas
}

## HAS_TESTS
makeCellInLikHelper <- function(transform, y, strucZeroArray) {
    ans <- logical(length = length(y))
    for (i in seq_along(y)) {
        is.missing <- is.na(y[i])
        if (is.missing) {
            i.after <- dembase::getIAfter(i = i,
                                          transform = transform,
                                          check = FALSE,
                                          useC = TRUE)
            contributes <- i.after > 0L
            ans[i] <- contributes
        }
        else
            ans[i] <- TRUE
    }
    if (!is.null(strucZeroArray)) {
        is.zero <- strucZeroArray == 0L
        ans[is.zero] <- FALSE
    }
    ans
}

## HAS_TESTS
makeComponentWeightMix <- function(dimBeta, iAlong, indexClassMaxMix,
                                   levelComponent, omegaComponent) {
    n.along <- dimBeta[iAlong]
    indexClassMaxMix <- indexClassMaxMix@.Data
    levelComponent <- levelComponent@.Data
    omegaComponent <- omegaComponent@.Data
    componentWeightMix <- stats::rnorm(n = n.along * indexClassMaxMix,
                                       mean = levelComponent,
                                       sd = omegaComponent)
    methods::new("ParameterVector", componentWeightMix)
}

## HAS_TESTS
makeCountsPred <- function(modelPred) {
    metadata <- modelPred@metadataY
    .Data <- array(NA_integer_,
                   dim = dim(metadata),
                   dimnames = dimnames(metadata))
    has.struc.zero <- methods::is(modelPred, "StrucZeroArrayMixin")
    if (has.struc.zero) {
        struc.zero.array <- modelPred@strucZeroArray@.Data
        .Data[struc.zero.array == 0L] <- 0L
    }
    methods::new("Counts",
                 .Data = .Data,
                 metadata = metadata)
}

## HAS_TESTS
makeDims <- function(dim, margins) {
    ans <- list(0L)
    if (length(margins) > 0L) {
        extra <- lapply(margins[-1L], function(x) dim[x])
        ans <- c(ans, extra)
    }
    ans
}

## HAS_TESTS
makeIAlong <- function(along, metadata) {
    kContinuousDimtypes <- c("age", "cohort", "time")
    continuous.labels <- paste(dQuote(kContinuousDimtypes), collapse = ", ")
    names <- names(metadata)
    dimtypes <- dembase::dimtypes(metadata, use.names = FALSE)
    DimScales <- dembase::DimScales(metadata, use.names = FALSE)
    is.continuous <- dimtypes %in% kContinuousDimtypes
    if (is.null(along)) {
        n.continuous <- sum(is.continuous)
        if (n.continuous == 0L)
            stop(gettextf("cannot use random walk prior when no dimensions have dimtype %s",
                          continuous.labels))
        if (n.continuous >= 2L)
            stop(gettextf("more than one dimension with dimtype %s, but '%s' not specified",
                          continuous.labels, "along"))
        along <- which(is.continuous)
    }
    else {
        along <- match(along, names, nomatch = 0L)
        if (along == 0L)
            stop(gettextf("'%s' outside valid range", "along"))
        dimtype.along <- dimtypes[along]
        if (!(dimtype.along %in% kContinuousDimtypes))
            stop(gettextf("cannot have random walk along dimension \"%s\" because dimension has dimtype \"%s\"",
                          names[along], dimtypes[along]))
    }
    DimScale.along <- DimScales[[along]]
    steps <- dembase::stepLengths(DimScale.along)
    steps <- steps[is.finite(steps)]
    if (length(steps) > 1L) {
        all.equal <- all(mapply(identical, steps[1L], steps[-1L]))
        if (!all.equal)
            stop(gettextf("cannot have random walk along dimension \"%s\" because steps irregular",
                          names[along]))
    }
    along
}

## HAS_TESTS
makeIteratorProdVectorMix <- function(dimBeta, iAlong) {
    dim <- dimBeta
    dim[iAlong] <- 1L
    MarginIterator(dim = dim)
}

## HAS_TESTS
makeLatentWeightMix <- function(dimBeta, iAlong, iteratorsDimsMix,
                                indexClassMix, indexClassMaxMix,
                                weightMix) {
    n.along <- dimBeta[iAlong]
    iterator.beta <- iteratorsDimsMix[[iAlong]]
    indexClassMaxMix <- indexClassMaxMix@.Data
    weightMix <- weightMix@.Data
    weightMix <- matrix(weightMix,
                        nrow = n.along,
                        ncol = indexClassMaxMix)
    J <- prod(dimBeta)
    ans <- numeric(length = J)
    iterator.beta <- resetS(iterator.beta,
                            useC = TRUE)
    for (i.along in seq_len(n.along)) {
        indices.beta <- iterator.beta@indices
        for (i.beta in indices.beta) {
            i.class <- indexClassMix[i.beta]
            v <- weightMix[i.along, i.class]
            ans[i.beta] <- stats::runif(n = 1L,
                                        min = 0,
                                        max = v)
        }
        iterator.beta <- advanceS(iterator.beta,
                                  useC = TRUE)
    }
    methods::new("UnitIntervalVec", ans)
}

## HAS_TESTS
makeLevelComponentWeightMix <- function(dimBeta, iAlong, indexClassMaxMix,
                                        phiMix, meanLevel, omegaLevel) {
    n.along <- dimBeta[iAlong]
    indexClassMaxMix <- indexClassMaxMix@.Data
    meanLevel <- meanLevel@.Data
    omegaLevel <- omegaLevel@.Data
    ans <- matrix(nrow = n.along,
                  ncol = indexClassMaxMix)
    mean.initial <- meanLevel / (1 - phiMix)
    sd.initial <- omegaLevel / sqrt(1 - phiMix^2)
    sd.rest <- omegaLevel
    ans[1L, ] <- stats::rnorm(n = indexClassMaxMix,
                              mean = mean.initial,
                              sd = sd.initial)
    for (i in seq.int(from = 2L, to = n.along)) {
        mean.i <- meanLevel + phiMix * ans[i - 1L, ]
        ans[i, ] <- stats::rnorm(n = indexClassMaxMix,
                                 mean = mean.i,
                                 sd = sd.rest)
    }
    ans <- as.double(ans)
    ans <- methods::new("ParameterVector", ans)
}

## HAS_TESTS
makeLinearBetas <- function(theta, formula) {
    checkForMarginalTerms(formula)
    data <- data.frame(expand.grid(dimnames(theta)), mean = as.numeric(theta))
    names(data)[length(data)] <- extractResponse(formula)
    mod <- stats::lm(formula, data = data)
    mod <- stats::aov(mod)
    if (mod$rank > 1L) {
        ans <- stats::model.tables(mod)$tables
        one.dim <- sapply(ans, function(x) identical(length(dim(x)), 1L))
        ans[one.dim] <- lapply(ans[one.dim], as.numeric)
        ans[!one.dim] <- lapply(ans[!one.dim], as.array)
    }
    else
        ans <- list()
    ans <- c(list("(Intercept)" = mean(theta)), ans)
    ans
}

## HAS_TESTS
makeMargins <- function(betas, y) {
    names.betas <- names(betas)
    n.beta <- length(betas)
    dimnames.y <- dimnames(y)
    names.y <- names(dimnames.y)
    ans <- vector(mode = "list", length = n.beta)
    ans[[1L]] <- 0L
    if (n.beta > 1L) {
        for (i in seq.int(from = 2L, to = n.beta)) {
            names.dims <- names(dimnames(betas[[i]]))
            if (is.null(names.dims))
                ans[[i]] <- match(names.betas[i], names.y)
            else
                ans[[i]] <- match(names.dims, names.y)
        }
    }
    ans
}

## HAS_TESTS
makeMeanLevelComponentWeightMix <- function(priorMean, priorSD) {
    mean <- priorMean@.Data
    sd <- priorSD@.Data
    ans <- stats::rnorm(n = 1L,
                        mean = mean,
                        sd = sd)
    methods::new("Parameter", ans)
}

## HAS_TESTS
makeNamesEta <- function(spec) {
    formula <- spec@innerFormula
    terms <- stats::terms(formula)
    ans <- attr(terms, "term.labels")
    has.intercept <- identical(attr(terms, "intercept"), 1L)
    if (has.intercept)
        ans <- c("(Intercept)", ans)
    ans
}

## HAS_TESTS
makeNamesSpecsPriors <- function(dots) {
    if (length(dots) > 0L)
        sapply(dots, extractResponse)
    else
        character()
}

## HAS_TESTS
## priors follow order implied by formula - not order implied by metadatax
makePriors <- function(betas, specs, namesSpecs, margins, y, sY, strucZeroArray) {
    n.beta <- length(betas)
    ans <- vector(mode = "list", length = n.beta)
    names.betas <- names(betas)
    dim.y <- dim(y)
    s <- seq_along(dim.y)
    metadata.y <- y@metadata
    for (i in seq_len(n.beta)) {
        beta <- betas[[i]]
        beta <- as.double(beta)
        name <- names.betas[i]
        if (any(is.na(beta)))
            stop(gettextf("'%s' for \"%s\" has missing values",
                          "beta", name))
        margin <- margins[[i]]
        if (identical(margin, 0L))
            metadata <- NULL
        else
            metadata <- metadata.y[margin]
        is.saturated <- identical(sort(margin), s)
        i.spec <- match(name, namesSpecs, nomatch = 0L)
        has.spec <- i.spec > 0L
        if (has.spec)
            spec <- specs[[i.spec]]
        else
            spec <- defaultPrior(beta = beta,
                                 metadata = metadata)
        ans[[i]] <- initialPrior(object = spec,
                                 beta = beta,
                                 metadata = metadata,
                                 sY = sY,
                                 isSaturated = is.saturated,
                                 margin = margin,
                                 strucZeroArray = strucZeroArray)
    }
    ans
}

## HAS_TESTS
makeProdVectorsMix <- function(vectorsMix, iAlong, dimBeta, indexClassMaxMix) {
    index.class.max <- indexClassMaxMix@.Data
    vectors <- vectorsMix[-iAlong]
    dim <- dimBeta[-iAlong]
    vectors <- lapply(vectors, methods::slot, ".Data")
    for (i.vector in seq_along(vectors))
        vectors[[i.vector]] <- matrix(vectors[[i.vector]],
                                      nrow = dim[i.vector],
                                      ncol = index.class.max)
    ans <- vector(mode = "list",
                  length = index.class.max)
    for (i.class in seq_len(index.class.max)) {
        vectors.i.class <- lapply(vectors, function(x) x[ , i.class])
        ans[[i.class]] <- Reduce(`%o%`, vectors.i.class)
    }
    ans <- unlist(ans)
    methods::new("ParameterVector", ans)
}

## HAS_TESTS
makeSigmaInitialPoisson <- function(y, exposure = NULL) {
    kMinimumValue <- 0.01
    y <- as.numeric(y)
    is.missing <- is.na(y)
    if (any(is.missing)) {
        ## 'y' has at least 1 non-missing value
        if (all(is.missing))
            stop(gettextf("'%s' has no non-missing values",
                          "y"))
        y <- y[!is.missing]
        exposure <- exposure[!is.missing]
        Recall(y = y, exposure = exposure)
    }
    ## 'y' is finite
    if (any(is.infinite(y)))
        stop(gettextf("'%s' has non-finite values",
                      "y"))
    ## 'y' not all 0
    if (all(y == 0))
        stop(gettextf("'%s' has no non-zero values",
                      "y"))
    if (is.null(exposure))
        theta <- y
    else {
        exposure <- as.numeric(exposure)
        ## 'exposure' has no missing values
        if (any(is.na(exposure)))
            stop(gettextf("'%s' has missing values",
                          "exposure"))
        ## 'exposure' is finite
        if (any(is.infinite(exposure)))
            stop(gettextf("'%s' has non-finite values",
                          "exposure"))
        ## 'exposure' not all 0
        if (all(exposure == 0))
            stop(gettextf("'%s' has no non-zero values",
                          "exposure"))
        non.zero <- exposure > 0
        theta <- y[non.zero] / exposure[non.zero]
    }
    if (length(theta) > 1L) {
        theta <- 0.5 * (theta + mean(theta))
        log.theta <- log(theta)
        ans <- stats::sd(log.theta)
        ans <- max(ans, kMinimumValue)
        ans
    }
    else
        1.0
}

## NO_TESTS
makeSpecsPriors <- function(dots) {
    ans <- vector(mode = "list", length = length(dots))
    for (i in seq_along(dots)) {
        formula <- dots[[i]]
        if (!methods::is(formula, "formula"))
            stop(gettextf("'%s' is not a formula",
                          deparse(substitute(formula))))
        if (!hasResponse(formula))
            stop(gettextf("formula '%s' does not include a response",
                          deparse(formula)))
        right.hand.side <- formula[[3L]]
        ans[[i]] <- eval(right.hand.side,
                         envir = environment(formula))
    }
    ans
}


makeThetaPoissonCMP <- function(y, exposure, lower, upper, strucZeroArray) {
    y.missing <- is.na(y@.Data)
    is.obs <- !is.na(y@.Data) & (struc.zero.array != 0L) 
    if (any(is.obs))
        mean.y.obs <- mean(y@.Data[is.obs]) 
    else
        mean.y.obs <- 0.5
    shape <- ifelse(is.obs, 0.05 * mean.y.obs + 0.95 * y@.Data, mean.y.obs)
    has.exposure <- !is.null(exposure)
    if (has.exposure) {
        mean.expose.obs <- mean(exposure[is.obs])
        rate <- ifelse(is.obs, 0.05 * mean.expose.obs + 0.95 * exposure, mean.expose.obs)
    }
    else
        rate <- 1
    ans <- stats::rgamma(n = length(y), shape = shape, rate = rate)
    is.too.low <- theta < lower
    n.too.low <- sum(is.too.low)
    width <- 0.05 * (upper - lower)
    if (is.infinite(width))
        width <- 100
    theta[is.too.low] <- stats::runif(n = n.too.low,
                                      min = lower,
                                      max = lower + width)
    is.too.high <- theta > upper
    n.too.high <- sum(is.too.high)
    theta[is.too.high] <- stats::runif(n = n.too.high,
                                       min = upper - width,
                                       max = upper)
    array(theta, dim = dim(y), dimnames = dimnames(y))
}







## HAS_TESTS
makeTransformsYToDatasets <- function(y, datasets, concordances,
                                      namesDatasets) {
    names.concordances <- names(concordances)
    ans <- vector(mode = "list", length = length(datasets))
    for (i in seq_along(ans)) {
        dataset <- datasets[[i]]
        name.dataset <- namesDatasets[i]
        i.concordances <- match(name.dataset, names.concordances, nomatch = 0L)
        has.concordances <- i.concordances > 0L
        if (has.concordances)
            concordances.dataset <- concordances[[i.concordances]]
        else
            concordances.dataset <- list()
        transform <- tryCatch(dembase::makeTransform(x = y,
                                                     y = dataset,
                                                     concordances = concordances.dataset,
                                                     subset = TRUE,
                                                     check = TRUE),
                              error = function(e) e)
        if (methods::is(transform, "error"))
            stop(gettextf("unable to collapse '%s' to make it compatible with dataset '%s' : %s",
                          "y", namesDatasets[i], transform$message))
        transform <- dembase::makeCollapseTransformExtra(transform)
        ans[[i]] <- transform
    }
    ans
}

## HAS_TESTS
makeValueAndMetaDataAg <- function(value) {
    if (methods::is(value, "DemographicArray")) {
        if (any(is.na(value)))
            stop(gettextf("'%s' has missing values",
                          "value"))
        metadata <- value@metadata
        value <- as.double(value@.Data)
    }
    else if (is.numeric(value)) {
        if (!identical(length(value), 1L))
            stop(gettextf("'%s' does not have length %d",
                          "value", 1L))
        if (is.na(value))
            stop(gettextf("'%s' is missing",
                          "value"))
        metadata <- NULL
        value <- as.double(value)
    }
    else
        stop(gettextf("'%s' has class \"%s\"",
                      "value", class(value)))
    value <- methods::new("ParameterVector", value)
    list(value = value, metadata = metadata)
}

## HAS_TESTS
makeVectorsMix <- function(dimBeta, iAlong, indexClassMaxMix,
                           omegaVectorsMix) {
    n.dim <- length(dimBeta)
    sd <- omegaVectorsMix@.Data
    length <- dimBeta * indexClassMaxMix
    length[iAlong] <- 0L
    ans <- vector(mode = "list",
                  length = n.dim)
    for (i in seq_len(n.dim)) {
        .Data <- stats::rnorm(n = length[i],
                              sd = sd)
        ans[[i]] <- methods::new("ParameterVector", .Data)
    }
    ans
}

## HAS_TESTS
makeWeightAg <- function(weight, default, model, thetaObj, transform, values) {
    if (is.null(weight)) {
        if (is.null(default))
            stop(gettext("no aggregate weights supplied, and no default weights"))
        else
            ans <- default
    }
    else {
        ans <- tryCatch(dembase::makeCompatible(x = weight,
                                                y = thetaObj,
                                                subset = TRUE),
                        error = function(e) e)
        if (methods::is(ans, "error"))
            stop(gettextf("%s not compatible with '%s' : %s",
                          "aggregate weight", "y", ans$message))
    }
    ans <- as.double(ans)
    for (i in seq_along(ans)) {
        k <- dembase::getIAfter(i,
                                transform = transform,
                                useC = TRUE)
        if (k == 0L)
            ans[i] <- NA
        else {
            if (is.na(ans[i]))
                stop(gettextf("element %d of '%s' is needed for aggregate values but is missing",
                              i, "weightAg"))
        }
    }
    normalise <- is.null(weight) && !methods::is(model, "PoissonVaryingNotUseExp")
    if (normalise) {
        for (k in seq_along(values)) {
            i.wt <- dembase::getIBefore(k, transform = transform, useC = TRUE)
            wt <- ans[i.wt]
            sum.wt <- sum(wt)
            if (sum.wt > 0)
                wt <- wt / sum.wt
            ans[i.wt] <- wt
        }
    }
    ans
}

## HAS_TESTS
makeWeightMix <- function(dimBeta, iAlong, indexClassMaxMix,
                          componentWeightMix) {
    n.along <- dimBeta[iAlong]
    index.class.max <- indexClassMaxMix@.Data
    component.weight <- componentWeightMix@.Data
    ans <- stats::pnorm(component.weight)
    ans <- matrix(ans,
                  nrow = n.along,
                  ncol = index.class.max)
    mult <- 1 - ans
    mult <- apply(mult,
                  MARGIN = 1L,
                  FUN = cumprod)
    mult <- t(mult)
    ans[ , -1L] <- ans[ , -1L] * mult[ , -index.class.max]
    ans <- as.double(ans)
    methods::new("UnitIntervalVec", ans)
}
    

