
## HAS_TESTS
alignDataModelsToDatasets <- function(dataModels, datasets, namesDatasets) {
    names.obs.mod <- sapply(dataModels, methods::slot, "nameY")
    obs.not.in.data <- setdiff(names.obs.mod, namesDatasets)
    data.not.in.obs <- setdiff(namesDatasets, names.obs.mod)
    ## no data models without datasets
    if (length(obs.not.in.data) > 0L)
        stop(gettextf("'%s' contains a model for '%s', but there is no dataset called '%s' in '%s'",
                      "dataModels", obs.not.in.data[1L], obs.not.in.data[1L], "datasets"))
    ## no datasets without data models
    if (length(data.not.in.obs) > 0L)
        stop(gettextf("'%s' contains a dataset called '%s', but '%s' does not contain a model for '%s'",
                      "datasets", data.not.in.obs[1L], "dataModels", data.not.in.obs[1L]))
    ans <- vector(mode = "list", length = length(datasets))
    for (i in seq_along(datasets)) {
        name.dataset <- namesDatasets[i]
        i.obs <- match(name.dataset, names.obs.mod)
        ans[[i]] <- dataModels[[i.obs]]
    }
    ans
}

## HAS_TESTS
alignSystemModelsToAccount <- function(systemModels, account) {
    names.sys.mod <- sapply(systemModels, methods::slot, "nameY")
    names.components <- account@namesComponents
    names.series <- c("population", names.components)
    sys.not.in.series <- setdiff(names.sys.mod, names.series)
    series.not.in.sys <- setdiff(names.series, names.sys.mod)
    ## no system models without series
    if (length(sys.not.in.series) > 0L)
        stop(gettextf("'%s' contains a system model for '%s', but there is no series called '%s' in '%s'",
                      "systemModels", sys.not.in.series[1L], sys.not.in.series[1L], "account"))
    ## no series without system models
    if (length(series.not.in.sys) > 0L)
        stop(gettextf("'%s' does not contain a model for series '%s' in '%s'",
                      "systemModels", series.not.in.sys[1L], "account"))
    ans <- vector(mode = "list", length = length(names.series))
    for (i in seq_along(names.series)) {
        name.series <- names.series[i]
        i.sys <- match(name.series, names.sys.mod)
        ans[[i]] <- systemModels[[i.sys]]
    }
    ## system model for 'population' does not have exposure term
    popn.uses.exposure <- ans[[1L]]@useExpose@.Data
    if (popn.uses.exposure)
        stop(gettextf("system model for '%s' uses exposure",
                      "population"))
    ans
}

## HAS_TESTS
checkAndTidyProbSmallUpdate <- function(probSmallUpdate) {
    ## 'probSmallUpdate' has length 1
    if (!identical(length(probSmallUpdate), 1L))
        stop(gettextf("'%s' does not have length %d",
                      "probSmallUpdate", 1L))
    ## 'probSmallUpdate' is not missing
    if (is.na(probSmallUpdate))
        stop(gettextf("'%s' is missing",
                      "probSmallUpdate"))
    ## 'probSmallUpdate' is numeric
    if (!is.numeric(probSmallUpdate))
        stop(gettextf("'%s' is not numeric",
                      "probSmallUpdate"))
    ## 'probSmallUpdate' is less than or equal to 1
    if (probSmallUpdate > 1)
        stop(gettextf("'%s' is greater than %d",
                      "probSmallUpdate", 1L))
    ## 'probSmallUpdate' is greater than or equal to 0
    if (probSmallUpdate < 0)
        stop(gettextf("'%s' is less than %d",
                      "probSmallUpdate", 0L))
    ## return
    as.double(probSmallUpdate)
}

## HAS_TESTS
checkAndTidySystemWeights <- function(weights, systemModels) {
    if (identical(weights, list())) {
        ans <- vector(mode = "list", length = length(systemModels))
        return(ans)
    }
    names.weights <- names(weights)
    checkListNames(names = names.weights,
                   listName = "weights")
    names.sys.mod <- sapply(systemModels, methods::slot, "nameY")
    weights.not.in.sys <- setdiff(names.weights, names.sys.mod)
    ## no weights without system models (system models without weights are OK)
    if (length(weights.not.in.sys) > 0L)
        stop(gettextf("'%s' contains weights for '%s', but '%s' does not contain a model for '%s'",
                      "weights", weights.not.in.sys[1L], "systemModels", weights.not.in.sys[1L]))
    ans <- vector(mode = "list", length = length(systemModels))
    for (i in seq_along(ans)) {
        spec <- systemModels[[i]]
        name.y <- spec@nameY
        i.weights <- match(name.y, names.weights, nomatch = 0L)
        has.weights <- i.weights > 0L
        if (has.weights) {
            uses.weights <- modelUsesWeights(spec)
            if (uses.weights)
                ans[[i]] <- weights[[i.weights]]
            else ## weights supplied only if system model needs them
                stop(gettextf("'%s' contains weights for '%s', but system model for '%s' does not use weights",
                              "weights", name.y, name.y))
        }
        else
            ans[[i]] <- NULL
    }
    ans
}


## HAS_TESTS
checkAndTidyUpdateSystemModel <- function(updateSystemModel, systemModels,
                                          componentNames) {
    uses.ag <- any(sapply(systemModels, methods::is, "Aggregate"))
    n <- length(systemModels)
    if (is.null(updateSystemModel)) {
        if (uses.ag)
            ans <- rep(TRUE, times = n)
        else
            ans  <- c(TRUE, rep(FALSE, times = n - 1L))
        return(ans)
    }
    if (!is.character(updateSystemModel))
        stop(gettextf("'%s' has class \"%s\"",
                      "updateSystemModel", class(updateSystemModel)))
    if (any(is.na(updateSystemModel)))
        stop(gettextf("'%s' has missing values",
                      "updateSystemModel"))
    if (any(duplicated(updateSystemModel)))
        stop(gettextf("'%s' has duplicates",
                      "updateSystemModel"))
    names.sys.mod <- c("population", componentNames)
    for (name in updateSystemModel)
        if (!(name %in% names.sys.mod))
            stop(gettextf("element \"%s\" of '%s' is not the name of a demographic series",
                          name, "updateSystemModel"))
    ans <- names.sys.mod %in% updateSystemModel
    if (uses.ag & !all(ans))
        stop(gettextf("one or more system models uses aggregate values, but '%s' does not include all system models",
                      "updateSystemModel"))
    ans
}

## HAS_TESTS
checkAndTidyUpdateDataModel <- function(updateDataModel, dataModels,
                                        namesDatasets) {
    uses.ag <- any(sapply(dataModels, methods::is, "Aggregate"))
    n <- length(dataModels)
    if (is.null(updateDataModel)) {
        ans <- if (uses.ag) rep(TRUE, times = n) else rep(FALSE, times = n)
        return(ans)
    }
    if (!is.character(updateDataModel))
        stop(gettextf("'%s' has class \"%s\"",
                      "updateDataModel", class(updateDataModel)))
    if (any(is.na(updateDataModel)))
        stop(gettextf("'%s' has missing values",
                      "updateDataModel"))
    if (any(duplicated(updateDataModel)))
        stop(gettextf("'%s' has duplicates",
                      "updateDataModel"))
    for (name in updateDataModel)
        if (!(name %in% namesDatasets))
            stop(gettextf("element \"%s\" of '%s' is not the name of a demographic series",
                          name, "updateDataModel"))
    ans <- namesDatasets %in% updateDataModel
    if (uses.ag & !all(ans))
        stop(gettextf("one or more data models uses aggregate values, but '%s' does not include all data models",
                      "updateDataModel"))
    ans
}


## HAS_TESTS
checkSystemModels <- function(systemModels) {
    ## 'systemModels' is a list
    if (!is.list(systemModels))
        stop(gettextf("'%s' has class \"%s\"",
                      "systemModels", class(systemModels)))
    for (i in seq_along(systemModels)) {
        spec <- systemModels[[i]]
        ## element has class "SpecModel"
        if (!methods::is(spec, "SpecModel"))
            stop(gettextf("element %d of '%s' has class \"%s\"",
                          i, "systemModels", class(spec)))
        ## specification is valid
        return.value <- tryCatch(methods::validObject(spec),
                                 error = function(e) e)
        if (methods::is(return.value, "error"))
            stop(gettextf("element %d of '%s' is invalid : %s",
                          i, "systemModels", return.value$message))
        ## no 'series' argument supplied
        if (!identical(spec@series@.Data, "y"))
            stop(gettextf("element %d of '%s' has value for '%s' [\"%s\"] : in system models, series should instead be specified via response variable",
                          i, "systemModels", "series", spec@series@.Data))
    }
    NULL
}

## HAS_TESTS
## Note that every data model has to relate to one series,
## but every series does not have to have a data model.
makeSeriesIndices <- function(dataModels, account) {
    names.obs.mod <- sapply(dataModels, methods::slot, "series")
    names.components <- account@namesComponents
    names.series <- c("population", names.components)
    obs.not.in.series <- setdiff(names.obs.mod, names.series)
    if (length(obs.not.in.series) > 0L)
        stop(gettextf("'%s' contains a model for '%s', but '%s' does not have a series called '%s'",
                      "dataModels", obs.not.in.series[1L], "account", obs.not.in.series[1L]))
    ans <- integer(length = length(dataModels))
    for (i in seq_along(dataModels)) {
        name.obs <- names.obs.mod[i]
        i.series <- match(name.obs, names.series)
        ans[i] <- i.series - 1L # 'population' has index 0; first component has index 1
    }
    ans
}

## HAS_TESTS
checkExactDataModels <- function(dataModels,
                                 datasets,
                                 seriesIndices,
                                 namesComponents,
                                 namesDatasets) {
    is.exact <- sapply(dataModels, methods::is, "SpecExact")
    if (!any(is.exact))
        return(NULL)        
    ## 'population' does not have Exact data model
    is.popn <- seriesIndices == 0L
    is.popn.and.exact <- is.popn & is.exact
    i.popn.and.exact <- match(TRUE, is.popn.and.exact, nomatch = 0L)
    if (i.popn.and.exact > 0L)
        stop(gettextf("data model for dataset '%s' is '%s' but refers to '%s' series",
                      namesDatasets[i.popn.and.exact], "Exact", "population"))
    ## datasets with Exact data models do not have any NAs
    has.na <- sapply(datasets, anyNA)
    is.exact.with.na <- is.exact & has.na
    i.exact.with.na <- match(TRUE, is.exact.with.na, nomatch = 0L)
    if (i.exact.with.na > 0L)
        stop(gettextf("data model for dataset '%s' is '%s' but dataset '%s' has missing values",
                      namesDatasets[i.exact.with.na],
                      "Exact",
                      namesDatasets[i.exact.with.na]))
    ## if a series has an Exact data model,
    ## it does not have any other data models
    has.exact <- tapply(is.exact, seriesIndices, any)
    num.models <- table(seriesIndices)
    is.too.many <- has.exact & (num.models > 1L)
    if (any(is.too.many)) {
        i.too.many <- as.integer(names(has.exact))[is.too.many][1L]
        stop(gettextf("series \"%s\" has '%s' data model, but also has other data models",
                      namesComponents[i.too.many], "Exact"))
    }
    ## must have component that does not have Exact model
    if (identical(sum(is.exact), length(namesComponents)))
        stop(gettextf("all components have '%s' data models",
                      "Exact"))
    NULL
}

## NO_TESTS
makeCumProbComp <- function(nComponents, dataModels, seriesIndices) {
    for (i in seq_along(dataModels)) {
        if (methods::is(dataModels[[i]], "SpecExact")) {
            i.comp <- seriesIndices[i]
            nComponents[i.comp] <- 0L
        }
    }
    cumsum(nComponents) / sum(nComponents)
}

## HAS_TESTS
makeTransformsAccountToDatasets <- function(account, datasets, concordances,
                                            namesDatasets, seriesIndices) {
    population <- account@population
    components <- account@components
    series <- c(list(population), components)
    names.components <- account@namesComponents
    names.series <- c("population", names.components)
    names.concordances <- names(concordances)
    ans <- vector(mode = "list", length = length(datasets))
    for (i in seq_along(ans)) {
        dataset <- datasets[[i]]
        index <- seriesIndices[i] + 1L
        name.dataset <- namesDatasets[i]
        i.concordances <- match(name.dataset, names.concordances, nomatch = 0L)
        has.concordances <- i.concordances > 0L
        if (has.concordances)
            concordances.dataset <- concordances[[i.concordances]]
        else
            concordances.dataset <- list()
        transform <- tryCatch(dembase::makeTransform(x = series[[index]],
                                                     y = dataset,
                                                     concordances = concordances.dataset,
                                                     subset = TRUE,
                                                     check = TRUE),
                              error = function(e) e)
        if (methods::is(transform, "error"))
            stop(gettextf("unable to collapse series '%s' to make it compatible with dataset '%s' : %s",
                          names.series[index], namesDatasets[i], transform$message))
        transform <- dembase::makeCollapseTransformExtra(transform)
        ans[[i]] <- transform
    }
    ans
}

