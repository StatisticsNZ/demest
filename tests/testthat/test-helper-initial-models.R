
context("helper-initial-models")

n.test <- 5
test.identity <- FALSE
test.extended <- FALSE

test_that("DimScaleIsRegular works", {
    DimScaleIsRegular <- demest:::DimScaleIsRegular
    x <- new("Intervals", dimvalues = c(0, 5, 10, Inf))
    expect_true(DimScaleIsRegular(x))
    x <- new("Points", dimvalues = c(0, 5, 15))
    expect_false(DimScaleIsRegular(x))
    x <- new("Intervals", dimvalues = c(0, 5))
    expect_true(DimScaleIsRegular(x))
    x <- new("Points", dimvalues = 1)
    expect_true(DimScaleIsRegular(x))
    expect_error(DimScaleIsRegular("x"),
                 "'x' has class \"character\"")
})

test_that("addAgCertain works", {
    addAgCertain <- demest:::addAgCertain
    initialModel <- demest:::initialModel
    makeWeightAg <- demest:::makeWeightAg
    makeCollapseTransformExtra <- dembase:::makeCollapseTransformExtra
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    ## scalar
    aggregate <- AgCertain(value = 0.4)
    spec <- Model(y ~ Binomial(mean ~ age + region))
    x <- initialModel(spec, y = y, exposure = exposure)
    ans.obtained <- addAgCertain(object = x,
                                 aggregate = aggregate,
                                 defaultWeights = exposure)
    transform <- makeCollapseTransformExtra(makeTransform(x = y, y = 0))
    ans.expected <- list(theta = rep(0.4, 20),
                         value = new("ParameterVector", 0.4),
                         weight = makeWeightAg(weight = NULL,
                             default = exposure,
                             model = x,
                             thetaObj = y / y,
                             transform = transform,
                             values = 0.4),
                         transform = transform,
                         metadata = NULL,
                         mu = rep(0, 20),
                         slotsToExtract = new("BinomialVaryingAgCertain")@slotsToExtract,
                         iMethodModel = new("BinomialVaryingAgCertain")@iMethodModel)
    expect_identical(ans.obtained, ans.expected)
    ## Values
    value <- Counts(array(c(0.1, 0.2, 0.3),
                          dim = 3,
                          dimnames = list(region = letters[1:3])))
    aggregate <- AgCertain(value = value)
    spec <- Model(y ~ Binomial(mean ~ age + region))
    x <- initialModel(spec, y = y, exposure = exposure)
    ans.obtained <- addAgCertain(object = x,
                                 aggregate = aggregate,
                                 defaultWeights = exposure)
    transform <- makeCollapseTransformExtra(makeTransform(x = y, y = value, subset = T))
    ans.expected <- list(theta = c(rep(0.1, 5), rep(0.2, 5), rep(0.3, 5), x@theta[16:20]),
                         value = new("ParameterVector", c(0.1, 0.2, 0.3)),
                         weight = makeWeightAg(weight = NULL,
                             default = exposure,
                             thetaObj = y / y,
                             model = x,
                             transform = transform,
                             values = value),
                         transform = transform,
                         metadata = value@metadata,
                         mu = rep(0, 20),
                         slotsToExtract = new("BinomialVaryingAgCertain")@slotsToExtract,
                         iMethodModel = new("BinomialVaryingAgCertain")@iMethodModel)
    expect_identical(ans.obtained, ans.expected)
})

test_that("addAgNormal works", {
    addAgNormal <- demest:::addAgNormal
    initialModel <- demest:::initialModel
    makeWeightAg <- demest:::makeWeightAg
    makeCollapseTransformExtra <- dembase:::makeCollapseTransformExtra
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    ## scalar
    aggregate <- AgNormal(value = 0.4, sd = 0.3)
    spec <- Model(y ~ Binomial(mean ~ age + region))
    x <- initialModel(spec, y = y, exposure = exposure)
    ans.obtained <- addAgNormal(object = x,
                                aggregate = aggregate,
                                defaultWeights = exposure)
    transform <- makeCollapseTransformExtra(makeTransform(x = y, y = 0))
    ans.expected <- list(value = new("ParameterVector", sum(x@theta * exposure) / sum(exposure)),
                         mean = new("ParameterVector", 0.4),
                         scale = new("Scale", 0.1),
                         sd = new("ScaleVec", 0.3),
                         weight = makeWeightAg(weight = NULL,
                             default = exposure,
                             model = x,
                             thetaObj = y / y,
                             transform = transform,
                             values = 0.4),
                         transform = transform,
                         metadata = NULL,
                         mu = rep(0, 20),
                         slotsToExtract = new("BinomialVaryingAgNormal")@slotsToExtract,
                         iMethodModel = new("BinomialVaryingAgNormal")@iMethodModel)
    expect_equal(ans.obtained, ans.expected)
    ## Values
    value <- Counts(array(c(0.1, 0.2, 0.3),
                          dim = 3,
                          dimnames = list(region = letters[1:3])))
    aggregate <- AgNormal(value = value, sd = sqrt(value))
    spec <- Model(y ~ Binomial(mean ~ age + region))
    x <- initialModel(spec, y = y, exposure = exposure)
    ans.obtained <- addAgNormal(object = x,
                                aggregate = aggregate,
                                defaultWeights = exposure)
    transform <- makeCollapseTransformExtra(makeTransform(x = y, y = value, subset = T))
    value.ag <- dembase::collapse(x@theta * exposure, transform = transform) / exposure
    ans.expected <- list(value = new("ParameterVector", as.double(value.ag)),
                         mean = new("ParameterVector", c(0.1, 0.2, 0.3)),
                         scale = new("Scale", 0.1),
                         sd = new("ScaleVec", as.double(sqrt(value))),
                         weight = makeWeightAg(weight = NULL,
                             default = exposure,
                             model = x,
                             thetaObj = y / y,
                             transform = transform,
                             values = value),
                         transform = transform,
                         metadata = value@metadata,
                         mu = rep(0, 20),
                         slotsToExtract = new("BinomialVaryingAgNormal")@slotsToExtract,
                         iMethodModel = new("BinomialVaryingAgNormal")@iMethodModel)
    expect_equal(ans.obtained, ans.expected)
})

test_that("addAgPoisson works", {
    addAgPoisson <- demest:::addAgPoisson
    initialModel <- demest:::initialModel
    makeWeightAg <- demest:::makeWeightAg
    makeCollapseTransformExtra <- dembase:::makeCollapseTransformExtra
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    ## scalar
    aggregate <- AgPoisson(value = 0.4)
    spec <- Model(y ~ Poisson(mean ~ age + region))
    x <- initialModel(spec, y = y, exposure = exposure)
    ans.obtained <- addAgPoisson(object = x,
                                aggregate = aggregate,
                                defaultWeights = exposure)
    transform <- makeCollapseTransformExtra(makeTransform(x = y, y = 0))
    ans.expected <- list(value = new("ParameterVector", sum(x@theta * exposure) / sum(exposure)),
                         mean = new("ParameterVector", 0.4),
                         scale = new("Scale", 0.1),
                         weight = makeWeightAg(weight = NULL,
                             default = exposure,
                             model = x,
                             thetaObj = y / y,
                             transform = transform,
                             values = 0.4),
                         exposure = new("ScaleVec", sum(as.numeric(exposure))),
                         transform = transform,
                         metadata = NULL,
                         mu = rep(0, 20),
                         slotsToExtract = new("PoissonVaryingUseExpAgPoisson")@slotsToExtract,
                         iMethodModel = new("PoissonVaryingUseExpAgPoisson")@iMethodModel)
          expect_equal(ans.obtained, ans.expected)
    ## Values
    value <- Counts(array(c(0.1, 0.2, 0.3),
                          dim = 3,
                          dimnames = list(region = letters[1:3])))
    aggregate <- AgPoisson(value = value)
    spec <- Model(y ~ Poisson(mean ~ age + region))
    x <- initialModel(spec, y = y, exposure = exposure)
    ans.obtained <- addAgPoisson(object = x,
                                aggregate = aggregate,
                                defaultWeights = exposure)
    transform <- makeCollapseTransformExtra(makeTransform(x = y, y = value, subset = T))
    value.ag <- dembase::collapse(x@theta * exposure, transform = transform) / exposure
    ans.expected <- list(value = new("ParameterVector", as.double(value.ag)),
                         mean = new("ParameterVector", c(0.1, 0.2, 0.3)),
                         scale = new("Scale", 0.1),
                         weight = makeWeightAg(weight = NULL,
                             default = exposure,
                             model = x,
                             thetaObj = y / y,
                             transform = transform,
                             values = value),
                         exposure = new("ScaleVec", as.numeric(dembase::collapse(exposure, transform = transform))),
                         transform = transform,
                         metadata = value@metadata,
                         mu = rep(0, 20),
                         slotsToExtract = new("PoissonVaryingUseExpAgPoisson")@slotsToExtract,
                         iMethodModel = new("PoissonVaryingUseExpAgPoisson")@iMethodModel)
    expect_equal(ans.obtained, ans.expected)
})

test_that("addAgFun works", {
    addAgFun <- demest:::addAgFun
    initialModel <- demest:::initialModel
    makeWeightAg <- demest:::makeWeightAg
    makeMetaDataSubarraysBefore <- dembase::makeMetaDataSubarraysBefore
    makeCollapseTransformExtra <- dembase:::makeCollapseTransformExtra
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])),
                       dimscales = c(age = "Intervals"))
    y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])),
                dimscales = c(age = "Intervals"))
    ## scalar
    FUN <- function(x, weights) sum(x * sqrt(weights)) + 1
    aggregate <- AgFun(value = 0.4, sd = 0.3, FUN = FUN)
    spec <- Model(y ~ Poisson(mean ~ age + region))
    x <- initialModel(spec, y = y, exposure = exposure)
    ans.obtained <- addAgFun(object = x,
                             aggregate = aggregate,
                             defaultWeights = exposure)
    transform <- makeCollapseTransformExtra(makeTransform(x = y, y = 0))
    xArgs <- list(Values(array(x@theta,
                               dim = dim(y),
                               dimnames = dimnames(y)),
                         dimscales = c(age = "Intervals")))
    weight <- makeWeightAg(weight = NULL,
                           default = exposure,
                           model = x,
                           thetaObj = y / y,
                           transform = transform,
                           values = 0.4)
    weight <- Counts(array(weight,
                           dim = dim(y),
                           dimnames = dimnames(y)),
                     dimscales = c(age = "Intervals"))
    weightsArgs <- list(weight)
    ans.expected <- list(value = new("ParameterVector", FUN(xArgs[[1]], weightsArgs[[1]])),
                         mean = new("ParameterVector", 0.4),
                         sd = new("ScaleVec", 0.3),
                         metadata = NULL,
                         transform = transform,
                         funAg = FUN,
                         xArgs = xArgs,
                         weightsArgs = weightsArgs,
                         slotsToExtract = new("PoissonVaryingUseExpAgFun")@slotsToExtract,
                         iMethodModel = new("PoissonVaryingUseExpAgFun")@iMethodModel)
    expect_equal(ans.obtained, ans.expected)
    ## Values
    value <- Counts(array(c(0.1, 0.2, 0.3),
                          dim = 3,
                          dimnames = list(region = letters[1:3])))
    FUN <- function(x, weights) sum(x * sqrt(weights)) + 1
    aggregate <- AgFun(value = value, sd = sqrt(value), FUN = FUN)
    spec <- Model(y ~ Poisson(mean ~ age + region))
    x <- initialModel(spec, y = y, exposure = exposure)
    ans.obtained <- addAgFun(object = x,
                             aggregate = aggregate,
                             defaultWeights = exposure)
    transform <- makeCollapseTransformExtra(makeTransform(x = y, y = value, subset = TRUE))
    metadata.args <- makeMetaDataSubarraysBefore(metadata = y@metadata,
                                                 transform = transform)
    weight <- makeWeightAg(weight = NULL,
                           default = exposure,
                           model = x,
                           thetaObj = y / y,
                           transform = transform,
                           values = value)
    weight <- Counts(array(weight,
                           dim = dim(y),
                           dimnames = dimnames(y)),
                     dimscales = c(age = "Intervals"))
    xArgs <- list(new("Values",
                        .Data = array(x@theta[1:5], dim = c(5, 1), dimnames = dimnames(metadata.args[[1]])),
                        metadata = metadata.args[[1]]),
                    new("Values",
                        .Data = array(x@theta[6:10], dim = c(5, 1), dimnames = dimnames(metadata.args[[2]])),
                        metadata = metadata.args[[2]]),
                    new("Values",
                        .Data = array(x@theta[11:15], dim = c(5, 1), dimnames = dimnames(metadata.args[[3]])),
                        metadata = metadata.args[[3]]))
    weightsArgs <- list(new("Counts",
                              .Data = array(weight[1:5], dim = c(5, 1), dimnames = dimnames(metadata.args[[1]])),
                              metadata = metadata.args[[1]]),
                          new("Counts", 
                              .Data = array(weight[6:10], dim = c(5, 1), dimnames = dimnames(metadata.args[[2]])),
                              metadata = metadata.args[[2]]),
                          new("Counts", 
                              .Data = array(weight[11:15], dim = c(5, 1), dimnames = dimnames(metadata.args[[3]])),
                              metadata = metadata.args[[3]]))
    ans.expected <- list(value = new("ParameterVector",
                             c(FUN(xArgs[[1]], weightsArgs[[1]]),
                               FUN(xArgs[[2]], weightsArgs[[2]]),
                               FUN(xArgs[[3]], weightsArgs[[3]]))),
                         mean = new("ParameterVector", c(0.1, 0.2, 0.3)),
                         sd = new("ScaleVec", as.double(sqrt(value))),
                         metadata = value@metadata,
                         transform = transform,
                         funAg = FUN,
                         xArgs = xArgs,
                         weightsArgs = weightsArgs,
                         slotsToExtract = new("PoissonVaryingUseExpAgFun")@slotsToExtract,
                         iMethodModel = new("PoissonVaryingUseExpAgFun")@iMethodModel)
    expect_equal(ans.obtained, ans.expected)    
})

test_that("addAgFun throws appropriate errors", {
    addAgFun <- demest:::addAgFun
    initialModel <- demest:::initialModel
    makeCollapseTransformExtra <- dembase:::makeCollapseTransformExtra
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])),
                       dimscales = c(age = "Intervals"))
    y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])),
                dimscales = c(age = "Intervals"))
    value <- Counts(array(c(0.1, 0.2, 0.3),
                          dim = 3,
                          dimnames = list(region = letters[1:3])))
    FUN <- function(x, weights) sum(x * sqrt(weights)) + 1
    aggregate <- AgFun(value = value, sd = sqrt(value), FUN = FUN)
    spec <- Model(y ~ Poisson(mean ~ age + region))
    x <- initialModel(spec, y = y, exposure = exposure)
    x <- addAgFun(object = x,
                  aggregate = aggregate,
                  defaultWeights = exposure)
    expect_true(validObject(x))
    ## FUN throws error
    FUN.wrong <- function(x, weights) stop("my error")
    aggregate <- AgFun(value = value, sd = sqrt(value), FUN = FUN.wrong)
    spec <- Model(y ~ Poisson(mean ~ age + region))
    x <- initialModel(spec, y = y, exposure = exposure)
    expect_error(addAgFun(object = x,
                          aggregate = aggregate,
                          defaultWeights = exposure),
                 "error applying 'FUN' : my error")
    ## FUN returns non-numeric
    FUN.wrong <- function(x, weights) "wrong"
    aggregate <- AgFun(value = value, sd = sqrt(value), FUN = FUN.wrong)
    spec <- Model(y ~ Poisson(mean ~ age + region))
    x <- initialModel(spec, y = y, exposure = exposure)
    expect_error(addAgFun(object = x,
                          aggregate = aggregate,
                          defaultWeights = exposure),
                 "return value from 'FUN' has class \"character\"")
    ## FUN returns value with length != 1
    FUN.wrong <- function(x, weights) 1:2
    aggregate <- AgFun(value = value, sd = sqrt(value), FUN = FUN.wrong)
    spec <- Model(y ~ Poisson(mean ~ age + region))
    x <- initialModel(spec, y = y, exposure = exposure)
    expect_error(addAgFun(object = x,
                          aggregate = aggregate,
                          defaultWeights = exposure),
                 "return value from 'FUN' has length 2")
})

test_that("addAgLife works", {
    addAgLife <- demest:::addAgLife
    makeLifeExpBirth <- demest:::makeLifeExpBirth
    initialModel <- demest:::initialModel
    makeCollapseTransformExtra <- dembase:::makeCollapseTransformExtra
    expose <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])),
                       dimscales = c(age= "Intervals"))
    y <- Counts(array(rbinom(n = 20, size = expose, prob = 0.1),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])),
                dimscales = c(age= "Intervals"))
    ## scalar
    aggregate <- AgLife(value = 30, sd = 0.3)
    spec <- Model(y ~ Poisson(mean ~ age + region))
    x <- initialModel(spec, y = y, exposure = expose)
    ans.obtained <- addAgLife(object = x,
                              aggregate = aggregate,
                              defaultWeights = expose)
    mx <- (collapseDimension(x@theta*expose, dim = "region")
        / collapseDimension(expose, dim = "region"))
    transform <- makeTransform(x = y,
                               y = mx)
    transform <- makeCollapseTransformExtra(transform)
    metadataAg <- NULL
    metadataMx <- mx@metadata
    ax <- makeAxStart(mx)
    ax <- expandAx(ax = ax, object = mx)
    nx <- rep(1, 5)
    nAge <- new("Length", 5L)
    mx <- as.double(mx)
    value <- makeLifeExpBirth(mx = mx,
                              nx = nx,
                              ax = ax,
                              iAge0 = 1L,
                              nAge = nAge@.Data)
    value <- as.double(value)
    ans.expected <- list(value = new("ParameterVector", value),
                         mean = new("ParameterVector", 30),
                         sd = new("ScaleVec", 0.3),
                         metadataAg = metadataAg,
                         transform = transform,
                         metadataMx = metadataMx,
                         mx = mx,
                         ax = as.numeric(ax),
                         nx = nx,
                         nAge = nAge,
                         slotsToExtract = c(new("PoissonVaryingUseExp")@slotsToExtract,
                                            "valueAg", "mxAg"),
                         iMethodModel = 29L)
    expect_equal(ans.obtained, ans.expected)
    ## Values
    value <- Counts(array(c(0.1, 0.2, 0.3),
                          dim = 3,
                          dimnames = list(region = letters[1:3])))
    aggregate <- AgLife(value = value, sd = sqrt(value))
    spec <- Model(y ~ Poisson(mean ~ age + region))
    x <- initialModel(spec, y = y, exposure = expose)
    ans.obtained <- addAgLife(object = x,
                              aggregate = aggregate,
                              defaultWeights = expose)
    mx <- matrix(x@theta,nr=5)[,-4]*expose[,-4] / expose[,-4]
    transform <- makeTransform(x = y,
                               y = mx,
                               subset = TRUE)
    transform <- makeCollapseTransformExtra(transform)
    metadataAg <- value@metadata
    metadataMx <- mx@metadata
    ax <- makeAxStart(mx)
    ax <- expandAx(ax = ax, object = mx)
    nx <- rep(1, 5)
    nAge <- new("Length", 5L)
    mean <- as.numeric(value)
    value <- numeric(3)
    for (i in 1:3) {
        value[i] <- makeLifeExpBirth(mx = mx,
                                     nx = nx,
                                     ax = ax,
                                     iAge0 = 5L * (i-1L) + 1L,
                                     nAge = nAge@.Data)
    }
    value <- as.double(value)
    mx <- as.double(mx)
    ans.expected <- list(value = new("ParameterVector", value),
                         mean = new("ParameterVector", mean),
                         sd = new("ScaleVec", sqrt(mean)),
                         metadataAg = metadataAg,
                         transform = transform,
                         metadataMx = metadataMx,
                         mx = mx,
                         ax = as.numeric(ax),
                         nx = nx,
                         nAge = nAge,
                         slotsToExtract = c(new("PoissonVaryingUseExp")@slotsToExtract,
                                            "valueAg", "mxAg"),
                         iMethodModel = 29L)
    expect_equal(ans.obtained, ans.expected)
})


test_that("alignDataModelsToDatasets works", {
    alignDataModelsToDatasets <- demest:::alignDataModelsToDatasets
    data.models <- list(Model(census ~ PoissonBinomial(prob = 0.98)),
                        Model(tax ~ Poisson(mean ~ age)))
    datasets <- list(Counts(array(1:6,
                                  dim = 3:2,
                                  dimnames = list(age = 0:2, sex = c("f", "m")))),
                     Counts(array(1:4,
                                  dim = c(2, 2),
                                  dimnames = list(age = 0:1, sex = c("m", "f")))))
    namesDatasets <- c("tax", "census")
    expect_identical(alignDataModelsToDatasets(dataModels = data.models,
                                               datasets = datasets,
                                               namesDatasets = namesDatasets),
                     data.models[2:1])
    ## no observation models without datasets
    obs.mod.wrong <- c(data.models,
                       list(Model(wrong ~ PoissonBinomial(prob = 0.99))))
    expect_error(alignDataModelsToDatasets(dataModels = obs.mod.wrong,
                                           datasets = datasets,
                                           namesDatasets = namesDatasets),
                 "'dataModels' contains a model for 'wrong', but there is no dataset called 'wrong' in 'datasets'")
    ## no datasets without observation models
    datasets.wrong <- c(datasets, list(datasets[[1]]))
    names.datasets.wrong <- c(namesDatasets, "wrong")
    expect_error(alignDataModelsToDatasets(dataModels = data.models,
                                           datasets = datasets.wrong,
                                           namesDatasets = names.datasets.wrong),
                 "'datasets' contains a dataset called 'wrong', but 'dataModels' does not contain a model for 'wrong'")
})

test_that("checkAndTidyDatasets works", {
    checkAndTidyDatasets <- demest:::checkAndTidyDatasets
    x <- list(deaths = Counts(array(1:6, dim = 3:2, dimnames = list(age = 0:2, sex = c("f", "m")))),
              births= Counts(array(1:4, dim = c(2, 2), dimnames = list(age = 0:1, sex = c("m", "f")))))
    expect_identical(checkAndTidyDatasets(x),
                     lapply(x, toInteger))
    expect_error(checkAndTidyDatasets("wrong"),
                 "'datasets' has class \"character\"")
    expect_error(checkAndTidyDatasets(list()),
                 "'datasets' has length 0")
    x.wrong <- x
    x.wrong <- unname(x.wrong)
    expect_error(checkAndTidyDatasets(x.wrong),
                 "'datasets' does not have names")
    x.wrong <- x
    names(x.wrong)[1] <- NA
    expect_error(checkAndTidyDatasets(x.wrong),
                 "names for 'datasets' has missing values")
    x.wrong <- x
    names(x.wrong)[1] <- ""
    expect_error(checkAndTidyDatasets(x.wrong),
                 "names for 'datasets' has blanks")
    x.wrong <- x
    names(x.wrong)[1] <- "births"
    expect_error(checkAndTidyDatasets(x.wrong),
                 "names for 'datasets' has duplicates")
    x.wrong <- x
    x.wrong[[1]] <- as(x.wrong[[1]], "matrix")
    expect_error(checkAndTidyDatasets(x.wrong),
                 "dataset 'deaths' has class \"matrix\"")
    x.wrong <- x
    x.wrong[[1]][1] <- 1.1
    expect_error(checkAndTidyDatasets(x.wrong),
                 "dataset 'deaths' has non-integer values")
    x.wrong <- x
    x.wrong[[2]][1] <- -1
    expect_error(checkAndTidyDatasets(x.wrong),
                 "dataset 'births' has negative values")
    deaths <- Counts(array(1:6, dim = 3:2, dimnames = list(age = 0:2, sex = c("f", "m"))))
    deaths[1] <- NA
    births <- Counts(array(1:4, dim = c(2, 2), dimnames = list(age = 0:1, sex = c("m", "f"))))
    subtotals <- CountsOne(10, labels = "1", name = "age")
    births[2,] <- NA
    births <- attachSubtotals(births, subtotals = subtotals)
    x <- list(deaths = deaths, births = births)
    set.seed(1)
    ans.obtained <- checkAndTidyDatasets(x)
    set.seed(1)
    ans.expected <- x
    expect_identical(ans.obtained, ans.expected)
})

test_that("checkAndTidyExposure works", {
    checkAndTidyExposure <- demest:::checkAndTidyExposure
    exposure <- Counts(array((1:24) * 1.0,
                             dim = 4:2,
                             dimnames = list(age = 0:3, region = c("a", "b", "c"), sex = c("f", "m"))))
    y <- aperm(exposure, perm = c("sex", "age", "region"))
    expect_identical(checkAndTidyExposure(exposure = exposure, y = y),
                     y)
    expect_identical(checkAndTidyExposure(exposure = NULL, y = y),
                     NULL)
    exposure.wrong <- as(exposure, "Values")
    expect_error(checkAndTidyExposure(exposure.wrong, y = y),
                 "'exposure' has class \"Values\"")
    exposure[1] <- NA
    expect_error(checkAndTidyExposure(exposure = exposure, y = y),
                 "'exposure' has missing values in places where 'y' does not")
    exposure[1] <- -1
    expect_error(checkAndTidyExposure(exposure = exposure, y = y),
                 "'exposure' has negative values")
    exposure <- Counts(array((1:24) * 1.0,
                             dim = 4:2,
                             dimnames = list(age = 0:3, region = c("a", "b", "c"), sex = c("f", "m"))))
    exposure[1:4] <- NA
    y <- aperm(exposure, perm = c("sex", "age", "region"))
    expect_identical(checkAndTidyExposure(exposure = exposure, y = y),
                     y)
})

test_that("checkAndTidyListArgForEstimateFun works", {
    checkAndTidyListArgForEstimateFun <- demest:::checkAndTidyListArgForEstimateFun
    ans.obtained <- checkAndTidyListArgForEstimateFun(arg = list(),
                                                      name = "data",
                                                      isCounts = TRUE)
    ans.expected <- NULL
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- checkAndTidyListArgForEstimateFun(arg = list(model = list(),
                                                                 dataModels = list()),
                                                      name = "data",
                                                      isCounts = TRUE)
    ans.expected <- list(model = list(),
                         dataModels = list())
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- checkAndTidyListArgForEstimateFun(arg = list(systemModels = list(),
                                                                 dataModels = list()),
                                                      name = "data",
                                                      isCounts = FALSE)
    ans.expected <- list(systemModels = list(),
                         dataModels = list())
    expect_identical(ans.obtained, ans.expected)
    expect_error(checkAndTidyListArgForEstimateFun(arg = list(list(),
                                                              list()),
                                                      name = "data",
                                                   isCounts = TRUE),
                 "'data' does not have names")
    expect_error(checkAndTidyListArgForEstimateFun(arg = list(list(),
                                                              list()),
                                                      name = "data",
                                                   isCounts = TRUE),
                 "'data' does not have names")
    expect_error(checkAndTidyListArgForEstimateFun(arg = list(model = list(),
                                                              list()),
                                                      name = "data",
                                                   isCounts = TRUE),
                 "names for 'data' have blanks")
    expect_error(checkAndTidyListArgForEstimateFun(arg = list(model = list(),
                                                              model = list()),
                                                      name = "data",
                                                   isCounts = TRUE),
                 "names for 'data' have duplicates")
    expect_error(checkAndTidyListArgForEstimateFun(arg = list(model = list(),
                                                              wrong = list()),
                                                      name = "data",
                                                   isCounts = TRUE),
                 "invalid name for 'data' : \"wrong\"")
    expect_error(checkAndTidyListArgForEstimateFun(arg = list(model = list(),
                                                              dataModels = list()),
                                                      name = "data",
                                                   isCounts = FALSE),
                 "invalid name for 'data' : \"model\"")
    expect_error(checkAndTidyListArgForEstimateFun(arg = list(systemModels = 1,
                                                              dataModels = list()),
                                                      name = "data",
                                                   isCounts = FALSE),
                 "element \"systemModels\" of 'data' does not have class \"list\"")
    expect_error(checkAndTidyListArgForEstimateFun(arg = list(systemModels = list(),
                                                              dataModels = 1),
                                                      name = "data",
                                                   isCounts = FALSE),
                 "element \"dataModels\" of 'data' does not have class \"list\"")
    expect_error(checkAndTidyListArgForEstimateFun(arg = list(model = 1,
                                                              dataModels = list()),
                                                      name = "aggregate",
                                                   isCounts = TRUE),
                 "'aggregate' contains elements not of class \"SpecAggregate\"")
    expect_error(checkAndTidyListArgForEstimateFun(arg = list(model = "a",
                                                              dataModels = list()),
                                                      name = "upper",
                                                   isCounts = TRUE),
                 "'upper' contains elements not of type \"numeric\"")
})

test_that("checkAndTidySDAg works", {
    checkAndTidySDAg <- demest:::checkAndTidySDAg
    sd <- Counts(array(1:4,
                       dim = 4,
                       dimnames = list(region = 1:4)))
    value <- 4:1
    metadata <- sd@metadata
    ans.obtained <- checkAndTidySDAg(sd = sd,
                                     value = value,
                                     metadata = metadata)
    ans.expected <- new("ScaleVec", as.double(1:4))
    expect_identical(ans.obtained, ans.expected)
    sd <- 1L
    value <- 3
    metadata <- NULL
    ans.obtained <- checkAndTidySDAg(sd = sd,
                                     value = value,
                                     metadata = metadata)
    ans.expected <- new("ScaleVec", 1)
    expect_identical(ans.obtained, ans.expected)
    sd <- 1L
    value <- 4:1
    metadata <- Values(array(4:1,
                             dim = 4,
                             dimnames = list(region = 1:4)))@metadata
    ans.obtained <- checkAndTidySDAg(sd = sd,
                                     value = value,
                                     metadata = metadata)
    ans.expected <- new("ScaleVec", rep(as.double(1), 4))
    expect_identical(ans.obtained, ans.expected)
    ## 'sd' and 'value' have same length
    sd <- Counts(array(1:4,
                       dim = 4,
                       dimnames = list(region = 1:4)))
    value <- 4:2
    metadata <- sd@metadata
    expect_error(checkAndTidySDAg(sd = sd,
                                  value = value,
                                  metadata = metadata),
                 "'sd' and 'value' have different lengths")
    ## 'sd' has same metadata as 'value'
    sd <- Counts(array(1:4,
                       dim = 4,
                       dimnames = list(region = 1:4)))
    value <- 4:1
    metadata <- Counts(array(1:4,
                             dim = 4,
                             dimnames = list(region = 4:1)))@metadata
    expect_error(checkAndTidySDAg(sd = sd,
                                  value = value,
                                  metadata = metadata),
                 "'sd' and 'value' have different metadata")
    ## 'sd' has no missing values"
    sd <- Counts(array(c(NA, 1:3),
                       dim = 4,
                       dimnames = list(region = 1:4)))
    value <- 4:1
    metadata <- sd@metadata
    expect_error(checkAndTidySDAg(sd = sd,
                                  value = value,
                                  metadata = metadata),
                 "'sd' has missing values")
    ## 'sd' has no negative values
    sd <- Counts(array(c(-1, 1:3),
                       dim = 4,
                       dimnames = list(region = 1:4)))
    value <- 4:1
    metadata <- sd@metadata
    expect_error(checkAndTidySDAg(sd = sd,
                                  value = value,
                                  metadata = metadata),
                 "'sd' has negative values")
    ## 'sd' has length 1
    sd <- 1:2
    value <- 3:2
    metadata <- NULL
    expect_error(checkAndTidySDAg(sd = sd,
                                  value = value,
                                  metadata = metadata),
                 "'sd' is numeric but does not have length 1")
    ## 'sd' is not missing    
    sd <- as.numeric(NA)
    value <- 3
    metadata <- NULL
    expect_error(checkAndTidySDAg(sd = sd,
                                  value = value,
                                  metadata = metadata),
                 "'sd' is missing")
    ## 'sd' is non-negative    
    sd <- -1
    value <- 3
    metadata <- NULL
    expect_error(checkAndTidySDAg(sd = sd,
                                  value = value,
                                  metadata = metadata),
                 "'sd' is negative")
})

test_that("checkAndTidyWeights works", {
    checkAndTidyWeights <- demest:::checkAndTidyWeights
    weights <- Counts(array(1:24,
                             dim = 4:2,
                             dimnames = list(age = 0:3, region = c("a", "b", "c"), sex = c("f", "m"))))
    y <- aperm(toDouble(weights), perm = c("sex", "age", "region"))
    expect_identical(checkAndTidyWeights(weights = weights, y = y),
                     y)
    expect_identical(checkAndTidyWeights(weights = NULL, y = y),
                     NULL)
    weights[1] <- NA
    expect_error(checkAndTidyWeights(weights = weights, y = y),
                 "'weights' has missing values in places where 'y' does not")
    weights[1] <- -1
    expect_error(checkAndTidyWeights(weights = weights, y = y),
                 "'weights' has negative values")
    weights <- Counts(array(1:24,
                             dim = 4:2,
                            dimnames = list(age = 0:3, region = c("a", "b", "c"), sex = c("f", "m"))))
    weights[1:10] <- NA
    y <- aperm(toDouble(weights), perm = c("sex", "age", "region"))
    expect_identical(checkAndTidyWeights(weights = weights, y = y),
                     y)
})

test_that("checkAndTidyY works", {
    checkAndTidyY <- demest:::checkAndTidyY
    y <- Counts(array(1:24,
                      dim = 4:2,
                      dimnames = list(age = 0:3, region = c("a", "b", "c"), sex = c("f", "m"))))
    ## 'y' is Demographic
    y.wrong <- as(y, "array")
    expect_error(checkAndTidyY(y.wrong),
                 "'y' has class \"array\"")
    ## 'y' has no zero-length dimensions
    y.wrong <- Counts(array(1L,
                            dim = c(0, 3:2),
                            dimnames = list(age = character(), region = c("a", "b", "c"), sex = c("f", "m"))))
    expect_error(checkAndTidyY(y.wrong),
                 "dimension \"age\" of 'y' has length 0")
    ## 'y' does not have iteration dimension
    y.wrong <- Counts(array(1:24,
                            dim = 4:2,
                            dimnames = list(iter = 1:4, region = c("a", "b", "c"), sex = c("f", "m"))))
    expect_error(checkAndTidyY(y.wrong),
                 "dimension \"iter\" of 'y' has dimtype \"iteration\"")
    ## 'y' does not have quantile dimension
    y.wrong <- Counts(array(1:24,
                            dim = 4:2,
                            dimnames = list(quantile = c(0, 0.1, 0.8, 1),
                                region = c("a", "b", "c"), sex = c("f", "m"))))
    expect_error(checkAndTidyY(y.wrong),
                 "dimension \"quantile\" of 'y' has dimtype \"quantile\"")
    y <- Counts(array(1:24,
                      dim = 4:2,
                      dimnames = list(age = 0:3, region = c("a", "b", "c"), sex = c("f", "m"))))
    ## 'y' has at least 2 non-missing values
    y.wrong <- Counts(array(c(1, rep(NA, 23)),
                            dim = 4:2,
                            dimnames = list(age = 0:3,
                                region = c("a", "b", "c"),
                                sex = c("f", "m"))))
    expect_error(checkAndTidyY(y.wrong),
                 "'y' has fewer than 2 non-missing values")
})

test_that("checkAxAg works", {
    checkAxAg <- demest:::checkAxAg
    expect_identical(checkAxAg(ax = NULL, value = 1),
                     NULL)
    expect_identical(checkAxAg(ax = Values(array(0.5,
                                                 dim = 2,
                                                 dimnames = list(age = c("0", "1-4")))),
                               value = 1),
                     NULL)
    expect_identical(checkAxAg(ax = Values(array(0.5,
                                                 dim = c(2, 2),
                                                 dimnames = list(age = c("0", "1-4"),
                                                                 sex = c("female", "male")))),
                               value = ValuesOne(1:2, labels = c("female", "male"), name = "sex")),
                     NULL)
    expect_error(checkAxAg(ax = ValuesOne(integer(), labels = character(), name = "age",
                                          dimscale = "Intervals"),
                           value = 1),
                 "'ax' has length 0")
    expect_error(checkAxAg(ax = "wrong",
                           value = 1),
                 "'ax' has class \"character\"")
    expect_error(checkAxAg(ax = ValuesOne(1:2, labels = c("female", "male"), name = "sex"),
                           value = 1),
                 "'ax' does not have a dimension with dimtype \"age\"")
    expect_error(checkAxAg(ax = ValuesOne(1:2, labels = c(0, 5), name = "age"),
                           value = 1),
                 "dimension of 'ax' with dimtype \"age\" does not have dimscale \"Intervals\"")
    expect_error(checkAxAg(ax = Values(array(0.5,
                                             dim = c(2, 2),
                                             dimnames = list(age = c("0", "1-4"),
                                                             sex = c("female", "male")))),
                           value = 1),
                 "'value' is not a demographic array, but 'ax' has more than one dimension")
    expect_error(checkAxAg(ax = Values(array(0.5,
                                             dim = c(2, 2, 2),
                                             dimnames = list(age = c("0", "1-4"),
                                                             region = c("a", "b"),
                                                             sex = c("female", "male")))),
                           value = ValuesOne(1:2, labels = c("female", "male"), name = "sex")),
                 "'ax' and 'value' not compatible")
})    

test_that("checkConcordances works", {
    checkConcordances <- demest:::checkConcordances
    Concordance <- dembase::Concordance
    x <- Values(array(1:4,
                      dim = c(2, 2),
                      dimnames = list(age = c(0, "1+"),
                                      region = c("a", "b"))))
    concordances <- list()
    expect_identical(checkConcordances(concordances = concordances),
                     NULL)
    conc <- Concordance(data.frame(from = c("a", "b"), to = "A"))
    concordances <- list(region = conc)
    expect_identical(checkConcordances(concordances = concordances),
                     NULL)
    expect_error(checkConcordances(concordances = NULL),
                 "'concordances' has class \"NULL\"")
    expect_error(checkConcordances(concordances = list(eth = conc),
                                   "'concordances' has elements not of class \"ManyToOne\""))
    conc <- Concordance(data.frame(from = c("a", "b"), to = "A"))
    expect_error(checkConcordances(concordances = list(conc),
                                   "'concordances' does not have names"))
    conc <- Concordance(data.frame(from = c("a", "b"), to = "A"))
    expect_error(checkConcordances(concordances = list(eth = conc, eth = conc)),
                 "'concordances' has duplicate names")
})

test_that("checkConcordancesDatasets works", {
    checkConcordancesDatasets <- demest:::checkConcordancesDatasets
    concordances <- list(reg_births = list(region = Concordance(data.frame(from = c("B", "C"), to = c("b", "b")))),
                         census = list(region = Concordance(data.frame(from = c("C", "B"), to = c("b", "b")))))
    datasets <- list(census = Counts(array(1:4,
                                           dim = c(2, 2),
                                           dimnames = list(age = c(0, "1+"),
                                                           region = c("a", "b")))),
                     reg_births = Counts(array(1:2,
                                           dim = c(1, 2),
                                           dimnames = list(age = "1+",
                                                           region = c("a", "b")))))
    expect_identical(checkConcordancesDatasets(concordances = concordances,
                                               datasets = datasets,
                                               namesDatasets = names(datasets)),
                     NULL)
    expect_identical(checkConcordancesDatasets(concordances = list(),
                                               datasets = datasets,
                                               namesDatasets = names(datasets)),
                     NULL)
    expect_error(checkConcordancesDatasets(concordances = "wrong",
                                           datasets = datasets,
                                           namesDatasets = names(datasets)),
                 "'concordances' has class \"character\"")
    concordances.wrong <- concordances
    names(concordances.wrong) <- NULL
    expect_error(checkConcordancesDatasets(concordances = concordances.wrong,
                                           datasets = datasets,
                                           namesDatasets = names(datasets)),
                 "'concordances' does not have names")
    concordances.wrong <- concordances
    names(concordances.wrong) <- c("census", "census")
    expect_error(checkConcordancesDatasets(concordances = concordances.wrong,
                                           datasets = datasets,
                                           namesDatasets = names(datasets)
                                           ),
                 "'concordances' has duplicate names")
    concordances.wrong <- concordances
    names(concordances.wrong) <- c("census", "wrong")
    expect_error(checkConcordancesDatasets(concordances = concordances.wrong,
                                           datasets = datasets,
                                               namesDatasets = names(datasets)),
                 "'concordances' has an element called \"wrong\", but \"wrong\" is not the name of a dataset")
    concordances.wrong <- concordances
    concordances.wrong[[1L]] <- "wrong"
    expect_error(checkConcordancesDatasets(concordances = concordances.wrong,
                                           datasets = datasets,
                                           namesDatasets = names(datasets)),
                 "element \"reg_births\" of 'concordances' is not a list")
    concordances.wrong <- concordances
    concordances.wrong[[2]][[1]] <- "wrong"
    expect_error(checkConcordancesDatasets(concordances = concordances.wrong,
                                           datasets = datasets,
                                           namesDatasets = names(datasets)),
                 "element \"census\" of 'concordances' has elements not of class \"ManyToOne\"")
    concordances.wrong <- concordances
    names(concordances.wrong[[2]]) <- NULL
    expect_error(checkConcordancesDatasets(concordances = concordances.wrong,
                                           datasets = datasets,
                                           namesDatasets = names(datasets)),
                 "element \"census\" of 'concordances' does not have names")
    concordances.wrong <- concordances
    concordances.wrong[[2]][2] <- concordances.wrong[[2]][1]
    names(concordances.wrong[[2]])[2] <- "region"
    expect_error(checkConcordancesDatasets(concordances = concordances.wrong,
                                           datasets = datasets,
                                           namesDatasets = names(datasets)),
                 "element \"census\" of 'concordances' has duplicate names")
})

test_that("checkForMarginalTerms works", {
    checkForMarginalTerms <- demest:::checkForMarginalTerms
    expect_identical(checkForMarginalTerms(~ age * sex),
                     NULL)
    expect_identical(checkForMarginalTerms(~ 1),
                     NULL)
    expect_error(checkForMarginalTerms(~ age - 1),
                 "formula '~age \\- 1' does not include an intercept")
    expect_error(checkForMarginalTerms(~ age*sex - age),
                 "term 'age' is marginal to term 'age:sex' but is not included in formula '~age \\* sex \\- age'")
    expect_error(checkForMarginalTerms(~ age*sex - sex),
                 "term 'sex' is marginal to term 'age:sex' but is not included in formula '~age \\* sex \\- sex'")
    expect_error(checkForMarginalTerms(~ age*sex*region - region:sex),
                 paste("term 'sex:region' is marginal to term 'age:sex:region' but is not included",
                       "in formula '~age \\* sex \\* region \\- region:sex'"))
})

test_that("checkFormulaMu works", {
    checkFormulaMu <- demest:::checkFormulaMu
    expect_identical(checkFormulaMu(mean ~ age + sex),
                     NULL)
    expect_error(checkFormulaMu( ~ age + sex),
                 "'~age \\+ sex' is not a valid formula")
    expect_error(checkFormulaMu(prob ~ age + sex),
                 "formula 'prob ~ age \\+ sex' does not have response 'mean'")
})


test_that("checkFilename works", {
    checkFilename <- demest:::checkFilename
    ## filename is character
    expect_error(checkFilename(filename = 1),
                 "'filename' does not have type \"character\"")
    ## filename has length 1
    expect_error(checkFilename(filename = c("myfile", "myotherfile")),
                 "'filename' does not have length 1")
    ## filename is not missing
    expect_error(checkFilename(filename = as.character(NA)),
                 "'filename' is missing")
})


test_that("checkFunAg works", {
    checkFunAg <- demest:::checkFunAg
    FUN <- function(x, weights) 1
    expect_identical(checkFunAg(FUN),
                     NULL)
    expect_error(checkFunAg("wrong"),
                 "'FUN' has class \"character\"")
    FUN <- function(x, wrong) 1
    expect_error(checkFunAg(FUN),
                 "'FUN' does not have formal arguments 'x' and 'weights'")
})

test_that("checkLengthDimInFormula works", {
    checkLengthDimInFormula <- demest:::checkLengthDimInFormula
    y <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(age = c("0-4", "5-9", "10+"), sex = c("f", "m"))))
    formula <- mean ~ age * sex
    expect_identical(checkLengthDimInFormula(y = y, formula = formula),
                     NULL)
    y <- Counts(array(1:6,
                      dim = c(3, 2, 1),
                      dimnames = list(age = c("0-4", "5-9", "10+"), sex = c("f", "m"),
                          time = "2012")),
                dimscales = c(time = "Intervals"))
    formula <- mean ~ age * sex
    expect_identical(checkLengthDimInFormula(y = y, formula = formula),
                     NULL)
    formula <- mean ~ age * sex + time
    expect_error(checkLengthDimInFormula(y = y, formula = formula),
                 "dimension \"time\" is used in formula 'mean ~ age \\* sex \\+ time' but has length 1")
    y <- Counts(array(1:6,
                      dim = c(3, 2, 1, 1),
                      dimnames = list(age = c("0-4", "5-9", "10+"), sex = c("f", "m"),
                          region = "a", time = "2012")),
                dimscales = c(time = "Intervals"))
    formula <- mean ~ age * sex * region * time
    expect_error(checkLengthDimInFormula(y = y, formula = formula),
                 paste("dimension \"region\" is used in formula 'mean ~ age \\* sex \\* region",
                       "\\* time' but has length 1"))
})

test_that("checkListNames works", {
    checkListNames <- demest:::checkListNames
    expect_error(checkListNames(NULL, "datasets"),
                 "'datasets' does not have names")
    expect_error(checkListNames(c("births", NA), "datasets"),
                 "names for 'datasets' has missing values")
    expect_error(checkListNames(c("", "deaths"), "datasets"),
                 "names for 'datasets' has blanks")
    expect_error(checkListNames(c("births", "births"), "datasets"),
                 "names for 'datasets' has duplicates")
})

test_that("defaultPrior generates appropriate Specification objects from valid inputs", {
    defaultPrior <- demest:::defaultPrior
    ## ExchFixed because length <= 2
    beta <- rnorm(2)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:2])))
    expect_identical(defaultPrior(beta = beta, metadata = metadata),
                     ExchFixed())
    ## Exchangeable because interaction between state
    beta <- rnorm(12)
    metadata <- new("MetaData",
                    nms = c("region", "eth"),
                    dimtypes = c("state", "state"),
                    DimScales = list(new("Categories", dimvalues = letters[1:4]),
                    new("Categories", dimvalues = letters[20:22])))
    expect_identical(defaultPrior(beta = beta, metadata = metadata),
                     Exch())
    ## Exchangeable because dimtype is "state"
    beta <- rnorm(5)
    metadata <- new("MetaData",
                    nms = "state",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:5])))
    expect_identical(defaultPrior(beta = beta, metadata = metadata),
                     Exch())
    ## DLM because main effect with time
    beta <- rnorm(5)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2000:2004)))
    expect_identical(defaultPrior(beta = beta, metadata = metadata),
                     DLM())
    expect_false(defaultPrior(beta = beta, metadata = metadata)@phiKnown)
    ## DLM because main effect with time - even though points irregular
    beta <- rnorm(5)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = c(2000:2003, 2005))))
    expect_identical(defaultPrior(beta = beta, metadata = metadata),
                     DLM())
    expect_false(defaultPrior(beta = beta, metadata = metadata)@phiKnown)
    ## Exch, because main effect with age
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "age",
                    dimtypes = "age",
                    DimScales = list(new("Intervals", dimvalues = 0:10)))
    expect_identical(defaultPrior(beta = beta, metadata = metadata),
                     Exch())
    ## Exch because main effect with cohort
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "cohort",
                    dimtypes = "cohort",
                    DimScales = list(new("Intervals", dimvalues = 0:10)))
    expect_identical(defaultPrior(beta = beta, metadata = metadata),
                     Exch())
    ## DLM, no trend because interaction with time
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = c("sex", "time"),
                    dimtypes = c("sex", "time"),
                    DimScales = list(new("Sexes", dimvalues = c("f", "m")),
                        new("Points", dimvalues = 2000:2004)))
    expect_identical(defaultPrior(beta = beta, metadata = metadata),
                     DLM(along = "time", trend = NULL))
    ## DLM because interaction with time
    beta <- rnorm(50)
    metadata <- new("MetaData",
                    nms = c("age", "time"),
                    dimtypes = c("age", "time"),
                    DimScales = list(new("Intervals", dimvalues = 0:10),
                        new("Points", dimvalues = 2000:2004)))
    expect_identical(defaultPrior(beta = beta, metadata = metadata),
                     DLM(trend = NULL, along = "time"))
    ## DLM because interaction with time
    beta <- rnorm(50)
    metadata <- new("MetaData",
                    nms = c("cohort", "time"),
                    dimtypes = c("cohort", "time"),
                    DimScales = list(new("Intervals", dimvalues = 0:10),
                        new("Points", dimvalues = c(2000:2003, 2005))))
    expect_identical(defaultPrior(beta = beta, metadata = metadata),
                     DLM(trend = NULL, along = "time"))
})

test_that("defaultPrior throws appropriate errors", {
    defaultPrior <- demest:::defaultPrior
    beta <- rnorm(1)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1])))
    expect_error(defaultPrior(beta = beta, metadata = metadata),
                 "'beta' for \"region\" has length 1")
    beta <- rnorm(2)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:3])))
    expect_error(defaultPrior(beta = beta, metadata = metadata),
                 "length of 'beta' for \"region\" \\[2\\] not equal to product of dimensions \\[3\\]")
})

test_that("checkDataModels works", {
    checkDataModels <- demest:::checkDataModels
    x <- list(Model(tax ~ Poisson(mean ~ age),
                    age ~ Exch(error = Error(robust = TRUE))),
              Model(census ~ PoissonBinomial(prob = 0.97)))
    expect_identical(checkDataModels(x), NULL)
    ## 'dataModels' is a list
    expect_error(checkDataModels("wrong"),
                 "'dataModels' has class \"character\"")
    ## all elements have class "SpecModel"
    x.wrong <- x
    x.wrong[[2]] <- "wrong"
    expect_error(checkDataModels(x.wrong),
                 "element 2 of 'dataModels' has class \"character\"")
    ## all elements use exposure
    x.wrong <- x
    x.wrong[[1]] <- Model(tax ~ Poisson(mean ~ age, useExpose = FALSE),
                          age ~ Exch(error = Error(robust = TRUE)))
    expect_error(checkDataModels(x.wrong),
                 "model 1 of 'dataModels' does not use exposure")
    ## element has name
    x.wrong <- x
    x.wrong[[1]]@nameY@.Data <- as.character(NA)
    expect_error(checkDataModels(x.wrong),
                 "element 1 of 'dataModels' has no name for response variable")
    x.wrong <- x
    x.wrong[[2]]@nameY@.Data <- ""
    expect_error(checkDataModels(x.wrong),
                 "element 2 of 'dataModels' has no name for response variable")
    ## specification of of model is valid
    x.wrong <- x
    x.wrong[[1]]@namesSpecsPriors <- "wrong"
    expect_error(checkDataModels(x.wrong),
                 "error in data model for 'tax'")
    ## 'series' argument supplied if needed
    expect_error(checkDataModels(x, needsNonDefaultSeriesArg = TRUE),
                 "'series' argument not supplied in data model for 'tax'")
    ## no 'series' argument supplied if not needed
    x.wrong <- x
    x.wrong[[1]]@series@.Data <- "population"
    expect_warning(checkDataModels(x.wrong),
                   "non-default argument for 'series' in data model for 'tax' ignored")
})

test_that("checkSpecWeightAg works", {
    checkSpecWeightAg <- demest:::checkSpecWeightAg
    ## weights, metadata both NULL
    ans.obtained <- checkSpecWeightAg(weights = NULL, metadata = NULL)
    ans.expected <- NULL
    expect_identical(ans.obtained, ans.expected)
    ## weights Counts, metadata NULL
    weights <- Counts(array(c(NA, 1:3),
                            dim = 4,
                            dimnames = list(region = 1:4)))
    ans.obtained <- checkSpecWeightAg(weights = weights, metadata = NULL)
    ans.expected <- NULL
    expect_identical(ans.obtained, ans.expected)
    ## weights Counts, metadata non-NULL
    weights <- Counts(array(1:4,
                            dim = 4,
                            dimnames = list(region = 1:4)))
    metadata <- weights@metadata
    ans.obtained <- checkSpecWeightAg(weights = weights, metadata = metadata)
    ans.expected <- NULL
    expect_identical(ans.obtained, ans.expected)
    ## weights is Values
    weights <- Values(array(1:4,
                            dim = 4,
                            dimnames = list(region = 1:4)))
    metadata <- weights@metadata
    expect_error(checkSpecWeightAg(weights = weights, metadata = metadata),
                 "'weights' has class \"Values\"")
    ## weights and metadata not compatible
    weights <- Counts(array(1,
                            dim = 4,
                            dimnames = list(region = 1:4)))
    metadata <- Counts(array(1,
                            dim = 4,
                            dimnames = list(region = 2:5)))@metadata
    expect_error(checkSpecWeightAg(weights = weights, metadata = metadata),
                 "'weights' and 'value' not compatible")
})

test_that("checkTermsFromFormulaFound works", {
    checkTermsFromFormulaFound <- demest:::checkTermsFromFormulaFound
    y <- Counts(array(1:6, dim = c(3, 2), dimnames = list(age = 0:2, sex = c("f", "m"))))
    expect_identical(checkTermsFromFormulaFound(y = y, mean ~ age + sex),
                     NULL)
    expect_identical(checkTermsFromFormulaFound(y = y, mean ~ sex),
                     NULL)
    expect_identical(checkTermsFromFormulaFound(y = y, mean ~ 1),
                     NULL)
    expect_error(checkTermsFromFormulaFound(y = y, mean ~ age + wrong),
                 sprintf("dimension %s from formula 'mean ~ age \\+ wrong' not found",
                         dQuote("wrong")))
    expect_error(checkTermsFromFormulaFound(y = y, mean ~ age + wrong1 + wrong2),
                 sprintf("dimensions %s from formula 'mean ~ age \\+ wrong1 \\+ wrong2' not found",
                         paste(dQuote(c("wrong1", "wrong2")), collapse = ", ")))
})

test_that("convertToFormulaOrder works", {
    convertToFormulaOrder <- demest:::convertToFormulaOrder
    loglm <- MASS::loglm
    formulaMu <- mean ~ region * age
    y <- array(rpois(n = 20, lambda = 10),
               dim = c(5, 4),
               dimnames = list(age = 0:4, region = letters[1:4]))
    betas <- loglm(formulaMu, data = y)$param
    ans <- convertToFormulaOrder(betas = betas, formulaMu = formulaMu)
    expect_identical(names(ans)[1], names(betas)[1])
    expect_identical(ans[[1]], betas[[1]])
    expect_identical(names(ans)[-1], attr(terms(formulaMu), "term.labels"))
    expect_identical(unname(sort(unlist(ans))), unname(sort(unlist(betas))))
    formulaMu <- mean ~ region * age
    y <- array(rpois(n = 40, lambda = 10),
               dim = c(5, 2, 4),
               dimnames = list(age = 0:4, sex = c("f", "m"), region = letters[1:4]))
    betas1 <- loglm(formulaMu, data = y)$param
    ans1 <- convertToFormulaOrder(betas = betas1, formulaMu = formulaMu)
    betas2 <- loglm(formulaMu, data = aperm(y, perm = c(3, 1, 2)))$param
    ans2 <- convertToFormulaOrder(betas = betas2, formulaMu = formulaMu)
    betas3 <- loglm(formulaMu, data = aperm(y, perm = c(2, 3, 1)))$param
    ans3 <- convertToFormulaOrder(betas = betas3, formulaMu = formulaMu)
    expect_equal(ans1, ans2)
    expect_equal(ans1, ans3)
    ## intercept only
    formulaMu <- mean ~ 1
    betas <- list("(Intercept)" = rnorm(1))
    ans <- convertToFormulaOrder(betas = betas, formulaMu = formulaMu)
    expect_identical(ans, betas)
})


## test_that("expectedValuesForNAs works", {
##     for (seed in seq_len(n.test)) {
##         set.seed(seed)
##         ## nicely behaved y
##         y <- Counts(array(rnorm(n = 160,
##                                 mean = outer(outer(1:10, c(-1, 1), "+"), rnorm(8), "+"),
##                                 sd = 0.1),
##                           dim = c(10, 2, 8),
##                           dimnames = list(age = 0:9, sex = c("f", "m"), region = 1:8)))
##         i.miss <- sample(x = length(y), size = round(0.4 * length(y)))
##         i.miss <- sort(i.miss)
##         y[i.miss] <- NA
##         ans.obtained <- expectedValuesForNAs(y)
##         d <- as.data.frame(y, direction = "long", midpoints = TRUE)
##         mod <- lm(count ~ ., data = d)
##         predicted <- predict(mod, newdata = d[-length(d)])
##         predicted <- predicted[i.miss]
##         ans.expected <- list(iMissing = i.miss, predicted = predicted)
##         expect_identical(ans.obtained, ans.expected)
##         ## y with all NAs for age 0
##         y <- Counts(array(rnorm(n = 160,
##                                 mean = outer(outer(1:10, c(-1, 1), "+"), rnorm(8), "+"),
##                                 sd = 0.1),
##                           dim = c(10, 2, 8),
##                           dimnames = list(age = 0:9, sex = c("f", "m"), region = 1:8)))
##         i.miss <- sample(x = length(y), size = round(0.4 * length(y)))
##         i.miss <- sort(i.miss)
##         y[i.miss] <- NA
##         y[1,,] <- NA
##         i.miss <- which(is.na(y))
##         ans.obtained <- expectedValuesForNAs(y)
##         d <- as.data.frame(y, direction = "long", midpoints = TRUE)
##         mod <- lm(count ~ ., data = d)
##         predicted <- predict(mod, newdata = d[-length(d)])
##         predicted <- predicted[i.miss]
##         ans.expected <- list(iMissing = i.miss, predicted = predicted)
##         expect_identical(ans.obtained, ans.expected)
##         expect_true(!any(is.na(ans.obtained$predicted)))
##         ## y with all NAs for region 1
##         y <- Counts(array(rnorm(n = 160,
##                                 mean = outer(outer(1:10, c(-1, 1), "+"), rnorm(8), "+"),
##                                 sd = 0.1),
##                           dim = c(10, 2, 8),
##                           dimnames = list(age = 0:9, sex = c("f", "m"), region = 1:8)))
##         i.miss <- sample(x = length(y), size = round(0.4 * length(y)))
##         i.miss <- sort(i.miss)
##         y[i.miss] <- NA
##         y[,,1] <- NA
##         i.miss <- which(is.na(y))
##         ans.obtained <- expectedValuesForNAs(y)
##         d <- as.data.frame(y, direction = "long", midpoints = TRUE)
##         d$region[d$region == "1"] <- "2"
##         mod <- lm(count ~ ., data = d, na.action = na.exclude)
##         predicted <- predict(mod, newdata = d[-length(d)])
##         predicted <- predicted[i.miss]
##         ans.expected <- list(iMissing = i.miss, predicted = predicted)
##         expect_identical(ans.obtained, ans.expected)
##         expect_true(!any(is.na(ans.obtained$predicted)))
##         ## y with only 10% of values
##         y <- Counts(array(rnorm(n = 160,
##                                 mean = outer(outer(1:10, c(-1, 1), "+"), rnorm(8), "+"),
##                                 sd = 0.1),
##                           dim = c(10, 2, 8),
##                           dimnames = list(age = 0:9, sex = c("f", "m"), region = 1:8)))
##         i.miss <- sample(x = length(y), size = round(0.9 * length(y)))
##         i.miss <- sort(i.miss)
##         y[i.miss] <- NA
##         y[,,1] <- NA
##         i.miss <- which(is.na(y))
##         ans.obtained <- expectedValuesForNAs(y)
##         expect_true(!any(is.na(ans.obtained$predicted)))
##         ## use log scale
##         ## y with all NAs for region 1
##         y <- Counts(array(exp(rnorm(n = 160,
##                                     mean = outer(outer(seq(-4, 5, 1), c(-1, 1), "+"), rnorm(8), "+"),
##                                     sd = 0.1)),
##                           dim = c(10, 2, 8),
##                           dimnames = list(age = 0:9, sex = c("f", "m"), region = 1:8)))
##         i.miss <- sample(x = length(y), size = round(0.4 * length(y)))
##         i.miss <- sort(i.miss)
##         y[i.miss] <- NA
##         y[,,1] <- NA
##         i.miss <- which(is.na(y))
##         ans.obtained <- expectedValuesForNAs(y, log = TRUE)
##         d <- as.data.frame(y, direction = "long", midpoints = TRUE)
##         d$count <- log(d$count)
##         d$region[d$region == "1"] <- "2"
##         mod <- lm(count ~ ., data = d, na.action = na.exclude)
##         predicted <- predict(mod, newdata = d[-length(d)])
##         predicted <- predicted[i.miss]
##         predicted <- exp(predicted)
##         ans.expected <- list(iMissing = i.miss, predicted = predicted)
##         expect_identical(ans.obtained, ans.expected)
##         expect_true(!any(is.na(ans.obtained$predicted)))
##     }
## })


test_that("formulaIsInterceptOnly works", {
    formulaIsInterceptOnly <- demest:::formulaIsInterceptOnly
    expect_true(formulaIsInterceptOnly(mean ~ 1))
    expect_true(formulaIsInterceptOnly(mean~1))
    expect_true(formulaIsInterceptOnly(mean~age - age))
    expect_false(formulaIsInterceptOnly(mean ~ age))
    expect_false(formulaIsInterceptOnly(mean~ age - 1))
})

test_that("imputeCountsInternal works", {
    imputeCountsInternal <- demest:::imputeCountsInternal
    ## no missing values, no subtotals
    x <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(region = c("a", "b", "c"),
                          sex = c("f", "m"))))
    expect_identical(imputeCountsInternal(x),
                     x)
    ## missing values, no subtotals
    x <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(region = c("a", "b", "c"),
                          sex = c("f", "m"))))
    x[1] <- NA
    expect_is(imputeCountsInternal(x), "Counts")
    expect_true(!any(is.na(imputeCountsInternal(x))))
    ## subtotals
    x <- Counts(array(1:6,
                      dim = c(3, 2),
                      dimnames = list(region = c("a", "b", "c"),
                          sex = c("f", "m"))))
    x[1,] <- NA
    subtotals <- CountsOne(values = 1, labels = "a", name = "region")
    x <- attachSubtotals(x, subtotals = subtotals)
    ans <- imputeCountsInternal(x)
    expect_is(ans, "CountsWithSubtotalsInternal")
    expect_true(!any(is.na(ans)))
})

test_that("initialDataModels works", {
    initialDataModels <- demest:::initialDataModels
    initialModel <- demest:::initialModel
    y <- Counts(array(1:24, dim = 2:4, dimnames = list(sex = c("f", "m"), reg = 1:3, age = 0:3)))
    data.models <- list(Model(reg.pop ~ Poisson(mean ~ age), age ~ Exch(error = Error(robust = TRUE))),
                        Model(est.pop ~ Poisson(mean ~ age + sex)),
                        Model(census ~ PoissonBinomial(prob = 0.9)))
    datasets <- list(reg.pop = Counts(array(1:12, dim = 3:4, dimnames = list(reg = 1:3, age = 0:3))),
                     est.pop = Counts(array(1:18, dim = c(2, 3, 3),
                     dimnames = list(sex = c("m", "f"), reg = 3:1, age = 0:2))),
                     census = y[,1,])
    transforms <- list(makeTransform(x = y, y = datasets[[1]], subset = TRUE),
                       makeTransform(x = y, y = datasets[[2]], subset = TRUE),
                       makeTransform(x = y, y = datasets[[3]], subset = TRUE))
    set.seed(100)
    ans.obtained <- initialDataModels(dataModels = data.models,
                                       datasets = datasets,
                                       y = y,
                                       transforms = transforms)
    set.seed(100)
    ans.expected <- list(initialModel(data.models[[1]],
                                      y = datasets[[1]],
                                      exposure = dembase::collapse(y, transforms[[1]])),
                         initialModel(data.models[[2]],
                                      y = datasets[[2]],
                                      exposure = dembase::collapse(y, transforms[[2]])),
                         initialModel(data.models[[3]],
                                      y = datasets[[3]],
                                      exposure = dembase::collapse(y, transforms[[3]])))
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})

test_that("jitterBetas works", {
    jitterBetas <- demest:::jitterBetas
    initialPrior <- demest:::initialPrior
    strucZeroArray <- CountsOne(0:9, labels = letters[1:10], name = "reg")
    for (i in seq_len(n.test)) {
        betas <- list(rnorm(1), rnorm(10))
        prior1 <- initialPrior(ExchFixed(),
                               beta = betas[[1]],
                               metadata = NULL,
                               sY = NULL,
                               isSaturated = FALSE,
                               margin = 0L,
                               strucZeroArray = strucZeroArray)
        prior2 <- initialPrior(Exch(),
                               beta = betas[[2]],
                               metadata = strucZeroArray@metadata,
                               sY = NULL,
                               isSaturated = TRUE,
                               margin = 1L,
                               strucZeroArray = strucZeroArray)
        priors <- list(prior1, prior2)
        betas.jittered <- jitterBetas(betas = betas, priorsBetas = priors)
        expect_true(betas.jittered[[1]] != 0)
        expect_true(is.na(betas.jittered[[2]][1]))
        expect_true(all(betas.jittered[[2]][-1] != 0))
    }
})

test_that("makeCellInLikHelper works", {
    makeCellInLikHelper <- demest:::makeCellInLikHelper
    makeCollapseTransformExtra <- dembase:::makeCollapseTransformExtra
    y <- Counts(array(rbinom(n = 20, size = 10, prob = 0.5),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    y[c(1, 20)] <- NA
    value <- Counts(array(c(0.1, 0.2, 0.3),
                          dim = 3,
                          dimnames = list(region = letters[1:3])))
    transform <- makeTransform(x = y, y = value, subset = TRUE)
    transform <- makeCollapseTransformExtra(transform)
    strucZeroArray <- Values(array(rep(c(0L, 1L), times = c(2, 18)),
                                   dim = c(5, 4),
                                   dimnames = list(age = 0:4, region = letters[1:4])))
    ans.obtained <- makeCellInLikHelper(transform = transform,
                                        y = y,
                                        strucZeroArray = strucZeroArray)
    ans.expected <- rep(c(FALSE, TRUE, FALSE), times = c(2, 17, 1))
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeCellInLikHelper works", {
    makeCellInLikHelper <- demest:::makeCellInLikHelper
    initialModel <- demest:::initialModel
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    y[c(1, 20)] <- NA
    value <- Counts(array(c(0.1, 0.2, 0.3),
                          dim = 3,
                          dimnames = list(region = letters[1:3])))
    aggregate <- AgCertain(value = value)
    spec <- Model(y ~ Poisson(mean ~ age + region),
                  aggregate = aggregate)
    x <- initialModel(spec, y = y, exposure = exposure)
    transform <- x@transformAg
    strucZeroArray <- Values(array(rep(c(0L, 1L), times = c(2, 18)),
                                   dim = c(5, 4),
                                   dimnames = list(age = 0:4, region = letters[1:4])))
    ans.obtained <- makeCellInLikHelper(transform = transform,
                                        y = y,
                                        strucZeroArray = strucZeroArray)
    ans.expected <- rep(c(FALSE, TRUE, FALSE), times = c(2, 17, 1))
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeAlphaMix works", {
    makeAlphaMix <- demest:::makeAlphaMix
    set.seed(1)
    dimBeta <- c(3:5, 20L)
    iAlong <- 4L
    nBetaNoAlongMix <- as.integer(prod(dimBeta[-iAlong]))
    posProdVectors1Mix <- as.integer(prod(dimBeta))
    posProdVectors2Mix <- as.integer(prod(dimBeta[-iAlong]))
    indexClassMaxMix <- new("Counter", 10L)
    prodVectorsMix <- new("ParameterVector",
                          rnorm(n = nBetaNoAlongMix * indexClassMaxMix@.Data))
    indexClassMix <- sample(indexClassMaxMix@.Data,
                            size = prod(dimBeta),
                            replace = TRUE)
    ans.obtained <- makeAlphaMix(prodVectorsMix = prodVectorsMix,
                                 indexClassMix = indexClassMix,
                                 indexClassMaxMix = indexClassMaxMix,
                                 nBetaNoAlongMix = nBetaNoAlongMix,
                                 posProdVectors1Mix = posProdVectors1Mix,
                                 posProdVectors2Mix = posProdVectors2Mix)
    ans.expected <- numeric(length(indexClassMix))
    pv <- matrix(prodVectorsMix@.Data, ncol = indexClassMaxMix@.Data)
    i.beta.no.along <- rep(1:60, times = 20)
    for (i in seq_along(ans.expected)) {
        ans.expected[i] <- pv[i.beta.no.along[i], indexClassMix[i]]
    }
    ans.expected <- new("ParameterVector", ans.expected)
    expect_identical(ans.obtained, ans.expected)
})
    
test_that("makeComponentWeightMix works", {
    makeComponentWeightMix <- demest:::makeComponentWeightMix
    dimBeta <- 4:6
    iAlong <- 3L
    indexClassMaxMix <- new("Counter", 10L)
    levelComponent <- new("ParameterVector",
                          rnorm(n = 60))
    omegaComponent <- new("Scale", 0.4)
    set.seed(1)
    ans.obtained <- makeComponentWeightMix(dimBeta = dimBeta,
                                           iAlong = iAlong,
                                           indexClassMaxMix = indexClassMaxMix,
                                           levelComponent = levelComponent,
                                           omegaComponent = omegaComponent)
    set.seed(1)
    ans.expected <- rnorm(n = 60,
                          mean = levelComponent@.Data,
                          sd = omegaComponent@.Data)
    ans.expected <- new("ParameterVector", ans.expected)
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})

test_that("makeCountsPred works", {
    makeCountsPred <- demest:::makeCountsPred
    initialModel <- demest:::initialModel
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(rpois(n = 20, lambda = 0.3 * exposure),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    y[,1] <- 0L
    structuralZeros = ValuesOne(c(0, 1, 1, 1), labels = letters[1:4], name = "region")
    spec <- Model(y ~ Poisson(mean ~ age + region, structuralZeros = structuralZeros))
    set.seed(1)
    x <- initialModel(spec, y = y, exposure = exposure)
    ## makeCountsPred designed for PoissonVaryingUseExpPredict, but should work with non-predict version
    ans.obtained <- makeCountsPred(x)
    ans.expected <- Counts(array(NA_integer_,
                                 dim = c(5, 4),
                                 dimnames = list(age = 0:4, region = letters[1:4])))
    ans.expected[,1] <- 0L
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeDims works with valid inputs", {
    makeDims <- demest:::makeDims
    dim <- 5:4
    margins <- list(0L, 1L, 2L, 1:2)
    expect_identical(makeDims(dim = dim, margins = margins),
                     list(0L, 5L, 4L, 5:4))
    dim <- 5:4
    margins <- list(0L, 2L, 2:1, 1L)
    expect_identical(makeDims(dim = dim, margins = margins),
                     list(0L, 4L, 4:5, 5L))
    dim <- 2:7
    margins <- list(0L, 4L, 5L, 6L, 4:5, c(4L, 6L), 5:6, 4:6)
    expect_identical(makeDims(dim = dim, margins = margins),
                     list(0L, 5L, 6L, 7L, 5:6, c(5L, 7L), 6:7, 5:7))
    dim <- 2:7
    margins <- list(0L)
    expect_identical(makeDims(dim = dim, margins = margins),
                     list(0L))
})

test_that("makeIAlong works with valid inputs", {
    makeIAlong <- demest:::makeIAlong
    ## metadata length 1, dimension specified
    metadata <- new("MetaData",
                    nms = "age",
                    dimtypes = "age",
                    DimScales = list(new("Intervals", dimvalues = c(0, 5, 10, Inf))))
    expect_identical(makeIAlong(along = NULL, metadata = metadata),
                     1L)
    ## metadata length 1, dimension specified
    metadata <- new("MetaData",
                    nms = "age",
                    dimtypes = "age",
                    DimScales = list(new("Intervals", dimvalues = c(0, 5, 10, Inf))))
    expect_identical(makeIAlong(along = "age", metadata = metadata),
                     1L)
    ## metadata length 2, dimension not specified
    metadata <- new("MetaData",
                    nms = c("age", "region"),
                    dimtypes = c("age", "state"),
                    DimScales = list(new("Intervals", dimvalues = c(0, 5, 10, Inf)),
                    new("Categories", dimvalues = c("a", "b"))))
    expect_identical(makeIAlong(along = NULL, metadata = metadata),
                     1L)
    ## metadata length 2, dimension specified
    metadata <- new("MetaData",
                    nms = c("age", "region"),
                    dimtypes = c("age", "state"),
                    DimScales = list(new("Intervals", dimvalues = c(0, 5, 10, Inf)),
                    new("Categories", dimvalues = c("a", "b"))))
    expect_identical(makeIAlong(along = "age", metadata = metadata),
                     1L)
})

test_that("makeIAlong throws appropriate errors", {
    makeIAlong <- demest:::makeIAlong
    ## no dimension specified, no continuous dimensions
    metadata <- new("MetaData",
                    nms = c("sex", "region"),
                    dimtypes = c("state", "state"),
                    DimScales = list(new("Categories", dimvalues = c("m", "f")),
                    new("Categories", dimvalues = c("a", "b"))))
    expect_error(makeIAlong(along = NULL, metadata = metadata),
                 sprintf("cannot use random walk prior when no dimensions have dimtype %s",
                         paste(dQuote(c("age", "cohort", "time")), collapse = ", ")))
    ## no dimension specified, more than one continuous dimensions
    metadata <- new("MetaData",
                    nms = c("age", "time"),
                    dimtypes = c("age", "time"),
                    DimScales = list(new("Intervals", dimvalues = c(0, 5, 10, Inf)),
                    new("Points", dimvalues = c(2000:2003))))
    expect_error(makeIAlong(along = NULL, metadata = metadata),
                 sprintf("more than one dimension with dimtype %s, but 'along' not specified",
                         paste(dQuote(c("age", "cohort", "time")), collapse = ", ")))
    ## invalid dimension specified
    metadata <- new("MetaData",
                    nms = c("age", "region"),
                    dimtypes = c("age", "state"),
                    DimScales = list(new("Intervals", dimvalues = c(0, 5, 10, Inf)),
                    new("Categories", dimvalues = c("a", "b"))))
    expect_error(makeIAlong(along = "wrong", metadata = metadata),
                 "'along' outside valid range")
    ## dimension not continuous
    metadata <- new("MetaData",
                    nms = c("age", "region"),
                    dimtypes = c("age", "state"),
                    DimScales = list(new("Intervals", dimvalues = c(0, 5, 10, Inf)),
                    new("Categories", dimvalues = c("a", "b"))))
    expect_error(makeIAlong(along = "region", metadata = metadata),
                 paste("cannot have random walk along dimension \"region\" because",
                       "dimension has dimtype \"state\""))
    ## steps not regular, name not specified
    metadata <- new("MetaData",
                    nms = c("age", "region"),
                    dimtypes = c("age", "state"),
                    DimScales = list(new("Intervals", dimvalues = c(0, 5, 9, Inf)),
                    new("Categories", dimvalues = c("a", "b"))))
    expect_error(makeIAlong(along = NULL, metadata = metadata),
                 "cannot have random walk along dimension \"age\" because steps irregular")
    ## steps not regular, name specified
    metadata <- new("MetaData",
                    nms = c("age", "region"),
                    dimtypes = c("age", "state"),
                    DimScales = list(new("Intervals", dimvalues = c(0, 5, 9, Inf)),
                    new("Categories", dimvalues = c("a", "b"))))
    expect_error(makeIAlong(along = "age", metadata = metadata),
                 "cannot have random walk along dimension \"age\" because steps irregular")
})

test_that("makeIndexClassMix works", {
    makeIndexClassMix <- demest:::makeIndexClassMix
    dimBeta <- 4:6
    iAlong <- 3L
    indexClassMaxMix <- new("Counter", 10L)
    weightMix <- new("UnitIntervalVec", runif(60))
    set.seed(1)
    ans.obtained <- makeIndexClassMix(dimBeta = dimBeta,
                                      iAlong = iAlong,
                                      indexClassMaxMix = indexClassMaxMix,
                                      weightMix = weightMix)
    set.seed(1)
    ans.expected <- array(dim = 4:6)
    wt <- matrix(weightMix@.Data, nrow = 6)
    wt[, 1] <- wt[, 1] + 0.01
    for (k in 1:6) {
        for (j in 1:5) {
            for (i in 1:4) {
                ans.expected[i,j,k] <- sample.int(n = 10, size = 1, prob = wt[k, ])
            }
        }
    }
    ans.expected <- as.integer(ans.expected)
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})

test_that("makeIteratorProdVectorMix works", {
    makeIteratorProdVectorMix <- demest:::makeIteratorProdVectorMix
    MarginIterator <- demest:::MarginIterator
    ans.obtained <- makeIteratorProdVectorMix(dimBeta = 3:5,
                                              iAlong = 1L)
    ans.expected <- MarginIterator(dim = c(1L, 4L, 5L))
    expect_identical(ans.obtained, ans.expected)
})

## CAN PROBABLY DELETE
## test_that("makeIMainEffects works with valid inputs", {
##     makeIMainEffects <- demest:::makeIMainEffects
##     margins <- list(0L, 1L, 2L, 1:2)
##     expect_identical(makeIMainEffects(margins),
##                      list(0L, 0L, 0L, 2:3))
##     margins <- list(0L, 2L, 2:1, 1L)
##     expect_identical(makeIMainEffects(margins),
##                      list(0L, 0L, c(2L, 4L), 0L))
##     margins <- list(0L, 4L, 5L, 6L, 4:5, c(4L, 6L), 5:6, 4:6)
##     expect_identical(makeIMainEffects(margins),
##                      list(0L, 0L, 0L, 0L, 2:3, c(2L, 4L), 3:4, 2:4))
##     margins <- list(0L)
##     expect_identical(makeIMainEffects(margins),
##                      list(0L))
## })

## CAN PROBABLY DELETE
## test_that("makeIteratorMargins works with valid inputs", {
##     makeIteratorMargins <- demest:::makeIteratorMargins
##     MarginIterator <- demest:::MarginIterator
##     dim <- 5:4
##     margins <- list(0L, 1L, 2L, 1:2)
##     expect_identical(y <- makeIteratorMargins(dim = dim, margins = margins),
##                      list(NULL, NULL, NULL, MarginIterator(5:4)))
##     dim <- 5:4
##     margins <- list(0L, 2L, 2:1, 1L)
##     expect_identical(makeIteratorMargins(dim = dim, margins = margins),
##                      list(NULL, NULL, MarginIterator(4:5), NULL))
##     dim <- 2:7
##     margins <- list(0L, 4L, 5L, 6L, 4:5, c(4L, 6L), 5:6, 4:6)
##     expect_identical(y <- makeIteratorMargins(dim = dim, margins = margins),
##                      list(NULL, NULL, NULL, NULL, MarginIterator(5:6), MarginIterator(c(5L, 7L)),
##                           MarginIterator(6:7), MarginIterator(5:7)))
##     dim <- 2:7
##     margins <- list(0L)
##     expect_identical(makeIteratorMargins(dim = dim, margins = margins),
##                      list(NULL))
## })

test_that("makeLatentComponentWeightMix works", {
    makeLatentComponentWeightMix <- demest:::makeLatentComponentWeightMix
    makeComponentWeightMix <- demest:::makeComponentWeightMix
    makeWeightMix <- demest:::makeWeightMix
    makeIndexClassMix <- demest:::makeIndexClassMix
    SliceIterator <- demest:::SliceIterator
    resetS <- demest:::resetS
    advanceS <- demest:::advanceS
    rtnorm1 <- demest:::rtnorm1
    dimBeta <- 4:6
    iAlong <- 2L
    indexClassMaxMix <- new("Counter", 10L)
    levelComponent <- new("ParameterVector", rnorm(50))
    componentWeightMix <- makeComponentWeightMix(dimBeta = dimBeta,
                                                 iAlong = iAlong,
                                                 indexClassMaxMix = indexClassMaxMix,
                                                 levelComponent = levelComponent,
                                                 omegaComponent = 0.4)
    weightMix <- makeWeightMix(dimBeta = dimBeta,
                               iAlong = iAlong,
                               indexClassMaxMix = indexClassMaxMix,
                               componentWeightMix = componentWeightMix)
    indexClassMix <- makeIndexClassMix(dimBeta = dimBeta,
                                       iAlong = iAlong,
                                       indexClassMaxMix = indexClassMaxMix,
                                       weightMix = weightMix)
    iteratorsDimsMix <- lapply(1:3, function(x) SliceIterator(dim = 4:6, x))
    set.seed(1)
    ans.obtained <-
        makeLatentComponentWeightMix(dimBeta = dimBeta,
                                     iAlong = iAlong,
                                     indexClassMix = indexClassMix,
                                     indexClassMaxMix = indexClassMaxMix,
                                     componentWeightMix = componentWeightMix,
                                     iteratorsDimsMix = iteratorsDimsMix)
    set.seed(1)
    cwm <- matrix(componentWeightMix@.Data,
                  nrow = 5)
    ans.expected <- matrix(nrow = 120, ncol = 10)
    iterator.beta <- iteratorsDimsMix[[2]]
    iterator.beta <- resetS(iterator.beta)
    for (i.along in 1:5) {
        indices.beta <- iterator.beta@indices
        for (i.beta in indices.beta) {
            k <- indexClassMix[i.beta]
            W <- cwm[i.along, k]
            for (j in 1:10) {
                if (j < k)
                    ans.expected[i.beta, j] <- rtnorm1(mean = W, sd = 1, lower = -Inf, upper = 0)
                else if (j == k)
                    ans.expected[i.beta, j] <- rtnorm1(mean = W, sd = 1, lower = 0, upper = Inf)
                else
                    ans.expected[i.beta, j] <- rtnorm1(mean = W, sd = 1, lower = -Inf, upper = Inf)
            }
        }
        iterator.beta <- advanceS(iterator.beta)
    }
    ans.expected <- new("ParameterVector", as.double(ans.expected))
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})

test_that("makeLatentWeightMix works", {
    makeLatentWeightMix <- demest:::makeLatentWeightMix
    makeComponentWeightMix <- demest:::makeComponentWeightMix
    makeWeightMix <- demest:::makeWeightMix
    makeIndexClassMix <- demest:::makeIndexClassMix
    SliceIterator <- demest:::SliceIterator
    resetS <- demest:::resetS
    advanceS <- demest:::advanceS
    dimBeta <- 4:6
    iAlong <- 2L
    indexClassMaxMix <- new("Counter", 10L)
    levelComponent <- new("ParameterVector", rnorm(50))
    componentWeightMix <- makeComponentWeightMix(dimBeta = dimBeta,
                                                 iAlong = iAlong,
                                                 indexClassMaxMix = indexClassMaxMix,
                                                 levelComponent = levelComponent,
                                                 omegaComponent = 0.4)
    weightMix <- makeWeightMix(dimBeta = dimBeta,
                               iAlong = iAlong,
                               indexClassMaxMix = indexClassMaxMix,
                               componentWeightMix = componentWeightMix)
    indexClassMix <- makeIndexClassMix(dimBeta = dimBeta,
                                       iAlong = iAlong,
                                       indexClassMaxMix = indexClassMaxMix,
                                       weightMix = weightMix)
    iteratorsDimsMix <- lapply(1:3, function(x) SliceIterator(dim = 4:6, x))
    set.seed(1)
    ans.obtained <-
        makeLatentWeightMix(dimBeta = dimBeta,
                            iAlong = iAlong,
                            iteratorsDimsMix = iteratorsDimsMix,
                            indexClassMix = indexClassMix,
                            indexClassMaxMix = indexClassMaxMix,
                            weightMix = weightMix)
    set.seed(1)
    w <- matrix(weightMix@.Data, nrow = 5)
    i.along <- as.integer(slice.index(array(dim = dimBeta), 2))
    v <- w[cbind(i.along, indexClassMix)]
    ans.expected <- numeric(120)
    for (i in 1:5)
        ans.expected[i.along == i] <- runif(n = 24, max = v[i.along == i])
    ans.expected <- new("UnitIntervalVec", ans.expected)
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})



test_that("makeLinearBetas leads to correct fitted values", {
    makeLinearBetas <- demest:::makeLinearBetas
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ## saturated model
        theta <- array(rnorm(24),
                       dim = 4:2,
                       dimnames = list(age = 0:3, region = c("a", "b", "c"), sex = c("f", "m")))
        formula <- mean ~ age * region * sex
        betas <- makeLinearBetas(theta = theta, formula = formula)
        betas <- lapply(betas, as.numeric)
        fitted.obtained <- betas[[1]] + betas[[2]] + rep(betas[[3]], each = 4) +
            rep(betas[[4]], each = 12) + betas[[5]] +
                c(rep(betas[[6]][1:4], times = 3), rep(betas[[6]][5:8], times = 3)) +
                    rep(betas[[7]], each = 4) + betas[[8]]
        fitted.expected <- as.numeric(theta)
        expect_equal(fitted.obtained, fitted.expected)
        ## unsaturated model
        theta <- array(rnorm(24),
                       dim = 4:2,
                       dimnames = list(age = 0:3, region = c("a", "b", "c"), sex = c("f", "m")))
        formula <- mean ~ age + region * sex
        betas <- makeLinearBetas(theta = theta, formula = formula)
        betas <- lapply(betas, as.numeric)
        fitted.obtained <- betas[[1]] + betas[[2]] + rep(betas[[3]], each = 4) +
            rep(betas[[4]], each = 12) + rep(betas[[5]], each = 4)
        fitted.expected <- fitted(lm(formula,
                                     data = data.frame(expand.grid(dimnames(theta)), mean = as.numeric(theta))))
        fitted.expected <- unname(fitted.expected)
        expect_equal(fitted.obtained, fitted.expected)
        ## intercept only
        theta <- array(rnorm(24),
                       dim = 4:2,
                       dimnames = list(age = 0:3, region = c("a", "b", "c"), sex = c("f", "m")))
        formula <- mean ~ 1
        betas <- makeLinearBetas(theta = theta, formula = formula)
        betas <- lapply(betas, as.numeric)
        fitted.obtained <- rep(betas[[1]], times = length(theta))
        fitted.expected <- fitted(lm(formula,
                                     data = data.frame(expand.grid(dimnames(theta)), mean = as.numeric(theta))))
        fitted.expected <- unname(fitted.expected)
        expect_equal(fitted.obtained, fitted.expected)
    }
})

test_that("makeLinearBetas returns terms in expected order", {
    makeLinearBetas <- demest:::makeLinearBetas
    theta <- array(rnorm(24),
                   dim = 4:2,
                   dimnames = list(age = 0:3, region = c("a", "b", "c"), sex = c("f", "m")))
    ans <- makeLinearBetas(theta = theta, formula = mean ~ sex + age + region)
    expect_identical(names(ans), c("(Intercept)", "sex", "age", "region"))
    ans <- makeLinearBetas(theta = theta, formula = mean ~ region*age + sex)
    expect_identical(names(ans), c("(Intercept)", "region", "age", "sex", "region:age"))
    ans <- makeLinearBetas(theta = theta, formula = mean ~ region*age + sex*age)
    expect_identical(names(ans), c("(Intercept)", "region", "age", "sex", "region:age", "age:sex"))
})

test_that("makeLevelComponentWeightMix works", {
    makeLevelComponentWeightMix <- demest:::makeLevelComponentWeightMix
    dimBeta <- 4:6
    iAlong <- 3L
    indexClassMaxMix <- new("Counter", 10L)
    phiMix <- 0.7
    meanLevel <- 0.4
    omegaLevel <- 0.1
    set.seed(1)
    ans.obtained <- makeLevelComponentWeightMix(dimBeta = dimBeta,
                                                iAlong = iAlong,
                                                indexClassMaxMix = indexClassMaxMix,
                                                phiMix = phiMix,
                                                meanLevel = meanLevel,
                                                omegaLevel = omegaLevel)
    set.seed(1)
    ans.expected <- matrix(nrow = 6, ncol = 10)
    ans.expected[1,] <- rnorm(n = 10,
                              mean = meanLevel / (1-phiMix),
                              sd = omegaLevel / sqrt(1-phiMix^2))
    for (i in 2:6)
        ans.expected[i,] <- rnorm(n = 10,
                                  mean = meanLevel + phiMix * ans.expected[i-1,],
                                  sd = omegaLevel)
    ans.expected <- new("ParameterVector", as.double(ans.expected))
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})

test_that("makeMargins works with valid input", {
    makeMargins <- demest:::makeMargins
    betas <- list("(Intercept)" = rnorm(1),
                  age = rnorm(10),
                  sex = rnorm(2),
                  region = rnorm(5),
                  "age:sex" = array(rnorm(20),
                  dim = c(10, 2),
                  dimnames = list(age = 0:9, sex = c("f", "m"))),
                  "age:region" = array(rnorm(50),
                  dim = c(10, 5),
                  dimnames = list(age = 0:9, region = letters[1:5])),
                  "sex:region" = array(rnorm(10),
                  dim = c(2, 5),
                  dimnames = list(sex = c("f", "m"), region = letters[1:5])),
                  "age:sex:region" = array(rnorm(100),
                  dim = c(10, 2, 5),
                  dimnames = list(age = 0:9, sex = c("f", "m"), region = letters[1:5])))
    y <- Counts(array(rpois(n = 100, lambda = 10),
                      dim = c(10, 2, 5),
                      dimnames = list(age = 0:9, sex = c("f", "m"), region = letters[1:5])))
    ans.obtained <- makeMargins(beta = betas, y = y)
    ans.expected <- list(0L, 1L, 2L, 3L, 1:2, c(1L, 3L), 2:3, 1:3)
    expect_identical(ans.obtained, ans.expected)
    betas <- list("(Intercept)" = rnorm(1),
                  age = rnorm(10),
                  sex = rnorm(2),
                  "age:sex" = array(rnorm(20), dim = c(10, 2), dimnames = list(age = 0:9, sex = c("f", "m"))))
    y <- Counts(array(rpois(n = 20, lambda = 10),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    ans.obtained <- makeMargins(beta = betas, y = y)
    ans.expected <- list(0L, 2L, 1L, 2:1)
    expect_identical(ans.obtained, ans.expected)
    ## intercept only
    betas <- list("(Intercept)" = rnorm(1))
    y <- Counts(array(rpois(n = 20, lambda = 10),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    ans.obtained <- makeMargins(beta = betas, y = y)
    ans.expected <- list(0L)
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeMeanLevelComponentWeightMix works", {
    makeMeanLevelComponentWeightMix <- demest:::makeMeanLevelComponentWeightMix
    priorMean <- new("Parameter", 0.4)
    priorSD <- new("Scale", 1.1)
    set.seed(1)
    ans.obtained <- makeMeanLevelComponentWeightMix(priorMean = priorMean,
                                                    priorSD = priorSD)
    set.seed(1)
    ans.expected <- rnorm(n = 1,
                          mean = priorMean@.Data,
                          sd = priorSD@.Data)
    ans.expected <- new("Parameter", ans.expected)
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})

test_that("makeNamesSpecsPriors works", {
    makeNamesSpecsPriors <- demest:::makeNamesSpecsPriors
    dots <- list(age ~ DLM(), sex ~ ExchFixed(), region ~ Exch())
    ans.obtained <- makeNamesSpecsPriors(dots)
    ans.expected <- c("age", "sex", "region")
    expect_identical(ans.obtained, ans.expected)
    dots <- list()
    ans.obtained <- makeNamesSpecsPriors(dots)
    ans.expected <- character()
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeProdVectorsMix works", {
    makeProdVectorsMix <- demest:::makeProdVectorsMix
    set.seed(1)
    vectorsMix <- list(new("ParameterVector",
                           rnorm(n = 30,
                                 sd = 0.2)),
                       new("ParameterVector", numeric()),
                       new("ParameterVector",
                           rnorm(n = 50,
                                 sd = 0.1)))
    ans.obtained <- makeProdVectorsMix(vectorsMix = vectorsMix,
                                       iAlong = 2L,
                                       dimBeta = c(3L, 10L, 5L),
                                       indexClassMaxMix = new("Counter", 10L))
    vec1 <- matrix(vectorsMix[[1]]@.Data, nr = 3)
    vec3 <- matrix(vectorsMix[[3]]@.Data, nr = 5)
    ans.expected <- vector(mode = "list", length = 10)
    for (i in 1:10)
        ans.expected[[i]] <- outer(vec1[,i], vec3[,i])
    ans.expected <- as.double(unlist(ans.expected))
    ans.expected <- new("ParameterVector", ans.expected)
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})

test_that("makePriors works when given valid inputs", {
    makePriors <- demest:::makePriors
    initialPrior <- demest:::initialPrior
    ## main effects
    betas <- list("(Intercept)" = rnorm(1),
                  age = rnorm(5),
                  region = rnorm(4))
    specs <- list(DLM(trend = NULL))
    namesSpecs <- "age"
    margins <- list(0L, 1L, 2L)
    y <- Counts(array(rpois(n = 20, lambda = 20),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    strucZeroArray <- Counts(array(rep(c(0L, 1L), times = c(5, 15)),
                                   dim = c(5, 4),
                                   dimnames = list(age = 0:4, region = letters[1:4])))
    sY <- sd(y)
    set.seed(1)
    ans.obtained <- makePriors(betas = betas,
                               specs = specs,
                               namesSpecs = namesSpecs,
                               margins = margins,
                               y = y,
                               sY = sY,
                               strucZeroArray = strucZeroArray)
    set.seed(1)
    ans.expected <- list(initialPrior(ExchFixed(),
                                      beta = betas[[1]],
                                      metadata = NULL,
                                      sY = sY,
                                      isSaturated = FALSE,
                                      margin = 0L,
                                      strucZeroArray = strucZeroArray),
                         initialPrior(DLM(trend = NULL),
                                      beta = betas[[2]],
                                      metadata = y@metadata[1],
                                      sY = sY,
                                      isSaturated = FALSE,
                                      margin = 1L,
                                      strucZeroArray = strucZeroArray),
                         initialPrior(Exch(),
                                      beta = betas[[3]],
                                      metadata = y@metadata[2],
                                      sY = sY,
                                      isSaturated = FALSE,
                                      margin = 2L,
                                      strucZeroArray = strucZeroArray))
    expect_identical(ans.obtained, ans.expected)
    ## main effects, order of formula different from y
    betas <- list("(Intercept)" = rnorm(1),
                  region = rnorm(4),
                  age = rnorm(5))
    specs <- list(DLM())
    namesSpecs <- "age"
    margins <- list(0L, 2L, 1L)
    y <- Counts(array(rpois(n = 20, lambda = 20),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    strucZeroArray <- Counts(array(rep(c(0L, 1L), times = c(5, 15)),
                                   dim = c(5, 4),
                                   dimnames = list(age = 0:4, region = letters[1:4])))
    set.seed(1)
    ans.obtained <- makePriors(betas = betas,
                               specs = specs,
                               namesSpecs = namesSpecs,
                               margins = margins,
                               y = y,
                               sY = NULL,
                               strucZeroArray = strucZeroArray)
    set.seed(1)
    ans.expected <- list(initialPrior(ExchFixed(),
                                      beta = betas[[1]],
                                      metadata = NULL,
                                      sY = NULL,
                                      isSaturated = FALSE,
                                      margin = 0L,
                                      strucZeroArray = strucZeroArray),
                         initialPrior(Exch(),
                                      beta = betas[[2]],
                                      metadata = y@metadata[2],
                                      sY = NULL,
                                      isSaturated = FALSE,
                                      margin = 2L,
                                      strucZeroArray = strucZeroArray),
                         initialPrior(DLM(),
                                      beta = betas[[3]],
                                      metadata = y@metadata[1],
                                      sY = NULL,
                                      isSaturated = FALSE,
                                      margin = 1L,
                                      strucZeroArray = strucZeroArray))
    expect_identical(ans.obtained, ans.expected)
    ## intercept only
    betas <- list("(Intercept)" = rnorm(1))
    specs <- list()
    margins <- list(0L)
    y <- Counts(array(rpois(n = 20, lambda = 20),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    strucZeroArray <- Counts(array(rep(c(0L, 1L), times = c(5, 15)),
                                   dim = c(5, 4),
                                   dimnames = list(age = 0:4, region = letters[1:4])))
    sY <- sd(y)
    set.seed(1)
    ans.obtained <- makePriors(betas = betas,
                               specs = specs,
                               namesSpecs = namesSpecs,
                               margins = margins,
                               y = y,
                               sY = sY,
                               strucZeroArray = strucZeroArray)
    set.seed(1)
    ans.expected <- list(initialPrior(ExchFixed(),
                                      beta = betas[[1]],
                                      metadata = NULL,
                                      sY = sY,
                                      isSaturated = FALSE,
                                      margin = 0L,
                                      strucZeroArray = strucZeroArray))
    expect_identical(ans.obtained, ans.expected)
    ## single dimension only
    betas <- list("(Intercept)" = rnorm(1),
                  region = rnorm(4))
    margins <- list(0L, 2L)
    specs <- list()
    namesSpecs <- character()
    y <- Counts(array(rpois(n = 20, lambda = 20),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    strucZeroArray <- Counts(array(rep(c(0L, 1L), times = c(5, 15)),
                                   dim = c(5, 4),
                                   dimnames = list(age = 0:4, region = letters[1:4])))
    set.seed(1)
    ans.obtained <- makePriors(betas = betas,
                               specs = specs,
                               namesSpecs = namesSpecs,
                               margins = margins,
                               y = y,
                               sY = NULL,
                               strucZeroArray = strucZeroArray)
    set.seed(1)
    ans.expected <- list(initialPrior(ExchFixed(),
                                      beta = betas[[1]],
                                      metadata = NULL,
                                      sY = NULL,
                                      isSaturated = FALSE,
                                      margin = 0L,
                                      strucZeroArray = strucZeroArray),
                         initialPrior(Exch(),
                                      beta = betas[[2]],
                                      metadata = y@metadata[2],
                                      sY = NULL,
                                      isSaturated = FALSE,
                                      margin = 2L,
                                      strucZeroArray = strucZeroArray))
    expect_identical(ans.obtained, ans.expected)
    ## orig-dest dimensions
    betas <- list("(Intercept)" = rnorm(1),
                  age = rnorm(5),
                  region_orig= rnorm(4),
                  region_dest = rnorm(4),
                  "region_orig:region_dest" = array(rnorm(16),
                      dim = c(4, 4),
                      dimnames = list(region_orig = letters[1:4], region_dest = letters[1:4])))
    specs <- list(DLM(trend = NULL))
    namesSpecs <- "age"
    margins <- list(0L, 1L, 2L, 3L, 2:3)
    y <- Counts(array(rpois(n = 80, lambda = 20),
                      dim = c(5, 4, 4),
                      dimnames = list(age = 0:4, region_orig = letters[1:4],
                          region_dest = letters[1:4])))
    strucZeroArray <- Counts(array(rep(as.integer(1 - diag(4)), times = 5),
                                   dim = c(5, 4, 4),
                                   dimnames = list(age = 0:4, region_orig = letters[1:4],
                                                   region_dest = letters[1:4])))
    set.seed(1)
    ans.obtained <- makePriors(betas = betas,
                               specs = specs,
                               namesSpecs = namesSpecs,
                               margins = margins,
                               y = y,
                               sY = NULL,
                               strucZeroArray = strucZeroArray)
    set.seed(1)
    metadata.reg.main <- new("MetaData",
                             nms = "region",
                             dimtypes = "state",
                             DimScales = list(new("Categories", dimvalues = letters[1:4])))
    metadata.reg.inter <- new("MetaData",
                              nms = c("region_orig", "region_dest"),
                              dimtypes = c("origin", "destination"),
                              DimScales = list(new("Categories", dimvalues = letters[1:4]),
                                  new("Categories", dimvalues = letters[1:4])))
    ans.expected <- list(initialPrior(ExchFixed(),
                                      beta = betas[[1]],
                                      metadata = NULL,
                                      sY = NULL,
                                      isSaturated = FALSE,
                                      margin = 0L,
                                      strucZeroArray = strucZeroArray),
                         initialPrior(DLM(trend = NULL),
                                      beta = betas[[2]],
                                      metadata = y@metadata[1],
                                      sY = NULL,
                                      isSaturated = FALSE,
                                      margin = 1L,
                                      strucZeroArray = strucZeroArray),
                         initialPrior(Exch(),
                                      beta = betas[[3]],
                                      metadata = metadata.reg.main,
                                      sY = NULL,
                                      isSaturated = FALSE,
                                      margin = 2L,
                                      strucZeroArray = strucZeroArray),
                         initialPrior(Exch(),
                                      beta = betas[[4]],
                                      metadata = metadata.reg.main,
                                      sY = NULL,
                                      isSaturated = FALSE,
                                      margin = 3L,
                                      strucZeroArray = strucZeroArray),
                         initialPrior(Exch(),
                                      beta = betas[[5]],
                                      metadata = metadata.reg.inter,
                                      sY = NULL,
                                      isSaturated = FALSE,
                                      margin = 2:3,
                                      strucZeroArray = strucZeroArray))
    expect_identical(ans.obtained, ans.expected)
    ## saturated model
    betas <- list("(Intercept)" = rnorm(1),
                  age = rnorm(5),
                  region = rnorm(4),
                  "age:region" = rnorm(20))
    specs <- list(DLM(trend = NULL))
    namesSpecs <- "age"
    margins <- list(0L, 1L, 2L, 1:2)
    y <- Counts(array(rpois(n = 20, lambda = 20),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    sY <- sd(y)
    set.seed(1)
    ans.obtained <- makePriors(betas = betas,
                               specs = specs,
                               namesSpecs = namesSpecs,
                               margins = margins,
                               y = y,
                               sY = sY,
                               strucZeroArray = strucZeroArray)
    set.seed(1)
    ans.expected <- list(initialPrior(ExchFixed(),
                                      beta = betas[[1]],
                                      metadata = NULL,
                                      sY = sY,
                                      isSaturated = FALSE,
                                      margin = 0L,
                                      strucZeroArray = strucZeroArray),
                         initialPrior(DLM(trend = NULL),
                                      beta = betas[[2]],
                                      metadata = y@metadata[1],
                                      sY = sY,
                                      isSaturated = FALSE,
                                      margin = 1L,
                                      strucZeroArray = strucZeroArray),
                         initialPrior(Exch(),
                                      beta = betas[[3]],
                                      metadata = y@metadata[2],
                                      sY = sY,
                                      isSaturated = FALSE,
                                      margin = 2L,
                                      strucZeroArray = strucZeroArray),
                         initialPrior(Exch(),
                                      beta = betas[[4]],
                                      metadata = y@metadata,
                                      sY = sY,
                                      isSaturated = TRUE,
                                      margin = 1:2,
                                      strucZeroArray = strucZeroArray))
    expect_identical(ans.obtained, ans.expected)
})

test_that("makePriors throws appropriate errors", {
    makePriors <- demest:::makePriors
    ## 'beta' has no missing values
    betas <- list("(Intercept)" = rnorm(1),
                  age = c(rnorm(4), NA),
                  region = rnorm(4))
    specs <- list()
    namesSpecs <- character()
    margins <- list(0L, 1L, 2L)
    y <- Counts(array(rpois(n = 20, lambda = 20),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(5, 4),
                                   dimnames = list(age = 0:4, region = letters[1:4])))
    expect_error(makePriors(betas = betas,
                            specs = specs,
                            namesSpecs = namesSpecs,
                            margins = margins,
                            y = y,
                            sY = NULL,
                            strucZeroArray = strucZeroArray),
                 "'beta' for \"age\" has missing values")
})

test_that("makeSpecsPriors works", {
    makeSpecsPriors <- demest:::makeSpecsPriors
    dots <- list(age ~ DLM(), sex ~ ExchFixed(), region ~ Exch())
    ans.obtained <- makeSpecsPriors(dots)
    ans.expected <- list(DLM(), ExchFixed(), Exch())
    expect_identical(ans.obtained, ans.expected)
    expect_error(makeSpecsPriors(list(age = 1)),
                 "'1' is not a formula")
    expect_error(makeSpecsPriors(list(~ Exch())),
                 "formula '~Exch\\(\\)' does not include a response")
})

test_that("makeTransformsYToDatasets works", {
    makeTransformsYToDatasets <- demest:::makeTransformsYToDatasets
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    y <- Counts(array(1:60, dim = 3:5, dimnames = list(reg = 1:3, age = 0:3, time = 2000:2004)),
                dimscales = c(time = "Intervals"))
    datasets <- list(Counts(array(1:45, dim = c(3, 3, 5),
                                  dimnames = list(reg = 1:3, age = 1:3, time = 2000:2004)),
                            dimscales = c(time = "Intervals")),
                     Counts(array(1:12, dim = c(3, 4, 1),
                                  dimnames = list(reg = 1:3, age = 0:3, time = 2000)),
                            dimscales = c(time = "Intervals")))
    concordances <- list(census = list(reg = Concordance(data.frame(from = 1:3, to = c(1, 4, 4)))))
    datasets[[2]] <- collapseCategories(datasets[[2]], dimension = "reg", old = 2:3, new = 4)
    ans.obtained <- makeTransformsYToDatasets(y = y, datasets = datasets,
                                              concordances = concordances,
                                              namesDatasets = c("tax", "census"))
    ans.expected <- list(makeTransform(x = y, y = datasets[[1]], subset = TRUE),
                         makeTransform(x = y, y = datasets[[2]], concordances = concordances[[1]],
                                       subset = TRUE))
    ans.expected <- lapply(ans.expected, makeCollapseTransformExtra)
    expect_identical(ans.obtained, ans.expected)
    datasets.wrong <- c(datasets,
                        list(Counts(array(1:6, dim = 6, dimnames = list(time = 2000:2005)),
                                    dimscales = c(time = "Intervals"))))
    expect_error(makeTransformsYToDatasets(y = y,
                                           datasets = datasets.wrong,
                                           concordances = concordances,
                                           namesDatasets = c("tax", "census", "wrong")),
                 "unable to collapse 'y' to make it compatible with dataset 'wrong' :")
})

test_that("makeValueAndMetaDataAg works", {
    makeValueAndMetaDataAg <- demest:::makeValueAndMetaDataAg
    value <- Counts(array(1:4,
                          dim = 4,
                          dimnames = list(region = 1:4)))
    ans.obtained <- makeValueAndMetaDataAg(value)
    ans.expected <- list(value = new("ParameterVector", as.double(1:4)),
                         metadata = value@metadata)
    expect_identical(ans.obtained, ans.expected)
    value <- 3L
    ans.obtained <- makeValueAndMetaDataAg(value)
    ans.expected <- list(value = new("ParameterVector", 3),
                         metadata = NULL)
    expect_identical(ans.obtained, ans.expected)
    value <- Values(array(3L,
                          dim = 1L,
                          dimnames = list(reg = "a")))
    ans.obtained <- makeValueAndMetaDataAg(value)
    ans.expected <- list(value = new("ParameterVector", 3),
                         metadata = value@metadata)
    expect_identical(ans.obtained, ans.expected)
    value <- Counts(array(c(NA, 1:3),
                         dim = 4,
                         dimnames = list(region = 1:4)))
    expect_error(makeValueAndMetaDataAg(value),
                 "'value' has missing values")
    expect_error(makeValueAndMetaDataAg(1:2),
                 "'value' does not have length 1")
    expect_error(makeValueAndMetaDataAg(as.numeric(NA)),
                 "'value' is missing")
    expect_error(makeValueAndMetaDataAg("a"),
                 "'value' has class \"character\"")
})

test_that("makeVectorsMix works", {
    makeVectorsMix <- demest:::makeVectorsMix
    dimBeta <- 3:5
    iAlong <- 2L
    indexClassMaxMix <- new("Counter", 10L)
    omegaVectorsMix <- new("Scale", 0.2)
    set.seed(1)
    ans.obtained <- makeVectorsMix(dimBeta = dimBeta,
                                   iAlong = iAlong,
                                   indexClassMaxMix = indexClassMaxMix,
                                   omegaVectorsMix = omegaVectorsMix)
    set.seed(1)
    ans.expected <- replicate(n = 3, numeric(), simplify = FALSE)
    ans.expected[[1]] <- new("ParameterVector",
                             rnorm(n = 30,
                                   sd = 0.2))
    ans.expected[[2]] <- new("ParameterVector", ans.expected[[2]])
    ans.expected[[3]] <- new("ParameterVector",
                             rnorm(n = 50,
                                   sd = 0.2))
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})

test_that("makeWeightAg works", {
    makeWeightAg <- demest:::makeWeightAg
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    exposure <- Counts(array(rpois(n = 20, lambda = 20),
                             dim = c(5, 4),
                             dimnames = list(age = 0:4, region = letters[1:4])))
    thetaObj <- Counts(array(1,
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    weights <- Counts(array(1,
                            dim = c(5, 4),
                            dimnames = list(age = 0:4, region = letters[1:4])))
    model <- new("PoissonVaryingUseExp")
    ## weights supplied, aggregate scalar
    values <- 0.4
    transform <- makeTransform(x = thetaObj, y = values)
    transform <- makeCollapseTransformExtra(transform)
    ans.obtained <- makeWeightAg(weight = weights,
                                 default = exposure,
                                 model = model,
                                 thetaObj = thetaObj,
                                 transform = transform,
                                 values = values)
    ans.expected <- as.double(rep(1, 20))
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
    ## weights supplied, aggregate scalar - no exposure
    values <- 0.4
    transform <- makeTransform(x = thetaObj, y = values)
    transform <- makeCollapseTransformExtra(transform)
    ans.obtained <- makeWeightAg(weight = weights,
                                 default = exposure,
                                 model = new("PoissonVaryingNotUseExp"),
                                 thetaObj = thetaObj,
                                 transform = transform,
                                 values = values)
    ans.expected <- as.double(rep(1, 20))
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
    ## weights not supplied, aggregate scalar
    values <- 0.4
    transform <- makeTransform(x = thetaObj, y = values)
    transform <- makeCollapseTransformExtra(transform)
    ans.obtained <- makeWeightAg(weight = NULL,
                                 default = exposure,
                                 model = model,
                                 thetaObj = thetaObj,
                                 transform = transform,
                                 values = values)
    ans.expected <- as.double(exposure)/sum(exposure)
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
    ## weights not supplied, aggregate vector and cover all cells
    values <- (collapseDimension(thetaObj, dim = "age")
               / collapseDimension(exposure, dim = "age"))
    transform <- makeTransform(x = thetaObj, y = values)
    transform <- makeCollapseTransformExtra(transform)
    ans.obtained <- makeWeightAg(weight = NULL,
                                 default = exposure,
                                 model = model,
                                 thetaObj = thetaObj,
                                 transform = transform,
                                 values = values)
    ans.expected <- apply(as(exposure, "array"), 2, prop.table)
    ans.expected <- as.double(ans.expected)
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
    ## weights not supplied, aggregate vector and do not cover all cells
    y1 <- subarray(thetaObj, age > 1)
    exposure1 <- subarray(exposure, age > 1)
    values <- (collapseDimension(y1, dim = "region")
               / collapseDimension(exposure1, dim = "region"))
    transform <- makeTransform(x = thetaObj, y = values, subset = TRUE)
    transform <- makeCollapseTransformExtra(transform)
    ans.obtained <- makeWeightAg(weight = NULL,
                                 default = exposure,
                                 model = model,
                                 thetaObj = thetaObj,
                                 transform = transform,
                                 values = values)
    ans.expected <- t(apply(as(exposure[-1,], "array"), 1, prop.table))
    ans.expected <- rbind(rep(NA, 4), ans.expected)
    ans.expected <- as.double(ans.expected)
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
    ## error if supplied weights has NA for value needed by aggregate
    weights.na <- weights
    weights.na[1] <- NA
    values <- 0.4
    transform <- makeTransform(x = thetaObj, y = values)
    transform <- makeCollapseTransformExtra(transform)
    expect_error(makeWeightAg(weight = weights.na,
                              default = exposure,
                              model = model,
                              thetaObj = thetaObj,
                              transform = transform,
                              values = values),
                 "element 1 of 'weightAg' is needed for aggregate values but is missing")
    ## error if no weights supplied and no default
    expect_error(makeWeightAg(weight = NULL,
                              default = NULL,
                              model = model,
                              thetaObj = thetaObj,
                              transform = transform,
                              values = values),
                 "no aggregate weights supplied, and no default weights")    
})

test_that("makeWeightMix works", {
    makeWeightMix <- demest:::makeWeightMix
    dimBeta <- 4:6
    iAlong <- 3L
    indexClassMaxMix <- new("Counter", 10L)
    componentWeightMix <- new("ParameterVector",
                              rnorm(n = 60))
    ans.obtained <- makeWeightMix(dimBeta = dimBeta,
                                  iAlong = iAlong,
                                  indexClassMaxMix = indexClassMaxMix,
                                  componentWeightMix = componentWeightMix)
    ans.expected <- apply(matrix(componentWeightMix@.Data, nrow = 6),
                          1,
                          function(x) {
                              n <- length(x)
                              ans <- pnorm(x)
                              m <- cumprod(1 - ans)
                              ans[-1] <- ans[-1] * m[-n]
                              ans
                          })
    ans.expected <- t(ans.expected)
    ans.expected <- new("UnitIntervalVec", ans.expected)
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})

test_that("makeSigmaInitialPoisson works with valid inputs", {
    makeSigmaInitialPoisson <- demest:::makeSigmaInitialPoisson
    ## no exposure
    y <- rpois(n = 20, lambda = 3)
    ans.obtained <- makeSigmaInitialPoisson(y)
    ans.expected <- sd(log(0.5 * mean(y) + 0.5 * y))
    expect_equal(ans.obtained, ans.expected)
    ## exposure
    exposure <- abs(rnorm(n = 20, mean = 5)) + 0.01
    y <- abs(rnorm(n = 20, mean = 10))
    ans.obtained <- makeSigmaInitialPoisson(y = y, exposure = exposure)
    ans.expected <- sd(log(0.5 * (y/exposure) + 0.5 * (mean(y/exposure))))
    expect_equal(ans.obtained, ans.expected)
    ## single value
    y <- 3
    ans.obtained <- makeSigmaInitialPoisson(y)
    ans.expected <- 1.0
    expect_equal(ans.obtained, ans.expected)
    ## minimum value
    y <- c(1, 1)
    ans.obtained <- makeSigmaInitialPoisson(y)
    ans.expected <- 0.01
    expect_equal(ans.obtained, ans.expected)
    ## has missing - no exposure
    y <- rpois(n = 20, lambda = 3)
    y[c(3, 5)] <- NA
    ans.obtained <- makeSigmaInitialPoisson(y)
    ans.expected <- sd(log(0.5 * mean(y[-c(3, 5)]) + 0.5 * y[-c(3, 5)]))
    expect_equal(ans.obtained, ans.expected)
    ## has missing - with exposure
    exposure <- abs(rnorm(n = 20, mean = 5)) + 0.01
    y <- abs(rnorm(n = 20, mean = 10))
    exposure[1] <- NA
    y[1:2] <- NA
    ans.obtained <- makeSigmaInitialPoisson(y = y, exposure = exposure)
    ans.expected <- sd(log(0.5 * (y[-(1:2)]/exposure[-(1:2)]) +
                               0.5 * (mean(y[-(1:2)]/exposure[-(1:2)]))))
    expect_equal(ans.obtained, ans.expected)
})

test_that("makeSigmaInitialPoisson throws appropriate errors", {
    makeSigmaInitialPoisson <- demest:::makeSigmaInitialPoisson
    ## 'y' has at least one non-missing value
    expect_error(makeSigmaInitialPoisson(rep(NA, 5)),
                 "'y' has no non-missing values")
    ## 'y' is finite
    expect_error(makeSigmaInitialPoisson(c(Inf, 1:3)),
                 "'y' has non-finite values")
    ## 'y' not all 0
    expect_error(makeSigmaInitialPoisson(rep(0, 10)),
                 "'y' has no non-zero values")
    ## 'exposure' has no missing values
    expect_error(makeSigmaInitialPoisson(y = 1:5, exposure = c(NA, 1:4)),
                 "'exposure' has missing values")
    ## 'exposure' is finite
    expect_error(makeSigmaInitialPoisson(y = 1:5, exposure = c(-Inf, 1:4)),
                 "'exposure' has non-finite values")
    ## 'exposure' not all 0
    expect_error(makeSigmaInitialPoisson(y = 1:5, exposure = rep(0, 5)),
                 "'exposure' has no non-zero values")
})

