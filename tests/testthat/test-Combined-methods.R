
context("Combined-methods")

n.test <- 5
test.identity <- FALSE
test.extended <- FALSE

## getSeriesForDataset #############################################################

## test_that("getSeriesForDataset works with CombinedModelPoissonNotHasExp", {
##     getSeriesForDataset <- demest:::getSeriesForDataset
##     initialCombinedCounts <- demest:::initialCombinedCounts
##     makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
##     updateCombined <- demest:::updateCombined
##     set.seed(100)
##     y <- Counts(array(c(1:11, 20L),
##                       dim = c(6, 2),
##                       dimnames = list(age = 0:5, sex = c("f", "m"))))
##     model <- Model(y ~ Poisson(mean ~ age, useExpose = FALSE))
##     data.models <- list(Model(register ~ PoissonBinomial(prob = 0.98)),
##                               Model(tax ~ Binomial(mean ~ 1)))
##     datasets <- list(Counts(array(c(0L, 2:12), dim = c(6, 2),
##                                   dimnames = list(age = 0:5, sex = c("f", "m")))),
##                          Counts(array(c(1:5, NA), dim = 6, dimnames = list(age = 0:5))))
##     namesDatasets <- c("register", "tax")
##     transforms <- list(makeTransform(x = y, y = datasets[[1]]),
##                        makeTransform(x = y, y = datasets[[2]]))
##     transforms <- lapply(transforms, makeCollapseTransformExtra)
##     combined <- initialCombinedCounts(model,
##                                y = y,
##                                exposure = NULL,
##                                dataModels = data.models,
##                                datasets = datasets,
##                                namesDatasets = namesDatasets,
##                                transforms = transforms)
##     ans.obtained <- getSeriesForDataset(combined = combined, dataset = "register")
##     ans.expected <- combined@y
##     expect_identical(ans.obtained, ans.expected)
##     ans.obtained <- getSeriesForDataset(combined = combined, dataset = "tax")
##     ans.expected <- combined@y
##     expect_identical(ans.obtained, ans.expected)
## })

## test_that("getSeriesForDataset works with CombinedAccountMovements", {
##     getSeriesForDataset <- demest:::getSeriesForDataset
##     initialCombinedAccount <- demest:::initialCombinedAccount
##     makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
##     set.seed(1)
##     population <- CountsOne(values = seq(200, 300, 10),
##                             labels = seq(2000, 2100, 10),
##                             name = "time")
##     births <- CountsOne(values = rpois(n = 10, lambda = 5),
##                         labels = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-"),
##                         name = "time")
##     deaths <- CountsOne(values = rpois(n = 10, lambda = 5),
##                         labels = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-"),
##                         name = "time")
##     account <- Movements(population = population,
##                          births = births,
##                          exits = list(deaths = deaths))
##     account <- makeConsistent(account)
##     systemModels <- list(Model(population ~ Poisson(mean ~ time, useExpose = FALSE)),
##                          Model(births ~ Poisson(mean ~ 1)),
##                          Model(deaths ~ Poisson(mean ~ 1)))
##     systemWeights <- rep(list(NULL), 3)
##     data.models <- list(Model(tax ~ Poisson(mean ~ 1), series = "deaths"),
##                               Model(census ~ PoissonBinomial(prob = 0.9), series = "population"))
##     seriesIndices <- c(2L, 0L)
##     datasets <- list(Counts(array(7L,
##                                   dim = 10,
##                                   dimnames = list(time = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-")))),
##                      Counts(array(seq.int(110L, 210L, 10L),
##                                   dim = 11,
##                                   dimnames = list(time = seq(2000, 2100, 10)))))
##     namesDatasets <- c("tax", "census")
##     transforms <- list(makeTransform(x = deaths, y = datasets[[1]], subset = TRUE),
##                        makeTransform(x = population, y = datasets[[2]], subset = TRUE))
##     transforms <- lapply(transforms, makeCollapseTransformExtra)
##     x <- initialCombinedAccount(account = account,
##                                  systemModels = systemModels,
##                                  systemWeights = systemWeights,
##                                  dataModels = data.models,
##                                  seriesIndices = seriesIndices,
##                                  datasets = datasets,
##                                  namesDatasets = namesDatasets,
##                                 transforms = transforms)
##     ans.obtained <- getSeriesForDataset(combined = x, dataset = "tax")
##     ans.expected <- x@account@components[[2]]
##     expect_identical(ans.obtained, ans.expected)
##     ans.obtained <- getSeriesForDataset(combined = x, dataset = "census")
##     ans.expected <- x@account@population
##     expect_identical(ans.obtained, ans.expected)
## })


## drawCombined - CombinedModel ####################################################

test_that("drawCombined draws appropriate slots with CombinedModelBinomaial", {
    drawCombined <- demest:::drawCombined
    initialCombinedModelSimulate <- demest:::initialCombinedModelSimulate
    exposure <- Counts(array(as.integer(rpois(n = 24, lambda = 10)),
                      dim = 2:4,
                             dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rbinom(n = 24, size = exposure, prob = 0.5)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(time = "Intervals"))
    spec <- Model(y ~ Binomial(mean ~ sex * age + time),
                  `(Intercept)` ~ ExchFixed(mean = -1, sd = 0.3),
                  sex ~ ExchFixed(sd = 0.1),
                  age ~ DLM(level = Level(scale = HalfT(scale = 0.03)),
                            trend = NULL,
                            damp = NULL,
                            error = Error(scale = HalfT(scale = 0.03))),
                  sex:age ~ Exch(error = Error(scale = HalfT(scale = 0.001))),
                  time ~ DLM(level = Level(scale = HalfT(scale = 0.03)),
                            trend = NULL,
                            damp = NULL,
                            error = Error(scale = HalfT(scale = 0.03))),
                  priorSD = HalfT(scale = 0.1))
    x0 <- initialCombinedModelSimulate(spec,
                                       y = y,
                                       exposure = exposure,
                                       weights = NULL)
    x1 <- drawCombined(x0, nUpdate = 1L)
    for (name in "model")
        expect_false(identical(slot(x1, name), slot(x0, name)))
    for (name in c("y", "iMethodCombined", "slotsToExtract"))
        expect_true(identical(slot(x1, name), slot(x0, name)))
})

test_that("R, specific C, and generic C versions of drawCombined give same answer with CombinedModelBinomial", {
    drawCombined <- demest:::drawCombined
    initialCombinedModelSimulate <- demest:::initialCombinedModelSimulate
    spec <- Model(y ~ Binomial(mean ~ sex * age + time),
                  `(Intercept)` ~ ExchFixed(mean = -1, sd = 0.3),
                  sex ~ ExchFixed(sd = 0.1),
                  age ~ DLM(level = Level(scale = HalfT(scale = 0.03)),
                            trend = NULL,
                            damp = NULL,
                            error = Error(scale = HalfT(scale = 0.03))),
                  sex:age ~ Exch(error = Error(scale = HalfT(scale = 0.001))),
                  time ~ DLM(level = Level(scale = HalfT(scale = 0.03)),
                             trend = NULL,
                             damp = NULL,
                             error = Error(scale = HalfT(scale = 0.03))),
                  priorSD = HalfT(scale = 0.1))
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        exposure <- Counts(array(as.integer(rpois(n = 24, lambda = 10)),
                                 dim = 2:4,
                                 dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                           dimscales = c(time = "Intervals"))
        y <- Counts(array(as.integer(rbinom(n = 24, size = exposure, prob = 0.5)),
                          dim = 2:4,
                          dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                    dimscales = c(time = "Intervals"))
        x <- initialCombinedModelSimulate(spec,
                                          y = y,
                                          exposure = exposure,
                                          weights = NULL)
        set.seed(seed + 1)
        ans.R <- drawCombined(x, nUpdate = 3L, useC = FALSE)
        set.seed(seed + 1)
        ans.C.specific <- drawCombined(x, nUpdate = 3L, useC = TRUE, useSpecific = TRUE)
        set.seed(seed + 1)
        ans.C.generic <- drawCombined(x, nUpdate = 3L, useC = TRUE, useSpecific = FALSE)
        if (test.identity)
            expect_identical(ans.C.specific, ans.R)
        else
            expect_equal(ans.C.specific, ans.R)
        expect_identical(ans.C.specific, ans.C.generic)
    }
})



## drawCombined - Account ###################################################

test_that("drawCombined works with CombinedAccountMovements - no benchmarks", {
    drawCombined <- demest:::drawCombined
    updateAccount <- demest:::updateAccount
    initialCombinedAccountSimulate <- demest:::initialCombinedAccountSimulate
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    population <- Counts(array(seq(1000L, 1500L, 100L),
                               dim = c(3, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                               time = c(2000, 2005))))
    deaths <- Counts(array(c(10L, 5L, 3L),
                           dim = c(3, 1),
                           dimnames = list(reg = c("a", "b", "c"),
                                           time = "2001-2005")))
    internal <- Counts(array(c(0L, 50L, 40L,
                               20L, 0L, 30L,
                               60L, 20L, 0L),
                             dim = c(3, 3, 1),
                             dimnames = list(reg_orig = c("a", "b", "c"),
                                             reg_dest = c("a", "b", "c"),
                                             time = "2001-2005")))
    internal <- collapseOrigDest(internal, to = "net")
    account <- Movements(population = population,
                         internal = internal,
                         exits = list(deaths = deaths))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ reg + time, useExpose = FALSE),
                               `(Intercept)` ~ ExchFixed(mean = 1, sd = 0.2),
                               reg ~ ExchFixed(sd = 0.1),
                               time ~ ExchFixed(sd = 0.1),
                               priorSD = HalfT(scale = 0.1)),
                         Model(internal ~ Normal(mean ~ reg),
                               `(Intercept)` ~ ExchFixed(mean = 0, sd = 0.2),
                               reg ~ ExchFixed(sd = 0.1),
                               priorSD = HalfT(scale = 0.1)),
                         Model(deaths ~ Poisson(mean ~ reg),
                               `(Intercept)` ~ ExchFixed(mean = 0, sd = 0.2),
                               reg ~ ExchFixed(sd = 0.1),
                               priorSD = HalfT(scale = 0.1)))
    systemWeights <- list(NULL,
                          Counts(array(1,
                                       dim = c(3, 1),
                                       dimnames = list(reg = c("a", "b", "c"),
                                                       time = "2001-2005"))),
                          NULL)
    mean <- ValuesOne(1, labels = "2001-2005", name = "time")
    data.models <- list(Model(tax ~ NormalFixed(mean = mean, sd = 0.1), series = "internal"),
                        Model(census ~ PoissonBinomial(prob = 0.9), series = "population"))
    seriesIndices <- c(1L, 0L)
    datasets <- list(internal + 10L,
                     population - 5L)
    namesDatasets <- c("tax", "census")
    transforms <- list(makeTransform(x = internal, y = datasets[[1]], subset = TRUE),
                       makeTransform(x = population, y = datasets[[2]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x0 <- initialCombinedAccountSimulate(account = account,
                                         systemModels = systemModels,
                                         systemWeights = systemWeights,
                                         dataModels = data.models,
                                         seriesIndices = seriesIndices,
                                         datasets = datasets,
                                         namesDatasets = namesDatasets,
                                         transforms = transforms)
    set.seed(seed)
    ans.obtained <- drawCombined(x0, nUpdate = 5L)
    set.seed(seed)
    ans.expected <- x0
    for (i in 1:5)
        ans.expected <- updateAccount(ans.expected)
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})


test_that("R, C-specific, and C-generic versions of drawCombined give same answer with CombinedAccountMovements - no benchmarks", {
    drawCombined <- demest:::drawCombined
    updateAccount <- demest:::updateAccount
    initialCombinedAccountSimulate <- demest:::initialCombinedAccountSimulate
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    population <- Counts(array(seq(1000L, 1500L, 100L),
                               dim = c(3, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                               time = c(2000, 2005))))
    deaths <- Counts(array(c(10L, 5L, 3L),
                           dim = c(3, 1),
                           dimnames = list(reg = c("a", "b", "c"),
                                           time = "2001-2005")))
    internal <- Counts(array(c(0L, 50L, 40L,
                               20L, 0L, 30L,
                               60L, 20L, 0L),
                             dim = c(3, 3, 1),
                             dimnames = list(reg_orig = c("a", "b", "c"),
                                             reg_dest = c("a", "b", "c"),
                                             time = "2001-2005")))
    internal <- collapseOrigDest(internal, to = "net")
    account <- Movements(population = population,
                         internal = internal,
                         exits = list(deaths = deaths))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ reg + time, useExpose = FALSE),
                               `(Intercept)` ~ ExchFixed(mean = 1, sd = 0.2),
                               reg ~ ExchFixed(sd = 0.1),
                               time ~ ExchFixed(sd = 0.1),
                               priorSD = HalfT(scale = 0.1)),
                         Model(internal ~ Normal(mean ~ reg),
                               `(Intercept)` ~ ExchFixed(mean = 0, sd = 0.2),
                               reg ~ ExchFixed(sd = 0.1),
                               priorSD = HalfT(scale = 0.1)),
                         Model(deaths ~ Poisson(mean ~ reg),
                               `(Intercept)` ~ ExchFixed(mean = 0, sd = 0.2),
                               reg ~ ExchFixed(sd = 0.1),
                               priorSD = HalfT(scale = 0.1)))
    systemWeights <- list(NULL,
                          Counts(array(1,
                                       dim = c(3, 1),
                                       dimnames = list(reg = c("a", "b", "c"),
                                                       time = "2001-2005"))),
                          NULL)
    mean <- ValuesOne(1, labels = "2001-2005", name = "time")
    data.models <- list(Model(tax ~ NormalFixed(mean = mean, sd = 0.1), series = "internal"),
                        Model(census ~ PoissonBinomial(prob = 0.9), series = "population"))
    seriesIndices <- c(1L, 0L)
    datasets <- list(internal + 10L,
                     population - 5L)
    namesDatasets <- c("tax", "census")
    transforms <- list(makeTransform(x = internal, y = datasets[[1]], subset = TRUE),
                       makeTransform(x = population, y = datasets[[2]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x0 <- initialCombinedAccountSimulate(account = account,
                                         systemModels = systemModels,
                                         systemWeights = systemWeights,
                                         dataModels = data.models,
                                         seriesIndices = seriesIndices,
                                         datasets = datasets,
                                         namesDatasets = namesDatasets,
                                         transforms = transforms)
    set.seed(1)
    ans.R <- drawCombined(x0,
                          nUpdate = 5L,
                          useC = FALSE)
    set.seed(1)
    ans.C.specific <- drawCombined(x0,
                          nUpdate = 5L,
                          useC = TRUE,
                          useSpecific = TRUE)
    set.seed(1)
    ans.C.generic <- drawCombined(x0,
                          nUpdate = 5L,
                          useC = FALSE,
                          useSpecific = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C.specific)
    else
        expect_equal(ans.R, ans.C.specific)
    expect_identical(ans.C.specific, ans.C.generic)
})




## drawDataModels ######################################################

test_that("drawDataModels works with CombinedAccountMovements", {
    drawDataModels <- demest:::drawDataModels
    updateAccount <- demest:::updateAccount
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    checkDataModelsSuitableForSimulation <- demest:::checkDataModelsSuitableForSimulation
    setDatasetsToMissing <- demest:::setDatasetsToMissing
    drawModelUseExp <- demest:::drawModelUseExp
    set.seed(1)
    population <- CountsOne(values = seq(100L, 200L, 10L),
                            labels = seq(2000, 2100, 10),
                            name = "time")
    births <- CountsOne(values = rpois(n = 10, lambda = 15),
                        labels = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-"),
                        name = "time")
    deaths <- CountsOne(values = rpois(n = 10, lambda = 5),
                        labels = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-"),
                        name = "time")
    account <- Movements(population = population,
                         births = births,
                         exits = list(deaths = deaths))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ time, useExpose = FALSE)),
                         Model(births ~ Poisson(mean ~ 1)),
                         Model(deaths ~ Poisson(mean ~ 1)))
    systemWeights <- rep(list(NULL), 3)
    data.models <- list(Model(tax ~ Poisson(mean ~ time),
                              `(Intercept)` ~ ExchFixed(mean = -1, sd = 0.3),
                              time ~ Exch(error = Error(scale = HalfT(scale = 0.2))),
                              priorSD = HalfT(scale = 0.1),
                              series = "deaths"),
                        Model(census ~ PoissonBinomial(prob = 0.9),
                              series = "population"))
    seriesIndices <- c(2L, 0L)
    datasets <- list(subarray(deaths, time > 2010, drop = FALSE) + 1L,
                     subarray(population, time < 2090, drop = FALSE) - 1L)
    namesDatasets <- c("tax", "census")
    checkDataModelsSuitableForSimulation(dataModels = data.models,
                                         datasets = datasets,
                                         namesDatasets = namesDatasets)
    transforms <- list(makeTransform(x = deaths, y = datasets[[1]], subset = TRUE),
                       makeTransform(x = population, y = datasets[[2]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x <- initialCombinedAccount(account = account,
                                systemModels = systemModels,
                                systemWeights = systemWeights,
                                dataModels = data.models,
                                seriesIndices = seriesIndices,
                                datasets = datasets,
                                namesDatasets = namesDatasets,
                                transforms = transforms)
    x <- updateAccount(x)
    set.seed(1)
    expect_error(drawDataModels(x),
                 "'datasets' have not been set to missing")
    x <- setDatasetsToMissing(x)
    ans.obtained <- drawDataModels(x)
    set.seed(1)
    ans.expected <- x
    exposure1 <- toDouble(collapse(ans.expected@account@components[[2]],
                                   transform = transforms[[1]]))
    exposure2 <- collapse(ans.expected@account@population,
                          transform = transforms[[2]])
    ans.expected@dataModels[[1]] <- drawModelUseExp(ans.expected@dataModels[[1]],
                                                    y = ans.expected@datasets[[1]],
                                                    exposure = exposure1)
    ans.expected@dataModels[[2]] <- drawModelUseExp(ans.expected@dataModels[[2]],
                                                    y = ans.expected@datasets[[2]],
                                                    exposure = exposure2)
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})


## drawSystemModels ####################################################

test_that("drawSystemModels works with CombinedAccountMovements", {
    drawSystemModels <- demest:::drawSystemModels
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    checkSystemModelsSuitableForSimulation <- demest:::checkSystemModelsSuitableForSimulation
    updateExpectedExposure <- demest:::updateExpectedExposure
    set.seed(1)
    population <- CountsOne(values = seq(200, 300, 10),
                            labels = seq(2000, 2100, 10),
                            name = "time")
    births <- CountsOne(values = rpois(n = 10, lambda = 5),
                        labels = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-"),
                        name = "time")
    deaths <- CountsOne(values = rpois(n = 10, lambda = 5),
                        labels = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-"),
                        name = "time")
    account <- Movements(population = population,
                         births = births,
                         exits = list(deaths = deaths))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ time, useExpose = FALSE),
                               `(Intercept)` ~ ExchFixed(mean = -1, sd = 0.3),
                               time ~ DLM(level = Level(scale = HalfT(scale = 0.05)),
                                          trend = NULL,
                                          damp = NULL,
                                          error = Error(scale = HalfT(scale = 0.3))),
                               priorSD = HalfT(scale = 0.1)),
                         Model(births ~ Poisson(mean ~ time),
                               `(Intercept)` ~ ExchFixed(mean = -1, sd = 0.3),
                               time ~ Exch(error = Error(scale = HalfT(scale = 0.2))),
                               priorSD = HalfT(scale = 0.1)),
                         Model(deaths ~ Poisson(mean ~ time),
                               `(Intercept)` ~ ExchFixed(mean = -1, sd = 0.3),
                               time ~ Exch(error = Error(scale = HalfT(scale = 0.2))),
                               priorSD = HalfT(scale = 0.1)))
    checkSystemModelsSuitableForSimulation(systemModels = systemModels,
                                           account = account)
    systemWeights <- rep(list(NULL), 3)
    data.models <- list(Model(tax ~ Poisson(mean ~ 1), series = "deaths"),
                        Model(census ~ PoissonBinomial(prob = 0.9), series = "population"))
    seriesIndices <- c(2L, 0L)
    datasets <- list(Counts(array(7L,
                                  dim = 10,
                                  dimnames = list(time = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-")))),
                     Counts(array(seq.int(110L, 210L, 10L),
                                  dim = 11,
                                  dimnames = list(time = seq(2000, 2100, 10)))))
    namesDatasets <- c("tax", "census")
    transforms <- list(makeTransform(x = deaths, y = datasets[[1]], subset = TRUE),
                       makeTransform(x = population, y = datasets[[2]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x <- initialCombinedAccount(account = account,
                                systemModels = systemModels,
                                systemWeights = systemWeights,
                                dataModels = data.models,
                                seriesIndices = seriesIndices,
                                datasets = datasets,
                                namesDatasets = namesDatasets,
                                transforms = transforms)
    expect_true(validObject(x))
    set.seed(1)
    ans.obtained <- drawSystemModels(x)
    ans.obtained <- updateExpectedExposure(ans.obtained)
    expect_true(validObject(ans.obtained))
    for (i in 1:3) {
        expect_false(identical(ans.obtained@systemModels[[i]]@betas,
                               x@systemModels[[i]]@betas))
        expect_false(identical(ans.obtained@systemModels[[i]]@theta,
                               x@systemModels[[i]]@theta))
        expect_false(identical(ans.obtained@systemModels[[i]]@sigma,
                               x@systemModels[[i]]@sigma))
    }
})




## predictCombined - CombinedModel ####################################################

test_that("test that predictCombined gives valid answer with CombinedModelNormal", {
    predictCombined <- demest:::predictCombined
    initialCombinedModelPredict <- demest:::initialCombinedModelPredict
    initialCombinedModel <- demest:::initialCombinedModel
    predictModelNotUseExp <- demest:::predictModelNotUseExp
    extractValues <- demest:::extractValues
    transferParamModel <- demest:::transferParamModel
    y <- Values(array(rnorm(n = 30),
                      dim = c(2, 3, 5),
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2004)),
                dimscales = c(time = "Intervals"))
    weights <- Counts(array(runif(n = 30),
                            dim = c(2, 3, 5),
                            dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2004)),
                      dimscales = c(time = "Intervals"))
    spec <- Model(y ~ Normal(mean ~ age + time))
    combined.est <- initialCombinedModel(spec, y = y, exposure = NULL, weights = weights)
    combined.pred <- initialCombinedModelPredict(combined = combined.est,
                                                 along = 3L,
                                                 labels = NULL,
                                                 n = 2,
                                                 covariates = NULL,
                                                 aggregate = NULL,
                                                 lower = NULL,
                                                 upper = NULL,
                                                 yIsCounts = FALSE)
    values <- extractValues(combined.est)
    lengthIter <- length(values)
    filename <- tempfile()
    con <- file(filename, "wb")
    writeBin(values, con = con)
    close(con)
    set.seed(1)
    ans.obtained <- predictCombined(combined.pred,
                                    filename = filename,
                                    lengthIter = lengthIter,
                                    iteration = 1L,
                                    nUpdate = 1L)
    set.seed(1)
    ans.expected <- combined.pred
    model <- ans.expected@model
    model <- transferParamModel(model = model,
                                filename = filename,
                                lengthIter = lengthIter,
                                iteration = 1L)
    model <- predictModelNotUseExp(object = model, y = combined.pred@y)
    ans.expected@model <- model
    expect_identical(ans.obtained, ans.expected)
})

test_that("R, C.specific, and C.generic versions of predictCombined give same answer with CombinedModelNormal", {
    predictCombined <- demest:::predictCombined
    initialCombinedModelPredict <- demest:::initialCombinedModelPredict
    initialCombinedModel <- demest:::initialCombinedModel
    predictModelNotUseExp <- demest:::predictModelNotUseExp
    extractValues <- demest:::extractValues
    transferParamModel <- demest:::transferParamModel
    y <- Values(array(rnorm(n = 30),
                      dim = c(2, 3, 5),
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2004)),
                dimscales = c(time = "Intervals"))
    weights <- Counts(array(runif(n = 30),
                            dim = c(2, 3, 5),
                            dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2004)),
                      dimscales = c(time = "Intervals"))
    spec <- Model(y ~ Normal(mean ~ age + time))
    combined.est <- initialCombinedModel(spec, y = y, exposure = NULL, weights = weights)
    combined.pred <- initialCombinedModelPredict(combined = combined.est,
                                                 along = 3L,
                                                 labels = NULL,
                                                 n = 2,
                                                 covariates = NULL,
                                                 aggregate = NULL,
                                                 lower = NULL,
                                                 upper = NULL,
                                                 yIsCounts = FALSE)
    values <- extractValues(combined.est)
    lengthIter <- length(values)
    filename <- tempfile()
    con <- file(filename, "wb")
    writeBin(values, con = con)
    close(con)
    set.seed(1)
    ans.R <- predictCombined(combined.pred,
                             filename = filename,
                             lengthIter = lengthIter,
                             iteration = 1L,
                             nUpdate = 1L,
                             useC = FALSE)
    set.seed(1)
    ans.C.specific <- predictCombined(combined.pred,
                                      filename = filename,
                                      lengthIter = lengthIter,
                                      iteration = 1L,
                                      nUpdate = 1L,
                                      useC = TRUE,
                                      useSpecific = TRUE)
    set.seed(1)
    ans.C.generic <- predictCombined(combined.pred,
                                     filename = filename,
                                     lengthIter = lengthIter,
                                     iteration = 1L,
                                     nUpdate = 1L,
                                     useC = TRUE,
                                     useSpecific = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C.specific)
    else
        expect_equal(ans.R, ans.C.specific)
    expect_identical(ans.C.specific, ans.C.generic)
})

test_that("test that predictCombined gives valid answer with CombinedModelPoissonNotHasExp", {
    predictCombined <- demest:::predictCombined
    initialCombinedModelPredict <- demest:::initialCombinedModelPredict
    initialCombinedModel <- demest:::initialCombinedModel
    predictModelNotUseExp <- demest:::predictModelNotUseExp
    extractValues <- demest:::extractValues
    transferParamModel <- demest:::transferParamModel
    y <- Counts(array(as.integer(rpois(n = 30, lambda = 50)),
                      dim = c(2, 3, 5),
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2004)),
                dimscales = c(time = "Intervals"))
    spec <- Model(y ~ Poisson(mean ~ age + time, useExpose = FALSE))
    combined.est <- initialCombinedModel(spec, y = y, exposure = NULL, weights = NULL)
    combined.pred <- initialCombinedModelPredict(combined = combined.est,
                                                 along = 3L,
                                                 labels = NULL,
                                                 n = 2,
                                                 covariates = NULL,
                                                 aggregate = NULL,
                                                 lower = NULL,
                                                 upper = NULL,
                                                 yIsCounts = TRUE)
    values <- extractValues(combined.est)
    lengthIter <- length(values)
    filename <- tempfile()
    con <- file(filename, "wb")
    writeBin(values, con = con)
    close(con)
    set.seed(1)
    ans.obtained <- predictCombined(combined.pred,
                                    filename = filename,
                                    lengthIter = lengthIter,
                                    iteration = 1L,
                                    nUpdate = 1L)
    set.seed(1)
    ans.expected <- combined.pred
    model <- ans.expected@model
    model <- transferParamModel(model = model,
                                filename = filename,
                                lengthIter = lengthIter,
                                iteration = 1L)
    model <- predictModelNotUseExp(object = model, y = combined.pred@y)
    ans.expected@model <- model
    expect_identical(ans.obtained, ans.expected)
})

test_that("R, C.specific, and C.generic versions of predictCombined give same answer with CombinedModelPoissonNotHasExp", {
    predictCombined <- demest:::predictCombined
    initialCombinedModelPredict <- demest:::initialCombinedModelPredict
    initialCombinedModel <- demest:::initialCombinedModel
    predictModelNotUseExp <- demest:::predictModelNotUseExp
    extractValues <- demest:::extractValues
    transferParamModel <- demest:::transferParamModel
    y <- Counts(array(as.integer(rpois(n = 30, lambda = 50)),
                      dim = c(2, 3, 5),
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2004)),
                dimscales = c(time = "Intervals"))
    spec <- Model(y ~ Poisson(mean ~ age + time, useExpose = FALSE))
    combined.est <- initialCombinedModel(spec, y = y, exposure = NULL, weights = NULL)
    combined.pred <- initialCombinedModelPredict(combined = combined.est,
                                                 along = 3L,
                                                 labels = NULL,
                                                 n = 2,
                                                 covariates = NULL,
                                                 aggregate = NULL,
                                                 lower = NULL,
                                                 upper = NULL,
                                                 yIsCounts = TRUE)
    values <- extractValues(combined.est)
    lengthIter <- length(values)
    filename <- tempfile()
    con <- file(filename, "wb")
    writeBin(values, con = con)
    close(con)
    set.seed(1)
    ans.R <- predictCombined(combined.pred,
                             filename = filename,
                             lengthIter = lengthIter,
                             iteration = 1L,
                             nUpdate = 1L,
                             useC = FALSE)
    set.seed(1)
    ans.C.specific <- predictCombined(combined.pred,
                                      filename = filename,
                                      lengthIter = lengthIter,
                                      iteration = 1L,
                                      nUpdate = 1L,
                                      useC = TRUE,
                                      useSpecific = TRUE)
    set.seed(1)
    ans.C.generic <- predictCombined(combined.pred,
                                     filename = filename,
                                     lengthIter = lengthIter,
                                     iteration = 1L,
                                     nUpdate = 1L,
                                     useC = TRUE,
                                     useSpecific = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C.specific)
    else
        expect_equal(ans.R, ans.C.specific)
    expect_identical(ans.C.specific, ans.C.generic)
})

test_that("test that predictCombined gives valid answer with CombinedModelBinomial", {
    predictCombined <- demest:::predictCombined
    initialCombinedModelPredict <- demest:::initialCombinedModelPredict
    initialCombinedModel <- demest:::initialCombinedModel
    predictModelUseExp <- demest:::predictModelUseExp
    extractValues <- demest:::extractValues
    transferParamModel <- demest:::transferParamModel
    exposure <- Counts(array(as.integer(rpois(n = 30, lambda = 50)),
                      dim = c(2, 3, 5),
                             dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2004)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rbinom(n = 30, size = exposure, prob = 0.5)),
                      dim = c(2, 3, 5),
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2004)),
                dimscales = c(time = "Intervals"))
    spec <- Model(y ~ Binomial(mean ~ age + time))
    combined.est <- initialCombinedModel(spec, y = y, exposure = exposure, weights = NULL)
    combined.pred <- initialCombinedModelPredict(combined = combined.est,
                                                 along = 3L,
                                                 labels = NULL,
                                                 n = 2,
                                                 covariates = NULL,
                                                 aggregate = NULL,
                                                 lower = NULL,
                                                 upper = NULL,
                                                 yIsCounts = TRUE)
    values <- extractValues(combined.est)
    lengthIter <- length(values)
    filename <- tempfile()
    con <- file(filename, "wb")
    writeBin(values, con = con)
    close(con)
    set.seed(1)
    ans.obtained <- predictCombined(combined.pred,
                                    filename = filename,
                                    lengthIter = lengthIter,
                                    iteration = 1L,
                                    nUpdate = 1L)
    set.seed(1)
    ans.expected <- combined.pred
    model <- ans.expected@model
    model <- transferParamModel(model = model,
                                filename = filename,
                                lengthIter = lengthIter,
                                iteration = 1L)
    model <- predictModelUseExp(object = model, y = combined.pred@y, exposure = combined.pred@exposure)
    ans.expected@model <- model
    expect_identical(ans.obtained, ans.expected)
})

test_that("R, C.specific, and C.generic versions of predictCombined give same answer with CombinedModelBinomial", {
    predictCombined <- demest:::predictCombined
    initialCombinedModelPredict <- demest:::initialCombinedModelPredict
    initialCombinedModel <- demest:::initialCombinedModel
    predictModelUseExp <- demest:::predictModelUseExp
    extractValues <- demest:::extractValues
    transferParamModel <- demest:::transferParamModel
    exposure <- Counts(array(as.integer(rpois(n = 30, lambda = 50)),
                      dim = c(2, 3, 5),
                             dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2004)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rbinom(n = 30, size = exposure, prob = 0.5)),
                      dim = c(2, 3, 5),
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2004)),
                dimscales = c(time = "Intervals"))
    spec <- Model(y ~ Binomial(mean ~ age + time))
    combined.est <- initialCombinedModel(spec, y = y, exposure = exposure, weights = NULL)
    combined.pred <- initialCombinedModelPredict(combined = combined.est,
                                                 along = 3L,
                                                 labels = NULL,
                                                 n = 2,
                                                 covariates = NULL,
                                                 aggregate = NULL,
                                                 lower = NULL,
                                                 upper = NULL,
                                                 yIsCounts = TRUE)
    values <- extractValues(combined.est)
    lengthIter <- length(values)
    filename <- tempfile()
    con <- file(filename, "wb")
    writeBin(values, con = con)
    close(con)
    set.seed(1)
    ans.R <- predictCombined(combined.pred,
                             filename = filename,
                             lengthIter = lengthIter,
                             iteration = 1L,
                             nUpdate = 1L,
                             useC = FALSE)
    set.seed(1)
    ans.C.specific <- predictCombined(combined.pred,
                                      filename = filename,
                                      lengthIter = lengthIter,
                                      iteration = 1L,
                                      nUpdate = 1L,
                                      useC = TRUE,
                                      useSpecific = TRUE)
    set.seed(1)
    ans.C.generic <- predictCombined(combined.pred,
                                     filename = filename,
                                     lengthIter = lengthIter,
                                     iteration = 1L,
                                     nUpdate = 1L,
                                     useC = TRUE,
                                     useSpecific = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C.specific)
    else
        expect_equal(ans.R, ans.C.specific)
    expect_identical(ans.C.specific, ans.C.generic)
})

test_that("test that predictCombined gives valid answer with CombinedModelPoissonHasExp", {
    predictCombined <- demest:::predictCombined
    initialCombinedModelPredict <- demest:::initialCombinedModelPredict
    initialCombinedModel <- demest:::initialCombinedModel
    predictModelUseExp <- demest:::predictModelUseExp
    extractValues <- demest:::extractValues
    transferParamModel <- demest:::transferParamModel
    exposure <- Counts(array(runif(n = 30, max = 50),
                             dim = c(2, 3, 5),
                             dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2004)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rpois(n = 30, lambda = 0.5 * exposure)),
                      dim = c(2, 3, 5),
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2004)),
                dimscales = c(time = "Intervals"))
    spec <- Model(y ~ Poisson(mean ~ age + time))
    combined.est <- initialCombinedModel(spec, y = y, exposure = exposure, weights = NULL)
    combined.pred <- initialCombinedModelPredict(combined = combined.est,
                                                 along = 3L,
                                                 labels = NULL,
                                                 n = 2,
                                                 covariates = NULL,
                                                 aggregate = NULL,
                                                 lower = NULL,
                                                 upper = NULL,
                                                 yIsCounts = TRUE)
    values <- extractValues(combined.est)
    lengthIter <- length(values)
    filename <- tempfile()
    con <- file(filename, "wb")
    writeBin(values, con = con)
    close(con)
    set.seed(1)
    ans.obtained <- predictCombined(combined.pred,
                                    filename = filename,
                                    lengthIter = lengthIter,
                                    iteration = 1L,
                                    nUpdate = 1L)
    set.seed(1)
    ans.expected <- combined.pred
    model <- ans.expected@model
    model <- transferParamModel(model = model,
                                filename = filename,
                                lengthIter = lengthIter,
                                iteration = 1L)
    model <- predictModelUseExp(object = model, y = combined.pred@y, exposure = combined.pred@exposure)
    ans.expected@model <- model
    expect_identical(ans.obtained, ans.expected)
})

test_that("R, C.specific, and C.generic versions of predictCombined give same answer with CombinedModelPoissonHasExp", {
    predictCombined <- demest:::predictCombined
    initialCombinedModelPredict <- demest:::initialCombinedModelPredict
    initialCombinedModel <- demest:::initialCombinedModel
    predictModelUseExp <- demest:::predictModelUseExp
    extractValues <- demest:::extractValues
    transferParamModel <- demest:::transferParamModel
    exposure <- Counts(array(runif(n = 30, max = 50),
                             dim = c(2, 3, 5),
                             dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2004)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rpois(n = 30, lambda = 0.5 * exposure)),
                      dim = c(2, 3, 5),
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2004)),
                dimscales = c(time = "Intervals"))
    spec <- Model(y ~ Poisson(mean ~ age + time))
    combined.est <- initialCombinedModel(spec, y = y, exposure = exposure, weights = NULL)
    combined.pred <- initialCombinedModelPredict(combined = combined.est,
                                                 along = 3L,
                                                 labels = NULL,
                                                 n = 2,
                                                 covariates = NULL,
                                                 aggregate = NULL,
                                                 lower = NULL,
                                                 upper = NULL,
                                                 yIsCounts = TRUE)
    values <- extractValues(combined.est)
    lengthIter <- length(values)
    filename <- tempfile()
    con <- file(filename, "wb")
    writeBin(values, con = con)
    close(con)
    set.seed(1)
    ans.R <- predictCombined(combined.pred,
                             filename = filename,
                             lengthIter = lengthIter,
                             iteration = 1L,
                             nUpdate = 1L,
                             useC = FALSE)
    set.seed(1)
    ans.C.specific <- predictCombined(combined.pred,
                                      filename = filename,
                                      lengthIter = lengthIter,
                                      iteration = 1L,
                                      nUpdate = 1L,
                                      useC = TRUE,
                                      useSpecific = TRUE)
    set.seed(1)
    ans.C.generic <- predictCombined(combined.pred,
                                     filename = filename,
                                     lengthIter = lengthIter,
                                     iteration = 1L,
                                     nUpdate = 1L,
                                     useC = TRUE,
                                     useSpecific = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C.specific)
    else
        expect_equal(ans.R, ans.C.specific)
    expect_identical(ans.C.specific, ans.C.generic)
})


## predictCombined - CombinedCounts ###################################################


test_that("predictCombined works with object of class CombinedCountsPoissonHasExp", {
    predictCombined <- demest:::predictCombined
    initialCombinedCounts <- demest:::initialCombinedCounts
    initialCombinedCountsPredict <- demest:::initialCombinedCountsPredict
    predictModelUseExp <- demest:::predictModelUseExp
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    extractValues <- demest:::extractValues
    transferParamModel <- demest:::transferParamModel
    object <- Model(y ~ Poisson(mean ~ sex * region))
    y <- Counts(array(1:24,
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), region = 1:3, time = 0:3)),
                dimscales = c(time = "Intervals"))
    exposure <- y + 2
    y[24] <- NA
    datasets <- list(Counts(array(c(1:11, NA),
                                  dim = c(2, 3, 2),
                                  dimnames = list(sex = c("f", "m"), region = 1:3, time = 2:3)),
                            dimscales = c(time = "Intervals")),
                     Counts(array(1:12,
                                  dim = 3:4,
                                  dimnames = list(region = 1:3, time = 0:3)),
                            dimscales = c(time = "Intervals")))
    namesDatasets <- c("tax", "census")
    transforms <- list(makeTransform(x = y, y = datasets[[1]], subset = TRUE),
                       makeTransform(x = y, y = datasets[[2]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    data.models <- list(Model(tax ~ Poisson(mean ~ time + sex)),
                        Model(census ~ PoissonBinomial(prob = 0.9)))
    x.est <- initialCombinedCounts(object = object,
                                   y = y,
                                   exposure = exposure,
                                   dataModels = data.models,
                                   datasets = datasets,
                                   namesDatasets = namesDatasets,
                                   transforms = transforms)
    exposure.pred <- toDouble(extrapolate(exposure, labels = c("4", "5"))[,,5:6])
    x.pred <- initialCombinedCountsPredict(x.est,
                                           along = 3L,
                                           labels = NULL,
                                           n = 2L,
                                           exposure = exposure.pred,
                                           covariates = list(),
                                           aggregate = list(),
                                           lower = list(),
                                           upper = list())
    expect_is(x.pred, "CombinedCountsPoissonHasExp")
    expect_true(validObject(x.pred))
    values <- extractValues(x.est)
    lengthIter <- length(values)
    filename <- tempfile()
    con <- file(filename, "wb")
    writeBin(values, con = con)
    close(con)
    set.seed(1)
    ans.obtained <- predictCombined(x.pred,
                                    filename = filename,
                                    lengthIter = lengthIter,
                                    iteration = 1L,
                                    nUpdate = 1L)
    set.seed(1)
    ans.expected <- x.pred
    model <- ans.expected@model
    model <- transferParamModel(model = model,
                                filename = filename,
                                lengthIter = lengthIter,
                                iteration = 1L)
    model <- predictModelUseExp(object = model, y = x.pred@y, exposure = x.pred@exposure)
    ans.expected@model <- model
    ans.expected@y[] <- rpois(n = length(model@theta), lambda = model@theta * exposure.pred)
    ans.expected@dataModels[[1]] <- predictModelUseExp(object = ans.expected@dataModels[[1]],
                                                       y = ans.expected@datasets[[1]],
                                                       exposure = toDouble(ans.expected@datasets[[1]]))
    ans.expected@dataModels[[2]] <- predictModelUseExp(object = ans.expected@dataModels[[2]],
                                                       y = ans.expected@datasets[[2]],
                                                       exposure = ans.expected@datasets[[2]])
    expect_identical(ans.obtained, ans.expected)    
})


test_that("R, C generic and C specific versions of predictCombined give same answer with object of class CombinedCountsPoissonHasExp", {
    predictCombined <- demest:::predictCombined
    initialCombinedCounts <- demest:::initialCombinedCounts
    initialCombinedCountsPredict <- demest:::initialCombinedCountsPredict
    predictModelUseExp <- demest:::predictModelUseExp
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    extractValues <- demest:::extractValues
    object <- Model(y ~ Poisson(mean ~ sex * region))
    y <- Counts(array(1:24,
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), region = 1:3, time = 0:3)),
                dimscales = c(time = "Intervals"))
    exposure <- y + 2
    y[24] <- NA
    datasets <- list(Counts(array(c(1:11, NA),
                                  dim = c(2, 3, 2),
                                  dimnames = list(sex = c("f", "m"), region = 1:3, time = 2:3)),
                            dimscales = c(time = "Intervals")),
                     Counts(array(1:12,
                                  dim = 3:4,
                                  dimnames = list(region = 1:3, time = 0:3)),
                            dimscales = c(time = "Intervals")))
    namesDatasets <- c("tax", "census")
    transforms <- list(makeTransform(x = y, y = datasets[[1]], subset = TRUE),
                       makeTransform(x = y, y = datasets[[2]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    data.models <- list(Model(tax ~ Poisson(mean ~ time + sex)),
                        Model(census ~ PoissonBinomial(prob = 0.9)))
    x.est <- initialCombinedCounts(object = object,
                                   y = y,
                                   exposure = exposure,
                                   dataModels = data.models,
                                   datasets = datasets,
                                   namesDatasets = namesDatasets,
                                   transforms = transforms)
    exposure.pred <- toDouble(extrapolate(exposure, labels = c("4", "5"))[,,5:6])
    x.pred <- initialCombinedCountsPredict(x.est,
                                           along = 3L,
                                           labels = NULL,
                                           n = 2L,
                                           exposure = exposure.pred,
                                           covariates = list(),
                                           aggregate = list(),
                                           lower = list(),
                                           upper = list())
    expect_is(x.pred, "CombinedCountsPoissonHasExp")
    expect_true(validObject(x.pred))
    values <- extractValues(x.est)
    lengthIter <- length(values)
    filename <- tempfile()
    con <- file(filename, "wb")
    writeBin(values, con = con)
    close(con)
    set.seed(1)
    ans.R <- predictCombined(x.pred,
                             filename = filename,
                             lengthIter = lengthIter,
                             iteration = 1L,
                             nUpdate = 1L,
                             useC = FALSE)
    set.seed(1)
    ans.C.specific <- predictCombined(x.pred,
                                      filename = filename,
                                      lengthIter = lengthIter,
                                      iteration = 1L,
                                      nUpdate = 1L,
                                      useC = TRUE,
                                      useSpecific = TRUE)
    set.seed(1)
    ans.C.generic <- predictCombined(x.pred,
                                     filename = filename,
                                     lengthIter = lengthIter,
                                     iteration = 1L,
                                     nUpdate = 1L,
                                     useC = TRUE,
                                     useSpecific = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C.specific)
    else
        expect_equal(ans.R, ans.C.specific)
    expect_identical(ans.C.specific, ans.C.generic)
})



## updateCombined - CombinedModel #####################################################

## Assume that underlying updating functions work correctly.  Only check that
## appropriate slots are updated.

test_that("updateCombined updates appropriate slots with CombinedModelNormal", {
    updateCombined <- demest:::updateCombined
    initialCombinedModel <- demest:::initialCombinedModel
    y <- Counts(array(rnorm(24),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(time = "Intervals"))
    y[1] <- NA
    spec <- Model(y ~ Normal(mean ~ sex * age + time))
    x0 <- initialCombinedModel(spec, y = y, exposure = NULL, weights = NULL)
    x1 <- updateCombined(x0)
    for (name in "model")
        expect_false(identical(slot(x1, name), slot(x0, name)))
    for (name in c("y", "iMethodCombined", "slotsToExtract"))
        expect_true(identical(slot(x1, name), slot(x0, name)))
})

test_that("R, specific C, and generic C versions of updateCombined give same answer with CombinedModelNormal", {
    updateCombined <- demest:::updateCombined
    initialCombinedModel <- demest:::initialCombinedModel
    seed <- 1
    set.seed(seed)
    y <- Counts(array(rnorm(24),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(time = "Intervals"))
    y[1] <- NA
    spec <- Model(y ~ Normal(mean ~ sex * age + time))
    x <- initialCombinedModel(spec, y = y, exposure = NULL, weights = NULL)
    set.seed(seed + 1)
    ans.R <- updateCombined(x, useC = FALSE)
    set.seed(seed + 1)
    ans.C.specific <- updateCombined(x, useC = TRUE, useSpecific = TRUE)
    set.seed(seed + 1)
    ans.C.generic <- updateCombined(x, useC = TRUE, useSpecific = FALSE)
    if (test.identity)
        expect_identical(ans.C.specific, ans.R)
    else
        expect_equal(ans.C.specific, ans.R)
    expect_identical(ans.C.specific, ans.C.generic)
})

test_that("updateCombined updates appropriate slots with CombinedModelPoissonNotHasExp", {
    updateCombined <- demest:::updateCombined
    initialCombinedModel <- demest:::initialCombinedModel
    set.seed(1)
    y <- Counts(array(as.integer(rpois(n = 24, lambda = 30)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(age = "Intervals", time = "Intervals"))
    y[1] <- NA
    spec <- Model(y ~ Poisson(mean ~ sex * age + time, useExpose = FALSE))
    x0 <- initialCombinedModel(spec, y = y, exposure = NULL, weights = NULL)
    x1 <- updateCombined(x0)
    for (name in "model")
        expect_false(identical(slot(x1, name), slot(x0, name)))
    for (name in c("y", "iMethodCombined", "slotsToExtract"))
        expect_true(identical(slot(x1, name), slot(x0, name)))
})

## tests equal but not identical
test_that("R, specific C, and generic C versions of updateCombined give same answer with CombinedModelPoissonNotHasExp", {
    updateCombined <- demest:::updateCombined
    initialCombinedModel <- demest:::initialCombinedModel
    seed <- 1
    set.seed(seed)
    y <- Counts(array(as.integer(rpois(n = 24, lambda = 30)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(time = "Intervals"))
    y[1] <- NA
    spec <- Model(y ~ Poisson(mean ~ sex * age + time, useExpose = FALSE))
    x <- initialCombinedModel(spec, y = y, exposure = NULL, weights = NULL)
    set.seed(seed + 1)
    ans.R <- updateCombined(x, useC = FALSE)
    set.seed(seed + 1)
    ans.C.specific <- updateCombined(x, useC = TRUE, useSpecific = TRUE)
    set.seed(seed + 1)
    ans.C.generic <- updateCombined(x, useC = TRUE, useSpecific = FALSE)
    if (test.identity)
        expect_identical(ans.C.specific, ans.R)
    else
        expect_equal(ans.C.specific, ans.R)
    expect_identical(ans.C.specific, ans.C.generic)
})

test_that("updateCombined updates appropriate slots with CombinedModelBinomaial", {
    updateCombined <- demest:::updateCombined
    initialCombinedModel <- demest:::initialCombinedModel
    exposure <- Counts(array(as.integer(rpois(n = 24, lambda = 10)),
                      dim = 2:4,
                             dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rbinom(n = 24, size = exposure, prob = 0.8)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(time = "Intervals"))
    exposure[1] <- NA
    y[1] <- NA
    spec <- Model(y ~ Binomial(mean ~ sex * age + time))
    x0 <- initialCombinedModel(spec, y = y, exposure = exposure, weights = NULL)
    x1 <- updateCombined(x0)
    for (name in "model")
        expect_false(identical(slot(x1, name), slot(x0, name)))
    for (name in c("y", "iMethodCombined", "slotsToExtract"))
        expect_true(identical(slot(x1, name), slot(x0, name)))
})

test_that("R, specific C, and generic C versions of updateCombined give same answer with CombinedModelBinomial", {
    updateCombined <- demest:::updateCombined
    initialCombinedModel <- demest:::initialCombinedModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        exposure <- Counts(array(as.integer(rpois(n = 24, lambda = 10)),
                                 dim = 2:4,
                                 dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                           dimscales = c(time = "Intervals"))
        y <- Counts(array(as.integer(rbinom(n = 24, size = exposure, prob = 0.8)),
                          dim = 2:4,
                          dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                    dimscales = c(time = "Intervals"))
        exposure[1] <- NA
        y[1] <- NA
        spec <- Model(y ~ Binomial(mean ~ sex * age + time))
        x <- initialCombinedModel(spec, y = y, exposure = exposure, weights = NULL)
        set.seed(seed + 1)
        ans.R <- updateCombined(x, useC = FALSE)
        set.seed(seed + 1)
        ans.C.specific <- updateCombined(x, useC = TRUE, useSpecific = TRUE)
        set.seed(seed + 1)
        ans.C.generic <- updateCombined(x, useC = TRUE, useSpecific = FALSE)
        if (test.identity)
            expect_identical(ans.C.specific, ans.R)
        else
            expect_equal(ans.C.specific, ans.R)
        expect_identical(ans.C.specific, ans.C.generic)
    }
})

test_that("updateCombined updates appropriate slots with CombinedModelPoissonUseExp", {
    updateCombined <- demest:::updateCombined
    initialCombinedModel <- demest:::initialCombinedModel
    exposure <- Counts(array(runif(n = 24, max = 20),
                      dim = 2:4,
                             dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rpois(n = 24, lambda = 0.5 * exposure)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(time = "Intervals"))
    exposure[1] <- NA
    y[1] <- NA
    spec <- Model(y ~ Poisson(mean ~ sex * age + time))
    x0 <- initialCombinedModel(spec, y = y, exposure = exposure, weights = NULL)
    x1 <- updateCombined(x0)
    for (name in "model")
        expect_false(identical(slot(x1, name), slot(x0, name)))
    for (name in c("y", "iMethodCombined", "slotsToExtract"))
        expect_true(identical(slot(x1, name), slot(x0, name)))
})

test_that("R, specific C, and generic C versions of updateCombined give same answer with CombinedModelPoissonUseExp", {
    updateCombined <- demest:::updateCombined
    initialCombinedModel <- demest:::initialCombinedModel
    seed <- 1
    set.seed(seed)
    exposure <- Counts(array(runif(n = 24, max = 20),
                             dim = 2:4,
                             dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rpois(n = 24, lambda = 0.5 * exposure)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(time = "Intervals"))
    exposure[1] <- NA
    y[1] <- NA
    spec <- Model(y ~ Poisson(mean ~ sex * age + time))
    x <- initialCombinedModel(spec, y = y, exposure = exposure, weights = NULL)
    set.seed(seed + 1)
    ans.R <- updateCombined(x, useC = FALSE)
    set.seed(seed + 1)
    ans.C.specific <- updateCombined(x, useC = TRUE, useSpecific = TRUE)
    set.seed(seed + 1)
    ans.C.generic <- updateCombined(x, useC = TRUE, useSpecific = FALSE)
    if (test.identity)
        expect_identical(ans.C.specific, ans.R)
    else
        expect_equal(ans.C.specific, ans.R)
    expect_identical(ans.C.specific, ans.C.generic)
})


test_that("updateCombined updates appropriate slots with CombinedModelCMPNotHasExp", {
    updateCombined <- demest:::updateCombined
    initialCombinedModel <- demest:::initialCombinedModel
    set.seed(1)
    y <- Counts(array(as.integer(rpois(n = 24, lambda = 30)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(age = "Intervals", time = "Intervals"))
    y[1] <- NA
    spec <- Model(y ~ CMP(mean ~ sex * age + time, useExpose = FALSE))
    x0 <- initialCombinedModel(spec, y = y, exposure = NULL, weights = NULL)
    x1 <- updateCombined(x0)
    for (name in "model")
        expect_false(identical(slot(x1, name), slot(x0, name)))
    for (name in c("y", "iMethodCombined", "slotsToExtract"))
        expect_true(identical(slot(x1, name), slot(x0, name)))
})

## tests equal but not identical
test_that("R, specific C, and generic C versions of updateCombined give same answer with CombinedModelCMPNotHasExp", {
    updateCombined <- demest:::updateCombined
    initialCombinedModel <- demest:::initialCombinedModel
    seed <- 1
    set.seed(seed)
    y <- Counts(array(as.integer(rpois(n = 24, lambda = 30)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(time = "Intervals"))
    y[1] <- NA
    spec <- Model(y ~ CMP(mean ~ sex * age + time, useExpose = FALSE))
    x <- initialCombinedModel(spec, y = y, exposure = NULL, weights = NULL)
    set.seed(seed + 1)
    ans.R <- updateCombined(x, useC = FALSE)
    set.seed(seed + 1)
    ans.C.specific <- updateCombined(x, useC = TRUE, useSpecific = TRUE)
    set.seed(seed + 1)
    ans.C.generic <- updateCombined(x, useC = TRUE, useSpecific = FALSE)
    if (test.identity)
        expect_identical(ans.C.specific, ans.R)
    else
        expect_equal(ans.C.specific, ans.R)
    expect_identical(ans.C.specific, ans.C.generic)
})

test_that("updateCombined updates appropriate slots with CombinedModelCMPUseExp", {
    updateCombined <- demest:::updateCombined
    initialCombinedModel <- demest:::initialCombinedModel
    exposure <- Counts(array(runif(n = 24, max = 20),
                      dim = 2:4,
                             dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rpois(n = 24, lambda = 0.5 * exposure)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(time = "Intervals"))
    exposure[1] <- NA
    y[1] <- NA
    spec <- Model(y ~ CMP(mean ~ sex * age + time))
    x0 <- initialCombinedModel(spec, y = y, exposure = exposure, weights = NULL)
    x1 <- updateCombined(x0)
    for (name in "model")
        expect_false(identical(slot(x1, name), slot(x0, name)))
    for (name in c("y", "iMethodCombined", "slotsToExtract"))
        expect_true(identical(slot(x1, name), slot(x0, name)))
})

test_that("R, specific C, and generic C versions of updateCombined give same answer with CombinedModelCMPUseExp", {
    updateCombined <- demest:::updateCombined
    initialCombinedModel <- demest:::initialCombinedModel
    seed <- 1
    set.seed(seed)
    exposure <- Counts(array(runif(n = 24, max = 20),
                             dim = 2:4,
                             dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rpois(n = 24, lambda = 0.5 * exposure)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(time = "Intervals"))
    exposure[1] <- NA
    y[1] <- NA
    spec <- Model(y ~ CMP(mean ~ sex * age + time))
    x <- initialCombinedModel(spec, y = y, exposure = exposure, weights = NULL)
    set.seed(seed + 1)
    ans.R <- updateCombined(x, useC = FALSE)
    set.seed(seed + 1)
    ans.C.specific <- updateCombined(x, useC = TRUE, useSpecific = TRUE)
    set.seed(seed + 1)
    ans.C.generic <- updateCombined(x, useC = TRUE, useSpecific = FALSE)
    if (test.identity)
        expect_identical(ans.C.specific, ans.R)
    else
        expect_equal(ans.C.specific, ans.R)
    expect_identical(ans.C.specific, ans.C.generic)
})



## CombinedCounts #####################################################################

test_that("updateCombined method for CombinedCountsPoissonNotHasExp updates correct slots", {
    initialCombinedCounts <- demest:::initialCombinedCounts
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    updateCombined <- demest:::updateCombined
    set.seed(100)
    y <- Counts(array(c(1:12),
                      dim = c(6, 2),
                      dimnames = list(age = 0:5, sex = c("f", "m"))))
    model <- Model(y ~ Poisson(mean ~ age, useExpose = FALSE))
    data.models <- list(Model(register ~ PoissonBinomial(prob = 0.98)),
                        Model(tax ~ Binomial(mean ~ 1)))
    datasets <- list(Counts(array(c(0L, 2:12), dim = c(6, 2),
                                  dimnames = list(age = 0:5, sex = c("f", "m")))),
                     Counts(array(c(1:5, NA), dim = 6, dimnames = list(age = 0:5))))
    namesDatasets <- c("register", "tax")
    transforms <- list(makeTransform(x = y, y = datasets[[1]]),
                       makeTransform(x = y, y = datasets[[2]]))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x0 <- initialCombinedCounts(model,
                                y = y,
                                exposure = NULL,
                                dataModels = data.models,
                                datasets = datasets,
                                namesDatasets = namesDatasets,
                                transforms = transforms)
    x0@y[1] <- 100L
    x1 <- updateCombined(x0)
    for (name in c("model", "y", "dataModels")) {
        expect_false(identical(slot(x1, name), slot(x0, name)))
    }
    for (name in c("namesDatasets", "datasets", "transforms", "iMethodCombined", "slotsToExtract"))
        expect_true(identical(slot(x1, name), slot(x0, name)))
})

## tests equal but not identical
test_that("R, specific C, and generic C versions of updateCombined method for CombinedCountsPoissonNotHasExp give same answer", {
    initialCombinedCounts <- demest:::initialCombinedCounts
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    updateCombined <- demest:::updateCombined
    set.seed(10)
    y <- Counts(array(c(1:11, 20L),
                      dim = c(6, 2),
                      dimnames = list(age = 0:5, sex = c("f", "m"))))
    model <- Model(y ~ Poisson(mean ~ age, useExpose = FALSE))
    data.models <- list(Model(register ~ PoissonBinomial(prob = 0.98)),
                        Model(tax ~ Binomial(mean ~ 1)))
    datasets <- list(Counts(array(c(0L, 2:12), dim = c(6, 2),
                                  dimnames = list(age = 0:5, sex = c("f", "m")))),
                         Counts(array(c(1:5, NA), dim = 6, dimnames = list(age = 0:5))))
    namesDatasets <- c("register", "tax")
    transforms <- list(makeTransform(x = y, y = datasets[[1]]),
                       makeTransform(x = y, y = datasets[[2]]))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x0 <- initialCombinedCounts(model,
                               y = y,
                               exposure = NULL,
                               dataModels = data.models,
                               datasets = datasets,
                               namesDatasets = namesDatasets,
                               transforms = transforms)
    set.seed(100)
    x1.R <- updateCombined(x0, useC = FALSE)
    set.seed(100)
    x1.C.specific <- updateCombined(x0, useC = TRUE, useSpecific = TRUE)
    set.seed(100)
    x1.C.generic <- updateCombined(x0, useC = TRUE, useSpecific = FALSE)
    if (test.identity)
        expect_identical(x1.R, x1.C.specific)
    else
        expect_equal(x1.R, x1.C.specific)
    expect_identical(x1.C.specific, x1.C.generic)
})

test_that("updateCombined method for CombinedCountsPoissonHasExp updates correct slots", {
    initialCombinedCounts <- demest:::initialCombinedCounts
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    updateCombined <- demest:::updateCombined
    set.seed(100)
    y <- Counts(array(c(1:11, 20L),
                      dim = c(6, 2),
                      dimnames = list(age = 0:5, sex = c("f", "m"))))
    exposure <- Counts(array(runif(12, max = 20),
                      dim = c(6, 2),
                      dimnames = list(age = 0:5, sex = c("f", "m"))))
    model <- Model(y ~ Poisson(mean ~ age))
    data.models <- list(Model(register ~ PoissonBinomial(prob = 0.98)),
                        Model(tax ~ Binomial(mean ~ 1)))
    datasets <- list(Counts(array(c(0L, 2:12), dim = c(6, 2),
                                  dimnames = list(age = 0:5, sex = c("f", "m")))),
                         Counts(array(c(1:5, NA), dim = 6, dimnames = list(age = 0:5))))
    namesDatasets <- c("register", "tax")
    transforms <- list(makeTransform(x = y, y = datasets[[1]]),
                       makeTransform(x = y, y = datasets[[2]]))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x0 <- initialCombinedCounts(model,
                               y = y,
                               exposure = exposure,
                               dataModels = data.models,
                               datasets = datasets,
                               namesDatasets = namesDatasets,
                               transforms = transforms)
    x1 <- updateCombined(x0)
    for (name in c("model", "y", "dataModels")) {
        expect_false(identical(slot(x1, name), slot(x0, name)))
    }
    for (name in c("namesDatasets", "datasets", "exposure", "transforms", "iMethodCombined", "slotsToExtract"))
        expect_true(identical(slot(x1, name), slot(x0, name)))
})

## tests equal but not identical
test_that("R, specific C, and generic C versions of updateCombined method for CombinedCountsPoissonHasExp give same answer", {
    initialCombinedCounts <- demest:::initialCombinedCounts
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    updateCombined <- demest:::updateCombined
    set.seed(10)
    y <- Counts(array(c(1:11, 20L),
                      dim = c(6, 2),
                      dimnames = list(age = 0:5, sex = c("f", "m"))))
    exposure <- Counts(array(runif(12, max = 20),
                      dim = c(6, 2),
                      dimnames = list(age = 0:5, sex = c("f", "m"))))
    model <- Model(y ~ Poisson(mean ~ age))
    data.models <- list(Model(register ~ PoissonBinomial(prob = 0.98)),
                        Model(tax ~ Binomial(mean ~ 1)))
    datasets <- list(Counts(array(c(0L, 2:12), dim = c(6, 2),
                                  dimnames = list(age = 0:5, sex = c("f", "m")))),
                         Counts(array(c(1:5, NA), dim = 6, dimnames = list(age = 0:5))))
    namesDatasets <- c("register", "tax")
    transforms <- list(makeTransform(x = y, y = datasets[[1]]),
                       makeTransform(x = y, y = datasets[[2]]))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x0 <- initialCombinedCounts(model,
                               y = y,
                               exposure = exposure,
                               dataModels = data.models,
                               datasets = datasets,
                               namesDatasets = namesDatasets,
                               transforms = transforms)
    set.seed(100)
    x1.R <- updateCombined(x0, useC = FALSE)
    set.seed(100)
    x1.C.specific <- updateCombined(x0, useC = TRUE, useSpecific = TRUE)
    set.seed(100)
    x1.C.generic <- updateCombined(x0, useC = TRUE, useSpecific = FALSE)
    if (test.identity)
        expect_identical(x1.R, x1.C.specific)
    else
        expect_equal(x1.R, x1.C.specific)
    expect_identical(x1.C.specific, x1.C.generic)
})

test_that("updateCombined method for CombinedCountsBinomial updates correct slots", {
    initialCombinedCounts <- demest:::initialCombinedCounts
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    updateCombined <- demest:::updateCombined
    set.seed(100)
    y <- Counts(array(c(1:11, 20L),
                      dim = c(6, 2),
                      dimnames = list(age = 0:5, sex = c("f", "m"))))
    exposure <- y + y
    model <- Model(y ~ Binomial(mean ~ age))
    data.models <- list(Model(register ~ PoissonBinomial(prob = 0.98)),
                        Model(tax ~ Binomial(mean ~ 1)))
    datasets <- list(Counts(array(c(0L, 2:12), dim = c(6, 2),
                                  dimnames = list(age = 0:5, sex = c("f", "m")))),
                         Counts(array(c(1:5, NA), dim = 6, dimnames = list(age = 0:5))))
    namesDatasets <- c("register", "tax")
    transforms <- list(makeTransform(x = y, y = datasets[[1]]),
                       makeTransform(x = y, y = datasets[[2]]))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x0 <- initialCombinedCounts(model,
                               y = y,
                               exposure = exposure,
                               dataModels = data.models,
                               datasets = datasets,
                               namesDatasets = namesDatasets,
                               transforms = transforms)
    x1 <- updateCombined(x0)
    for (i in 1:5)
        x1 <- updateCombined(x1)
    for (name in c("model", "y", "dataModels")) {
        expect_false(identical(slot(x1, name), slot(x0, name)))
    }
    for (name in c("namesDatasets", "datasets", "exposure", "transforms", "iMethodCombined", "slotsToExtract"))
        expect_true(identical(slot(x1, name), slot(x0, name)))
})

## tests equal but not identical
test_that("R, specific C, and generic C versions of updateCombined method for CombinedCountsBinomial give same answer", {
    initialCombinedCounts <- demest:::initialCombinedCounts
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    updateCombined <- demest:::updateCombined
    set.seed(10)
    y <- Counts(array(c(1:11, 20L),
                      dim = c(6, 2),
                      dimnames = list(age = 0:5, sex = c("f", "m"))))
    exposure <- y + y
    model <- Model(y ~ Binomial(mean ~ age))
    data.models <- list(Model(register ~ PoissonBinomial(prob = 0.98)),
                        Model(tax ~ Binomial(mean ~ 1)))
    datasets <- list(Counts(array(c(0L, 2:12), dim = c(6, 2),
                                  dimnames = list(age = 0:5, sex = c("f", "m")))),
                         Counts(array(c(1:5, NA), dim = 6, dimnames = list(age = 0:5))))
    namesDatasets <- c("register", "tax")
    transforms <- list(makeTransform(x = y, y = datasets[[1]]),
                       makeTransform(x = y, y = datasets[[2]]))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x0 <- initialCombinedCounts(model,
                               y = y,
                               exposure = exposure,
                               dataModels = data.models,
                               datasets = datasets,
                               namesDatasets = namesDatasets,
                               transforms = transforms)
    set.seed(100)
    x1.R <- updateCombined(x0, useC = FALSE)
    set.seed(100)
    x1.C.specific <- updateCombined(x0, useC = TRUE, useSpecific = TRUE)
    set.seed(100)
    x1.C.generic <- updateCombined(x0, useC = TRUE, useSpecific = FALSE)
    if (test.identity)
        expect_identical(x1.R, x1.C.specific)
    else
        expect_equal(x1.R, x1.C.specific)
    expect_identical(x1.C.specific, x1.C.generic)
})


## Accounts ##############################################################################

test_that("diffLogDensAccount works with CombinedAccountMovementsHasAge", {
    diffLogDensAccount <- demest:::diffLogDensAccount
    updateProposalAccount <- demest:::updateProposalAccount
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    set.seed(1)
    popn <- Counts(array(rpois(n = 90, lambda = 500),
                         dim = c(3, 2, 5, 3),
                         dimnames = list(age = c("0-4", "5-9", "10+"),
                                         sex = c("f", "m"),
                                         reg = 1:5,
                                         time = c(2000, 2005, 2010))))
    births <- Counts(array(rpois(n = 90, lambda = 5),
                           dim = c(1, 2, 5, 2, 2),
                           dimnames = list(age = "5-9",
                                           sex = c("m", "f"),
                                           reg = 1:5,
                                           time = c("2001-2005", "2006-2010"),
                                           triangle = c("Lower", "Upper"))))
    internal <- Counts(array(rpois(n = 300, lambda = 10),
                             dim = c(3, 2, 5, 5, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                             sex = c("m", "f"),
                                             reg_orig = 1:5,
                                             reg_dest = 1:5,
                                             time = c("2001-2005", "2006-2010"),
                                             triangle = c("Lower", "Upper"))))
    internal <- collapseOrigDest(internal, to = "pool")
    deaths <- Counts(array(rpois(n = 72, lambda = 10),
                           dim = c(3, 2, 5, 2, 2),
                           dimnames = list(age = c("0-4", "5-9", "10+"),
                                           sex = c("m", "f"),
                                           reg = 5:1,
                                           time = c("2001-2005", "2006-2010"),
                                           triangle = c("Lower", "Upper"))))
    account <- Movements(population = popn,
                         births = births,
                         internal = internal,
                         exits = list(deaths = deaths))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ age + sex, useExpose = FALSE)),
                         Model(births ~ Poisson(mean ~ 1)),
                         Model(internal ~ Poisson(mean ~ reg)),
                         Model(deaths ~ Poisson(mean ~ 1)))
    systemWeights <- list(NULL, NULL, NULL, NULL)
    census <- subarray(popn, time == "2000", drop = FALSE) + 2L
    register <- Counts(array(rpois(n = 90, lambda = popn),
                             dim = dim(popn),
                             dimnames = dimnames(popn)))
    reg.births <- Counts(array(rbinom(n = 90, size = births, prob = 0.98),
                               dim = dim(births),
                               dimnames = dimnames(births)))
    address.change <- Counts(array(rpois(n = 180, lambda = internal),
                                   dim = dim(internal),
                                   dimnames = dimnames(internal)))
    reg.deaths <- Counts(array(rbinom(n = 90, size = deaths, prob = 0.98),
                               dim = dim(deaths),
                               dimnames = dimnames(deaths))) + 1L
    datasets <- list(census, register, reg.births, address.change, reg.deaths)
    namesDatasets <- c("census", "register", "reg.births", "address.change", "reg.deaths")
    data.models <- list(Model(census ~ PoissonBinomial(prob = 0.95), series = "population"),
                              Model(register ~ Poisson(mean ~ 1), series = "population"),
                              Model(reg.births ~ PoissonBinomial(prob = 0.98), series = "births"),
                              Model(address.change ~ Poisson(mean ~ 1), series = "internal"),
                              Model(reg.deaths ~ PoissonBinomial(prob = 0.98), series = "deaths"))
    seriesIndices <- c(0L, 0L, 1L, 2L, 3L)
    transforms <- list(makeTransform(x = population(account), y = datasets[[1]], subset = TRUE),
                       makeTransform(x = population(account), y = datasets[[2]], subset = TRUE),
                       makeTransform(x = components(account, "births"), y = datasets[[3]], subset = TRUE),
                       makeTransform(x = components(account, "internal"), y = datasets[[4]], subset = TRUE),
                       makeTransform(x = components(account, "deaths"), y = datasets[[5]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x0 <- initialCombinedAccount(account = account,
                                 systemModels = systemModels,
                                 systemWeights = systemWeights,
                                 dataModels = data.models,
                                 seriesIndices = seriesIndices,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x1 <- updateProposalAccount(x0)
        if (x1@generatedNewProposal@.Data) {
            updated <- TRUE
            ans.obtained <- diffLogDensAccount(x1)
            expect_true(is.numeric(ans.obtained))
            expect_true(!is.na(ans.obtained))
        }
    }
    if (!updated)
        warning("not updated")
})

test_that("R and C versions of diffLogDensAccount give same answer with CombinedAccountMovementsHasAge", {
    diffLogDensAccount <- demest:::diffLogDensAccount
    updateProposalAccount <- demest:::updateProposalAccount
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    popn <- Counts(array(rpois(n = 90, lambda = 500),
                         dim = c(3, 2, 5, 3),
                         dimnames = list(age = c("0-4", "5-9", "10+"),
                                         sex = c("f", "m"),
                                         reg = 1:5,
                                         time = c(2000, 2005, 2010))))
    births <- Counts(array(rpois(n = 90, lambda = 5),
                           dim = c(1, 2, 5, 2, 2),
                           dimnames = list(age = "5-9",
                                           sex = c("m", "f"),
                                           reg = 1:5,
                                           time = c("2001-2005", "2006-2010"),
                                           triangle = c("Lower", "Upper"))))
    internal <- Counts(array(rpois(n = 300, lambda = 10),
                             dim = c(3, 2, 5, 5, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                             sex = c("m", "f"),
                                             reg_orig = 1:5,
                                             reg_dest = 1:5,
                                             time = c("2001-2005", "2006-2010"),
                                             triangle = c("Lower", "Upper"))))
    internal <- collapseOrigDest(internal, to = "pool")
    deaths <- Counts(array(rpois(n = 72, lambda = 10),
                           dim = c(3, 2, 5, 2, 2),
                           dimnames = list(age = c("0-4", "5-9", "10+"),
                                           sex = c("m", "f"),
                                           reg = 5:1,
                                           time = c("2001-2005", "2006-2010"),
                                           triangle = c("Lower", "Upper"))))
    account <- Movements(population = popn,
                         births = births,
                         internal = internal,
                         exits = list(deaths = deaths))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ age + sex, useExpose = FALSE)),
                         Model(births ~ Poisson(mean ~ 1)),
                         Model(internal ~ Poisson(mean ~ reg)),
                         Model(deaths ~ Poisson(mean ~ 1)))
    systemWeights <- list(NULL, NULL, NULL, NULL)
    census <- subarray(popn, time == "2000", drop = FALSE) + 2L
    register <- Counts(array(rpois(n = 90, lambda = popn),
                             dim = dim(popn),
                             dimnames = dimnames(popn)))
    reg.births <- Counts(array(rbinom(n = 90, size = births, prob = 0.98),
                               dim = dim(births),
                               dimnames = dimnames(births)))
    address.change <- Counts(array(rpois(n = 180, lambda = internal),
                                   dim = dim(internal),
                                   dimnames = dimnames(internal)))
    reg.deaths <- Counts(array(rbinom(n = 90, size = deaths, prob = 0.98),
                               dim = dim(deaths),
                               dimnames = dimnames(deaths))) + 1L
    datasets <- list(census, register, reg.births, address.change, reg.deaths)
    namesDatasets <- c("census", "register", "reg.births", "address.change", "reg.deaths")
    data.models <- list(Model(census ~ PoissonBinomial(prob = 0.95), series = "population"),
                              Model(register ~ Poisson(mean ~ 1), series = "population"),
                              Model(reg.births ~ PoissonBinomial(prob = 0.98), series = "births"),
                              Model(address.change ~ Poisson(mean ~ 1), series = "internal"),
                              Model(reg.deaths ~ PoissonBinomial(prob = 0.98), series = "deaths"))
    seriesIndices <- c(0L, 0L, 1L, 2L, 3L)
    transforms <- list(makeTransform(x = population(account), y = datasets[[1]], subset = TRUE),
                       makeTransform(x = population(account), y = datasets[[2]], subset = TRUE),
                       makeTransform(x = components(account, "births"), y = datasets[[3]], subset = TRUE),
                       makeTransform(x = components(account, "internal"), y = datasets[[4]], subset = TRUE),
                       makeTransform(x = components(account, "deaths"), y = datasets[[5]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x0 <- initialCombinedAccount(account = account,
                                 systemModels = systemModels,
                                 systemWeights = systemWeights,
                                 dataModels = data.models,
                                 seriesIndices = seriesIndices,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x1 <- updateProposalAccount(x0)
        if (x1@generatedNewProposal@.Data) {
            updated <- TRUE
            ans.R <- diffLogDensAccount(x1, useC = FALSE)
            set.seed(seed)
            ans.C.generic <- diffLogDensAccount(x1, useC = TRUE, useSpecific = FALSE)
            set.seed(seed)
            ans.C.specific <- diffLogDensAccount(x1, useC = TRUE, useSpecific = TRUE)
            if (test.identity)
                expect_identical(ans.R, ans.C.generic)
            else
                expect_equal(ans.R, ans.C.generic)
            expect_identical(ans.C.specific, ans.C.generic)
        }
    }
    if (!updated)
        warning("not updated")
})


## diffLogLikAccount

test_that("diffLogLikAccount works with CombinedAccountMovementsHasAge", {
    diffLogLikAccount <- demest:::diffLogLikAccount
    updateProposalAccount <- demest:::updateProposalAccount
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    popn <- Counts(array(rpois(n = 90, lambda = 500),
                         dim = c(3, 2, 5, 3),
                         dimnames = list(age = c("0-4", "5-9", "10+"),
                                         sex = c("f", "m"),
                                         reg = 1:5,
                                         time = c(2000, 2005, 2010))))
    births <- Counts(array(rpois(n = 90, lambda = 5),
                           dim = c(1, 2, 5, 2, 2),
                           dimnames = list(age = "5-9",
                                           sex = c("m", "f"),
                                           reg = 1:5,
                                           time = c("2001-2005", "2006-2010"),
                                           triangle = c("Lower", "Upper"))))
    internal <- Counts(array(rpois(n = 300, lambda = 10),
                             dim = c(3, 2, 5, 5, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                             sex = c("m", "f"),
                                             reg_orig = 1:5,
                                             reg_dest = 1:5,
                                             time = c("2001-2005", "2006-2010"),
                                             triangle = c("Lower", "Upper"))))
    internal <- collapseOrigDest(internal, to = "pool")
    deaths <- Counts(array(rpois(n = 72, lambda = 10),
                           dim = c(3, 2, 5, 2, 2),
                           dimnames = list(age = c("0-4", "5-9", "10+"),
                                           sex = c("m", "f"),
                                           reg = 5:1,
                                           time = c("2001-2005", "2006-2010"),
                                           triangle = c("Lower", "Upper"))))
    account <- Movements(population = popn,
                         births = births,
                         internal = internal,
                         exits = list(deaths = deaths))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ age + sex, useExpose = FALSE)),
                         Model(births ~ Poisson(mean ~ 1)),
                         Model(internal ~ Poisson(mean ~ reg)),
                         Model(deaths ~ Poisson(mean ~ 1)))
    systemWeights <- list(NULL, NULL, NULL, NULL)
    census <- subarray(popn, time == "2000", drop = FALSE) + 2L
    register <- Counts(array(rpois(n = 90, lambda = popn),
                             dim = dim(popn),
                             dimnames = dimnames(popn)))
    reg.births <- Counts(array(rbinom(n = 90, size = births, prob = 0.98),
                               dim = dim(births),
                               dimnames = dimnames(births)))
    address.change <- Counts(array(rpois(n = 180, lambda = internal),
                                   dim = dim(internal),
                                   dimnames = dimnames(internal)))
    reg.deaths <- Counts(array(rbinom(n = 90, size = deaths, prob = 0.98),
                               dim = dim(deaths),
                               dimnames = dimnames(deaths))) + 1L
    datasets <- list(census, register, reg.births, address.change, reg.deaths)
    namesDatasets <- c("census", "register", "reg.births", "address.change", "reg.deaths")
    data.models <- list(Model(census ~ PoissonBinomial(prob = 0.95), series = "population"),
                              Model(register ~ Poisson(mean ~ 1), series = "population"),
                              Model(reg.births ~ PoissonBinomial(prob = 0.98), series = "births"),
                              Model(address.change ~ Poisson(mean ~ 1), series = "internal"),
                              Model(reg.deaths ~ PoissonBinomial(prob = 0.98), series = "deaths"))
    seriesIndices <- c(0L, 0L, 1L, 2L, 3L)
    transforms <- list(makeTransform(x = population(account), y = datasets[[1]], subset = TRUE),
                       makeTransform(x = population(account), y = datasets[[2]], subset = TRUE),
                       makeTransform(x = components(account, "births"), y = datasets[[3]], subset = TRUE),
                       makeTransform(x = components(account, "internal"), y = datasets[[4]], subset = TRUE),
                       makeTransform(x = components(account, "deaths"), y = datasets[[5]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x0 <- initialCombinedAccount(account = account,
                                 systemModels = systemModels,
                                 systemWeights = systemWeights,
                                 dataModels = data.models,
                                 seriesIndices = seriesIndices,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x1 <- updateProposalAccount(x0)
        if (x1@generatedNewProposal@.Data) {
            updated <- TRUE
            ans.obtained <- diffLogLikAccount(x1)
            expect_true(is.numeric(ans.obtained))
            expect_true(!is.na(ans.obtained))
        }
    }
    if (!updated)
        warning("not updated")
})

test_that("R and C versions of diffLogLikAccount give same answer with CombinedAccountMovementsHasAge", {
    diffLogLikAccount <- demest:::diffLogLikAccount
    updateProposalAccount <- demest:::updateProposalAccount
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    popn <- Counts(array(rpois(n = 90, lambda = 500),
                         dim = c(3, 2, 5, 3),
                         dimnames = list(age = c("0-4", "5-9", "10+"),
                                         sex = c("f", "m"),
                                         reg = 1:5,
                                         time = c(2000, 2005, 2010))))
    births <- Counts(array(rpois(n = 90, lambda = 5),
                           dim = c(1, 2, 5, 2, 2),
                           dimnames = list(age = "5-9",
                                           sex = c("m", "f"),
                                           reg = 1:5,
                                           time = c("2001-2005", "2006-2010"),
                                           triangle = c("Lower", "Upper"))))
    internal <- Counts(array(rpois(n = 300, lambda = 10),
                             dim = c(3, 2, 5, 5, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                             sex = c("m", "f"),
                                             reg_orig = 1:5,
                                             reg_dest = 1:5,
                                             time = c("2001-2005", "2006-2010"),
                                             triangle = c("Lower", "Upper"))))
    internal <- collapseOrigDest(internal, to = "pool")
    deaths <- Counts(array(rpois(n = 72, lambda = 10),
                           dim = c(3, 2, 5, 2, 2),
                           dimnames = list(age = c("0-4", "5-9", "10+"),
                                           sex = c("m", "f"),
                                           reg = 5:1,
                                           time = c("2001-2005", "2006-2010"),
                                           triangle = c("Lower", "Upper"))))
    account <- Movements(population = popn,
                         births = births,
                         internal = internal,
                         exits = list(deaths = deaths))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ age + sex, useExpose = FALSE)),
                         Model(births ~ Poisson(mean ~ 1)),
                         Model(internal ~ Poisson(mean ~ reg)),
                         Model(deaths ~ Poisson(mean ~ 1)))
    systemWeights <- list(NULL, NULL, NULL, NULL)
    census <- subarray(popn, time == "2000", drop = FALSE) + 2L
    register <- Counts(array(rpois(n = 90, lambda = popn),
                             dim = dim(popn),
                             dimnames = dimnames(popn)))
    reg.births <- Counts(array(rbinom(n = 90, size = births, prob = 0.98),
                               dim = dim(births),
                               dimnames = dimnames(births)))
    address.change <- Counts(array(rpois(n = 180, lambda = internal),
                                   dim = dim(internal),
                                   dimnames = dimnames(internal)))
    reg.deaths <- Counts(array(rbinom(n = 90, size = deaths, prob = 0.98),
                               dim = dim(deaths),
                               dimnames = dimnames(deaths))) + 1L
    datasets <- list(census, register, reg.births, address.change, reg.deaths)
    namesDatasets <- c("census", "register", "reg.births", "address.change", "reg.deaths")
    data.models <- list(Model(census ~ PoissonBinomial(prob = 0.95), series = "population"),
                              Model(register ~ Poisson(mean ~ 1), series = "population"),
                              Model(reg.births ~ PoissonBinomial(prob = 0.98), series = "births"),
                              Model(address.change ~ Poisson(mean ~ 1), series = "internal"),
                              Model(reg.deaths ~ PoissonBinomial(prob = 0.98), series = "deaths"))
    seriesIndices <- c(0L, 0L, 1L, 2L, 3L)
    transforms <- list(makeTransform(x = population(account), y = datasets[[1]], subset = TRUE),
                       makeTransform(x = population(account), y = datasets[[2]], subset = TRUE),
                       makeTransform(x = components(account, "births"), y = datasets[[3]], subset = TRUE),
                       makeTransform(x = components(account, "internal"), y = datasets[[4]], subset = TRUE),
                       makeTransform(x = components(account, "deaths"), y = datasets[[5]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x0 <- initialCombinedAccount(account = account,
                                 systemModels = systemModels,
                                 systemWeights = systemWeights,
                                 dataModels = data.models,
                                 seriesIndices = seriesIndices,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x1 <- updateProposalAccount(x0)
        if (x1@generatedNewProposal@.Data) {
            updated <- TRUE
            set.seed(seed)
            ans.R <- diffLogLikAccount(x1, useC = FALSE)
            set.seed(seed)
            ans.C.generic <- diffLogLikAccount(x1, useC = TRUE, useSpecific = FALSE)
            set.seed(seed)
            ans.C.specific <- diffLogLikAccount(x1, useC = TRUE, useSpecific = TRUE)
            if (test.identity)
                expect_identical(ans.R, ans.C.generic)
            else
                expect_equal(ans.R, ans.C.generic)
            expect_identical(ans.C.specific, ans.C.generic)
        }
    }
    if (!updated)
        warning("not updated")
})

## updateProposalAccount

test_that("updateProposalAccount works with CombinedAccountMovementsHasAge", {
    updateProposalAccount <- demest:::updateProposalAccount
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    popn <- Counts(array(rpois(n = 90, lambda = 500),
                         dim = c(3, 2, 5, 3),
                         dimnames = list(age = c("0-4", "5-9", "10+"),
                                         sex = c("f", "m"),
                                         reg = 1:5,
                                         time = c(2000, 2005, 2010))))
    births <- Counts(array(rpois(n = 90, lambda = 5),
                           dim = c(1, 2, 5, 2, 2),
                           dimnames = list(age = "5-9",
                                           sex = c("m", "f"),
                                           reg = 1:5,
                                           time = c("2001-2005", "2006-2010"),
                                           triangle = c("Lower", "Upper"))))
    internal <- Counts(array(rpois(n = 300, lambda = 10),
                             dim = c(3, 2, 5, 5, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                             sex = c("m", "f"),
                                             reg_orig = 1:5,
                                             reg_dest = 1:5,
                                             time = c("2001-2005", "2006-2010"),
                                             triangle = c("Lower", "Upper"))))
    internal <- collapseOrigDest(internal, to = "pool")
    deaths <- Counts(array(rpois(n = 72, lambda = 10),
                           dim = c(3, 2, 5, 2, 2),
                           dimnames = list(age = c("0-4", "5-9", "10+"),
                                           sex = c("m", "f"),
                                           reg = 5:1,
                                           time = c("2001-2005", "2006-2010"),
                                           triangle = c("Lower", "Upper"))))
    account <- Movements(population = popn,
                         births = births,
                         internal = internal,
                         exits = list(deaths = deaths))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ age + sex, useExpose = FALSE)),
                         Model(births ~ Poisson(mean ~ 1)),
                         Model(internal ~ Poisson(mean ~ reg)),
                         Model(deaths ~ Poisson(mean ~ 1)))
    systemWeights <- list(NULL, NULL, NULL, NULL)
    census <- subarray(popn, time == "2000", drop = FALSE) + 2L
    register <- Counts(array(rpois(n = 90, lambda = popn),
                             dim = dim(popn),
                             dimnames = dimnames(popn)))
    reg.births <- Counts(array(rbinom(n = 90, size = births, prob = 0.98),
                               dim = dim(births),
                               dimnames = dimnames(births)))
    address.change <- Counts(array(rpois(n = 180, lambda = internal),
                                   dim = dim(internal),
                                   dimnames = dimnames(internal)))
    reg.deaths <- Counts(array(rbinom(n = 90, size = deaths, prob = 0.98),
                               dim = dim(deaths),
                               dimnames = dimnames(deaths))) + 1L
    datasets <- list(census, register, reg.births, address.change, reg.deaths)
    namesDatasets <- c("census", "register", "reg.births", "address.change", "reg.deaths")
    data.models <- list(Model(census ~ PoissonBinomial(prob = 0.95), series = "population"),
                              Model(register ~ Poisson(mean ~ 1), series = "population"),
                              Model(reg.births ~ PoissonBinomial(prob = 0.98), series = "births"),
                              Model(address.change ~ Poisson(mean ~ 1), series = "internal"),
                              Model(reg.deaths ~ PoissonBinomial(prob = 0.98), series = "deaths"))
    seriesIndices <- c(0L, 0L, 1L, 2L, 3L)
    transforms <- list(makeTransform(x = population(account), y = datasets[[1]], subset = TRUE),
                       makeTransform(x = population(account), y = datasets[[2]], subset = TRUE),
                       makeTransform(x = components(account, "births"), y = datasets[[3]], subset = TRUE),
                       makeTransform(x = components(account, "internal"), y = datasets[[4]], subset = TRUE),
                       makeTransform(x = components(account, "deaths"), y = datasets[[5]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x0 <- initialCombinedAccount(account = account,
                                 systemModels = systemModels,
                                 systemWeights = systemWeights,
                                 dataModels = data.models,
                                 seriesIndices = seriesIndices,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    expect_true(validObject(x0))
    expect_is(x0, "CombinedAccountMovementsHasAge")
    updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x1 <- updateProposalAccount(x0)
        if (x1@generatedNewProposal@.Data)
            updated <- TRUE
        expect_is(x1, "CombinedAccountMovementsHasAge")
        expect_true(validObject(x1))
    }
    if (!updated)
        warning("not updated")
})

test_that("R and C versions of updateProposalAccount give same answer with CombinedAccountMovementsHasAge", {
    updateProposalAccount <- demest:::updateProposalAccount
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    popn <- Counts(array(rpois(n = 90, lambda = 500),
                         dim = c(3, 2, 5, 3),
                         dimnames = list(age = c("0-4", "5-9", "10+"),
                                         sex = c("f", "m"),
                                         reg = 1:5,
                                         time = c(2000, 2005, 2010))))
    births <- Counts(array(rpois(n = 90, lambda = 5),
                           dim = c(1, 2, 5, 2, 2),
                           dimnames = list(age = "5-9",
                                           sex = c("m", "f"),
                                           reg = 1:5,
                                           time = c("2001-2005", "2006-2010"),
                                           triangle = c("Lower", "Upper"))))
    internal <- Counts(array(rpois(n = 300, lambda = 10),
                             dim = c(3, 2, 5, 5, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                             sex = c("m", "f"),
                                             reg_orig = 1:5,
                                             reg_dest = 1:5,
                                             time = c("2001-2005", "2006-2010"),
                                             triangle = c("Lower", "Upper"))))
    internal <- collapseOrigDest(internal, to = "pool")
    deaths <- Counts(array(rpois(n = 72, lambda = 10),
                           dim = c(3, 2, 5, 2, 2),
                           dimnames = list(age = c("0-4", "5-9", "10+"),
                                           sex = c("m", "f"),
                                           reg = 5:1,
                                           time = c("2001-2005", "2006-2010"),
                                           triangle = c("Lower", "Upper"))))
    account <- Movements(population = popn,
                         births = births,
                         internal = internal,
                         exits = list(deaths = deaths))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ age + sex, useExpose = FALSE)),
                         Model(births ~ Poisson(mean ~ 1)),
                         Model(internal ~ Poisson(mean ~ reg)),
                         Model(deaths ~ Poisson(mean ~ 1)))
    systemWeights <- list(NULL, NULL, NULL, NULL)
    census <- subarray(popn, time == "2000", drop = FALSE) + 2L
    register <- Counts(array(rpois(n = 90, lambda = popn),
                             dim = dim(popn),
                             dimnames = dimnames(popn)))
    reg.births <- Counts(array(rbinom(n = 90, size = births, prob = 0.98),
                               dim = dim(births),
                               dimnames = dimnames(births)))
    address.change <- Counts(array(rpois(n = 180, lambda = internal),
                                   dim = dim(internal),
                                   dimnames = dimnames(internal)))
    reg.deaths <- Counts(array(rbinom(n = 90, size = deaths, prob = 0.98),
                               dim = dim(deaths),
                               dimnames = dimnames(deaths))) + 1L
    datasets <- list(census, register, reg.births, address.change, reg.deaths)
    namesDatasets <- c("census", "register", "reg.births", "address.change", "reg.deaths")
    data.models <- list(Model(census ~ PoissonBinomial(prob = 0.95), series = "population"),
                              Model(register ~ Poisson(mean ~ 1), series = "population"),
                              Model(reg.births ~ PoissonBinomial(prob = 0.98), series = "births"),
                              Model(address.change ~ Poisson(mean ~ 1), series = "internal"),
                              Model(reg.deaths ~ PoissonBinomial(prob = 0.98), series = "deaths"))
    seriesIndices <- c(0L, 0L, 1L, 2L, 3L)
    transforms <- list(makeTransform(x = population(account), y = datasets[[1]], subset = TRUE),
                       makeTransform(x = population(account), y = datasets[[2]], subset = TRUE),
                       makeTransform(x = components(account, "births"), y = datasets[[3]], subset = TRUE),
                       makeTransform(x = components(account, "internal"), y = datasets[[4]], subset = TRUE),
                       makeTransform(x = components(account, "deaths"), y = datasets[[5]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x0 <- initialCombinedAccount(account = account,
                                 systemModels = systemModels,
                                 systemWeights = systemWeights,
                                 dataModels = data.models,
                                 seriesIndices = seriesIndices,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    expect_true(validObject(x0))
    expect_is(x0, "CombinedAccountMovementsHasAge")
    updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- updateProposalAccount(x0, useC = FALSE)
        if (ans.R@generatedNewProposal@.Data)
            updated <- TRUE
        set.seed(seed)
        ans.C.generic <- updateProposalAccount(x0, useC = TRUE, useSpecific = FALSE)
        set.seed(seed)
        ans.C.specific <- updateProposalAccount(x0, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C.generic)
        else
            expect_equal(ans.R, ans.C.generic)
        expect_identical(ans.C.specific, ans.C.generic)
    }
    if (!updated)
        warning("not updated")
})

test_that("updateValuesAccount works with CombinedAccountMovements", {
    updateValuesAccount <- demest:::updateValuesAccount
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    updateProposalAccount <- demest:::updateProposalAccount
    updateCellMove <- demest:::updateCellMove
    updateSubsequentPopnMove <- demest:::updateSubsequentPopnMove
    updateSubsequentExpMove <- demest:::updateSubsequentExpMove
    set.seed(1)
    population <- CountsOne(values = seq(200, 300, 10),
                            labels = seq(2000, 2100, 10),
                            name = "time")
    births <- CountsOne(values = rpois(n = 10, lambda = 5),
                        labels = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-"),
                        name = "time")
    deaths <- CountsOne(values = rpois(n = 10, lambda = 5),
                        labels = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-"),
                        name = "time")
    account <- Movements(population = population,
                         births = births,
                         exits = list(deaths = deaths))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ time, useExpose = FALSE)),
                         Model(births ~ Poisson(mean ~ 1)),
                         Model(deaths ~ Poisson(mean ~ 1)))
    systemWeights <- rep(list(NULL), 3)
    data.models <- list(Model(tax ~ Poisson(mean ~ 1), series = "deaths"),
                              Model(census ~ PoissonBinomial(prob = 0.9), series = "population"))
    seriesIndices <- c(2L, 0L)
    datasets <- list(Counts(array(7L,
                                  dim = 10,
                                  dimnames = list(time = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-")))),
                     Counts(array(seq.int(110L, 210L, 10L),
                                  dim = 11,
                                  dimnames = list(time = seq(2000, 2100, 10)))))
    namesDatasets <- c("tax", "census")
    transforms <- list(makeTransform(x = deaths, y = datasets[[1]], subset = TRUE),
                       makeTransform(x = population, y = datasets[[2]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x <- initialCombinedAccount(account = account,
                                 systemModels = systemModels,
                                 systemWeights = systemWeights,
                                 dataModels = data.models,
                                 seriesIndices = seriesIndices,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    expect_true(validObject(x))
    for (seed in seq_len(2 * n.test)) {
        set.seed(seed)
        while (!x@generatedNewProposal@.Data)
            x <- updateProposalAccount(x)        
        ans.obtained <- updateValuesAccount(x)
        ans.expected <- x
        ans.expected <- updateCellMove(x)
        ans.expected <- updateSubsequentPopnMove(ans.expected)
        ans.expected <- updateSubsequentExpMove(ans.expected)
        expect_identical(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of updateValuesAccount give same answer with CombinedAccountMovements", {
    updateValuesAccount <- demest:::updateValuesAccount
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    updateProposalAccount <- demest:::updateProposalAccount
    updateCellMove <- demest:::updateCellMove
    updateSubsequentPopnMove <- demest:::updateSubsequentPopnMove
    updateSubsequentExpMove <- demest:::updateSubsequentExpMove
    population <- CountsOne(values = seq(100, 200, 10),
                            labels = seq(2000, 2100, 10),
                            name = "time")
    births <- CountsOne(values = rpois(n = 10, lambda = 15),
                        labels = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-"),
                        name = "time")
    deaths <- CountsOne(values = rpois(n = 10, lambda = 5),
                        labels = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-"),
                        name = "time")
    account <- Movements(population = population,
                         births = births,
                         exits = list(deaths = deaths))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ time, useExpose = FALSE)),
                         Model(births ~ Poisson(mean ~ 1)),
                         Model(deaths ~ Poisson(mean ~ 1)))
    systemWeights <- rep(list(NULL), 3)
    data.models <- list(Model(tax ~ Poisson(mean ~ 1), series = "deaths"),
                              Model(census ~ PoissonBinomial(prob = 0.9), series = "population"))
    seriesIndices <- c(2L, 0L)
    datasets <- list(Counts(array(7L,
                                  dim = 10,
                                  dimnames = list(time = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-")))),
                     Counts(array(seq.int(110L, 210L, 10L),
                                  dim = 11,
                                  dimnames = list(time = seq(2000, 2100, 10)))))
    namesDatasets <- c("tax", "census")
    transforms <- list(makeTransform(x = deaths, y = datasets[[1]], subset = TRUE),
                       makeTransform(x = population, y = datasets[[2]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x <- initialCombinedAccount(account = account,
                                 systemModels = systemModels,
                                 systemWeights = systemWeights,
                                 dataModels = data.models,
                                 seriesIndices = seriesIndices,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    expect_true(validObject(x))
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        while (!x@generatedNewProposal@.Data)
            x <- updateProposalAccount(x)
        ans.R <- updateValuesAccount(x, useC = FALSE)
        ans.C.generic <- updateValuesAccount(x, useC = TRUE, useSpecific = FALSE)
        ans.C.specific <- updateValuesAccount(x, useC = TRUE, useSpecific = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C.generic)
        else
            expect_equal(ans.R, ans.C.generic)
        expect_identical(ans.C.specific, ans.C.generic)
    }
})


test_that("updateValuesAccount works with CombinedAccountMovements", {
    updateValuesAccount <- demest:::updateValuesAccount
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    updateProposalAccount <- demest:::updateProposalAccount
    updateCellMove <- demest:::updateCellMove
    updateSubsequentPopnMove <- demest:::updateSubsequentPopnMove
    updateSubsequentExpMove <- demest:::updateSubsequentExpMove
    set.seed(1)
    population <- CountsOne(values = seq(200, 300, 10),
                            labels = seq(2000, 2100, 10),
                            name = "time")
    births <- CountsOne(values = rpois(n = 10, lambda = 5),
                        labels = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-"),
                        name = "time")
    deaths <- CountsOne(values = rpois(n = 10, lambda = 5),
                        labels = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-"),
                        name = "time")
    account <- Movements(population = population,
                         births = births,
                         exits = list(deaths = deaths))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ time, useExpose = FALSE)),
                         Model(births ~ Poisson(mean ~ 1)),
                         Model(deaths ~ Poisson(mean ~ 1)))
    systemWeights <- rep(list(NULL), 3)
    data.models <- list(Model(tax ~ Poisson(mean ~ 1), series = "deaths"),
                              Model(census ~ PoissonBinomial(prob = 0.9), series = "population"))
    seriesIndices <- c(2L, 0L)
    datasets <- list(Counts(array(7L,
                                  dim = 10,
                                  dimnames = list(time = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-")))),
                     Counts(array(seq.int(110L, 210L, 10L),
                                  dim = 11,
                                  dimnames = list(time = seq(2000, 2100, 10)))))
    namesDatasets <- c("tax", "census")
    transforms <- list(makeTransform(x = deaths, y = datasets[[1]], subset = TRUE),
                       makeTransform(x = population, y = datasets[[2]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x <- initialCombinedAccount(account = account,
                                 systemModels = systemModels,
                                 systemWeights = systemWeights,
                                 dataModels = data.models,
                                 seriesIndices = seriesIndices,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    expect_true(validObject(x))
    for (seed in seq_len(2 * n.test)) {
        set.seed(seed)
        while (!x@generatedNewProposal@.Data)
            x <- updateProposalAccount(x)
        ans.obtained <- updateValuesAccount(x)
        ans.expected <- x
        ans.expected <- updateCellMove(x)
        ans.expected <- updateSubsequentPopnMove(ans.expected)
        ans.expected <- updateSubsequentExpMove(ans.expected)
        expect_identical(ans.obtained, ans.expected)
    }
})

test_that("updatedExpectedExposure works with CombinedAccountMovements - no age", {
    updateExpectedExposure <- demest:::updateExpectedExposure
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    updateCellMove <- demest:::updateCellMove
    updateSubsequentPopnMove <- demest:::updateSubsequentPopnMove
    updateSubsequentExpMove <- demest:::updateSubsequentExpMove
    updateProposalAccount <- demest:::updateProposalAccount
    set.seed(1)
    ## time first of 3 dimensions
    population <- Counts(array(101:112,
                               dim = c(3, 2, 2),
                               dimnames = list(time = c(2000, 2010, 2020),
                                               reg = c("a", "b"),
                                               sex = c("f", "m"))))
    arrivals <- Counts(array(1:8,
                             dim = c(2, 2, 2),
                             dimnames = list(time = c("2001-2010", "2011-2020"),
                                             reg = c("a", "b"),
                                             sex = c("f", "m"))))
    account <- Movements(population = population,
                         entries = list(arrivals = arrivals))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ time, useExpose = FALSE)),
                         Model(arrivals ~ Poisson(mean ~ 1)))
    systemWeights <- rep(list(NULL), 2)
    data.models <- list(Model(census ~ PoissonBinomial(prob = 0.9), series = "population"))
    seriesIndices <- 0L
    datasets <- list(population + 1L)
    namesDatasets <- "census"
    transforms <- list(makeTransform(x = population, y = datasets[[1]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x <- initialCombinedAccount(account = account,
                                 systemModels = systemModels,
                                 systemWeights = systemWeights,
                                 dataModels = data.models,
                                 seriesIndices = seriesIndices,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    expect_true(validObject(x))
    ans.obtained <- updateExpectedExposure(x)@expectedExposure@.Data
    theta.popn <- Counts(array(x@systemModels[[1]]@theta,
                               dim = dim(population),
                               dimnames = dimnames(population)))
    ans.expected <- exposure(theta.popn)@.Data
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
    ## time second of 3 dimensions
    population <- Counts(array(101:112,
                               dim = c(3, 2, 2),
                               dimnames = list(time = c(2000, 2010, 2020),
                                               reg = c("a", "b"),
                                               sex = c("f", "m"))))
    arrivals <- Counts(array(1:8,
                             dim = c(2, 2, 2),
                             dimnames = list(time = c("2001-2010", "2011-2020"),
                                             reg = c("a", "b"),
                                             sex = c("f", "m"))))
    population <- aperm(population, aperm =  c(2, 1, 3))
    arrivals <- aperm(arrivals, aperm = c(2, 1, 3))
    account <- Movements(population = population,
                         entries = list(arrivals = arrivals))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ time, useExpose = FALSE)),
                         Model(arrivals ~ Poisson(mean ~ 1)))
    systemWeights <- rep(list(NULL), 2)
    data.models <- list(Model(census ~ PoissonBinomial(prob = 0.9), series = "population"))
    seriesIndices <- 0L
    datasets <- list(population + 1L)
    namesDatasets <- "census"
    transforms <- list(makeTransform(x = population, y = datasets[[1]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x <- initialCombinedAccount(account = account,
                                 systemModels = systemModels,
                                 systemWeights = systemWeights,
                                 dataModels = data.models,
                                 seriesIndices = seriesIndices,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    expect_true(validObject(x))
    ans.obtained <- updateExpectedExposure(x)@expectedExposure@.Data
    theta.popn <- Counts(array(x@systemModels[[1]]@theta,
                               dim = dim(population),
                               dimnames = dimnames(population)))
    ans.expected <- exposure(theta.popn)@.Data
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)        
    ## time third of 3 dimensions
    population <- Counts(array(101:112,
                               dim = c(3, 2, 2),
                               dimnames = list(time = c(2000, 2010, 2020),
                                               reg = c("a", "b"),
                                               sex = c("f", "m"))))
    arrivals <- Counts(array(1:8,
                             dim = c(2, 2, 2),
                             dimnames = list(time = c("2001-2010", "2011-2020"),
                                             reg = c("a", "b"),
                                             sex = c("f", "m"))))
    population <- aperm(population, aperm =  3:1)
    arrivals <- aperm(arrivals, aperm = 3:1)
    account <- Movements(population = population,
                         entries = list(arrivals = arrivals))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ time, useExpose = FALSE)),
                         Model(arrivals ~ Poisson(mean ~ 1)))
    systemWeights <- rep(list(NULL), 2)
    data.models <- list(Model(census ~ PoissonBinomial(prob = 0.9), series = "population"))
    seriesIndices <- 0L
    datasets <- list(population + 1L)
    namesDatasets <- "census"
    transforms <- list(makeTransform(x = population, y = datasets[[1]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x <- initialCombinedAccount(account = account,
                                 systemModels = systemModels,
                                 systemWeights = systemWeights,
                                 dataModels = data.models,
                                 seriesIndices = seriesIndices,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    expect_true(validObject(x))
    ans.obtained <- updateExpectedExposure(x)@expectedExposure@.Data
    theta.popn <- Counts(array(x@systemModels[[1]]@theta,
                               dim = dim(population),
                               dimnames = dimnames(population)))
    ans.expected <- exposure(theta.popn)@.Data
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)        
})

test_that("R and C versions of updatedExpectedExposure give same answer with CombinedAccountMovements - no age", {
    updateExpectedExposure <- demest:::updateExpectedExposure
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    updateProposalAccount <- demest:::updateProposalAccount
    ## time first of 3 dimensions
    population <- Counts(array(101:112,
                               dim = c(3, 2, 2),
                               dimnames = list(time = c(2000, 2010, 2020),
                                               reg = c("a", "b"),
                                               sex = c("f", "m"))))
    arrivals <- Counts(array(1:8,
                             dim = c(2, 2, 2),
                             dimnames = list(time = c("2001-2010", "2011-2020"),
                                             reg = c("a", "b"),
                                             sex = c("f", "m"))))
    account <- Movements(population = population,
                         entries = list(arrivals = arrivals))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ time, useExpose = FALSE)),
                         Model(arrivals ~ Poisson(mean ~ 1)))
    systemWeights <- rep(list(NULL), 2)
    data.models <- list(Model(census ~ PoissonBinomial(prob = 0.9), series = "population"))
    seriesIndices <- 0L
    datasets <- list(population + 1L)
    namesDatasets <- "census"
    transforms <- list(makeTransform(x = population, y = datasets[[1]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x <- initialCombinedAccount(account = account,
                                 systemModels = systemModels,
                                 systemWeights = systemWeights,
                                 dataModels = data.models,
                                 seriesIndices = seriesIndices,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    ans.R <- updateExpectedExposure(x, useC = FALSE)
    ans.C <- updateExpectedExposure(x, useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
    ## time second of 3 dimensions
    population <- Counts(array(101:112,
                               dim = c(3, 2, 2),
                               dimnames = list(time = c(2000, 2010, 2020),
                                               reg = c("a", "b"),
                                               sex = c("f", "m"))))
    arrivals <- Counts(array(1:8,
                             dim = c(2, 2, 2),
                             dimnames = list(time = c("2001-2010", "2011-2020"),
                                             reg = c("a", "b"),
                                             sex = c("f", "m"))))
    population <- aperm(population, aperm =  c(2, 1, 3))
    arrivals <- aperm(arrivals, aperm = c(2, 1, 3))
    account <- Movements(population = population,
                         entries = list(arrivals = arrivals))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ time, useExpose = FALSE)),
                         Model(arrivals ~ Poisson(mean ~ 1)))
    systemWeights <- rep(list(NULL), 2)
    data.models <- list(Model(census ~ PoissonBinomial(prob = 0.9), series = "population"))
    seriesIndices <- 0L
    datasets <- list(population + 1L)
    namesDatasets <- "census"
    transforms <- list(makeTransform(x = population, y = datasets[[1]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x <- initialCombinedAccount(account = account,
                                 systemModels = systemModels,
                                 systemWeights = systemWeights,
                                 dataModels = data.models,
                                 seriesIndices = seriesIndices,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    ans.R <- updateExpectedExposure(x, useC = FALSE)
    ans.C <- updateExpectedExposure(x, useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
    ## time third of 3 dimensions
    population <- Counts(array(101:112,
                               dim = c(3, 2, 2),
                               dimnames = list(time = c(2000, 2010, 2020),
                                               reg = c("a", "b"),
                                               sex = c("f", "m"))))
    arrivals <- Counts(array(1:8,
                             dim = c(2, 2, 2),
                             dimnames = list(time = c("2001-2010", "2011-2020"),
                                             reg = c("a", "b"),
                                             sex = c("f", "m"))))
    population <- aperm(population, aperm =  3:1)
    arrivals <- aperm(arrivals, aperm = 3:1)
    account <- Movements(population = population,
                         entries = list(arrivals = arrivals))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ time, useExpose = FALSE)),
                         Model(arrivals ~ Poisson(mean ~ 1)))
    systemWeights <- rep(list(NULL), 2)
    data.models <- list(Model(census ~ PoissonBinomial(prob = 0.9), series = "population"))
    seriesIndices <- 0L
    datasets <- list(population + 1L)
    namesDatasets <- "census"
    transforms <- list(makeTransform(x = population, y = datasets[[1]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x <- initialCombinedAccount(account = account,
                                 systemModels = systemModels,
                                 systemWeights = systemWeights,
                                 dataModels = data.models,
                                 seriesIndices = seriesIndices,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    ans.R <- updateExpectedExposure(x, useC = FALSE)
    ans.C <- updateExpectedExposure(x, useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
})

test_that("updatedExpectedExposure works with CombinedAccountMovementsHasAge", {
    updateExpectedExposure <- demest:::updateExpectedExposure
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    updateProposalAccount <- demest:::updateProposalAccount
    ## time first of 3 dimensions
    population <- Counts(array(101:112,
                               dim = c(3, 2, 2),
                               dimnames = list(time = c(2000, 2010, 2020),
                                               age = c("0-9", "10+"),
                                               sex = c("f", "m"))))
    arrivals <- Counts(array(1:8,
                             dim = c(2, 2, 2),
                             dimnames = list(time = c("2001-2010", "2011-2020"),
                                             age = c("0-9", "10+"),
                                             sex = c("f", "m"))))
    account <- Movements(population = population,
                         entries = list(arrivals = arrivals))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ time, useExpose = FALSE)),
                         Model(arrivals ~ Poisson(mean ~ 1)))
    systemWeights <- rep(list(NULL), 2)
    data.models <- list(Model(census ~ PoissonBinomial(prob = 0.9), series = "population"))
    seriesIndices <- 0L
    datasets <- list(population + 1L)
    namesDatasets <- "census"
    transforms <- list(makeTransform(x = population, y = datasets[[1]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x <- initialCombinedAccount(account = account,
                                 systemModels = systemModels,
                                 systemWeights = systemWeights,
                                 dataModels = data.models,
                                 seriesIndices = seriesIndices,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    expect_true(validObject(x))
    ans.obtained <- updateExpectedExposure(x)@expectedExposure@.Data
    theta.popn <- Counts(array(x@systemModels[[1]]@theta,
                               dim = dim(population),
                               dimnames = dimnames(population)))
    ans.expected <- exposure(theta.popn, triangles = TRUE)@.Data
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
    ## time second of 3 dimensions
    population <- Counts(array(101:112,
                               dim = c(3, 2, 2),
                               dimnames = list(time = c(2000, 2010, 2020),
                                               age = c("0-9", "10+"),
                                               sex = c("f", "m"))))
    arrivals <- Counts(array(1:8,
                             dim = c(2, 2, 2),
                             dimnames = list(time = c("2001-2010", "2011-2020"),
                                             age = c("0-9", "10+"),
                                             sex = c("f", "m"))))
    population <- aperm(population, aperm =  c(2, 1, 3))
    arrivals <- aperm(arrivals, aperm = c(2, 1, 3))
    account <- Movements(population = population,
                         entries = list(arrivals = arrivals))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ time, useExpose = FALSE)),
                         Model(arrivals ~ Poisson(mean ~ 1)))
    systemWeights <- rep(list(NULL), 2)
    data.models <- list(Model(census ~ PoissonBinomial(prob = 0.9), series = "population"))
    seriesIndices <- 0L
    datasets <- list(population + 1L)
    namesDatasets <- "census"
    transforms <- list(makeTransform(x = population, y = datasets[[1]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x <- initialCombinedAccount(account = account,
                                 systemModels = systemModels,
                                 systemWeights = systemWeights,
                                 dataModels = data.models,
                                 seriesIndices = seriesIndices,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    expect_true(validObject(x))
    ans.obtained <- updateExpectedExposure(x)@expectedExposure@.Data
    theta.popn <- Counts(array(x@systemModels[[1]]@theta,
                               dim = dim(population),
                               dimnames = dimnames(population)))
    ans.expected <- exposure(theta.popn, triangles = TRUE)@.Data
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)        
    ## time third of 3 dimensions
    population <- Counts(array(101:112,
                               dim = c(3, 2, 2),
                               dimnames = list(time = c(2000, 2010, 2020),
                                             age = c("0-9", "10+"),
                                               sex = c("f", "m"))))
    arrivals <- Counts(array(1:8,
                             dim = c(2, 2, 2),
                             dimnames = list(time = c("2001-2010", "2011-2020"),
                                             age = c("0-9", "10+"),
                                             sex = c("f", "m"))))
    population <- aperm(population, aperm =  3:1)
    arrivals <- aperm(arrivals, aperm = 3:1)
    account <- Movements(population = population,
                         entries = list(arrivals = arrivals))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ time, useExpose = FALSE)),
                         Model(arrivals ~ Poisson(mean ~ 1)))
    systemWeights <- rep(list(NULL), 2)
    data.models <- list(Model(census ~ PoissonBinomial(prob = 0.9), series = "population"))
    seriesIndices <- 0L
    datasets <- list(population + 1L)
    namesDatasets <- "census"
    transforms <- list(makeTransform(x = population, y = datasets[[1]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x <- initialCombinedAccount(account = account,
                                 systemModels = systemModels,
                                 systemWeights = systemWeights,
                                 dataModels = data.models,
                                 seriesIndices = seriesIndices,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    expect_true(validObject(x))
    ans.obtained <- updateExpectedExposure(x)@expectedExposure@.Data
    theta.popn <- Counts(array(x@systemModels[[1]]@theta,
                               dim = dim(population),
                               dimnames = dimnames(population)))
    ans.expected <- exposure(theta.popn, triangles = TRUE)@.Data
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)        
})

test_that("R and C give same answer for updatedExpectedExposure with CombinedAccountMovementsHasAge", {
    updateExpectedExposure <- demest:::updateExpectedExposure
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    updateProposalAccount <- demest:::updateProposalAccount
    ## time first of 3 dimensions
    population <- Counts(array(101:112,
                               dim = c(3, 2, 2),
                               dimnames = list(time = c(2000, 2010, 2020),
                                               age = c("0-9", "10+"),
                                               sex = c("f", "m"))))
    arrivals <- Counts(array(1:8,
                             dim = c(2, 2, 2),
                             dimnames = list(time = c("2001-2010", "2011-2020"),
                                             age = c("0-9", "10+"),
                                             sex = c("f", "m"))))
    account <- Movements(population = population,
                         entries = list(arrivals = arrivals))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ time, useExpose = FALSE)),
                         Model(arrivals ~ Poisson(mean ~ 1)))
    systemWeights <- rep(list(NULL), 2)
    data.models <- list(Model(census ~ PoissonBinomial(prob = 0.9), series = "population"))
    seriesIndices <- 0L
    datasets <- list(population + 1L)
    namesDatasets <- "census"
    transforms <- list(makeTransform(x = population, y = datasets[[1]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x <- initialCombinedAccount(account = account,
                                 systemModels = systemModels,
                                 systemWeights = systemWeights,
                                 dataModels = data.models,
                                 seriesIndices = seriesIndices,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    ans.R <- updateExpectedExposure(x, useC = FALSE)
    ans.C <- updateExpectedExposure(x, useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
    ## time second of 3 dimensions
    population <- Counts(array(101:112,
                               dim = c(3, 2, 2),
                               dimnames = list(time = c(2000, 2010, 2020),
                                               age = c("0-9", "10+"),
                                               sex = c("f", "m"))))
    arrivals <- Counts(array(1:8,
                             dim = c(2, 2, 2),
                             dimnames = list(time = c("2001-2010", "2011-2020"),
                                             age = c("0-9", "10+"),
                                             sex = c("f", "m"))))
    population <- aperm(population, aperm =  c(2, 1, 3))
    arrivals <- aperm(arrivals, aperm = c(2, 1, 3))
    account <- Movements(population = population,
                         entries = list(arrivals = arrivals))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ time, useExpose = FALSE)),
                         Model(arrivals ~ Poisson(mean ~ 1)))
    systemWeights <- rep(list(NULL), 2)
    data.models <- list(Model(census ~ PoissonBinomial(prob = 0.9), series = "population"))
    seriesIndices <- 0L
    datasets <- list(population + 1L)
    namesDatasets <- "census"
    transforms <- list(makeTransform(x = population, y = datasets[[1]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x <- initialCombinedAccount(account = account,
                                 systemModels = systemModels,
                                 systemWeights = systemWeights,
                                 dataModels = data.models,
                                 seriesIndices = seriesIndices,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    ans.R <- updateExpectedExposure(x, useC = FALSE)
    ans.C <- updateExpectedExposure(x, useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
    ## time third of 3 dimensions
    population <- Counts(array(101:112,
                               dim = c(3, 2, 2),
                               dimnames = list(time = c(2000, 2010, 2020),
                                             age = c("0-9", "10+"),
                                               sex = c("f", "m"))))
    arrivals <- Counts(array(1:8,
                             dim = c(2, 2, 2),
                             dimnames = list(time = c("2001-2010", "2011-2020"),
                                             age = c("0-9", "10+"),
                                             sex = c("f", "m"))))
    population <- aperm(population, aperm =  3:1)
    arrivals <- aperm(arrivals, aperm = 3:1)
    account <- Movements(population = population,
                         entries = list(arrivals = arrivals))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ time, useExpose = FALSE)),
                         Model(arrivals ~ Poisson(mean ~ 1)))
    systemWeights <- rep(list(NULL), 2)
    data.models <- list(Model(census ~ PoissonBinomial(prob = 0.9), series = "population"))
    seriesIndices <- 0L
    datasets <- list(population + 1L)
    namesDatasets <- "census"
    transforms <- list(makeTransform(x = population, y = datasets[[1]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x <- initialCombinedAccount(account = account,
                                 systemModels = systemModels,
                                 systemWeights = systemWeights,
                                 dataModels = data.models,
                                 seriesIndices = seriesIndices,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    ans.R <- updateExpectedExposure(x, useC = FALSE)
    ans.C.specific <- updateExpectedExposure(x, useC = TRUE, useSpecific = TRUE)
    ans.C.generic <- updateExpectedExposure(x, useC = TRUE, useSpecific = FALSE)
    if (test.identity)
        expect_identical(ans.R, ans.C.specific)
    else
        expect_equal(ans.R, ans.C.specific)
    expect_identical(ans.C.specific, ans.C.generic)
})

test_that("updateSystemModels works with CombinedAccountMovements", {
    updateSystemModels <- demest:::updateSystemModels    
    updateAccount <- demest:::updateAccount
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    updateModelNotUseExp <- demest:::updateModelNotUseExp
    updateModelUseExp <- demest:::updateModelUseExp
    collapse <- dembase::collapse
    ## Possibilities for models: 
    ## uses exposure, is births, does not have transform from the transforms.exp.to.comp list DONE
    ## uses exposure, is births, has transform from the transforms.exp.to.comp list 
    ## uses exposure, not is births, does not have transform from the transforms.exp.to.comp list DONE
    ## uses exposure, not is births, has transform from the transforms.exp.to.comp list
    ## does not use exposure, is Normal DONE
    ## does not use exposure, is not Normal DONE
    ## CASE 1 - NO TRANSFORMS IN transform.exp.to.comp LIST
    set.seed(0)
    population <- Counts(array(seq(1000L, 1500L, 100L),
                               dim = c(3, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                               time = c(2000, 2005))))
    births <- Counts(array(10L,
                           dim = c(3, 1),
                           dimnames = list(reg = c("a", "b", "c"),
                                           time = c("2001-2005"))))
    deaths <- Counts(array(c(10L, 5L, 3L),
                           dim = c(3, 1),
                           dimnames = list(reg = c("a", "b", "c"),
                                           time = "2001-2005")))
    internal <- Counts(array(c(0L, 50L, 40L,
                               20L, 0L, 30L,
                               60L, 20L, 0L),
                             dim = c(3, 3, 1),
                             dimnames = list(reg_orig = c("a", "b", "c"),
                                             reg_dest = c("a", "b", "c"),
                                             time = "2001-2005")))
    internal <- collapseOrigDest(internal, to = "net")
    account <- Movements(population = population,
                         births = births,
                         internal = internal,
                         exits = list(deaths = deaths))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ reg, useExpose = FALSE)),
                         Model(births ~ Poisson(mean ~ 1)),
                         Model(internal ~ Normal(mean ~ 1)),
                         Model(deaths ~ Poisson(mean ~ reg)))
    systemWeights <- list(NULL,
                          NULL,
                          Counts(array(1,
                                       dim = c(3, 1),
                                       dimnames = list(reg = c("a", "b", "c"),
                                                       time = "2001-2005"))),
                          NULL)
    mean <- ValuesOne(1, labels = "2001-2005", name = "time")
    data.models <- list(Model(tax ~ NormalFixed(mean = mean, sd = 0.1), series = "internal"),
                        Model(census ~ PoissonBinomial(prob = 0.9), series = "population"))
    seriesIndices <- c(2L, 0L)
    datasets <- list(internal + 10L,
                     population - 5L)
    namesDatasets <- c("tax", "census")
    transforms <- list(makeTransform(x = internal, y = datasets[[1]], subset = TRUE),
                       makeTransform(x = population, y = datasets[[2]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x0 <- initialCombinedAccount(account = account,
                                 systemModels = systemModels,
                                 systemWeights = systemWeights,
                                 dataModels = data.models,
                                 seriesIndices = seriesIndices,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    x1 <- updateAccount(x0)
    set.seed(1)
    ans.obtained <- updateSystemModels(x1)
    set.seed(1)
    ans.expected <- x1
    ans.expected@systemModels[[1L]] <- updateModelNotUseExp(ans.expected@systemModels[[1]],
                                                            y = ans.expected@account@population)
    ans.expected@systemModels[[2L]] <- updateModelUseExp(ans.expected@systemModels[[2]],
                                                         y = ans.expected@account@components[[1]],
                                                         exposure = ans.expected@exposure)
    ans.expected@systemModels[[3L]] <- updateModelNotUseExp(ans.expected@systemModels[[3]],
                                                            y = toDouble(ans.expected@account@components[[2]]))
    ans.expected@systemModels[[4L]] <- updateModelUseExp(ans.expected@systemModels[[4]],
                                                         y = ans.expected@account@components[[3]],
                                                         exposure = ans.expected@exposure)
    if (test.identity) {
        expect_identical(ans.obtained, ans.expected)
    } else {
        expect_equal(ans.obtained, ans.expected)
    }
    ## CASE 2 - TRANSFORMS IN transform.exp.to.comp LIST
    population <- Counts(array(101:108,
                               dim = c(2, 2, 2),
                               dimnames = list(reg = c("a", "b"),
                                               age = c("0-9", "10+"),
                                               time = c(2000, 2010))))
    births <- Counts(array(c(50L, 40L,
                             60L, 40L),
                           dim = c(2, 2, 1, 1),
                           dimnames = list(reg_parent = c("a", "b"),
                                           reg_child = c("a", "b"),
                                           age = "10+",
                                           time = "2001-2010")))
    internal <- Counts(array(c(0L, 2L,
                               1L, 0L,
                               0L, 3L,
                               2L, 0L),
                             dim = c(2, 2, 2, 1),
                             dimnames = list(reg_orig = c("a", "b"),
                                             reg_dest = c("a", "b"),
                                             age = c("0-9", "10+"),
                                             time = "2001-2010")))
    account <- Movements(population = population,
                         births = births,
                         internal = internal)
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ time, useExpose = FALSE)),
                         Model(births ~ Poisson(mean ~ 1)),
                         Model(internal ~ Poisson(mean ~ 1, structuralZeros = "diag")))
    systemWeights <- rep(list(NULL), 3)
    data.models <- list(Model(census ~ PoissonBinomial(prob = 0.9), series = "population"))
    seriesIndices <- 0L
    datasets <- list(population + 1L)
    namesDatasets <- "census"
    transforms <- list(makeTransform(x = population, y = datasets[[1]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    set.seed(0)
    x0 <- initialCombinedAccount(account = account,
                                 systemModels = systemModels,
                                 systemWeights = systemWeights,
                                 dataModels = data.models,
                                 seriesIndices = seriesIndices,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    set.seed(0)
    x1 <- updateAccount(x0)
    set.seed(1)
    ans.obtained <- updateSystemModels(x1)
    set.seed(1)
    ans.expected <- x1
    ans.expected@systemModels[[1L]] <- updateModelNotUseExp(ans.expected@systemModels[[1]],
                                                            y = ans.expected@account@population)
    expose.births <- Counts(array(extend(collapse(ans.expected@exposure,
                                                  transform = ans.expected@transformExpToBirths),
                                         transform = ans.expected@transformsExpToComp[[1]]),
                                  dim = dim(x1@account@components[[1]]),
                                  dimnames = dimnames(x1@account@components[[1]])))
    ans.expected@systemModels[[2L]] <- updateModelUseExp(ans.expected@systemModels[[2]],
                                                         y = ans.expected@account@components[[1]],
                                                         exposure = toDouble(expose.births))
    expose.internal <- Counts(array(extend(ans.expected@exposure,
                                           transform = ans.expected@transformsExpToComp[[2]]),
                                  dim = dim(x1@account@components[[2]]),
                                  dimnames = dimnames(x1@account@components[[2]])))
    ans.expected@systemModels[[3L]] <- updateModelUseExp(ans.expected@systemModels[[3]],
                                                         y = ans.expected@account@components[[2]],
                                                         exposure = toDouble(expose.internal))
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})


test_that("R and C versions of updateSystemModels give same answer with CombinedAccountMovements", {
    updateSystemModels <- demest:::updateSystemModels    
    updateAccount <- demest:::updateAccount
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    updateModelNotUseExp <- demest:::updateModelNotUseExp
    updateModelUseExp <- demest:::updateModelUseExp
    ## Possibilities for models: 
    ## uses exposure, is births, does not have transform from the transforms.exp.to.comp list DONE
    ## uses exposure, is births, has transform from the transforms.exp.to.comp list 
    ## uses exposure, not is births, does not have transform from the transforms.exp.to.comp list DONE
    ## uses exposure, not is births, has transform from the transforms.exp.to.comp list
    ## does not use exposure, is Normal DONE
    ## does not use exposure, is not Normal DONE
    ## CASE 1 - NO TRANSFORMS IN transform.exp.to.comp LIST
    set.seed(0)
    population <- Counts(array(seq(1000L, 1500L, 100L),
                               dim = c(3, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                               time = c(2000, 2005))))
    births <- Counts(array(10L,
                           dim = c(3, 1),
                           dimnames = list(reg = c("a", "b", "c"),
                                           time = c("2001-2005"))))
    deaths <- Counts(array(c(10L, 5L, 3L),
                           dim = c(3, 1),
                           dimnames = list(reg = c("a", "b", "c"),
                                           time = "2001-2005")))
    internal <- Counts(array(c(0L, 50L, 40L,
                               20L, 0L, 30L,
                               60L, 20L, 0L),
                             dim = c(3, 3, 1),
                             dimnames = list(reg_orig = c("a", "b", "c"),
                                             reg_dest = c("a", "b", "c"),
                                             time = "2001-2005")))
    internal <- collapseOrigDest(internal, to = "net")
    account <- Movements(population = population,
                         births = births,
                         internal = internal,
                         exits = list(deaths = deaths))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ reg, useExpose = FALSE)),
                         Model(births ~ Poisson(mean ~ 1)),
                         Model(internal ~ Normal(mean ~ 1)),
                         Model(deaths ~ Poisson(mean ~ reg)))
    systemWeights <- list(NULL,
                          NULL,
                          Counts(array(1,
                                       dim = c(3, 1),
                                       dimnames = list(reg = c("a", "b", "c"),
                                                       time = "2001-2005"))),
                          NULL)
    mean <- ValuesOne(1, labels = "2001-2005", name = "time")
    data.models <- list(Model(tax ~ NormalFixed(mean = mean, sd = 0.1), series = "internal"),
                        Model(census ~ PoissonBinomial(prob = 0.9), series = "population"))
    seriesIndices <- c(2L, 0L)
    datasets <- list(internal + 10L,
                     population - 5L)
    namesDatasets <- c("tax", "census")
    transforms <- list(makeTransform(x = internal, y = datasets[[1]], subset = TRUE),
                       makeTransform(x = population, y = datasets[[2]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x0 <- initialCombinedAccount(account = account,
                                 systemModels = systemModels,
                                 systemWeights = systemWeights,
                                 dataModels = data.models,
                                 seriesIndices = seriesIndices,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    x1 <- updateAccount(x0)
    set.seed(1)
    ans.obtained <- updateSystemModels(x1)
    set.seed(1)
    ans.R <- updateSystemModels(x1, useC = FALSE)
    set.seed(1)
    ans.C.specific <- updateSystemModels(x1, useC = TRUE, useSpecific = TRUE)
    set.seed(1)
    ans.C.generic <- updateSystemModels(x1, useC = TRUE, useSpecific = FALSE)
    if (test.identity)
        expect_identical(ans.R, ans.C.specific)
    else
        expect_equal(ans.R, ans.C.specific)
    expect_identical(ans.C.specific, ans.C.generic)
    ## CASE 2 - TRANSFORMS IN transform.exp.to.comp LIST
    population <- Counts(array(101:108,
                               dim = c(2, 2, 2),
                               dimnames = list(reg = c("a", "b"),
                                               age = c("0-9", "10+"),
                                               time = c(2000, 2010))))
    births <- Counts(array(c(50L, 40L,
                             60L, 40L),
                           dim = c(2, 2, 1, 1),
                           dimnames = list(reg_parent = c("a", "b"),
                                           reg_child = c("a", "b"),
                                           age = "10+",
                                           time = "2001-2010")))
    internal <- Counts(array(c(0L, 2L,
                               1L, 0L,
                               0L, 3L,
                               2L, 0L),
                             dim = c(2, 2, 2, 1),
                             dimnames = list(reg_orig = c("a", "b"),
                                             reg_dest = c("a", "b"),
                                             age = c("0-9", "10+"),
                                             time = "2001-2010")))
    account <- Movements(population = population,
                         births = births,
                         internal = internal)
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ time, useExpose = FALSE)),
                         Model(births ~ Poisson(mean ~ 1)),
                         Model(internal ~ Poisson(mean ~ 1, structuralZeros = "diag")))
    systemWeights <- rep(list(NULL), 3)
    data.models <- list(Model(census ~ PoissonBinomial(prob = 0.9), series = "population"))
    seriesIndices <- 0L
    datasets <- list(population + 1L)
    namesDatasets <- "census"
    transforms <- list(makeTransform(x = population, y = datasets[[1]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    set.seed(0)
    x0 <- initialCombinedAccount(account = account,
                                 systemModels = systemModels,
                                 systemWeights = systemWeights,
                                 dataModels = data.models,
                                 seriesIndices = seriesIndices,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    set.seed(0)
    x1 <- updateAccount(x0)
    set.seed(1)
    ans.obtained <- updateSystemModels(x1)
    set.seed(1)
    ans.R <- updateSystemModels(x1, useC = FALSE)
    set.seed(1)
    ans.C.specific <- updateSystemModels(x1, useC = TRUE, useSpecific = TRUE)
    set.seed(1)
    ans.C.generic <- updateSystemModels(x1, useC = TRUE, useSpecific = FALSE)
    if (test.identity)
        expect_identical(ans.R, ans.C.specific)
    else
        expect_equal(ans.R, ans.C.specific)
    expect_identical(ans.C.specific, ans.C.generic)
})


test_that("updateCombined works with CombinedAccountMovements", {
    updateCombined <- demest:::updateCombined
    updateAccount <- demest:::updateAccount
    updateSystemModels <- demest:::updateSystemModels
    updateExpectedExposure <- demest:::updateExpectedExposure
    updateDataModelsAccount <- demest:::updateDataModelsAccount
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    updateModelNotUseExp <- demest:::updateModelNotUseExp
    updateModelUseExp <- demest:::updateModelUseExp
    population <- Counts(array(seq(1000L, 1500L, 100L),
                               dim = c(3, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                               time = c(2000, 2005))))
    deaths <- Counts(array(c(10L, 5L, 3L),
                           dim = c(3, 1),
                           dimnames = list(reg = c("a", "b", "c"),
                                           time = "2001-2005")))
    internal <- Counts(array(c(0L, 50L, 40L,
                               20L, 0L, 30L,
                               60L, 20L, 0L),
                             dim = c(3, 3, 1),
                             dimnames = list(reg_orig = c("a", "b", "c"),
                                             reg_dest = c("a", "b", "c"),
                                             time = "2001-2005")))
    internal <- collapseOrigDest(internal, to = "net")
    account <- Movements(population = population,
                         internal = internal,
                         exits = list(deaths = deaths))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ reg, useExpose = FALSE)),
                         Model(internal ~ Normal(mean ~ 1)),
                         Model(deaths ~ Poisson(mean ~ reg)))
    systemWeights <- list(NULL,
                          Counts(array(1,
                                       dim = c(3, 1),
                                       dimnames = list(reg = c("a", "b", "c"),
                                                       time = "2001-2005"))),
                          NULL)
    mean <- ValuesOne(1, labels = "2001-2005", name = "time")
    data.models <- list(Model(tax ~ NormalFixed(mean = mean, sd = 0.1), series = "internal"),
                              Model(census ~ PoissonBinomial(prob = 0.9), series = "population"))
    seriesIndices <- c(1L, 0L)
    datasets <- list(internal + 10L,
                     population - 5L)
    namesDatasets <- c("tax", "census")
    transforms <- list(makeTransform(x = internal, y = datasets[[1]], subset = TRUE),
                       makeTransform(x = population, y = datasets[[2]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x0 <- initialCombinedAccount(account = account,
                                 systemModels = systemModels,
                                 systemWeights = systemWeights,
                                 dataModels = data.models,
                                 seriesIndices = seriesIndices,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.obtained <- updateCombined(x0, n = 1L)
        set.seed(seed)
        ans.expected <- x0
        ans.expected <- updateAccount(ans.expected)
        ans.expected <- updateSystemModels(ans.expected)
        ans.expected <- updateExpectedExposure(ans.expected)
        ans.expected <- updateDataModelsAccount(ans.expected)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of updateCombined give same answer with CombinedAccountMovements", {
    updateCombined <- demest:::updateCombined
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    updateModelNotUseExp <- demest:::updateModelNotUseExp
    updateModelUseExp <- demest:::updateModelUseExp
    population <- Counts(array(seq(1000L, 1500L, 100L),
                               dim = c(3, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                               time = c(2000, 2005))))
    deaths <- Counts(array(c(10L, 5L, 3L),
                           dim = c(3, 1),
                           dimnames = list(reg = c("a", "b", "c"),
                                           time = "2001-2005")))
    internal <- Counts(array(c(0L, 50L, 40L,
                               20L, 0L, 30L,
                               60L, 20L, 0L),
                             dim = c(3, 3, 1),
                             dimnames = list(reg_orig = c("a", "b", "c"),
                                             reg_dest = c("a", "b", "c"),
                                             time = "2001-2005")))
    internal <- collapseOrigDest(internal, to = "net")
    account <- Movements(population = population,
                         internal = internal,
                         exits = list(deaths = deaths))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ reg, useExpose = FALSE)),
                         Model(internal ~ Normal(mean ~ 1)),
                         Model(deaths ~ Poisson(mean ~ reg)))
    systemWeights <- list(NULL,
                          Counts(array(1,
                                             dim = c(3, 1),
                                             dimnames = list(reg = c("a", "b", "c"),
                                                             time = "2001-2005"))),
                          NULL)
    mean <- ValuesOne(1, labels = "2001-2005", name = "time")
    data.models <- list(Model(tax ~ NormalFixed(mean = mean, sd = 0.1), series = "internal"),
                              Model(census ~ PoissonBinomial(prob = 0.9), series = "population"))
    seriesIndices <- c(1L, 0L)
    datasets <- list(internal + 10L,
                     population - 5L)
    namesDatasets <- c("tax", "census")
    transforms <- list(makeTransform(x = internal, y = datasets[[1]], subset = TRUE),
                       makeTransform(x = population, y = datasets[[2]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x0 <- initialCombinedAccount(account = account,
                                 systemModels = systemModels,
                                 systemWeights = systemWeights,
                                 dataModels = data.models,
                                 seriesIndices = seriesIndices,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    set.seed(1)
    ans.R <- updateCombined(x0, n = 2L, useC = FALSE)
    set.seed(1)
    ans.C.generic <- updateCombined(x0, n = 2L, useC = TRUE, useSpecific = FALSE)
    set.seed(1)
    ans.C.specific <- updateCombined(x0, n = 2L, useC = TRUE, useSpecific = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C.generic)
    else
        expect_equal(ans.R, ans.C.generic)
    expect_identical(ans.C.generic, ans.C.specific)
})




