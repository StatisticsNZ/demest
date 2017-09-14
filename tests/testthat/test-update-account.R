
context("update-account")

n.test <- 5
test.identity <- FALSE
test.extended <- TRUE

## updating proposals ##########################################################################

test_that("updateProposalAccountMovePopn works with CombinedAccountMovements - no age", {
    updateProposalAccountMovePopn <- demest:::updateProposalAccountMovePopn
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
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
    observationModels <- list(Model(tax ~ Poisson(mean ~ 1), series = "deaths"),
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
    x0 <- initialCombinedAccount(account = account,
                                systemModels = systemModels,
                                systemWeights = systemWeights,
                                observationModels = observationModels,
                                seriesIndices = seriesIndices,
                                datasets = datasets,
                                namesDatasets = namesDatasets,
                                transforms = transforms)
    expect_true(validObject(x0))
    x1 <- updateProposalAccountMovePopn(x0)
    expect_is(x1, "CombinedAccountMovements")
})

test_that("R and C versions of updateProposalAccountMovePopn give same answer CombinedAccountMovements - no age", {
    updateProposalAccountMovePopn <- demest:::updateProposalAccountMovePopn
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
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
    observationModels <- list(Model(tax ~ Poisson(mean ~ 1), series = "deaths"),
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
    x0 <- initialCombinedAccount(account = account,
                                systemModels = systemModels,
                                systemWeights = systemWeights,
                                observationModels = observationModels,
                                seriesIndices = seriesIndices,
                                datasets = datasets,
                                namesDatasets = namesDatasets,
                                transforms = transforms)
    expect_true(validObject(x0))
    ans.R <- updateProposalAccountMovePopn(x0, useC = FALSE)
    ans.C <- updateProposalAccountMovePopn(x0, useC = TRUE)
    expect_identical(ans.R, ans.C)
})

test_that("updateProposalAccountMovePopn works with CombinedAccountMovementsHasAge", {
    updateProposalAccountMovePopn <- demest:::updateProposalAccountMovePopn
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    popn <- Counts(array(rpois(n = 90, lambda = 100),
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
                                           triangle = c("TL", "TU"))))
    internal <- Counts(array(rpois(n = 300, lambda = 10),
                             dim = c(3, 2, 5, 5, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                             sex = c("m", "f"),
                                             reg_orig = 1:5,
                                             reg_dest = 1:5,
                                             time = c("2001-2005", "2006-2010"),
                                             triangle = c("TL", "TU"))))
    deaths <- Counts(array(rpois(n = 72, lambda = 10),
                           dim = c(3, 2, 5, 2, 2),
                           dimnames = list(age = c("0-4", "5-9", "10+"),
                                           sex = c("m", "f"),
                                           reg = 5:1,
                                           time = c("2001-2005", "2006-2010"),
                                           triangle = c("TL", "TU"))))
    account <- Movements(population = popn,
                         births = births,
                         internal = internal,
                         exits = list(deaths = deaths))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ age + sex, useExpose = FALSE)),
                         Model(births ~ Poisson(mean ~ 1)),
                         Model(internal ~ Poisson(mean ~ reg_orig + reg_dest)),
                         Model(deaths ~ Poisson(mean ~ 1)))
    systemWeights <- list(NULL, NULL, NULL, NULL)
    census <- subarray(popn, time == "2000", drop = FALSE) + 2L
    register <- Counts(array(rpois(n = 90, lambda = popn),
                             dim = dim(popn),
                             dimnames = dimnames(popn)))
    reg.births <- Counts(array(rbinom(n = 90, size = births, prob = 0.98),
                               dim = dim(births),
                               dimnames = dimnames(births)))
    address.change <- Counts(array(rpois(n = 300, lambda = internal),
                                   dim = dim(internal),
                                   dimnames = dimnames(internal)))
    reg.deaths <- Counts(array(rbinom(n = 90, size = deaths, prob = 0.98),
                               dim = dim(deaths),
                               dimnames = dimnames(deaths))) + 1L
    datasets <- list(census, register, reg.births, address.change, reg.deaths)
    namesDatasets <- c("census", "register", "reg.births", "address.change", "reg.deaths")
    observationModels <- list(Model(census ~ PoissonBinomial(prob = 0.95), series = "population"),
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
                                observationModels = observationModels,
                                seriesIndices = seriesIndices,
                                datasets = datasets,
                                namesDatasets = namesDatasets,
                                transforms = transforms)
    expect_true(validObject(x0))
    expect_is(x0, "CombinedAccountMovementsHasAge")
    x1 <- updateProposalAccountMovePopn(x0)
    expect_is(x1, "CombinedAccountMovements")
})

test_that("R and C versions of updateProposalAccountMovePopn give same answer CombinedAccountMovements - with age", {
    updateProposalAccountMovePopn <- demest:::updateProposalAccountMovePopn
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    popn <- Counts(array(rpois(n = 90, lambda = 100),
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
                                           triangle = c("TL", "TU"))))
    internal <- Counts(array(rpois(n = 300, lambda = 10),
                             dim = c(3, 2, 5, 5, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                             sex = c("m", "f"),
                                             reg_orig = 1:5,
                                             reg_dest = 1:5,
                                             time = c("2001-2005", "2006-2010"),
                                             triangle = c("TL", "TU"))))
    deaths <- Counts(array(rpois(n = 72, lambda = 10),
                           dim = c(3, 2, 5, 2, 2),
                           dimnames = list(age = c("0-4", "5-9", "10+"),
                                           sex = c("m", "f"),
                                           reg = 5:1,
                                           time = c("2001-2005", "2006-2010"),
                                           triangle = c("TL", "TU"))))
    account <- Movements(population = popn,
                         births = births,
                         internal = internal,
                         exits = list(deaths = deaths))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ age + sex, useExpose = FALSE)),
                         Model(births ~ Poisson(mean ~ 1)),
                         Model(internal ~ Poisson(mean ~ reg_orig + reg_dest)),
                         Model(deaths ~ Poisson(mean ~ 1)))
    systemWeights <- list(NULL, NULL, NULL, NULL)
    census <- subarray(popn, time == "2000", drop = FALSE) + 2L
    register <- Counts(array(rpois(n = 90, lambda = popn),
                             dim = dim(popn),
                             dimnames = dimnames(popn)))
    reg.births <- Counts(array(rbinom(n = 90, size = births, prob = 0.98),
                               dim = dim(births),
                               dimnames = dimnames(births)))
    address.change <- Counts(array(rpois(n = 300, lambda = internal),
                                   dim = dim(internal),
                                   dimnames = dimnames(internal)))
    reg.deaths <- Counts(array(rbinom(n = 90, size = deaths, prob = 0.98),
                               dim = dim(deaths),
                               dimnames = dimnames(deaths))) + 1L
    datasets <- list(census, register, reg.births, address.change, reg.deaths)
    namesDatasets <- c("census", "register", "reg.births", "address.change", "reg.deaths")
    observationModels <- list(Model(census ~ PoissonBinomial(prob = 0.95), series = "population"),
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
                                observationModels = observationModels,
                                seriesIndices = seriesIndices,
                                datasets = datasets,
                                namesDatasets = namesDatasets,
                                transforms = transforms)
    expect_true(validObject(x0))
    expect_is(x0, "CombinedAccountMovementsHasAge")
    ans.R <- updateProposalAccountMovePopn(x0, useC = FALSE)
    ans.C <- updateProposalAccountMovePopn(x0, useC = TRUE)
    expect_identical(ans.R, ans.C)
})



## Calculating log-likelihood #################################################

test_that("diffLogLikPopnOneCell works", {
    diffLogLikPopnOneCell <- demest:::diffLogLikPopnOneCell
    initialModel <- demest:::initialModel
    Population <- dembase:::Population
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    ## one to one
    popn <- Counts(array(rpois(n = 90, lambda = 100),
                         dim = c(3, 2, 5, 3),
                         dimnames = list(age = c("0-4", "5-9", "10+"),
                                         sex = c("f", "m"),
                                         reg = 1:5,
                                         time = c(2000, 2005, 2010))))
    popn <- Population(popn)
    dataset <- Counts(array(rpois(n = 90, lambda = popn),
                            dim = dim(popn),
                            dimnames = dimnames(popn)))
    spec <- Model(dataset ~ Poisson(mean ~ age + sex))
    model <- initialModel(spec, y = dataset, exposure = popn)
    transform <- makeTransform(x = popn, y = dataset, subset = TRUE)
    transform <- makeCollapseTransformExtra(transform)
    ans.obtained <- diffLogLikPopnOneCell(iAfter = 6L,
                                          diff = 3L,
                                          population = popn,
                                          model = model,
                                          dataset = dataset,
                                          transform = transform)
    ans.expected <- (dpois(x = dataset[[6]], lambda = model@theta[6] * (popn[[6]] + 3L), log = TRUE)
        - dpois(x = dataset[[6]], lambda = model@theta[6] * popn[[6]], log = TRUE))
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else {
        expect_equal(ans.obtained, ans.expected)
    }
    ## many to one
    popn <- Counts(array(rpois(n = 90, lambda = 100),
                         dim = c(3, 2, 5, 3),
                         dimnames = list(age = c("0-4", "5-9", "10+"),
                                         sex = c("f", "m"),
                                         reg = 1:5,
                                         time = c(2000, 2005, 2010))))
    popn <- Population(popn)
    dataset <- Counts(array(rpois(n = 90, lambda = popn),
                            dim = dim(popn),
                            dimnames = dimnames(popn)))
    dataset <- collapseDimension(dataset, dimension = "reg")
    spec <- Model(dataset ~ Poisson(mean ~ age + sex))
    popn.collapsed <- makeCompatible(popn, dataset)
    popn.collapsed <- Population(popn.collapsed)
    model <- initialModel(spec, y = dataset, exposure = popn.collapsed)
    transform <- makeTransform(x = popn, y = dataset, subset = TRUE)
    transform <- dembase:::makeCollapseTransformExtra(transform)
    ans.obtained <- diffLogLikPopnOneCell(iAfter = 13L,
                                          diff = -10L,
                                          population = popn,
                                          model = model,
                                          dataset = dataset,
                                          transform = transform)
    ans.expected <- dpois(x = dataset[[13]],
                          lambda = model@theta[13] * (popn.collapsed[[13]] - 10L),
                          log = TRUE) -
        dpois(x = dataset[[13]],
              lambda = model@theta[13] * popn.collapsed[[13]],
              log = TRUE)
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})

test_that("R and C versions of diffLogLikPopnOneCell give same answer", {
    diffLogLikPopnOneCell <- demest:::diffLogLikPopnOneCell
    initialModel <- demest:::initialModel
    Population <- dembase:::Population
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    ## one to one
    popn <- Counts(array(rpois(n = 90, lambda = 100),
                         dim = c(3, 2, 5, 3),
                         dimnames = list(age = c("0-4", "5-9", "10+"),
                                         sex = c("f", "m"),
                                         reg = 1:5,
                                         time = c(2000, 2005, 2010))))
    popn <- Population(popn)
    dataset <- Counts(array(rpois(n = 90, lambda = popn),
                            dim = dim(popn),
                            dimnames = dimnames(popn)))
    spec <- Model(dataset ~ Poisson(mean ~ age + sex))
    model <- initialModel(spec, y = dataset, exposure = popn)
    transform <- makeTransform(x = popn, y = dataset, subset = TRUE)
    transform <- makeCollapseTransformExtra(transform)
    ans.R <- diffLogLikPopnOneCell(iAfter = 6L,
                                   diff = 3L,
                                   population = popn,
                                   model = model,
                                   dataset = dataset,
                                   transform = transform,
                                   useC = FALSE)
    ans.C <- diffLogLikPopnOneCell(iAfter = 6L,
                                   diff = 3L,
                                   population = popn,
                                   model = model,
                                   dataset = dataset,
                                   transform = transform,
                                   useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
    ## many to one
    popn <- Counts(array(rpois(n = 90, lambda = 100),
                         dim = c(3, 2, 5, 3),
                         dimnames = list(age = c("0-4", "5-9", "10+"),
                                         sex = c("f", "m"),
                                         reg = 1:5,
                                         time = c(2000, 2005, 2010))))
    popn <- Population(popn)
    dataset <- Counts(array(rpois(n = 90, lambda = popn),
                            dim = dim(popn),
                            dimnames = dimnames(popn)))
    dataset <- collapseDimension(dataset, dimension = "reg")
    spec <- Model(dataset ~ Poisson(mean ~ age + sex))
    popn.collapsed <- makeCompatible(popn, dataset)
    popn.collapsed <- Population(popn.collapsed)
    model <- initialModel(spec, y = dataset, exposure = popn.collapsed)
    transform <- makeTransform(x = popn, y = dataset, subset = TRUE)
    transform <- dembase:::makeCollapseTransformExtra(transform)
    ans.R <- diffLogLikPopnOneCell(iAfter = 13L,
                                   diff = -10L,
                                   population = popn,
                                   model = model,
                                   dataset = dataset,
                                   transform = transform,
                                   useC = FALSE)
    ans.C <- diffLogLikPopnOneCell(iAfter = 13L,
                                   diff = -10L,
                                   population = popn,
                                   model = model,
                                   dataset = dataset,
                                   transform = transform,
                                   useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
})

test_that("diffLogLikPopnOneDataset works", {
    diffLogLikPopnOneDataset <- demest:::diffLogLikPopnOneDataset
    diffLogLikPopnOneCell <- demest:::diffLogLikPopnOneCell
    initialModel <- demest:::initialModel
    Population <- dembase:::Population
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    CohortIterator <- demest:::CohortIterator
    popn <- Counts(array(rpois(n = 90, lambda = 100),
                         dim = c(3, 2, 5, 3),
                         dimnames = list(age = c("0-4", "5-9", "10+"),
                                         sex = c("f", "m"),
                                         reg = 1:5,
                                         time = c(2000, 2005, 2010))))
    popn <- Population(popn)
    iterator <- CohortIterator(popn)
    dataset <- Counts(array(rpois(n = 90, lambda = popn),
                            dim = dim(popn),
                            dimnames = dimnames(popn)))
    spec <- Model(dataset ~ Poisson(mean ~ age + sex))
    model <- initialModel(spec, y = dataset, exposure = popn)
    transform <- makeTransform(x = popn, y = dataset, subset = TRUE)
    transform <- makeCollapseTransformExtra(transform)
    ## three steps
    ans.obtained <- diffLogLikPopnOneDataset(diff = 3L,
                                             iFirst = 1L,
                                             iterator = iterator,
                                             population = popn,
                                             model = model,
                                             dataset = dataset,
                                             transform = transform)
    ans.expected <- diffLogLikPopnOneCell(iAfter = 1L,
                                          diff = 3L,
                                          population = popn,
                                          model = model,
                                          dataset = dataset,
                                          transform = transform) +
        diffLogLikPopnOneCell(iAfter = 32L,
                              diff = 3L,
                              population = popn,
                              model = model,
                              dataset = dataset,
                              transform = transform) +
        diffLogLikPopnOneCell(iAfter = 63L,
                              diff = 3L,
                              population = popn,
                              model = model,
                              dataset = dataset,
                              transform = transform)
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
    ## one step
    ans.obtained <- diffLogLikPopnOneDataset(diff = -3L,
                                             iFirst = 62L,
                                             iterator = iterator,
                                             population = popn,
                                             model = model,
                                             dataset = dataset,
                                             transform = transform)
    ans.expected <- diffLogLikPopnOneCell(iAfter = 62L,
                                          diff = -3L,
                                          population = popn,
                                          model = model,
                                          dataset = dataset,
                                          transform = transform)
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})


test_that("R and C versions of diffLogLikPopnOneDataset give same answer", {
    diffLogLikPopnOneDataset <- demest:::diffLogLikPopnOneDataset
    diffLogLikPopnOneCell <- demest:::diffLogLikPopnOneCell
    initialModel <- demest:::initialModel
    Population <- dembase:::Population
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    CohortIterator <- demest:::CohortIterator
    popn <- Counts(array(rpois(n = 90, lambda = 100),
                         dim = c(3, 2, 5, 3),
                         dimnames = list(age = c("0-4", "5-9", "10+"),
                                         sex = c("f", "m"),
                                         reg = 1:5,
                                         time = c(2000, 2005, 2010))))
    popn <- Population(popn)
    iterator <- CohortIterator(popn)
    dataset <- Counts(array(rpois(n = 90, lambda = popn),
                            dim = dim(popn),
                            dimnames = dimnames(popn)))
    spec <- Model(dataset ~ Poisson(mean ~ age + sex))
    model <- initialModel(spec, y = dataset, exposure = popn)
    transform <- makeTransform(x = popn, y = dataset, subset = TRUE)
    transform <- makeCollapseTransformExtra(transform)
    ## three steps
    ans.R <- diffLogLikPopnOneDataset(diff = 3L,
                                      iFirst = 1L,
                                      iterator = iterator,
                                      population = popn,
                                      model = model,
                                      dataset = dataset,
                                      transform = transform,
                                      useC = FALSE)
    ans.C <- diffLogLikPopnOneDataset(diff = 3L,
                                      iFirst = 1L,
                                      iterator = iterator,
                                      population = popn,
                                      model = model,
                                      dataset = dataset,
                                      transform = transform,
                                      useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else {
        expect_equal(ans.R, ans.C)
    }
    ## one step
    ans.R <- diffLogLikPopnOneDataset(diff = -3L,
                                      iFirst = 62L,
                                      iterator = iterator,
                                      population = popn,
                                      model = model,
                                      dataset = dataset,
                                      transform = transform,
                                      useC = FALSE)
    ans.C <- diffLogLikPopnOneDataset(diff = -3L,
                                      iFirst = 62L,
                                      iterator = iterator,
                                      population = popn,
                                      model = model,
                                      dataset = dataset,
                                      transform = transform,
                                      useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
})

test_that("diffLogLikPopn works", {
    diffLogLikPopnOneDataset <- demest:::diffLogLikPopnOneDataset
    diffLogLikPopnOneCell <- demest:::diffLogLikPopnOneCell
    initialModel <- demest:::initialModel
    Population <- dembase:::Population
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    CohortIterator <- demest:::CohortIterator
    popn <- Counts(array(rpois(n = 90, lambda = 100),
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
                                           triangle = c("TL", "TU"))))
    internal <- Counts(array(rpois(n = 300, lambda = 10),
                             dim = c(3, 2, 5, 5, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                             sex = c("m", "f"),
                                             reg_orig = 1:5,
                                             reg_dest = 1:5,
                                             time = c("2001-2005", "2006-2010"),
                                             triangle = c("TL", "TU"))))
    deaths <- Counts(array(rpois(n = 72, lambda = 10),
                           dim = c(3, 2, 5, 2, 2),
                           dimnames = list(age = c("0-4", "5-9", "10+"),
                                           sex = c("m", "f"),
                                           reg = 5:1,
                                           time = c("2001-2005", "2006-2010"),
                                           triangle = c("TL", "TU"))))
    census <- subarray(popn, time == "2000", drop = FALSE) + 2L
    register <- Counts(array(rpois(n = 90, lambda = popn),
                             dim = dim(popn),
                             dimnames = dimnames(popn)))
    reg.births <- Counts(array(rbinom(n = 90, size = births, prob = 0.98),
                               dim = dim(births),
                               dimnames = dimnames(births)))
    address.change <- Counts(array(rpois(n = 300, lambda = internal),
                                   dim = dim(internal),
                                   dimnames = dimnames(internal)))
    reg.deaths <- Counts(array(rbinom(n = 90, size = deaths, prob = 0.98),
                               dim = dim(deaths),
                               dimnames = dimnames(deaths))) + 1L
    datasets <- list(census, register, reg.births, address.change, reg.deaths)
    observationModels <- list(Model(census ~ PoissonBinomial(prob = 0.95), series = "population"),
                              Model(register ~ Poisson(mean ~ 1), series = "population"),
                              Model(reg.births ~ PoissonBinomial(prob = 0.98), series = "births"),
                              Model(address.change ~ Poisson(mean ~ 1), series = "internal"),
                              Model(reg.deaths ~ PoissonBinomial(prob = 0.98), series = "deaths"))
    seriesIndices <- c(0L, 0L, 1L, 2L, 3L)
    transforms <- list(makeTransform(x = popn, y = datasets[[1]], subset = TRUE),
                       makeTransform(x = popn,, y = datasets[[2]], subset = TRUE),
                       makeTransform(x = births, y = datasets[[3]], subset = TRUE),
                       makeTransform(x = internal, y = datasets[[4]], subset = TRUE),
                       makeTransform(x = deaths, y = datasets[[5]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    popn <- Population(popn)
    iterator <- CohortIterator(popn)
    observationModels <- list(initialModel(object = observationModels[[1L]],
                                            y = datasets[[1L]],
                                            exposure = collapse(popn, transform = transforms[[1L]])),
                              initialModel(object = observationModels[[2L]],
                                            y = datasets[[2L]],
                                            exposure = collapse(popn, transform = transforms[[2L]])),
                              initialModel(object = observationModels[[3L]],
                                            y = datasets[[3L]],
                                            exposure = collapse(births, transform = transforms[[3L]])),
                              initialModel(object = observationModels[[4L]],
                                            y = datasets[[4L]],
                                            exposure = collapse(internal, transform = transforms[[4L]])),
                              initialModel(object = observationModels[[5L]],
                                            y = datasets[[5L]],
                                            exposure = collapse(deaths, transform = transforms[[5L]])))
    ans.obtained <- diffLogLikPopn(diff = 5L,
                                   iFirst  = 11L,
                                   iterator = iterator,
                                   population = popn,
                                   observationModels = observationModels,
                                   datasets = datasets,
                                   seriesIndices = seriesIndices,
                                   transforms = transforms)
    ans.expected <- diffLogLikPopnOneDataset(diff = 5L,
                                             iFirst = 11L,
                                             iterator = iterator,
                                             population = popn,
                                             model = observationModels[[1L]],
                                             dataset = datasets[[1L]],
                                             transform = transforms[[1L]]) + 
        diffLogLikPopnOneDataset(diff = 5L,
                                 iFirst = 11L,
                                 iterator = iterator,
                                 population = popn,
                                 model = observationModels[[2L]],
                                 dataset = datasets[[2L]],
                                 transform = transforms[[2L]])
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})

test_that("R and C versions of diffLogLikPopn give same answer", {
    diffLogLikPopnOneDataset <- demest:::diffLogLikPopnOneDataset
    diffLogLikPopnOneCell <- demest:::diffLogLikPopnOneCell
    initialModel <- demest:::initialModel
    Population <- dembase:::Population
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    CohortIterator <- demest:::CohortIterator
    popn <- Counts(array(rpois(n = 90, lambda = 100),
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
                                           triangle = c("TL", "TU"))))
    internal <- Counts(array(rpois(n = 300, lambda = 10),
                             dim = c(3, 2, 5, 5, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                             sex = c("m", "f"),
                                             reg_orig = 1:5,
                                             reg_dest = 1:5,
                                             time = c("2001-2005", "2006-2010"),
                                             triangle = c("TL", "TU"))))
    deaths <- Counts(array(rpois(n = 72, lambda = 10),
                           dim = c(3, 2, 5, 2, 2),
                           dimnames = list(age = c("0-4", "5-9", "10+"),
                                           sex = c("m", "f"),
                                           reg = 5:1,
                                           time = c("2001-2005", "2006-2010"),
                                           triangle = c("TL", "TU"))))
    census <- subarray(popn, time == "2000", drop = FALSE) + 2L
    register <- Counts(array(rpois(n = 90, lambda = popn),
                             dim = dim(popn),
                             dimnames = dimnames(popn)))
    reg.births <- Counts(array(rbinom(n = 90, size = births, prob = 0.98),
                               dim = dim(births),
                               dimnames = dimnames(births)))
    address.change <- Counts(array(rpois(n = 300, lambda = internal),
                                   dim = dim(internal),
                                   dimnames = dimnames(internal)))
    reg.deaths <- Counts(array(rbinom(n = 90, size = deaths, prob = 0.98),
                               dim = dim(deaths),
                               dimnames = dimnames(deaths))) + 1L
    datasets <- list(census, register, reg.births, address.change, reg.deaths)
    observationModels <- list(Model(census ~ PoissonBinomial(prob = 0.95), series = "population"),
                              Model(register ~ Poisson(mean ~ 1), series = "population"),
                              Model(reg.births ~ PoissonBinomial(prob = 0.98), series = "births"),
                              Model(address.change ~ Poisson(mean ~ 1), series = "internal"),
                              Model(reg.deaths ~ PoissonBinomial(prob = 0.98), series = "deaths"))
    seriesIndices <- c(0L, 0L, 1L, 2L, 3L)
    transforms <- list(makeTransform(x = popn, y = datasets[[1]], subset = TRUE),
                       makeTransform(x = popn,, y = datasets[[2]], subset = TRUE),
                       makeTransform(x = births, y = datasets[[3]], subset = TRUE),
                       makeTransform(x = internal, y = datasets[[4]], subset = TRUE),
                       makeTransform(x = deaths, y = datasets[[5]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    popn <- Population(popn)
    iterator <- CohortIterator(popn)
    observationModels <- list(initialModel(object = observationModels[[1L]],
                                           y = datasets[[1L]],
                                           exposure = collapse(popn, transform = transforms[[1L]])),
                              initialModel(object = observationModels[[2L]],
                                           y = datasets[[2L]],
                                           exposure = collapse(popn, transform = transforms[[2L]])),
                              initialModel(object = observationModels[[3L]],
                                           y = datasets[[3L]],
                                           exposure = collapse(births, transform = transforms[[3L]])),
                              initialModel(object = observationModels[[4L]],
                                           y = datasets[[4L]],
                                           exposure = collapse(internal, transform = transforms[[4L]])),
                              initialModel(object = observationModels[[5L]],
                                           y = datasets[[5L]],
                                           exposure = collapse(deaths, transform = transforms[[5L]])))
    ans.R <- diffLogLikPopn(diff = 5L,
                            iFirst  = 11L,
                            iterator = iterator,
                            population = popn,
                            observationModels = observationModels,
                            datasets = datasets,
                            seriesIndices = seriesIndices,
                            transforms = transforms,
                            useC = FALSE)
    ans.C <- diffLogLikPopn(diff = 5L,
                            iFirst  = 11L,
                            iterator = iterator,
                            population = popn,
                            observationModels = observationModels,
                            datasets = datasets,
                            seriesIndices = seriesIndices,
                            transforms = transforms,
                            useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
})

test_that("diffLogLikAccountMovePopn works", {
    diffLogLikAccountMovePopn <- demest:::diffLogLikAccountMovePopn
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
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
    observationModels <- list(Model(tax ~ Poisson(mean ~ 1), series = "deaths"),
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
                                observationModels = observationModels,
                                seriesIndices = seriesIndices,
                                datasets = datasets,
                                namesDatasets = namesDatasets,
                                transforms = transforms)
    x <- updateProposalAccountMovePopn(x)
    x <- diffLogLikAccountMovePopn(x)
    expect_true(validObject(x))
})

test_that("R and C versions of diffLogLikAccountMovePopn give same answer", {
    diffLogLikAccountMovePopn <- demest:::diffLogLikAccountMovePopn
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
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
    observationModels <- list(Model(tax ~ Poisson(mean ~ 1), series = "deaths"),
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
                                observationModels = observationModels,
                                seriesIndices = seriesIndices,
                                datasets = datasets,
                                namesDatasets = namesDatasets,
                                transforms = transforms)
    x <- updateProposalAccountMovePopn(x)
    ans.R <- diffLogLikAccountMovePopn(x, useC = FALSE)
    ans.C <- diffLogLikAccountMovePopn(x, useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
})

