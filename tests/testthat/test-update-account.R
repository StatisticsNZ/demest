
context("update-account")

n.test <- 5
test.identity <- FALSE
test.extended <- TRUE

## updating proposals ##########################################################################

## updateProposalAccountMovePopn

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
    updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x1 <- updateProposalAccountMovePopn(x0)
        if (x1@generatedNewProposal@.Data)
            updated <- TRUE
        expect_is(x1, "CombinedAccountMovements")
        expect_true(validObject(x1))
    }
    if (!updated)
        warning("not updated")
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
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- updateProposalAccountMovePopn(x0, useC = FALSE)
        set.seed(seed)
        ans.C <- updateProposalAccountMovePopn(x0, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
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
    updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x1 <- updateProposalAccountMovePopn(x0)
        if (x1@generatedNewProposal@.Data)
            updated <- TRUE
        expect_is(x1, "CombinedAccountMovements")
        expect_true(validObject(x1))
    }
    if (!updated)
        warning("not updated")
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
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- updateProposalAccountMovePopn(x0, useC = FALSE)
        set.seed(seed)
        ans.C <- updateProposalAccountMovePopn(x0, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

## updateProposalAccountMoveOrigDest

test_that("updateProposalAccountMoveOrigDest works with CombinedAccountMovements - no age", {
    updateProposalAccountMoveOrigDest <- demest:::updateProposalAccountMoveOrigDest
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    population <- Counts(array(seq(1000L, 1500L, 100L),
                               dim = c(3, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                               time = c(2000, 2005))))
    internal <- Counts(array(c(0L, 50L, 40L,
                               20L, 0L, 30L,
                               60L, 20L, 0L),
                             dim = c(3, 3, 1),
                             dimnames = list(reg_orig = c("a", "b", "c"),
                                             reg_dest = c("a", "b", "c"),
                                             time = "2001-2005")))
    account <- Movements(population = population,
                         internal = internal)
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ reg, useExpose = FALSE)),
                         Model(internal ~ Poisson(mean ~ 1)))
    systemWeights <- list(NULL)
    observationModels <- list(Model(tax ~ Poisson(mean ~ 1), series = "internal"),
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
                                 observationModels = observationModels,
                                 seriesIndices = seriesIndices,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    updated <- FALSE
    x0@iComp <- 1L
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x1 <- updateProposalAccountMoveOrigDest(x0)
        if (x1@generatedNewProposal@.Data)
            updated <- TRUE
        expect_is(x1, "CombinedAccountMovements")
    }
    if (!updated)
        warning("not updated")
})

test_that("R and C versions of updateProposalAccountMoveOrigDest give same answer with CombinedAccountMovements - no age", {
    updateProposalAccountMoveOrigDest <- demest:::updateProposalAccountMoveOrigDest
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    population <- Counts(array(seq(1000L, 1500L, 100L),
                               dim = c(3, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                               time = c(2000, 2005))))
    internal <- Counts(array(c(0L, 50L, 40L,
                               20L, 0L, 30L,
                               60L, 20L, 0L),
                             dim = c(3, 3, 1),
                             dimnames = list(reg_orig = c("a", "b", "c"),
                                             reg_dest = c("a", "b", "c"),
                                             time = "2001-2005")))
    account <- Movements(population = population,
                         internal = internal)
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ reg, useExpose = FALSE)),
                         Model(internal ~ Poisson(mean ~ 1)))
    systemWeights <- list(NULL)
    observationModels <- list(Model(tax ~ Poisson(mean ~ 1), series = "internal"),
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
                                 observationModels = observationModels,
                                 seriesIndices = seriesIndices,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    expect_true(validObject(x0))
    x0@iComp <- 1L
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- updateProposalAccountMoveOrigDest(x0, useC = FALSE)
        ans.C <- updateProposalAccountMoveOrigDest(x0, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updateProposalAccountMoveOrigDest works with CombinedAccountMovementsHasAge", {
    set.seed(1)
    updateProposalAccountMoveOrigDest <- demest:::updateProposalAccountMoveOrigDest
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
    x0@iComp <- 2L
    updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x1 <- updateProposalAccountMoveOrigDest(x0)
        if (x1@generatedNewProposal@.Data)
            updated <- TRUE
        expect_is(x1, "CombinedAccountMovementsHasAge")
        expect_true(validObject(x1))
    }
    if (!updated)
        warning("not updated")
})

test_that("R and C versions of updateProposalAccountMoveOrigDest give same answer with CombinedAccountMovementsHasAge", {
    updateProposalAccountMoveOrigDest <- demest:::updateProposalAccountMoveOrigDest
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
    x0@iComp <- 2L
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- updateProposalAccountMoveOrigDest(x0, useC = FALSE)
        set.seed(seed)
        ans.C <- updateProposalAccountMoveOrigDest(x0, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

## updateProposalAccountMoveBirths

test_that("updateProposalAccountMoveBirths works with CombinedAccountMovements - no age", {
    updateProposalAccountMoveBirths <- demest:::updateProposalAccountMoveBirths
    initialcombinedaccount <- demest:::initialCombinedAccount
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
    updated <- FALSE
    x0@iComp <- 1L
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x1 <- updateProposalAccountMoveBirths(x0)
        if (x1@generatedNewProposal@.Data)
            updated <- TRUE
        expect_is(x1, "CombinedAccountMovements")
        expect_true(validObject(x1))
    }
    if (!updated)
        warning("not updated")
})

test_that("R and C versions of updateProposalAccountMoveBirths give same answer with CombinedAccountMovements - no age", {
    updateProposalAccountMoveBirths <- demest:::updateProposalAccountMoveBirths
    initialcombinedaccount <- demest:::initialCombinedAccount
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
    updated <- FALSE
    x0@iComp <- 1L
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- updateProposalAccountMoveBirths(x0, useC = FALSE)
        set.seed(seed)
        ans.C <- updateProposalAccountMoveBirths(x0, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
    if (!updated)
        warning("not updated")
})

test_that("updateProposalAccountMoveBirths works with CombinedAccountMovementsHasAge", {
    updateProposalAccountMoveBirths <- demest:::updateProposalAccountMoveBirths
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
    x0@iComp <- 1L
    updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x1 <- updateProposalAccountMoveBirths(x0)
        if (x1@generatedNewProposal@.Data)
            updated <- TRUE
        expect_is(x1, "CombinedAccountMovementsHasAge")
        expect_true(validObject(x1))
    }
    if (!updated)
        warning("not updated")
})

test_that("R and C versions of updateProposalAccountMoveBirths give same answer with CombinedAccountMovementsHasAge", {
    updateProposalAccountMoveBirths <- demest:::updateProposalAccountMoveBirths
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
    x0@iComp <- 1L
    updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- updateProposalAccountMoveBirths(x0, useC = FALSE)
        set.seed(seed)
        ans.C <- updateProposalAccountMoveBirths(x0, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
    if (!updated)
        warning("not updated")
})

test_that("updateProposalAccountMoveBirths works with CombinedAccountMovements - Parent-Child dimensions", {
    updateProposalAccountMoveBirths <- demest:::updateProposalAccountMoveBirths
    initialcombinedaccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    population <- Counts(array(c(200L, 220L, 190L,
                                 220L, 180L, 190L),
                               dim = c(3, 2),
                               dimnames = list(eth = c("A", "B", "C"),
                                               time = c("2000", "2005"))))
    births <- Counts(array(c(40L, 30L, 10L,
                             15L, 40L, 10L,
                             20L, 10L, 50L),
                           dim = c(3, 3, 1),
                           dimnames = list(eth_parent = c("A", "B", "C"),
                                           eth_child = c("A", "B", "C"),
                                           time = "2001-2005")))
    deaths <- Counts(array(c(25L, 10L, 5L),
                           dim = c(3, 1),
                           dimnames = list(eth = c("A", "B", "C"),
                                           time = "2001-2005")))
    account <- Movements(population = population,
                         births = births,
                         exits = list(deaths = deaths))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ time, useExpose = FALSE)),
                         Model(births ~ Poisson(mean ~ 1)),
                         Model(deaths ~ Poisson(mean ~ 1)))
    systemWeights <- rep(list(NULL), 3)
    observationModels <- list(Model(reg.births ~ PoissonBinomial(prob = 0.9), series = "births"),
                              Model(tax ~ Poisson(mean ~ 1), series = "deaths"),
                              Model(census ~ PoissonBinomial(prob = 0.9), series = "population"))
    seriesIndices <- c(1L, 2L, 0L)
    datasets <- list(births + 1L,
                     deaths - 5L,
                     population + 10L)
    namesDatasets <- c("reg.births", "tax", "census")
    transforms <- list(makeTransform(x = births, y = datasets[[1]], subset = TRUE),
                       makeTransform(x = deaths, y = datasets[[2]], subset = TRUE),
                       makeTransform(x = population, y = datasets[[3]], subset = TRUE))
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
    updated <- FALSE
    x0@iComp <- 1L
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x1 <- updateProposalAccountMoveBirths(x0)
        if (x1@generatedNewProposal@.Data)
            updated <- TRUE
        expect_is(x1, "CombinedAccountMovements")
        expect_true(validObject(x1))
    }
    if (!updated)
        warning("not updated")
})

test_that("R and C versions of updateProposalAccountMoveBirths give same answer with CombinedAccountMovements - Parent-Child dimensions", {
    updateProposalAccountMoveBirths <- demest:::updateProposalAccountMoveBirths
    initialcombinedaccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    population <- Counts(array(c(200L, 220L, 190L,
                                 220L, 180L, 190L),
                               dim = c(3, 2),
                               dimnames = list(eth = c("A", "B", "C"),
                                               time = c("2000", "2005"))))
    births <- Counts(array(c(40L, 30L, 10L,
                             15L, 40L, 10L,
                             20L, 10L, 50L),
                           dim = c(3, 3, 1),
                           dimnames = list(eth_parent = c("A", "B", "C"),
                                           eth_child = c("A", "B", "C"),
                                           time = "2001-2005")))
    deaths <- Counts(array(c(25L, 10L, 5L),
                           dim = c(3, 1),
                           dimnames = list(eth = c("A", "B", "C"),
                                           time = "2001-2005")))
    account <- Movements(population = population,
                         births = births,
                         exits = list(deaths = deaths))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ time, useExpose = FALSE)),
                         Model(births ~ Poisson(mean ~ 1)),
                         Model(deaths ~ Poisson(mean ~ 1)))
    systemWeights <- rep(list(NULL), 3)
    observationModels <- list(Model(reg.births ~ PoissonBinomial(prob = 0.9), series = "births"),
                              Model(tax ~ Poisson(mean ~ 1), series = "deaths"),
                              Model(census ~ PoissonBinomial(prob = 0.9), series = "population"))
    seriesIndices <- c(1L, 2L, 0L)
    datasets <- list(births + 1L,
                     deaths - 5L,
                     population + 10L)
    namesDatasets <- c("reg.births", "tax", "census")
    transforms <- list(makeTransform(x = births, y = datasets[[1]], subset = TRUE),
                       makeTransform(x = deaths, y = datasets[[2]], subset = TRUE),
                       makeTransform(x = population, y = datasets[[3]], subset = TRUE))
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
    updated <- FALSE
    x0@iComp <- 1L
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- updateProposalAccountMoveBirths(x0, useC = FALSE)
        set.seed(seed)
        ans.C <- updateProposalAccountMoveBirths(x0, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
    if (!updated)
        warning("not updated")
})

## updateProposalAccountMoveComp

test_that("updateProposalAccountMoveComp works with CombinedAccountMovements - no age", {
    updateProposalAccountMoveComp <- demest:::updateProposalAccountMoveComp
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
    x0@iComp <- 2L
    updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x1 <- updateProposalAccountMoveComp(x0)
        if (x1@generatedNewProposal@.Data)
            updated <- TRUE
        expect_is(x1, "CombinedAccountMovements")
        expect_true(validObject(x1))
    }
    if (!updated)
        warning("not updated")
})

test_that("R and C versions of updateProposalAccountMoveComp give same answer - no age", {
    updateProposalAccountMoveComp <- demest:::updateProposalAccountMoveComp
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
    x0@iComp <- 2L
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- updateProposalAccountMoveComp(x0, useC = FALSE)
        set.seed(seed)
        ans.C <- updateProposalAccountMoveComp(x0, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("updateProposalAccountMoveComp works with CombinedAccountMovementsHasAge", {
    updateProposalAccountMoveComp <- demest:::updateProposalAccountMoveComp
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
    x0@iComp <- 3L
    updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x1 <- updateProposalAccountMoveComp(x0)
        if (x1@generatedNewProposal@.Data)
            updated <- TRUE
        expect_is(x1, "CombinedAccountMovementsHasAge")
        expect_true(validObject(x1))
    }
    if (!updated)
        warning("not updated")
})

test_that("R and C versions of updateProposalAccountMoveComp give same answer", {
    updateProposalAccountMoveComp <- demest:::updateProposalAccountMoveComp
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
    x0@iComp <- 3L
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- updateProposalAccountMoveComp(x0, useC = FALSE)
        set.seed(seed)
        ans.C <- updateProposalAccountMoveComp(x0, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
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

test_that("diffLogLikCellComp works", {
    diffLogLikCellComp <- demest:::diffLogLikCellComp
    diffLogLikCellOneDataset <- demest:::diffLogLikCellOneDataset
    initialModel <- demest:::initialModel
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    component <- Counts(array(rpois(n = 180, lambda = 100),
                              dim = c(3, 2, 5, 3, 2),
                              dimnames = list(age = c("0-4", "5-9", "10+"),
                                              sex = c("f", "m"),
                                              reg = 1:5,
                                              time = c("2001-2005", "2006-2010", "2011-2015"),
                                              triangle = c("TL", "TU"))))
    component <- new("ExitsMovements",
                     .Data = component@.Data,
                     metadata = component@metadata)
    dataset1 <- Counts(array(rpois(n = 180, lambda = component),
                             dim = dim(component),
                             dimnames = dimnames(component)))
    dataset2 <- collapseDimension(dataset1, dimension = "reg")
    diff <- 1L
    iComp <- 2L
    iCell <- 5L
    datasets <- list(dataset1, dataset2)
    seriesIndices <- c(2L, 2L)
    observationModels <- list(initialModel(Model(component ~ Poisson(mean ~ age + sex)),
                                           y = component,
                                           exposure = dataset1),
                              initialModel(Model(component ~ Poisson(mean ~ age + sex)),
                                           y = collapseDimension(component, dimension = "reg"),
                                           exposure = dataset1))
    transforms <- lapply(list(makeTransform(x = component, y = dataset1),
                              makeTransform(x = component, y = dataset2)),
                         makeCollapseTransformExtra)
    ans.obtained <- diffLogLikCellComp(diff = diff,
                                       iComp = iComp,
                                       iCell = iCell,
                                       component = component,
                                       observationModels = observationModels,
                                       datasets = datasets,
                                       seriesIndices = seriesIndices,
                                       transforms = transforms)
    ans.expected <- diffLogLikCellOneDataset(diff = diff,
                                             iCell = iCell,
                                             component = component,
                                             model = observationModels[[1]],
                                             dataset = datasets[[1]],
                                             transform = transforms[[1]]) +
        diffLogLikCellOneDataset(diff = diff,
                                 iCell = iCell,
                                 component = component,
                                 model = observationModels[[2]],
                                 dataset = datasets[[2]],
                                 transform = transforms[[2]])
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of diffLogLikCellComp give same answer", {
    diffLogLikCellComp <- demest:::diffLogLikCellComp
    diffLogLikCellOneDataset <- demest:::diffLogLikCellOneDataset
    initialModel <- demest:::initialModel
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    component <- Counts(array(rpois(n = 180, lambda = 100),
                              dim = c(3, 2, 5, 3, 2),
                              dimnames = list(age = c("0-4", "5-9", "10+"),
                                              sex = c("f", "m"),
                                              reg = 1:5,
                                              time = c("2001-2005", "2006-2010", "2011-2015"),
                                              triangle = c("TL", "TU"))))
    component <- new("ExitsMovements",
                     .Data = component@.Data,
                     metadata = component@metadata)
    dataset1 <- Counts(array(rpois(n = 180, lambda = component),
                             dim = dim(component),
                             dimnames = dimnames(component)))
    dataset2 <- collapseDimension(dataset1, dimension = "reg")
    diff <- 1L
    iComp <- 2L
    iCell <- 5L
    datasets <- list(dataset1, dataset2)
    seriesIndices <- c(2L, 2L)
    observationModels <- list(initialModel(Model(component ~ Poisson(mean ~ age + sex)),
                                           y = component,
                                           exposure = dataset1),
                              initialModel(Model(component ~ Poisson(mean ~ age + sex)),
                                           y = collapseDimension(component, dimension = "reg"),
                                           exposure = dataset1))
    transforms <- lapply(list(makeTransform(x = component, y = dataset1),
                              makeTransform(x = component, y = dataset2)),
                         makeCollapseTransformExtra)
    ans.R <- diffLogLikCellComp(diff = diff,
                                iComp = iComp,
                                iCell = iCell,
                                component = component,
                                observationModels = observationModels,
                                datasets = datasets,
                                seriesIndices = seriesIndices,
                                transforms = transforms,
                                useC = FALSE)
    ans.C <- diffLogLikCellComp(diff = diff,
                                iComp = iComp,
                                iCell = iCell,
                                component = component,
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

test_that("diffLogLikCellOneDataset works", {
    diffLogLikCellOneDataset <- demest:::diffLogLikCellOneDataset
    initialModel <- demest:::initialModel
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    ## one to one
    deaths <- Counts(array(rpois(n = 180, lambda = 100),
                           dim = c(3, 2, 5, 3, 2),
                           dimnames = list(age = c("0-4", "5-9", "10+"),
                                         sex = c("f", "m"),
                                         reg = 1:5,
                                         time = c("2001-2005", "2006-2010", "2011-2015"),
                                         triangle = c("TL", "TU"))))
    deaths <- new("ExitsMovements",
                  .Data = deaths@.Data,
                  metadata = deaths@metadata)
    dataset <- Counts(array(rpois(n = 180, lambda = deaths),
                            dim = dim(deaths),
                            dimnames = dimnames(deaths)))
    spec <- Model(dataset ~ Poisson(mean ~ age + sex))
    model <- initialModel(spec, y = dataset, exposure = deaths)
    transform <- makeTransform(x = deaths, y = dataset, subset = TRUE)
    transform <- makeCollapseTransformExtra(transform)
    ans.obtained <- diffLogLikCellOneDataset(diff = 3L,
                                             iCell = 5L,
                                             component = deaths,
                                             model = model,
                                             dataset = dataset,
                                             transform = transform)
    ans.expected <- (dpois(x = dataset[[5L]], lambda = model@theta[5L] * (deaths[[5L]] + 3L), log = TRUE)
        - dpois(x = dataset[[5L]], lambda = model@theta[5] * deaths[[5]], log = TRUE))
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else {
        expect_equal(ans.obtained, ans.expected)
    }
    ## many to one
    deaths <- Counts(array(rpois(n = 180, lambda = 100),
                           dim = c(3, 2, 5, 3, 2),
                           dimnames = list(age = c("0-4", "5-9", "10+"),
                                         sex = c("f", "m"),
                                         reg = 1:5,
                                         time = c("2001-2005", "2006-2010", "2011-2015"),
                                         triangle = c("TL", "TU"))))
    deaths <- new("ExitsMovements",
                  .Data = deaths@.Data,
                  metadata = deaths@metadata)
    dataset <- Counts(array(rpois(n = 180, lambda = deaths),
                            dim = dim(deaths),
                            dimnames = dimnames(deaths)))
    dataset <- collapseDimension(dataset, dimension = "reg")
    spec <- Model(dataset ~ Poisson(mean ~ age + sex))
    deaths.collapsed <- makeCompatible(deaths, dataset)
    model <- initialModel(spec, y = dataset, exposure = deaths.collapsed)
    transform <- makeTransform(x = deaths, y = dataset, subset = TRUE)
    transform <- dembase:::makeCollapseTransformExtra(transform)
    ans.obtained <- diffLogLikCellOneDataset(diff = -10L,
                                             iCell = 13L,
                                             component = deaths,
                                             model = model,
                                             dataset = dataset,
                                             transform = transform)
    ans.expected <- dpois(x = dataset[[1]],
                          lambda = model@theta[1] * (deaths.collapsed[[1]] - 10L),
                          log = TRUE) -
        dpois(x = dataset[[1]],
              lambda = model@theta[1] * deaths.collapsed[[1]],
              log = TRUE)
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})

test_that("R and C versions of diffLogLikCellOneDataset give same answer", {
    diffLogLikCellOneDataset <- demest:::diffLogLikCellOneDataset
    initialModel <- demest:::initialModel
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    ## one to one
    deaths <- Counts(array(rpois(n = 180, lambda = 100),
                           dim = c(3, 2, 5, 3, 2),
                           dimnames = list(age = c("0-4", "5-9", "10+"),
                                           sex = c("f", "m"),
                                           reg = 1:5,
                                           time = c("2001-2005", "2006-2010", "2011-2015"),
                                           triangle = c("TL", "TU"))))
    deaths <- new("ExitsMovements",
                  .Data = deaths@.Data,
                  metadata = deaths@metadata)
    dataset <- Counts(array(rpois(n = 180, lambda = deaths),
                            dim = dim(deaths),
                            dimnames = dimnames(deaths)))
    spec <- Model(dataset ~ Poisson(mean ~ age + sex))
    model <- initialModel(spec, y = dataset, exposure = deaths)
    transform <- makeTransform(x = deaths, y = dataset, subset = TRUE)
    transform <- makeCollapseTransformExtra(transform)
    ans.R <- diffLogLikCellOneDataset(diff = 3L,
                                      iCell = 5L,
                                      component = deaths,
                                      model = model,
                                      dataset = dataset,
                                      transform = transform,
                                      useC = FALSE)
    ans.C <- diffLogLikCellOneDataset(diff = 3L,
                                      iCell = 5L,
                                      component = deaths,
                                      model = model,
                                      dataset = dataset,
                                      transform = transform,
                                      useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
    ## many to one
    deaths <- Counts(array(rpois(n = 180, lambda = 100),
                           dim = c(3, 2, 5, 3, 2),
                           dimnames = list(age = c("0-4", "5-9", "10+"),
                                           sex = c("f", "m"),
                                           reg = 1:5,
                                           time = c("2001-2005", "2006-2010", "2011-2015"),
                                           triangle = c("TL", "TU"))))
    deaths <- new("ExitsMovements",
                  .Data = deaths@.Data,
                  metadata = deaths@metadata)
    dataset <- Counts(array(rpois(n = 180, lambda = deaths),
                            dim = dim(deaths),
                            dimnames = dimnames(deaths)))
    dataset <- collapseDimension(dataset, dimension = "reg")
    spec <- Model(dataset ~ Poisson(mean ~ age + sex))
    deaths.collapsed <- makeCompatible(deaths, dataset)
    model <- initialModel(spec, y = dataset, exposure = deaths.collapsed)
    transform <- makeTransform(x = deaths, y = dataset, subset = TRUE)
    transform <- dembase:::makeCollapseTransformExtra(transform)
    ans.R <- diffLogLikCellOneDataset(diff = -10L,
                                      iCell = 13L,
                                      component = deaths,
                                      model = model,
                                      dataset = dataset,
                                      transform = transform,
                                      useC = FALSE)
    ans.C <- diffLogLikCellOneDataset(diff = -10L,
                                      iCell = 13L,
                                      component = deaths,
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
    diffLogLikPopn <- demest:::diffLogLikPopn
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
    diffLogLikPopn <- demest:::diffLogLikPopn
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

test_that("diffLogLikPopnPair works with CombinedAccountMovements", {
    diffLogLikPopnPair <- demest:::diffLogLikPopnPair
    diffLogLikPopnOneDataset <- demest:::diffLogLikPopnOneDataset
    initialModel <- demest:::initialModel
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    CohortIterator <- demest:::CohortIterator
    Population <- dembase:::Population
    population <- Counts(array(seq(1000L, 1500L, 100L),
                               dim = c(3, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                               time = c(2000, 2005))))
    population <- Population(population)
    internal <- Counts(array(c(0L, 50L, 40L,
                               20L, 0L, 30L,
                               60L, 20L, 0L),
                             dim = c(3, 3, 1),
                             dimnames = list(reg_orig = c("a", "b", "c"),
                                             reg_dest = c("a", "b", "c"),
                                             time = "2001-2005")))
    systemModels <- list(Model(population ~ Poisson(mean ~ reg, useExpose = FALSE)),
                         Model(internal ~ Poisson(mean ~ 1)))
    datasets <- list(internal + 10L,
                     population - 5L)
    observationModels <- list(initialModel(Model(tax ~ Poisson(mean ~ 1), series = "internal"),
                                           y = datasets[[1]],
                                           exposure = internal),
                              initialModel(Model(census ~ PoissonBinomial(prob = 0.9), series = "population"),
                                           y = datasets[[2]],
                                           exposure = population))
    seriesIndices <- c(1L, 0L)
    transforms <- list(makeTransform(x = internal, y = datasets[[1]], subset = TRUE),
                       makeTransform(x = population, y = datasets[[2]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    iterator <- CohortIterator(population)
    diff <- -4L
    ans.obtained <- diffLogLikPopnPair(diff = diff,
                                       iPopnOrig = 6L,
                                       iPopnDest = 5L,
                                       iterator = iterator,
                                       population = population,
                                       observationModels = observationModels,
                                       datasets = datasets,
                                       seriesIndices = seriesIndices,
                                       transforms = transforms)
    ans.expected <- diffLogLikPopnOneDataset(diff = -diff,
                                             iFirst = 6L,
                                             iterator = iterator,
                                             population = population,
                                             model = observationModels[[2]],
                                             dataset = datasets[[2]],
                                             transform = transforms[[2]]) +
        diffLogLikPopnOneDataset(diff = diff,
                                 iFirst = 5L,
                                 iterator = iterator,
                                 population = population,
                                 model = observationModels[[2]],
                                 dataset = datasets[[2]],
                                 transform = transforms[[2]])
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})


test_that("R and C versions of diffLogLikPopnPair give same answer", {
    diffLogLikPopnPair <- demest:::diffLogLikPopnPair
    initialModel <- demest:::initialModel
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    CohortIterator <- demest:::CohortIterator
    Population <- dembase:::Population
    population <- Counts(array(seq(1000L, 1500L, 100L),
                               dim = c(3, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                               time = c(2000, 2005))))
    population <- Population(population)
    internal <- Counts(array(c(0L, 50L, 40L,
                               20L, 0L, 30L,
                               60L, 20L, 0L),
                             dim = c(3, 3, 1),
                             dimnames = list(reg_orig = c("a", "b", "c"),
                                             reg_dest = c("a", "b", "c"),
                                             time = "2001-2005")))
    systemModels <- list(Model(population ~ Poisson(mean ~ reg, useExpose = FALSE)),
                         Model(internal ~ Poisson(mean ~ 1)))
    datasets <- list(internal + 10L,
                     population - 5L)
    observationModels <- list(initialModel(Model(tax ~ Poisson(mean ~ 1), series = "internal"),
                                           y = datasets[[1]],
                                           exposure = internal),
                              initialModel(Model(census ~ PoissonBinomial(prob = 0.9), series = "population"),
                                           y = datasets[[2]],
                                           exposure = population))
    seriesIndices <- c(1L, 0L)
    transforms <- list(makeTransform(x = internal, y = datasets[[1]], subset = TRUE),
                       makeTransform(x = population, y = datasets[[2]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    iterator <- CohortIterator(population)
    diff <- -4L
    ans.R <- diffLogLikPopnPair(diff = diff,
                                iPopnOrig = 6L,
                                iPopnDest = 5L,
                                iterator = iterator,
                                population = population,
                                observationModels = observationModels,
                                datasets = datasets,
                                seriesIndices = seriesIndices,
                                transforms = transforms,
                                useC = FALSE)
    ans.C <- diffLogLikPopnPair(diff = diff,
                                iPopnOrig = 6L,
                                iPopnDest = 5L,
                                iterator = iterator,
                                population = population,
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
    set.seed(1)
    diffLogLikAccountMovePopn <- demest:::diffLogLikAccountMovePopn
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
    x <- initialCombinedAccount(account = account,
                                systemModels = systemModels,
                                systemWeights = systemWeights,
                                observationModels = observationModels,
                                seriesIndices = seriesIndices,
                                datasets = datasets,
                                namesDatasets = namesDatasets,
                                transforms = transforms)
    x <- updateProposalAccountMovePopn(x)
    ans <- diffLogLikAccountMovePopn(x)
    expect_true(is.numeric(ans))
    expect_identical(length(ans), 1L)
})

test_that("R and C versions of diffLogLikAccountMovePopn give same answer", {
    diffLogLikAccountMovePopn <- demest:::diffLogLikAccountMovePopn
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    updateProposalAccountMovePopn <- demest:::updateProposalAccountMovePopn
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
    while (!x@generatedNewProposal@.Data) {
        x <- updateProposalAccountMovePopn(x)
    }
    ans.R <- diffLogLikAccountMovePopn(x, useC = FALSE)
    ans.C <- diffLogLikAccountMovePopn(x, useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
})

test_that("diffLogLikAccountMoveComp works", {
    diffLogLikAccountMoveComp <- demest:::diffLogLikAccountMoveComp
    updateProposalAccountMoveComp <- demest:::updateProposalAccountMoveComp
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
    x@iComp <- 2L
    x <- updateProposalAccountMoveComp(x)
    ans <- diffLogLikAccountMoveComp(x)
    expect_true(is.numeric(ans))
    expect_identical(length(ans), 1L)
})


test_that("R and C versions of diffLogLikAccountMoveComp give same answer", {
    diffLogLikAccountMoveComp <- demest:::diffLogLikAccountMoveComp
    updateProposalAccountMoveComp <- demest:::updateProposalAccountMoveComp
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
    x@iComp <- 2L
    x <- updateProposalAccountMoveComp(x)
    ans.R <- diffLogLikAccountMoveComp(x, useC = FALSE)
    ans.C <- diffLogLikAccountMoveComp(x, useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
})

test_that("diffLogLikAccountMoveOrigDest works", {
    diffLogLikAccountMoveOrigDest <- demest:::diffLogLikAccountMoveOrigDest
    updateProposalAccountMoveOrigDest <- demest:::updateProposalAccountMoveOrigDest
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    population <- Counts(array(seq(1000L, 1500L, 100L),
                               dim = c(3, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                               time = c(2000, 2005))))
    internal <- Counts(array(c(0L, 50L, 40L,
                               20L, 0L, 30L,
                               60L, 20L, 0L),
                             dim = c(3, 3, 1),
                             dimnames = list(reg_orig = c("a", "b", "c"),
                                             reg_dest = c("a", "b", "c"),
                                             time = "2001-2005")))
    account <- Movements(population = population,
                         internal = internal)
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ reg, useExpose = FALSE)),
                         Model(internal ~ Poisson(mean ~ 1)))
    systemWeights <- list(NULL)
    observationModels <- list(Model(tax ~ Poisson(mean ~ 1), series = "internal"),
                              Model(census ~ PoissonBinomial(prob = 0.9), series = "population"))
    seriesIndices <- c(1L, 0L)
    datasets <- list(internal + 10L,
                     population - 5L)
    namesDatasets <- c("tax", "census")
    transforms <- list(makeTransform(x = internal, y = datasets[[1]], subset = TRUE),
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
    x@iComp <- 1L
    updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x <- updateProposalAccountMoveOrigDest(x)
        if (x@generatedNewProposal@.Data) {
            updated <- TRUE
            ans.obtained <- diffLogLikAccountMoveOrigDest(x, useC = FALSE)
            expect_identical(length(ans.obtained), 1L)
            expect_true(is.numeric(ans.obtained))
        }
    }
    if (!updated)
        warning("proposal not updated")
})


test_that("R and C versions of diffLogLikAccountMoveOrigDest give same answer", {
    diffLogLikAccountMoveOrigDest <- demest:::diffLogLikAccountMoveOrigDest
    updateProposalAccountMoveOrigDest <- demest:::updateProposalAccountMoveOrigDest
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    population <- Counts(array(seq(1000L, 1500L, 100L),
                               dim = c(3, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                               time = c(2000, 2005))))
    internal <- Counts(array(c(0L, 50L, 40L,
                               20L, 0L, 30L,
                               60L, 20L, 0L),
                             dim = c(3, 3, 1),
                             dimnames = list(reg_orig = c("a", "b", "c"),
                                             reg_dest = c("a", "b", "c"),
                                             time = "2001-2005")))
    account <- Movements(population = population,
                         internal = internal)
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ reg, useExpose = FALSE)),
                         Model(internal ~ Poisson(mean ~ 1)))
    systemWeights <- list(NULL)
    observationModels <- list(Model(tax ~ Poisson(mean ~ 1), series = "internal"),
                              Model(census ~ PoissonBinomial(prob = 0.9), series = "population"))
    seriesIndices <- c(1L, 0L)
    datasets <- list(internal + 10L,
                     population - 5L)
    namesDatasets <- c("tax", "census")
    transforms <- list(makeTransform(x = internal, y = datasets[[1]], subset = TRUE),
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
    x@iComp <- 1L
    updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x <- updateProposalAccountMoveOrigDest(x)
        if (x@generatedNewProposal@.Data) {
            updated <- TRUE
            ans.R <- diffLogLikAccountMoveOrigDest(x, useC = FALSE)
            ans.C <- diffLogLikAccountMoveOrigDest(x, useC = TRUE)
            if (test.identity)
                expect_identical(ans.R, ans.C)
            else
                expect_equal(ans.R, ans.C)
        }
        if (!updated)
            warning("proposal not updated")
    }
})




## LOG DENSITY ################################################################

## diffLogDensPopn

test_that("diffLogDensPopn works", {
    diffLogDensPopn <- demest:::diffLogDensPopn
    diffLogDensPopnOneCohort <- demest:::diffLogDensPopnOneCohort
    updateProposalAccountMoveComp <- demest:::updateProposalAccountMoveComp
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
    x@iComp <- 2L
    x <- updateProposalAccountMoveComp(x)
    ans.obtained <- diffLogDensPopn(x)
    ans.expected <- diffLogDensPopnOneCohort(diff = -x@diffProp,
                                             population = x@account@population,
                                             i = x@iPopnNext,
                                             iterator = x@iteratorPopn,
                                             theta = x@systemModels[[1]]@theta)
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else {
        expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of diffLogDensPopn give same answer", {
    diffLogDensPopn <- demest:::diffLogDensPopn
    diffLogDensPopnOneCohort <- demest:::diffLogDensPopnOneCohort
    updateProposalAccountMoveComp <- demest:::updateProposalAccountMoveComp
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
    x@iComp <- 2L
    x <- updateProposalAccountMoveComp(x)
    ans.R <- diffLogDensPopn(x, useC = FALSE)
    ans.C <- diffLogDensPopn(x, useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else {
        expect_equal(ans.R, ans.C)
    }
})

## diffLogDensPopnOneCohort

test_that("diffLogDensPopnOneCohort works", {
    diffLogDensPopnOneCohort <- demest:::diffLogDensPopnOneCohort
    Population <- dembase:::Population
    CohortIterator <- demest:::CohortIterator
    popn <- Counts(array(rpois(n = 90, lambda = 100),
                         dim = c(3, 2, 5, 3),
                         dimnames = list(age = c("0-4", "5-9", "10+"),
                                         sex = c("f", "m"),
                                         reg = 1:5,
                                         time = c(2000, 2005, 2010))))
    popn <- Population(popn)
    iterator <- CohortIterator(popn)
    theta <- Values(array(rpois(n = 90, lambda = popn),
                          dim = dim(popn),
                          dimnames = dimnames(popn))) + 0.01
    ans.obtained <- diffLogDensPopnOneCohort(diff = 3L,
                                             population = popn,
                                             i = 7L,
                                             iterator = iterator,
                                             theta = theta)
    ii <- c(7, 38, 69)
    ans.expected <- (sum(dpois(x = popn[ii] + 3,
                               lambda = theta[ii],
                               log = TRUE))
        - sum(dpois(x = popn[ii],
                    lambda = theta[ii],
                    log = TRUE)))
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else {
        expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of diffLogDensPopnOneCohort give same answer", {
    diffLogDensPopnOneCohort <- demest:::diffLogDensPopnOneCohort
    Population <- dembase:::Population
    CohortIterator <- demest:::CohortIterator
    popn <- Counts(array(rpois(n = 90, lambda = 100),
                         dim = c(3, 2, 5, 3),
                         dimnames = list(age = c("0-4", "5-9", "10+"),
                                         sex = c("f", "m"),
                                         reg = 1:5,
                                         time = c(2000, 2005, 2010))))
    popn <- Population(popn)
    iterator <- CohortIterator(popn)
    theta <- Values(array(rpois(n = 90, lambda = popn),
                          dim = dim(popn),
                          dimnames = dimnames(popn))) + 0.01
    for (i in 1:12) {
        ans.R <- diffLogDensPopnOneCohort(diff = 3L,
                                          population = popn,
                                          i = 7L,
                                          iterator = iterator,
                                          theta = theta,
                                          useC = FALSE)
        ans.C <- diffLogDensPopnOneCohort(diff = 3L,
                                          population = popn,
                                          i = 7L,
                                          iterator = iterator,
                                          theta = theta,
                                          useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else {
            expect_equal(ans.R, ans.C)
        }
    }
})

## diffLogDensJumpOrigDest

test_that("diffLogDensJumpOrigDest works with CombinedAccountMovements - no age", {
    diffLogDensJumpOrigDest <- demest:::diffLogDensJumpOrigDest
    updateProposalAccountMoveOrigDest <- demest:::updateProposalAccountMoveOrigDest
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    population <- Counts(array(seq(1000L, 1500L, 100L),
                               dim = c(3, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                               time = c(2000, 2005))))
    internal <- Counts(array(c(0L, 50L, 40L,
                               20L, 0L, 30L,
                               60L, 20L, 0L),
                             dim = c(3, 3, 1),
                             dimnames = list(reg_orig = c("a", "b", "c"),
                                             reg_dest = c("a", "b", "c"),
                                             time = "2001-2005")))
    account <- Movements(population = population,
                         internal = internal)
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ reg, useExpose = FALSE)),
                         Model(internal ~ Poisson(mean ~ 1)))
    systemWeights <- list(NULL)
    observationModels <- list(Model(tax ~ Poisson(mean ~ 1), series = "internal"),
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
                                 observationModels = observationModels,
                                 seriesIndices = seriesIndices,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    updated <- FALSE
    x0@iComp <- 1L
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x <- updateProposalAccountMoveOrigDest(x0)
        if (x@generatedNewProposal@.Data) {
            ans.obtained <- diffLogDensJumpOrigDest(x)
            ans.expected <- (dpois(x@account@components[[1]][x@iCell] + x@diffProp,
                                   lambda = (x@exposure[x@iExposure] - 0.5 * x@diffProp) * x@systemModels[[2]]@theta[x@iCell],
                                   log = TRUE)
                - dpois(x@account@components[[1]][x@iCell],
                        lambda = (x@exposure[x@iExposure] - 0.5 * x@diffProp) * x@systemModels[[2]]@theta[x@iCell],
                        log = TRUE)
                + dpois(x@account@components[[1]][x@iCell],
                        lambda = x@expectedExposure[x@iExposure] * x@systemModels[[2]]@theta[x@iCell],
                        log = TRUE)
                - dpois(x@account@components[[1]][x@iCell] + x@diffProp,
                        lambda = x@expectedExposure[x@iExposure] * x@systemModels[[2]]@theta[x@iCell],
                        log = TRUE))
            if (test.identity)
                expect_identical(ans.obtained, ans.expected)
            else
                expect_equal(ans.obtained, ans.expected)
        }
    }
    if (!updated)
        warning("not updated")
})

test_that("R and C versions of diffLogDensJumpOrigDest give same answer - no age", {
    diffLogDensJumpOrigDest <- demest:::diffLogDensJumpOrigDest
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    population <- Counts(array(seq(1000L, 1500L, 100L),
                               dim = c(3, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                               time = c(2000, 2005))))
    internal <- Counts(array(c(0L, 50L, 40L,
                               20L, 0L, 30L,
                               60L, 20L, 0L),
                             dim = c(3, 3, 1),
                             dimnames = list(reg_orig = c("a", "b", "c"),
                                             reg_dest = c("a", "b", "c"),
                                             time = "2001-2005")))
    account <- Movements(population = population,
                         internal = internal)
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ reg, useExpose = FALSE)),
                         Model(internal ~ Poisson(mean ~ 1)))
    systemWeights <- list(NULL)
    observationModels <- list(Model(tax ~ Poisson(mean ~ 1), series = "internal"),
                              Model(census ~ PoissonBinomial(prob = 0.9), series = "population"))
    seriesIndices <- c(1L, 0L)
    datasets <- list(internal + 10L,
                     population - 5L)
    namesDatasets <- c("tax", "census")
    transforms <- list(makeTransform(x = internal, y = datasets[[1]], subset = TRUE),
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
    updated <- FALSE
    x@iComp <- 1L
    ans.R <- diffLogDensJumpOrigDest(x, useC = FALSE)
    ans.C <- diffLogDensJumpOrigDest(x, useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
})

test_that("diffLogDensJumpOrigDest works - with age", {
    diffLogDensJumpOrigDest <- demest:::diffLogDensJumpOrigDest
    updateProposalAccountMoveOrigDest <- demest:::updateProposalAccountMoveOrigDest
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
    x <- initialCombinedAccount(account = account,
                                systemModels = systemModels,
                                systemWeights = systemWeights,
                                observationModels = observationModels,
                                seriesIndices = seriesIndices,
                                datasets = datasets,
                                namesDatasets = namesDatasets,
                                transforms = transforms)
    x@iComp <- 2L
    updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x <- updateProposalAccountMoveOrigDest(x)
        if (x@generatedNewProposal@.Data) {
            updated <- TRUE
            ans.obtained <- diffLogDensJumpOrigDest(x)
            if (x@isLowerTriangle) {
                ans.expected <- (dpois(x@account@components[[2]][x@iCell] + x@diffProp,
                                       lambda = (x@exposure[x@iExposure] - 0.5 * x@diffProp) * x@systemModels[[3]]@theta[x@iCell],
                                       log = TRUE)
                    - dpois(x@account@components[[2]][x@iCell],
                            lambda = (x@exposure[x@iExposure] - 0.5 * x@diffProp) * x@systemModels[[3]]@theta[x@iCell],
                            log = TRUE)
                    + dpois(x@account@components[[2]][x@iCell],
                            lambda = x@expectedExposure[x@iExposure] * x@systemModels[[3]]@theta[x@iCell],
                            log = TRUE)
                    - dpois(x@account@components[[2]][x@iCell] + x@diffProp,
                            lambda = x@expectedExposure[x@iExposure] * x@systemModels[[3]]@theta[x@iCell],
                            log = TRUE))
            }
            else {
                ans.expected <- (dpois(x@account@components[[2]][x@iCell] + x@diffProp,
                                       lambda = (x@exposure[x@iExposure]) * x@systemModels[[3]]@theta[x@iCell],
                                       log = TRUE)
                    - dpois(x@account@components[[2]][x@iCell],
                            lambda = (x@exposure[x@iExposure]) * x@systemModels[[3]]@theta[x@iCell],
                            log = TRUE)
                    + dpois(x@account@components[[2]][x@iCell],
                            lambda = x@expectedExposure[x@iExposure] * x@systemModels[[3]]@theta[x@iCell],
                            log = TRUE)
                    - dpois(x@account@components[[2]][x@iCell] + x@diffProp,
                            lambda = x@expectedExposure[x@iExposure] * x@systemModels[[3]]@theta[x@iCell],
                            log = TRUE))
            }
            expect_equal(ans.obtained, ans.expected)
            if (test.identity)
                expect_identical(ans.obtained, ans.expected)
            else
                expect_equal(ans.obtained, ans.expected)
        }
    }
    if (!updated)
        warning("not updated")
})

test_that("R and C versions of diffLogDensJumpOrigDest give same answer - with age", {
    diffLogDensJumpOrigDest <- demest:::diffLogDensJumpOrigDest
    updateProposalAccountMoveOrigDest <- demest:::updateProposalAccountMoveOrigDest
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
    x <- initialCombinedAccount(account = account,
                                systemModels = systemModels,
                                systemWeights = systemWeights,
                                observationModels = observationModels,
                                seriesIndices = seriesIndices,
                                datasets = datasets,
                                namesDatasets = namesDatasets,
                                transforms = transforms)
    x@iComp <- 2L
    updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x <- updateProposalAccountMoveOrigDest(x)
        if (x@generatedNewProposal@.Data) {
            updated <- TRUE
            ans.R <- diffLogDensJumpOrigDest(x, useC = FALSE)
            ans.C <- diffLogDensJumpOrigDest(x, useC = TRUE)
            if (test.identity)
                expect_identical(ans.R, ans.C)
            else
                expect_equal(ans.R, ans.C)
        }
    }
    if (!updated)
        warning("not updated")
})

## diffLogDensExpOrigDestPoolNet

test_that("diffLogDensExpOrigDestPoolNet works with CombinedAccountMovements - no age", {
    diffLogDensExpOrigDestPoolNet <- demest:::diffLogDensExpOrigDestPoolNet
    updateProposalAccountMoveOrigDest <- demest:::updateProposalAccountMoveOrigDest
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    getICellCompFromExp <- demest:::getICellCompFromExp
    population <- Counts(array(seq(1000L, 1500L, 100L),
                               dim = c(3, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                               time = c(2000, 2005))))
    internal <- Counts(array(c(0L, 50L, 40L,
                               20L, 0L, 30L,
                               60L, 20L, 0L),
                             dim = c(3, 3, 1),
                             dimnames = list(reg_orig = c("a", "b", "c"),
                                             reg_dest = c("a", "b", "c"),
                                             time = "2001-2005")))
    net.mig <- Counts(array(c(5L, -1L, 10L),
                             dim = c(3, 1),
                             dimnames = list(reg = c("a", "b", "c"),
                                             time = "2001-2005")))
    account <- Movements(population = population,
                         internal = internal,
                         net = list(net.mig = net.mig))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ reg, useExpose = FALSE)),
                         Model(internal ~ Poisson(mean ~ 1)),
                         Model(net.mig ~ Normal(mean ~ 1)))
    systemWeights <- list(NULL, NULL, Counts(array(1,
                                                   dim = c(3, 1),
                                                   dimnames = list(reg = c("a", "b", "c"),
                                                                   time = "2001-2005"))))
    observationModels <- list(Model(tax ~ Poisson(mean ~ 1), series = "internal"),
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
                                 observationModels = observationModels,
                                 seriesIndices = seriesIndices,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    updated <- FALSE
    x0@iComp <- 1L
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x <- updateProposalAccountMoveOrigDest(x0)
        if (x@generatedNewProposal@.Data) {
            ans.obtained <- diffLogDensExpOrigDestPoolNet(x)
            i.cell.orig <- getICellCompFromExp(x@iExpFirst, x@mappingsFromExp[[1]])
            i.cell.dest <- getICellCompFromExp(x@iExpFirstOther, x@mappingsFromExp[[1]])
            ans.expected <- diffLogDensExpOneOrigDestParChPool(iCell = i.cell.orig,
                                                               hasAge = FALSE,
                                                               component = x@account@components[[1]],
                                                               theta = x@systemModels[[2]]@theta,
                                                               iteratorComp = x@iteratorsComp[[1]],
                                                               iExpFirst = x@iExpFirst,
                                                               exposure = x@exposure,
                                                               iteratorExposure = x@iteratorExposure,
                                                               diff = -x@diffProp) +
                diffLogDensExpOneOrigDestParChPool(iCell = i.cell.dest,
                                                   hasAge = FALSE,
                                                   component = x@account@components[[1]],
                                                   theta = x@systemModels[[2]]@theta,
                                                   iteratorComp = x@iteratorsComp[[1]],
                                                   iExpFirst = x@iExpFirstOther,
                                                   exposure = x@exposure,
                                                   iteratorExposure = x@iteratorExposure,
                                                   diff = x@diffProp)
            if (test.identity)
                expect_identical(ans.obtained, ans.expected)
            else
                expect_equal(ans.obtained, ans.expected)
        }
    }
    if (!updated)
        warning("not updated")
})

test_that("R and C versions of diffLogDensExpOrigDestPoolNet give same answer with CombinedAccountMovements - no age", {
    diffLogDensExpOrigDestPoolNet <- demest:::diffLogDensExpOrigDestPoolNet
    updateProposalAccountMoveOrigDest <- demest:::updateProposalAccountMoveOrigDest
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    getICellCompFromExp <- demest:::getICellCompFromExp
    population <- Counts(array(seq(1000L, 1500L, 100L),
                               dim = c(3, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                               time = c(2000, 2005))))
    internal <- Counts(array(c(0L, 50L, 40L,
                               20L, 0L, 30L,
                               60L, 20L, 0L),
                             dim = c(3, 3, 1),
                             dimnames = list(reg_orig = c("a", "b", "c"),
                                             reg_dest = c("a", "b", "c"),
                                             time = "2001-2005")))
    net.mig <- Counts(array(c(5L, -1L, 10L),
                             dim = c(3, 1),
                             dimnames = list(reg = c("a", "b", "c"),
                                             time = "2001-2005")))
    account <- Movements(population = population,
                         internal = internal,
                         net = list(net.mig = net.mig))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ reg, useExpose = FALSE)),
                         Model(internal ~ Poisson(mean ~ 1)),
                         Model(net.mig ~ Normal(mean ~ 1)))
    systemWeights <- list(NULL, NULL, Counts(array(1,
                                                   dim = c(3, 1),
                                                   dimnames = list(reg = c("a", "b", "c"),
                                                                   time = "2001-2005"))))
    observationModels <- list(Model(tax ~ Poisson(mean ~ 1), series = "internal"),
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
                                 observationModels = observationModels,
                                 seriesIndices = seriesIndices,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    updated <- FALSE
    x0@iComp <- 1L
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x <- updateProposalAccountMoveOrigDest(x0)
        if (x@generatedNewProposal@.Data) {
            ans.R <- diffLogDensExpOrigDestPoolNet(x, useC = FALSE)
            ans.C <- diffLogDensExpOrigDestPoolNet(x, useC = TRUE)
            if (test.identity)
                expect_identical(ans.R, ans.C)
            else
                expect_equal(ans.R, ans.C)
        }
    }
    if (!updated)
        warning("not updated")
})


test_that("diffLogDensExpOrigDestPoolNet works - with age", {
    ## diffLogDensExpOrigDestPoolNet <- demest:::diffLogDensExpOrigDestPoolNet
    updateProposalAccountMoveOrigDest <- demest:::updateProposalAccountMoveOrigDest
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    getICellBirthsFromExp <- demest:::getICellBirthsFromExp
    getICellCompFromExp <- demest:::getICellCompFromExp
    diffLogDensExpOneComp <- demest:::diffLogDensExpOneComp
    diffLogDensExpOneOrigDestParChPool <- demest:::diffLogDensExpOneOrigDestParChPool
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
    x0@iComp <- 2L
    updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x <- updateProposalAccountMoveOrigDest(x0)
        if (x@generatedNewProposal@.Data && (x@iExpFirst != x@iExpFirstOther)) {
            updated <- TRUE
            ans.obtained <- diffLogDensExpOrigDestPoolNet(x)
            i.cell.orig <- getICellBirthsFromExp(x@iExpFirst, x@mappingsFromExp[[1]])
            i.cell.dest <- getICellBirthsFromExp(x@iExpFirstOther, x@mappingsFromExp[[1]])
            if (i.cell.orig > 0L && i.cell.orig != i.cell.dest) {
                log.diff.births <- diffLogDensExpOneComp(iCell = i.cell.orig,
                                                         hasAge = TRUE,
                                                         component = x@account@components[[1]],
                                                         theta = x@systemModels[[2]]@theta,
                                                         iteratorComp = x@iteratorsComp[[1]],
                                                         iExpFirst = x@iExpFirst,
                                                         exposure = x@exposure,
                                                         iteratorExposure = x@iteratorExposure,
                                                         diff = -x@diffProp) +
                    diffLogDensExpOneComp(iCell = i.cell.dest,
                                          hasAge = TRUE,
                                          component = x@account@components[[1]],
                                          theta = x@systemModels[[2]]@theta,
                                          iteratorComp = x@iteratorsComp[[1]],
                                          iExpFirst = x@iExpFirstOther,
                                          exposure = x@exposure,
                                          iteratorExposure = x@iteratorExposure,
                                          diff = x@diffProp)
            }
            else
                log.diff.births <- 0
            i.cell.orig <- getICellCompFromExp(x@iExpFirst, x@mappingsFromExp[[2]])
            i.cell.dest <- getICellCompFromExp(x@iExpFirstOther, x@mappingsFromExp[[2]])
            log.diff.internal <- diffLogDensExpOneOrigDestParChPool(iCell = i.cell.orig,
                                                                    hasAge = FALSE,
                                                                    component = x@account@components[[2]],
                                                                    theta = x@systemModels[[3]]@theta,
                                                                    iteratorComp = x@iteratorsComp[[2]],
                                                                    iExpFirst = x@iExpFirst,
                                                                    exposure = x@exposure,
                                                                    iteratorExposure = x@iteratorExposure,
                                                                    diff = -x@diffProp) +
                diffLogDensExpOneOrigDestParChPool(iCell = i.cell.dest,
                                                   hasAge = FALSE,
                                                   component = x@account@components[[2]],
                                                   theta = x@systemModels[[3]]@theta,
                                                   iteratorComp = x@iteratorsComp[[2]],
                                                   iExpFirst = x@iExpFirstOther,
                                                   exposure = x@exposure,
                                                   iteratorExposure = x@iteratorExposure,
                                                   diff = x@diffProp)
            i.cell.orig <- getICellCompFromExp(x@iExpFirst, x@mappingsFromExp[[3]])
            i.cell.dest <- getICellCompFromExp(x@iExpFirstOther, x@mappingsFromExp[[3]])
            log.diff.deaths <- diffLogDensExpOneComp(iCell = i.cell.orig,
                                                     hasAge = TRUE,
                                                     component = x@account@components[[3]],
                                                     theta = x@systemModels[[4]]@theta,
                                                     iteratorComp = x@iteratorsComp[[3]],
                                                     iExpFirst = x@iExpFirst,
                                                     exposure = x@exposure,
                                                     iteratorExposure = x@iteratorExposure,
                                                     diff = -x@diffProp) +
                diffLogDensExpOneComp(iCell = i.cell.dest,
                                      hasAge = TRUE,
                                      component = x@account@components[[3]],
                                      theta = x@systemModels[[4]]@theta,
                                      iteratorComp = x@iteratorsComp[[3]],
                                      iExpFirst = x@iExpFirstOther,
                                      exposure = x@exposure,
                                      iteratorExposure = x@iteratorExposure,
                                      diff = x@diffProp)
            ans.expected <- log.diff.births + log.diff.internal + log.diff.deaths
            print(seed)
            print(c(ans.obtained, ans.expected))
            if (test.identity)
                expect_identical(ans.obtained, ans.expected)
            else
                expect_equal(ans.obtained, ans.expected)
        }
    }
    if (!updated)
        warning("not updated")
})




## diffLogDensExpOneOrigDestParChPool

test_that("diffLogDensExpOneOrigDestParChPool works", {
    diffLogDensExpOneOrigDestParChPool <- demest:::diffLogDensExpOneOrigDestParChPool
    updateProposalAccountMoveOrigDest <- demest:::updateProposalAccountMoveOrigDest
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    population <- Counts(array(seq(1000L, 1500L, 100L),
                               dim = c(3, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                               time = c(2000, 2005))))
    internal <- Counts(array(c(0L, 50L, 40L,
                               20L, 0L, 30L,
                               60L, 20L, 0L),
                             dim = c(3, 3, 1),
                             dimnames = list(reg_orig = c("a", "b", "c"),
                                             reg_dest = c("a", "b", "c"),
                                             time = "2001-2005")))
    account <- Movements(population = population,
                         internal = internal)
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ reg, useExpose = FALSE)),
                         Model(internal ~ Poisson(mean ~ 1)))
    systemWeights <- list(NULL)
    observationModels <- list(Model(tax ~ Poisson(mean ~ 1), series = "internal"),
                              Model(census ~ PoissonBinomial(prob = 0.9), series = "population"))
    seriesIndices <- c(1L, 0L)
    datasets <- list(internal + 10L,
                     population - 5L)
    namesDatasets <- c("tax", "census")
    transforms <- list(makeTransform(x = internal, y = datasets[[1]], subset = TRUE),
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
    x@iComp <- 1L
    updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x <- updateProposalAccountMoveOrigDest(x)
        if (x@generatedNewProposal@.Data) {
            updated <- TRUE
            ans.obtained <- diffLogDensExpOneOrigDestParChPool(iCell = getICellCompFromExp(i = x@iExpFirst,
                                                                                           mapping = x@mappingsFromExp[[1]]),
                                                               hasAge = FALSE,
                                                               component = x@account@components[[1L]],
                                                               theta = x@systemModels[[2]]@theta,
                                                               iteratorComp = x@iteratorsComp[[1L]],
                                                               iExpFirst = x@iExpFirst,
                                                               exposure = x@exposure,
                                                               iteratorExposure = x@iteratorExposure,
                                                               diff = x@diffProp)
            i.cell <- getICellCompFromExp(i = x@iExpFirst,
                                          mapping = x@mappingsFromExp[[1]])
            ans.expected <- sum(dpois(x = internal[i.cell + c(0, 3, 6)],
                                      lambda = (x@exposure[i.cell] + 0.5 * x@diffProp) * x@systemModels[[2]]@theta[i.cell + c(0, 3, 6)],
                                      log = TRUE)) -
                sum(dpois(x = internal[i.cell + c(0, 3, 6)],
                          lambda = x@exposure[i.cell] * x@systemModels[[2]]@theta[i.cell + c(0, 3, 6)],
                          log = TRUE))
            if (test.identity)
                expect_identical(ans.obtained, ans.expected)
            else
                expect_equal(ans.obtained, ans.expected)
        }
        if (!updated)
            warning("not updated")
    }
})


test_that("R and C versions of diffLogDensExpOneOrigDestParChPool give same answer", {
    diffLogDensExpOneOrigDestParChPool <- demest:::diffLogDensExpOneOrigDestParChPool
    getICellCompFromExp <- demest:::getICellCompFromExp
    updateProposalAccountMoveOrigDest <- demest:::updateProposalAccountMoveOrigDest
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    population <- Counts(array(seq(1000L, 1500L, 100L),
                               dim = c(3, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                               time = c(2000, 2005))))
    internal <- Counts(array(c(0L, 50L, 40L,
                               20L, 0L, 30L,
                               60L, 20L, 0L),
                             dim = c(3, 3, 1),
                             dimnames = list(reg_orig = c("a", "b", "c"),
                                             reg_dest = c("a", "b", "c"),
                                             time = "2001-2005")))
    account <- Movements(population = population,
                         internal = internal)
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ reg, useExpose = FALSE)),
                         Model(internal ~ Poisson(mean ~ 1)))
    systemWeights <- list(NULL)
    observationModels <- list(Model(tax ~ Poisson(mean ~ 1), series = "internal"),
                              Model(census ~ PoissonBinomial(prob = 0.9), series = "population"))
    seriesIndices <- c(1L, 0L)
    datasets <- list(internal + 10L,
                     population - 5L)
    namesDatasets <- c("tax", "census")
    transforms <- list(makeTransform(x = internal, y = datasets[[1]], subset = TRUE),
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
    x@iComp <- 1L
    updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x <- updateProposalAccountMoveOrigDest(x)
        if (x@generatedNewProposal@.Data) {
            updated <- TRUE
            ans.R <- diffLogDensExpOneOrigDestParChPool(iCell = getICellCompFromExp(i = x@iExpFirst,
                                                                                    mapping = x@mappingsFromExp[[1]]),
                                                        hasAge = FALSE,
                                                        component = x@account@components[[1L]],
                                                        theta = x@systemModels[[2]]@theta,
                                                        iteratorComp = x@iteratorsComp[[1L]],
                                                        iExpFirst = x@iExpFirst,
                                                        exposure = x@exposure,
                                                        iteratorExposure = x@iteratorExposure,
                                                        diff = x@diffProp,
                                                        useC = FALSE)
            ans.C <- diffLogDensExpOneOrigDestParChPool(iCell = getICellCompFromExp(i = x@iExpFirst,
                                                                                    mapping = x@mappingsFromExp[[1]]),
                                                        hasAge = FALSE,
                                                        component = x@account@components[[1L]],
                                                        theta = x@systemModels[[2]]@theta,
                                                        iteratorComp = x@iteratorsComp[[1L]],
                                                        iExpFirst = x@iExpFirst,
                                                        exposure = x@exposure,
                                                        iteratorExposure = x@iteratorExposure,
                                                        diff = x@diffProp,
                                                        useC = TRUE)
            if (test.identity)
                expect_identical(ans.R, ans.C)
            else
                expect_equal(ans.R, ans.C)
        }
        if (!updated)
            warning("not updated")
    }
})

## diffLogDensExpOneComp

test_that("diffLogDensExpOneComp works", {
    diffLogDensExpOneComp <- demest:::diffLogDensExpOneComp
    CohortIterator <- demest:::CohortIterator
    population <- CountsOne(values = seq(100, 200, 10),
                            labels = seq(2000, 2100, 10),
                            name = "time")
    deaths <- CountsOne(values = rpois(n = 10, lambda = 5),
                        labels = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-"),
                        name = "time")
    deaths <- new("ExitsMovements",
                  .Data = deaths@.Data,
                  metadata = deaths@metadata)
    theta <- ValuesOne(runif(n = 10), labels = dimnames(deaths)$time, name = "time")
    iteratorComp <- CohortIterator(deaths)
    iCell <- 4L
    iExpFirst <- 4L
    expose <- exposure(population)
    expose <- new("Exposure",
                  .Data = expose@.Data,
                  metadata = expose@metadata)
    iteratorExposure <- CohortIterator(expose)
    hasAge <- FALSE
    diff <- -3L
    ans.obtained <- diffLogDensExpOneComp(iCell = iCell,
                                          hasAge = hasAge,
                                          component = deaths,
                                          theta = theta,
                                          iteratorComp = iteratorComp,
                                          iExpFirst = iExpFirst,
                                          exposure = expose,
                                          iteratorExposure = iteratorExposure,
                                          diff = diff)
    ans.expected <- (dpois(deaths[4], lambda = theta[4] * (expose[4] - 3/2), log = TRUE)
        - dpois(deaths[4], lambda = theta[4] * expose[4], log = TRUE)
        + sum(dpois(deaths[5:10], lambda = theta[5:10] * (expose[5:10] - 3), log = TRUE))
        - sum(dpois(deaths[5:10], lambda = theta[5:10] * expose[5:10], log = TRUE)))
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})

test_that("R and C versions of diffLogDensExpOneComp give same answer", {
    diffLogDensExpOneComp <- demest:::diffLogDensExpOneComp
    CohortIterator <- demest:::CohortIterator
    population <- CountsOne(values = seq(100, 200, 10),
                            labels = seq(2000, 2100, 10),
                            name = "time")
    deaths <- CountsOne(values = rpois(n = 10, lambda = 5),
                        labels = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-"),
                        name = "time")
    deaths <- new("ExitsMovements",
                  .Data = deaths@.Data,
                  metadata = deaths@metadata)
    theta <- ValuesOne(runif(n = 10), labels = dimnames(deaths)$time, name = "time")
    iteratorComp <- CohortIterator(deaths)
    iCell <- 4L
    iExpFirst <- 4L
    expose <- exposure(population)
    expose <- new("Exposure",
                  .Data = expose@.Data,
                  metadata = expose@metadata)
    iteratorExposure <- CohortIterator(expose)
    hasAge <- FALSE
    diff <- -3L
    ans.R <- diffLogDensExpOneComp(iCell = iCell,
                                   hasAge = hasAge,
                                   component = deaths,
                                   theta = theta,
                                   iteratorComp = iteratorComp,
                                   iExpFirst = iExpFirst,
                                   exposure = expose,
                                   iteratorExposure = iteratorExposure,
                                   diff = diff,
                                   useC = FALSE)
    ans.C <- diffLogDensExpOneComp(iCell = iCell,
                                   hasAge = hasAge,
                                   component = deaths,
                                   theta = theta,
                                   iteratorComp = iteratorComp,
                                   iExpFirst = iExpFirst,
                                   exposure = expose,
                                   iteratorExposure = iteratorExposure,
                                   diff = diff,
                                   useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
})



##         diffLogDensJumpPoolWithExpose
##         diffLogDensJumpPoolNoExpose
##         diffLogDensJumpNet


## diffLogDensJumpComp

test_that("diffLogDensJumpComp works - no age", {
    diffLogDensJumpComp <- demest:::diffLogDensJumpComp
    updateProposalAccountMoveComp <- demest:::updateProposalAccountMoveComp
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
    x@iComp <- 2L
    updated <- FALSE
    for (seed in seq_len(n.test)) {
        x <- updateProposalAccountMoveComp(x)
        if (x@generatedNewProposal@.Data) {
            updated <- TRUE
            ans.obtained <- diffLogDensJumpComp(x)
            ans.expected <- (dpois(x@account@components[[2]][x@iCell] + x@diffProp,
                                   lambda = (x@exposure[x@iExposure] - 0.5 * x@diffProp) * x@systemModels[[3]]@theta[x@iCell],
                                   log = TRUE)
                - dpois(x@account@components[[2]][x@iCell],
                        lambda = (x@exposure[x@iExposure] - 0.5 * x@diffProp) * x@systemModels[[3]]@theta[x@iCell],
                        log = TRUE)
                + dpois(x@account@components[[2]][x@iCell],
                        lambda = x@expectedExposure[x@iExposure] * x@systemModels[[3]]@theta[x@iCell],
                        log = TRUE)
                - dpois(x@account@components[[2]][x@iCell] + x@diffProp,
                        lambda = x@expectedExposure[x@iExposure] * x@systemModels[[3]]@theta[x@iCell],
                        log = TRUE))
            expect_equal(ans.obtained, ans.expected)
            if (test.identity)
                expect_identical(ans.obtained, ans.expected)
            else
                expect_equal(ans.obtained, ans.expected)
        }
    }
    if (!updated)
        warning("not updated")
})

test_that("R and C versions of diffLogDensJumpComp give same answer - no age", {
    diffLogDensJumpComp <- demest:::diffLogDensJumpComp
    updateProposalAccountMoveComp <- demest:::updateProposalAccountMoveComp
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
    x@iComp <- 2L
    updated <- FALSE
    for (seed in seq_len(n.test)) {
        x <- updateProposalAccountMoveComp(x)
        if (x@generatedNewProposal@.Data) {
            updated <- TRUE
            ans.R <- diffLogDensJumpComp(x, useC = FALSE)
            ans.C <- diffLogDensJumpComp(x, useC = FALSE)
            if (test.identity)
                expect_identical(ans.R, ans.C)
            else
                expect_equal(ans.R, ans.C)
        }
    }
    if (!updated)
        warning("not updated")
})

test_that("diffLogDensJumpComp works - with age", {
    diffLogDensJumpComp <- demest:::diffLogDensJumpComp
    updateProposalAccountMoveComp <- demest:::updateProposalAccountMoveComp
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
    x <- initialCombinedAccount(account = account,
                                systemModels = systemModels,
                                systemWeights = systemWeights,
                                observationModels = observationModels,
                                seriesIndices = seriesIndices,
                                datasets = datasets,
                                namesDatasets = namesDatasets,
                                transforms = transforms)
    x@iComp <- 3L
    updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x <- updateProposalAccountMoveComp(x)
        if (x@generatedNewProposal@.Data) {
            updated <- TRUE
            ans.obtained <- diffLogDensJumpComp(x)
            if (x@isLowerTriangle) {
                ans.expected <- (dpois(x@account@components[[3]][x@iCell] + x@diffProp,
                                       lambda = (x@exposure[x@iCell] - 0.5 * x@diffProp) * x@systemModels[[4]]@theta[x@iCell],
                                       log = TRUE)
                    - dpois(x@account@components[[3]][x@iCell],
                            lambda = (x@exposure[x@iCell] - 0.5 * x@diffProp) * x@systemModels[[4]]@theta[x@iCell],
                            log = TRUE)
                    + dpois(x@account@components[[3]][x@iCell],
                            lambda = x@expectedExposure[x@iCell] * x@systemModels[[4]]@theta[x@iCell],
                            log = TRUE)
                    - dpois(x@account@components[[3]][x@iCell] + x@diffProp,
                            lambda = x@expectedExposure[x@iCell] * x@systemModels[[4]]@theta[x@iCell],
                            log = TRUE))
            }
            else {
                ans.expected <- (dpois(x@account@components[[3]][x@iCell] + x@diffProp,
                                       lambda = (x@exposure[x@iCell]) * x@systemModels[[4]]@theta[x@iCell],
                                       log = TRUE)
                    - dpois(x@account@components[[3]][x@iCell],
                            lambda = (x@exposure[x@iCell]) * x@systemModels[[4]]@theta[x@iCell],
                            log = TRUE)
                    + dpois(x@account@components[[3]][x@iCell],
                            lambda = x@expectedExposure[x@iCell] * x@systemModels[[4]]@theta[x@iCell],
                            log = TRUE)
                    - dpois(x@account@components[[3]][x@iCell] + x@diffProp,
                            lambda = x@expectedExposure[x@iCell] * x@systemModels[[4]]@theta[x@iCell],
                            log = TRUE))
            }
            expect_equal(ans.obtained, ans.expected)
            if (test.identity)
                expect_identical(ans.obtained, ans.expected)
            else
                expect_equal(ans.obtained, ans.expected)
        }
    }
    if (!updated)
        warning("not updated")
})

test_that("R and C versions of diffLogDensJumpComp give same answer - with age", {
    diffLogDensJumpComp <- demest:::diffLogDensJumpComp
    updateProposalAccountMoveComp <- demest:::updateProposalAccountMoveComp
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
    x <- initialCombinedAccount(account = account,
                                systemModels = systemModels,
                                systemWeights = systemWeights,
                                observationModels = observationModels,
                                seriesIndices = seriesIndices,
                                datasets = datasets,
                                namesDatasets = namesDatasets,
                                transforms = transforms)
    x@iComp <- 3L
    updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x <- updateProposalAccountMoveComp(x)
        if (x@generatedNewProposal@.Data) {
            ans.R <- diffLogDensJumpComp(x, useC = FALSE)
            ans.C <- diffLogDensJumpComp(x, useC = FALSE)
            if (test.identity)
                expect_identical(ans.R, ans.C)
            else
                expect_equal(ans.R, ans.C)
        }
    }
    if (!updated)
        warning("not updated")
})

## diffLogDensExpComp

test_that("diffLogDensExpComp works", {
    diffLogDensExpComp <- demest:::diffLogDensExpComp
    updateProposalAccountMoveComp <- demest:::updateProposalAccountMoveComp
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
    x <- initialCombinedAccount(account = account,
                                systemModels = systemModels,
                                systemWeights = systemWeights,
                                observationModels = observationModels,
                                seriesIndices = seriesIndices,
                                datasets = datasets,
                                namesDatasets = namesDatasets,
                                transforms = transforms)
    x@iComp <- 3L
    updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x <- updateProposalAccountMoveComp(x)
        if (x@generatedNewProposal@.Data) {
            updated <- TRUE
            ans.obtained <- diffLogDensExpComp(x)
            ans.expected <- 0
            i.cell.births <- getICellBirthsFromExp(x@iExpFirst, x@mappingsFromExp[[1]])
            if (i.cell.births > 0L)
                ans.expected <- ans.expected + diffLogDensExpOneComp(iCell= i.cell.births,
                                                                     hasAge = TRUE,
                                                                     component = x@account@components[[1]],
                                                                     theta = x@systemModels[[2]]@theta,
                                                                     iteratorComp = x@iteratorsComp[[1]],
                                                                     iExpFirst = x@iExpFirst,
                                                                     exposure = x@exposure,
                                                                     iteratorExposure = x@iteratorExposure,
                                                                     diff = -x@diffProp)
            i.cell.internal <- getICellCompFromExp(x@iExpFirst, x@mappingsFromExp[[2]])
            ans.expected <- ans.expected + diffLogDensExpOneOrigDestParChPool(iCell = i.cell.internal,
                                                                              hasAge = TRUE,
                                                                              component = x@account@components[[2]],
                                                                              theta = x@systemModels[[3]]@theta,
                                                                              iteratorComp = x@iteratorsComp[[2]],
                                                                              iExpFirst = x@iExpFirst,
                                                                              exposure = x@exposure,
                                                                              iteratorExposure = x@iteratorExposure,
                                                                              diff = -x@diffProp)
            i.cell.deaths <- getICellCompFromExp(x@iExpFirst, x@mappingsFromExp[[3]])
                ans.expected <- ans.expected + diffLogDensExpOneComp(iCell= i.cell.deaths,
                                                                     hasAge = TRUE,
                                                                     component = x@account@components[[3]],
                                                                     theta = x@systemModels[[4]]@theta,
                                                                     iteratorComp = x@iteratorsComp[[3]],
                                                                     iExpFirst = x@iExpFirst,
                                                                     exposure = x@exposure,
                                                                     iteratorExposure = x@iteratorExposure,
                                                                     diff = -x@diffProp)
            if (test.identity)
                expect_identical(ans.obtained, ans.expected)
            else
                expect_equal(ans.obtained, ans.expected)
        }
    }
    if (!updated)
        warning("not updated")
})

test_that("R and C versions of diffLogDensExpComp give same value", {
    diffLogDensExpComp <- demest:::diffLogDensExpComp
    updateProposalAccountMoveComp <- demest:::updateProposalAccountMoveComp
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
    x <- initialCombinedAccount(account = account,
                                systemModels = systemModels,
                                systemWeights = systemWeights,
                                observationModels = observationModels,
                                seriesIndices = seriesIndices,
                                datasets = datasets,
                                namesDatasets = namesDatasets,
                                transforms = transforms)
    x@iComp <- 3L
    updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x <- updateProposalAccountMoveComp(x)
        if (x@generatedNewProposal@.Data) {
            updated <- TRUE
            ans.R <- diffLogDensExpComp(x, useC = FALSE)
            ans.C <- diffLogDensExpComp(x, useC = TRUE)
            if (test.R)
                expect_identical(ans.R, ans.C)
            else
                expect_equal(ans.R, ans.C)
        }
    }
    if (!updated)
        warning("not updated")
})


test_that("diffLogDensExpComp works with CombinedAccountMovements - Parent-Child dimensions", {
    updateProposalAccountMoveBirths <- demest:::updateProposalAccountMoveBirths
    initialcombinedaccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    population <- Counts(array(c(200L, 220L, 190L,
                                 220L, 180L, 190L),
                               dim = c(3, 2),
                               dimnames = list(eth = c("A", "B", "C"),
                                               time = c("2000", "2005"))))
    births <- Counts(array(c(40L, 30L, 10L,
                             15L, 40L, 10L,
                             20L, 10L, 50L),
                           dim = c(3, 3, 1),
                           dimnames = list(eth_parent = c("A", "B", "C"),
                                           eth_child = c("A", "B", "C"),
                                           time = "2001-2005")))
    deaths <- Counts(array(c(25L, 10L, 5L),
                           dim = c(3, 1),
                           dimnames = list(eth = c("A", "B", "C"),
                                           time = "2001-2005")))
    account <- Movements(population = population,
                         births = births,
                         exits = list(deaths = deaths))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ time, useExpose = FALSE)),
                         Model(births ~ Poisson(mean ~ 1)),
                         Model(deaths ~ Poisson(mean ~ 1)))
    systemWeights <- rep(list(NULL), 3)
    observationModels <- list(Model(reg.births ~ PoissonBinomial(prob = 0.9), series = "births"),
                              Model(tax ~ Poisson(mean ~ 1), series = "deaths"),
                              Model(census ~ PoissonBinomial(prob = 0.9), series = "population"))
    seriesIndices <- c(1L, 2L, 0L)
    datasets <- list(births + 1L,
                     deaths - 5L,
                     population + 10L)
    namesDatasets <- c("reg.births", "tax", "census")
    transforms <- list(makeTransform(x = births, y = datasets[[1]], subset = TRUE),
                       makeTransform(x = deaths, y = datasets[[2]], subset = TRUE),
                       makeTransform(x = population, y = datasets[[3]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x <- initialCombinedAccount(account = account,
                                systemModels = systemModels,
                                systemWeights = systemWeights,
                                observationModels = observationModels,
                                seriesIndices = seriesIndices,
                                datasets = datasets,
                                namesDatasets = namesDatasets,
                                transforms = transforms)
    expect_true(validObject(x))
    updated <- FALSE
    x@iComp <- 1L
    updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x <- updateProposalAccountMoveBirths(x)
        if (x@generatedNewProposal@.Data) {
            updated <- TRUE
            ans.obtained <- diffLogDensExpComp(x)
            ans.expected <- 0
            i.cell.births <- getICellBirthsFromExp(x@iExpFirst, x@mappingsFromExp[[1]])
            if (i.cell.births > 0L)
                ans.expected <- ans.expected + diffLogDensExpOneOrigDestParChPool(iCell= i.cell.births,
                                                                                  hasAge = FALSE,
                                                                                  component = x@account@components[[1]],
                                                                                  theta = x@systemModels[[2]]@theta,
                                                                                  iteratorComp = x@iteratorsComp[[1]],
                                                                                  iExpFirst = x@iExpFirst,
                                                                                  exposure = x@exposure,
                                                                                  iteratorExposure = x@iteratorExposure,
                                                                                  diff = x@diffProp)
            i.cell.deaths <- getICellCompFromExp(x@iExpFirst, x@mappingsFromExp[[2]])
            ans.expected <- ans.expected + diffLogDensExpOneComp(iCell= i.cell.deaths,
                                                                 hasAge = FALSE,
                                                                 component = x@account@components[[2]],
                                                                 theta = x@systemModels[[3]]@theta,
                                                                 iteratorComp = x@iteratorsComp[[2]],
                                                                 iExpFirst = x@iExpFirst,
                                                                 exposure = x@exposure,
                                                                 iteratorExposure = x@iteratorExposure,
                                                                 diff = x@diffProp)
            if (test.identity)
                expect_identical(ans.obtained, ans.expected)
            else
                expect_equal(ans.obtained, ans.expected)
        }
    }
    if (!updated)
        warning("not updated")
})

test_that("R and C versions of diffLogDensExpComp give same answer with CombinedAccountMovements - Parent-Child dimensions", {
    updateProposalAccountMoveBirths <- demest:::updateProposalAccountMoveBirths
    initialcombinedaccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    population <- Counts(array(c(200L, 220L, 190L,
                                 220L, 180L, 190L),
                               dim = c(3, 2),
                               dimnames = list(eth = c("A", "B", "C"),
                                               time = c("2000", "2005"))))
    births <- Counts(array(c(40L, 30L, 10L,
                             15L, 40L, 10L,
                             20L, 10L, 50L),
                           dim = c(3, 3, 1),
                           dimnames = list(eth_parent = c("A", "B", "C"),
                                           eth_child = c("A", "B", "C"),
                                           time = "2001-2005")))
    deaths <- Counts(array(c(25L, 10L, 5L),
                           dim = c(3, 1),
                           dimnames = list(eth = c("A", "B", "C"),
                                           time = "2001-2005")))
    account <- Movements(population = population,
                         births = births,
                         exits = list(deaths = deaths))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ time, useExpose = FALSE)),
                         Model(births ~ Poisson(mean ~ 1)),
                         Model(deaths ~ Poisson(mean ~ 1)))
    systemWeights <- rep(list(NULL), 3)
    observationModels <- list(Model(reg.births ~ PoissonBinomial(prob = 0.9), series = "births"),
                              Model(tax ~ Poisson(mean ~ 1), series = "deaths"),
                              Model(census ~ PoissonBinomial(prob = 0.9), series = "population"))
    seriesIndices <- c(1L, 2L, 0L)
    datasets <- list(births + 1L,
                     deaths - 5L,
                     population + 10L)
    namesDatasets <- c("reg.births", "tax", "census")
    transforms <- list(makeTransform(x = births, y = datasets[[1]], subset = TRUE),
                       makeTransform(x = deaths, y = datasets[[2]], subset = TRUE),
                       makeTransform(x = population, y = datasets[[3]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x <- initialCombinedAccount(account = account,
                                systemModels = systemModels,
                                systemWeights = systemWeights,
                                observationModels = observationModels,
                                seriesIndices = seriesIndices,
                                datasets = datasets,
                                namesDatasets = namesDatasets,
                                transforms = transforms)
    expect_true(validObject(x))
    updated <- FALSE
    x@iComp <- 1L
    updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x <- updateProposalAccountMoveBirths(x)
        if (x@generatedNewProposal@.Data) {
            updated <- TRUE
            ans.R <- diffLogDensExpComp(x, useC = FALSE)
            ans.C <- diffLogDensExpComp(x, useC = TRUE)
            if (test.R)
                expect_identical(ans.R, ans.C)
            else
                expect_equal(ans.R, ans.C)
        }
    }
    if (!updated)
        warning("not updated")
})



## UPDATE VALUES ################################################################

test_that("updateSubsequentPopnMove works", {
    updateSubsequentPopnMove <- demest:::updateSubsequentPopnMove
    updateProposalAccountMovePopn <- demest:::updateProposalAccountMovePopn
    updateProposalAccountMoveComp <- demest:::updateProposalAccountMoveComp
    updateProposalAccountMoveOrigDest <- demest:::updateProposalAccountMoveOrigDest
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
    internal <- Counts(array(rpois(n = 300, lambda = 5),
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
                         Model(internal ~ Poisson(mean ~ reg_orig)),
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
    x <- initialCombinedAccount(account = account,
                                systemModels = systemModels,
                                systemWeights = systemWeights,
                                observationModels = observationModels,
                                seriesIndices = seriesIndices,
                                datasets = datasets,
                                namesDatasets = namesDatasets,
                                transforms = transforms)
    tested.popn <- FALSE
    tested.orig.dest <- FALSE
    tested.comp <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ## updating population
        x0 <- x
        x0@iComp <- 0L
        x1 <- updateProposalAccountMovePopn(x0)
        if (x1@generatedNewProposal@.Data) {
            tested.popn <- TRUE
            ans.obtained <- updateSubsequentPopnMove(x1)
            expect_equal(sum(ans.obtained@account@population),
                         sum(x1@account@population) + 3 * x1@diffProp)
        }
        ## updating orig-dest
        x0 <- x
        x0@iComp <- 2L
        x1 <- updateProposalAccountMoveOrigDest(x0)
        if (x1@generatedNewProposal@.Data) {
            tested.orig.dest <- TRUE
            ans.obtained <- updateSubsequentPopnMove(x1)
            expect_equal(sum(ans.obtained@account@population),
                         sum(x1@account@population))
        }
        ## updating component
        x0 <- x
        x0@iComp <- 3L
        x1 <- updateProposalAccountMoveComp(x0)
        if (x1@generatedNewProposal@.Data) {
            tested.comp <- TRUE
            ans.obtained <- updateSubsequentPopnMove(x1)
            time <- as.data.frame(x1@account@components[[3]], direction = "long")[x1@iCell, "time"]
            if (time == "2001-2005")
                expect_equal(sum(ans.obtained@account@population),
                             sum(x1@account@population) - 2 * x1@diffProp)
            else
                expect_equal(sum(ans.obtained@account@population),
                             sum(x1@account@population) - x1@diffProp)
        }
    }
    if (!tested.popn)
        warning("updateSubsequentPopnMove not tested with popn")
    if (!tested.orig.dest)
        warning("updateSubsequentPopnMove not tested with orig-dest")
    if (!tested.comp)
        warning("updateSubsequentPopnMove not tested with comp")
})

test_that("R and C versions of updateSubsequentPopnMove give same answer", {
    updateSubsequentPopnMove <- demest:::updateSubsequentPopnMove
    updateProposalAccountMovePopn <- demest:::updateProposalAccountMovePopn
    updateProposalAccountMoveComp <- demest:::updateProposalAccountMoveComp
    updateProposalAccountMoveOrigDest <- demest:::updateProposalAccountMoveOrigDest
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
    internal <- Counts(array(rpois(n = 300, lambda = 5),
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
                         Model(internal ~ Poisson(mean ~ reg_orig)),
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
    x <- initialCombinedAccount(account = account,
                                systemModels = systemModels,
                                systemWeights = systemWeights,
                                observationModels = observationModels,
                                seriesIndices = seriesIndices,
                                datasets = datasets,
                                namesDatasets = namesDatasets,
                                transforms = transforms)
    tested.popn <- FALSE
    tested.orig.dest <- FALSE
    tested.comp <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ## updating population
        x0 <- x
        x0@iComp <- 0L
        x1 <- updateProposalAccountMovePopn(x0)
        if (x1@generatedNewProposal@.Data) {
            tested.popn <- TRUE
            ans.R <- updateSubsequentPopnMove(x1, useC = FALSE)
            ans.C <- updateSubsequentPopnMove(x1, useC = TRUE)
            if (test.identity)
                expect_identical(ans.R, ans.C)
            else
                expect_equal(ans.R, ans.C)
        }
        ## updating orig-dest
        x0 <- x
        x0@iComp <- 2L
        x1 <- updateProposalAccountMoveOrigDest(x0)
        if (x1@generatedNewProposal@.Data) {
            tested.orig.dest <- TRUE
            ans.R <- updateSubsequentPopnMove(x1, useC = FALSE)
            ans.C <- updateSubsequentPopnMove(x1, useC = TRUE)
            if (test.identity)
                expect_identical(ans.R, ans.C)
            else
                expect_equal(ans.R, ans.C)
        }
        ## updating component
        x0 <- x
        x0@iComp <- 3L
        x1 <- updateProposalAccountMoveComp(x0)
        if (x1@generatedNewProposal@.Data) {
            tested.comp <- TRUE
            ans.R <- updateSubsequentPopnMove(x1, useC = FALSE)
            ans.C <- updateSubsequentPopnMove(x1, useC = TRUE)
            if (test.identity)
                expect_identical(ans.R, ans.C)
            else
                expect_equal(ans.R, ans.C)
        }
    }
    if (!tested.popn)
        warning("updateSubsequentPopnMove not tested with popn")
    if (!tested.orig.dest)
        warning("updateSubsequentPopnMove not tested with orig-dest")
    if (!tested.comp)
        warning("updateSubsequentPopnMove not tested with comp")
})



