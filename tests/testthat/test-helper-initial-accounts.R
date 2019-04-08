
context("helper-functions")

n.test <- 5
test.identity <- FALSE
test.extended <- FALSE


test_that("alignSystemModelsToAccount works", {
    alignSystemModelsToAccount <- demest:::alignSystemModelsToAccount
    system.models <- list(Model(deaths ~ Poisson(mean ~ time)),
                          Model(population ~ Poisson(mean ~ eth, useExpose = FALSE)),
                          Model(births ~ Poisson(mean ~ eth)))
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
    ans.obtained <- alignSystemModelsToAccount(systemModels = system.models,
                                               account = account)
    ans.expected <- list(Model(population ~ Poisson(mean ~ eth, useExpose = FALSE)),
                         Model(births ~ Poisson(mean ~ eth)),
                         Model(deaths ~ Poisson(mean ~ time)))
    expect_identical(ans.obtained, ans.expected)
    ## no system models without series
    system.models.wrong <- c(system.models,
                             Model(wrong ~ Poisson(mean ~ eth)))
    expect_error(alignSystemModelsToAccount(systemModels = system.models.wrong,
                                            account = account),
                 "'systemModels' contains a system model for 'wrong', but there is no series called 'wrong' in 'account'")
    ## no series without system models
    system.models.wrong <- system.models[-1]
    expect_error(alignSystemModelsToAccount(systemModels = system.models.wrong,
                                            account = account),
                 "'systemModels' does not contain a model for series 'deaths' in 'account'")
    ## model for population does not have exposure term
    system.models.wrong <-  system.models
    system.models.wrong[2] <- list(Model(population ~ Poisson(mean ~ eth, useExpose = TRUE)))
    expect_error(alignSystemModelsToAccount(systemModels = system.models.wrong,
                                            account = account),
                 "system model for 'population' uses exposure")
})

test_that("checkAndTidySystemWeights works", {
    checkAndTidySystemWeights <- demest:::checkAndTidySystemWeights
    weights <- list(migration = Counts(array(1,
                                             dim = c(2, 3),
                                             dimnames = list(age = c("0-4", "5+"),
                                                             reg = c("a", "b", "c")))))
    system.models <- list(Model(population ~ Poisson(mean ~ age + sex)),
                          Model(migration ~ Normal(mean ~ age + region)))
    ans.obtained <- checkAndTidySystemWeights(weights = weights,
                                              systemModels = system.models)
    ans.expected <- list(NULL, weights[[1]])
    expect_identical(ans.obtained, ans.expected)
    ## not weights without system model
    weights.wrong <- weights
    names(weights.wrong)[1] <- "wrong"
    expect_error(checkAndTidySystemWeights(weights.wrong, systemModels = system.models),
                 "'weights' contains weights for 'wrong', but 'systemModels' does not contain a model for 'wrong'")
    ## weights supplied only if system model needs them
    weights.wrong <- weights
    names(weights.wrong)[1] <- "population"
    expect_error(checkAndTidySystemWeights(weights.wrong, systemModels = system.models),
                 "'weights' contains weights for 'population', but system model for 'population' does not use weights")
})

test_that("checkSystemModels works", {
    checkSystemModels <- demest:::checkSystemModels
    x <- list(Model(population ~ Poisson(mean ~ age + sex)),
              Model(deaths ~ Poisson(mean ~ age + region)))
    ans.obtained <- checkSystemModels(x)
    ans.expected <- NULL
    expect_identical(ans.obtained, ans.expected)
    ## 'systemModels' is a list
    expect_error(checkSystemModels("wrong"),
                 "'systemModels' has class \"character\"")
    ## element has class "SpecModel"
    x.wrong <- x
    x.wrong[[1]] <- "wrong"
    expect_error(checkSystemModels(x.wrong),
                 "element 1 of 'systemModels' has class \"character\"")
    ## specification is valid
    x.wrong <- x
    x.wrong[[1]]@upper <- -2
    expect_error(checkSystemModels(x.wrong),
                 "element 1 of 'systemModels' is invalid")
    ## no 'series' argument supplied
    x.wrong <- x
    x.wrong[[1]] <- Model(population ~ Poisson(mean ~ age + sex),
                          series = "wrong")
    expect_error(checkSystemModels(x.wrong),
                 "element 1 of 'systemModels' has value for 'series' \\[\"wrong\"\\] : in system models, series should instead be specified via response variable")
})

test_that("makeSeriesIndices works", {
    makeSeriesIndices <- demest:::makeSeriesIndices
    obs.models <- list(Model(census ~ Poisson(mean ~ eth), series = "population"),
                       Model(reg.births ~ Poisson(mean ~ eth), series = "births"),
                       Model(reg.deaths ~ Poisson(mean ~ time), series = "deaths"),
                       Model(health ~ PoissonBinomial(prob = 0.95), series = "population"))
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
    ans.obtained <- makeSeriesIndices(dataModels = obs.models,
                                      account = account)
    ans.expected <- c(0L, 1L, 2L, 0L)
    expect_identical(ans.obtained, ans.expected)
    ## no observation models without series
    obs.models.wrong <- c(obs.models,
                          Model(wrong ~ Poisson(mean ~ eth), series = "wrong"))
    expect_error(makeSeriesIndices(dataModels = obs.models.wrong,
                                   account = account),
                 "'dataModels' contains a model for 'wrong', but 'account' does not have a series called 'wrong'")
})

test_that("makeTransformsAccountToDatasets works", {
    makeTransformsAccountToDatasets <- demest:::makeTransformsAccountToDatasets
    makeCollapseTransformExtra <- dembase:::makeCollapseTransformExtra
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
    datasets <- list(population + 1L, births + 1L, deaths + 1L)
    datasets[[3]] <- collapseCategories(datasets[[3]], dimension = "eth", old = c("B", "C"), new = "D")
    namesDatasets <- c("population.data", "births.data", "deaths.data")
    seriesIndices <- 0:2
    concordances <- list(deaths.data = list(eth = Concordance(data.frame(from = c("A", "B", "C"),
                                                                         to = c("A", "D", "D")))))
    ans.obtained <- makeTransformsAccountToDatasets(account = account,
                                                    datasets = datasets,
                                                    concordances = concordances,
                                                    namesDatasets = namesDatasets,
                                                    seriesIndices = seriesIndices)
    ans.expected <- list(makeTransform(account@population, datasets[[1]]),
                         makeTransform(account@components[[1]], datasets[[2]]),
                         makeTransform(account@components[[2]], datasets[[3]],
                                       concordances = concordances[[1]]))
    ans.expected <- lapply(ans.expected, makeCollapseTransformExtra)
    expect_identical(ans.obtained, ans.expected)
    ## no observation models without series
    deaths.wrong <- Counts(array(c(25L, 10L, 5L),
                                 dim = c(3, 1),
                                 dimnames = list(eth = c("A", "B", "D"),
                                                 time = "2001-2005")))
    datasets.wrong <- list(population + 1L, births + 1L, deaths.wrong)
    expect_error(makeTransformsAccountToDatasets(account = account,
                                                 datasets = datasets.wrong,
                                                 namesDatasets = namesDatasets,
                                                 concordances = concordances,
                                                 seriesIndices = seriesIndices),
                 "unable to collapse series 'deaths' to make it compatible with dataset 'deaths.data'")
})
