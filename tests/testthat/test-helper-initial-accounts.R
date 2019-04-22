
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

test_that("checkAndTidyUpdateDataModel works - no aggregate values", {
    checkAndTidyUpdateDataModel <- demest:::checkAndTidyUpdateDataModel
    initialModel <- demest:::initialModel
    specs <- list(Model(census ~ PoissonBinomial(prob = 0.99), series = "population"),
                  Model(arrivals ~ Poisson(mean ~ time), series = "inmigration"))
    datasets <- list(Counts(array(1L,
                                  dim = 4,
                                  dimnames = list(time = 2000:2003)),
                            dimscales = c(time = "Points")),
                     Counts(array(1L,
                                  dim = 3,
                                  dimnames = list(time = 2001:2003)),
                            dimscales = c(time = "Intervals")))
    dataModels <- list(initialModel(specs[[1]], y = datasets[[1]], exposure = datasets[[1]]),
                       initialModel(specs[[2]], y = datasets[[2]], exposure = datasets[[2]]))
    ans.obtained <- checkAndTidyUpdateDataModel(updateDataModel = NULL,
                                                dataModels = dataModels,
                                                namesDatasets = c("census", "arrivals"))
    ans.expected <- c(FALSE, FALSE)
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- checkAndTidyUpdateDataModel(updateDataModel = "census",
                                                dataModels = dataModels,
                                                namesDatasets = c("census", "arrivals"))
    ans.expected <- c(TRUE, FALSE)
    expect_identical(ans.obtained, ans.expected)
    expect_error(checkAndTidyUpdateDataModel(updateDataModel = FALSE,
                                             dataModels = dataModels,
                                             namesDatasets = c("census", "arrivals")),
                 "'updateDataModel' has class \"logical\"")
    expect_error(checkAndTidyUpdateDataModel(updateDataModel = as.character(NA),
                                             dataModels = dataModels,
                                             namesDatasets = c("census", "arrivals")),
                 "'updateDataModel' has missing values")
    expect_error(checkAndTidyUpdateDataModel(updateDataModel = c("population", "population"),
                                             dataModels = dataModels,
                                             namesDatasets = c("census", "arrivals")),
                 "'updateDataModel' has duplicates")
    expect_error(checkAndTidyUpdateDataModel(updateDataModel = "wrong",
                                             dataModels = dataModels,
                                             namesDatasets = c("census", "arrivals")),
                 "element \"wrong\" of 'updateDataModel' is not the name of a demographic series")
})


test_that("checkAndTidyUpdateDataModel works - with aggregate values", {
    checkAndTidyUpdateDataModel <- demest:::checkAndTidyUpdateDataModel
    initialModel <- demest:::initialModel
    specs <- list(Model(census ~ PoissonBinomial(prob = 0.99), series = "population"),
                  Model(arrivals ~ Poisson(mean ~ time), series = "inmigration",
                        aggregate = AgCertain(3)))
    datasets <- list(Counts(array(1L,
                                  dim = 4,
                                  dimnames = list(time = 2000:2003)),
                            dimscales = c(time = "Points")),
                     Counts(array(1L,
                                  dim = 3,
                                  dimnames = list(time = 2001:2003)),
                            dimscales = c(time = "Intervals")))
    dataModels <- list(initialModel(specs[[1]], y = datasets[[1]], exposure = datasets[[1]]),
                       initialModel(specs[[2]], y = datasets[[2]], exposure = datasets[[2]]))
    ans.obtained <- checkAndTidyUpdateDataModel(updateDataModel = NULL,
                                                dataModels = dataModels,
                                                namesDatasets = c("census", "arrivals"))
    ans.expected <- c(TRUE, TRUE)
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- checkAndTidyUpdateDataModel(updateDataModel = c("arrivals", "census"),
                                                dataModels = dataModels,
                                                namesDatasets = c("census", "arrivals"))
    ans.expected <- c(TRUE, TRUE)
    expect_identical(ans.obtained, ans.expected)
    expect_error(checkAndTidyUpdateDataModel(updateDataModel = FALSE,
                                             dataModels = dataModels,
                                             namesDatasets = c("census", "arrivals")),
                 "'updateDataModel' has class \"logical\"")
    expect_error(checkAndTidyUpdateDataModel(updateDataModel = as.character(NA),
                                             dataModels = dataModels,
                                             namesDatasets = c("census", "arrivals")),
                 "'updateDataModel' has missing values")
    expect_error(checkAndTidyUpdateDataModel(updateDataModel = c("population", "population"),
                                             dataModels = dataModels,
                                             namesDatasets = c("census", "arrivals")),
                 "'updateDataModel' has duplicates")
    expect_error(checkAndTidyUpdateDataModel(updateDataModel = "wrong",
                                             dataModels = dataModels,
                                             namesDatasets = c("census", "arrivals")),
                 "element \"wrong\" of 'updateDataModel' is not the name of a demographic series")
})

test_that("checkAndTidyUpdateSystemModel works - no aggregate values", {
    checkAndTidyUpdateSystemModel <- demest:::checkAndTidyUpdateSystemModel
    initialModel <- demest:::initialModel
    specs <- list(Model(population ~ Poisson(mean ~ time, useExpose = FALSE)),
                  Model(inmigration ~ Poisson(mean ~ time)))
    data <- list(Counts(array(1L,
                              dim = 4,
                              dimnames = list(time = 2000:2003)),
                        dimscales = c(time = "Points")),
                 Counts(array(1L,
                              dim = 3,
                              dimnames = list(time = 2001:2003)),
                        dimscales = c(time = "Intervals")))
    systemModels <- list(initialModel(specs[[1]], y = data[[1]], exposure = NULL),
                         initialModel(specs[[2]], y = data[[2]], exposure = data[[2]]))
    ans.obtained <- checkAndTidyUpdateSystemModel(updateSystemModel = NULL,
                                                  systemModels = systemModels,
                                                  componentNames = "inmigration")
    ans.expected <- c(FALSE, FALSE)
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- checkAndTidyUpdateSystemModel(updateSystemModel = "population",
                                                  systemModels = systemModels,
                                                  componentNames = "inmigration")
    ans.expected <- c(TRUE, FALSE)
    expect_identical(ans.obtained, ans.expected)
    expect_error(checkAndTidyUpdateSystemModel(updateSystemModel = FALSE,
                                               systemModels = systemModels,
                                               componentNames = "inmigration"),
                 "'updateSystemModel' has class \"logical\"")
    expect_error(checkAndTidyUpdateSystemModel(updateSystemModel = as.character(NA),
                                               systemModels = systemModels,
                                               componentNames = "inmigration"),
                 "'updateSystemModel' has missing values")
    expect_error(checkAndTidyUpdateSystemModel(updateSystemModel = c("population", "population"),
                                               systemModels = systemModels,
                                               componentNames = "inmigration"),
                 "'updateSystemModel' has duplicates")
    expect_error(checkAndTidyUpdateSystemModel(updateSystemModel = "wrong",
                                               systemModels = systemModels,
                                               componentNames = "inmigration"),
                 "element \"wrong\" of 'updateSystemModel' is not the name of a demographic series")
})

test_that("checkAndTidyUpdateSystemModel works - with aggregate values", {
    checkAndTidyUpdateSystemModel <- demest:::checkAndTidyUpdateSystemModel
    initialModel <- demest:::initialModel
    specs <- list(Model(population ~ Poisson(mean ~ time, useExpose = FALSE)),
                  Model(inmigration ~ Poisson(mean ~ time),
                        aggregate = AgCertain(3)))
    data <- list(Counts(array(1L,
                              dim = 4,
                              dimnames = list(time = 2000:2003)),
                        dimscales = c(time = "Points")),
                 Counts(array(1L,
                              dim = 3,
                              dimnames = list(time = 2001:2003)),
                        dimscales = c(time = "Intervals")))
    systemModels <- list(initialModel(specs[[1]], y = data[[1]], exposure = NULL),
                         initialModel(specs[[2]], y = data[[2]], exposure = data[[2]]))
    ans.obtained <- checkAndTidyUpdateSystemModel(updateSystemModel = NULL,
                                                  systemModels = systemModels,
                                                  componentNames = "inmigration")
    ans.expected <- c(TRUE, TRUE)
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- checkAndTidyUpdateSystemModel(updateSystemModel = c("inmigration", "population"),
                                                  systemModels = systemModels,
                                                  componentNames = "inmigration")
    ans.expected <- c(TRUE, TRUE)
    expect_identical(ans.obtained, ans.expected)
    expect_error(checkAndTidyUpdateSystemModel(updateSystemModel = FALSE,
                                               systemModels = systemModels,
                                               componentNames = "inmigration"),
                 "'updateSystemModel' has class \"logical\"")
    expect_error(checkAndTidyUpdateSystemModel(updateSystemModel = as.character(NA),
                                               systemModels = systemModels,
                                               componentNames = "inmigration"),
                 "'updateSystemModel' has missing values")
    expect_error(checkAndTidyUpdateSystemModel(updateSystemModel = c("population", "population"),
                                               systemModels = systemModels,
                                               componentNames = "inmigration"),
                 "'updateSystemModel' has duplicates")
    expect_error(checkAndTidyUpdateSystemModel(updateSystemModel = "wrong",
                                               systemModels = systemModels,
                                               componentNames = "inmigration"),
                 "element \"wrong\" of 'updateSystemModel' is not the name of a demographic series")
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
