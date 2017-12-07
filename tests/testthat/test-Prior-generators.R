

context("Prior-generators")

n.test <- 5
test.identity <- FALSE
test.extended <- TRUE


## test generator function and initialPrior together ############################################

test_that("generator function and initialPrior work with ExchFixed", {
    initialPrior <- demest:::initialPrior
    set.seed(100)
    ## intercept
    spec <- ExchFixed()
    expect_is(spec, "SpecExchFixed")
    beta <- rnorm(1)
    strucZeroArray <- Counts(array(c(1L, 0L),
                                   dim = c(2:3, 3),
                                   dimnames = list(sex = c("f", "m"),
                                                   eth_orig = c("a", "b", "c"),
                                                   eth_dest = c("a", "b", "c"))))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = NULL,
                          sY = 50,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "ExchFixed")
    expect_identical(prior@J, new("Length", 1L))
    expect_identical(prior@tau, new("Scale", 500))
    expect_identical(prior@allStrucZero, FALSE)
    ## length 2
    spec <- ExchFixed()
    expect_is(spec, "SpecExchFixed")
    beta <- rnorm(2)
    metadata <- new("MetaData",
                    nms = "sex",
                    dimtypes = "sex",
                    DimScales = list(new("Sexes", dimvalues = c("f", "m"))))
    strucZeroArray <- Counts(array(c(1L, 0L),
                                   dim = c(2:3, 3),
                                   dimnames = list(sex = c("f", "m"),
                                                   eth_orig = c("a", "b", "c"),
                                                   eth_dest = c("a", "b", "c"))))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "ExchFixed")
    expect_identical(prior@J, new("Length", 2L))
    expect_identical(prior@tau, new("Scale", 1.0))
    expect_identical(prior@allStrucZero, c(FALSE, TRUE))
    ## no metadata but J > 1
    spec <- ExchFixed()
    expect_is(spec, "SpecExchFixed")
    beta <- rnorm(2)
    expect_error(initialPrior(spec,
                              beta = beta,
                              metadata = NULL,
                              sY = 50,
                              isSaturated = FALSE,
                              strucZeroArray = strucZeroArray),
                 "'metadata' is NULL but 'J' is greater than 1")
    ## has metadata but J == 1
    spec <- ExchFixed()
    expect_is(spec, "SpecExchFixed")
    beta <- rnorm(1)
    metadata <- new("MetaData",
                    nms = "sex",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = c("f", "m"))))
    expect_error(initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE,
                              strucZeroArray = strucZeroArray),
                 "'metadata' is not NULL but 'J' is less than or equal to 1")
})

test_that("generator function and initialPrior work with ExchNormZero", {
    set.seed(100)
    initialPrior <- demest:::initialPrior
    ## main effect; sY is NULL
    spec <- Exch()
    expect_is(spec, "SpecExchNormZero")
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(c(rep(1L, 19), 0L),
                                   dim = c(2, 10),
                                   dimnames = list(sex = c("f", "m"), region = letters[1:10])))
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "ExchNormZero")
    expect_identical(prior@J, new("Length", 10L))
    expect_identical(prior@ATau, new("Scale", 1.0))
    expect_identical(prior@nuTau, new("DegreesFreedom", 7.0))
    expect_identical(prior@tauMax, new("Scale", qhalft(p = 0.999, df = 7, scale = 1)))
    expect_identical(prior@allStrucZero, rep(FALSE, 10))
    ## interaction; sY is 1000
    spec <- Exch()
    expect_is(spec, "SpecExchNormZero")
    beta <- rnorm(20, mean = 200)
    metadata <- new("MetaData",
                    nms = c("region", "sex"),
                    dimtypes = c("state", "sex"),
                    DimScales = list(new("Categories", dimvalues = letters[1:10]),
                                     new("Sexes", dimvalues = c("f", "m"))))
    strucZeroArray <- Counts(array(c(rep(1L, 19), 0L),
                                   dim = c(2, 10),
                                   dimnames = list(sex = c("f", "m"), region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = 1000,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "ExchNormZero")
    expect_identical(prior@J, new("Length", 20L))
    expect_identical(prior@ATau, new("Scale", 500))
    expect_identical(prior@nuTau, new("DegreesFreedom", 7.0))
    expect_identical(prior@tauMax, new("Scale", qhalft(p = 0.999, df = 7, scale = 500)))
    expect_identical(prior@allStrucZero, c(rep(FALSE, 19), TRUE))
    ## ATau = 10; nuTau = 1
    spec <- Exch(error = Error(scale = HalfT(df = 1, scale = 10)))
    expect_is(spec, "SpecExchNormZero")
    beta <- rnorm(20, mean = 200)
    metadata <- new("MetaData",
                    nms = c("region", "sex"),
                    dimtypes = c("state", "sex"),
                    DimScales = list(new("Categories", dimvalues = letters[1:10]),
                                     new("Sexes", dimvalues = c("f", "m"))))
    strucZeroArray <- Counts(array(c(rep(1L, 19), 0L),
                                   dim = c(2, 10),
                                   dimnames = list(sex = c("f", "m"), region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = 1000,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "ExchNormZero")
    expect_identical(prior@ATau, new("Scale", 10))
    expect_identical(prior@nuTau, new("DegreesFreedom", 1))
    ## max = 1.5
    spec <- Exch(error = Error(scale = HalfT(max = 1.5)))
    expect_is(spec, "SpecExchNormZero")
    beta <- rnorm(20, mean = 200)
    metadata <- new("MetaData",
                    nms = c("region", "sex"),
                    dimtypes = c("state", "sex"),
                    DimScales = list(new("Categories", dimvalues = letters[1:10]),
                                     new("Sexes", dimvalues = c("f", "m"))))
    strucZeroArray <- Counts(array(rep(c(1L, 0L), times = 10),
                                   dim = c(2, 10),
                                   dimnames = list(sex = c("f", "m"), region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "ExchNormZero")
    expect_identical(prior@ATau, new("Scale", 0.5))
    expect_identical(prior@nuTau, new("DegreesFreedom", 7))
    expect_identical(prior@tauMax, new("Scale", 1.5))
    expect_identical(prior@allStrucZero, rep(c(FALSE, TRUE), each = 10))
})

test_that("generator function and initialPrior work with ExchRobustZero", {
    set.seed(100)
    initialPrior <- demest:::initialPrior
    spec <- Exch(error = Error(robust = TRUE))
    expect_is(spec, "SpecExchRobustZero")
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(rep(c(1L, 0L), times = 10),
                                   dim = c(2, 10),
                                   dimnames = list(sex = c("f", "m"), region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "ExchRobustZero")
    expect_identical(prior@J, new("Length", 10L))
    expect_identical(prior@ATau, new("Scale", 1.0))
    expect_identical(prior@nuTau, new("DegreesFreedom", 7.0))
    expect_identical(prior@nuBeta, new("DegreesFreedom", 4.0))
    expect_identical(prior@allStrucZero, rep(FALSE, 10))
})

test_that("generator function and initialPrior work with ExchNormCov", {
    set.seed(100)
    initialPrior <- demest:::initialPrior
    data <- data.frame(region = rep(letters[1:10], times = 2),
                       sex = rep(c("f", "m"), each = 10),
                       income = rnorm(20),
                       cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
    formula <- mean ~ income * cat
    contrastsArg = list(cat = diag(3))
    spec <- Exch(covariates = Covariates(formula = formula,
                     data = data, contrastsArg = contrastsArg))
    expect_is(spec, "SpecExchNormCov")
    beta <- rnorm(20)
    metadata <- new("MetaData",
                    nms = c("region", "sex"),
                    dimtypes = c("state", "sex"),
                    DimScales = list(new("Categories", dimvalues = letters[1:10]),
                                     new("Sexes", dimvalues = c("f", "m"))))
    strucZeroArray <- Counts(array(rep(c(1L, 0L), times = c(18, 2)),
                                   dim = c(2, 10),
                                   dimnames = list(sex = c("f", "m"), region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "ExchNormCov")
    expect_identical(prior@J, new("Length", 20L))
    expect_identical(prior@ATau, new("Scale", 0.5))
    expect_identical(prior@nuTau, new("DegreesFreedom", 7.0))
    expect_identical(prior@contrastsArg, contrastsArg)
    expect_identical(prior@formula, formula)
    expect_identical(prior@allStrucZero, rep(c(rep(FALSE, 9), TRUE), 2))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = TRUE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "ExchNormCov")
    expect_identical(prior@isSaturated, new("LogicalFlag", TRUE))
    expect_identical(prior@allStrucZero, rep(c(rep(FALSE, 9), TRUE), 2))
})

test_that("generator function and initialPrior work with ExchRobustCov", {
    set.seed(100)
    initialPrior <- demest:::initialPrior
    data <- data.frame(region = rep(letters[1:10], times = 2),
                       sex = rep(c("f", "m"), each = 10),
                       income = rnorm(20),
                       cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
    formula <- mean ~ income * cat
    contrastsArg = list(cat = diag(3))
    spec <- Exch(covariates = Covariates(formula = formula,
                     data = data, contrastsArg = contrastsArg),
                 error = Error(robust = TRUE))
    expect_is(spec, "SpecExchRobustCov")
    beta <- rnorm(20)
    metadata <- new("MetaData",
                    nms = c("region", "sex"),
                    dimtypes = c("state", "sex"),
                    DimScales = list(new("Categories", dimvalues = letters[1:10]),
                        new("Sexes", dimvalues = c("f", "m"))))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(2, 10),
                                   dimnames = list(sex = c("f", "m"), region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "ExchRobustCov")
    expect_identical(prior@J, new("Length", 20L))
    expect_identical(prior@ATau, new("Scale", 0.5))
    expect_identical(prior@nuBeta, new("DegreesFreedom", 4.0))
    expect_identical(prior@nuTau, new("DegreesFreedom", 7.0))
    expect_identical(prior@contrastsArg, contrastsArg)
    expect_identical(prior@formula, formula)
    expect_identical(prior@allStrucZero, rep(FALSE, 20))
})

test_that("generator function and initialPrior work with DLMNoTrendNormZeroNoSeason", {
    set.seed(100)
    initialPrior <- demest:::initialPrior
    spec <- DLM(trend = NULL)
    expect_is(spec, "SpecDLMNoTrendNormZeroNoSeason")
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = c(2, 10),
                                   dimnames = list(sex = c("f", "m"),
                                                   time = 2001:2010)),
                             dimscales = c(time = "Points"))
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMNoTrendNormZeroNoSeason")
})

test_that("generator function and initialPrior work with DLMWithTrendNormZeroNoSeason", {
    set.seed(100)
    initialPrior <- demest:::initialPrior
    spec <- DLM()
    expect_is(spec, "SpecDLMWithTrendNormZeroNoSeason")
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(2, 10),
                                   dimnames = list(sex = c("f", "m"),
                                                   time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMWithTrendNormZeroNoSeason")
    spec <- DLM(trend = Trend(initial = Initial(mean = 0.02, sd = 0.05)))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_identical(prior@m0WithTrend[[1]], c(0, 0.02))
    expect_identical(prior@CWithTrend[[1]], diag(c(100, 0.05^2)))
})

test_that("generator function and initialPrior work with DLMNoTrendNormZeroWithSeason", {
    set.seed(100)
    initialPrior <- demest:::initialPrior
    spec <- DLM(trend = NULL, season = Season(n = 4))
    expect_is(spec, "SpecDLMNoTrendNormZeroWithSeason")
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(2, 10),
                                   dimnames = list(sex = c("f", "m"),
                                                   time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMNoTrendNormZeroWithSeason")
})

test_that("generator function and initialPrior work with DLMWithTrendNormZeroWithSeason", {
    set.seed(100)
    initialPrior <- demest:::initialPrior
    spec <- DLM(season = Season(n = 4))
    expect_is(spec, "SpecDLMWithTrendNormZeroWithSeason")
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = c(2, 10),
                                   dimnames = list(sex = c("f", "m"),
                                                   time = 2001:2010)),
                             dimscales = c(time = "Points"))
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMWithTrendNormZeroWithSeason")
})

test_that("generator function and initialPrior work with DLMNoTrendNormZeroNoSeason", {
    set.seed(100)
    initialPrior <- demest:::initialPrior
    data <- data.frame(time = rep(2001:2010, times = 2),
                       reg = rep(c("a", "b"), each = 10),
                       gdp = rnorm(20),
                       cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
    formula <- mean ~ gdp * cat
    contrastsArg = list(cat = diag(3))
    spec <- DLM(trend = NULL,
                covariates = Covariates(formula = formula,
                    data = data,
                    contrastsArg = contrastsArg))
    expect_is(spec, "SpecDLMNoTrendNormCovNoSeason")
    beta <- rnorm(20)
    strucZeroArray <- Counts(array(1L,
                                   dim = c(2, 10),
                                   dimnames = list(reg = c("a", "b"),
                                                   time = 2001:2010)),
                             dimscales = c(time = "Points"))
    metadata <- new("MetaData",
                    nms = c("time", "reg"),
                    dimtypes = c("time", "state"),
                    DimScales = list(new("Points", dimvalues = 2001:2010),
                        new("Categories", dimvalues = c("a", "b"))))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = 200,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMNoTrendNormCovNoSeason")
})

test_that("generator function and initialPrior work with DLMWithTrendNormCovNoSeason", {
    set.seed(100)
    initialPrior <- demest:::initialPrior
    data <- data.frame(time = rep(2001:2010, times = 2),
                       reg = rep(c("a", "b"), each = 10),
                       gdp = rnorm(20),
                       cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
    formula <- mean ~ gdp * cat
    contrastsArg = list(cat = diag(3))
    spec <- DLM(covariates = Covariates(formula = formula,
                    data = data,
                    contrastsArg = contrastsArg))
    expect_is(spec, "SpecDLMWithTrendNormCovNoSeason")
    beta <- rnorm(20)
    strucZeroArray <- Counts(array(1L,
                                   dim = c(2, 10),
                                   dimnames = list(reg = c("a", "b"),
                                                   time = 2001:2010)),
                             dimscales = c(time = "Points"))
    metadata <- new("MetaData",
                    nms = c("time", "reg"),
                    dimtypes = c("time", "state"),
                    DimScales = list(new("Points", dimvalues = 2001:2010),
                        new("Categories", dimvalues = c("a", "b"))))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = 200,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMWithTrendNormCovNoSeason")
})

test_that("generator function and initialPrior work with DLMNoTrendNormCovWithSeason", {
    set.seed(100)
    initialPrior <- demest:::initialPrior
    data <- data.frame(time = rep(2001:2010, times = 2),
                       reg = rep(c("a", "b"), each = 10),
                       gdp = rnorm(20),
                       cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
    formula <- mean ~ gdp * cat
    contrastsArg = list(cat = diag(3))
    spec <- DLM(trend = NULL,
                season = Season(n = 2),
                covariates = Covariates(formula = formula,
                    data = data,
                    contrastsArg = contrastsArg))
    expect_is(spec, "SpecDLMNoTrendNormCovWithSeason")
    beta <- rnorm(20)
    strucZeroArray <- Counts(array(1L,
                                   dim = c(2, 10),
                                   dimnames = list(reg = c("a", "b"),
                                                   time = 2001:2010)),
                             dimscales = c(time = "Points"))
    metadata <- new("MetaData",
                    nms = c("time", "reg"),
                    dimtypes = c("time", "state"),
                    DimScales = list(new("Points", dimvalues = 2001:2010),
                        new("Categories", dimvalues = c("a", "b"))))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = 200,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMNoTrendNormCovWithSeason")
})

test_that("generator function and initialPrior work with DLMWithTrendNormCovWithSeason", {
    set.seed(100)
    initialPrior <- demest:::initialPrior
    data <- data.frame(time = rep(2001:2010, times = 2),
                       reg = rep(c("a", "b"), each = 10),
                       gdp = rnorm(20),
                       cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
    formula <- mean ~ gdp * cat
    contrastsArg = list(cat = diag(3))
    spec <- DLM(season = Season(n = 2),
                covariates = Covariates(formula = formula,
                    data = data,
                    contrastsArg = contrastsArg))
    expect_is(spec, "SpecDLMWithTrendNormCovWithSeason")
    beta <- rnorm(20)
    strucZeroArray <- Counts(array(1L,
                                   dim = c(2, 10),
                                   dimnames = list(reg = c("a", "b"),
                                                   time = 2001:2010)),
                             dimscales = c(time = "Points"))
    metadata <- new("MetaData",
                    nms = c("time", "reg"),
                    dimtypes = c("time", "state"),
                    DimScales = list(new("Points", dimvalues = 2001:2010),
                        new("Categories", dimvalues = c("a", "b"))))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = 200,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMWithTrendNormCovWithSeason")
})

test_that("generator function and initialPrior work with DLMNoTrendRobustZeroNoSeason", {
    set.seed(100)
    initialPrior <- demest:::initialPrior
    spec <- DLM(trend = NULL,
                error = Error(robust = TRUE))
    expect_is(spec, "SpecDLMNoTrendRobustZeroNoSeason")
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(2, 10),
                                   dimnames = list(reg = c("a", "b"),
                                                   time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMNoTrendRobustZeroNoSeason")
})

test_that("generator function and initialPrior work with DLMWithTrendRobustZeroNoSeason", {
    set.seed(100)
    initialPrior <- demest:::initialPrior
    spec <- DLM(error = Error(robust = TRUE))
    expect_is(spec, "SpecDLMWithTrendRobustZeroNoSeason")
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = c(2, 10),
                                   dimnames = list(reg = c("a", "b"),
                                                   time = 2001:2010)),
                             dimscales = c(time = "Points"))
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMWithTrendRobustZeroNoSeason")
})

test_that("generator function and initialPrior work with DLMNoTrendRobustZeroWithSeason", {
    set.seed(100)
    initialPrior <- demest:::initialPrior
    spec <- DLM(trend = NULL,
                season = Season(n = 4),
                error = Error(robust = TRUE))
    expect_is(spec, "SpecDLMNoTrendRobustZeroWithSeason")
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(2, 10),
                                   dimnames = list(reg = c("a", "b"),
                                                   time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMNoTrendRobustZeroWithSeason")
})

test_that("generator function and initialPrior work with DLMWithTrendRobustZeroWithSeason", {
    set.seed(100)
    initialPrior <- demest:::initialPrior
    spec <- DLM(season = Season(n = 4),
                error = Error(robust = TRUE))
    expect_is(spec, "SpecDLMWithTrendRobustZeroWithSeason")
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(2, 10),
                                   dimnames = list(reg = c("a", "b"),
                                                   time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMWithTrendRobustZeroWithSeason")
})

test_that("generator function and initialPrior work with DLMNoTrendRobustZeroNoSeason", {
    set.seed(100)
    initialPrior <- demest:::initialPrior
    data <- data.frame(time = rep(2001:2010, times = 2),
                       reg = rep(c("a", "b"), each = 10),
                       gdp = rnorm(20),
                       cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
    formula <- mean ~ gdp * cat
    contrastsArg = list(cat = diag(3))
    spec <- DLM(trend = NULL,
                covariates = Covariates(formula = formula,
                    data = data,
                    contrastsArg = contrastsArg),
                error = Error(robust = TRUE))
    expect_is(spec, "SpecDLMNoTrendRobustCovNoSeason")
    beta <- rnorm(20)
    strucZeroArray <- Counts(array(1L,
                                   dim = c(2, 10),
                                   dimnames = list(reg = c("a", "b"),
                                                   time = 2001:2010)),
                             dimscales = c(time = "Points"))
    metadata <- new("MetaData",
                    nms = c("time", "reg"),
                    dimtypes = c("time", "state"),
                    DimScales = list(new("Points", dimvalues = 2001:2010),
                        new("Categories", dimvalues = c("a", "b"))))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = 200,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMNoTrendRobustCovNoSeason")
})

test_that("generator function and initialPrior work with DLMWithTrendRobustCovNoSeason", {
    set.seed(100)
    initialPrior <- demest:::initialPrior
    data <- data.frame(time = rep(2001:2010, times = 2),
                       reg = rep(c("a", "b"), each = 10),
                       gdp = rnorm(20),
                       cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
    formula <- mean ~ gdp * cat
    contrastsArg = list(cat = diag(3))
    spec <- DLM(covariates = Covariates(formula = formula,
                    data = data,
                    contrastsArg = contrastsArg),
                error = Error(robust = TRUE))
    expect_is(spec, "SpecDLMWithTrendRobustCovNoSeason")
    beta <- rnorm(20)
    metadata <- new("MetaData",
                    nms = c("time", "reg"),
                    dimtypes = c("time", "state"),
                    DimScales = list(new("Points", dimvalues = 2001:2010),
                        new("Categories", dimvalues = c("a", "b"))))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(2, 10),
                                   dimnames = list(reg = c("a", "b"),
                                                   time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = 200,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMWithTrendRobustCovNoSeason")
})

test_that("generator function and initialPrior work with DLMNoTrendRobustCovWithSeason", {
    set.seed(100)
    initialPrior <- demest:::initialPrior
    data <- data.frame(time = rep(2001:2010, times = 2),
                       reg = rep(c("a", "b"), each = 10),
                       gdp = rnorm(20),
                       cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
    formula <- mean ~ gdp * cat
    contrastsArg = list(cat = diag(3))
    spec <- DLM(trend = NULL,
                season = Season(n = 2),
                covariates = Covariates(formula = formula,
                    data = data,
                    contrastsArg = contrastsArg),
                error = Error(robust = TRUE))
    expect_is(spec, "SpecDLMNoTrendRobustCovWithSeason")
    beta <- rnorm(20)
    strucZeroArray <- Counts(array(1L,
                                   dim = c(2, 10),
                                   dimnames = list(reg = c("a", "b"),
                                                   time = 2001:2010)),
                             dimscales = c(time = "Points"))
    metadata <- new("MetaData",
                    nms = c("time", "reg"),
                    dimtypes = c("time", "state"),
                    DimScales = list(new("Points", dimvalues = 2001:2010),
                        new("Categories", dimvalues = c("a", "b"))))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = 200,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMNoTrendRobustCovWithSeason")
    expect_identical(prior@isSaturated, new("LogicalFlag", FALSE))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = 200,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMNoTrendRobustCovWithSeason")
    expect_identical(prior@isSaturated, new("LogicalFlag", FALSE))
})

test_that("generator function and initialPrior work with DLMWithTrendRobustCovWithSeason", {
    set.seed(100)
    initialPrior <- demest:::initialPrior
    data <- data.frame(time = rep(2001:2010, times = 2),
                       reg = rep(c("a", "b"), each = 10),
                       gdp = rnorm(20),
                       cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
    formula <- mean ~ gdp * cat
    contrastsArg = list(cat = diag(3))
    spec <- DLM(season = Season(n = 2),
                covariates = Covariates(formula = formula,
                    data = data,
                    contrastsArg = contrastsArg),
                error = Error(robust = TRUE))
    expect_is(spec, "SpecDLMWithTrendRobustCovWithSeason")
    beta <- rnorm(20)
    strucZeroArray <- Counts(array(1L,
                                   dim = c(2, 10),
                                   dimnames = list(reg = c("a", "b"),
                                                   time = 2001:2010)),
                             dimscales = c(time = "Points"))
    metadata <- new("MetaData",
                    nms = c("time", "reg"),
                    dimtypes = c("time", "state"),
                    DimScales = list(new("Points", dimvalues = 2001:2010),
                        new("Categories", dimvalues = c("a", "b"))))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = 200,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMWithTrendRobustCovWithSeason")
})

test_that("generator function and initialPrior work with KnownCertain", {
    initialPrior <- demest:::initialPrior
    mean <- ValuesOne(1:5, labels = 0:4, name = "age", dimscale = "Intervals")
    spec <- Known(mean)
    expect_is(spec, "SpecKnownCertain")
    beta <- rnorm(4)
    strucZeroArray <- Counts(array(1L,
                                   dim = 4,
                                   dimnames = list(age = 0:3)),
                             dimscales = c(age = "Intervals"))
    metadata <- new("MetaData",
                    nms = "age",
                    dimtypes = "age",
                    DimScales = list(new("Intervals", dimvalues = 0:4)))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = 50,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "KnownCertain")
    expect_identical(prior@J, new("Length", 4))
    ## 'mean' is "Values"
    expect_error(Known(mean = 1:4),
                 "'mean' has class \"integer\"")
    ## 'mean' has no missing values
    mean <- ValuesOne(c(NA, 2:5), labels = 0:4, name = "age", dimscale = "Intervals")
    expect_error(Known(mean),
                 "'mean' has missing values")
    ## mean has length of 2 or more
    mean <- ValuesOne(1, labels = "0-4", name = "age")
    expect_error(Known(mean),
                 "'mean' has length 1")
    ## mean compatible
    mean <- ValuesOne(1:5, labels = 0:4, name = "age", dimscale = "Intervals")
    spec <- Known(mean)
    beta <- rnorm(4)
    metadata <- new("MetaData",
                    nms = "age",
                    dimtypes = "age",
                    DimScales = list(new("Intervals", dimvalues = 1:6)))
    expect_error(initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = 50,
                              isSaturated = FALSE,
                              strucZeroArray = strucZeroArray),
                 "metadata for 'Known' prior for 'age' not compatible with metadata for 'y'")
    ## includes structural zeros
    mean <- ValuesOne(c(0L, 2:5), labels = 0:4, name = "age", dimscale = "Intervals")
    spec <- Known(mean)
    expect_is(spec, "SpecKnownCertain")
    beta <- rnorm(4)
    strucZeroArray <- Counts(array(c(0L, 1L, 1L, 1L),
                                   dim = 4,
                                   dimnames = list(age = 0:3)),
                             dimscales = c(age = "Intervals"))
    metadata <- new("MetaData",
                    nms = "age",
                    dimtypes = "age",
                    DimScales = list(new("Intervals", dimvalues = 0:4)))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = 50,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_true(validObject(prior))
    expect_identical(prior@allStrucZero, c(TRUE, FALSE, FALSE, FALSE))
    mean.wrong <- ValuesOne(1:5, labels = 0:4, name = "age", dimscale = "Intervals")
    spec.wrong <- Known(mean.wrong)
    expect_error(initialPrior(spec.wrong,
                              beta = beta,
                              metadata = metadata,
                              sY = 50,
                              isSaturated = FALSE,
                              strucZeroArray = strucZeroArray),
                 "all cells contributing to element '\\[0\\]' of \"Known\" prior for 'age' are structural zeros, but element '\\[0\\]' does not equal 0")
})

test_that("generator function and initialPrior work with KnownUncertain", {
    initialPrior <- demest:::initialPrior
    ## 'sd' is Values
    mean <- ValuesOne(1:5, labels = 0:4, name = "age", dimscale = "Intervals")
    sd <- ValuesOne(1:5, labels = 0:4, name = "age", dimscale = "Intervals")
    spec <- Known(mean = mean,
                  sd = sd)
    expect_is(spec, "SpecKnownUncertain")
    beta <- rnorm(4)
    metadata <- new("MetaData",
                    nms = "age",
                    dimtypes = "age",
                    DimScales = list(new("Intervals", dimvalues = 0:4)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 4,
                                   dimnames = list(age = 0:3)),
                             dimscales = c(age = "Intervals"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = 50,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "KnownUncertain")
    expect_identical(prior@J, new("Length", 4))
    ## 'sd' is numeric
    mean <- ValuesOne(1:5, labels = 0:4, name = "age", dimscale = "Intervals")
    sd <- 0.1
    spec <- Known(mean = mean,
                  sd = sd)
    expect_is(spec, "SpecKnownUncertain")
    beta <- rnorm(4)
    metadata <- new("MetaData",
                    nms = "age",
                    dimtypes = "age",
                    DimScales = list(new("Intervals", dimvalues = 0:4)))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = 50,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "KnownUncertain")
    expect_identical(prior@J, new("Length", 4))
    ## 'sd' is compatible with 'mean'
    mean <- ValuesOne(1:5, labels = 0:4, name = "age", dimscale = "Intervals")
    sd <- ValuesOne(1:5, labels = 1:5, name = "age", dimscale = "Intervals")
    expect_error(Known(mean = mean, sd = sd),
                 "'sd' and 'mean' not compatible")
    ## 'sd' has no missing values
    mean <- ValuesOne(1:5, labels = 0:4, name = "age", dimscale = "Intervals")
    sd <- ValuesOne(c(NA, 2:5), labels = 0:4, name = "age", dimscale = "Intervals")
    expect_error(Known(mean = mean, sd = sd),
                 "'sd' has missing values")
    ## 'sd' has no negative values
    mean <- ValuesOne(1:5, labels = 0:4, name = "age", dimscale = "Intervals")
    sd <- ValuesOne(c(-1, 2:5), labels = 0:4, name = "age", dimscale = "Intervals")
    expect_error(Known(mean = mean, sd = sd),
                 "'sd' has negative values")
    ## 'sd' has length 1
    mean <- ValuesOne(1:5, labels = 0:4, name = "age", dimscale = "Intervals")
    expect_error(Known(mean = mean, sd = 1:2),
                 "'sd' is numeric but does not have length 1")
    ## 'sd' is not missing
    mean <- ValuesOne(1:5, labels = 0:4, name = "age", dimscale = "Intervals")
    expect_error(Known(mean = mean, sd = as.numeric(NA)),
                 "'sd' is missing")
    ## 'sd' is non-negative
    mean <- ValuesOne(1:5, labels = 0:4, name = "age", dimscale = "Intervals")
    expect_error(Known(mean = mean, sd = -0.1),
                 "'sd' is negative")
    ## sd is Values or numeric
    mean <- ValuesOne(1:5, labels = 0:4, name = "age", dimscale = "Intervals")
    expect_error(Known(mean = mean, sd = "1"),
                 "'sd' has class \"character\"")
    ## includes structural zeros
    mean <- ValuesOne(c(0L, 2:5), labels = 0:4, name = "age", dimscale = "Intervals")
    sd <- ValuesOne(0:4, labels = 0:4, name = "age", dimscale = "Intervals")
    spec <- Known(mean = mean,
                  sd = sd)
    expect_is(spec, "SpecKnownUncertain")
    beta <- rnorm(4)
    strucZeroArray <- Counts(array(c(0L, 1L, 1L, 1L),
                                   dim = 4,
                                   dimnames = list(age = 0:3)),
                             dimscales = c(age = "Intervals"))
    metadata <- new("MetaData",
                    nms = "age",
                    dimtypes = "age",
                    DimScales = list(new("Intervals", dimvalues = 0:4)))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = 50,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_true(validObject(prior))
    expect_identical(prior@allStrucZero, c(TRUE, FALSE, FALSE, FALSE))
    sd.wrong <- ValuesOne(1:5, labels = 0:4, name = "age", dimscale = "Intervals")
    spec.wrong <- Known(mean = mean,
                        sd = sd.wrong)
    expect_error(initialPrior(spec.wrong,
                              beta = beta,
                              metadata = metadata,
                              sY = 50,
                              isSaturated = FALSE,
                              strucZeroArray = strucZeroArray),
                 "all cells contributing to element '\\[0\\]' of \"Known\" prior for 'age' are structural zeros, but element '\\[0\\]' does not have standard deviation 0")
})



## Mix

test_that("generator function and initialPrior work with MixNormZero", {
    set.seed(100)
    initialPrior <- demest:::initialPrior
    beta <- rnorm(200)
    metadata <- new("MetaData",
                    nms = c("time", "reg", "age"),
                    dimtypes = c("time", "state", "age"),
                    DimScales = list(new("Points", dimvalues = 2001:2010),
                                     new("Categories", dimvalues = c("a", "b")),
                                     new("Intervals", dimvalues = as.numeric(0:10))))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(10, 2, 10),
                                   dimnames = list(age = 0:9,
                                                   reg = c("a", "b"),
                                                   time = 2001:2010)),
                             dimscales = c(time = "Points"))
    ## all defaults
    spec <- Mix()
    expect_is(spec, "SpecMixNormZero")
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "MixNormZero")
    ## nondefault
    spec <- Mix(along = "age",
                components = Components(scale = HalfT(df = 4, scale = 0.5)),
                weights = Weights(mean = 0.5,
                                  sd = 0.5,
                                  scale1 = HalfT(scale = 0.4),
                                  scale2 = HalfT(mult = 0.5)),
                error = Error(scale = HalfT(df = 6)),
                maxComponents = 11)
    beta <- rnorm(200)
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_identical(prior@iAlong, 3L)
    expect_identical(prior@AVectorsMix, new("Scale", 0.5))
    expect_identical(prior@nuVectorsMix, new("DegreesFreedom", 4))
    expect_identical(prior@priorMeanLevelComponentWeightMix, new("Parameter", 0.5))
    expect_identical(prior@priorSDLevelComponentWeightMix, new("Scale", 0.5))
    expect_identical(prior@AComponentWeightMix, new("Scale", 0.4))
    expect_identical(prior@ALevelComponentWeightMix, new("Scale", 0.125))
    expect_identical(prior@nuTau, new("DegreesFreedom", 6))
    expect_identical(prior@tolerance, new("Parameter", 1e-5))
    ## has structural zeros
    strucZeroArray.wrong <- Counts(array(c(1L, 0L),
                                   dim = c(10, 2, 10),
                                   dimnames = list(age = 0:9,
                                                   reg = c("a", "b"),
                                                   time = 2001:2010)),
                             dimscales = c(time = "Points"))
    expect_error(initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray.wrong),
                 "'time\\:reg\\:age' has elements where all contributing cells are structural zeros; priors with class \"Mix\" cannot be used in such cases")
})


## Zero

test_that("generator function and initialPrior work with Zero", {
    initialPrior <- demest:::initialPrior
    spec <- Zero()
    expect_is(spec, "SpecZero")
    beta <- rnorm(4)
    metadata <- new("MetaData",
                    nms = "age",
                    dimtypes = "age",
                    DimScales = list(new("Intervals", dimvalues = 0:4)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 4L,
                                   dimnames = list(age = 0:3)))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = 50,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "Zero")
})



## initialPriorPredict ###############################################################

test_that("initialPriorPredict works with ExchFixed", {
    set.seed(100)
    initialPriorPredict <- demest:::initialPriorPredict
    initialPrior <- demest:::initialPrior
    spec <- ExchFixed()
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "ExchFixed")
    metadata.new <- new("MetaData",
                        nms = "region",
                        dimtypes = "state",
                        DimScales = list(new("Categories", dimvalues = letters[11:15])))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5L,
                                   dimnames = list(region = letters[11:15])))
    beta.new <- rnorm(5)
    ans.obtained <- initialPriorPredict(prior,
                                        data = NULL,
                                        metadata = metadata.new,
                                        name = "region",
                                        along = "region",
                                        strucZeroArray = strucZeroArray)
    ans.expected <- initialPrior(spec,
                                 beta = beta.new,
                                 metadata = metadata.new,
                                 sY = NULL,
                                 multScale = 1,
                                 isSaturated = FALSE,
                                 strucZeroArray = strucZeroArray)
    ans.expected@tau <- prior@tau
    expect_identical(ans.obtained, ans.expected)
})

test_that("initialPriorPredict workw with ExchNormZero", {
    set.seed(100)
    initialPriorPredict <- demest:::initialPriorPredict
    initialPrior <- demest:::initialPrior
    spec <- Exch()
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "ExchNormZero")
    metadata.new <- new("MetaData",
                        nms = "region",
                        dimtypes = "state",
                        DimScales = list(new("Categories", dimvalues = letters[11:15])))
    beta.new <- rnorm(5)
    strucZeroArray <- Counts(array(1L,
                                   dim = 5L,
                                   dimnames = list(region = letters[11:15])))
    ans.obtained <- initialPriorPredict(prior,
                                        data = NULL,
                                        metadata = metadata.new,
                                        name = "region",
                                        along = "region",
                                        strucZeroArray = strucZeroArray)
    ans.expected <- initialPrior(spec,
                                 beta = beta.new,
                                 metadata = metadata.new,
                                 sY = NULL,
                                 multScale = 1,
                                 isSaturated = FALSE,
                                 strucZeroArray = strucZeroArray)
    ans.expected@tau <- prior@tau
    expect_identical(ans.obtained, ans.expected)
})

test_that("initialPriorPredict works with ExchRobustZero", {
    set.seed(100)
    initialPriorPredict <- demest:::initialPriorPredict
    initialPrior <- demest:::initialPrior
    spec <- Exch(error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "ExchRobustZero")
    metadata.new <- new("MetaData",
                        nms = "region",
                        dimtypes = "state",
                        DimScales = list(new("Categories", dimvalues = letters[11:15])))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5L,
                                   dimnames = list(region = letters[11:15])))
    beta.new <- rnorm(5)
    prior.new <- initialPriorPredict(prior,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "region",
                                     along = "region",
                                     strucZeroArray = strucZeroArray)
    expect_identical(prior.new@J@.Data, 5L)
    expect_identical(prior.new@nuTau, prior@nuTau)
    expect_identical(prior.new@ATau, prior@ATau)
    expect_identical(prior.new@tau, prior@tau)
    expect_identical(prior.new@tauMax, new("Scale", qhalft(0.999, 7, 1)))
    expect_identical(prior.new@nuBeta, prior@nuBeta)
})

test_that("initialPriorPredict works with ExchNormCov", {
    set.seed(100)
    initialPriorPredict <- demest:::initialPriorPredict
    initialPrior <- demest:::initialPrior
    data <- data.frame(region = rep(letters[1:10], times = 2),
                       sex = rep(c("f", "m"), each = 10),
                       income = rnorm(20),
                       cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
    formula <- mean ~ income * cat
    contrastsArg = list(cat = diag(3))
    spec <- Exch(covariates = Covariates(formula = formula,
                     data = data, contrastsArg = contrastsArg))
    expect_is(spec, "SpecExchNormCov")
    beta <- rnorm(20)
    strucZeroArray <- Counts(array(1L,
                                   dim = c(10L, 2L),
                                   dimnames = list(region = letters[1:10],
                                                   sex = c("f", "m"))))
    metadata <- new("MetaData",
                    nms = c("region", "sex"),
                    dimtypes = c("state", "sex"),
                    DimScales = list(new("Categories", dimvalues = letters[1:10]),
                        new("Sexes", dimvalues = c("f", "m"))))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "ExchNormCov")
    metadata.new <- new("MetaData",
                        nms = c("region", "sex"),
                        dimtypes = c("state", "sex"),
                        DimScales = list(new("Categories", dimvalues = letters[11:15]),
                            new("Sexes", dimvalues = c("f", "m"))))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(5L, 2L),
                                   dimnames = list(region = letters[11:15],
                                                   sex = c("f", "m"))))
    data <- data.frame(region = rep(letters[11:15], times = 2),
                             sex = rep(c("f", "m"), each = 5),
                             income = rnorm(10),
                             cat = sample(c("x" ,"y", "z"), size = 10, replace = TRUE))
    beta.new <- rnorm(10)
    prior.new <- initialPriorPredict(prior,
                                     data = data,
                                     metadata = metadata.new,
                                     name = "region:sex",
                                     along = "region",
                                     strucZeroArray = strucZeroArray)
    expect_identical(prior.new@J@.Data, 10L)
    expect_identical(prior.new@nuTau, prior@nuTau)
    expect_identical(prior.new@ATau, prior@ATau)
    expect_identical(prior.new@tau, prior@tau)
    expect_identical(prior.new@contrastsArg, prior@contrastsArg)
    expect_identical(prior.new@formula, prior@formula)
    expect_identical(prior.new@nuEtaCoef, prior@nuEtaCoef)
    expect_identical(prior.new@UEtaCoef, prior@UEtaCoef)
    expect_identical(prior.new@eta, prior@eta)
})

test_that("initialPriorPredict works with ExchRobustCov", {
    set.seed(100)
    initialPriorPredict <- demest:::initialPriorPredict
    initialPrior <- demest:::initialPrior
    data <- data.frame(region = rep(letters[1:10], times = 2),
                       sex = rep(c("f", "m"), each = 10),
                       income = rnorm(20),
                       cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
    formula <- mean ~ income * cat
    contrastsArg = list(cat = diag(3))
    spec <- Exch(covariates = Covariates(formula = formula,
                     data = data, contrastsArg = contrastsArg),
                 error = Error(robust = TRUE))
    expect_is(spec, "SpecExchRobustCov")
    beta <- rnorm(20)
    strucZeroArray <- Counts(array(1L,
                                   dim = c(10L, 2L),
                                   dimnames = list(region = letters[1:10],
                                                   sex = c("f", "m"))))
    metadata <- new("MetaData",
                    nms = c("region", "sex"),
                    dimtypes = c("state", "sex"),
                    DimScales = list(new("Categories", dimvalues = letters[1:10]),
                        new("Sexes", dimvalues = c("f", "m"))))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "ExchRobustCov")
    metadata.new <- new("MetaData",
                        nms = c("region", "sex"),
                        dimtypes = c("state", "sex"),
                        DimScales = list(new("Categories", dimvalues = letters[11:15]),
                            new("Sexes", dimvalues = c("f", "m"))))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(5L, 2L),
                                   dimnames = list(region = letters[11:15],
                                                   sex = c("f", "m"))))
    data <- data.frame(region = rep(letters[11:15], times = 2),
                             sex = rep(c("f", "m"), each = 5),
                             income = rnorm(10),
                             cat = sample(c("x" ,"y", "z"), size = 10, replace = TRUE))
    beta.new <- rnorm(10)
    prior.new <- initialPriorPredict(prior,
                                     data = data,
                                     metadata = metadata.new,
                                     name = "region:sex",
                                     along = "region",
                                     strucZeroArray = strucZeroArray)
    expect_identical(prior.new@J@.Data, 10L)
    expect_identical(prior.new@nuTau, prior@nuTau)
    expect_identical(prior.new@ATau, prior@ATau)
    expect_identical(prior.new@tau, prior@tau)
    expect_identical(prior.new@contrastsArg, prior@contrastsArg)
    expect_identical(prior.new@formula, prior@formula)
    expect_identical(prior.new@nuEtaCoef, prior@nuEtaCoef)
    expect_identical(prior.new@UEtaCoef, prior@UEtaCoef)
    expect_identical(prior.new@eta, prior@eta)
    expect_identical(prior.new@nuBeta, prior@nuBeta)
})


## DLM - Norm, Zero

test_that("initialPriorPredict works with DLMNoTrendNormZeroNoSeason", {
    set.seed(100)
    initialPriorPredict <- demest:::initialPriorPredict
    initialPrior <- demest:::initialPrior
    spec <- DLM(trend = NULL)
    expect_is(spec, "SpecDLMNoTrendNormZeroNoSeason")
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMNoTrendNormZeroNoSeason")
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L,
                                     strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMNoTrendNormZeroNoSeason")
    expect_identical(prior.new@J@.Data, 5L)
    expect_identical(prior.new@K@.Data, 5L)
    expect_identical(prior.new@L@.Data, 1L)
    expect_identical(prior.new@nuTau, prior@nuTau)
    expect_identical(prior.new@ATau, prior@ATau)
    expect_identical(prior.new@tau, prior@tau)
    expect_identical(prior.new@CNoTrend[[1]], 0)
})

test_that("initialPriorPredict works with DLMWithTrendNormZeroNoSeason", {
    set.seed(100)
    initialPriorPredict <- demest:::initialPriorPredict
    initialPrior <- demest:::initialPrior
    spec <- DLM()
    expect_is(spec, "SpecDLMWithTrendNormZeroNoSeason")
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMWithTrendNormZeroNoSeason")
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L,
                                     strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMWithTrendNormZeroNoSeason")
    expect_identical(prior.new@J@.Data, 5L)
    expect_identical(prior.new@K@.Data, 5L)
    expect_identical(prior.new@L@.Data, 1L)
    expect_identical(prior.new@nuTau, prior@nuTau)
    expect_identical(prior.new@ATau, prior@ATau)
    expect_identical(prior.new@tau, prior@tau)
    expect_identical(prior.new@CWithTrend[[1]], matrix(0, nr = 2, nc = 2))
})

test_that("initialPriorPredict works with DLMNoTrendNormZeroWithSeason", {
    set.seed(100)
    initialPriorPredict <- demest:::initialPriorPredict
    initialPrior <- demest:::initialPrior
    spec <- DLM(trend = NULL,
                season = Season(n = 2))
    expect_is(spec, "SpecDLMNoTrendNormZeroWithSeason")
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMNoTrendNormZeroWithSeason")
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L,
                                     strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMNoTrendNormZeroWithSeason")
    expect_identical(prior.new@J@.Data, 5L)
    expect_identical(prior.new@K@.Data, 5L)
    expect_identical(prior.new@L@.Data, 1L)
    expect_identical(prior.new@nuTau, prior@nuTau)
    expect_identical(prior.new@ATau, prior@ATau)
    expect_identical(length(prior.new@s@.Data), 6L)
    expect_identical(prior.new@tau, prior@tau)
    expect_identical(prior.new@CNoTrend[[1]], 0)
    expect_identical(prior.new@CSeason[[1]], rep(0, 2))
})

test_that("initialPriorPredict works with DLMWithTrendNormZeroWithSeason", {
    set.seed(100)
    initialPriorPredict <- demest:::initialPriorPredict
    initialPrior <- demest:::initialPrior
    spec <- DLM(season = Season(n = 4))
    expect_is(spec, "SpecDLMWithTrendNormZeroWithSeason")
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMWithTrendNormZeroWithSeason")
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L,
                                     strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMWithTrendNormZeroWithSeason")
    expect_identical(prior.new@J@.Data, 5L)
    expect_identical(prior.new@K@.Data, 5L)
    expect_identical(prior.new@L@.Data, 1L)
    expect_identical(prior.new@nuTau, prior@nuTau)
    expect_identical(prior.new@ATau, prior@ATau)
    expect_identical(prior.new@tau, prior@tau)
    expect_identical(prior.new@CSeason[[1]], rep(0, 4))
    expect_identical(prior.new@CWithTrend[[1]], matrix(0, nr = 2, nc = 2))
})


## DLM - Norm, Cov

test_that("initialPriorPredict works with DLMNoTrendNormCovNoSeason", {
    set.seed(100)
    initialPriorPredict <- demest:::initialPriorPredict
    initialPrior <- demest:::initialPrior
    data <- data.frame(time = 2001:2010,
                       income = rnorm(10))
    formula <- mean ~ income
    spec <- DLM(trend = NULL,
                covariates = Covariates(mean ~ income, data = data))
    expect_is(spec, "SpecDLMNoTrendNormCovNoSeason")
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMNoTrendNormCovNoSeason")
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L,
                                     strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMNoTrendNormCovNoSeason")
    expect_identical(prior.new@J@.Data, 5L)
    expect_identical(prior.new@K@.Data, 5L)
    expect_identical(prior.new@L@.Data, 1L)
    expect_identical(prior.new@nuTau, prior@nuTau)
    expect_identical(prior.new@ATau, prior@ATau)
    expect_identical(prior.new@tau, prior@tau)
    expect_identical(prior.new@CNoTrend[[1]], 0)
    expect_identical(dim(prior.new@Z), c(5L, 2L))
})

test_that("initialPriorPredict works with DLMWithTrendNormCovNoSeason", {
    set.seed(100)
    initialPriorPredict <- demest:::initialPriorPredict
    initialPrior <- demest:::initialPrior
    data <- data.frame(time = 2001:2010,
                       income = rnorm(10))
    formula <- mean ~ income
    spec <- DLM(covariates = Covariates(mean ~ income, data = data))
    expect_is(spec, "SpecDLMWithTrendNormCovNoSeason")
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMWithTrendNormCovNoSeason")
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L,
                                     strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMWithTrendNormCovNoSeason")
    expect_identical(prior.new@J@.Data, 5L)
    expect_identical(prior.new@K@.Data, 5L)
    expect_identical(prior.new@L@.Data, 1L)
    expect_identical(prior.new@nuTau, prior@nuTau)
    expect_identical(prior.new@ATau, prior@ATau)
    expect_identical(prior.new@tau, prior@tau)
    expect_identical(prior.new@CWithTrend[[1]], matrix(0, nr = 2, nc = 2))
    expect_identical(dim(prior.new@Z), c(5L, 2L))
})

test_that("initialPriorPredict works with DLMNoTrendNormCovWithSeason", {
    set.seed(100)
    initialPriorPredict <- demest:::initialPriorPredict
    initialPrior <- demest:::initialPrior
    data <- data.frame(time = 2001:2010,
                       income = rnorm(10))
    formula <- mean ~ income
    spec <- DLM(trend = NULL,
                season = Season(n = 2),
                covariates = Covariates(mean ~ income, data = data))
    expect_is(spec, "SpecDLMNoTrendNormCovWithSeason")
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMNoTrendNormCovWithSeason")
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L,
                                     strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMNoTrendNormCovWithSeason")
    expect_identical(prior.new@J@.Data, 5L)
    expect_identical(prior.new@K@.Data, 5L)
    expect_identical(prior.new@L@.Data, 1L)
    expect_identical(prior.new@nuTau, prior@nuTau)
    expect_identical(prior.new@ATau, prior@ATau)
    expect_identical(length(prior.new@s@.Data), 6L)
    expect_identical(prior.new@tau, prior@tau)
    expect_identical(prior.new@CNoTrend[[1]], 0)
    expect_identical(prior.new@CSeason[[1]], rep(0, 2))
    expect_identical(dim(prior.new@Z), c(5L, 2L))
})

test_that("initialPriorPredict works with DLMWithTrendNormCovWithSeason", {
    set.seed(100)
    initialPriorPredict <- demest:::initialPriorPredict
    initialPrior <- demest:::initialPrior
    data <- data.frame(time = 2001:2010,
                       income = rnorm(10))
    formula <- mean ~ income
    spec <- DLM(season = Season(n = 4),
                covariates = Covariates(mean ~ income, data = data))
    expect_is(spec, "SpecDLMWithTrendNormCovWithSeason")
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMWithTrendNormCovWithSeason")
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L,
                                     strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMWithTrendNormCovWithSeason")
    expect_identical(prior.new@J@.Data, 5L)
    expect_identical(prior.new@K@.Data, 5L)
    expect_identical(prior.new@L@.Data, 1L)
    expect_identical(prior.new@nuTau, prior@nuTau)
    expect_identical(prior.new@ATau, prior@ATau)
    expect_identical(prior.new@tau, prior@tau)
    expect_identical(prior.new@CSeason[[1]], rep(0, 4))
    expect_identical(prior.new@CWithTrend[[1]], matrix(0, nr = 2, nc = 2))
    expect_identical(dim(prior.new@Z), c(5L, 2L))
})


## DLM - Robust, Zero

test_that("initialPriorPredict works with DLMNoTrendRobustZeroNoSeason", {
    set.seed(100)
    initialPriorPredict <- demest:::initialPriorPredict
    initialPrior <- demest:::initialPrior
    spec <- DLM(trend = NULL,
                error = Error(robust = TRUE))
    expect_is(spec, "SpecDLMNoTrendRobustZeroNoSeason")
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMNoTrendRobustZeroNoSeason")
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L,
                                     strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMNoTrendRobustZeroNoSeason")
    expect_identical(prior.new@J@.Data, 5L)
    expect_identical(prior.new@K@.Data, 5L)
    expect_identical(prior.new@L@.Data, 1L)
    expect_identical(prior.new@nuTau, prior@nuTau)
    expect_identical(prior.new@ATau, prior@ATau)
    expect_identical(prior.new@tau, prior@tau)
    expect_identical(prior.new@CNoTrend[[1]], 0)
})

test_that("initialPriorPredict works with DLMWithTrendRobustZeroNoSeason", {
    set.seed(100)
    initialPriorPredict <- demest:::initialPriorPredict
    initialPrior <- demest:::initialPrior
    spec <- DLM(error = Error(robust = TRUE))
    expect_is(spec, "SpecDLMWithTrendRobustZeroNoSeason")
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMWithTrendRobustZeroNoSeason")
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    prior.new <- initialPriorPredict(prior = prior,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L,
                                     strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMWithTrendRobustZeroNoSeason")
    expect_identical(prior.new@J@.Data, 5L)
    expect_identical(prior.new@K@.Data, 5L)
    expect_identical(prior.new@L@.Data, 1L)
    expect_identical(prior.new@nuTau, prior@nuTau)
    expect_identical(prior.new@ATau, prior@ATau)
    expect_identical(prior.new@tau, prior@tau)
    expect_identical(prior.new@CWithTrend[[1]], matrix(0, nr = 2, nc = 2))
})

test_that("initialPriorPredict works with DLMNoTrendRobustZeroWithSeason", {
    set.seed(100)
    initialPriorPredict <- demest:::initialPriorPredict
    initialPrior <- demest:::initialPrior
    spec <- DLM(trend = NULL,
                season = Season(n = 2),
                error = Error(robust = TRUE))
    expect_is(spec, "SpecDLMNoTrendRobustZeroWithSeason")
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMNoTrendRobustZeroWithSeason")
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L,
                                     strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMNoTrendRobustZeroWithSeason")
    expect_identical(prior.new@J@.Data, 5L)
    expect_identical(prior.new@K@.Data, 5L)
    expect_identical(prior.new@L@.Data, 1L)
    expect_identical(prior.new@nuTau, prior@nuTau)
    expect_identical(prior.new@ATau, prior@ATau)
    expect_identical(length(prior.new@s@.Data), 6L)
    expect_identical(prior.new@tau, prior@tau)
    expect_identical(prior.new@CNoTrend[[1]], 0)
    expect_identical(prior.new@CSeason[[1]], rep(0, 2))
})

test_that("initialPriorPredict works with DLMWithTrendRobustZeroWithSeason", {
    set.seed(100)
    initialPriorPredict <- demest:::initialPriorPredict
    initialPrior <- demest:::initialPrior
    spec <- DLM(season = Season(n = 4),
                error = Error(robust = TRUE))
    expect_is(spec, "SpecDLMWithTrendRobustZeroWithSeason")
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMWithTrendRobustZeroWithSeason")
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L,
                                     strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMWithTrendRobustZeroWithSeason")
    expect_identical(prior.new@J@.Data, 5L)
    expect_identical(prior.new@K@.Data, 5L)
    expect_identical(prior.new@L@.Data, 1L)
    expect_identical(prior.new@nuTau, prior@nuTau)
    expect_identical(prior.new@ATau, prior@ATau)
    expect_identical(prior.new@tau, prior@tau)
    expect_identical(prior.new@CSeason[[1]], rep(0, 4))
    expect_identical(prior.new@CWithTrend[[1]], matrix(0, nr = 2, nc = 2))
})


## DLM - Robust, Cov

test_that("initialPriorPredict works with DLMNoTrendRobustCovNoSeason", {
    set.seed(100)
    initialPriorPredict <- demest:::initialPriorPredict
    initialPrior <- demest:::initialPrior
    data <- data.frame(time = 2001:2010,
                       income = rnorm(10))
    formula <- mean ~ income
    spec <- DLM(trend = NULL,
                covariates = Covariates(mean ~ income, data = data),
                error = Error(robust = TRUE))
    expect_is(spec, "SpecDLMNoTrendRobustCovNoSeason")
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMNoTrendRobustCovNoSeason")
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMNoTrendRobustCovNoSeason")
    expect_identical(prior.new@J@.Data, 5L)
    expect_identical(prior.new@K@.Data, 5L)
    expect_identical(prior.new@L@.Data, 1L)
    expect_identical(prior.new@nuTau, prior@nuTau)
    expect_identical(prior.new@ATau, prior@ATau)
    expect_identical(prior.new@tau, prior@tau)
    expect_identical(prior.new@CNoTrend[[1]], 0)
    expect_identical(dim(prior.new@Z), c(5L, 2L))
})

test_that("initialPriorPredict works with DLMWithTrendRobustCovNoSeason", {
    set.seed(100)
    initialPriorPredict <- demest:::initialPriorPredict
    initialPrior <- demest:::initialPrior
    data <- data.frame(time = 2001:2010,
                       income = rnorm(10))
    formula <- mean ~ income
    spec <- DLM(covariates = Covariates(mean ~ income, data = data),
                error = Error(robust = TRUE))
    expect_is(spec, "SpecDLMWithTrendRobustCovNoSeason")
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMWithTrendRobustCovNoSeason")
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMWithTrendRobustCovNoSeason")
    expect_identical(prior.new@J@.Data, 5L)
    expect_identical(prior.new@K@.Data, 5L)
    expect_identical(prior.new@L@.Data, 1L)
    expect_identical(prior.new@nuTau, prior@nuTau)
    expect_identical(prior.new@ATau, prior@ATau)
    expect_identical(prior.new@tau, prior@tau)
    expect_identical(prior.new@CWithTrend[[1]], matrix(0, nr = 2, nc = 2))
    expect_identical(dim(prior.new@Z), c(5L, 2L))
})

test_that("initialPriorPredict works with DLMNoTrendRobustCovWithSeason", {
    set.seed(100)
    initialPriorPredict <- demest:::initialPriorPredict
    initialPrior <- demest:::initialPrior
    data <- data.frame(time = 2001:2010,
                       income = rnorm(10))
    formula <- mean ~ income
    spec <- DLM(trend = NULL,
                season = Season(n = 2),
                covariates = Covariates(mean ~ income, data = data),
                error = Error(robust = TRUE))
    expect_is(spec, "SpecDLMNoTrendRobustCovWithSeason")
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMNoTrendRobustCovWithSeason")
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5)) 
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMNoTrendRobustCovWithSeason")
    expect_identical(prior.new@J@.Data, 5L)
    expect_identical(prior.new@K@.Data, 5L)
    expect_identical(prior.new@L@.Data, 1L)
    expect_identical(prior.new@nuTau, prior@nuTau)
    expect_identical(prior.new@ATau, prior@ATau)
    expect_identical(length(prior.new@s@.Data), 6L)
    expect_identical(prior.new@tau, prior@tau)
    expect_identical(prior.new@CNoTrend[[1]], 0)
    expect_identical(prior.new@CSeason[[1]], rep(0, 2))
    expect_identical(dim(prior.new@Z), c(5L, 2L))
})

test_that("initialPriorPredict works with DLMWithTrendRobustCovWithSeason", {
    set.seed(100)
    initialPriorPredict <- demest:::initialPriorPredict
    initialPrior <- demest:::initialPrior
    data <- data.frame(time = 2001:2010,
                       income = rnorm(10))
    formula <- mean ~ income
    spec <- DLM(season = Season(n = 4),
                covariates = Covariates(mean ~ income, data = data),
                error = Error(robust = TRUE))
    expect_is(spec, "SpecDLMWithTrendRobustCovWithSeason")
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMWithTrendRobustCovWithSeason")
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L, strucZeroArray = strucZeroArray)
    expect_is(prior.new, "DLMWithTrendRobustCovWithSeason")
    expect_identical(prior.new@J@.Data, 5L)
    expect_identical(prior.new@K@.Data, 5L)
    expect_identical(prior.new@L@.Data, 1L)
    expect_identical(prior.new@nuTau, prior@nuTau)
    expect_identical(prior.new@ATau, prior@ATau)
    expect_identical(prior.new@tau, prior@tau)
    expect_identical(prior.new@CSeason[[1]], rep(0, 4))
    expect_identical(prior.new@CWithTrend[[1]], matrix(0, nr = 2, nc = 2))
    expect_identical(dim(prior.new@Z), c(5L, 2L))
})



## Mix

test_that("initialPriorPredict works with MixNormZeroPredict", {
    set.seed(100)
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    beta <- rnorm(200)
    metadata <- new("MetaData",
                    nms = c("time", "reg", "age"),
                    dimtypes = c("time", "state", "age"),
                    DimScales = list(new("Points", dimvalues = 2001:2010),
                                     new("Categories", dimvalues = c("a", "b")),
                                     new("Intervals", dimvalues = as.numeric(0:10))))
    strucZeroArray <- Counts(array(1L,
                                   dim = dim(metadata),
                                   dimnames = dimnames(metadata)),
                             dimscales = c(time = "Points", age = "Intervals"))
    spec <- Mix()
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = c("time", "reg", "age"),
                        dimtypes = c("time", "state", "age"),
                        DimScales = list(new("Points", dimvalues = 2011:2030),
                                         new("Categories", dimvalues = c("a", "b")),
                                         new("Intervals", dimvalues = as.numeric(0:10))))
    strucZeroArray <- Counts(array(1L,
                                   dim = dim(metadata.new),
                                   dimnames = dimnames(metadata.new)),
                             dimscales = c(time = "Points", age = "Intervals"))
    prior.pred <- initialPriorPredict(prior,
                                      metadata = metadata.new,
                                      name = "time:reg:age",
                                      along = 1L,
                                      strucZeroArray = strucZeroArray)
    expect_is(prior.pred, "MixNormZeroPredict")
})


## Known

test_that("initialPriorPredict works with KnownCertain", {
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    mean <- ValuesOne(1:10, labels = 0:9, name = "time", dimscale = "Intervals")
    spec <- Known(mean)
    expect_is(spec, "SpecKnownCertain")
    beta <- rnorm(5)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Intervals", dimvalues = 0:5)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 0:4)),
                             dimscales = c(time = "Intervals"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Intervals", dimvalues = 5:10)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 5:9)),
                             dimscales = c(time = "Intervals"))
    prior.pred <- initialPriorPredict(prior,
                                      metadata = metadata.new,
                                      name = "time",
                                      along = 1L,
                                      data = NULL,
                                      strucZeroArray = strucZeroArray)
    expect_is(prior.pred, "KnownCertain")
    metadata.wrong <- new("MetaData",
                          nms = "time",
                          dimtypes = "time",
                          DimScales = list(new("Intervals", dimvalues = 5:11)))
    expect_error(initialPriorPredict(prior,
                                     metadata = metadata.wrong,
                                     name = "time",
                                     along = 1L,
                                     data = NULL),
                 "metadata for 'Known' prior for 'time' not compatible with metadata for 'y'")
})

test_that("initialPriorPredict works with KnownUncertain", {
    initialPrior <- demest:::initialPrior
    initialPriorPredict <- demest:::initialPriorPredict
    mean <- ValuesOne(1:10, labels = 0:9, name = "time", dimscale = "Intervals")
    spec <- Known(mean, sd = 1)
    expect_is(spec, "SpecKnownUncertain")
    beta <- rnorm(5)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Intervals", dimvalues = 0:5)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 0:4)),
                             dimscales = c(time = "Intervals"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Intervals", dimvalues = 5:10)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 5:9)),
                             dimscales = c(time = "Intervals"))
    prior.pred <- initialPriorPredict(prior,
                                      metadata = metadata.new,
                                      name = "time",
                                      along = 1L,
                                      data = NULL,
                                      strucZeroArray = strucZeroArray)
    expect_is(prior.pred, "KnownUncertain")
    metadata.wrong <- new("MetaData",
                          nms = "time",
                          dimtypes = "time",
                          DimScales = list(new("Intervals", dimvalues = 5:11)))
    expect_error(initialPriorPredict(prior,
                                     metadata = metadata.wrong,
                                     name = "time",
                                     along = 1L,
                                     data = NULL),
                 "metadata for 'Known' prior for 'time' not compatible with metadata for 'y'")
})




