
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = NULL,
                          sY = 50)
    expect_is(prior, "ExchFixed")
    expect_identical(prior@J, new("Length", 1L))
    expect_identical(prior@tau, new("Scale", 500))
    ## length 2
    spec <- ExchFixed()
    expect_is(spec, "SpecExchFixed")
    beta <- rnorm(2)
    metadata <- new("MetaData",
                    nms = "sex",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = c("f", "m"))))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
    expect_is(prior, "ExchFixed")
    expect_identical(prior@J, new("Length", 2L))
    expect_identical(prior@tau, new("Scale", 1.0))
    ## no metadata but J > 1
    spec <- ExchFixed()
    expect_is(spec, "SpecExchFixed")
    beta <- rnorm(2)
    expect_error(initialPrior(spec,
                              beta = beta,
                              metadata = NULL,
                              sY = 50),
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
                              sY = NULL),
                 "'metadata' is not NULL but 'J' is less than or equal to 1")
})

test_that("generator function and initialPrior work with ExchNormZero", {
    set.seed(100)
    initialPrior <- demest:::initialPrior
    ## main effect; sY is NULL
    spec <- Exch()
    expect_is(spec, "SpecExchNormZero")
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
    expect_is(prior, "ExchNormZero")
    expect_identical(prior@J, new("Length", 10L))
    expect_identical(prior@ATau, new("Scale", 1.0))
    expect_identical(prior@nuTau, new("DegreesFreedom", 7.0))
    expect_identical(prior@tauMax, new("Scale", qhalft(p = 0.999, df = 7, scale = 1)))
    ## interaction; sY is 1000
    spec <- Exch()
    expect_is(spec, "SpecExchNormZero")
    beta <- rnorm(20, mean = 200)
    metadata <- new("MetaData",
                    nms = c("region", "sex"),
                    dimtypes = c("state", "state"),
                    DimScales = list(new("Categories", dimvalues = letters[1:10]),
                        new("Categories", dimvalues = c("f", "m"))))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = 1000,
                          multScale = 1)
    expect_is(prior, "ExchNormZero")
    expect_identical(prior@J, new("Length", 20L))
    expect_identical(prior@ATau, new("Scale", 500))
    expect_identical(prior@nuTau, new("DegreesFreedom", 7.0))
    expect_identical(prior@tauMax, new("Scale", qhalft(p = 0.999, df = 7, scale = 500)))
    ## ATau = 10; nuTau = 1
    spec <- Exch(error = Error(scale = HalfT(df = 1, scale = 10)))
    expect_is(spec, "SpecExchNormZero")
    beta <- rnorm(20, mean = 200)
    metadata <- new("MetaData",
                    nms = c("region", "sex"),
                    dimtypes = c("state", "state"),
                    DimScales = list(new("Categories", dimvalues = letters[1:10]),
                        new("Categories", dimvalues = c("f", "m"))))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = 1000,
                          multScale = 1)
    expect_is(prior, "ExchNormZero")
    expect_identical(prior@ATau, new("Scale", 10))
    expect_identical(prior@nuTau, new("DegreesFreedom", 1))
    ## max = 1.5
    spec <- Exch(error = Error(scale = HalfT(max = 1.5)))
    expect_is(spec, "SpecExchNormZero")
    beta <- rnorm(20, mean = 200)
    metadata <- new("MetaData",
                    nms = c("region", "sex"),
                    dimtypes = c("state", "state"),
                    DimScales = list(new("Categories", dimvalues = letters[1:10]),
                        new("Categories", dimvalues = c("f", "m"))))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1)
    expect_is(prior, "ExchNormZero")
    expect_identical(prior@ATau, new("Scale", 0.5))
    expect_identical(prior@nuTau, new("DegreesFreedom", 7))
    expect_identical(prior@tauMax, new("Scale", 1.5))
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1)
    expect_is(prior, "ExchRobustZero")
    expect_identical(prior@J, new("Length", 10L))
    expect_identical(prior@ATau, new("Scale", 1.0))
    expect_identical(prior@nuTau, new("DegreesFreedom", 7.0))
    expect_identical(prior@nuBeta, new("DegreesFreedom", 4.0))
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
                    dimtypes = c("state", "state"),
                    DimScales = list(new("Categories", dimvalues = letters[1:10]),
                        new("Categories", dimvalues = c("f", "m"))))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1)
    expect_is(prior, "ExchNormCov")
    expect_identical(prior@J, new("Length", 20L))
    expect_identical(prior@ATau, new("Scale", 0.5))
    expect_identical(prior@nuTau, new("DegreesFreedom", 7.0))
    expect_identical(prior@contrastsArg, contrastsArg)
    expect_identical(prior@formula, formula)
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
                    dimtypes = c("state", "state"),
                    DimScales = list(new("Categories", dimvalues = letters[1:10]),
                        new("Categories", dimvalues = c("f", "m"))))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1)
    expect_is(prior, "ExchRobustCov")
    expect_identical(prior@J, new("Length", 20L))
    expect_identical(prior@ATau, new("Scale", 0.5))
    expect_identical(prior@nuBeta, new("DegreesFreedom", 4.0))
    expect_identical(prior@nuTau, new("DegreesFreedom", 7.0))
    expect_identical(prior@contrastsArg, contrastsArg)
    expect_identical(prior@formula, formula)
})

test_that("generator function and initialPrior work with DLMNoTrendNormZeroNoSeason", {
    set.seed(100)
    initialPrior <- demest:::initialPrior
    spec <- DLM(trend = NULL)
    expect_is(spec, "SpecDLMNoTrendNormZeroNoSeason")
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1)
    expect_is(prior, "DLMWithTrendNormZeroNoSeason")
    spec <- DLM(trend = Trend(initial = Initial(mean = 0.02, sd = 0.05)))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1)
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1)
    expect_is(prior, "DLMNoTrendNormZeroWithSeason")
})

test_that("generator function and initialPrior work with DLMWithTrendNormZeroWithSeason", {
    set.seed(100)
    initialPrior <- demest:::initialPrior
    spec <- DLM(season = Season(n = 4))
    expect_is(spec, "SpecDLMWithTrendNormZeroWithSeason")
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1)
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
    metadata <- new("MetaData",
                    nms = c("time", "reg"),
                    dimtypes = c("time", "state"),
                    DimScales = list(new("Points", dimvalues = 2001:2010),
                        new("Categories", dimvalues = c("a", "b"))))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = 200,
                          multScale = 1)
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
    metadata <- new("MetaData",
                    nms = c("time", "reg"),
                    dimtypes = c("time", "state"),
                    DimScales = list(new("Points", dimvalues = 2001:2010),
                        new("Categories", dimvalues = c("a", "b"))))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = 200,
                          multScale = 1)
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
    metadata <- new("MetaData",
                    nms = c("time", "reg"),
                    dimtypes = c("time", "state"),
                    DimScales = list(new("Points", dimvalues = 2001:2010),
                        new("Categories", dimvalues = c("a", "b"))))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = 200,
                          multScale = 1)
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
    metadata <- new("MetaData",
                    nms = c("time", "reg"),
                    dimtypes = c("time", "state"),
                    DimScales = list(new("Points", dimvalues = 2001:2010),
                        new("Categories", dimvalues = c("a", "b"))))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = 200,
                          multScale = 1)
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1)
    expect_is(prior, "DLMNoTrendRobustZeroNoSeason")
})

test_that("generator function and initialPrior work with DLMWithTrendRobustZeroNoSeason", {
    set.seed(100)
    initialPrior <- demest:::initialPrior
    spec <- DLM(error = Error(robust = TRUE))
    expect_is(spec, "SpecDLMWithTrendRobustZeroNoSeason")
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1)
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1)
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1)
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
    metadata <- new("MetaData",
                    nms = c("time", "reg"),
                    dimtypes = c("time", "state"),
                    DimScales = list(new("Points", dimvalues = 2001:2010),
                        new("Categories", dimvalues = c("a", "b"))))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = 200,
                          multScale = 1)
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = 200,
                          multScale = 1)
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
    metadata <- new("MetaData",
                    nms = c("time", "reg"),
                    dimtypes = c("time", "state"),
                    DimScales = list(new("Points", dimvalues = 2001:2010),
                        new("Categories", dimvalues = c("a", "b"))))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = 200,
                          multScale = 1)
    expect_is(prior, "DLMNoTrendRobustCovWithSeason")
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
    metadata <- new("MetaData",
                    nms = c("time", "reg"),
                    dimtypes = c("time", "state"),
                    DimScales = list(new("Points", dimvalues = 2001:2010),
                        new("Categories", dimvalues = c("a", "b"))))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = 200,
                          multScale = 1)
    expect_is(prior, "DLMWithTrendRobustCovWithSeason")
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
    ## all defaults
    spec <- Mix()
    expect_is(spec, "SpecMixNormZero")
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1)
    expect_is(prior, "MixNormZero")
    ## nondefault
    spec <- Mix(along = "age",
                vectors = Vectors(scale = HalfT(df = 4, scale = 0.5, mult = 2)),
                weights = Weights(mean = 0.5,
                                  sd = 0.5,
                                  temporary = HalfT(scale = 0.4),
                                  permanent = HalfT(mult = 0.5)),
                error = Error(scale = HalfT(df = 6)),
                maxClass = 11)
    beta <- rnorm(200)
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1)
    expect_identical(prior@iAlong, 3L)
    expect_identical(prior@AVectorsMix, new("Scale", 0.5))
    expect_identical(prior@nuVectorsMix, new("DegreesFreedom", 4))
    expect_identical(prior@priorMeanLevelComponentWeightMix, new("Parameter", 0.5))
    expect_identical(prior@priorSDLevelComponentWeightMix, new("Scale", 0.5))
    expect_identical(prior@AComponentWeightMix, new("Scale", 0.4))
    expect_identical(prior@ALevelComponentWeightMix, new("Scale", 0.125))
    expect_identical(prior@nuTau, new("DegreesFreedom", 6))
})




## initialPriorPredict ###############################################################

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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1)
    expect_is(prior, "ExchNormZero")
    metadata.new <- new("MetaData",
                        nms = "region",
                        dimtypes = "state",
                        DimScales = list(new("Categories", dimvalues = letters[11:15])))
    beta.new <- rnorm(5)
    ans.obtained <- initialPriorPredict(prior,
                                        data = NULL,
                                        metadata = metadata.new,
                                        name = "region",
                                        along = "region")
    ans.expected <- initialPrior(spec,
                                 beta = beta.new,
                                 metadata = metadata.new,
                                 sY = NULL,
                                 multScale = 1)
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1)
    expect_is(prior, "ExchRobustZero")
    metadata.new <- new("MetaData",
                        nms = "region",
                        dimtypes = "state",
                        DimScales = list(new("Categories", dimvalues = letters[11:15])))
    beta.new <- rnorm(5)
    prior.new <- initialPriorPredict(prior,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "region",
                                     along = "region")
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
    metadata <- new("MetaData",
                    nms = c("region", "sex"),
                    dimtypes = c("state", "state"),
                    DimScales = list(new("Categories", dimvalues = letters[1:10]),
                        new("Categories", dimvalues = c("f", "m"))))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1)
    expect_is(prior, "ExchNormCov")
    metadata.new <- new("MetaData",
                        nms = c("region", "sex"),
                        dimtypes = c("state", "state"),
                        DimScales = list(new("Categories", dimvalues = letters[11:15]),
                            new("Categories", dimvalues = c("f", "m"))))
    data <- data.frame(region = rep(letters[11:15], times = 2),
                             sex = rep(c("f", "m"), each = 5),
                             income = rnorm(10),
                             cat = sample(c("x" ,"y", "z"), size = 10, replace = TRUE))
    beta.new <- rnorm(10)
    prior.new <- initialPriorPredict(prior,
                                     data = data,
                                     metadata = metadata.new,
                                     name = "region:sex",
                                     along = "region")
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
    metadata <- new("MetaData",
                    nms = c("region", "sex"),
                    dimtypes = c("state", "state"),
                    DimScales = list(new("Categories", dimvalues = letters[1:10]),
                        new("Categories", dimvalues = c("f", "m"))))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1)
    expect_is(prior, "ExchRobustCov")
    metadata.new <- new("MetaData",
                        nms = c("region", "sex"),
                        dimtypes = c("state", "state"),
                        DimScales = list(new("Categories", dimvalues = letters[11:15]),
                            new("Categories", dimvalues = c("f", "m"))))
    data <- data.frame(region = rep(letters[11:15], times = 2),
                             sex = rep(c("f", "m"), each = 5),
                             income = rnorm(10),
                             cat = sample(c("x" ,"y", "z"), size = 10, replace = TRUE))
    beta.new <- rnorm(10)
    prior.new <- initialPriorPredict(prior,
                                     data = data,
                                     metadata = metadata.new,
                                     name = "region:sex",
                                     along = "region")
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1)
    expect_is(prior, "DLMNoTrendNormZeroNoSeason")
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1)
    expect_is(prior, "DLMWithTrendNormZeroNoSeason")
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1)
    expect_is(prior, "DLMNoTrendNormZeroWithSeason")
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1)
    expect_is(prior, "DLMWithTrendNormZeroWithSeason")
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1)
    expect_is(prior, "DLMNoTrendNormCovNoSeason")
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1)
    expect_is(prior, "DLMWithTrendNormCovNoSeason")
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1)
    expect_is(prior, "DLMNoTrendNormCovWithSeason")
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1)
    expect_is(prior, "DLMWithTrendNormCovWithSeason")
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1)
    expect_is(prior, "DLMNoTrendRobustZeroNoSeason")
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1)
    expect_is(prior, "DLMWithTrendRobustZeroNoSeason")
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1)
    expect_is(prior, "DLMNoTrendRobustZeroWithSeason")
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1)
    expect_is(prior, "DLMWithTrendRobustZeroWithSeason")
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior,
                                     data = NULL,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1)
    expect_is(prior, "DLMNoTrendRobustCovNoSeason")
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1)
    expect_is(prior, "DLMWithTrendRobustCovNoSeason")
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1)
    expect_is(prior, "DLMNoTrendRobustCovWithSeason")
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1)
    expect_is(prior, "DLMWithTrendRobustCovWithSeason")
    data.new <- data.frame(time = 2011:2015,
                           income = rnorm(5))
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    prior.new <- initialPriorPredict(prior = prior,
                                     data = data.new,
                                     metadata = metadata.new,
                                     name = "time",
                                     along = 1L)
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




