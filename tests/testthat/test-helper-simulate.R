
context("helper-simulate")

n.test <- 5
test.identity <- FALSE
test.extended <- FALSE


test_that("checkAllDimensionsHavePriors works", {
    checkAllDimensionsHavePriors <- demest:::checkAllDimensionsHavePriors
    model <- Model(y ~ Poisson(mean ~ age * sex),
               `(Intercept)` ~ ExchFixed(sd = 10), 
               age ~ Exch(error = Error(scale = HalfT(scale = 0.1))),
               sex ~ ExchFixed(sd = 0.1),
               age:sex ~ ExchFixed(sd = 0.05),
               priorSD = HalfT(scale = 0.2))
    y <- Counts(array(0L,
                      dim = c(2, 3),
                      dimnames = list(sex = c("F", "M"),
                                      age = c("0-4", "5-9", "10+"))))
    expect_identical(checkAllDimensionsHavePriors(model = model, y = y),
                     NULL)
    y <- Counts(array(0L,
                      dim = c(2, 3, 3),
                      dimnames = list(sex = c("F", "M"),
                                      age = c("0-4", "5-9", "10+"),
                                      region = c("a", "b", "c"))))
    expect_error(checkAllDimensionsHavePriors(model = model, y = y),
                 "no prior specified for \"region\" dimension in model for 'y'")
    model <- Model(y ~ Poisson(mean ~ age * sex),
               age ~ Exch(error = Error(scale = HalfT(scale = 0.1))),
               sex ~ ExchFixed(sd = 0.1),
               age:sex ~ ExchFixed(sd = 0.05),
               priorSD = HalfT(scale = 0.2))
    y <- Counts(array(0L,
                      dim = c(2, 3),
                      dimnames = list(sex = c("F", "M"),
                                      age = c("0-4", "5-9", "10+"))))
    expect_error(checkAllDimensionsHavePriors(model = model, y = y),
                 "no prior specified for intercept in model for 'y'")
    model <- Model(d ~ Round3())
    expect_identical(checkAllDimensionsHavePriors(model = model, y = y),
                     NULL)
})


## checkPriorInform ########################################################

test_that("checkPriorInform_prohibited works", {
    checkPriorInform_prohibited <- demest:::checkPriorInform_prohibited
    object <- ExchFixed()
    expect_identical(checkPriorInform_prohibited(object = object,
                                                 nameSlot = "multTau",
                                                 nameArg = "mult",
                                                 nameFun = "ExchFixed"),
                     NULL)
    object <- ExchFixed(mult = 3)
    expect_identical(checkPriorInform_prohibited(object = object,
                                                 nameSlot = "multTau",
                                                 nameArg = "mult",
                                                 nameFun = "ExchFixed"),
                     "value for 'mult' supplied in call to 'ExchFixed'")                 
})

test_that("checkPriorInform_required works", {
    checkPriorInform_required <- demest:::checkPriorInform_required
    object <- ExchFixed(sd = 3)
    expect_identical(checkPriorInform_required(object = object,
                                               nameSlot = "tau",
                                               nameArg = "sd",
                                               nameFun = "ExchFixed"),
                     NULL)
    object <- ExchFixed()
    expect_identical(checkPriorInform_required(object = object,
                                               nameSlot = "tau",
                                               nameArg = "sd",
                                               nameFun = "ExchFixed"),
                     "value for 'sd' not supplied in call to 'ExchFixed'")                 
})

test_that("checkPriorInform_ExchFixed works", {
    checkPriorInform_ExchFixed <- demest:::checkPriorInform_ExchFixed
    object <- ExchFixed(sd = 3)
    expect_identical(checkPriorInform_ExchFixed(object = object),
                     NULL)
    object <- ExchFixed(mult = 3)
    expect_identical(checkPriorInform_ExchFixed(object = object),
                     "value for 'mult' supplied in call to 'ExchFixed'")                 
    object <- ExchFixed()
    expect_identical(checkPriorInform_ExchFixed(object = object),
                     "value for 'sd' not supplied in call to 'ExchFixed'")                 
})

test_that("checkPriorInform_Error works", {
    checkPriorInform_Error <- demest:::checkPriorInform_Error
    object <- Exch(error = Error(scale = HalfT(scale = 0.2)))
    expect_identical(checkPriorInform_Error(object = object),
                     NULL)
    object <- Exch(error = Error(scale = HalfT(mult = 3)))
    expect_identical(checkPriorInform_Error(object = object),
                     "value for 'mult' supplied in call to 'HalfT' when specifying 'error'")                 
    object <- Exch()
    expect_identical(checkPriorInform_Error(object = object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'error'")
})

test_that("checkPriorInform_Covariates works", {
    checkPriorInform_Covariates <- demest:::checkPriorInform_Covariates
    data <- data.frame(region = letters[1:10],
                       income = rnorm(10))
    object <- Exch(covariates = Covariates(mean ~ income,
                                           data = data,
                                           intercept = Norm(sd = 10),
                                           coef = TDist(scale = c(0.4, 0.3))))
    expect_identical(checkPriorInform_Covariates(object = object),
                     NULL)
    object <- Exch(covariates = Covariates(mean ~ income,
                                           data = data,
                                           intercept = Norm(),
                                           coef = TDist(scale = 0.4)))
    expect_identical(checkPriorInform_Covariates(object = object),
                     "value for 'sd' not supplied in call to 'Norm' when specifying 'covariates'")
    object <- Exch(covariates = Covariates(mean ~ income,
                                           data = data,
                                           intercept = Norm(sd = 3),
                                           coef = TDist()))
    expect_identical(checkPriorInform_Covariates(object = object),
                     "value for 'scale' not supplied in call to 'TDist' when specifying 'covariates'")
})

test_that("checkPriorInform_Level works", {
    checkPriorInform_Level <- demest:::checkPriorInform_Level
    object <- DLM(level = Level(scale = HalfT(scale = 0.02)))
    expect_identical(checkPriorInform_Level(object = object),
                     NULL)
    object <- DLM(level = Level(scale = HalfT(mult = 3)))
    expect_identical(checkPriorInform_Level(object = object),
                     "value for 'mult' supplied in call to 'HalfT' when specifying 'level'")
    object <- DLM()
    expect_identical(checkPriorInform_Level(object = object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'level'")
})


test_that("checkPriorInform_Trend works", {
    checkPriorInform_Trend <- demest:::checkPriorInform_Trend
    object <- DLM(trend = Trend(initial = Initial(sd = 0.01),
                                scale = HalfT(scale = 0.4)))
    expect_identical(checkPriorInform_Trend(object = object),
                     NULL)
    object <- DLM(trend = Trend(initial = Initial(mult = 3),
                                scale = HalfT(scale = 0.4)))
    expect_identical(checkPriorInform_Trend(object = object),
                     "value for 'mult' supplied in call to 'Initial' when specifying 'trend'")
    object <- DLM(trend = Trend(scale = HalfT(scale = 0.4)))
    expect_identical(checkPriorInform_Trend(object = object),
                     "value for 'sd' not supplied in call to 'Initial' when specifying 'trend'")
    object <- DLM(trend = Trend(initial = Initial(sd = 3),
                                scale = HalfT(mult = 3)))
    expect_identical(checkPriorInform_Trend(object = object),
                     "value for 'mult' supplied in call to 'HalfT' when specifying 'trend'")
    object <- DLM(trend = Trend(initial = Initial(sd = 3)))
    expect_identical(checkPriorInform_Trend(object = object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'trend'")
})

test_that("checkPriorInform_Season works", {
    checkPriorInform_Season <- demest:::checkPriorInform_Season
    object <- DLM(season = Season(n = 4, scale = HalfT(scale = 0.4)))
    expect_identical(checkPriorInform_Season(object = object),
                     NULL)
    object <- DLM(season = Season(n = 4, scale = HalfT(mult = 3)))
    expect_identical(checkPriorInform_Season(object = object),
                     "value for 'mult' supplied in call to 'HalfT' when specifying 'season'")
    object <- DLM(season = Season(n = 3))
    expect_identical(checkPriorInform_Season(object = object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'season'")
})

test_that("checkPriorInform_Components works", {
    checkPriorInform_Components <- demest:::checkPriorInform_Components
    object <- Mix(components = Components(scale = HalfT(scale = 0.4)))
    expect_identical(checkPriorInform_Components(object = object),
                     NULL)
    object <- Mix(components = Components(scale = HalfT(mult = 0.4)))
    expect_identical(checkPriorInform_Components(object = object),
                     "value for 'mult' supplied in call to 'HalfT' when specifying 'components'")
    object <- Mix(components = Components())
    expect_identical(checkPriorInform_Components(object = object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'components'")
})

test_that("checkPriorInform_Weights works", {
    checkPriorInform_Weights <- demest:::checkPriorInform_Weights
    object <- Mix(weights = Weights(scale1 = HalfT(scale = 0.4),
                                    scale2 = HalfT(scale = 0.3)))
    expect_identical(checkPriorInform_Weights(object = object),
                     NULL)
    object <- Mix(weights = Weights(scale1 = HalfT(mult = 0.4),
                                    scale2 = HalfT(scale = 0.3)))
    expect_identical(checkPriorInform_Weights(object = object),
                     "value for 'mult' supplied in call to 'HalfT' when specifying 'scale1' for 'weights'")
    object <- Mix(weights = Weights(scale2 = HalfT(scale = 0.3)))
    expect_identical(checkPriorInform_Weights(object = object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'scale1' for 'weights'")
    object <- Mix(weights = Weights(scale1 = HalfT(scale = 0.4),
                                    scale2 = HalfT(mult = 0.3)))
    expect_identical(checkPriorInform_Weights(object = object),
                     "value for 'mult' supplied in call to 'HalfT' when specifying 'scale2' for 'weights'")
    object <- Mix(weights = Weights(scale1 = HalfT(scale = 0.3)))
    expect_identical(checkPriorInform_Weights(object = object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'scale2' for 'weights'")
})

test_that("checkPriorSDInformative works", {
    checkPriorSDInformative <- demest:::checkPriorSDInformative
    model <- Model(y ~ Poisson(mean ~ region),
                   `(Intercept)` ~ ExchFixed(mean = -1, sd = 0.2),
                   region ~ Exch(error = Error(scale = HalfT(scale = 0.3))),
                   priorSD = HalfT(scale = 0.1))
    expect_is(model, "SpecVarying")
    expect_identical(checkPriorSDInformative(model),
                     NULL)
    model <- Model(y ~ Poisson(mean ~ region),
                   `(Intercept)` ~ ExchFixed(mean = -1, sd = 0.2),
                   region ~ Exch(),
                   priorSD = HalfT(mult = 0.1))
    expect_error(checkPriorSDInformative(model),
                 "problem with specification of 'priorSD' in model for 'y' : value for 'mult' supplied in call to 'HalfT'")
    model <- Model(y ~ Poisson(mean ~ region),
                   `(Intercept)` ~ ExchFixed(mean = -1),
                   region ~ Exch(),
                   priorSD = HalfT())
    expect_error(checkPriorSDInformative(model),
                 "problem with specification of 'priorSD' in model for 'y' : 'scale' argument not supplied in call to 'HalfT'")
    model <- Model(y ~ Poisson(mean ~ region),
                   `(Intercept)` ~ ExchFixed(mean = -1),
                   region ~ Exch())
    expect_error(checkPriorSDInformative(model),
                 "problem with specification of 'priorSD' in model for 'y' : 'priorSD' argument not supplied in call to 'Model'")
})

test_that("checkPriorsAreInformative works", {
    checkPriorsAreInformative <- demest:::checkPriorsAreInformative
    model <- Model(y ~ Poisson(mean ~ region),
                   `(Intercept)` ~ ExchFixed(mean = -1, sd = 0.2),
                   region ~ Exch(error = Error(scale = HalfT(scale = 0.3))))
    expect_is(model, "SpecVarying")
    expect_identical(checkPriorsAreInformative(model),
                     NULL)
    model <- Model(y ~ Poisson(mean ~ region),
                   `(Intercept)` ~ ExchFixed(mean = -1, sd = 0.2),
                   region ~ Exch())
    expect_error(checkPriorsAreInformative(model),
                 "problem with prior for 'region' in model for 'y'")
    model <- Model(y ~ Poisson(mean ~ region),
                   `(Intercept)` ~ ExchFixed(mean = -1),
                   region ~ Exch())
    expect_error(checkPriorsAreInformative(model),
                 "problem with prior for '\\(Intercept\\)' in model for 'y'")
    model <- Model(d ~ Round3())
    expect_identical(checkPriorsAreInformative(model),
                     NULL)
})
               

## drawPrior, drawModel ################################################

test_that("drawBeta_standard works", {
    drawBeta_standard <- demest:::drawBeta_standard
    initialPrior <- demest:::initialPrior
    betaHat <- demest:::betaHat
    getV <- demest:::getV
    ## no structural zeros
    spec <- Exch(error = Error(scale = HalfT(scale = 0.1)))
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
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.obtained <- drawBeta_standard(prior)
        set.seed(seed)
        ans.expected <- rnorm(n = prior@J@.Data,
                              mean = betaHat(prior),
                              sd = sqrt(getV(prior)))
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
    ## with structural zeros
    spec <- Exch(error = Error(scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(c(0L, rep(1L, 9)),
                                   dim = 10L,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.obtained <- drawBeta_standard(prior)
        set.seed(seed)
        ans.expected <- c(NA,
                          rnorm(n = prior@J@.Data - 1,
                                mean = betaHat(prior)[-1],
                                sd = sqrt(getV(prior))[-1]))
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of drawBeta_standard give same answer", {
    drawBeta_standard <- demest:::drawBeta_standard
    initialPrior <- demest:::initialPrior
    betaHat <- demest:::betaHat
    getV <- demest:::getV
    ## no structural zeros
    spec <- Exch(error = Error(scale = HalfT(scale = 0.1)))
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
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- drawBeta_standard(model, useC = FALSE)
        set.seed(seed)
        ans.C <- drawBeta_standard(model, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
    ## with structural zeros
    spec <- Exch(error = Error(scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(c(0L, rep(1L, 9)),
                                   dim = 10L,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- drawBeta_standard(model, useC = FALSE)
        set.seed(seed)
        ans.C <- drawBeta_standard(model, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("drawBetas works", {
    initialModel <- demest:::initialModel
    drawBetas <- demest:::drawBetas
    spec <- Model(y ~ Poisson(mean ~ age + sex),
                  `(Intercept)` ~ ExchFixed(sd = 10), 
                  age ~ Exch(error = Error(scale = HalfT(scale = 0.1))),
                  sex ~ ExchFixed(sd = 0.1),
                  priorSD = HalfT(scale = 0.2))
    y <- Counts(array(1L,
                      dim = 2:3,
                      dimnames = list(sex = c("F", "M"),
                                      age = c("0-4", "5-9", "10+"))))
    exposure <- Counts(array(1:6,
                             dim = 2:3,
                             dimnames = list(sex = c("F", "M"),
                                             age = c("0-4", "5-9", "10+"))))
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        model <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed)
        ans.obtained <- drawBetas(model)
        set.seed(seed)
        ans.expected <- model
        for (i in 1:3)
            ans.expected@betas[[i]] <- drawBetas(ans.expected@priorsBetas[[i]])
        expect_identical(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of drawBetas give same answer", {
    initialModel <- demest:::initialModel
    drawBeta <- demest:::drawBeta
    spec <- Model(y ~ Poisson(mean ~ age + sex),
                  `(Intercept)` ~ ExchFixed(sd = 10), 
                  age ~ Exch(error = Error(scale = HalfT(scale = 0.1))),
                  sex ~ ExchFixed(sd = 0.1),
                  priorSD = HalfT(scale = 0.2))
    y <- Counts(array(1L,
                      dim = 2:3,
                      dimnames = list(sex = c("F", "M"),
                                      age = c("0-4", "5-9", "10+"))))
    exposure <- Counts(array(1:6,
                             dim = 2:3,
                             dimnames = list(sex = c("F", "M"),
                                             age = c("0-4", "5-9", "10+"))))
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        model <- initialModel(spec, y = y, exposure = exposure)
        set.seed(seed)
        ans.R <- drawBetas(model, useC = FALSE)
        set.seed(seed)
        ans.C <- drawBetas(model, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("R version of drawEta works", {
    drawEta <- demest:::drawEta
    initialPrior <- demest:::initialPrior
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    data <- data.frame(region = letters[1:10],
                       income = rnorm(10),
                       cat = rep(c("a", "b"), each = 5))
    spec <- Exch(covariate = Covariates(mean ~ income + cat,
                                        data = data,
                                        intercept = Norm(sd = 1),
                                        coef = TDist(mean = c(-1, 1),
                                                     scale = 0.1)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.obtained <- drawEta(prior)@eta@.Data
        set.seed(seed)
        ans.expected <- rnorm(n = prior@P@.Data,
                              mean = c(0,  prior@meanEtaCoef@.Data),
                              sd = c(prior@AEtaIntercept@.Data, sqrt(prior@UEtaCoef@.Data)))
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of drawEta give same answer", {
    drawEta <- demest:::drawEta
    initialPrior <- demest:::initialPrior
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    data <- data.frame(region = letters[1:10],
                       income = rnorm(10),
                       cat = rep(c("a", "b"), each = 5))
    spec <- Exch(covariate = Covariates(mean ~ income + cat,
                                        data = data,
                                        intercept = Norm(sd = 1),
                                        coef = TDist(mean = c(-1, 1),
                                                     scale = 0.1)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- drawEta(prior, useC = FALSE)
        set.seed(seed)
        ans.C <- drawEta(prior, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("R version of drawOmegaAlpha works", {
    drawOmegaAlpha <- demest:::drawOmegaAlpha
    initialPrior <- demest:::initialPrior
    spec <- DLM(level = Level(scale = HalfT(scale = 0.2)),
                trend = NULL,
                damp = NULL,
                error = Error(scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMNoTrendNormZeroNoSeason")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.obtained <- drawOmegaAlpha(prior)@omegaAlpha@.Data
        set.seed(seed)
        ans.expected <- rhalft(n = 1, df = prior@nuAlpha@.Data, scale = prior@AAlpha@.Data)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of version of drawOmegaAlpha give same answer", {
    drawOmegaAlpha <- demest:::drawOmegaAlpha
    initialPrior <- demest:::initialPrior
    spec <- DLM(level = Level(scale = HalfT(scale = 0.2)),
                trend = NULL,
                damp = NULL,
                error = Error(scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMNoTrendNormZeroNoSeason")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- drawOmegaAlpha(prior, useC = FALSE)
        set.seed(seed)
        ans.C <- drawOmegaAlpha(prior, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("R version of drawOmegaComponentWeightMix works", {
    drawOmegaComponentWeightMix <- demest:::drawOmegaComponentWeightMix
    initialPrior <- demest:::initialPrior
    spec <- Mix(components = Components(scale = HalfT(scale = 0.1)),
                weights = Weights(scale1 = HalfT(scale = 0.1),
                                  scale2 = HalfT(scale = 0.1)),
                error = Error(scale = HalfT(scale = 0.1)))
    beta <- rnorm(200)
    metadata <- new("MetaData",
                    nms = c("time", "reg", "age"),
                    dimtypes = c("time", "state", "age"),
                    DimScales = list(new("Points", dimvalues = 2001:2010),
                                     new("Categories", dimvalues = c("a", "b")),
                                     new("Intervals", dimvalues = as.numeric(0:10))))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(10, 2, 10),
                                   dimnames = list(time = 2001:2010,
                                                   reg = c("a", "b"),
                                                   age = 0:9)),
                             dimscales = c(time = "Points", age = "Intervals"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          margin = 1:3,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "MixNormZero")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.obtained <- drawOmegaComponentWeightMix(prior)@omegaComponentWeightMix@.Data
        set.seed(seed)
        ans.expected <- rhalft(n = 1,
                               df = prior@nuComponentWeightMix@.Data,
                               scale = prior@AComponentWeightMix@.Data)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C version of drawOmegaComponentWeightMix give same answer", {
    drawOmegaComponentWeightMix <- demest:::drawOmegaComponentWeightMix
    initialPrior <- demest:::initialPrior
    spec <- Mix(components = Components(scale = HalfT(scale = 0.1)),
                weights = Weights(scale1 = HalfT(scale = 0.1),
                                  scale2 = HalfT(scale = 0.1)),
                error = Error(scale = HalfT(scale = 0.1)))
    beta <- rnorm(200)
    metadata <- new("MetaData",
                    nms = c("time", "reg", "age"),
                    dimtypes = c("time", "state", "age"),
                    DimScales = list(new("Points", dimvalues = 2001:2010),
                                     new("Categories", dimvalues = c("a", "b")),
                                     new("Intervals", dimvalues = as.numeric(0:10))))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(10, 2, 10),
                                   dimnames = list(time = 2001:2010,
                                                   reg = c("a", "b"),
                                                   age = 0:9)),
                             dimscales = c(time = "Points", age = "Intervals"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          margin = 1:3,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "MixNormZero")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- drawOmegaComponentWeightMix(prior, useC = FALSE)
        set.seed(seed)
        ans.C <- drawOmegaComponentWeightMix(prior, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("R version of drawOmegaDelta works", {
    drawOmegaDelta <- demest:::drawOmegaDelta
    initialPrior <- demest:::initialPrior
    spec <- DLM(level = Level(scale = HalfT(scale = 0.2)),
                trend = Trend(initial = Initial(sd = 0.2),
                              scale = HalfT(scale = 0.2)),
                damp = NULL,
                error = Error(scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMWithTrendNormZeroNoSeason")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.obtained <- drawOmegaDelta(prior)@omegaDelta@.Data
        set.seed(seed)
        ans.expected <- rhalft(n = 1, df = prior@nuDelta@.Data, scale = prior@ADelta@.Data)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of version of drawOmegaDelta give same answer", {
    drawOmegaDelta <- demest:::drawOmegaDelta
    initialPrior <- demest:::initialPrior
    spec <- DLM(level = Level(scale = HalfT(scale = 0.2)),
                trend = Trend(initial = Initial(sd = 0.2),
                              scale = HalfT(scale = 0.2)),
                damp = NULL,
                error = Error(scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMWithTrendNormZeroNoSeason")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- drawOmegaDelta(prior, useC = FALSE)
        set.seed(seed)
        ans.C <- drawOmegaDelta(prior, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("R version of drawOmegaLevelComponentWeightMix works", {
    drawOmegaLevelComponentWeightMix <- demest:::drawOmegaLevelComponentWeightMix
    initialPrior <- demest:::initialPrior
    spec <- Mix(components = Components(scale = HalfT(scale = 0.1)),
                weights = Weights(scale1 = HalfT(scale = 0.1),
                                  scale2 = HalfT(scale = 0.1)),
                error = Error(scale = HalfT(scale = 0.1)))
    beta <- rnorm(200)
    metadata <- new("MetaData",
                    nms = c("time", "reg", "age"),
                    dimtypes = c("time", "state", "age"),
                    DimScales = list(new("Points", dimvalues = 2001:2010),
                                     new("Categories", dimvalues = c("a", "b")),
                                     new("Intervals", dimvalues = as.numeric(0:10))))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(10, 2, 10),
                                   dimnames = list(time = 2001:2010,
                                                   reg = c("a", "b"),
                                                   age = 0:9)),
                             dimscales = c(time = "Points", age = "Intervals"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          margin = 1:3,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "MixNormZero")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.obtained <- drawOmegaLevelComponentWeightMix(prior)@omegaLevelComponentWeightMix@.Data
        set.seed(seed)
        ans.expected <- rhalft(n = 1,
                               df = prior@nuLevelComponentWeightMix@.Data,
                               scale = prior@ALevelComponentWeightMix@.Data)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C version of drawOmegaLevelComponentWeightMix give same answer", {
    drawOmegaLevelComponentWeightMix <- demest:::drawOmegaLevelComponentWeightMix
    initialPrior <- demest:::initialPrior
    spec <- Mix(components = Components(scale = HalfT(scale = 0.1)),
                weights = Weights(scale1 = HalfT(scale = 0.1),
                                  scale2 = HalfT(scale = 0.1)),
                error = Error(scale = HalfT(scale = 0.1)))
    beta <- rnorm(200)
    metadata <- new("MetaData",
                    nms = c("time", "reg", "age"),
                    dimtypes = c("time", "state", "age"),
                    DimScales = list(new("Points", dimvalues = 2001:2010),
                                     new("Categories", dimvalues = c("a", "b")),
                                     new("Intervals", dimvalues = as.numeric(0:10))))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(10, 2, 10),
                                   dimnames = list(time = 2001:2010,
                                                   reg = c("a", "b"),
                                                   age = 0:9)),
                             dimscales = c(time = "Points", age = "Intervals"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          margin = 1:3,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "MixNormZero")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- drawOmegaLevelComponentWeightMix(prior, useC = FALSE)
        set.seed(seed)
        ans.C <- drawOmegaLevelComponentWeightMix(prior, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("R version of drawOmegaSeason works", {
    drawOmegaSeason <- demest:::drawOmegaSeason
    initialPrior <- demest:::initialPrior
    spec <- DLM(level = Level(scale = HalfT(scale = 0.2)),
                trend = Trend(initial = Initial(sd = 0.2),
                              scale = HalfT(scale = 0.2)),
                season = Season(n = 4, scale = HalfT(scale = 0.05)),
                damp = NULL,
                error = Error(scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMWithTrendNormZeroWithSeason")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.obtained <- drawOmegaSeason(prior)@omegaSeason@.Data
        set.seed(seed)
        ans.expected <- rhalft(n = 1, df = prior@nuSeason@.Data, scale = prior@ASeason@.Data)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of version of drawOmegaSeason give same answer", {
    drawOmegaSeason <- demest:::drawOmegaSeason
    initialPrior <- demest:::initialPrior
    spec <- DLM(level = Level(scale = HalfT(scale = 0.2)),
                trend = Trend(initial = Initial(sd = 0.2),
                              scale = HalfT(scale = 0.2)),
                season = Season(n = 4, scale = HalfT(scale = 0.05)),
                damp = NULL,
                error = Error(scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMWithTrendNormZeroNoSeason")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- drawOmegaSeason(prior, useC = FALSE)
        set.seed(seed)
        ans.C <- drawOmegaSeason(prior, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("R version of drawOmegaVectorsMix works", {
    drawOmegaVectorsMix <- demest:::drawOmegaVectorsMix
    initialPrior <- demest:::initialPrior
    spec <- Mix(components = Components(scale = HalfT(scale = 0.1)),
                weights = Weights(scale1 = HalfT(scale = 0.1),
                                  scale2 = HalfT(scale = 0.1)),
                error = Error(scale = HalfT(scale = 0.1)))
    beta <- rnorm(200)
    metadata <- new("MetaData",
                    nms = c("time", "reg", "age"),
                    dimtypes = c("time", "state", "age"),
                    DimScales = list(new("Points", dimvalues = 2001:2010),
                                     new("Categories", dimvalues = c("a", "b")),
                                     new("Intervals", dimvalues = as.numeric(0:10))))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(10, 2, 10),
                                   dimnames = list(time = 2001:2010,
                                                   reg = c("a", "b"),
                                                   age = 0:9)),
                             dimscales = c(time = "Points", age = "Intervals"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          margin = 1:3,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "MixNormZero")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.obtained <- drawOmegaVectorsMix(prior)@omegaVectorsMix@.Data
        set.seed(seed)
        ans.expected <- rhalft(n = 1,
                               df = prior@nuVectorsMix@.Data,
                               scale = prior@AVectorsMix@.Data)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C version of drawOmegaVectorsMix give same answer", {
    drawOmegaVectorsMix <- demest:::drawOmegaVectorsMix
    initialPrior <- demest:::initialPrior
    spec <- Mix(components = Components(scale = HalfT(scale = 0.1)),
                weights = Weights(scale1 = HalfT(scale = 0.1),
                                  scale2 = HalfT(scale = 0.1)),
                error = Error(scale = HalfT(scale = 0.1)))
    beta <- rnorm(200)
    metadata <- new("MetaData",
                    nms = c("time", "reg", "age"),
                    dimtypes = c("time", "state", "age"),
                    DimScales = list(new("Points", dimvalues = 2001:2010),
                                     new("Categories", dimvalues = c("a", "b")),
                                     new("Intervals", dimvalues = as.numeric(0:10))))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(10, 2, 10),
                                   dimnames = list(time = 2001:2010,
                                                   reg = c("a", "b"),
                                                   age = 0:9)),
                             dimscales = c(time = "Points", age = "Intervals"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          margin = 1:3,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "MixNormZero")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- drawOmegaVectorsMix(prior, useC = FALSE)
        set.seed(seed)
        ans.C <- drawOmegaVectorsMix(prior, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("R version of drawPhi works", {
    drawPhi <- demest:::drawPhi
    initialPrior <- demest:::initialPrior
    spec <- DLM(level = Level(scale = HalfT(scale = 0.2)),
                trend = NULL,
                damp = Damp(shape1 = 3, shape2 = 3),
                error = Error(scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMNoTrendNormZeroNoSeason")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.obtained <- drawPhi(prior)@phi@.Data
        set.seed(seed)
        ans.expected <- (rbeta(n = 1,
                              shape1 = prior@shape1Phi@.Data,
                              shape2 = prior@shape2Phi@.Data) * (prior@maxPhi - prior@minPhi)
            + prior@minPhi)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of drawPhi give same answer", {
    drawPhi <- demest:::drawPhi
    initialPrior <- demest:::initialPrior
    spec <- DLM(level = Level(scale = HalfT(scale = 0.2)),
                trend = NULL,
                damp = Damp(shape1 = 3, shape2 = 3),
                error = Error(scale = HalfT(scale = 0.1)))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMNoTrendNormZeroNoSeason")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- drawPhi(prior, useC = FALSE)
        set.seed(seed)
        ans.C <- drawPhi(prior, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("R version of drawPhiMix works", {
    drawPhiMix <- demest:::drawPhiMix
    initialPrior <- demest:::initialPrior
    spec <- Mix(components = Components(scale = HalfT(scale = 0.1)),
                weights = Weights(scale1 = HalfT(scale = 0.1),
                                  scale2 = HalfT(scale = 0.1)),
                error = Error(scale = HalfT(scale = 0.1)))
    beta <- rnorm(200)
    metadata <- new("MetaData",
                    nms = c("time", "reg", "age"),
                    dimtypes = c("time", "state", "age"),
                    DimScales = list(new("Points", dimvalues = 2001:2010),
                                     new("Categories", dimvalues = c("a", "b")),
                                     new("Intervals", dimvalues = as.numeric(0:10))))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(10, 2, 10),
                                   dimnames = list(time = 2001:2010,
                                                   reg = c("a", "b"),
                                                   age = 0:9)),
                             dimscales = c(time = "Points", age = "Intervals"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          margin = 1:3,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "MixNormZero")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.obtained <- drawPhiMix(prior)@phiMix
        set.seed(seed)
        ans.expected <- (rbeta(n = 1,
                              shape1 = prior@shape1Phi@.Data,
                              shape2 = prior@shape2Phi@.Data) * (prior@maxPhi - prior@minPhi)
            + prior@minPhi)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C version of drawPhiMix give same answer", {
    drawPhiMix <- demest:::drawPhiMix
    initialPrior <- demest:::initialPrior
    spec <- Mix(components = Components(scale = HalfT(scale = 0.1)),
                weights = Weights(scale1 = HalfT(scale = 0.1),
                                  scale2 = HalfT(scale = 0.1)),
                error = Error(scale = HalfT(scale = 0.1)))
    beta <- rnorm(200)
    metadata <- new("MetaData",
                    nms = c("time", "reg", "age"),
                    dimtypes = c("time", "state", "age"),
                    DimScales = list(new("Points", dimvalues = 2001:2010),
                                     new("Categories", dimvalues = c("a", "b")),
                                     new("Intervals", dimvalues = as.numeric(0:10))))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(10, 2, 10),
                                   dimnames = list(time = 2001:2010,
                                                   reg = c("a", "b"),
                                                   age = 0:9)),
                             dimscales = c(time = "Points", age = "Intervals"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          margin = 1:3,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "MixNormZero")
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- drawPhiMix(prior, useC = FALSE)
        set.seed(seed)
        ans.C <- drawPhiMix(prior, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})




test_that("drawSigma_Varying works with BinomialVarying", {
    drawSigma_Varying <- demest:::drawSigma_Varying
    initialModel <- demest:::initialModel
    rhalftTrunc1 <- demest:::rhalftTrunc1
    exposure <- Counts(array(rpois(20, lambda  = 10),
                             dim = c(2, 10),
                             dimnames = list(sex = c("f", "m"), age = 0:9)))
    y <- Counts(array(rbinom(20, size = exposure, prob = 0.7),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    spec <- Model(y ~ Binomial(mean ~ sex + age),
                  priorSD = HalfT(scale = 0.1))
    model <- initialModel(spec, y = y, exposure = exposure)
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.expected <- drawSigma_Varying(model)
        set.seed(seed)
        ans.obtained <- model
        ans.obtained@sigma@.Data <- rhalftTrunc1(df = model@nuSigma@.Data,
                                                 scale = model@ASigma@.Data,
                                                 max = model@sigmaMax@.Data,
                                                 useC = TRUE)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of drawSigma_Varying give same answer with BinomialVarying", {
    drawSigma_Varying <- demest:::drawSigma_Varying
    initialModel <- demest:::initialModel
    rhalftTrunc1 <- demest:::rhalftTrunc1
    exposure <- Counts(array(rpois(20, lambda  = 10),
                             dim = c(2, 10),
                             dimnames = list(sex = c("f", "m"), age = 0:9)))
    y <- Counts(array(rbinom(20, size = exposure, prob = 0.7),
                      dim = c(2, 10),
                      dimnames = list(sex = c("f", "m"), age = 0:9)))
    spec <- Model(y ~ Binomial(mean ~ sex + age),
                  priorSD = HalfT(scale = 0.1))
    model <- initialModel(spec, y = y, exposure = exposure)
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- drawSigma_Varying(model, useC = FALSE)
        set.seed(seed)
        ans.C <- drawSigma_Varying(model, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("R version of drawTau works", {
    drawTau <- demest:::drawTau
    initialPrior <- demest:::initialPrior
    spec <- Exch(error = Error(scale = HalfT(scale = 0.1)))
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
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.obtained <- drawTau(prior)@tau@.Data
        set.seed(seed)
        ans.expected <- rhalft(n = 1, df = prior@nuTau@.Data, scale = prior@ATau@.Data)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of drawTau give same answer", {
    drawTau <- demest:::drawTau
    initialPrior <- demest:::initialPrior
    spec <- Exch(error = Error(scale = HalfT(scale = 0.1)))
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
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- drawTau(prior, useC = FALSE)
        set.seed(seed)
        ans.C <- drawTau(prior, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})






test_that("R version of drawUEtaCoef works", {
    drawUEtaCoef <- demest:::drawUEtaCoef
    initialPrior <- demest:::initialPrior
    rinvchisq1 <- demest:::rinvchisq1
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    data <- data.frame(region = letters[1:10],
                       income = rnorm(10),
                       cat = rep(c("a", "b"), each = 5))
    spec <- Exch(covariate = Covariates(mean ~ income + cat,
                                        data = data,
                                        intercept = Norm(sd = 1),
                                        coef = TDist(mean = c(-1, 1),
                                                     scale = 0.1)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.obtained <- drawUEtaCoef(prior)@UEtaCoef@.Data
        set.seed(seed)
        ans.expected <- mapply(rinvchisq1,
                               df = prior@nuEtaCoef@.Data,
                               scaleSq = prior@AEtaCoef@.Data^2)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of drawUEtaCoef give same answer", {
    drawUEtaCoef <- demest:::drawUEtaCoef
    initialPrior <- demest:::initialPrior
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    data <- data.frame(region = letters[1:10],
                       income = rnorm(10),
                       cat = rep(c("a", "b"), each = 5))
    spec <- Exch(covariate = Covariates(mean ~ income + cat,
                                        data = data,
                                        intercept = Norm(sd = 1),
                                        coef = TDist(mean = c(-1, 1),
                                                     scale = 0.1)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10L,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- drawUEtaCoef(prior, useC = FALSE)
        set.seed(seed)
        ans.R <- drawUEtaCoef(prior, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})




