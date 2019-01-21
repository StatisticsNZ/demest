
context("SpecPriors-methods")

n.test <- 5
test.identity <- FALSE
test.extended <- FALSE


## checkPriorIsInformative ##################################################


test_that("checkPriorIsInformative works with SpecExchFixed", {
    checkPriorIsInformative <- demest:::checkPriorIsInformative
    object <- ExchFixed(sd = 3)
    expect_is(object, "SpecExchFixed")
    expect_identical(checkPriorIsInformative(object = object),
                     NULL)
    object <- ExchFixed(mult = 3)
    expect_identical(checkPriorIsInformative(object = object),
                     "value for 'mult' supplied in call to 'ExchFixed'")                 
    object <- ExchFixed()
    expect_identical(checkPriorIsInformative(object = object),
                     "value for 'sd' not supplied in call to 'ExchFixed'")                 
})

test_that("checkPriorIsInformative works with SpecExchNormZero", {
    checkPriorIsInformative <- demest:::checkPriorIsInformative
    object <- Exch(error = Error(scale = HalfT(scale = 3)))
    expect_is(object, "SpecExchNormZero")
    expect_identical(checkPriorIsInformative(object = object),
                     NULL)
    object <- Exch(error = Error(scale = HalfT(mult = 3)))
    expect_identical(checkPriorIsInformative(object = object),
                     "value for 'mult' supplied in call to 'HalfT' when specifying 'error' in call to 'Exch'")
    object <- Exch()
    expect_identical(checkPriorIsInformative(object = object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'error' in call to 'Exch'")
})

test_that("checkPriorIsInformative works with SpecExchRobustZero", {
    checkPriorIsInformative <- demest:::checkPriorIsInformative
    object <- Exch(error = Error(robust = TRUE, scale = HalfT(scale = 3)))
    expect_is(object, "SpecExchRobustZero")
    expect_identical(checkPriorIsInformative(object = object),
                     NULL)
    object <- Exch(error = Error(robust = TRUE, scale = HalfT(mult = 3)))
    expect_identical(checkPriorIsInformative(object = object),
                     "value for 'mult' supplied in call to 'HalfT' when specifying 'error' in call to 'Exch'")
    object <- Exch(error = Error(robust = TRUE))
    expect_identical(checkPriorIsInformative(object = object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'error' in call to 'Exch'")
})

test_that("checkPriorIsInformative works with SpecExchNormCov", {
    checkPriorIsInformative <- demest:::checkPriorIsInformative
    data <- data.frame(region = letters[1:10],
                       income = rnorm(10))
    object <- Exch(covariates = Covariates(mean ~ income,
                                           data = data,
                                           intercept = Norm(sd = 10),
                                           coef = TDist(scale = c(0.4, 0.3))),
                   error = Error(scale = HalfT(scale = 2)))
    expect_is(object, "SpecExchNormCov")
    expect_identical(checkPriorIsInformative(object),
                     NULL)
    object <- Exch(covariates = Covariates(mean ~ income,
                                           data = data,
                                           intercept = Norm(),
                                           coef = TDist(scale = 0.4)),
                   error = Error(scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'sd' not supplied in call to 'Norm' when specifying 'covariates' in call to 'Exch'")
    object <- Exch(covariates = Covariates(mean ~ income,
                                           data = data,
                                           intercept = Norm(sd = 3),
                                           coef = TDist()),
                   error = Error(scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'scale' not supplied in call to 'TDist' when specifying 'covariates' in call to 'Exch'")
    object <- Exch(covariates = Covariates(mean ~ income,
                                           data = data,
                                           intercept = Norm(sd = 3),
                                           coef = TDist(scale = 0.2)),
                   error = Error(scale = HalfT()))
    expect_identical(checkPriorIsInformative(object = object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'error' in call to 'Exch'")
})

test_that("checkPriorIsInformative works with SpecExchRobustCov", {
    checkPriorIsInformative <- demest:::checkPriorIsInformative
    data <- data.frame(region = letters[1:10],
                       income = rnorm(10))
    object <- Exch(covariates = Covariates(mean ~ income,
                                           data = data,
                                           intercept = Norm(sd = 10),
                                           coef = TDist(scale = c(0.4, 0.3))),
                   error = Error(robust = TRUE, scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     NULL)
    expect_is(object, "SpecExchRobustCov")
    object <- Exch(covariates = Covariates(mean ~ income,
                                           data = data,
                                           intercept = Norm(),
                                           coef = TDist(scale = 0.4)),
                   error = Error(robust = TRUE, scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'sd' not supplied in call to 'Norm' when specifying 'covariates' in call to 'Exch'")
    object <- Exch(covariates = Covariates(mean ~ income,
                                           data = data,
                                           intercept = Norm(sd = 3),
                                           coef = TDist()),
                   error = Error(robust = TRUE, scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'scale' not supplied in call to 'TDist' when specifying 'covariates' in call to 'Exch'")
    object <- Exch(covariates = Covariates(mean ~ income,
                                           data = data,
                                           intercept = Norm(sd = 3),
                                           coef = TDist(scale = 0.2)),
                   error = Error(robust = TRUE, scale = HalfT()))
    expect_identical(checkPriorIsInformative(object = object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'error' in call to 'Exch'")
})

test_that("checkPriorIsInformative works with SpecDLMNoTrendNormZeroNoSeason", {
    checkPriorIsInformative <- demest:::checkPriorIsInformative
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = NULL,
                  error = Error(scale = HalfT(scale = 2)))
    expect_is(object, "SpecDLMNoTrendNormZeroNoSeason")
    expect_identical(checkPriorIsInformative(object),
                     NULL)
    object <- DLM(level = Level(scale = HalfT(mult = 2)),
                  trend = NULL,
                  error = Error(scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'mult' supplied in call to 'HalfT' when specifying 'level' in call to 'DLM'")
    object <- DLM(trend = NULL,
                  error = Error(scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'level' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = NULL,
                  error = Error(scale = HalfT(mult = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'mult' supplied in call to 'HalfT' when specifying 'error' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = NULL)
    expect_identical(checkPriorIsInformative(object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'error' in call to 'DLM'")
})


test_that("checkPriorIsInformative works with SpecDLMWithTrendNormZeroNoSeason", {
    checkPriorIsInformative <- demest:::checkPriorIsInformative
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = Trend(initial = Initial(sd = 1),
                                scale = HalfT(scale = 2)),
                  error = Error(scale = HalfT(scale = 2)))
    expect_is(object, "SpecDLMWithTrendNormZeroNoSeason")
    expect_identical(checkPriorIsInformative(object),
                     NULL)
    object <- DLM(level = Level(scale = HalfT(mult = 2)),
                  trend = Trend(initial = Initial(sd = 1),
                                scale = HalfT(scale = 2)),
                  error = Error(scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'mult' supplied in call to 'HalfT' when specifying 'level' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = Trend(scale = HalfT(scale = 2)),
                  error = Error(scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'sd' not supplied in call to 'Initial' when specifying 'trend' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = Trend(initial = Initial(sd = 1),
                                scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'error' in call to 'DLM'")
})

test_that("checkPriorIsInformative works with SpecDLMNoTrendNormZeroWithSeason", {
    checkPriorIsInformative <- demest:::checkPriorIsInformative
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = NULL,
                  season = Season(n = 4, scale = HalfT(scale = 2)),
                  error = Error(scale = HalfT(scale = 2)))
    expect_is(object, "SpecDLMNoTrendNormZeroWithSeason")
    expect_identical(checkPriorIsInformative(object),
                     NULL)
    object <- DLM(trend = NULL,
                  season = Season(n = 4, scale = HalfT(scale = 2)),
                  error = Error(scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'level' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  season = Season(n = 4),
                  trend = NULL,
                  error = Error(scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'season' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  season = Season(n = 4, scale = HalfT(mult = 3)),
                  trend = NULL,
                  error = Error(scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'mult' supplied in call to 'HalfT' when specifying 'season' in call to 'DLM'")
})


test_that("checkPriorIsInformative works with SpecDLMWithTrendNormZeroWithSeason", {
    checkPriorIsInformative <- demest:::checkPriorIsInformative
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = Trend(initial = Initial(sd = 1),
                                scale = HalfT(scale = 2)),
                  season = Season(n = 4,
                                  scale = HalfT(scale = 2)),
                  error = Error(scale = HalfT(scale = 2)))
    expect_is(object, "SpecDLMWithTrendNormZeroWithSeason")
    expect_identical(checkPriorIsInformative(object),
                     NULL)
    object <- DLM(level = Level(scale = HalfT(mult = 2)),
                  trend = Trend(initial = Initial(sd = 1),
                                scale = HalfT(scale = 2)),
                  error = Error(scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'mult' supplied in call to 'HalfT' when specifying 'level' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = Trend(scale = HalfT(scale = 2)),
                  error = Error(scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'sd' not supplied in call to 'Initial' when specifying 'trend' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = Trend(initial = Initial(sd = 1),
                                scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'error' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = Trend(initial = Initial(sd = 1),
                                scale = HalfT(scale = 2)),
                  season = Season(n = 4, scale = HalfT(mult = 3)),
                  error = Error(scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'mult' supplied in call to 'HalfT' when specifying 'season' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = Trend(initial = Initial(sd = 1),
                                scale = HalfT(scale = 2)),
                  season = Season(n = 4),
                  error = Error(scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'season' in call to 'DLM'")
})

test_that("checkPriorIsInformative works with SpecDLMNoTrendNormCovNoSeason", {
    checkPriorIsInformative <- demest:::checkPriorIsInformative
    data <- data.frame(time = 2001:2010,
                       has.policy = rep(0:1, each = 5))
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = NULL,
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10),
                                          coef = TDist(scale = 0.1)),
                  error = Error(scale = HalfT(scale = 2)))
    expect_is(object, "SpecDLMNoTrendNormCovNoSeason")
    expect_identical(checkPriorIsInformative(object),
                     NULL)
    object <- DLM(level = Level(scale = HalfT(mult = 2)),
                  trend = NULL,
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10),
                                          coef = TDist(scale = 0.1)),
                  error = Error(scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'mult' supplied in call to 'HalfT' when specifying 'level' in call to 'DLM'")
    object <- DLM(trend = NULL,
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10),
                                          coef = TDist(scale = 0.1)),
                  error = Error(scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'level' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = NULL,
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10),
                                          coef = TDist(scale = 0.1)),
                  error = Error(scale = HalfT(mult = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'mult' supplied in call to 'HalfT' when specifying 'error' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = NULL,
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          coef = TDist(scale = 0.1)),
                  error = Error(scale = HalfT(scale = 1)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'sd' not supplied in call to 'Norm' when specifying 'covariates' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = NULL,
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10)),
                  error = Error(scale = HalfT(scale = 1)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'scale' not supplied in call to 'TDist' when specifying 'covariates' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10),
                                          coef = TDist(scale = 0.1)),
                  trend = NULL)
    expect_identical(checkPriorIsInformative(object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'error' in call to 'DLM'")
})

test_that("checkPriorIsInformative works with SpecDLMWithTrendNormCovNoSeason", {
    checkPriorIsInformative <- demest:::checkPriorIsInformative
    data <- data.frame(time = 2001:2010,
                       has.policy = rep(0:1, each = 5))
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = Trend(initial = Initial(sd = 1),
                                scale = HalfT(scale = 2)),,
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10),
                                          coef = TDist(scale = 0.1)),
                  error = Error(scale = HalfT(scale = 2)))
    expect_is(object, "SpecDLMWithTrendNormCovNoSeason")
    expect_identical(checkPriorIsInformative(object),
                     NULL)
    object <- DLM(level = Level(scale = HalfT(mult = 2)),
                  trend = Trend(initial = Initial(sd = 1),
                                scale = HalfT(scale = 2)),
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10),
                                          coef = TDist(scale = 0.1)),
                  error = Error(scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'mult' supplied in call to 'HalfT' when specifying 'level' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = Trend(scale = HalfT(scale = 2)),
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10),
                                          coef = TDist(scale = 0.1)),
                  error = Error(scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'sd' not supplied in call to 'Initial' when specifying 'trend' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = Trend(initial = Initial(sd = 1),
                                scale = HalfT(scale = 2)),
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          coef = TDist(scale = 0.1)),
                  error = Error(scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'sd' not supplied in call to 'Norm' when specifying 'covariates' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = Trend(initial = Initial(sd = 1),
                                scale = HalfT(scale = 2)),
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10),
                                          coef = TDist(scale = 0.1)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'error' in call to 'DLM'")
})

test_that("checkPriorIsInformative works with SpecDLMNoTrendNormCovWithSeason", {
    checkPriorIsInformative <- demest:::checkPriorIsInformative
    data <- data.frame(time = 2001:2010,
                       has.policy = rep(0:1, each = 5))
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = NULL,
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10),
                                          coef = TDist(scale = 0.1)),
                  season = Season(n = 4, scale = HalfT(scale = 2)),
                  error = Error(scale = HalfT(scale = 2)))
    expect_is(object, "SpecDLMNoTrendNormCovWithSeason")
    expect_identical(checkPriorIsInformative(object),
                     NULL)
    object <- DLM(trend = NULL,
                  season = Season(n = 4, scale = HalfT(scale = 2)),
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10),
                                          coef = TDist(scale = 0.1)),
                  error = Error(scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'level' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  season = Season(n = 4),
                  trend = NULL,
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10),
                                          coef = TDist(scale = 0.1)),
                  error = Error(scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'season' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  season = Season(n = 4, scale = HalfT(scale = 0.1)),
                  trend = NULL,
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10)),
                  error = Error(scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'scale' not supplied in call to 'TDist' when specifying 'covariates' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  season = Season(n = 4, scale = HalfT(mult = 3)),
                  trend = NULL,
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10),
                                          coef = TDist(scale = 0.1)),
                  error = Error(scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'mult' supplied in call to 'HalfT' when specifying 'season' in call to 'DLM'")
})


test_that("checkPriorIsInformative works with SpecDLMWithTrendNormCovWithSeason", {
    checkPriorIsInformative <- demest:::checkPriorIsInformative
    data <- data.frame(time = 2001:2010,
                       has.policy = rep(0:1, each = 5))
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = Trend(initial = Initial(sd = 1),
                                scale = HalfT(scale = 2)),
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10),
                                          coef = TDist(scale = 0.1)),
                  season = Season(n = 4,
                                  scale = HalfT(scale = 2)),
                  error = Error(scale = HalfT(scale = 2)))
    expect_is(object, "SpecDLMWithTrendNormCovWithSeason")
    expect_identical(checkPriorIsInformative(object),
                     NULL)
    object <- DLM(level = Level(scale = HalfT(mult = 2)),
                  trend = Trend(initial = Initial(sd = 1),
                                scale = HalfT(scale = 2)),
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10),
                                          coef = TDist(scale = 0.1)),
                  season = Season(n = 4,
                                  scale = HalfT(scale = 2)),
                  error = Error(scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'mult' supplied in call to 'HalfT' when specifying 'level' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = Trend(scale = HalfT(scale = 2)),
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10),
                                          coef = TDist(scale = 0.1)),
                  season = Season(n = 4,
                                  scale = HalfT(scale = 2)),
                  error = Error(scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'sd' not supplied in call to 'Initial' when specifying 'trend' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10),
                                          coef = TDist(scale = 0.1)),
                  trend = Trend(initial = Initial(sd = 1),
                                scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'error' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = Trend(initial = Initial(sd = 1),
                                scale = HalfT(scale = 2)),
                  season = Season(n = 4, scale = HalfT(mult = 3)),
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10),
                                          coef = TDist(scale = 0.1)),
                  error = Error(scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'mult' supplied in call to 'HalfT' when specifying 'season' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = Trend(initial = Initial(sd = 1),
                                scale = HalfT(scale = 2)),
                  season = Season(n = 4),
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10),
                                          coef = TDist(scale = 0.1)),
                  error = Error(scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'season' in call to 'DLM'")
})

test_that("checkPriorIsInformative works with SpecDLMNoTrendRobustZeroNoSeason", {
    checkPriorIsInformative <- demest:::checkPriorIsInformative
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = NULL,
                  error = Error(robust = TRUE, scale = HalfT(scale = 2)))
    expect_is(object, "SpecDLMNoTrendRobustZeroNoSeason")
    expect_identical(checkPriorIsInformative(object),
                     NULL)
    object <- DLM(level = Level(scale = HalfT(mult = 2)),
                  trend = NULL,
                  error = Error(robust = TRUE, scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'mult' supplied in call to 'HalfT' when specifying 'level' in call to 'DLM'")
    object <- DLM(trend = NULL,
                  error = Error(robust = TRUE, scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'level' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = NULL,
                  error = Error(robust = TRUE, scale = HalfT(mult = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'mult' supplied in call to 'HalfT' when specifying 'error' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = NULL,
                  error = Error(robust = TRUE))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'error' in call to 'DLM'")
})

test_that("checkPriorIsInformative works with SpecDLMWithTrendRobustZeroNoSeason", {
    checkPriorIsInformative <- demest:::checkPriorIsInformative
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = Trend(initial = Initial(sd = 1),
                                scale = HalfT(scale = 2)),
                  error = Error(robust = TRUE, scale = HalfT(scale = 2)))
    expect_is(object, "SpecDLMWithTrendRobustZeroNoSeason")
    expect_identical(checkPriorIsInformative(object),
                     NULL)
    object <- DLM(level = Level(scale = HalfT(mult = 2)),
                  trend = Trend(initial = Initial(sd = 1),
                                scale = HalfT(scale = 2)),
                  error = Error(robust = TRUE, scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'mult' supplied in call to 'HalfT' when specifying 'level' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = Trend(scale = HalfT(scale = 2)),
                  error = Error(robust = TRUE, scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'sd' not supplied in call to 'Initial' when specifying 'trend' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = Trend(initial = Initial(sd = 1),
                                scale = HalfT(scale = 2)),
                  error = Error(robust = TRUE))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'error' in call to 'DLM'")
})

test_that("checkPriorIsInformative works with SpecDLMNoTrendRobustZeroWithSeason", {
    checkPriorIsInformative <- demest:::checkPriorIsInformative
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = NULL,
                  season = Season(n = 4, scale = HalfT(scale = 2)),
                  error = Error(robust = TRUE, scale = HalfT(scale = 2)))
    expect_is(object, "SpecDLMNoTrendRobustZeroWithSeason")
    expect_identical(checkPriorIsInformative(object),
                     NULL)
    object <- DLM(trend = NULL,
                  season = Season(n = 4, scale = HalfT(scale = 2)),
                  error = Error(robust = TRUE, scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'level' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  season = Season(n = 4),
                  trend = NULL,
                  error = Error(robust = TRUE, scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'season' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  season = Season(n = 4, scale = HalfT(mult = 3)),
                  trend = NULL,
                  error = Error(robust = TRUE, scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'mult' supplied in call to 'HalfT' when specifying 'season' in call to 'DLM'")
})

test_that("checkPriorIsInformative works with SpecDLMWithTrendRobustZeroWithSeason", {
    checkPriorIsInformative <- demest:::checkPriorIsInformative
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = Trend(initial = Initial(sd = 1),
                                scale = HalfT(scale = 2)),
                  season = Season(n = 4,
                                  scale = HalfT(scale = 2)),
                  error = Error(robust = TRUE, scale = HalfT(scale = 2)))
    expect_is(object, "SpecDLMWithTrendRobustZeroWithSeason")
    expect_identical(checkPriorIsInformative(object),
                     NULL)
    object <- DLM(level = Level(scale = HalfT(mult = 2)),
                  trend = Trend(initial = Initial(sd = 1),
                                scale = HalfT(scale = 2)),
                  error = Error(robust = TRUE, scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'mult' supplied in call to 'HalfT' when specifying 'level' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = Trend(scale = HalfT(scale = 2)),
                  error = Error(robust = TRUE, scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'sd' not supplied in call to 'Initial' when specifying 'trend' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = Trend(initial = Initial(sd = 1),
                                scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'error' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = Trend(initial = Initial(sd = 1),
                                scale = HalfT(scale = 2)),
                  season = Season(n = 4, scale = HalfT(mult = 3)),
                  error = Error(robust = TRUE, scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'mult' supplied in call to 'HalfT' when specifying 'season' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = Trend(initial = Initial(sd = 1),
                                scale = HalfT(scale = 2)),
                  season = Season(n = 4),
                  error = Error(robust = TRUE, scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'season' in call to 'DLM'")
})

test_that("checkPriorIsInformative works with SpecDLMNoTrendRobustCovNoSeason", {
    checkPriorIsInformative <- demest:::checkPriorIsInformative
    data <- data.frame(time = 2001:2010,
                       has.policy = rep(0:1, each = 5))
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = NULL,
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10),
                                          coef = TDist(scale = 0.1)),
                  error = Error(robust = TRUE, scale = HalfT(scale = 2)))
    expect_is(object, "SpecDLMNoTrendRobustCovNoSeason")
    expect_identical(checkPriorIsInformative(object),
                     NULL)
    object <- DLM(level = Level(scale = HalfT(mult = 2)),
                  trend = NULL,
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10),
                                          coef = TDist(scale = 0.1)),
                  error = Error(robust = TRUE, scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'mult' supplied in call to 'HalfT' when specifying 'level' in call to 'DLM'")
    object <- DLM(trend = NULL,
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10),
                                          coef = TDist(scale = 0.1)),
                  error = Error(robust = TRUE, scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'level' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = NULL,
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10),
                                          coef = TDist(scale = 0.1)),
                  error = Error(robust = TRUE, scale = HalfT(mult = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'mult' supplied in call to 'HalfT' when specifying 'error' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = NULL,
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          coef = TDist(scale = 0.1)),
                  error = Error(robust = TRUE, scale = HalfT(scale = 1)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'sd' not supplied in call to 'Norm' when specifying 'covariates' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = NULL,
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10)),
                  error = Error(robust = TRUE, scale = HalfT(scale = 1)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'scale' not supplied in call to 'TDist' when specifying 'covariates' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10),
                                          coef = TDist(scale = 0.1)),
                  trend = NULL,
                  error = Error(robust = TRUE))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'error' in call to 'DLM'")
})

test_that("checkPriorIsInformative works with SpecDLMWithTrendRobustCovNoSeason", {
    checkPriorIsInformative <- demest:::checkPriorIsInformative
    data <- data.frame(time = 2001:2010,
                       has.policy = rep(0:1, each = 5))
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = Trend(initial = Initial(sd = 1),
                                scale = HalfT(scale = 2)),,
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10),
                                          coef = TDist(scale = 0.1)),
                  error = Error(robust = TRUE, scale = HalfT(scale = 2)))
    expect_is(object, "SpecDLMWithTrendRobustCovNoSeason")
    expect_identical(checkPriorIsInformative(object),
                     NULL)
    object <- DLM(level = Level(scale = HalfT(mult = 2)),
                  trend = Trend(initial = Initial(sd = 1),
                                scale = HalfT(scale = 2)),
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10),
                                          coef = TDist(scale = 0.1)),
                  error = Error(robust = TRUE, scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'mult' supplied in call to 'HalfT' when specifying 'level' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = Trend(scale = HalfT(scale = 2)),
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10),
                                          coef = TDist(scale = 0.1)),
                  error = Error(robust = TRUE, scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'sd' not supplied in call to 'Initial' when specifying 'trend' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = Trend(initial = Initial(sd = 1),
                                scale = HalfT(scale = 2)),
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          coef = TDist(scale = 0.1)),
                  error = Error(robust = TRUE, scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'sd' not supplied in call to 'Norm' when specifying 'covariates' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = Trend(initial = Initial(sd = 1),
                                scale = HalfT(scale = 2)),
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10),
                                          coef = TDist(scale = 0.1)),
                  error = Error(robust = TRUE))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'error' in call to 'DLM'")
})

test_that("checkPriorIsInformative works with SpecDLMNoTrendRobustCovWithSeason", {
    checkPriorIsInformative <- demest:::checkPriorIsInformative
    data <- data.frame(time = 2001:2010,
                       has.policy = rep(0:1, each = 5))
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = NULL,
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10),
                                          coef = TDist(scale = 0.1)),
                  season = Season(n = 4, scale = HalfT(scale = 2)),
                  error = Error(robust = TRUE, scale = HalfT(scale = 2)))
    expect_is(object, "SpecDLMNoTrendRobustCovWithSeason")
    expect_identical(checkPriorIsInformative(object),
                     NULL)
    object <- DLM(trend = NULL,
                  season = Season(n = 4, scale = HalfT(scale = 2)),
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10),
                                          coef = TDist(scale = 0.1)),
                  error = Error(robust = TRUE, scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'level' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  season = Season(n = 4),
                  trend = NULL,
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10),
                                          coef = TDist(scale = 0.1)),
                  error = Error(robust = TRUE, scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'season' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  season = Season(n = 4, scale = HalfT(scale = 0.1)),
                  trend = NULL,
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10)),
                  error = Error(robust = TRUE, scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'scale' not supplied in call to 'TDist' when specifying 'covariates' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  season = Season(n = 4, scale = HalfT(mult = 3)),
                  trend = NULL,
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10),
                                          coef = TDist(scale = 0.1)),
                  error = Error(robust = TRUE, scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'mult' supplied in call to 'HalfT' when specifying 'season' in call to 'DLM'")
})

test_that("checkPriorIsInformative works with SpecDLMWithTrendRobustCovWithSeason", {
    checkPriorIsInformative <- demest:::checkPriorIsInformative
    data <- data.frame(time = 2001:2010,
                       has.policy = rep(0:1, each = 5))
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = Trend(initial = Initial(sd = 1),
                                scale = HalfT(scale = 2)),
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10),
                                          coef = TDist(scale = 0.1)),
                  season = Season(n = 4,
                                  scale = HalfT(scale = 2)),
                  error = Error(robust = TRUE, scale = HalfT(scale = 2)))
    expect_is(object, "SpecDLMWithTrendRobustCovWithSeason")
    expect_identical(checkPriorIsInformative(object),
                     NULL)
    object <- DLM(level = Level(scale = HalfT(mult = 2)),
                  trend = Trend(initial = Initial(sd = 1),
                                scale = HalfT(scale = 2)),
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10),
                                          coef = TDist(scale = 0.1)),
                  season = Season(n = 4,
                                  scale = HalfT(scale = 2)),
                  error = Error(robust = TRUE, scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'mult' supplied in call to 'HalfT' when specifying 'level' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = Trend(scale = HalfT(scale = 2)),
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10),
                                          coef = TDist(scale = 0.1)),
                  season = Season(n = 4,
                                  scale = HalfT(scale = 2)),
                  error = Error(robust = TRUE, scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'sd' not supplied in call to 'Initial' when specifying 'trend' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10),
                                          coef = TDist(scale = 0.1)),
                  trend = Trend(initial = Initial(sd = 1),
                                scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'error' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = Trend(initial = Initial(sd = 1),
                                scale = HalfT(scale = 2)),
                  season = Season(n = 4, scale = HalfT(mult = 3)),
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10),
                                          coef = TDist(scale = 0.1)),
                  error = Error(robust = TRUE, scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'mult' supplied in call to 'HalfT' when specifying 'season' in call to 'DLM'")
    object <- DLM(level = Level(scale = HalfT(scale = 2)),
                  trend = Trend(initial = Initial(sd = 1),
                                scale = HalfT(scale = 2)),
                  season = Season(n = 4),
                  covariates = Covariates(mean ~ has.policy,
                                          data = data,
                                          intercept = Norm(sd = 10),
                                          coef = TDist(scale = 0.1)),
                  error = Error(robust = TRUE, scale = HalfT(scale = 2)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'season' in call to 'DLM'")
})

test_that("checkPriorIsInformative works with SpecKnown", {
    checkPriorIsInformative <- demest:::checkPriorIsInformative
    object <- Known(mean = ValuesOne(1:5, labels = 1:5, name = "region"),
                    sd = 1)
    expect_is(object, "SpecKnownUncertain")
    expect_identical(checkPriorIsInformative(object),
                     NULL)
})

test_that("checkPriorIsInformative works with SpecMix", {
    checkPriorIsInformative <- demest:::checkPriorIsInformative
    object <- Mix(components = Components(scale = HalfT(scale = 0.1)),
                  weights = Weights(scale1 = HalfT(scale = 0.1),
                                    scale2 = HalfT(scale = 0.1)),
                  error = Error(scale = HalfT(scale = 0.1)))
    expect_is(object, "SpecMixNormZero")
    expect_identical(checkPriorIsInformative(object),
                     NULL)
    object <- Mix(components = Components(scale = HalfT(mult = 0.1)),
                  weights = Weights(scale1 = HalfT(scale = 0.1),
                                    scale2 = HalfT(scale = 0.1)),
                  error = Error(scale = HalfT(scale = 0.1)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'mult' supplied in call to 'HalfT' when specifying 'components' in call to 'Mix'")
    object <- Mix(components = Components(),
                  weights = Weights(scale1 = HalfT(scale = 0.1),
                                    scale2 = HalfT(scale = 0.1)),
                  error = Error(scale = HalfT(scale = 0.1)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'components' in call to 'Mix'")
    object <- Mix(components = Components(scale = HalfT(scale = 0.1)),
                  weights = Weights(scale1 = HalfT(mult = 0.1),
                                    scale2 = HalfT(scale = 0.1)),
                  error = Error(scale = HalfT(scale = 0.1)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'mult' supplied in call to 'HalfT' when specifying 'scale1' for 'weights' in call to 'Mix'")
    object <- Mix(components = Components(scale = HalfT(scale = 0.1)),
                  weights = Weights(scale1 = HalfT(scale = 0.1)),
                  error = Error(scale = HalfT(scale = 0.1)))
    expect_identical(checkPriorIsInformative(object),
                     "value for 'scale' not supplied in call to 'HalfT' when specifying 'scale2' for 'weights' in call to 'Mix'")
})



test_that("checkPriorIsInformative works with SpecZero", {
    checkPriorIsInformative <- demest:::checkPriorIsInformative
    object <- Zero()
    expect_is(object, "SpecZero")
    expect_identical(checkPriorIsInformative(object),
                     NULL)
})












## show ########################################################

if (FALSE) {
    
    test_that("show works with Covariates", {
        data <- data.frame(region = letters[1:10],
                           income = rnorm(10),
                           area = rep(c("u", "r"), each = 5))
        covariates <- Covariates(mean ~ income + area,
                                 data = data)
        show(covariates)
        covariates <- Covariates(mean ~ income + area,
                                 data = data,
                                 contrastsArg = list(area = diag(2)))
        show(covariates)
        covariates <- Covariates(mean ~ income + area,
                                 data = data,
                                 contrastsArg = list(area = diag(2)),
                                 intercept = Norm(scale = 2),
                                 coef = HalfT(df = 4, scale = 2, max = 5))
        show(covariates)
    })

    test_that("show works with ErrorNorm", {
        error <- Error()
        show(error)
        error <- Error(scale = HalfT(scale = 3, max = 5))
        show(error)
    })

    test_that("show works with ErrorRobust", {
        error <- Error(robust = TRUE)
        show(error)
        error <- Error(robust = TRUE, df = 10, scale = HalfT(scale = 3, max = 5))
        show(error)
    })

}
