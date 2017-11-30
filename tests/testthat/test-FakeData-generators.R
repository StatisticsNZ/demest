
context("FakeData-generators")

n.test <- 5
test.identity <- FALSE
test.extended <- TRUE



## fakeModel ##############################################################################

test_that("fakeModel works with PoissonVarying, no exposure", {
    model <- Model(y ~ Poisson(mean ~ age + sex, useExpose = FALSE),
                   `(Intercept)` ~ ExchFixed(mean = -1, sd = 0.1),
                   age ~ DLM(level = Level(scale = HalfT(scale = 0.1)),
                             trend = NULL,
                             error = Error(scale = HalfT(scale = 0.1))),
                   sex ~ ExchFixed(sd = 0.2),
                   priorSD = HalfT(scale = 0.2))
    template.y <- Counts(array(0,
                               dim = c(10, 2),
                               dimnames = list(age = 0:9, sex = c("f", "m"))),
                         dimscales = c(age = "Intervals"))
    ans.obtained <- fakeModel(model,
                              templateY = template.y)
    expect_is(ans.obtained, "FakeModel")
    expose <- Counts(array(1,
                           dim = c(10, 2),
                           dimnames = list(age = 0:9, sex = c("f", "m"))),
                     dimscales = c(age = "Intervals"))
    expect_error(fakeModel(model = model,
                           exposure = expose,
                           templateY = template.y),
                 "'exposure' argument supplied")
})

test_that("fakeModel works with PoissonVarying, has exposure", {
    model <- Model(y ~ Poisson(mean ~ age + sex),
                   `(Intercept)` ~ ExchFixed(mean = -1, sd = 0.1),
                   age ~ DLM(level = Level(scale = HalfT(scale = 0.1)),
                             trend = NULL,
                             error = Error(scale = HalfT(scale = 0.1))),
                   sex ~ ExchFixed(sd = 0.2),
                   priorSD = HalfT(scale = 0.2))
    template.y <- Counts(array(0,
                               dim = c(10, 2),
                               dimnames = list(age = 0:9, sex = c("f", "m"))),
                         dimscales = c(age = "Intervals"))
    expose <- Counts(array(1,
                           dim = c(10, 2),
                           dimnames = list(age = 0:9, sex = c("f", "m"))),
                     dimscales = c(age = "Intervals"))
    ans.obtained <- fakeModel(model,
                              templateY = template.y,
                              exposure = expose)
    expect_is(ans.obtained, "FakeModelExposure")
    expect_error(fakeModel(model = model,
                           templateY = template.y),
                 "model 'y ~ Poisson\\(mean ~ age \\+ sex\\)' uses exposure")
})

