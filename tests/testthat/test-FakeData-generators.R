
## context("FakeData-generators")

## n.test <- 5
## test.identity <- FALSE
## test.extended <- TRUE



## ## fakeData ##############################################################################

## test_that("fakeData works with PoissonVarying, no exposure", {
##     rinvchisq1 <- demest:::rinvchisq1
##     mean.sex <- Values(array(c(-1, 1), dim = 2, dimnames = list(sex = c("f", "m"))))
##     intercept <- 2
##     model <- Model(y ~ Poisson(mean ~ age + sex,
##                                exposure = FALSE,
##                                priorSD = list(df = 5, scale = 5)),
##                    age ~ Exch(sd = 1),
##                    sex ~ Known(mean = mean.sex))
##     template.y <- Counts(array(0,
##                                dim = c(10, 2),
##                                dimnames = list(age = 0:9, sex = c("f", "m"))))
##     set.seed(1)
##     ans.obtained <- fakeData(model = model,
##                              intercept = intercept,
##                              templateY = template.y)
##     set.seed(1)
##     beta.age <- rnorm(n = 10, sd = 1)
##     beta.age <- beta.age - mean(beta.age)
##     beta.sex <- c(-1, 1)
##     sigma <- rinvchisq1(df = 5, scale = 5)
##     mu <- intercept + beta.age + rep(beta.sex, each = 10)
##     log.theta <- rnorm(n = 20, mean = mu, sd = sigma)
##     theta <- exp(log.theta)
##     y <- rpois(n = 20, lambda = theta)
##     y <- Counts(array(y, dim = dim(template.y), dimnames = dimnames(template.y)))
##     theta <- Counts(array(theta, dim = dim(template.y), dimnames = dimnames(template.y)))
##     beta.age <- Values(array(beta.age, dim = 10, dimnames = list(age = 0:9)))
##     beta.sex <- Values(array(beta.sex, dim = 2, dimnames = list(sex = c("f", "m"))))
##     betas <- list("(Intercept)" = intercept, age = beta.age, sex = beta.sex)
##     ans.expected <- new("FakeData",
##                         call = call("Model", formula = y ~ Poisson(mean ~ age + sex, exposure = FALSE, priorSD = list(df = 5, scale = 5)),
##                         age ~ Exch(sd = 1), sex ~ Known(mean = mean.sex)),
##                         y = y,
##                         model = list(likelihood = list(mean = theta),
##                         prior = list(param = betas, sd = sigma)))
##     expect_equal(ans.obtained, ans.expected)
##     exposure <- Counts(array(rpois(n = 20, lambda = 100),
##                              dim = c(10, 2), dimnames = list(age = 0:9, sex = c("f", "m"))))
##     expect_error(fakeData(model = model, exposure = exposure,
##                           intercept = intercept, templateY = template.y),
##                  "model does not include exposure term, but 'exposure' argument supplied")
##     weights <- Counts(array(rpois(n = 20, lambda = 100),
##                             dim = c(10, 2), dimnames = list(age = 0:9, sex = c("f", "m"))))
##     expect_warning(fakeData(model = model, weights = weights, intercept = intercept, templateY = template.y),
##                    "'weights' argument ignored when distribution is Poisson")
## })

## test_that("fakeData works with PoissonVarying, no exposure - Poly prior for age", {
##     rinvchisq1 <- demest:::rinvchisq1
##     fakeBeta <- demest:::fakeBeta
##     mean.sex <- Values(array(c(-1, 1), dim = 2, dimnames = list(sex = c("f", "m"))))
##     intercept <- 2
##     model <- Model(y ~ Poisson(mean ~ age + sex, exposure = FALSE, priorSD = list(df = 5, scale = 5)),
##                    age ~ Poly(order = 2, sdObs = 1, sdTrend = c(0.3, 0.1),
##                               priorMeanTrend = c(0, 0.1),
##                               priorVarTrend = diag(2)),
##                    sex ~ Known(mean = mean.sex))
##     template.y <- Counts(array(0, dim = c(10, 2), dimnames = list(age = 0:9, sex = c("f", "m"))))
##     set.seed(1)
##     ans.obtained <- fakeData(model = model, intercept = intercept, templateY = template.y)
##     set.seed(1)
##     spec.age <- new("SpecPoly",
##                     trend = new("SpecPolyComponentTrend",
##                         q = 2L,
##                         priorsW = list(new("SpecPriorVarDLMNormKnown", tau = 0.3),
##                             new("SpecPriorVarDLMNormKnown", tau = 0.1)),
##                         m0 = c(0, 0.1),
##                         C0 = diag(2)),
##                     priorV = new("SpecPriorVarDLMNormKnown", tau = 1))
##     beta.age <- fakeBeta(object = spec.age, metadata = template.y@metadata[1])
##     beta.sex <- c(-1, 1)
##     sigma <- rinvchisq1(df = 5, scale = 5)
##     mu <- intercept + beta.age + rep(beta.sex, each = 10)
##     log.theta <- rnorm(n = 20, mean = mu, sd = sigma)
##     theta <- exp(log.theta)
##     y <- rpois(n = 20, lambda = theta)
##     y <- Counts(array(y, dim = dim(template.y), dimnames = dimnames(template.y)))
##     theta <- Counts(array(theta, dim = dim(template.y), dimnames = dimnames(template.y)))
##     beta.age <- Values(array(beta.age, dim = 10, dimnames = list(age = 0:9)))
##     beta.sex <- Values(array(beta.sex, dim = 2, dimnames = list(sex = c("f", "m"))))
##     betas <- list("(Intercept)" = intercept, age = beta.age, sex = beta.sex)
##     ans.expected <- new("FakeData",
##                         call = call("Model",
##                             formula = y ~ Poisson(mean ~ age + sex, exposure = FALSE, priorSD = list(df = 5, scale = 5)),
##                             age ~ Poly(order = 2, sdObs = 1, sdTrend = c(0.3, 0.1),
##                                        priorMeanTrend = c(0, 0.1),
##                                        priorVarTrend = diag(2)),
##                             sex ~ Known(mean = mean.sex)),
##                         y = y,
##                         model = list(likelihood = list(mean = theta),
##                             prior = list(param = betas, sd = sigma)))
##     expect_equal(ans.obtained, ans.expected)
##     exposure <- Counts(array(rpois(n = 20, lambda = 100),
##                              dim = c(10, 2), dimnames = list(age = 0:9, sex = c("f", "m"))))
##     expect_error(fakeData(model = model, exposure = exposure,
##                           intercept = intercept, templateY = template.y),
##                  "model does not include exposure term, but 'exposure' argument supplied")
##     weights <- Counts(array(rpois(n = 20, lambda = 100),
##                             dim = c(10, 2), dimnames = list(age = 0:9, sex = c("f", "m"))))
##     expect_warning(fakeData(model = model, weights = weights, intercept = intercept, templateY = template.y),
##                    "'weights' argument ignored when distribution is Poisson")
## })

## test_that("fakeData works with PoissonVarying, uses exposure", {
##     rinvchisq1 <- demest:::rinvchisq1
##     mean.sex <- Values(array(c(-1, 1), dim = 2, dimnames = list(sex = c("f", "m"))))
##     intercept <- -1
##     model <- Model(y ~ Poisson(mean ~ age + sex, priorSD = list(df = 5, scale = 5)),
##                    age ~ Exch(sd = 1),
##                    sex ~ Known(mean = mean.sex))
##     exposure <- Counts(array(rpois(n = 20, lambda = 100),
##                              dim = c(10, 2), dimnames = list(age = 0:9, sex = c("f", "m"))))
##     template.y <- Counts(array(0, dim = c(10, 2), dimnames = list(age = 0:9, sex = c("f", "m"))))
##     set.seed(1)
##     ans.obtained <- fakeData(model = model, intercept = intercept, exposure = exposure, templateY = template.y)
##     set.seed(1)
##     beta.age <- rnorm(n = 10, sd = 1)
##     beta.age <- beta.age - mean(beta.age)
##     beta.sex <- c(-1, 1)
##     sigma <- rinvchisq1(df = 5, scale = 5)
##     mu <- intercept + beta.age + rep(beta.sex, each = 10)
##     log.theta <- rnorm(n = 20, mean = mu, sd = sigma)
##     theta <- exp(log.theta)
##     y <- rpois(n = 20, lambda = theta * exposure)
##     y <- Counts(array(y, dim = dim(template.y), dimnames = dimnames(template.y)))
##     theta <- Values(array(theta, dim = dim(template.y), dimnames = dimnames(template.y)))
##     beta.age <- Values(array(beta.age, dim = 10, dimnames = list(age = 0:9)))
##     beta.sex <- Values(array(beta.sex, dim = 2, dimnames = list(sex = c("f", "m"))))
##     betas <- list("(Intercept)" = intercept, age = beta.age, sex = beta.sex)
##     ans.expected <- new("FakeDataExposure",
##                         call = call("Model", formula = y ~ Poisson(mean ~ age + sex, priorSD = list(df = 5, scale = 5)),
##                         age ~ Exch(sd = 1),
##                         sex ~ Known(mean = mean.sex)),
##                         y = y,
##                         exposure = exposure,
##                         model = list(likelihood = list(mean = theta),
##                         prior = list(param = betas, sd = sigma)))
##     expect_equal(ans.obtained, ans.expected)
##     expect_error(fakeData(model = model, intercept = intercept, templateY = template.y),
##                  "model includes exposure term, but no 'exposure' argument supplied")
##     weights <- Counts(array(rpois(n = 20, lambda = 100),
##                             dim = c(10, 2), dimnames = list(age = 0:9, sex = c("f", "m"))))
##     expect_warning(fakeData(model = model, exposure = exposure,
##                             weights = weights, intercept = intercept, templateY = template.y),
##                    "'weights' argument ignored when distribution is Poisson")
## })

## test_that("fakeData throws error when passed SpecPoissonVaryingBench model", {
##     benchmarks <- Benchmarks(mean = 2)
##     intercept <- -1
##     mean.sex <- ValuesOne(values = 1:2, labels = c("f", "m"), name = "sex")
##     model <- Model(y ~ Poisson(mean ~ age + sex, priorSD = list(df = 5, scale = 5),
##                                benchmarks = benchmarks),
##                    age ~ Exch(sd = 1),
##                    sex ~ Known(mean = mean.sex))
##     exposure <- Counts(array(rpois(n = 20, lambda = 100),
##                              dim = c(10, 2), dimnames = list(age = 0:9, sex = c("f", "m"))))
##     template.y <- Counts(array(0, dim = c(10, 2), dimnames = list(age = 0:9, sex = c("f", "m"))))
##     expect_error(fakeData(model = model, intercept = intercept,
##                           exposure = exposure, templateY = template.y),
##                  "cannot generate fake data from benchmarked model")
## })

## test_that("fakeData works with BinomialVarying", {
##     rinvchisq1 <- demest:::rinvchisq1
##     mean.sex <- Values(array(c(-1, 1), dim = 2, dimnames = list(sex = c("f", "m"))))
##     intercept <- -1
##     model <- Model(y ~ Binomial(mean ~ age + sex,
##                                 priorSD = list(df = 5, scale = 5)),
##                    age ~ Exch(sd = 1),
##                    sex ~ Known(mean = mean.sex))
##     exposure <- Counts(array(rpois(n = 20, lambda = 100),
##                              dim = c(10, 2), dimnames = list(age = 0:9, sex = c("f", "m"))))
##     template.y <- Counts(array(0, dim = c(10, 2), dimnames = list(age = 0:9, sex = c("f", "m"))))
##     set.seed(1)
##     ans.obtained <- fakeData(model = model, intercept = intercept, exposure = exposure, templateY = template.y)
##     set.seed(1)
##     beta.age <- rnorm(n = 10, sd = 1)
##     beta.age <- beta.age - mean(beta.age)
##     beta.sex <- c(-1, 1)
##     sigma <- rinvchisq1(df = 5, scale = 5)
##     mu <- intercept + beta.age + rep(beta.sex, each = 10)
##     logit.theta <- rnorm(n = 20, mean = mu, sd = sigma)
##     theta <- exp(logit.theta) / (1 + exp(logit.theta))
##     y <- rbinom(n = 20, size = exposure, prob = theta)
##     y <- Counts(array(y, dim = dim(template.y), dimnames = dimnames(template.y)))
##     theta <- Values(array(theta, dim = dim(template.y), dimnames = dimnames(template.y)))
##     beta.age <- Values(array(beta.age, dim = 10, dimnames = list(age = 0:9)))
##     beta.sex <- Values(array(beta.sex, dim = 2, dimnames = list(sex = c("f", "m"))))
##     betas <- list("(Intercept)" = intercept, age = beta.age, sex = beta.sex)
##     ans.expected <- new("FakeDataExposure",
##                         call = call("Model", formula = y ~ Binomial(mean ~ age + sex, priorSD = list(df = 5, scale = 5)),
##                         age ~ Exch(sd = 1),
##                         sex ~ Known(mean = mean.sex)),
##                         y = y,
##                         exposure = exposure,
##                         model = list(likelihood = list(prob = theta),
##                         prior = list(param = betas, sd = sigma)))
##     expect_equal(ans.obtained, ans.expected)
##     expect_error(fakeData(model = model, intercept = intercept, templateY = template.y),
##                  "'exposure' cannot be NULL in a binomial model")
##     exposure.wrong <- exposure + 0.1
##     expect_error(fakeData(model = model, intercept = intercept,
##                           exposure = exposure.wrong, templateY = template.y),
##                  "'exposure' has non-integer values")
##     weights <- Counts(array(rpois(n = 20, lambda = 100),
##                             dim = c(10, 2), dimnames = list(age = 0:9, sex = c("f", "m"))))
##     expect_warning(fakeData(model = model, exposure = exposure,
##                             weights = weights, intercept = intercept, templateY = template.y),
##                    "'weights' argument ignored when distribution is Binomial")
## })

## test_that("fakeData throws error when passed SpecPoissonVaryingBench model", {
##     benchmarks <- Benchmarks(mean = 0.5)
##     intercept <- -1
##     mean.sex <- ValuesOne(values = c(0.1, 0.2), labels = c("f", "m"), name = "sex")
##     model <- Model(y ~ Binomial(mean ~ age + sex, priorSD = list(df = 5, scale = 5),
##                                 benchmarks = benchmarks),
##                    age ~ Exch(sd = 1),
##                    sex ~ Known(mean = mean.sex))
##     exposure <- Counts(array(rpois(n = 20, lambda = 100),
##                              dim = c(10, 2), dimnames = list(age = 0:9, sex = c("f", "m"))))
##     template.y <- Counts(array(0, dim = c(10, 2), dimnames = list(age = 0:9, sex = c("f", "m"))))
##     expect_error(fakeData(model = model, intercept = intercept,
##                           exposure = exposure, templateY = template.y),
##                  "cannot generate fake data from benchmarked model")
## })


## ## test_that("fakeData works with NormalVarying", {
## ##     rinvchisq1 <- demest:::rinvchisq1
## ##     mean.sex <- ValuesOne(values = c(-1, 1), labels = c("f", "m"), name = "sex")
## ##     intercept <- -1
## ##     model <- Model(y ~ Normal(mean ~ age + sex,
## ##                               sd = 0.5,
## ##                               priorSD = list(df = 5, scale = 5)),
## ##                    age ~ Exch(sd = 1),
## ##                    sex ~ Known(mean = mean.sex))
## ##     y.init <- Counts(array(0, dim = c(10, 2), dimnames = list(age = 0:9, sex = c("f", "m"))))
## ##     ## no weights supplied
## ##     set.seed(1)
## ##     ans.obtained <- fakeData(model = model, intercept = intercept, exposure = exposure, y = y.init)
## ##     set.seed(1)
## ##     beta.age <- rnorm(n = 10, sd = 1)
## ##     beta.age <- beta.age - mean(beta.age)
## ##     beta.sex <- c(-1, 1)
## ##     sigma <- rinvchisq1(df = 5, scale = 5)
## ##     theta <- intercept + beta.age + rep(beta.sex, each = 10)
## ##     y <- rnorm(n = 20, mean = theta, sd = model@varsigma)
## ##     y <- Counts(array(y, dim = dim(y.init), dimnames = dimnames(y.init)))
## ##     theta <- Values(array(theta, dim = dim(y.init), dimnames = dimnames(y.init)))
## ##     beta.age <- Values(array(beta.age, dim = 10, dimnames = list(age = 0:9)))
## ##     beta.sex <- Values(array(beta.sex, dim = 2, dimnames = list(sex = c("f", "m"))))
## ##     betas <- list("(Intercept)" = intercept, age = beta.age, sex = beta.sex)
## ##     ans.expected <- new("FakeDataExposure",
## ##                         y = y,
## ##                         exposure = exposure,
## ##                         model = list(likelihood = list(prob = theta,
## ##                                      sd = model@varsigma),
## ##                         prior = list(param = betas, sd = sigma)))
## ##     expect_identical(ans.obtained, ans.expected)
## ##     ## weights supplied
## ##     expect_error(fakeData(model = model, intercept = intercept, y = y.init),
## ##                  "'exposure' cannot be NULL in a binomial model")
## ##     exposure.wrong <- exposure + 0.1
## ##     expect_error(fakeData(model = model, intercept = intercept,
## ##                           exposure = exposure.wrong, y = y.init),
## ##                  "'exposure' has non-integer values")
## ##     weights <- Counts(array(rpois(n = 20, lambda = 100),
## ##                             dim = c(10, 2), dimnames = list(age = 0:9, sex = c("f", "m"))))
## ##     expect_warning(fakeData(model = model, exposure = exposure,
## ##                             weights = weights, intercept = intercept, y = y.init),
## ##                    "'weights' argument ignored when distribution is Binomial")
## ## })


