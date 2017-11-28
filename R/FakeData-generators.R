

## ## HAS_TESTS
## setMethod("fakeData",
##           signature(model = "SpecBinomialVaryingBench",
##                     templateY = "Counts"),
##           function(model, intercept, templateY, exposure = NULL, weights = NULL, uniform = TRUE) {
##               stop(gettext("cannot generate fake data from benchmarked model"))
##           })

## ## HAS_TESTS
## setMethod("fakeData",
##           signature(model = "SpecPoissonVaryingBench",
##                     templateY = "Counts"),
##           function(model, intercept, templateY, exposure = NULL, weights = NULL, uniform = TRUE) {
##               stop(gettext("cannot generate fake data from benchmarked model"))
##           })


## ## HAS_TESTS
## setMethod("fakeData",
##           signature(model = "SpecBinomialVarying",
##                     templateY = "Counts"),
##           function(model, intercept, templateY, exposure = NULL, weights = NULL, uniform = TRUE) {
##               call <- model@call
##               formula <- model@formulaMu
##               spec.priors <- model@priorsBetas
##               names.spec.priors <- model@namesPriorsBetas
##               df.prior.sigma <- model@dfPriorSigma
##               scale.prior.sigma <- model@scalePriorSigma
##               lower <- model@lower
##               upper <- model@upper
##               max.attempt <- model@maxAttempt
##               n <- length(templateY)
##               dim.y <- dim(templateY)
##               dimnames.y <- dimnames(templateY)
##               metadata.y <- templateY@metadata
##               if (is.null(exposure))
##                   stop(gettextf("'%s' cannot be %s in a binomial model",
##                                 "exposure", "NULL"))
##               exposure <- checkAndTidyExposure(exposure = exposure, y = templateY)
##               if (!isTRUE(all.equal(round(as.numeric(exposure)), as.numeric(exposure))))
##                   stop(gettextf("'%s' has non-integer values",
##                                 "exposure"))
##               if (!is.null(weights))
##                   warning(gettextf("'%s' argument ignored when distribution is %s",
##                                    "weights", "Binomial"))
##               betas <- makeFakeBetas(y = templateY,
##                                      formula = formula,
##                                      specPriors = spec.priors,
##                                      namesSpecPriors = names.spec.priors,
##                                      intercept = intercept)
##               zetas <- rep(1, times = length(betas))
##               sigma <- makeFakeSigma(dfPriorSigma = df.prior.sigma,
##                                      scalePriorSigma = scale.prior.sigma)
##               names.betas <- c("(Intercept)", names.spec.priors)
##               iterator <- makeIteratorBetas(betas = betas, namesBetas = names.betas, y = templateY)
##               mu <- makeMu(n = n,
##                            betas = betas,
##                            zetas = zetas,
##                            iterator = iterator,
##                            useC = TRUE)
##               has.limits <- (lower > 0) || (upper < 1)
##               if (has.limits) {
##                   lower <- log(lower / (1 - lower))
##                   upper <- log(upper / (1 - upper))
##                   logit.theta <- rnormTruncated(n = n,
##                                                 mean = mu,
##                                                 sd = sigma,
##                                                 lower = lower,
##                                                 upper = upper,
##                                                 maxAttempt = max.attempt,
##                                                 uniform = uniform,
##                                                 useC = TRUE)
##               }
##               else
##                   logit.theta <- stats::rnorm(n = n, mean = mu, sd = sigma)
##               theta <- exp(logit.theta) / (1 + exp(logit.theta))
##               .Data.y <- stats::rbinom(n = n, size = exposure, prob = theta)
##               .Data.y <- array(.Data.y, dim = dim.y, dimnames = dimnames.y)
##               y <- methods::new("Counts", .Data = .Data.y, metadata = metadata.y)
##               .Data.theta <- array(theta, dim = dim.y, dimnames = dimnames.y)
##               theta <- methods::new("Values", .Data = .Data.theta, metadata = metadata.y)
##               likelihood <- list(prob = theta)
##               betas <- makeFakeBetasOutput(betas = betas,
##                                            namesBetas = names.betas,
##                                            y = y)
##               prior <- list(param = betas, sd = sigma)
##               model <- list(likelihood = likelihood, prior = prior)
##               methods::new("FakeDataExposure", call = call, y = y, exposure = exposure, model = model)
##           })

## ## NO_TESTS
## setMethod("fakeData",
##           signature(model = "SpecNormalVarying",
##                     templateY = "Counts"),
##           function(model, intercept, templateY, exposure = NULL,
##                    weights = NULL, uniform = TRUE) {
##               call <- model@call
##               formula <- model@formulaMu
##               spec.priors <- model@priorsBetas
##               names.spec.priors <- model@namesPriorsBetas
##               df.prior.sigma <- model@dfPriorSigma
##               scale.prior.sigma <- model@scalePriorSigma
##               lower <- model@lower
##               upper <- model@upper
##               max.attempt <- model@maxAttempt
##               varsigma <- model@varsigma
##               if (is.null(varsigma))
##                   stop(gettextf("'%s' must be specified with function '%s'",
##                                 "likelihood.sd", "fakeData"))
##               n <- length(templateY)
##               dim.y <- dim(templateY)
##               dimnames.y <- dimnames(templateY)
##               metadata.y <- templateY@metadata
##               if (!is.null(exposure))
##                   stop(gettextf("'%s' must be %s in a normal model",
##                                 "exposure", "NULL"))
##               weights <- checkAndTidyWeights(weights = weights, y = templateY)
##               if (is.null(weights))
##                   weights <- rep(1, times = n)
##               else
##                   weights <- as.numeric(weights)
##               betas <- makeFakeBetas(y = templateY,
##                                      formula = formula,
##                                      specPriors = spec.priors,
##                                      namesSpecPriors = names.spec.priors,
##                                      intercept = intercept)
##               zetas <- rep(1, times = length(betas))
##               sigma <- makeFakeSigma(dfPriorSigma = df.prior.sigma,
##                                      scalePriorSigma = scale.prior.sigma)
##               names.betas <- c("(Intercept)", names.spec.priors)
##               iterator <- makeIteratorBetas(betas = betas, namesBetas = names.betas, y = templateY)
##               mu <- makeMu(n = n,
##                            betas = betas,
##                            zetas = zetas,
##                            iterator = iterator,
##                            useC = TRUE)
##               has.limits <- (lower > -Inf) || (upper < Inf)
##               if (has.limits) {
##                   theta <- rnormTruncated(n = n,
##                                           mean = mu,
##                                           sd = sigma,
##                                           lower = lower,
##                                           upper = upper,
##                                           maxAttempt = max.attempt,
##                                           uniform = uniform,
##                                           useC = TRUE)
##               }
##               else
##                   theta <- stats::rnorm(n = n, mean = mu, sd = sigma)
##               .Data.y <- stats::rnorm(n = n, mean = theta, sd = varsigma / weights)
##               .Data.y <- array(.Data.y, dim = dim.y, dimnames = dimnames.y)
##               y <- methods::new("Values", .Data = .Data.y, metadata = metadata.y)
##               .Data.theta <- array(theta, dim = dim.y, dimnames = dimnames.y)
##               theta <- methods::new("Values", .Data = .Data.theta, metadata = metadata.y)
##               likelihood <- list(mean = theta,
##                                  sd = varsigma)
##               betas <- makeFakeBetasOutput(betas = betas,
##                                            namesBetas = names.betas,
##                                            y = y)
##               prior <- list(param = betas, sd = sigma)
##               model <- list(likelihood = likelihood, prior = prior)
##               methods::new("FakeData", call = call, y = y, model = model)
##           })



