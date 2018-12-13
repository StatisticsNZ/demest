


## ## HAS_TESTS
## setMethod("fakeData",
##           signature(object = "SpecBinomialVarying",
##                     y = "missing",
##                     exposure = "Counts",
##                     weights = "missing"),
##           function(object, exposure, nIteration) {
##               call <- object@call
##               formula.mu <- object@formulaMu
##               specs.priors <- object@specsPriors
##               names.specs.priors <- object@namesSpecsPriors
##               lower <- object@lower
##               upper <- object@upper
##               aggregate <- object@aggregate
##               if (!methods::is(aggregate, "SpecAgPlaceholder"))
##                   stop(gettextf("'%s' argument not in call to '%s' when specifying model for fake data",
##                                 "aggregate", "Model"))
##               priorSD <- getPriorSD(object)
              

##               for (i in seq_len(nIteration)) {
                  


##               }


              
##               checkTermsFromFormulaFound(y = exposure, formula = formula.mu)
##               checkLengthDimInFormula(y = exposure, formula = formula.mu)
##               metadata <- exposure@metadata
##               dim <- dim(exposure)




##               theta <- stats::rbeta(n = length(y),
##                                     shape1 = m * (nu / sigma^2 - 1) + y.tmp,
##                                     shape2 = (1 - m) * (nu / sigma^2 - 1) + exposure.tmp - y.tmp)
##               ## need to avoid having all 'theta' equalling lower or upper bound
##               is.too.low <- theta < lower
##               n.too.low <- sum(is.too.low)
##               width <- 0.2 * (upper - lower)
##               if (is.infinite(width))
##                   width <- 100
##               theta[is.too.low] <- stats::runif(n = n.too.low, min = lower, max = lower + width)
##               is.too.high <- theta > upper
##               n.too.high <- sum(is.too.high)
##               theta[is.too.high] <- stats::runif(n = n.too.high, min = upper - width, max = upper)
##               lower <- log(lower / (1 - lower))
##               upper <- log(upper / (1 - upper))
##               if (any(!is.na(y)))
##                   scale.theta.multiplier <- median(sqrt(((exposure + 0.5) * (y + 0.5)) / (exposure - y + 0.5)),
##                                                    na.rm = TRUE)
##               else
##                   scale.theta.multiplier <- 1
##               scale.theta.multiplier <- methods::new("Scale", scale.theta.multiplier)
##               theta <- array(theta, dim = dim(y), dimnames = dimnames(y))
##               logit.theta <- log(theta / (1 - theta))
##               betas <- makeLinearBetas(theta = logit.theta, formula = formula.mu)
##               theta <- as.numeric(theta)
##               names.betas <- names(betas)
##               margins <- makeMargins(betas = betas, y = y)
##               struc.zero.array <- makeStrucZeroArray(structuralZeros = NULL,
##                                                      y = y) 
##               priors.betas <- makePriors(betas = betas,
##                                          specs = specs.priors,
##                                          namesSpecs = names.specs.priors,
##                                          margins = margins,
##                                          y = y,
##                                          sY = NULL,
##                                          strucZeroArray = struc.zero.array)
##               is.saturated <- sapply(priors.betas, function(x) x@isSaturated@.Data)
##               if (any(is.saturated)) {
##                   i.saturated <- which(is.saturated)
##                   prior.saturated <- priors.betas[[i.saturated]]
##                   A.sigma <- prior.saturated@ATau
##                   nu.sigma <- prior.saturated@nuTau
##                   sigma.max <- prior.saturated@tauMax
##                   sigma <- prior.saturated@tau
##               }
##               betas <- unname(lapply(betas, as.numeric))
##               betas <- jitterBetas(betas = betas, priorsBetas = priors.betas)
##               iterator.betas <- BetaIterator(dim = dim, margins = margins)
##               dims <- makeDims(dim = dim, margins = margins)
##               cellInLik <- rep(TRUE, times = length(theta))
##               model <- methods::new("BinomialVarying",
##                                     call = call,
##                                     theta = theta,
##                                     metadataY = metadataY,
##                                     cellInLik = cellInLik,
##                                     scaleTheta = scale.theta,
##                                     scaleThetaMultiplier = scale.theta.multiplier,
##                                     nAcceptTheta = methods::new("Counter", 0L),
##                                     nFailedPropTheta = methods::new("Counter", 0L),
##                                     sigma = sigma,
##                                     sigmaMax = sigma.max,
##                                     lower = lower,
##                                     upper = upper,
##                                     tolerance = tolerance,
##                                     maxAttempt = max.attempt,
##                                     ASigma = A.sigma,
##                                     nuSigma = nu.sigma,
##                                     betas = betas,
##                                     priorsBetas = priors.betas,
##                                     namesBetas = names.betas,
##                                     margins = margins,
##                                     iteratorBetas = iterator.betas,
##                                     dims = dims)
##               default.weights <- exposure
##               model <- addAg(model = model,
##                              aggregate = aggregate,
##                              defaultWeights = default.weights)
##               model <- makeCellInLik(model = model,
##                                      y = y)
##               model
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



