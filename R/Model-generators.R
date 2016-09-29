

## addAg ########################################################################

## SpecAgPlaceholder

## HAS_TESTS
setMethod("addAg",
          signature(model = "BinomialVarying",
                    aggregate = "SpecAgPlaceholder"),
          function(model, aggregate) {
              model
          })

## HAS_TESTS
setMethod("addAg",
          signature(model = "NormalVaryingVarsigmaKnown",
                    aggregate = "SpecAgPlaceholder"),
          function(model, aggregate) {
              model
          })

## HAS_TESTS
setMethod("addAg",
          signature(model = "NormalVaryingVarsigmaUnknown",
                    aggregate = "SpecAgPlaceholder"),
          function(model, aggregate) {
              model
          })

## HAS_TESTS
setMethod("addAg",
          signature(model = "PoissonVaryingNotUseExp",
                    aggregate = "SpecAgPlaceholder"),
          function(model, aggregate) {
              model
          })

## HAS_TESTS
setMethod("addAg",
          signature(model = "PoissonVaryingUseExp",
                    aggregate = "SpecAgPlaceholder"),
          function(model, aggregate) {
              model
          })


## SpecAgCertain

## HAS_TESTS
setMethod("addAg",
          signature(model = "BinomialVarying",
                    aggregate = "SpecAgCertain"),
          function(model, aggregate, defaultWeights) {
              l <- addAgCertain(object = model,
                                aggregate = aggregate,
                                defaultWeights = defaultWeights)
              model@theta <- l$theta
              class <- paste0(class(model), "AgCertain")
              methods::new(class,
                  model,
                  valueAg = l$value,
                  weightAg = l$weight,
                  transformAg = l$transform,
                  metadataAg = l$metadata,
                  mu = l$mu,
                  slotsToExtract = l$slotsToExtract,
                  iMethodModel = l$iMethodModel)
          })

## HAS_TESTS
setMethod("addAg",
          signature(model = "NormalVaryingVarsigmaKnown",
                    aggregate = "SpecAgCertain"),
          function(model, aggregate, defaultWeights) {
              l <- addAgCertain(object = model,
                                aggregate = aggregate,
                                defaultWeights = defaultWeights)
              model@theta <- l$theta
              class <- paste0(class(model), "AgCertain")
              methods::new(class,
                  model,
                  valueAg = l$value,
                  weightAg = l$weight,
                  transformAg = l$transform,
                  metadataAg = l$metadata,
                  mu = l$mu,
                  slotsToExtract = l$slotsToExtract,
                  iMethodModel = l$iMethodModel)
          })

## HAS_TESTS
setMethod("addAg",
          signature(model = "NormalVaryingVarsigmaUnknown",
                    aggregate = "SpecAgCertain"),
          function(model, aggregate, defaultWeights) {
              l <- addAgCertain(object = model,
                                aggregate = aggregate,
                                defaultWeights = defaultWeights)
              model@theta <- l$theta
              class <- paste0(class(model), "AgCertain")
              methods::new(class,
                  model,
                  valueAg = l$value,
                  weightAg = l$weight,
                  transformAg = l$transform,
                  metadataAg = l$metadata,
                  mu = l$mu,
                  slotsToExtract = l$slotsToExtract,
                  iMethodModel = l$iMethodModel)
          })

## HAS_TESTS
setMethod("addAg",
          signature(model = "PoissonVaryingNotUseExp",
                    aggregate = "SpecAgCertain"),
          function(model, aggregate, defaultWeights) {
              l <- addAgCertain(object = model,
                                aggregate = aggregate,
                                defaultWeights = defaultWeights)
              model@theta <- l$theta
              class <- paste0(class(model), "AgCertain")
              methods::new(class,
                  model,
                  valueAg = l$value,
                  weightAg = l$weight,
                  transformAg = l$transform,
                  metadataAg = l$metadata,
                  mu = l$mu,
                  slotsToExtract = l$slotsToExtract,
                  iMethodModel = l$iMethodModel)
          })

## HAS_TESTS
setMethod("addAg",
          signature(model = "PoissonVaryingUseExp",
                    aggregate = "SpecAgCertain"),
          function(model, aggregate, defaultWeights) {
              l <- addAgCertain(object = model,
                                aggregate = aggregate,
                                defaultWeights = defaultWeights)
              model@theta <- l$theta
              class <- paste0(class(model), "AgCertain")
              methods::new(class,
                  model,
                  valueAg = l$value,
                  weightAg = l$weight,
                  transformAg = l$transform,
                  metadataAg = l$metadata,
                  mu = l$mu,
                  slotsToExtract = l$slotsToExtract,
                  iMethodModel = l$iMethodModel)
          })

## SpecAgNormal

## HAS_TESTS
setMethod("addAg",
          signature(model = "BinomialVarying",
                    aggregate = "SpecAgNormal"),
          function(model, aggregate, defaultWeights) {
              l <- addAgNormal(object = model,
                               aggregate = aggregate,
                               defaultWeights = defaultWeights)
              class <- paste0(class(model), "AgNormal")
              methods::new(class,
                  model,
                  meanAg = l$mean,
                  metadataAg = l$metadata,                  
                  mu = l$mu,
                  nAcceptAg = methods::new("Counter", 0L),
                  nFailedPropValueAg = methods::new("Counter", 0L),
                  scaleAg = l$scale,
                  sdAg = l$sd,
                  transformAg = l$transform,
                  valueAg = l$value,
                  weightAg = l$weight,
                  slotsToExtract = l$slotsToExtract,
                  iMethodModel = l$iMethodModel)
          })

## HAS_TESTS
setMethod("addAg",
          signature(model = "NormalVaryingVarsigmaKnown",
                    aggregate = "SpecAgNormal"),
          function(model, aggregate, defaultWeights) {
              l <- addAgNormal(object = model,
                               aggregate = aggregate,
                               defaultWeights = defaultWeights)
              class <- paste0(class(model), "AgNormal")
              methods::new(class,
                  model,
                  meanAg = l$mean,
                  metadataAg = l$metadata,                  
                  mu = l$mu,
                  nAcceptAg = methods::new("Counter", 0L),
                  nFailedPropValueAg = methods::new("Counter", 0L),
                  scaleAg = l$scale,
                  sdAg = l$sd,
                  transformAg = l$transform,
                  valueAg = l$value,
                  weightAg = l$weight,
                  slotsToExtract = l$slotsToExtract,
                  iMethodModel = l$iMethodModel)
          })

## HAS_TESTS
setMethod("addAg",
          signature(model = "NormalVaryingVarsigmaUnknown",
                    aggregate = "SpecAgNormal"),
          function(model, aggregate, defaultWeights) {
              l <- addAgNormal(object = model,
                               aggregate = aggregate,
                               defaultWeights = defaultWeights)
              class <- paste0(class(model), "AgNormal")
              methods::new(class,
                  model,
                  meanAg = l$mean,
                  metadataAg = l$metadata,                  
                  mu = l$mu,
                  nAcceptAg = methods::new("Counter", 0L),
                  nFailedPropValueAg = methods::new("Counter", 0L),
                  scaleAg = l$scale,
                  sdAg = l$sd,
                  transformAg = l$transform,
                  valueAg = l$value,
                  weightAg = l$weight,
                  slotsToExtract = l$slotsToExtract,
                  iMethodModel = l$iMethodModel)
          })

## HAS_TESTS
setMethod("addAg",
          signature(model = "PoissonVaryingNotUseExp",
                    aggregate = "SpecAgNormal"),
          function(model, aggregate, defaultWeights) {
              l <- addAgNormal(object = model,
                               aggregate = aggregate,
                               defaultWeights = defaultWeights)
              class <- paste0(class(model), "AgNormal")
              methods::new(class,
                           model,
                           meanAg = l$mean,
                           metadataAg = l$metadata,                  
                           mu = l$mu,
                           nAcceptAg = methods::new("Counter", 0L),
                           nFailedPropValueAg = methods::new("Counter", 0L),
                           scaleAg = l$scale,
                           sdAg = l$sd,
                           transformAg = l$transform,
                           valueAg = l$value,
                           weightAg = l$weight,
                           slotsToExtract = l$slotsToExtract,
                           iMethodModel = l$iMethodModel)
          })

## HAS_TESTS
setMethod("addAg",
          signature(model = "PoissonVaryingUseExp",
                    aggregate = "SpecAgNormal"),
          function(model, aggregate, defaultWeights) {
              l <- addAgNormal(object = model,
                               aggregate = aggregate,
                               defaultWeights = defaultWeights)
              class <- paste0(class(model), "AgNormal")
              methods::new(class,
                  model,
                  meanAg = l$mean,
                  metadataAg = l$metadata,                  
                  mu = l$mu,
                  nAcceptAg = methods::new("Counter", 0L),
                  nFailedPropValueAg = methods::new("Counter", 0L),
                  scaleAg = l$scale,
                  sdAg = l$sd,
                  transformAg = l$transform,
                  valueAg = l$value,
                  weightAg = l$weight,
                  slotsToExtract = l$slotsToExtract,
                  iMethodModel = l$iMethodModel)
          })

## SpecAgPoisson

## HAS_TESTS
setMethod("addAg",
          signature(model = "BinomialVarying",
                    aggregate = "SpecAgPoisson"),
          function(model, aggregate) {
              stop(gettext("Poisson model for aggregate values can only be used with Poisson likelihood"))
          })

## HAS_TESTS
setMethod("addAg",
          signature(model = "NormalVaryingVarsigmaKnown",
                    aggregate = "SpecAgPoisson"),
          function(model, aggregate) {
              stop(gettext("Poisson model for aggregate values can only be used with Poisson likelihood"))
          })

## HAS_TESTS
setMethod("addAg",
          signature(model = "NormalVaryingVarsigmaUnknown",
                    aggregate = "SpecAgPoisson"),
          function(model, aggregate) {
              stop(gettext("Poisson model for aggregate values can only be used with Poisson likelihood"))
          })

## HAS_TESTS
setMethod("addAg",
          signature(model = "PoissonVaryingNotUseExp",
                    aggregate = "SpecAgPoisson"),
          function(model, aggregate, defaultWeights) {
              l <- addAgPoisson(object = model,
                               aggregate = aggregate,
                               defaultWeights = defaultWeights)
              class <- paste0(class(model), "AgPoisson")
              methods::new(class,
                  model,
                  exposureAg = l$exposure,
                  meanAg = l$mean,
                  metadataAg = l$metadata,                  
                  mu = l$mu,
                  nAcceptAg = methods::new("Counter", 0L),
                  nFailedPropValueAg = methods::new("Counter", 0L),
                  scaleAg = l$scale,
                  transformAg = l$transform,
                  valueAg = l$value,
                  weightAg = l$weight,
                  slotsToExtract = l$slotsToExtract,
                  iMethodModel = l$iMethodModel)
          })

## HAS_TESTS
setMethod("addAg",
          signature(model = "PoissonVaryingUseExp",
                    aggregate = "SpecAgPoisson"),
          function(model, aggregate, defaultWeights) {
              l <- addAgPoisson(object = model,
                                aggregate = aggregate,
                                defaultWeights = defaultWeights)
              class <- paste0(class(model), "AgPoisson")
              methods::new(class,
                           model,
                           exposureAg = l$exposure,
                           meanAg = l$mean,
                           metadataAg = l$metadata,                  
                           mu = l$mu,
                           nAcceptAg = methods::new("Counter", 0L),
                           nFailedPropValueAg = methods::new("Counter", 0L),
                           scaleAg = l$scale,
                           transformAg = l$transform,
                           valueAg = l$value,
                           weightAg = l$weight,
                           slotsToExtract = l$slotsToExtract,
                           iMethodModel = l$iMethodModel)
          })


## SpecAgFun

## HAS_TESTS
setMethod("addAg",
          signature(model = "BinomialVarying",
                    aggregate = "SpecAgFun"),
          function(model, aggregate, defaultWeights) {
              l <- addAgFun(object = model,
                            aggregate = aggregate,
                            defaultWeights = defaultWeights)
              class <- paste0(class(model), "AgFun")
              methods::new(class,
                           model,
                           funAg = l$funAg,
                           meanAg = l$mean,
                           metadataAg = l$metadata,
                           transformAg = l$transform,
                           sdAg = l$sd,
                           valueAg = l$value,
                           weightsArgsAg = l$weightsArgs,
                           xArgsAg = l$xArgs,
                           slotsToExtract = l$slotsToExtract,
                           iMethodModel = l$iMethodModel)
          })

## HAS_TESTS
setMethod("addAg",
          signature(model = "NormalVaryingVarsigmaKnown",
                    aggregate = "SpecAgFun"),
          function(model, aggregate, defaultWeights) {
              l <- addAgFun(object = model,
                            aggregate = aggregate,
                            defaultWeights = defaultWeights)
              class <- paste0(class(model), "AgFun")
              methods::new(class,
                           model,
                           funAg = l$funAg,
                           meanAg = l$mean,
                           metadataAg = l$metadata,
                           transformAg = l$transform,
                           sdAg = l$sd,
                           valueAg = l$value,
                           weightsArgsAg = l$weightsArgs,
                           xArgsAg = l$xArgs,
                           slotsToExtract = l$slotsToExtract,
                           iMethodModel = l$iMethodModel)
          })

## HAS_TESTS
setMethod("addAg",
          signature(model = "NormalVaryingVarsigmaUnknown",
                    aggregate = "SpecAgFun"),
          function(model, aggregate, defaultWeights) {
              l <- addAgFun(object = model,
                            aggregate = aggregate,
                            defaultWeights = defaultWeights)
              class <- paste0(class(model), "AgFun")
              methods::new(class,
                           model,
                           funAg = l$funAg,
                           meanAg = l$mean,
                           metadataAg = l$metadata,
                           transformAg = l$transform,
                           sdAg = l$sd,
                           valueAg = l$value,
                           weightsArgsAg = l$weightsArgs,
                           xArgsAg = l$xArgs,
                           slotsToExtract = l$slotsToExtract,
                           iMethodModel = l$iMethodModel)
          })

## HAS_TESTS
setMethod("addAg",
          signature(model = "PoissonVaryingNotUseExp",
                    aggregate = "SpecAgFun"),
          function(model, aggregate, defaultWeights) {
              l <- addAgFun(object = model,
                            aggregate = aggregate,
                            defaultWeights = defaultWeights)
              class <- paste0(class(model), "AgFun")
              methods::new(class,
                           model,
                           funAg = l$funAg,
                           meanAg = l$mean,
                           metadataAg = l$metadata,
                           transformAg = l$transform,
                           sdAg = l$sd,
                           valueAg = l$value,
                           weightsArgsAg = l$weightsArgs,
                           xArgsAg = l$xArgs,
                           slotsToExtract = l$slotsToExtract,
                           iMethodModel = l$iMethodModel)
          })

## HAS_TESTS
setMethod("addAg",
          signature(model = "PoissonVaryingUseExp",
                    aggregate = "SpecAgFun"),
          function(model, aggregate, defaultWeights) {
              l <- addAgFun(object = model,
                            aggregate = aggregate,
                            defaultWeights = defaultWeights)
              class <- paste0(class(model), "AgFun")
              methods::new(class,
                           model,
                           funAg = l$funAg,
                           meanAg = l$mean,
                           metadataAg = l$metadata,
                           transformAg = l$transform,
                           sdAg = l$sd,
                           valueAg = l$value,
                           weightsArgsAg = l$weightsArgs,
                           xArgsAg = l$xArgs,
                           slotsToExtract = l$slotsToExtract,
                           iMethodModel = l$iMethodModel)
          })





## initialModel ############################################################################

## Assume that 'y', 'exposure' (if any), and 'weights' (if any)
## are valid by the time they reach 'initialModel'.


## HAS_TESTS
setMethod("initialModel",
          signature(object = "SpecBinomialVarying",
                    y = "Counts",
                    exposure = "Counts",
                    weights = "missing"),
          function(object, y, exposure) {
              call <- object@call
              formula.mu <- object@formulaMu
              specs.priors <- object@specsPriors
              names.specs.priors <- object@namesSpecsPriors
              lower <- object@lower
              upper <- object@upper
              tolerance <- object@tolerance
              max.attempt <- object@maxAttempt
              scale.theta <- object@scaleTheta
              nu.sigma <- object@nuSigma
              A.sigma <- object@ASigma@.Data
              sigma.max <- object@sigmaMax@.Data
              aggregate <- object@aggregate
              checkTermsFromFormulaFound(y = y, formula = formula.mu)
              checkLengthDimInFormula(y = y, formula = formula.mu)
              metadataY <- y@metadata
              dim <- dim(y)
              y.tmp <- as.integer(y)
              exposure.tmp <- as.integer(exposure)
              y.tmp[is.na(y)] <- 0.0
              exposure.tmp[is.na(y)] <- 0.0
              m <- sum(y[!is.na(y)]) / sum(exposure[!is.na(y)])
              nu <- m * (1 - m)
              A.sigma <- makeASigma(A = A.sigma,
                                    sY = NULL,
                                    isSpec = FALSE)
              sigma.max <- makeScaleMax(scaleMax = sigma.max,
                                        A = A.sigma,
                                        nu = nu.sigma,
                                        isSpec = FALSE)
              sigma <- stats::runif(n = 1L,
                                    min = 0,
                                    max = min(nu, sigma.max@.Data))
              theta <- stats::rbeta(n = length(y),
                                    shape1 = m * (nu / sigma^2 - 1) + y.tmp,
                                    shape2 = (1 - m) * (nu / sigma^2 - 1) + exposure.tmp - y.tmp)
              ## need to avoid having all 'theta' equalling lower or upper bound
              is.too.low <- theta < lower
              n.too.low <- sum(is.too.low)
              width <- 0.2 * (upper - lower)
              if (is.infinite(width))
                  width <- 100
              theta[is.too.low] <- stats::runif(n = n.too.low, min = lower, max = lower + width)
              is.too.high <- theta > upper
              n.too.high <- sum(is.too.high)
              theta[is.too.high] <- stats::runif(n = n.too.high, min = upper - width, max = upper)
              lower <- log(lower / (1 - lower))
              upper <- log(upper / (1 - upper))
              scale.theta.multiplier <- mean(sqrt(1 + log(1 + exposure.tmp)))
              scale.theta.multiplier <- methods::new("Scale", scale.theta.multiplier)
              theta <- array(theta, dim = dim(y), dimnames = dimnames(y))
              logit.theta <- log(theta / (1 - theta))
              betas <- makeLinearBetas(theta = logit.theta, formula = formula.mu)
              checkNumberElementsBetas(betas = betas, y = y)
              theta <- as.numeric(theta)
              names.betas <- names(betas)
              margins <- makeMargins(betas = betas, y = y)
              priors.betas <- makePriors(betas = betas,
                                         specs = specs.priors,
                                         namesSpecs = names.specs.priors,
                                         margins = margins,
                                         y = y,
                                         sY = NULL)
              betas <- unname(lapply(betas, as.numeric))
              betas <- jitterBetas(betas)
              iterator.betas <- BetaIterator(dim = dim, margins = margins)
              dims <- makeDims(dim = dim, margins = margins)
              model <- methods::new("BinomialVarying",
                                    call = call,
                                    theta = theta,
                                    metadataY = metadataY,
                                    scaleTheta = scale.theta,
                                    scaleThetaMultiplier = scale.theta.multiplier,
                                    nAcceptTheta = methods::new("Counter", 0L),
                                    nFailedPropTheta = methods::new("Counter", 0L),
                                    sigma = methods::new("Scale", sigma),
                                    sigmaMax = sigma.max,
                                    lower = lower,
                                    upper = upper,
                                    tolerance = tolerance,
                                    maxAttempt = max.attempt,
                                    ASigma = A.sigma,
                                    nuSigma = nu.sigma,
                                    betas = betas,
                                    priorsBetas = priors.betas,
                                    namesBetas = names.betas,
                                    margins = margins,
                                    iteratorBetas = iterator.betas,
                                    dims = dims)
              default.weights <- exposure
              addAg(model = model,
                    aggregate = aggregate,
                    defaultWeights = default.weights)
          })

## HAS_TESTS
setMethod("initialModel",
          signature(object = "SpecNormalVaryingVarsigmaKnown",
                    y = "DemographicArray",
                    exposure = "missing",
                    weights = "Counts"),
          function(object, y, weights) {
              call <- object@call
              formula.mu <- object@formulaMu
              specs.priors <- object@specsPriors
              names.specs.priors <- object@namesSpecsPriors
              scale.theta <- object@scaleTheta
              lower <- object@lower
              upper <- object@upper
              tolerance <- object@tolerance
              max.attempt <- object@maxAttempt
              varsigma <- object@varsigma
              nu.sigma <- object@nuSigma
              A.sigma <- object@ASigma@.Data
              sigma.max <- object@sigmaMax@.Data
              aggregate <- object@aggregate
              checkTermsFromFormulaFound(y = y, formula = formula.mu)
              checkLengthDimInFormula(y = y, formula = formula.mu)
              metadataY <- y@metadata
              dim <- dim(y)
              w <- as.numeric(weights)
              n <- length(y)
              sY <- stats::sd(as.numeric(y), na.rm = TRUE) ## to deal with R <3.0 behaviour
              A.sigma <- makeASigma(A = A.sigma, sY = sY)
              sigma.max <- makeScaleMax(scaleMax = sigma.max,
                                        A = A.sigma,
                                        nu = nu.sigma)
              sigma <- stats::runif(n = 1L,
                                    min = 0,
                                    max = min(A.sigma@.Data, sigma.max@.Data))
              y.bar <- mean(y, na.rm = TRUE)
              mean <- 0.5 * y.bar + 0.5 * y
              mean[is.na(y)] <- y.bar
              theta <- rnormTruncated(n = n,
                                      mean = mean,
                                      sd = rep(sigma, times = n),
                                      lower = lower,
                                      upper = upper,
                                      tolerance = tolerance,
                                      maxAttempt = max.attempt,
                                      uniform = TRUE,
                                      useC = TRUE)
              theta <- array(theta, dim = dim(y), dimnames = dimnames(y))
              betas <- makeLinearBetas(theta = theta, formula = formula.mu)
              checkNumberElementsBetas(betas = betas, y = y)
              theta <- as.numeric(theta)
              names.betas <- names(betas)
              margins <- makeMargins(betas = betas, y = y)
              priors.betas <- makePriors(betas = betas,
                                         specs = specs.priors,
                                         namesSpecs = names.specs.priors,
                                         margins = margins,
                                         y = y,
                                         sY = sY)
              betas <- unname(lapply(betas, as.numeric))
              betas <- jitterBetas(betas)
              iterator.betas <- BetaIterator(dim = dim, margins = margins)
              dims <- makeDims(dim = dim, margins = margins)
              model <- methods::new("NormalVaryingVarsigmaKnown",
                           call = call,
                           theta = theta,
                           metadataY = metadataY,
                           w = w,
                           varsigma = varsigma,
                           lower = lower,
                           upper = upper,
                           tolerance = tolerance,
                           scaleTheta = scale.theta,
                           nAcceptTheta = methods::new("Counter", 0L),
                           nFailedPropTheta = methods::new("Counter", 0L),
                           maxAttempt = max.attempt,
                           sigma = methods::new("Scale", sigma),
                           sigmaMax = sigma.max,
                           ASigma = A.sigma,
                           nuSigma = nu.sigma,
                           betas = betas,
                           priorsBetas = priors.betas,
                           namesBetas = names.betas,
                           margins = margins,
                           iteratorBetas = iterator.betas,
                           dims = dims)
              default.weights <- weights
              addAg(model = model,
                    aggregate = aggregate,
                    defaultWeights = default.weights)
          })

## HAS_TESTS
setMethod("initialModel",
          signature(object = "SpecNormalVaryingVarsigmaUnknown",
                    y = "DemographicArray",
                    exposure = "missing",
                    weights = "Counts"),
          function(object, y, weights) {
              call <- object@call
              formula.mu <- object@formulaMu
              specs.priors <- object@specsPriors
              names.specs.priors <- object@namesSpecsPriors
              scale.theta <- object@scaleTheta
              lower <- object@lower
              upper <- object@upper
              tolerance <- object@tolerance
              max.attempt <- object@maxAttempt
              nu.varsigma <- object@nuVarsigma
              A.varsigma <- object@AVarsigma@.Data
              varsigma.max <- object@varsigmaMax@.Data
              nu.sigma <- object@nuSigma
              A.sigma <- object@ASigma@.Data
              sigma.max <- object@sigmaMax@.Data
              aggregate <- object@aggregate
              checkTermsFromFormulaFound(y = y, formula = formula.mu)
              checkLengthDimInFormula(y = y, formula = formula.mu)
              metadataY <- y@metadata
              dim <- dim(y)
              w <- as.numeric(weights)
              n <- length(y)
              sY <- stats::sd(as.numeric(y), na.rm = TRUE) ## to deal with R <3.0 behaviour
              A.varsigma <- makeASigma(A = A.varsigma, sY = sY)
              A.sigma <- makeASigma(A = A.sigma, sY = sY)
              varsigma.max <- makeScaleMax(scaleMax = varsigma.max,
                                           A = A.varsigma,
                                           nu = nu.varsigma)
              sigma.max <- makeScaleMax(scaleMax = sigma.max,
                                        A = A.sigma,
                                        nu = nu.sigma)
              varsigma <- stats::runif(n = 1L,
                                       min = 0,
                                       max = min(A.varsigma@.Data, varsigma.max@.Data))
              sigma <- stats::runif(n = 1L,
                                    min = 0,
                                    max = min(A.sigma@.Data, sigma.max@.Data))
              y.bar <- mean(y, na.rm = TRUE)
              mean <- 0.5 * y.bar + 0.5 * y
              mean[is.na(y)] <- y.bar
              theta <- rnormTruncated(n = n,
                                      mean = mean,
                                      sd = rep(sigma, times = n),
                                      lower = lower,
                                      upper = upper,
                                      tolerance = tolerance,
                                      maxAttempt = max.attempt,
                                      uniform = TRUE,
                                      useC = TRUE)
              theta <- array(theta, dim = dim(y), dimnames = dimnames(y))
              betas <- makeLinearBetas(theta = theta, formula = formula.mu)
              checkNumberElementsBetas(betas = betas, y = y)
              theta <- as.numeric(theta)
              names.betas <- names(betas)
              margins <- makeMargins(betas = betas, y = y)
              priors.betas <- makePriors(betas = betas,
                                         specs = specs.priors,
                                         namesSpecs = names.specs.priors,
                                         margins = margins,
                                         y = y,
                                         sY = sY)
              betas <- unname(lapply(betas, as.numeric))
              betas <- jitterBetas(betas)
              iterator.betas <- BetaIterator(dim = dim, margins = margins)
              dims <- makeDims(dim = dim, margins = margins)
              model <- methods::new("NormalVaryingVarsigmaUnknown",
                           call = call,
                           theta = theta,
                           metadataY = metadataY,
                           w = w,
                           varsigma = methods::new("Scale", varsigma),
                           varsigmaMax = varsigma.max,
                           AVarsigma = A.varsigma,
                           nuVarsigma = nu.varsigma,
                           lower = lower,
                           upper = upper,
                           tolerance = tolerance,
                           scaleTheta = scale.theta,
                           nAcceptTheta = methods::new("Counter", 0L),
                           nFailedPropTheta = methods::new("Counter", 0L),
                           maxAttempt = max.attempt,
                           sigma = methods::new("Scale", sigma),
                           sigmaMax = sigma.max,
                           ASigma = A.sigma,
                           nuSigma = nu.sigma,
                           betas = betas,
                           priorsBetas = priors.betas,
                           namesBetas = names.betas,
                           margins = margins,
                           iteratorBetas = iterator.betas,
                           dims = dims)
              default.weights <- weights
              addAg(model = model,
                    aggregate = aggregate,
                    defaultWeights = default.weights)
          })

## HAS_TESTS
setMethod("initialModel",
          signature(object = "SpecPoissonVarying",
                    y = "Counts",
                    exposure = "ANY",
                    weights = "missing"),
          function(object, y, exposure) {
              call <- object@call
              formula.mu <- object@formulaMu
              specs.priors <- object@specsPriors
              names.specs.priors <- object@namesSpecsPriors
              scale.theta <- object@scaleTheta
              lower <- object@lower
              upper <- object@upper
              tolerance <- object@tolerance
              max.attempt <- object@maxAttempt
              nu.sigma <- object@nuSigma
              A.sigma <- object@ASigma@.Data
              sigma.max <- object@sigmaMax@.Data
              aggregate <- object@aggregate
              checkTermsFromFormulaFound(y = y, formula = formula.mu)
              checkLengthDimInFormula(y = y, formula = formula.mu)
              metadataY <- y@metadata
              dim <- dim(y)
              has.exposure <- !is.null(exposure)
              y.missing <- is.na(y)
              mean.y.obs <- mean(y[!y.missing])
              shape <- ifelse(y.missing, mean.y.obs, 0.5 * mean.y.obs + 0.5 * y)
              if (has.exposure) {
                  mean.expose.obs <- mean(exposure[!y.missing])
                  rate <- ifelse(y.missing, mean.expose.obs, 0.5 * mean.expose.obs + 0.5 * exposure)
              }
              else
                  rate <- 1
              if (has.exposure) {
                  exposure.tmp <- as.double(exposure)
                  exposure.tmp[is.na(exposure.tmp)] <- 0
                  scale.theta.multiplier <- mean(sqrt(1 + log(1 + exposure.tmp)))
              }
              else
                  scale.theta.multiplier <- 1.0
              scale.theta.multiplier <- methods::new("Scale", scale.theta.multiplier)
              theta <- stats::rgamma(n = length(y), shape = shape, rate = rate)
              if (has.exposure)
                  sY <- NULL
              else
                  sY <-  stats::sd(log(as.numeric(y) + 1), na.rm = TRUE)
              A.sigma <- makeASigma(A = A.sigma,
                                    sY = sY)
              sigma.max <- makeScaleMax(scaleMax = sigma.max,
                                        A = A.sigma,
                                        nu = nu.sigma)
              sigma <- stats::runif(n = 1L,
                                    min = 0,
                                    max = min(A.sigma@.Data, sigma.max@.Data))
              ## need to avoid having all 'theta' equalling lower or upper bound
              is.too.low <- theta < lower
              n.too.low <- sum(is.too.low)
              width <- 0.2 * (upper - lower)
              if (is.infinite(width))
                  width <- 100
              theta[is.too.low] <- stats::runif(n = n.too.low, min = lower, max = lower + width)
              is.too.high <- theta > upper
              n.too.high <- sum(is.too.high)
              theta[is.too.high] <- stats::runif(n = n.too.high, min = upper - width, max = upper)
              lower <- log(lower)
              upper <- log(upper)
              theta <- array(theta, dim = dim(y), dimnames = dimnames(y))
              if (formulaIsInterceptOnly(formula.mu))
                  betas <- list("(Intercept)" = mean(log(theta)))
              else {
                  betas <- MASS::loglm(formula.mu, data = theta)$param
                  betas <- convertToFormulaOrder(betas = betas, formulaMu = formula.mu)
              }
              checkNumberElementsBetas(betas = betas, y = y)
              theta <- as.numeric(theta)
              names.betas <- names(betas)
              margins <- makeMargins(betas = betas, y = y)
              priors.betas <- makePriors(betas = betas,
                                         specs = specs.priors,
                                         namesSpecs = names.specs.priors,
                                         margins = margins,
                                         y = y,
                                         sY = sY)
              betas <- unname(lapply(betas, as.numeric))
              betas <- jitterBetas(betas)
              iterator.betas <- BetaIterator(dim = dim, margins = margins)
              dims <- makeDims(dim = dim, margins = margins)
              class <- if (has.exposure) "PoissonVaryingUseExp" else "PoissonVaryingNotUseExp"
              model <- methods::new(class,
                           call = call,
                           theta = theta,
                           metadataY = metadataY,
                           scaleTheta = scale.theta,
                           scaleThetaMultiplier = scale.theta.multiplier,
                           lower = lower,
                           upper = upper,
                           tolerance = tolerance,
                           nAcceptTheta = methods::new("Counter", 0L),
                           nFailedPropTheta = methods::new("Counter", 0L),
                           maxAttempt = max.attempt,
                           sigma = methods::new("Scale", sigma),
                           sigmaMax = sigma.max,
                           ASigma = A.sigma,
                           nuSigma = nu.sigma,
                           betas = betas,
                           priorsBetas = priors.betas,
                           namesBetas = names.betas,
                           margins = margins,
                           iteratorBetas = iterator.betas,
                           dims = dims)
              if (has.exposure)
                  default.weights <- exposure
              else {
                  .Data <- array(1.0,
                                 dim = dim(metadataY),
                                 dimnames = dimnames(metadataY))
                  default.weights <- methods::new("Counts",
                                         .Data = .Data,
                                         metadata = metadataY)
              }
              addAg(model = model,
                    aggregate = aggregate,
                    defaultWeights = default.weights)
          })

## HAS_TESTS
setMethod("initialModel",
          signature(object = "SpecPoissonBinomialMixture",
                    y = "Counts",
                    exposure = "Counts",
                    weights = "missing"),
          function(object, y, exposure) {
              call <- object@call
              prob <- object@prob
              metadataY <- y@metadata
              methods::new("PoissonBinomialMixture",
                  call = call,
                  prob = prob,
                  metadataY = metadataY)
          })


## initialModelPredict ################################################################

## 'initialModelPredict' does not retain information
## from any previous aggregate models

## HAS_TESTS
setMethod("initialModelPredict",
          signature(model = "BinomialVarying"),
          function(model, along, labels, n, offsetModel,
                   covariates, aggregate, lower, upper) {
              l <- initialModelPredictHelper(model = model,
                                             along = along,
                                             labels = labels,
                                             n = n,
                                             offsetModel = offsetModel,
                                             covariates = covariates)
              metadataY <- l$metadataY
              if (is.null(lower))
                  lower <- invlogit1(model@lower)
              if (is.null(upper))
                  upper <- invlogit1(model@upper)
              checkLowerAndUpper(lower = lower,
                                 upper = upper,
                                 distribution = "Binomial")
              logit <- function(x) log(x / (1 - x))
              lower <- logit(lower)
              upper <- logit(upper)
              ans <- methods::new("BinomialVaryingPredict",
                         model,
                         theta = l$theta,
                         metadataY = metadataY,
                         nAcceptTheta = methods::new("Counter", 0L),
                         lower = lower,
                         upper = upper,
                         nFailedPropTheta = methods::new("Counter", 0L),
                         betas = l$betas,
                         priorsBetas = l$priorsBetas,
                         iteratorBetas = l$iteratorBetas,
                         dims = l$dims,
                         betaIsPredicted = l$betaIsPredicted,
                         offsetsBetas = l$offsetsBetas,
                         offsetsPriorsBetas = l$offsetsPriorsBetas,
                         offsetsSigma = l$offsetsSigma,
                         iMethodModel = l$iMethodModel)
              if (!is.null(aggregate)) {
                  ans <- addAg(model = ans,
                               aggregate = aggregate,
                               defaultWeights = NULL)
              }
              ans
          })

## HAS_TESTS
setMethod("initialModelPredict",
          signature(model = "PoissonVarying"),
          function(model, along, labels, n, offsetModel,
                   covariates, aggregate, lower, upper) {
              l <- initialModelPredictHelper(model = model,
                                             along = along,
                                             labels = labels,
                                             n = n,
                                             offsetModel = offsetModel,
                                             covariates = covariates)
              metadataY <- l$metadataY
              if (is.null(lower))
                  lower <- exp(model@lower)
              if (is.null(upper))
                  upper <- exp(model@upper)
              checkLowerAndUpper(lower = lower,
                                 upper = upper,
                                 distribution = "Poisson")
              lower <- log(lower)
              upper <- log(upper)
              uses.exposure <- methods::is(model, "UseExposure")
              if (uses.exposure)
                  Class <- "PoissonVaryingUseExpPredict"
              else
                  Class <- "PoissonVaryingNotUseExpPredict"
              ans <- methods::new(Class,
                         model,
                         theta = l$theta,
                         metadataY = metadataY,
                         nAcceptTheta = methods::new("Counter", 0L),
                         lower = lower,
                         upper = upper,
                         nFailedPropTheta = methods::new("Counter", 0L),
                         betas = l$betas,
                         priorsBetas = l$priorsBetas,
                         iteratorBetas = l$iteratorBetas,
                         dims = l$dims,
                         betaIsPredicted = l$betaIsPredicted,
                         offsetsBetas = l$offsetsBetas,
                         offsetsPriorsBetas = l$offsetsPriorsBetas,
                         offsetsSigma = l$offsetsSigma,
                         iMethodModel = l$iMethodModel)
              if (!is.null(aggregate)) {
                  if (uses.exposure)
                      default.weights <- NULL
                  else
                      default.weights <- array(1.0,
                                               dim = dim(metadataY),
                                               dimnames = dimnames(metadataY))
                  ans <- addAg(model = ans,
                               aggregate = aggregate,
                               defaultWeights = default.weights)
              }
              ans
          })

## HAS_TESTS
setMethod("initialModelPredict",
          signature(model = "NormalVarying"),
          function(model, along, labels, n, offsetModel,
                   covariates, aggregate, lower, upper) {
              l <- initialModelPredictHelper(model = model,
                                             along = along,
                                             labels = labels,
                                             n = n,
                                             offsetModel = offsetModel,
                                             covariates = covariates)
              if (is.null(lower))
                  lower <- model@lower
              if (is.null(upper))
                  upper <- model@upper
              checkLowerAndUpper(lower = lower,
                                 upper = upper,
                                 distribution = "Normal")
              metadataY <- l$metadataY
              .Data <- array(1.0,
                             dim = dim(metadataY),
                             dimnames = dimnames(metadataY))
              weights <- methods::new("Counts", .Data = .Data, metadata = metadataY)
              w <- as.numeric(weights)
              if (methods::is(model, "VarsigmaKnown"))
                  ans <- methods::new("NormalVaryingVarsigmaKnownPredict",
                             model,
                             theta = l$theta,
                             metadataY = metadataY,
                             lower = lower,
                             upper = upper,
                             nFailedPropTheta = methods::new("Counter", 0L),
                             betas = l$betas,
                             priorsBetas = l$priorsBetas,
                             iteratorBetas = l$iteratorBetas,
                             dims = l$dims,
                             betaIsPredicted = l$betaIsPredicted,
                             offsetsBetas = l$offsetsBetas,
                             offsetsPriorsBetas = l$offsetsPriorsBetas,
                             offsetsSigma = l$offsetsSigma,
                             iMethodModel = l$iMethodModel,
                             w = w)
              else {
                  offsets.varsigma <- makeOffsetsVarsigma(model, offsetModel = offsetModel)
                  ans <- methods::new("NormalVaryingVarsigmaUnknownPredict",
                             model,
                             theta = l$theta,
                             metadataY = metadataY,
                             lower = lower,
                             upper = upper,
                             nFailedPropTheta = methods::new("Counter", 0L),
                             betas = l$betas,
                             priorsBetas = l$priorsBetas,
                             iteratorBetas = l$iteratorBetas,
                             dims = l$dims,
                             betaIsPredicted = l$betaIsPredicted,
                             offsetsBetas = l$offsetsBetas,
                             offsetsPriorsBetas = l$offsetsPriorsBetas,
                             offsetsVarsigma = offsets.varsigma,
                             offsetsSigma = l$offsetsSigma,
                             iMethodModel = l$iMethodModel,
                             w = w)
              }
              if (!is.null(aggregate)) {
                  ans <- addAg(model = ans,
                               aggregate = aggregate,
                               defaultWeights = NULL)
              }
              ans
          })

## HAS_TESTS
setMethod("initialModelPredict",
          signature(model = "PoissonBinomialMixture"),
          function(model, along, labels, n, offsetModel,
                   covariates, aggregate, lower, upper) {
              metadata.first <- model@metadataY
              i.method.model.first <- model@iMethodModel
              metadata.second <- makeMetadataPredict(metadata = metadata.first,
                                                   along = along,
                                                   labels = labels,
                                                   n = n)
              i.method.model.second <- i.method.model.first + 100L
              methods::new("PoissonBinomialMixturePredict",
                  prob = model@prob,
                  metadataY = metadata.second,
                  iMethodModel = i.method.model.second)
          })

