

## addAg ########################################################################

## SpecAgPlaceholder

## HAS_TESTS
setMethod("addAg",
          signature(model = "BinomialVarying",
                    aggregate = "SpecAgPlaceholder"),
          function(model, aggregate) {
              model
          })

## NO_TESTS
setMethod("addAg",
          signature(model = "CMPVaryingNotUseExp",
                    aggregate = "SpecAgPlaceholder"),
          function(model, aggregate) {
              model
          })

## NO_TESTS
setMethod("addAg",
          signature(model = "CMPVaryingUseExp",
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


## SpecAgLife

## HAS_TESTS
setMethod("addAg",
          signature(model = "PoissonVaryingUseExp",
                    aggregate = "SpecAgLife",
                    defaultWeights = "Counts"),
          function(model, aggregate, defaultWeights) {
              l <- addAgLife(object = model,
                             aggregate = aggregate,
                             defaultWeights = defaultWeights)
              class <- paste0(class(model), "AgLife")
              methods::new(class,
                           model,
                           axAg = l$ax,
                           meanAg = l$mean,
                           metadataAg = l$metadataAg,
                           metadataMxAg = l$metadataMx,
                           mxAg = l$mx,
                           nAgeAg = l$nAge,
                           nxAg = l$nx,
                           sdAg = l$sd,
                           transformThetaToMxAg = l$transform,
                           valueAg = l$value,
                           slotsToExtract = l$slotsToExtract,
                           iMethodModel = l$iMethodModel)
          })


## initialModel ############################################################################

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
              nu.sigma <- object@nuSigma@.Data
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
              A.sigma <- new("Scale", A.sigma)
              nu.sigma <- new("DegreesFreedom", nu.sigma)
              sigma.max <- new("Scale", sigma.max)
              m <- sum(y[!is.na(y)]) / sum(exposure[!is.na(y)])
              nu <- m * (1 - m)
              sigma <- stats::runif(n = 1L,
                                    min = 0,
                                    max = min(nu, sigma.max@.Data))
              sigma <- methods::new("Scale", sigma)
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
              if (any(!is.na(y)))
                  scale.theta.multiplier <- median(sqrt(((exposure + 0.5) * (y + 0.5)) / (exposure - y + 0.5)),
                                                   na.rm = TRUE)
              else
                  scale.theta.multiplier <- 1
              scale.theta.multiplier <- methods::new("Scale", scale.theta.multiplier)
              theta <- array(theta, dim = dim(y), dimnames = dimnames(y))
              logit.theta <- log(theta / (1 - theta))
              betas <- makeLinearBetas(theta = logit.theta, formula = formula.mu)
              theta <- as.numeric(theta)
              names.betas <- names(betas)
              margins <- makeMargins(betas = betas, y = y)
              struc.zero.array <- makeStrucZeroArray(structuralZeros = NULL,
                                                     y = y) 
              priors.betas <- makePriors(betas = betas,
                                         specs = specs.priors,
                                         namesSpecs = names.specs.priors,
                                         margins = margins,
                                         y = y,
                                         sY = NULL,
                                         strucZeroArray = struc.zero.array)
              is.saturated <- sapply(priors.betas, function(x) x@isSaturated@.Data)
              if (any(is.saturated)) {
                  i.saturated <- which(is.saturated)
                  prior.saturated <- priors.betas[[i.saturated]]
                  A.sigma <- prior.saturated@ATau
                  nu.sigma <- prior.saturated@nuTau
                  sigma.max <- prior.saturated@tauMax
                  sigma <- prior.saturated@tau
              }
              betas <- unname(lapply(betas, as.numeric))
              betas <- jitterBetas(betas = betas, priorsBetas = priors.betas)
              iterator.betas <- BetaIterator(dim = dim, margins = margins)
              dims <- makeDims(dim = dim, margins = margins)
              cellInLik <- rep(TRUE, times = length(theta))
              model <- methods::new("BinomialVarying",
                                    call = call,
                                    theta = theta,
                                    metadataY = metadataY,
                                    cellInLik = cellInLik,
                                    scaleTheta = scale.theta,
                                    scaleThetaMultiplier = scale.theta.multiplier,
                                    nAcceptTheta = methods::new("Counter", 0L),
                                    nFailedPropTheta = methods::new("Counter", 0L),
                                    sigma = sigma,
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
              model <- addAg(model = model,
                             aggregate = aggregate,
                             defaultWeights = default.weights)
              model <- makeCellInLik(model = model,
                                     y = y)
              model
          })


## HAS_TESTS
setMethod("initialModel",
          signature(object = "SpecCMPVarying",
                    y = "Counts",
                    exposure = "ANY",
                    weights = "missing"),
          function(object, y, exposure) {
              call <- object@call
              formula.mu <- object@formulaMu
              specs.priors <- object@specsPriors
              names.specs.priors <- object@namesSpecsPriors
              structural.zeros <- object@structuralZeros
              box.cox.param <- object@boxCoxParam
              scale.theta <- object@scaleTheta
              lower <- object@lower
              upper <- object@upper
              tolerance <- object@tolerance
              max.attempt <- object@maxAttempt
              nu.sigma <- object@nuSigma
              A.sigma <- object@ASigma@.Data
              mult.sigma <- object@multSigma
              sigma.max <- object@sigmaMax@.Data
              use.expose <- object@useExpose@.Data
              aggregate <- object@aggregate
              meanLogNuCMP <- object@meanLogNuCMP
              sdLogNuCMP <- object@sdLogNuCMP
              checkTermsFromFormulaFound(y = y, formula = formula.mu)
              checkLengthDimInFormula(y = y, formula = formula.mu)
              metadataY <- y@metadata
              dim <- dim(y)
              struc.zero.array <- makeStrucZeroArray(structuralZeros = structural.zeros, 
                                                     y = y) 
              y <- checkAndTidyYForStrucZero(y = y, 
                                             strucZeroArray = struc.zero.array) 
              has.exposure <- !is.null(exposure)
              if (has.exposure && !use.expose)
                  stop(gettextf("'%s' argument supplied, but model '%s' does not use exposure",
                                "exposure", deparse(call[[2L]])))
              if (!has.exposure && use.expose)
                  stop(gettextf("model '%s' uses exposure, but no '%s' argument supplied",
                                deparse(call[[2L]]), "exposure"))
              y.missing <- is.na(y@.Data)
              is.obs <- !is.na(y@.Data) & (struc.zero.array != 0L) 
              if (any(is.obs))
                  mean.y.obs <- mean(y@.Data[is.obs]) 
              else
                  mean.y.obs <- 0.5
              shape <- ifelse(is.obs, 0.05 * mean.y.obs + 0.95 * y@.Data, mean.y.obs)
              if (has.exposure) {
                  mean.expose.obs <- mean(exposure[is.obs])
                  rate <- ifelse(is.obs, 0.05 * mean.expose.obs + 0.95 * exposure, mean.expose.obs)
              }
              else
                  rate <- 1
              scale.theta.multiplier <- sqrt(mean.y.obs + 1)
              scale.theta.multiplier <- methods::new("Scale", scale.theta.multiplier)
              theta <- stats::rgamma(n = length(y), shape = shape, rate = rate)
              if (has.exposure)
                  sY <- NULL
              else
                  sY <-  stats::sd(log(as.numeric(y) + 1), na.rm = TRUE)
              A.sigma <- makeASigma(A = A.sigma,
                                    sY = sY,
                                    mult = mult.sigma)
              sigma.max <- makeScaleMax(scaleMax = sigma.max,
                                        A = A.sigma,
                                        nu = nu.sigma)
              sigma <- stats::runif(n = 1L,
                                    min = 0,
                                    max = min(A.sigma@.Data, sigma.max@.Data))
              sigma <- methods::new("Scale", sigma)
              ## need to avoid having all 'theta' equalling lower or upper bound
              is.too.low <- theta < lower
              n.too.low <- sum(is.too.low)
              width <- 0.05 * (upper - lower)
              if (is.infinite(width))
                  width <- 100
              theta[is.too.low] <- stats::runif(n = n.too.low, min = lower, max = lower + width)
              is.too.high <- theta > upper
              n.too.high <- sum(is.too.high)
              theta[is.too.high] <- stats::runif(n = n.too.high, min = upper - width, max = upper)
              if (box.cox.param > 0) {
                  lower <- (lower ^ box.cox.param - 1) / box.cox.param
                  upper <- (upper ^ box.cox.param - 1) / box.cox.param
              }
              else {
                  lower <- log(lower)
                  upper <- log(upper)
              }
              theta <- array(theta, dim = dim(y), dimnames = dimnames(y))
              if (formulaIsInterceptOnly(formula.mu))
                  betas <- list("(Intercept)" = mean(log(theta)))
              else {
                  betas <- MASS::loglm(formula.mu, data = theta)$param
                  betas <- convertToFormulaOrder(betas = betas, formulaMu = formula.mu)
              }
              theta <- as.numeric(theta)
              theta[struc.zero.array == 0L] <- NA
              meanLogNuCMP <- methods::new("Parameter", meanLogNuCMP)
              sdLogNuCMP <- methods::new("Scale", sdLogNuCMP)
              logNuCMP <- stats::rnorm(n = length(theta),
                                       mean = meanLogNuCMP@.Data,
                                       sd = sdLogNuCMP@.Data)
              nuCMP <- exp(logNuCMP)
              nuCMP[nuCMP < 0.5] <- 0.5
              nuCMP[nuCMP > 2] <- 2
              nuCMP <- new("ParameterVector", nuCMP)              
              names.betas <- names(betas)
              margins <- makeMargins(betas = betas, y = y)
              priors.betas <- makePriors(betas = betas,
                                         specs = specs.priors,
                                         namesSpecs = names.specs.priors,
                                         margins = margins,
                                         y = y,
                                         sY = sY,
                                         strucZeroArray = struc.zero.array)
              is.saturated <- sapply(priors.betas, function(x) x@isSaturated@.Data)
              if (any(is.saturated)) {
                  i.saturated <- which(is.saturated)
                  prior.saturated <- priors.betas[[i.saturated]]
                  A.sigma <- prior.saturated@ATau
                  nu.sigma <- prior.saturated@nuTau
                  sigma.max <- prior.saturated@tauMax
                  sigma <- prior.saturated@tau
              }
              betas <- unname(lapply(betas, as.numeric))
              betas <- jitterBetas(betas = betas,
                                   priorsBetas = priors.betas)
              iterator.betas <- BetaIterator(dim = dim, margins = margins)
              dims <- makeDims(dim = dim, margins = margins)
              class <- if (has.exposure) "CMPVaryingUseExp" else "CMPVaryingNotUseExp"
              cellInLik <- rep(TRUE, times = length(theta))
              model <- methods::new(class,
                                    call = call,
                                    theta = theta,
                                    cellInLik = cellInLik,
                                    strucZeroArray = struc.zero.array,
                                    metadataY = metadataY,
                                    scaleTheta = scale.theta,
                                    scaleThetaMultiplier = scale.theta.multiplier,
                                    boxCoxParam = box.cox.param,
                                    lower = lower,
                                    upper = upper,
                                    tolerance = tolerance,
                                    nAcceptTheta = methods::new("Counter", 0L),
                                    nFailedPropTheta = methods::new("Counter", 0L),
                                    nFailedPropYStar = methods::new("Counter", 0L), ## added 10/1/2018 JAH
                                    maxAttempt = max.attempt,
                                    sigma = sigma,
                                    sigmaMax = sigma.max,
                                    ASigma = A.sigma,
                                    nuSigma = nu.sigma,
                                    sdLogNuCMP = sdLogNuCMP,
                                    meanLogNuCMP = meanLogNuCMP,
                                    nuCMP = nuCMP,
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
              model <- addAg(model = model,
                             aggregate = aggregate,
                             defaultWeights = default.weights)
              model <- makeCellInLik(model = model,
                                     y = y,
                                     strucZeroArray = struc.zero.array)
              model
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
              varsigmaSetToZero <- object@varsigmaSetToZero
              nu.sigma <- object@nuSigma
              A.sigma <- object@ASigma@.Data
              mult.sigma <- object@multSigma
              sigma.max <- object@sigmaMax@.Data
              aggregate <- object@aggregate
              checkTermsFromFormulaFound(y = y, formula = formula.mu)
              checkLengthDimInFormula(y = y, formula = formula.mu)
              metadataY <- y@metadata
              dim <- dim(y)
              w <- as.numeric(weights)
              n <- length(y)
              sY <- stats::sd(as.numeric(y), na.rm = TRUE) ## to deal with R <3.0 behaviour
              A.sigma <- makeASigma(A = A.sigma,
                                    sY = sY,
                                    mult = mult.sigma)
              sigma.max <- makeScaleMax(scaleMax = sigma.max,
                                        A = A.sigma,
                                        nu = nu.sigma)
              sigma <- stats::runif(n = 1L,
                                    min = 0,
                                    max = min(A.sigma@.Data, sigma.max@.Data))
              sigma <- methods::new("Scale", sigma)
              if (varsigmaSetToZero@.Data)
                  theta <- as.numeric(y)
              else {
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
              }
              theta <- array(theta, dim = dim(y), dimnames = dimnames(y))
              betas <- makeLinearBetas(theta = theta, formula = formula.mu)
              theta <- as.numeric(theta)
              names.betas <- names(betas)
              margins <- makeMargins(betas = betas, y = y)
              struc.zero.array <- makeStrucZeroArray(structuralZeros = NULL,
                                                     y = y) 
              priors.betas <- makePriors(betas = betas,
                                         specs = specs.priors,
                                         namesSpecs = names.specs.priors,
                                         margins = margins,
                                         y = y,
                                         sY = sY,
                                         strucZeroArray = struc.zero.array)
              is.saturated <- sapply(priors.betas, function(x) x@isSaturated@.Data)
              if (any(is.saturated)) {
                  i.saturated <- which(is.saturated)
                  prior.saturated <- priors.betas[[i.saturated]]
                  A.sigma <- prior.saturated@ATau
                  nu.sigma <- prior.saturated@nuTau
                  sigma.max <- prior.saturated@tauMax
                  sigma <- prior.saturated@tau
              }
              betas <- unname(lapply(betas, as.numeric))
              betas <- jitterBetas(betas = betas, priorsBetas = priors.betas)
              iterator.betas <- BetaIterator(dim = dim, margins = margins)
              dims <- makeDims(dim = dim, margins = margins)
              cellInLik <- rep(TRUE, times = length(theta))
              model <- methods::new("NormalVaryingVarsigmaKnown",
                                    call = call,
                                    theta = theta,
                                    metadataY = metadataY,
                                    cellInLik = cellInLik,
                                    w = w,
                                    varsigma = varsigma,
                                    varsigmaSetToZero = varsigmaSetToZero,
                                    lower = lower,
                                    upper = upper,
                                    tolerance = tolerance,
                                    scaleTheta = scale.theta,
                                    nAcceptTheta = methods::new("Counter", 0L),
                                    nFailedPropTheta = methods::new("Counter", 0L),
                                    maxAttempt = max.attempt,
                                    sigma = sigma,
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
              model <- addAg(model = model,
                             aggregate = aggregate,
                             defaultWeights = default.weights)
              model <- makeCellInLik(model = model,
                                     y = y)
              model
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
              mult.sigma <- object@multSigma
              sigma.max <- object@sigmaMax@.Data
              aggregate <- object@aggregate
              checkTermsFromFormulaFound(y = y, formula = formula.mu)
              checkLengthDimInFormula(y = y, formula = formula.mu)
              metadataY <- y@metadata
              dim <- dim(y)
              w <- as.numeric(weights)
              n <- length(y)
              sY <- stats::sd(as.numeric(y), na.rm = TRUE) ## to deal with R <3.0 behaviour
              A.varsigma <- makeASigma(A = A.varsigma,
                                       sY = sY,
                                       mult = mult.sigma)
              A.sigma <- makeASigma(A = A.sigma,
                                    sY = sY,
                                    mult = mult.sigma)
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
              sigma <- methods::new("Scale", sigma)
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
              theta <- as.numeric(theta)
              names.betas <- names(betas)
              margins <- makeMargins(betas = betas, y = y)
              struc.zero.array <- makeStrucZeroArray(structuralZeros = NULL,
                                                     y = y) 
              priors.betas <- makePriors(betas = betas,
                                         specs = specs.priors,
                                         namesSpecs = names.specs.priors,
                                         margins = margins,
                                         y = y,
                                         sY = sY,
                                         strucZeroArray = struc.zero.array)
              is.saturated <- sapply(priors.betas, function(x) x@isSaturated@.Data)
              if (any(is.saturated)) {
                  i.saturated <- which(is.saturated)
                  prior.saturated <- priors.betas[[i.saturated]]
                  A.sigma <- prior.saturated@ATau
                  nu.sigma <- prior.saturated@nuTau
                  sigma.max <- prior.saturated@tauMax
                  sigma <- prior.saturated@tau
              }
              betas <- unname(lapply(betas, as.numeric))
              betas <- jitterBetas(betas = betas, priorsBetas = priors.betas)
              iterator.betas <- BetaIterator(dim = dim, margins = margins)
              dims <- makeDims(dim = dim, margins = margins)
              cellInLik <- rep(TRUE, times = length(theta))
              model <- methods::new("NormalVaryingVarsigmaUnknown",
                                    call = call,
                                    theta = theta,
                                    metadataY = metadataY,
                                    cellInLik = cellInLik,
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
                                    sigma = sigma,
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
              model <- addAg(model = model,
                             aggregate = aggregate,
                             defaultWeights = default.weights)
              model <- makeCellInLik(model = model,
                                     y = y)
              model
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
              structural.zeros <- object@structuralZeros
              box.cox.param <- object@boxCoxParam
              scale.theta <- object@scaleTheta
              lower <- object@lower
              upper <- object@upper
              tolerance <- object@tolerance
              max.attempt <- object@maxAttempt
              nu.sigma <- object@nuSigma
              A.sigma <- object@ASigma@.Data
              mult.sigma <- object@multSigma
              sigma.max <- object@sigmaMax@.Data
              use.expose <- object@useExpose@.Data
              aggregate <- object@aggregate
              checkTermsFromFormulaFound(y = y, formula = formula.mu)
              checkLengthDimInFormula(y = y, formula = formula.mu)
              metadataY <- y@metadata
              dim <- dim(y)
              struc.zero.array <- makeStrucZeroArray(structuralZeros = structural.zeros, 
                                                     y = y) 
              y <- checkAndTidyYForStrucZero(y = y, 
                                             strucZeroArray = struc.zero.array) 
              has.exposure <- !is.null(exposure)
              if (has.exposure && !use.expose)
                  stop(gettextf("'%s' argument supplied, but model '%s' does not use exposure",
                                "exposure", deparse(call[[2L]])))
              if (!has.exposure && use.expose)
                  stop(gettextf("model '%s' uses exposure, but no '%s' argument supplied",
                                deparse(call[[2L]]), "exposure"))
              is.obs <- !is.na(y@.Data) & (struc.zero.array != 0L) 
              if (any(is.obs))
                  mean.y.obs <- mean(y@.Data[is.obs]) 
              else
                  mean.y.obs <- 0.5
              ## shape <- ifelse(is.obs, 0.05 * mean.y.obs + 0.95 * y@.Data, mean.y.obs)
              shape <- ifelse(is.obs, 0.5 * mean.y.obs + 0.5 * y@.Data, mean.y.obs)
              if (has.exposure) {
                  mean.expose.obs <- mean(exposure[is.obs])
                  ## rate <- ifelse(is.obs, 0.05 * mean.expose.obs + 0.95 * exposure, mean.expose.obs)
                  rate <- ifelse(is.obs, 0.5 * mean.expose.obs + 0.5 * exposure, mean.expose.obs)
              }
              else
                  rate <- 1
              scale.theta.multiplier <- sqrt(mean.y.obs + 1)
              scale.theta.multiplier <- methods::new("Scale", scale.theta.multiplier)
              theta <- stats::rgamma(n = length(y), shape = shape, rate = rate)
              if (has.exposure)
                  sY <- NULL
              else
                  sY <-  stats::sd(log(as.numeric(y) + 1), na.rm = TRUE)
              A.sigma <- makeASigma(A = A.sigma,
                                    sY = sY,
                                    mult = mult.sigma)
              sigma.max <- makeScaleMax(scaleMax = sigma.max,
                                        A = A.sigma,
                                        nu = nu.sigma)
              sigma <- stats::runif(n = 1L,
                                    min = 0,
                                    max = min(A.sigma@.Data, sigma.max@.Data))
              sigma <- methods::new("Scale", sigma)
              ## need to avoid having all 'theta' equalling lower or upper bound
              is.too.low <- theta < lower
              n.too.low <- sum(is.too.low)
              ## width <- 0.05 * (upper - lower)
              width <- 0.2 * (upper - lower)
              if (is.infinite(width))
                  width <- 100
              theta[is.too.low] <- stats::runif(n = n.too.low, min = lower, max = lower + width)
              is.too.high <- theta > upper
              n.too.high <- sum(is.too.high)
              theta[is.too.high] <- stats::runif(n = n.too.high, min = upper - width, max = upper)
              if (box.cox.param > 0) {
                  lower <- (lower ^ box.cox.param - 1) / box.cox.param
                  upper <- (upper ^ box.cox.param - 1) / box.cox.param
              }
              else {
                  lower <- log(lower)
                  upper <- log(upper)
              }
              theta <- array(theta, dim = dim(y), dimnames = dimnames(y))
              if (formulaIsInterceptOnly(formula.mu))
                  betas <- list("(Intercept)" = mean(log(theta)))
              else {
                  betas <- MASS::loglm(formula.mu, data = theta)$param
                  betas <- convertToFormulaOrder(betas = betas, formulaMu = formula.mu)
              }
              theta <- as.numeric(theta)
              theta[struc.zero.array == 0L] <- NA
              names.betas <- names(betas)
              margins <- makeMargins(betas = betas, y = y)
              priors.betas <- makePriors(betas = betas,
                                         specs = specs.priors,
                                         namesSpecs = names.specs.priors,
                                         margins = margins,
                                         y = y,
                                         sY = sY,
                                         strucZeroArray = struc.zero.array)
              is.saturated <- sapply(priors.betas, function(x) x@isSaturated@.Data)
              if (any(is.saturated)) {
                  i.saturated <- which(is.saturated)
                  prior.saturated <- priors.betas[[i.saturated]]
                  A.sigma <- prior.saturated@ATau
                  nu.sigma <- prior.saturated@nuTau
                  sigma.max <- prior.saturated@tauMax
                  sigma <- prior.saturated@tau
              }
              betas <- unname(lapply(betas, as.numeric))
              betas <- jitterBetas(betas = betas,
                                   priorsBetas = priors.betas)
              iterator.betas <- BetaIterator(dim = dim, margins = margins)
              dims <- makeDims(dim = dim, margins = margins)
              class <- if (has.exposure) "PoissonVaryingUseExp" else "PoissonVaryingNotUseExp"
              cellInLik <- rep(TRUE, times = length(theta))
              model <- methods::new(class,
                                    call = call,
                                    theta = theta,
                                    cellInLik = cellInLik,
                                    strucZeroArray = struc.zero.array,
                                    metadataY = metadataY,
                                    scaleTheta = scale.theta,
                                    scaleThetaMultiplier = scale.theta.multiplier,
                                    boxCoxParam = box.cox.param,
                                    lower = lower,
                                    upper = upper,
                                    tolerance = tolerance,
                                    nAcceptTheta = methods::new("Counter", 0L),
                                    nFailedPropTheta = methods::new("Counter", 0L),
                                    maxAttempt = max.attempt,
                                    sigma = sigma,
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
              model <- addAg(model = model,
                             aggregate = aggregate,
                             defaultWeights = default.weights)
              model <- makeCellInLik(model = model,
                                     y = y,
                                     strucZeroArray = struc.zero.array)
              model
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

## HAS_TESTS
setMethod("initialModel",
          signature(object = "SpecNormalFixed",
                    y = "DemographicArray",
                    exposure = "ANY",
                    weights = "missing"),
          function(object, y, exposure) {
              call <- object@call
              meanAll <- object@mean
              sdAll <- object@sd
              metadataAll <- object@metadata
              metadataY <- y@metadata
              use.expose <- object@useExpose@.Data
              .Data.mean <- array(meanAll@.Data,
                                  dim = dim(metadataAll),
                                  dimnames = dimnames(metadataAll))
              .Data.sd <- array(sdAll@.Data,
                                dim = dim(metadataAll),
                                dimnames = dimnames(metadataAll))
              mean.before.subset <- new("Values",
                                        .Data = .Data.mean,
                                        metadata = metadataAll)
              sd.before.subset <- new("Values",
                                      .Data = .Data.sd,
                                      metadata = metadataAll)
              mean <- tryCatch(makeCompatible(x = mean.before.subset,
                                              y = y,
                                              subset = TRUE,
                                              check = TRUE),
                               error = function(e) e)
              if (methods::is(mean, "error"))
                  stop(gettextf("'%s' from %s model not compatible with data : %s",
                                "mean", "NormalFixed", mean$message))
              sd <- makeCompatible(x = sd.before.subset,
                                   y = y,
                                   subset = TRUE,
                                   check = FALSE)
              mean <- new("ParameterVector", mean@.Data)
              sd <- new("ScaleVec", sd@.Data)
              has.exposure <- !is.null(exposure)
              if (has.exposure && !use.expose)
                  stop(gettextf("'%s' argument supplied, but model '%s' does not use exposure",
                                "exposure", deparse(call[[2L]])))
              if (!has.exposure && use.expose)
                  stop(gettextf("model '%s' uses exposure, but no '%s' argument supplied",
                                deparse(call[[2L]]), "exposure"))
              if (has.exposure)
                  class <- "NormalFixedUseExp"
              else
                  class <- "NormalFixedNotUseExp"
              methods::new(class,
                           call = call,
                           mean = mean,
                           sd = sd,
                           metadataY = metadataY,
                           meanAll = meanAll,
                           sdAll = sdAll,
                           metadataAll = metadataAll)
          })

## HAS_TESTS
setMethod("initialModel",
          signature(object = "SpecRound3",
                    y = "Counts",
                    exposure = "Counts",
                    weights = "missing"),
          function(object, y, exposure) {
              call <- object@call
              metadataY <- y@metadata
              if (any((y@.Data[!is.na(y@.Data)] %% 3L) != 0L))
                  stop(gettextf("using '%s' data model, but data contains values not divisible by %d",
                                "Round3", 3L))
              methods::new("Round3",
                           call = call,
                           metadataY = metadataY)
          })
## HAS_TESTS
setMethod("initialModel",
          signature(object = "SpecTFixed",
                    y = "DemographicArray",
                    exposure = "ANY",
                    weights = "missing"),
          function(object, y, exposure) {
              call <- object@call
              meanAll <- object@mean
              sdAll <- object@sd
              metadataAll <- object@metadata
              nu <- object@nu
              metadataY <- y@metadata
              use.expose <- object@useExpose@.Data
              .Data.mean <- array(meanAll@.Data,
                                  dim = dim(metadataAll),
                                  dimnames = dimnames(metadataAll))
              .Data.sd <- array(sdAll@.Data,
                                dim = dim(metadataAll),
                                dimnames = dimnames(metadataAll))
              mean.before.subset <- new("Values",
                                        .Data = .Data.mean,
                                        metadata = metadataAll)
              sd.before.subset <- new("Values",
                                      .Data = .Data.sd,
                                      metadata = metadataAll)
              mean <- tryCatch(makeCompatible(x = mean.before.subset,
                                              y = y,
                                              subset = TRUE,
                                              check = TRUE),
                               error = function(e) e)
              if (methods::is(mean, "error"))
                  stop(gettextf("'%s' from %s model not compatible with data : %s",
                                "location", "TFixed", mean$message))
              sd <- makeCompatible(x = sd.before.subset,
                                   y = y,
                                   subset = TRUE,
                                   check = FALSE)
              mean <- new("ParameterVector", mean@.Data)
              sd <- new("ScaleVec", sd@.Data)
              has.exposure <- !is.null(exposure)
              if (has.exposure && !use.expose)
                  stop(gettextf("'%s' argument supplied, but model '%s' does not use exposure",
                                "exposure", deparse(call[[2L]])))
              if (!has.exposure && use.expose)
                  stop(gettextf("model '%s' uses exposure, but no '%s' argument supplied",
                                deparse(call[[2L]]), "exposure"))
              if (has.exposure)
                  class <- "TFixedUseExp"
              else
                  class <- "TFixedNotUseExp"
              methods::new(class,
                           call = call,
                           mean = mean,
                           sd = sd,
                           nu = nu,
                           metadataY = metadataY,
                           meanAll = meanAll,
                           sdAll = sdAll,
                           metadataAll = metadataAll)
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
                                  cellInLik = l$cellInLik,
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
                  ans <- makeCellInLik(ans)
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
              box.cox.param <- model@boxCoxParam
              if (box.cox.param > 0) {
                  if (is.null(lower))
                      lower <- (box.cox.param * model@lower + 1) ^ (1 / box.cox.param)
                  if (is.null(upper))
                      upper <- (box.cox.param * model@upper + 1) ^ (1 / box.cox.param)
              }
              else {
                  if (is.null(lower))
                      lower <- exp(model@lower)
                  if (is.null(upper))
                      upper <- exp(model@upper)
              }
              checkLowerAndUpper(lower = lower,
                                 upper = upper,
                                 distribution = "Poisson")
              lower <- model@lower
              upper <- model@upper
              uses.exposure <- methods::is(model, "UseExposure")
              if (uses.exposure)
                  Class <- "PoissonVaryingUseExpPredict"
              else
                  Class <- "PoissonVaryingNotUseExpPredict"
              ans <- methods::new(Class,
                                  model,
                                  theta = l$theta,
                                  metadataY = metadataY,
                                  cellInLik = l$cellInLik,
                                  nAcceptTheta = methods::new("Counter", 0L),
                                  lower = lower,
                                  upper = upper,
                                  nFailedPropTheta = methods::new("Counter", 0L),
                                  betas = l$betas,
                                  priorsBetas = l$priorsBetas,
                                  strucZeroArray = l$strucZeroArray,
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
                  ans <- makeCellInLik(model = ans,
                                       strucZeroArray = l$strucZeroArray)
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
                             cellInLik = l$cellInLik,
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
                             cellInLik = l$cellInLik,
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
                  ans <- makeCellInLik(ans)
              }
              ans
          })

## HAS_TESTS
setMethod("initialModelPredict",
          signature(model = "CMPVarying"),
          function(model, along, labels, n, offsetModel,
                   covariates, aggregate, lower, upper) {
              l <- initialModelPredictHelper(model = model,
                                             along = along,
                                             labels = labels,
                                             n = n,
                                             offsetModel = offsetModel,
                                             covariates = covariates)
              nu.cmp <- new("ParameterVector", rep(1, times = length(l$theta)))
              metadataY <- l$metadataY
              box.cox.param <- model@boxCoxParam
              if (box.cox.param > 0) {
                  if (is.null(lower))
                      lower <- (box.cox.param * model@lower + 1) ^ (1 / box.cox.param)
                  if (is.null(upper))
                      upper <- (box.cox.param * model@upper + 1) ^ (1 / box.cox.param)
              }
              else {
                  if (is.null(lower))
                      lower <- exp(model@lower)
                  if (is.null(upper))
                      upper <- exp(model@upper)
              }
              checkLowerAndUpper(lower = lower,
                                 upper = upper,
                                 distribution = "Poisson")
              lower <- model@lower
              upper <- model@upper
              uses.exposure <- methods::is(model, "UseExposure")
              if (uses.exposure)
                  Class <- "CMPVaryingUseExpPredict"
              else
                  Class <- "CMPVaryingNotUseExpPredict"
              ans <- methods::new(Class,
                                  model,
                                  theta = l$theta,
                                  nuCMP = nu.cmp,
                                  metadataY = metadataY,
                                  cellInLik = l$cellInLik,
                                  nAcceptTheta = methods::new("Counter", 0L),
                                  lower = lower,
                                  upper = upper,
                                  nFailedPropTheta = methods::new("Counter", 0L),
                                  betas = l$betas,
                                  priorsBetas = l$priorsBetas,
                                  strucZeroArray = l$strucZeroArray,
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
                  ans <- makeCellInLik(model = ans,
                                       strucZeroArray = l$strucZeroArray)
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

## HAS_TESTS
setMethod("initialModelPredict",
          signature(model = "Round3"),
          function(model, along, labels, n, offsetModel,
                   covariates, aggregate, lower, upper) {
              metadata.first <- model@metadataY
              i.method.model.first <- model@iMethodModel
              metadata.second <- makeMetadataPredict(metadata = metadata.first,
                                                     along = along,
                                                     labels = labels,
                                                     n = n)
              i.method.model.second <- i.method.model.first + 100L
              methods::new("Round3Predict",
                           metadataY = metadata.second,
                           iMethodModel = i.method.model.second)
          })

## HAS_TESTS
setMethod("initialModelPredict",
          signature(model = "NormalFixed"),
          function(model, along, labels, n, offsetModel,
                   covariates, aggregate, lower, upper) {
              i.method.model.first <- model@iMethodModel
              metadata.first <- model@metadataY
              meanAll <- model@meanAll
              sdAll <- model@sdAll
              metadataAll <- model@metadataAll
              metadata.second <- makeMetadataPredict(metadata = metadata.first,
                                                     along = along,
                                                     labels = labels,
                                                     n = n)
              .Data.mean <- array(meanAll@.Data,
                                  dim = dim(metadataAll),
                                  dimnames = dimnames(metadataAll))
              .Data.sd <- array(sdAll@.Data,
                                dim = dim(metadataAll),
                                dimnames = dimnames(metadataAll))
              .Data.second <- array(0L,
                                    dim = dim(metadata.second),
                                    dimnames = dimnames(metadata.second))
              mean.before.subset <- new("Values",
                                        .Data = .Data.mean,
                                        metadata = metadataAll)
              sd.before.subset <- new("Values",
                                      .Data = .Data.sd,
                                      metadata = metadataAll)
              y.second <- new("Values",
                              .Data = .Data.second,
                              metadata = metadata.second)
              mean <- tryCatch(makeCompatible(x = mean.before.subset,
                                              y = y.second,
                                              subset = TRUE,
                                              check = TRUE),
                               error = function(e) e)
              if (methods::is(mean, "error"))
                  stop(gettextf("'%s' from %s model not compatible with data : %s",
                                "mean", "NormalFixed", mean$message))
              ## check that don't need to expand 'mean' to make compatible with 'y'
              value <- tryCatch(makeCompatible(x = as(mean.before.subset, "Counts"),
                                               y = y.second,
                                               subset = TRUE,
                                               check = TRUE),
                                error = function(e) e)
              if (methods::is(value, "error"))
                  stop(gettextf("'%s' from %s model not compatible with data : %s",
                                "mean", "NormalFixed", value$message))
              sd <- makeCompatible(x = sd.before.subset,
                                   y = y.second,
                                   subset = TRUE,
                                   check = FALSE)
              mean <- new("ParameterVector", mean@.Data)
              sd <- new("ScaleVec", sd@.Data)
              class <- paste0(class(model), "Predict")
              i.method.model.second <- i.method.model.first + 100L
              methods::new(class,
                           mean = model@mean,
                           sd = model@sd,
                           metadataY = metadata.second,
                           meanAll = meanAll,
                           sdAll = sdAll,
                           metadataAll = metadataAll,
                           iMethodModel = i.method.model.second)
          })


## HAS_TESTS
setMethod("initialModelPredict",
          signature(model = "TFixed"),
          function(model, along, labels, n, offsetModel,
                   covariates, aggregate, lower, upper) {
              i.method.model.first <- model@iMethodModel
              metadata.first <- model@metadataY
              meanAll <- model@meanAll
              sdAll <- model@sdAll
              nu <- model@nu
              metadataAll <- model@metadataAll
              metadata.second <- makeMetadataPredict(metadata = metadata.first,
                                                     along = along,
                                                     labels = labels,
                                                     n = n)
              .Data.mean <- array(meanAll@.Data,
                                  dim = dim(metadataAll),
                                  dimnames = dimnames(metadataAll))
              .Data.sd <- array(sdAll@.Data,
                                dim = dim(metadataAll),
                                dimnames = dimnames(metadataAll))
              .Data.second <- array(0L,
                                    dim = dim(metadata.second),
                                    dimnames = dimnames(metadata.second))
              mean.before.subset <- new("Values",
                                        .Data = .Data.mean,
                                        metadata = metadataAll)
              sd.before.subset <- new("Values",
                                      .Data = .Data.sd,
                                      metadata = metadataAll)
              y.second <- new("Values",
                              .Data = .Data.second,
                              metadata = metadata.second)
              mean <- tryCatch(makeCompatible(x = mean.before.subset,
                                              y = y.second,
                                              subset = TRUE,
                                              check = TRUE),
                               error = function(e) e)
              if (methods::is(mean, "error"))
                  stop(gettextf("'%s' from %s model not compatible with data : %s",
                                "location", "TFixed", mean$message))
              ## check that don't need to expand 'mean' to make compatible with 'y'
              value <- tryCatch(makeCompatible(x = as(mean.before.subset, "Counts"),
                                               y = y.second,
                                               subset = TRUE,
                                               check = TRUE),
                                error = function(e) e)
              if (methods::is(value, "error"))
                  stop(gettextf("'%s' from %s model not compatible with data : %s",
                                "location", "TFixed", value$message))
              sd <- makeCompatible(x = sd.before.subset,
                                   y = y.second,
                                   subset = TRUE,
                                   check = FALSE)
              mean <- new("ParameterVector", mean@.Data)
              sd <- new("ScaleVec", sd@.Data)
              class <- paste0(class(model), "Predict")
              i.method.model.second <- i.method.model.first + 100L
              methods::new(class,
                           mean = model@mean,
                           sd = model@sd,
                           nu = nu,
                           metadataY = metadata.second,
                           meanAll = meanAll,
                           sdAll = sdAll,
                           metadataAll = metadataAll,
                           iMethodModel = i.method.model.second)
          })




