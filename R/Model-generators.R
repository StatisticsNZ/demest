

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
              model@thetaTransformed <- log(model@theta / (1 - model@theta))
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
              model@thetaTransformed <- model@theta
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
              model@thetaTransformed <- model@theta
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
              box.cox.param <- model@boxCoxParam
              if (box.cox.param > 0)
                  model@thetaTransformed <- (model@theta ^ box.cox.param - 1) / box.cox.param
              else
                  model@thetaTransformed <- log(model@theta)
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
              box.cox.param <- model@boxCoxParam
              if (box.cox.param > 0)
                  model@thetaTransformed <- (model@theta ^ box.cox.param - 1) / box.cox.param
              else
                  model@thetaTransformed <- log(model@theta)
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
              A.sigma <- methods::new("Scale", A.sigma)
              nu.sigma <- methods::new("DegreesFreedom", nu.sigma)
              sigma.max <- methods::new("Scale", sigma.max)
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
              theta.transformed <- log(theta / (1 - theta))
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
              val.betas <- lapply(betas, function(x) rep(0, length(x)))
              fun <- function(x) x@isZeroVar@.Data || x@isSaturated@.Data
              beta.equals.mean <- sapply(priors.betas, fun)
              iterator.betas <- BetaIterator(dim = dim, margins = margins)
              dims <- makeDims(dim = dim, margins = margins)
              mu <- makeMu(n = length(theta),
                           betas = betas,
                           iterator = iterator.betas,
                           useC = TRUE)
              cellInLik <- rep(TRUE, times = length(theta))
              model <- methods::new("BinomialVarying",
                                    call = call,
                                    theta = theta,
                                    thetaTransformed = theta.transformed,
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
                                    meansBetas = val.betas,
                                    variancesBetas = val.betas,
                                    priorsBetas = priors.betas,
                                    betaEqualsMean = beta.equals.mean,
                                    namesBetas = names.betas,
                                    margins = margins,
                                    iteratorBetas = iterator.betas,
                                    dims = dims,
                                    mu = mu)
              default.weights <- exposure
              model <- addAg(model = model,
                             aggregate = aggregate,
                             defaultWeights = default.weights)
              model <- makeCellInLik(model = model,
                                     y = y)
              model <- updateMu(model, useC = TRUE)
              model <- updateMeansBetas(model, useC = TRUE)
              model <- updateVariancesBetas(model, useC = TRUE)
              model <- updateLogPostBetas(model, useC = TRUE)
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
              if (box.cox.param > 0)
                  theta.transformed <- (theta ^ box.cox.param - 1) / box.cox.param
              else
                  theta.transformed <- log(theta)
              meanLogNuCMP <- methods::new("Parameter", meanLogNuCMP)
              sdLogNuCMP <- methods::new("Scale", sdLogNuCMP)
              logNuCMP <- stats::rnorm(n = length(theta),
                                       mean = meanLogNuCMP@.Data,
                                       sd = sdLogNuCMP@.Data)
              nuCMP <- exp(logNuCMP)
              nuCMP[nuCMP < 0.5] <- 0.5
              nuCMP[nuCMP > 2] <- 2
              nuCMP <- methods::new("ParameterVector", nuCMP)              
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
              val.betas <- lapply(betas, function(x) ifelse(is.na(x), as.double(NA), 0))
              fun <- function(x) x@isZeroVar@.Data || x@isSaturated@.Data
              beta.equals.mean <- sapply(priors.betas, fun)
              iterator.betas <- BetaIterator(dim = dim, margins = margins)
              dims <- makeDims(dim = dim, margins = margins)
              mu <- makeMu(n = length(theta),
                           betas = betas,
                           iterator = iterator.betas,
                           useC = TRUE)
              class <- if (has.exposure) "CMPVaryingUseExp" else "CMPVaryingNotUseExp"
              cellInLik <- rep(TRUE, times = length(theta))
              model <- methods::new(class,
                                    call = call,
                                    theta = theta,
                                    thetaTransformed = theta.transformed,
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
                                    meansBetas = val.betas,
                                    variancesBetas = val.betas,
                                    priorsBetas = priors.betas,
                                    betaEqualsMean = beta.equals.mean,
                                    namesBetas = names.betas,
                                    margins = margins,
                                    iteratorBetas = iterator.betas,
                                    dims = dims,
                                    mu = mu)
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
              model <- updateMu(model, useC = TRUE)
              model <- updateMeansBetas(model, useC = TRUE)
              model <- updateVariancesBetas(model, useC = TRUE)
              model <- updateLogPostBetas(model, useC = TRUE)
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
              val.betas <- lapply(betas, function(x) rep(0, length(x)))
              fun <- function(x) x@isZeroVar@.Data || x@isSaturated@.Data
              beta.equals.mean <- sapply(priors.betas, fun)
              iterator.betas <- BetaIterator(dim = dim, margins = margins)
              dims <- makeDims(dim = dim, margins = margins)
              mu <- makeMu(n = length(theta),
                           betas = betas,
                           iterator = iterator.betas,
                           useC = TRUE)
              cellInLik <- rep(TRUE, times = length(theta))
              model <- methods::new("NormalVaryingVarsigmaKnown",
                                    call = call,
                                    theta = theta,
                                    thetaTransformed = theta,
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
                                    meansBetas = val.betas,
                                    variancesBetas = val.betas,
                                    priorsBetas = priors.betas,
                                    betaEqualsMean = beta.equals.mean,
                                    namesBetas = names.betas,
                                    margins = margins,
                                    iteratorBetas = iterator.betas,
                                    dims = dims,
                                    mu = mu)
              default.weights <- weights
              model <- addAg(model = model,
                             aggregate = aggregate,
                             defaultWeights = default.weights)
              model <- makeCellInLik(model = model,
                                     y = y)
              model <- updateMu(model, useC = TRUE)
              model <- updateMeansBetas(model, useC = TRUE)
              model <- updateVariancesBetas(model, useC = TRUE)
              model <- updateLogPostBetas(model, useC = TRUE)
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
              val.betas <- lapply(betas, function(x) rep(0, length(x)))
              fun <- function(x) x@isZeroVar@.Data || x@isSaturated@.Data
              beta.equals.mean <- sapply(priors.betas, fun)
              iterator.betas <- BetaIterator(dim = dim, margins = margins)
              dims <- makeDims(dim = dim, margins = margins)
              mu <- makeMu(n = length(theta),
                           betas = betas,
                           iterator = iterator.betas,
                           useC = TRUE)
              cellInLik <- rep(TRUE, times = length(theta))
              model <- methods::new("NormalVaryingVarsigmaUnknown",
                                    call = call,
                                    theta = theta,
                                    thetaTransformed = theta,
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
                                    meansBetas = val.betas,
                                    variancesBetas = val.betas,
                                    priorsBetas = priors.betas,
                                    betaEqualsMean = beta.equals.mean,
                                    namesBetas = names.betas,
                                    margins = margins,
                                    iteratorBetas = iterator.betas,
                                    dims = dims,
                                    mu = mu)
              default.weights <- weights
              model <- addAg(model = model,
                             aggregate = aggregate,
                             defaultWeights = default.weights)
              model <- makeCellInLik(model = model,
                                     y = y)
              model <- updateMu(model, useC = TRUE)
              model <- updateMeansBetas(model, useC = TRUE)
              model <- updateVariancesBetas(model, useC = TRUE)
              model <- updateLogPostBetas(model, useC = TRUE)
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
              shape <- ifelse(is.obs, y@.Data + 1, 1)
              if (has.exposure)
                  rate <- ifelse(is.obs, exposure + 1, 1)
              else
                  rate <- 2
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
              ## too many zeros (including structural zeros) can lead to
              ## missing values - need to fix these up
              for (i in seq_along(betas)) {
                  betas[[i]][is.na(betas[[i]])] <- 0
              }
              theta <- as.numeric(theta)
              theta[struc.zero.array == 0L] <- NA
              if (box.cox.param > 0)
                  theta.transformed <- (theta ^ box.cox.param - 1) / box.cox.param
              else
                  theta.transformed <- log(theta)
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
              val.betas <- lapply(betas, function(x) ifelse(is.na(x), as.double(NA), 0))
              fun <- function(x) x@isZeroVar@.Data || x@isSaturated@.Data
              beta.equals.mean <- sapply(priors.betas, fun)
              iterator.betas <- BetaIterator(dim = dim, margins = margins)
              dims <- makeDims(dim = dim, margins = margins)
              mu <- makeMu(n = length(theta),
                           betas = betas,
                           iterator = iterator.betas,
                           useC = TRUE)
              class <- if (has.exposure) "PoissonVaryingUseExp" else "PoissonVaryingNotUseExp"
              cellInLik <- rep(TRUE, times = length(theta)) # temporary value
              model <- methods::new(class,
                                    call = call,
                                    theta = theta,
                                    thetaTransformed = theta.transformed,
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
                                    meansBetas = val.betas,
                                    variancesBetas = val.betas,
                                    priorsBetas = priors.betas,
                                    betaEqualsMean = beta.equals.mean,
                                    namesBetas = names.betas,
                                    margins = margins,
                                    iteratorBetas = iterator.betas,
                                    dims = dims,
                                    mu = mu)
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
              model <- updateMu(model, useC = TRUE)
              model <- updateMeansBetas(model, useC = TRUE)
              model <- updateVariancesBetas(model, useC = TRUE)
              model <- updateLogPostBetas(model, useC = TRUE)
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
              mean.before.subset <- methods::new("Values",
                                        .Data = .Data.mean,
                                        metadata = metadataAll)
              sd.before.subset <- methods::new("Values",
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
              mean <- methods::new("ParameterVector", mean@.Data)
              sd <- methods::new("ScaleVec", sd@.Data)
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
              mean.before.subset <- methods::new("Values",
                                        .Data = .Data.mean,
                                        metadata = metadataAll)
              sd.before.subset <- methods::new("Values",
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
              mean <- methods::new("ParameterVector", mean@.Data)
              sd <- methods::new("ScaleVec", sd@.Data)
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

## NO_TESTS
setMethod("initialModel",
          signature(object = "SpecLN2",
                    y = "Counts",
                    exposure = "Counts",
                    weights = "missing"),
          function(object, y, exposure) {
              A.varsigma <- object@AVarsigma@.Data
              A.sigma <- object@ASigma@.Data
              call <- object@call
              concordances <- object@concordances
              mult.sigma <- object@multSigma
              mult.varsigma <- object@multVarsigma
              nu.sigma <- object@nuSigma
              nu.varsigma <- object@nuVarsigma
              constraint.all <- object@constraintLN2
              sigma.max <- object@sigmaMax@.Data
              structural.zeros <- object@structuralZeros
              varsigma.max <- object@varsigmaMax@.Data
              metadataY <- y@metadata
              ## make struc.zero.array
              struc.zero.array <- makeStrucZeroArray(structuralZeros = structural.zeros, 
                                                     y = y) 
              y <- checkAndTidyYForStrucZero(y = y, 
                                             strucZeroArray = struc.zero.array)
              ## draw values for varsigma and sigma
              varsigma <- stats::runif(n = 1L,
                                       min = 0,
                                       max = min(A.varsigma, varsigma.max))
              sigma <- stats::runif(n = 1L,
                                    min = 0,
                                    max = min(A.sigma, sigma.max))
              A.varsigma <- methods::new("Scale", A.varsigma)
              A.sigma <- methods::new("Scale", A.sigma)
              varsigma <- methods::new("Scale", varsigma)
              sigma <- methods::new("Scale", sigma)
              varsigma.max <- methods::new("Scale", varsigma.max)
              sigma.max <- methods::new("Scale", sigma.max)
              ## subset 'constraint.all' to create 'constraint', allowing for
              ## the fact that some of the categories in 'constraint.all'
              ## are collapsed versions of ones in 'y', as described by
              ## 'concordances'
              y.collapsed <- y
              for (i in seq_along(concordances)) {
                  name <- names(concordances)[[i]]
                  if (name %in% names(y.collapsed)) {
                      concordance <- concordances[[i]]
                      y.collapsed <- collapseCategories(y.collapsed,
                                                        dimension = name,
                                                        concordance = concordance)
                  }
              }
              y.collapsed <- suppressMessages(tryCatch(error = function(e) e,
                                                       y.collapsed <- dembase:::makePairCompatible(e1 = y.collapsed,
                                                                                                   e2 = methods::as(constraint.all, "Counts"))))
              if (inherits(y.collapsed, "error"))
                  stop(gettextf("'%s' and '%s' not compatible : %s",
                                "constraint", "y", constraint$message))
              y.collapsed <- y.collapsed[[1L]]
              constraint <- tryCatch(error = function(e) e,
                                     dembase::makeCompatible(x = constraint.all,
                                                             y = y.collapsed,
                                                             subset = TRUE))
              if (inherits(constraint, "error"))
                  stop(gettextf("'%s' and '%s' not compatible : %s",
                                "constraint", "y", constraint$message))
              ## make transfom between 'y' and 'constraint'
              transform <- makeTransform(x = y,
                                         y = constraint,
                                         subset = FALSE,
                                         concordances = concordances)
              if (inherits(transform, "error"))
                  stop(gettextf("'%s' and '%s' not compatible : %s",
                                "constraint", "y", transform$message))
              transform <- makeCollapseTransformExtra(transform)
              ## randomly draw 'alpha', using residuals and constraint
              alpha <- numeric(length = length(constraint))
              resid <- log1p(y) - log1p(exposure)
              resid[is.na(resid)] <- 0
              resid <- dembase::collapse(resid,
                                         transform = transform)
              for (j in seq_along(alpha)) {
                  if (is.na(constraint[j]))
                      alpha[j] <- stats::rnorm(n = 1L,
                                               mean = resid[j],
                                               sd = sigma@.Data)
                  else if (constraint[j] == -1L)
                      alpha[j] <- rtnorm1(mean = resid[j],
                                          sd = sigma@.Data,
                                          upper = 0,
                                          useC = TRUE)
                  else if (constraint[j] == 0L)
                      alpha[j] <- 0
                  else if (constraint[j] == 1L)
                      alpha[j] <- rtnorm1(mean = resid[j],
                                          sd = sigma@.Data,
                                          lower = 0,
                                          useC = TRUE)
                  else
                      stop(gettext("invalid value for '%s' [%s]",
                                   "constraint", constraint[i]))
              }
              alpha <- methods::new("ParameterVector", alpha)
              n.cell.before <- rep(0L, times = length(alpha)) # temporary value
              cellInLik <- rep(TRUE, times = length(y)) # temporary value
              model <- methods::new("LN2",
                                    alphaLN2 = alpha,
                                    ASigma = A.sigma,
                                    AVarsigma = A.varsigma,
                                    call = call,
                                    cellInLik = cellInLik,
                                    metadataY = metadataY,
                                    nCellBeforeLN2 = n.cell.before,
                                    nuSigma = nu.sigma,
                                    nuVarsigma = nu.varsigma,
                                    constraintAllLN2 = constraint.all,
                                    constraintLN2 = constraint,
                                    sigma = sigma,
                                    sigmaMax = sigma.max,
                                    strucZeroArray = struc.zero.array,
                                    transformLN2 = transform,
                                    varsigma = varsigma,
                                    varsigmaMax = varsigma.max)
              model <- makeCellInLik(model = model,
                                     y = y,
                                     strucZeroArray = struc.zero.array)
              model <- makeNCellBeforeLN2(model)
              model
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
                                  thetaTransformed = l$thetaTransformed,
                                  metadataY = metadataY,
                                  cellInLik = l$cellInLik,
                                  nAcceptTheta = methods::new("Counter", 0L),
                                  lower = lower,
                                  upper = upper,
                                  nFailedPropTheta = methods::new("Counter", 0L),
                                  betas = l$betas,
                                  meansBetas = l$meansBetas,
                                  variancesBetas = l$variancesBetas,
                                  priorsBetas = l$priorsBetas,
                                  betaEqualsMean = l$betaEqualsMean,
                                  iteratorBetas = l$iteratorBetas,
                                  dims = l$dims,
                                  mu = l$mu,
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
                                  thetaTransformed = l$thetaTransformed,
                                  metadataY = metadataY,
                                  cellInLik = l$cellInLik,
                                  nAcceptTheta = methods::new("Counter", 0L),
                                  lower = lower,
                                  upper = upper,
                                  nFailedPropTheta = methods::new("Counter", 0L),
                                  betas = l$betas,
                                  meansBetas = l$meansBetas,
                                  variancesBetas = l$variancesBetas,
                                  priorsBetas = l$priorsBetas,
                                  betaEqualsMean = l$betaEqualsMean,
                                  strucZeroArray = l$strucZeroArray,
                                  iteratorBetas = l$iteratorBetas,
                                  dims = l$dims,
                                  mu = l$mu,
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
                             thetaTransformed = l$thetaTransformed,
                             metadataY = metadataY,
                             cellInLik = l$cellInLik,
                             lower = lower,
                             upper = upper,
                             nFailedPropTheta = methods::new("Counter", 0L),
                             betas = l$betas,
                             meansBetas = l$meansBetas,
                             variancesBetas = l$variancesBetas,
                             priorsBetas = l$priorsBetas,
                             betaEqualsMean = l$betaEqualsMean,
                             iteratorBetas = l$iteratorBetas,
                             dims = l$dims,
                             mu = l$mu,
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
                             thetaTransformed = l$thetaTransformed,
                             metadataY = metadataY,
                             cellInLik = l$cellInLik,
                             lower = lower,
                             upper = upper,
                             nFailedPropTheta = methods::new("Counter", 0L),
                             betas = l$betas,
                             meansBetas = l$meansBetas,
                             variancesBetas = l$variancesBetas,
                             priorsBetas = l$priorsBetas,
                             betaEqualsMean = l$betaEqualsMean,
                             iteratorBetas = l$iteratorBetas,
                             dims = l$dims,
                             mu = l$mu,
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
              nu.cmp <- methods::new("ParameterVector", rep(1, times = length(l$theta)))
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
                                  thetaTransformed = l$thetaTransformed,
                                  nuCMP = nu.cmp,
                                  metadataY = metadataY,
                                  cellInLik = l$cellInLik,
                                  nAcceptTheta = methods::new("Counter", 0L),
                                  lower = lower,
                                  upper = upper,
                                  nFailedPropTheta = methods::new("Counter", 0L),
                                  betas = l$betas,
                                  meansBetas = l$meansBetas,
                                  variancesBetas = l$variancesBetas,
                                  priorsBetas = l$priorsBetas,
                                  betaEqualsMean = l$betaEqualsMean,
                                  strucZeroArray = l$strucZeroArray,
                                  iteratorBetas = l$iteratorBetas,
                                  dims = l$dims,
                                  mu = l$mu,
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
              mean.before.subset <- methods::new("Values",
                                        .Data = .Data.mean,
                                        metadata = metadataAll)
              sd.before.subset <- methods::new("Values",
                                      .Data = .Data.sd,
                                      metadata = metadataAll)
              y.second <- methods::new("Values",
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
              value <- tryCatch(makeCompatible(x = methods::as(mean.before.subset, "Counts"),
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
              mean <- methods::new("ParameterVector", mean@.Data)
              sd <- methods::new("ScaleVec", sd@.Data)
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
              mean.before.subset <- methods::new("Values",
                                        .Data = .Data.mean,
                                        metadata = metadataAll)
              sd.before.subset <- methods::new("Values",
                                      .Data = .Data.sd,
                                      metadata = metadataAll)
              y.second <- methods::new("Values",
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
              value <- tryCatch(makeCompatible(x = methods::as(mean.before.subset, "Counts"),
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
              mean <- methods::new("ParameterVector", mean@.Data)
              sd <- methods::new("ScaleVec", sd@.Data)
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



## HAS_TESTS
setMethod("initialModelPredict",
          signature(model = "LN2"),
          function(model, along, labels, n, offsetModel,
                   covariates, aggregate, lower, upper) {
              i.method.model.first <- model@iMethodModel
              metadata.y <- model@metadataY
              alpha.first <- model@alphaLN2@.Data
              constraint.first <- model@constraintLN2
              constraint.all <- model@constraintAllLN2
              if (!is.null(labels))
                  n <- NULL
              metadata.y.second <- makeMetadataPredict(metadata = metadata.y,
                                                       along = along,
                                                       labels = labels,
                                                       n = n)
              DimScale.along <- DimScales(metadata.y.second)[[along]]
              extrapolate.struc.zero <- (methods::is(DimScale.along, "Intervals")
                  || methods::is(DimScale.along, "Points"))
              if (extrapolate.struc.zero) {
                  struc.zero.array.first <- model@strucZeroArray
                  labels <- labels(DimScale.along)
                  struc.zero.array.second <- extrapolateStrucZeroArray(struc.zero.array.first,
                                                                       along = along,
                                                                       labels = labels)
              }
              else {
                  .Data <- array(1L,
                                 dim = dim(metadata.y.second),
                                 dimnames = dimnames(metadata.y.second))
                  struc.zero.array.second <- methods::new("Counts",
                                                          .Data = .Data,
                                                          metadata = metadata.y.second)
              }
              metadata.constraint.first <- constraint.first@metadata
              name.along <- names(metadata.y)[along]
              along.constraint <- match(name.along,
                                        names(constraint.first),
                                        nomatch = 0L)
              if (along.constraint > 0L) {
                  metadata.constraint.second <- makeMetadataPredict(metadata = metadata.constraint.first,
                                                                    along = along.constraint,
                                                                    labels = labels,
                                                                    n = NULL)
                  .Data.template.second <- array(NA_integer_,
                                                 dim = dim(metadata.constraint.second),
                                                 dimnames = dimnames(metadata.constraint.second))
                  template.second <- methods::new("Values",
                                                  .Data = .Data.template.second,
                                                  metadata = metadata.constraint.second)
                  constraint.second <- tryCatch(error = function(e) e,
                                                dembase::makeCompatible(x = constraint.all,
                                                                        y = template.second,
                                                                        subset = TRUE))
                  if (inherits(constraint.second, "error"))
                      stop(gettextf("problems creating '%s' array for prediction : %s",
                                    "constraint", constraint.second$message))
                  transform.second <- tryCatch(error = function(e) e,
                                               dembase::makeTransform(x = struc.zero.array.second,
                                                                      y = constraint.second,
                                                                      subset = FALSE))
                  if (inherits(transform.second, "error"))
                      stop(gettextf("problems creating transform for prediction : %s",
                                    transform.second$message))
                  alpha.second <- numeric(length = length(constraint.second))
              }
              else {
                  constraint.second <- constraint.first
                  transform.second <- model@transformLN2
                  alpha.second <- alpha.first
              }
              transform.second <- dembase::makeCollapseTransformExtra(transform.second)
              alpha.second <- methods::new("ParameterVector", alpha.second)
              cell.in.lik.second <- rep(FALSE, times = prod(dim(metadata.y.second)))
              n.cell.before.second <- rep(0L, times = length(alpha.second)) # temporary value
              offsets.alpha <- c(first = offsetModel,
                                 last = offsetModel + length(alpha.second) - 1L)
              offsets.alpha <- methods::new("Offsets", offsets.alpha)
              offsets.sigma <- makeOffsetsSigma(model, offsetModel = offsetModel)
              offsets.varsigma <- makeOffsetsVarsigma(model, offsetModel = offsetModel)
              class <- paste0(class(model), "Predict")
              i.method.model.second <- i.method.model.first + 100L
              model <- methods::new(class,
                                    model,
                                    alphaLN2 = alpha.second,
                                    cellInLik = cell.in.lik.second,
                                    constraintLN2 = constraint.second,
                                    iMethodModel = i.method.model.second,
                                    metadataY = metadata.y.second,
                                    nCellBeforeLN2 = n.cell.before.second,
                                    offsetsAlphaLN2 = offsets.alpha,
                                    offsetsSigma = offsets.sigma,
                                    offsetsVarsigma = offsets.varsigma,
                                    strucZeroArray = struc.zero.array.second,
                                    transformLN2 = transform.second)
              model <- makeNCellBeforeLN2(model)
              model
          })





