
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
              scale.theta <- object@scaleTheta
              lower <- object@lower
              upper <- object@upper
              tolerance <- object@tolerance
              max.attempt <- object@maxAttempt
              nu.sigma <- object@nuSigma
              A.sigma <- object@ASigma@.Data
              sigma.max <- object@sigmaMax@.Data
              use.expose <- object@useExpose@.Data
              aggregate <- object@aggregate
              checkTermsFromFormulaFound(y = y, formula = formula.mu)
              checkLengthDimInFormula(y = y, formula = formula.mu)
              metadataY <- y@metadata
              dim <- dim(y)
              has.exposure <- !is.null(exposure)
              if (has.exposure && !use.expose)
                  stop(gettextf("'%s' argument supplied, but model '%s' does not use exposure",
                                "exposure", deparse(call[[2L]])))
              if (!has.exposure && use.expose)
                  stop(gettextf("model '%s' uses exposure, but no '%s' argument supplied",
                                deparse(call[[2L]]), "exposure"))
              y.missing <- is.na(y@.Data)
              if (!all(y.missing))
                  mean.y.obs <- mean(y@.Data[!y.missing])
              else
                  mean.y.obs <- 0.5
              shape <- ifelse(y.missing, mean.y.obs, 0.5 * mean.y.obs + 0.5 * y@.Data)
              if (has.exposure) {
                  mean.expose.obs <- mean(exposure[!y.missing])
                  rate <- ifelse(y.missing, mean.expose.obs, 0.5 * mean.expose.obs + 0.5 * exposure)
              }
              else
                  rate <- 1
              if (has.exposure)
                  scale.theta.multiplier <- sqrt(mean.y.obs + 1)
              else
                  scale.theta.multiplier <- 1.0
              scale.theta.multiplier <- methods::new("Scale", scale.theta.multiplier)
              theta <- stats::rgamma(n = length(y), shape = shape, rate = rate)
              ## sigma
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
              sigma <- methods::new("Scale", sigma)
              ## sdLogNu
              A.sd.log.nu <- makeASigma(A = A.sigma,
                                        sY = sY)
              sigma.max <- makeScaleMax(scaleMax = sigma.max,
                                        A = A.sigma,
                                        nu = nu.sigma)
              sigma <- stats::runif(n = 1L,
                                    min = 0,
                                    max = min(A.sigma@.Data, sigma.max@.Data))
              sigma <- methods::new("Scale", sigma)

              A

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
              theta <- as.numeric(theta)
              names.betas <- names(betas)
              margins <- makeMargins(betas = betas, y = y)
              priors.betas <- makePriors(betas = betas,
                                         specs = specs.priors,
                                         namesSpecs = names.specs.priors,
                                         margins = margins,
                                         y = y,
                                         sY = sY)
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
              betas <- jitterBetas(betas)
              iterator.betas <- BetaIterator(dim = dim, margins = margins)
              dims <- makeDims(dim = dim, margins = margins)
              class <- if (has.exposure) "PoissonVaryingUseExp" else "PoissonVaryingNotUseExp"
              cellInLik <- rep(TRUE, times = length(theta))
              model <- methods::new(class,

                                    ASDLogNuCMP
                                    multSDLogNuCMP
                                    nuSDLogNuCMP
                                    SDLogNuCMP
                                    sdMaxLogNuCMP
                                    meanLogNuCMP
                                    meanMeanLogNuCMP
                                    sdMeanLogNuCMP
                                    nuCMP
                                                                     




                                    call = call,
                                    theta = theta,
                                    cellInLik = cellInLik,
                                    metadataY = metadataY,
                                    scaleTheta = scale.theta,
                                    scaleThetaMultiplier = scale.theta.multiplier,
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
                                     y = y)
              model
          })




#' @rdname SpecModel-class
#' @export
setClass("SpecCMPVarying",
         prototype = prototype(useExpose = new("LogicalFlag", TRUE)),
         contains = "SpecVarying",
         validity = function(object) {
             lower <- object@lower
             ## 'lower' non-negative
             if (lower < 0)
                 return(gettextf("'%s' is less than %d",
                                 "lower", 0L))
             TRUE
         })



setClass("CMPVaryingNotUseExpPredict",
         prototype = prototype(iMethodModel = 132L),
         contains = c("CMPVaryingNotUseExp", "BetaIsPredicted", "OffsetsBetas",
             "OffsetsPriorsBetas", "OffsetsSigma"))


setClass("CMPVaryingUseExpPredict",
         prototype = prototype(iMethodModel = 133L),
         contains = c("CMPVaryingUseExp", "BetaIsPredicted", "OffsetsBetas",
             "OffsetsPriorsBetas", "OffsetsSigma"))


#' @rdname SpecLikelihood-class
#' @export
setClass("SpecLikelihoodCMP",
         prototype = prototype(useExpose = new("LogicalFlag", TRUE)),
         contains = c("MeanMeanLogNuCMPMixin",
                      "SDMeanLogNuCMPMixin",
                      "SpecAMixin",
                      "MultMixin",
                      "NuMixin",
                      "SpecScaleMaxMixin",
                      "FormulaMuMixin",
                      "UseExposeMixin"))



## HAS_TESTS
setMethod("SpecModel",
          signature(specInner = "SpecLikelihoodPoisson"),
          function(specInner, call, nameY, dots, lower, upper,
                   priorSD, jump, series, aggregate) {
              formula.mu <- specInner@formulaMu
              useExpose <- specInner@useExpose
              boxCoxParam <- specInner@boxCoxParam
              specs.priors <- makeSpecsPriors(dots)
              names.specs.priors <- makeNamesSpecsPriors(dots)
              if (is.null(lower))
                  lower <- 0
              if (is.null(upper))
                  upper <- Inf
              checkLowerAndUpper(lower = lower,
                                 upper = upper,
                                 distribution = "Poisson")
              if (is.null(priorSD))
                  priorSD <- HalfT()
              else {
                  if (!methods::is(priorSD, "HalfT"))
                      stop(gettextf("'%s' has class \"%s\"",
                                    "priorSD", class(priorSD)))
              }
              A.sigma <- priorSD@A
              nu.sigma <- priorSD@nu
              sigma.max <- priorSD@scaleMax
              scale.theta <- checkAndTidyJump(jump)
              series <- checkAndTidySeries(series)
              has.series <- !is.na(series@.Data)
              if (has.series) {
                  A.sigma <- makeASigma(A = A.sigma,
                                        sY = NULL,
                                        isSpec = TRUE)
                  sigma.max <- makeScaleMax(scaleMax = sigma.max,
                                            A = A.sigma,
                                            nu = nu.sigma,
                                            isSpec = TRUE)
              }
              if (is.null(aggregate))
                  aggregate <- methods::new("SpecAgPlaceholder")
              methods::new("SpecPoissonVarying",
                           ASigma = A.sigma,
                           boxCoxParam = boxCoxParam,
                           call = call,
                           formulaMu = formula.mu,
                           lower = lower,
                           specsPriors = specs.priors,
                           namesSpecsPriors = names.specs.priors,
                           nameY = nameY,
                           nuSigma = nu.sigma,
                           scaleTheta = scale.theta,
                           series = series,
                           sigmaMax = sigma.max,
                           upper = upper,
                           useExpose = useExpose,
                           aggregate = aggregate)
          })



CMP <- function(formula, dispersion = Dispersion(), useExpose = TRUE) {
    ## formula
    checkFormulaMu(formula)
    checkForMarginalTerms(formula)
    ## dispersion
    if (!methods::is(dispersion, "Dispersion"))
        stop(gettextf("'%s' has class \"%s\"",
                      dispersion, class(dispersion)))
    meanMeanLogNuCMP <- dispersion@meanMeanLogNuCMP
    sdMeanLogNuCMP <- dispersion@sdMeanLogNuCMP
    A <- dispersion@A
    mult <- dispersion@mult
    nu <- dispersion@nu
    scaleMax <- dispersion@scaleMax
    ## useExpose
    useExpose <- checkAndTidyLogicalFlag(x = useExpose,
                                         name = "useExpose")
    methods::new("SpecLikelihoodCMP",
                 formulaMu = formula,
                 meanMeanLogNuCMP = meanMeanLogNuCMP,
                 sdMeanLogNuCMP = sdMeanLogNuCMP,
                 A = A,
                 mult = mult,
                 nu = nu,
                 scaleMax = scaleMax,
                 useExpose = useExpose)
}
CMP(mean ~ age + sex)



printCMPLikEqns <- function(object) {
    formulaMu <- object@formulaMu
    useExpose <- object@useExpose@.Data
    mean <- object@meanMeanLogNuCMP@.Data
    sd <- object@sdMeanLogNuCMP@.Data
    nu <- object@nu@.Data
    A <- object@A@.Data
    max <- object@scaleMax@.Data
    terms <- expandTermsSpec(formulaMu)
    if (useExpose) {
        cat("              y[i] ~ CMP(rate[i] * exposure[i], dispersion[i])\n")
        cat("      log(rate[i]) ~ N(", terms, ", sd^2)\n", sep = "")
    }
    else {
        cat("              y[i] ~ CMP(count[i], dispersion[i])\n")
        cat("     log(count[i]) ~ N(", terms, ", sd^2)\n", sep = "")
    }
    cat("log(dispersion[i]) ~ N(mean[i], scale^2)\n")
    cat("           mean[i] ~ N(", mean, ", ", squaredOrNA(sd), ")\n", sep = "")
    cat("             scale ~ trunc-half-t(", nu, ", ", sep = "")
    cat(squaredOrNA(A), ", ", max, ")\n", sep = "")
}


#' @rdname show-methods
#' @export
setMethod("show",
          signature(object = "SpecLikelihoodCMP"),
          function(object) {
              cat("An object of class \"", class(object), "\"\n\n", sep = "")
              printCMPLikEqns(object)
          })

