
setClass("SpecLikelihoodLN2",
         contains = c("SpecLikelihood",
                      "NuVarsigmaMixin",
                      "SpecAVarsigmaMixin",
                      "SpecVarsigmaMaxMixin"),
         slots = c(restrict = "Values",
                   concordances = "list"),
         validity = function(object) {
             restrict <- object@restrict
             ## 'restrict' has type "integer"
             if (!is.integer(restrict))
                 return(gettextf("'%s' does not have type \"%s\"",
                                 "restrict", "integer"))
             ## all values of 'restrict' valid
             is.invalid <- !(restrict@.Data %in% c(NA, -1L, 0L, 1L))
             i.invalid <- match(TRUE, is.invalid, nomatch = 0L)
             if (i.invalid > 0L)
                 return(gettextf("'%s' has invalid value [%s]",
                                 "restrict", restrict@.Data[[i.invalid]]))
             ## concordances
             if (!identical(concordances, list())) {
                 if (!is.list(concordances))
                     stop(gettextf("'%s' has class \"%s\"",
                                   "concordances", class(concordances)))
                 if (!all(sapply(concordances, methods::is,"ManyToOne")))
                     stop(gettextf("'%s' has elements not of class \"%s\"",
                                   "concordances", "ManyToOne"))
                 names.conc <- names(concordances)
                 if (is.null(names.conc))
                     stop(gettextf("'%s' does not have names",
                                   "concordances"))
                 if (any(duplicated(names.conc)))
                     stop(gettextf("'%s' has duplicate names",
                                   "concordances"))
             }
             TRUE
         })

LN2 <- function(restrict, concordances = list(),
                priorSD = HalfT()) {
    ## restrict
    if (!methods::is(restict, "Values"))
        stop(gettextf("'%s' has class \"%s\"",
                      "restrict", class(restrict)))
    is.invalid <- !(restrict@.Data %in% c(NA, -1L, 0L, 1L))
    i.invalid <- match(TRUE, is.invalid, nomatch = 0L)
    if (i.invalid > 0L)
        stop(gettextf("'%s' has invalid value [%s]",
                      "restrict", restrict@.Data[[i.invalid]]))
    restrict <- toInteger(restrict)
    if (!methods::is(priorSD, "HalfT"))
        stop(gettextf("'%s' has class \"%s\"",
                      "priorSD", class(priorSD)))
    ## concordances
    if (!identical(concordances, list())) {
        if (!is.list(concordances))
            stop(gettextf("'%s' has class \"%s\"",
                          "concordances", class(concordances)))
        if (!all(sapply(concordances, methods::is,"ManyToOne")))
            stop(gettextf("'%s' has elements not of class \"%s\"",
                          "concordances", "ManyToOne"))
        names.conc <- names(concordances)
        if (is.null(names.conc))
            stop(gettextf("'%s' does not have names",
                          "concordances"))
        if (any(duplicated(names.conc)))
            stop(gettextf("'%s' has duplicate names",
                          "concordances"))
    }
    ## priorSD
    if (!methods::is(priorSD, "HalfT"))
        stop(gettextf("'%s' has class \"%s\"",
                      "priorSD", class(priorSD)))
    AVarsigma <- priorSD@A
    multVarsigma <- priorSD@multVarsigma
    nuVarsigma <- priorSD@nu
    varsigmaMax <- priorSD@scaleMax
    ## return
    methods::new("SpecLikelihoodLN2",
                 restrict = restrict,
                 AVarsigma = AVarsigma,
                 multVarsigma = multVarsigma,
                 nuVarsigma = nuVarsigma,
                 varsigmaMax = varsigmaMax)
}

setMethod("SpecModel",
          signature(specInner = "SpecLikelihoodLN2"),
          function(specInner, call, nameY, dots, lower, upper,
                   priorSD, jump,
                   series, aggregate) {
              restrict <- specInner@restrict
              A.varsigma <- specInner@AVarsigma
              mult.varsigma <- specInnter@multVarsigma
              nu.varsigma <- specInner@nuVarsigma
              varsigma.max <- specInner@varsigmaMax
              if (is.null(priorSD))
                  priorSD <- HalfT()
              else {
                  if (!methods::is(priorSD, "HalfT"))
                      stop(gettextf("'%s' has class \"%s\"",
                                    "priorSD", class(priorSD)))
              }
              A.sigma <- priorSD@A
              mult.sigma <- priorSD@mult
              nu.sigma <- priorSD@nu
              sigma.max <- priorSD@scaleMax
              if (length(dots) > 0L)
                  stop(gettextf("priors specified, but distribution is %s",
                                "LN2"))
              for (name in c("lower",
                             "upper",
                             "jump",
                             "aggregate")) {
                  value <- get(name)
                  if (!is.null(value))
                      stop(gettextf("'%s' specified, but distribution is %s",
                                    name, "LN2"))
              }
              series <- checkAndTidySeries(series)
              methods::new("SpecLN2",
                           ASigma = A.sigma,
                           AVarsigma = A.varsigma,
                           call = call,
                           nameY = nameY,
                           nuSigma = nu.sigma,
                           nuVarsigma = nu.varsigma,
                           restrict = restrict,
                           series = series,
                           multSigma = mult.sigma,
                           multVarsigma = mult.varsigma,
                           sigmaMax = sigma.max,
                           varsigmaMax = varsigma.max)
          })


setMethod("initialModel",
          signature(object = "SpecLN2",
                    y = "Counts",
                    exposure = "Counts",
                    weights = "missing"),
          function(object, y, exposure) {
              A.varsigma <- object@AVarsigma@.Data
              A.sigma <- object@ASigma@.Data
              call <- object@call
              mult.sigma <- object@multSigma
              mult.varsigma <- object@multVarsigma
              restrictAll <- object@restrict
              sigma.max <- object@sigmaMax@.Data
              nu.sigma <- object@nuSigma
              nu.varsigma <- object@nuVarsigma
              varsigma.max <- object@varsigmaMax@.Data
              metadataY <- y@metadata
              dim <- dim(y)
              n <- length(y)
              sY <- stats::sd(as.numeric(y), na.rm = TRUE) ## to deal with R <3.0 behaviour
              A.varsigma <- makeASigma(A = A.varsigma,
                                       sY = sY,
                                       mult = mult.varsigma)
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
              varsigma <- methods::new("Scale", varsigma)
              sigma <- methods::new("Scale", sigma)
              restrict <- tryCatch(error = function(e) e,
                                   makeCompatible(x = restictAll,
                                                  y = y,
                                                  subst = TRUE))
              if (inherits(restrict, "error"))
                  stop(gettextf("'%s' not compatible with '%s' : %s",
                                "restrict", "y", restrict$message))
              transformAlphaLN2 <- makeTransform(x = y,
                                                 y = restrict,
                                                 subset = FALSE,
                                                 concordances = concordances)
              cellInLik <- rep(TRUE, times = length(theta))
              methods::new("NormalVaryingVarsigmaUnknown",
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
          })





setClass("LN2",
         slots = c(alphaLN2 = "ParameterVector",
                   alphaIsZeroLN2 = "logical",
                   alphaIsZeroAllLN2 = "logical",
                   metadataAlphaLN2 = "MetaData",
                   metadataAlphaAllLN2 = "MetaData",
                   nCellBeforeLN2 = "integer",
                   transformAlphaLN2 = "CollapseTransform",
                   truncationAlphaLN2 = "integer",
                   truncationAlphaAllLN2 = "integer"),
         contains = c("Model",
                      "ASigmaMixin",
                      "CellInLikMixin",
                      "MaxAttemptMixin",
                      "NuSigmaMixin",
                      "SigmaMaxMixin",
                      "SigmaMixin",
                      "VarsigmaUnknown"),
         prototype = prototype(slotsToExtract = c("alphaLN2",
                                                  "varsigma",
                                                  "sigma"),
                               iMethodModel = 37L,
                               nuVarsigma = methods::new("DegreesFreedom", 7),
                               nuSigma = methods::new("DegreesFreedom", 7),
                               logPostSigma = methods::new("Parameter", 0),
                               logPostVarsigma = methods::new("Parameter", 0)),
         validity = function(object) {
             alphaLN2 <- object@alphaLN2@.Data
             alphaIsZeroLN2 <- object@alphaIsZeroLN2
             alphaIsZeroAllLN2 <- object@alphaIsZeroAllLN2
             metadataAlphaLN2 <- object@metadataAlphaLN2
             metadataAlphaAllLN2 <- object@metadataAlphaAllLN2
             nCellBeforeLN2 <- object@nCellBeforeLN2
             transformAlphaLN2 <- object@transformAlphaLN2
             truncationAlphaLN2 <- object@truncationAlphaLN2
             truncationAlphaAllLN2 <- object@truncationAlphaAllLN2
             ## 'alphaIsZeroLN2', 'alphaIsZeroAllLN2', 'nCellBeforeLN2',
             ## 'truncationAlphaLN2', 'truncationAlphaAllLN2' not NA
             for (name in c("alphaIsZeroLN2", "alphaIsZeroAllLN2", "nCellBeforeLN2",
                            "truncationAlphaLN2", "truncationAlphaAllLN2")) {
                 value <- methods::slot(object, name)
                 if (any(is.na(value)))
                     return(gettextf("'%s' has missing values",
                                     name))
             }
             ## values of 'nCellBeforeLN2' all non-negative
             if (any(nCellBeforeLN2 < 0L))
                 return(gettextf("'%s' has negative values",
                                 "nCellBeforeLN2"))
             ## 'truncationAlphaLN2', 'truncationAlphaAllLN2' consist
             ## entirely of -1s, 0s, and 1s
             for (name in c("truncationAlphaLN2", "truncationAlphaAllLN2")) {
                 value <- methods::slot(object, name)
                 if (!all(value %in% c(-1L, 0L, 1L)))
                     return(gettextf("'%s' has invalid values",
                                     name))
             }
             ## 'alphaIsZeroLN2' has same length as 'alphaLN2'
             if (!identical(length(alphaIsZeroLN2), length(alphaLN2)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "alphaIsZeroLN2", "alphaLN2"))
             ## 'alphaLN2' has length implied by 'metadataAlphaLN2'
             if (!identical(length(alphaLN2), prod(dim(metadataAlphaLN2))))
                 return(gettextf("'%s' and '%s' incompatible",
                                 "alphaLN2", "metadataAlphaLN2"))
             ## 'alphaIsZeroAllLN2' has length implied by 'metadataAlphaAllLN2'
             if (!identical(length(alphaIsZeroAllLN2), prod(dim(metadataAlphaAllLN2))))
                 return(gettextf("'%s' and '%s' incompatible",
                                 "alphaIsZeroAllLN2", "metadataAlphaAllLN2"))
             ## 'nCellBeforeLN2' has same length as 'alphaLN2'
             if (!identical(length(nCellBeforeLN2), length(alphaLN2)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "nCellBeforeLN2", "alphaLN2"))
             ## 'alphaLN2' has length implied by 'transformAlphaLN2'
             if (!identical(length(alphaLN2), prod(dim(metadataAlphaLN2))))
                 return(gettextf("'%s' and '%s' incompatible",
                                 "alphaLN2", "metadataAlphaLN2"))
             ## 'truncationAlphaLN2' has same length as 'alphaLN2'
             if (!identical(length(truncationAlphaLN2), length(alphaLN2)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "truncationAlphaLN2", "alphaLN2"))
             ## 'truncationAlphaAllLN2' has same length as 'alphaIsZeroAllLN2'
             if (!identical(length(truncationAlphaAllLN2), length(alphaIsZeroAllLN2)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "truncationAlphaAllLN2", "alphaIsZeroAllLN2"))
             TRUE
         })



setClass("OffsetsAlphaLN2",
         slots = c(offsetsAlphaLN2 = "OffsetsOrNULL"),
         contains = "VIRTUAL",
         validity = function(object) {
             offsetsAlphaLN2 <- object@offsetsAlphaLN2
             alphaLN2 <- object@alphaLN2
             if (!is.null(offsetsAlphaLN2)) {
                 first <- offsetsAlphaLN2[1L]
                 last <- offsetsAlphaLN2[2L]
                 if (!identical(last - first + 1L, length(alphaLN2)))
                     return(gettextf("'%s' and '%s' incompatible",
                                     "offsetsAlphaLN2", "alphaLN2"))
             }
             TRUE
         })


setClass("LN2Predict",
         prototype = prototype(iMethodModel = 137L),
         contains = c("LN2",
                      "OffsetsAlphaLN2",
                      "OffsetsVarsigma",
                      "OffsetsSigma"))

setMethod("drawModelUseExp",
          signature(object = "LN2"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(drawModelUseExp_LN2_R, object, y, exposure)
                  else
                      .Call(drawModelUseExp_R, object, y, exposure)
              }
              else {
                  object <- drawSigma_Varying(object) # not Varying, but works anyway
                  object <- drawVarsigma(object)
                  object <- predictAlphaLN2(object)
                  object
              }
          })


setMethod("logLikelihood",
          signature(model = "LN2",
                    count = "integer",
                    dataset = "Counts",
                    i = "integer"),
          function(model, count, dataset, i, useC = FALSE, useSpecific = FALSE) {
              ## count
              stopifnot(identical(length(count), 1L))
              stopifnot(!is.na(count))
              ## dataset
              stopifnot(is.integer(dataset))
              ## i
              stopifnot(identical(length(i), 1L))
              stopifnot(!is.na(i))
              stopifnot(i >= 1L)
              ## dataset and i
              stopifnot(i <= length(dataset))
              stopifnot(!is.na(dataset@.Data[i]))
              if (useC) {
                  if (useSpecific)
                      .Call(logLikelihood_NormalFixedUseExp_R, model, count, dataset, i)
                  else
                      .Call(logLikelihood_R, model, count, dataset, i)
              }
              else {
                  logLikelihood_LN2(model = model,
                                    count = count,
                                    dataset = dataset,
                                    i = i)
              }
          })



## Calling function should test that dataset[i] is not missing
logLikelihood_LN2 <- function(model, count, dataset, i, useC = FALSE) {
    ## model
    stopifnot(methods::is(model, "Model"))
    stopifnot(methods::is(model, "LN2"))
    ## count
    stopifnot(identical(length(count), 1L))
    stopifnot(is.integer(count))
    stopifnot(!is.na(count))
    ## dataset
    stopifnot(is.integer(dataset))
    ## i
    stopifnot(identical(length(i), 1L))
    stopifnot(is.integer(i))
    stopifnot(!is.na(i))
    stopifnot(i >= 1L)
    ## dataset and i
    stopifnot(i <= length(dataset))
    stopifnot(!is.na(dataset@.Data[i]))
    if (useC) {
        .Call(logLikelihood_LN2_R, model, count, dataset, i)
    }
    else {
        alpha <- model@alphaLN2@.Data
        transform <- model@transformLN2
        sd <- model@varsigma@.Data
        x <- log1p(dataset[[i]])
        j <- dembase::getIAfter(i = i,
                                transform = transform)
        mean <- log1p(count) + alpha[j]
        stats::dnorm(x = x,
                     mean = mean,
                     sd = sd,
                     log = TRUE)
    }
}

setMethod("makeOutputModel",
          signature(model = "NormalFixed"),
          function(model, pos, mcmc) {
              metadata <- model@metadataLN2
              alpha <- model@alphaLN2@.Data
              varsigma <- model@varsigma@.Data
              sigma <- model@sigma@.Data
              ## make alpha
              first <- pos
              pos <- first + length(alpha)
              .Data <- array(alpha,
                             dim = dim(metadata),
                             dimnames = dimnames(metadata))
              alpha <- methods::new("Values",
                                    metadata = metadata,
                                    .Data = .Data)
              s <- set_along(dim(metadata))
              alpha <- Skeleton(object = alpha,
                                first = first,
                                strucZeroArray = NULL,
                                margin = s)
              ## make varsigma
              first <- pos
              pos <- first + 1L
              varsigma <- Skeleton(first = first)
              ## make sigma
              first <- pos
              pos <- first + 1L
              sigma <- Skeleton(first = first)
              ## return value
              list(mean = alpha,
                   sd = varsigma,
                   priorSD = sigma)
          })


setMethod("updateModelUseExp",
          signature(object = "LN2"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(identical(length(y), length(object@cellInLik)))
              stopifnot(is.integer(y))
              stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0))
              ## exposure
              stopifnot(is.integer(exposure))
              stopifnot(all(exposure[!is.na(exposure)] >= 0L))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              stopifnot(all(is.na(exposure) <= is.na(y)))
              stopifnot(all(y@.Data[!is.na(y@.Data)] <= exposure[!is.na(y)]))
              if (useC) {
                  if (useSpecific)
                      .Call(updateModelUseExp_LN2_R, object, y, exposure)
                  else
                      .Call(updateModelUseExp_R, object, y, exposure)
              }
              else {
                  object <- updateAlphaLN2(object = object,
                                           y = y,
                                           exposure = exposure)
                  object <- updateSigmaLN2(object = object,
                                           y = y,
                                           exposure = exposure)
                  object <- updateSDAlphaLN2(object)
                  object
              }
          })


updateAlphaLN2 <- function(object, y, exposure, useC = FALSE) {
    ## object
    stopifnot(methods::is(object, "LN2"))
    stopifnot(methods::validObject(object))
    ## y
    stopifnot(identical(length(y), length(object@cellInLik)))
    stopifnot(is.integer(y))
    stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0))
    ## exposure
    stopifnot(is.integer(exposure))
    stopifnot(all(exposure[!is.na(exposure)] >= 0L))
    ## y and exposure
    stopifnot(identical(length(exposure), length(y)))
    stopifnot(all(is.na(exposure) <= is.na(y)))
    stopifnot(all(y@.Data[!is.na(y@.Data)] <= exposure[!is.na(y)]))
    if (useC) {
        .Call(updateAlphaLN2_R, object, y, exposure)
    }
    else {            
        alpha <- object@alphaLN2@.Data
        alpha.is.zero <- object@alphaIsZeroLN2
        cell.in.lik <- object@cellInLik
        n.cell.vec <- object@nCellBeforeLN2
        transform <- object@transformAlphaLN2
        truncation <- object@truncationAlphaLN2
        varsigma <- object@varsigma@.Data # variance of log(y + 1)
        sigma <- object@sigma@.Data # variance of alpha
        varsigma.sq <- varsigma^2
        sigma.sq <- sigma^2
        for (i.alpha in seq_along(alpha)) {
            if (!alpha.is.zero[i.alpha]) {
                i.y.vec <- dembase::getIBefore(i = i.alpha,
                                               transform = transform)
                n.cell <- n.cell.vec[i.alpha]
                trunc <- truncation[i.alpha]
                prec.data  <- n.cell / varsigma.sq
                prec.prior <- 1 / sigma.sq
                V <- 1 / (prec.data + prec.prior)
                y.vec <- y[i.y.vec]
                expose.vec <- exposure[i.y.vec]
                sum.y <- sum(log1p(y.vec))
                sum.expose  <- sum(log1p(expose.vec))
                mean.post <- prec.data * V * (sum.y - sum.expose) / n.cell
                sd.post <- sqrt(V)
                if (trunc == -1L) {
                    alpha[i.alpha] <- rtnorm1(mean = mean.post,
                                              sd = sd.post,
                                              lower = -Inf,
                                              upper = 0)
                }
                else if (trunc == 0L) {
                    alpha[i.alpha] <- stats::rnorm(n = 1L,
                                                   mean = mean.post,
                                                   sd = sd.post)
                }
                else {
                    alpha[i.alpha] <- rtnorm1(mean = mean.post,
                                              sd = sd.post,
                                              lower = 0,
                                              upper = Inf)
                }
            }
        }
        object@alphaLN2@.Data <- alpha
        object
    }
}
        
updateVarsigmaLN2 <- function(object, y, exposure, useC = FALSE) {
    ## object
    stopifnot(methods::is(object, "LN2"))
    stopifnot(methods::validObject(object))
    ## y
    stopifnot(identical(length(y), length(object@cellInLik)))
    stopifnot(is.integer(y))
    stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0))
    ## exposure
    stopifnot(is.integer(exposure))
    stopifnot(all(exposure[!is.na(exposure)] >= 0L))
    ## y and exposure
    stopifnot(identical(length(exposure), length(y)))
    stopifnot(all(is.na(exposure) <= is.na(y)))
    stopifnot(all(y@.Data[!is.na(y@.Data)] <= exposure[!is.na(y)]))
    if (useC) {
        .Call(updateSigmaLN2_R, object)
    }
    else {
        varsigma <- object@varsigma@.Data
        varsigma.max <- object@varsigmaMax@.Data
        A <- object@AVarsigma@.Data
        nu <- object@nuVarsigma@.Data
        alpha <- object@alphaLN2@.Data
        cell.in.lik <- object@cellInLik
        transform <- object@transformAlphaLN2
        V <- 0
        n <- 0L
        for (i in seq_along(y)) {
            if (cell.in.lik[i]) {
                j <- dembase::getIAfter(i = i,
                                        transform = transform)
                V <- V + (log1p(y[i]) - log1p(exposure[i]) - alpha[j])^2
                n <- n + 1L
            }
        }
        varsigma <- updateSDNorm(sigma = varsigma,
                                 A = A,
                                 nu = nu,
                                 V = V,
                                 n = n,
                                 max = varsigma.max)
        successfully.updated <- varsigma > 0
        if (successfully.updated)
            object@varsigma@.Data <- varsigma
        object
    }
}

updateSigmaLN2 <- function(object, useC = FALSE) {
    ## object
    stopifnot(methods::is(object, "LN2"))
    stopifnot(methods::validObject(object))
    if (useC) {
        .Call(updateSigmaLN2_R, object)
    }
    else {
        alpha <- object@alphaLN2@.Data
        alpha.is.zero <- object@alphaIsZeroLN2
        sigma <- object@sigma@.Data
        sigma.max <- object@sigmaMax@.Data
        A <- object@ASigma@.Data
        nu <- object@nuSigma@.Data
        V <- 0
        n <- 0L
        for (j in seq_along(alpha)) {
            if (!alpha.is.zero[j]) {
                V <- V + alpha[j]^2
                n <- n + 1L
            }
        }
        sigma <- updateSDNorm(sigma = sigma,
                              A = A,
                              nu = nu,
                              V = V,
                              n = n,
                              max = sigma.max)
        successfully.updated <- sigma > 0
        if (successfully.updated)
            object@sigma@.Data <- sigma
        object
    }
}

    
setMethod("predictModelUseExp",
          signature(object = "LN2"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(is.integer(y))
              stopifnot(identical(length(y), length(object@cellInLik)))
              stopifnot(all(is.na(y) | (y == 0L)))
              ## exposure
              stopifnot(is.double(exposure))
              ## y and exposure
              stopifnot(identical(length(exposure), length(y)))
              if (useC) {
                  if (useSpecific)
                      .Call(predictModelUseExp_LN2Predict_R,
                            object, y, exposure)
                  else
                      .Call(predictModelUseExp_R,
                            object, y, exposure)
              }
              else {
                  object <- predictAlphaLN2(object)
                  object
              }
          })


predictAlphaLN2 <- function(object, useC = FALSE) {
    ## object
    stopifnot(methods::validObject(object))
    stopifnot(methods::is(object, "LN2Predict"))
    if (useC) {
        .Call(predictAlphaLN2_R, object)
    }
    else {
        alpha <- object@alphaLN2@.Data
        alpha.is.zero <- object@alphaIsZeroLN2
        truncation <- object@truncationAlphaLN2
        sigma <- object@sigma@.Data # variance of alpha
        for (i.alpha in seq_along(alpha)) {
            if (!alpha.is.zero[i.alpha]) {
                x <- stats::rnorm(n = 1L,
                                  mean = 0,
                                  sd = sigma)
                if (trunc == -1L) {
                    alpha[i.alpha] <- -1 * abs(x)
                }
                else if (trunc == 0L) {
                    alpha[i.alpha] <- x
                }
                else {
                    alpha[i.alpha] <- abs(x)
                }
            }
        }
        object@alphaLN2@.Data <- alpha
        object
    }
}

setMethod("showModelHelper",
          signature(object = "LN2"),
          function(object) {
              printLN2ModEqns(object)
          })



makeNCellLN2 <- function(object) {
    alpha <- object@alphaLN2@.Data
    cell.in.lik <- object@cellInLik
    transform <- object@transform
    ans <- integer(length = length(alpha))
    for (i in seq_along(cell.in.lik)) {
        if (cell.in.lik.[i]) {
            j <- dembase::getIAfter(i = i,
                                    transform = transform,
                                    check = FALSE,
                                    useC = TRUE)
            ans[i] <- alpha[i] + 1L
        }
    }
    ans
}



printLN2LikEqns <- function(object) {
    cat("   log(y[i] + 1) ~ Normal(log(exposure[i] + 1) * alpha[j[i]], sd^2)\n")
}

printLN2ModEqns <- function(object) {
    call <- object@call
    series <- call$series
    ASigma <- object@ASigma@.Data
    sigmaMax <- object@sigmaMax
    nuSigma <- object@nuSigma
    AVarsigma <- object@AVarsigma@.Data
    varsigmaMax <- object@varsigmaMax
    nuVarsigma <- object@nuVarsigma
    name.y <- deparse(call$formula[[2L]])
    if (is.null(series)) {
        if (identical(name.y, "y"))
            exposure <- "exposure"
        else
            exposure <- "y"
    }
    else
        exposure <- series
    n.spaces <- max(5L - nchar(name.y), 0L)
    spaces <- rep(" ", n.spaces)
    response <- sprintf("%slog(%s + 1)", spaces, name.y)
    cat(name.y, response, " ~ Normal(log(", exposure, "[i] + 1) + alpha[j[i]], sd^2)\n",
        sep = "")
    cat("      alpha[j] ~ N*(0, sdAlpha^2)")
    cat("      sdAlpha ~ trunc-half-t(", nuSigma, ", ", sep = "")
    cat(squaredOrNA(AVarsigma),
        ", ",
        format(varsigmaMax, digits = 4),
        ")\n",
        sep = "")
    cat("      sd ~ trunc-half-t(", nuSigma, ", ", sep = "")
    cat(squaredOrNA(ASigma),
        ", ",
        format(sigmaMax, digits = 4),
        ")\n",
        sep = "")
}

printLN2SpecEqns <- function(object) {
    series <- object@series@.Data
    call <- object@call
    ASigma <- object@ASigma@.Data
    sigmaMax <- object@sigmaMax
    nuSigma <- object@nuSigma
    AVarsigma <- object@AVarsigma@.Data
    varsigmaMax <- object@varsigmaMax
    nuVarsigma <- object@nuVarsigma
    nameY <- object@nameY
    has.series <- !is.na(series)
    name.y <- deparse(call$formula[[2L]])
    name.y <- sprintf("%13s", nameY)
    if (has.series)
        exposure <- series        
    else
        exposure <- "exposure"
    cat(name.y, "[i] ~ Normal(", exposure, "[i] * mean[i], sd[i]^2)\n", sep = "")
    n.spaces <- max(5L - nchar(name.y), 0L)
    spaces <- rep(" ", n.spaces)
    response <- sprintf("%slog(%s + 1)", spaces, name.y)
    cat(name.y, response, " ~ Normal(log(", exposure, "[i] + 1) + alpha[j[i]], sd^2)\n",
        sep = "")
    cat("      alpha[j] ~ N*(0, sdAlpha^2)")
    cat("      sdAlpha ~ trunc-half-t(", nuSigma, ", ", sep = "")
    cat(squaredOrNA(AVarsigma),
        ", ",
        format(varsigmaMax, digits = 4),
        ")\n",
        sep = "")
    cat("      sd ~ trunc-half-t(", nuSigma, ", ", sep = "")
    cat(squaredOrNA(ASigma),
        ", ",
        format(sigmaMax, digits = 4),
        ")\n",
        sep = "")
}


setMethod("transferParamModel",
          signature(model = "LN2Predict"),
          function(model, filename, lengthIter, iteration,
                   useC = FALSE, useSpecific = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(transferParamModel_LN2_R,
                            model, filename, lengthIter, iteration)
                  else
                      .Call(transferParamModel_R,
                            model, filename, lengthIter, iteration)
              }
              else {
                  model <- transferParamVarsigma(model,
                                                 filename = filename,
                                                 lengthIter = lengthIter,
                                                 iteration = iteration)
                  model <- transferParamSigma(model,
                                              filename = filename,
                                              lengthIter = lengthIter,
                                              iteration = iteration)
                  model
              }
          })



setMethod("whereAcceptance",
          signature(object = "LN2"),
          function(object) list(NULL))

setMethod("whereAutocorr",
          signature(object = "LN2"),
          function(object) list(NULL))

setMethod("whereJump",
          signature(object = "LN2"),
          function(object) list(NULL))

setMethod("whereEstimated",
          signature(object = "LN2"),
          function(object) {
              list("alpha", "sd", "priorSD")
          })

setMethod("whereNoProposal",
          signature(object = "LN2"),
          function(object) list(NULL))

setMethod("whereTheta",
          signature(object = "LN2"),
          function(object) {
              stop(gettextf("'%s' has class \"%s\"",
                            "object", class(object)))
          })
