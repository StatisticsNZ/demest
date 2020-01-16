

validity_LN2 <- function(object) {
    restrictLN2 <- object@restrictLN2
    concordances <- object@concordances
    ## 'restrictLN2' has type "integer"
    if (!is.integer(restrictLN2))
        return(gettextf("'%s' does not have type \"%s\"",
                        "restrictLN2", "integer"))
    ## all values of 'restrictLN2' valid
    is.invalid <- !(restrictLN2@.Data %in% c(NA, -1L, 0L, 1L))
    i.invalid <- match(TRUE, is.invalid, nomatch = 0L)
    if (i.invalid > 0L)
        return(gettextf("'%s' has invalid value [%s]",
                        "restrictLN2", restrictLN2@.Data[[i.invalid]]))
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
}

## NO_TESTS
setClass("MultVarsigmaMixin",
         slots = c(multVarsigma = "Scale"),
         contains = "VIRTUAL")


setClass("SpecLikelihoodLN2",
         contains = c("SpecLikelihood",
                      "MultVarsigmaMixin",
                      "NuVarsigmaMixin",
                      "SpecAVarsigmaMixin",
                      "SpecVarsigmaMaxMixin",
                      "StructuralZerosMixin"),
         slots = c(restrictLN2 = "Values",
                   concordances = "list"),
         validity = validity_LN2)


setClass("SpecLN2",
         contains = c("SpecModel",
                      "MultSigmaMixin",
                      "MultVarsigmaMixin",
                      "NuSigmaMixin",
                      "NuVarsigmaMixin",
                      "SpecASigmaMixin",
                      "SpecAVarsigmaMixin",
                      "SpecSigmaMaxMixin",
                      "SpecVarsigmaMaxMixin",
                      "StructuralZerosMixin",
                      "SpecSeriesMixin",
                      "StructuralZerosMixin"),
         prototype = prototype(useExpose = methods::new("LogicalFlag", TRUE)),
         slots = c(restrictLN2 = "Values",
                   concordances = "list"),
         validity = validity_LN2)


setClass("LN2",
         slots = c(alphaLN2 = "ParameterVector",
                   restrictLN2 = "Values",
                   restrictAllLN2 = "Values",
                   nCellBeforeLN2 = "integer",
                   transformLN2 = "CollapseTransform"),
         contains = c("Model",
                      "ASigmaMixin",
                      "CellInLikMixin",
                      "MaxAttemptMixin",
                      "NuSigmaMixin",
                      "SigmaMaxMixin",
                      "SigmaMixin",
                      "StrucZeroArrayMixin",
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
             restrictLN2 <- object@restrictLN2
             restrictAllLN2 <- object@restrictAllLN2
             nCellBeforeLN2 <- object@nCellBeforeLN2
             transformLN2 <- object@transformLN2
             ## 'nCellBeforeLN2' not NA
             if (any(is.na(nCellBeforeLN2))) {
                 return(gettextf("'%s' has missing values",
                                 name))
             }
             ## values of 'nCellBeforeLN2' all non-negative
             if (any(nCellBeforeLN2 < 0L))
                 return(gettextf("'%s' has negative values",
                                 "nCellBeforeLN2"))
             ## 'restrictLN2', 'restrictAllLN2' consist
             ## entirely of NAs, -1s, 0s, and 1s
             for (name in c("restrictLN2", "restrictAllLN2")) {
                 value <- methods::slot(object, name)
                 if (!all(value %in% c(NA, -1L, 0L, 1L)))
                     return(gettextf("'%s' has invalid values",
                                     name))
             }
             ## 'alphaLN2' and 'restrictLN2' have same length
             if (!identical(length(alphaLN2), length(restrictLN2)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "alphaLN2", "restrictLN2"))
             ## length of 'restrictLN2' less than or equal to length of 'restructAllLN2'
             if (length(restrictLN2) > length(restrictAllLN2))
                 return(gettextf("length of '%s' greater than length of '%s'",
                                 "restrictLN2", "restrictAllLN2"))
             ## 'nCellBeforeLN2' has same length as 'alphaLN2'
             if (!identical(length(nCellBeforeLN2), length(alphaLN2)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "nCellBeforeLN2", "alphaLN2"))
             ## 'alphaLN2' has length implied by 'transformLN2'
             if (!identical(length(alphaLN2), prod(transformLN2@dimAfter)))
                 return(gettextf("'%s' and '%s' incompatible",
                                 "alphaLN2", "transformLN2"))
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

LN2 <- function(restrict, structuralZeros = NULL,
                concordances = list(), priorSD = HalfT()) {
    ## restrict
    if (!methods::is(restrict, "Values"))
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
    ## structuralZeros
    structuralZeros <- checkAndTidyStructuralZeros(structuralZeros)
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
    multVarsigma <- priorSD@mult
    nuVarsigma <- priorSD@nu
    varsigmaMax <- priorSD@scaleMax
    ## return
    methods::new("SpecLikelihoodLN2",
                 AVarsigma = AVarsigma,
                 concordances = concordances,
                 multVarsigma = multVarsigma,
                 restrictLN2 = restrict,
                 nuVarsigma = nuVarsigma,
                 structuralZeros = structuralZeros,
                 varsigmaMax = varsigmaMax)
}

test_that("LN2 corrects objects of class SpecLikelihoodLN2 from valid inputs", {
    restrict <- Values(array(c(NA, -1L, 0L, 1L),
                             dim = c(2, 2),
                             dimnames = list(age = c("0-39", "40+"),
                                             sex = c("Female", "Male"))))
    obj <- LN2(restrict = restrict)
    expect_is(obj, "SpecLikelihoodLN2")
    expect_true(validObject(obj))
    sz <- Values(array(c(1L, 0L, 0L, 1L),
                       dim = c(2, 2),
                       dimnames = list(age = c("0-39", "40+"),
                                       sex = c("Female", "Male"))))
    concordances <- list(sex = Concordance(data.frame(from = c("F", "M", "Female", "Male"),
                                                      to = c("Female", "Male", "Female", "Male"))))
    obj <- LN2(restrict = restrict,
               structuralZeros = sz,
               concordances = concordances)
    expect_is(obj, "SpecLikelihoodLN2")
    expect_true(validObject(obj))
})


setMethod("SpecModel",
          signature(specInner = "SpecLikelihoodLN2"),
          function(specInner, call, nameY, dots, lower, upper,
                   priorSD, jump,
                   series, aggregate) {
              A.varsigma <- specInner@AVarsigma
              mult.varsigma <- specInner@multVarsigma
              nu.varsigma <- specInner@nuVarsigma
              restrictLN2 <- specInner@restrictLN2
              structuralZeros <- specInner@structuralZeros
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
                           multSigma = mult.sigma,
                           multVarsigma = mult.varsigma,
                           nameY = nameY,
                           nuSigma = nu.sigma,
                           nuVarsigma = nu.varsigma,
                           restrictLN2 = restrictLN2,
                           series = series,
                           structuralZeros = structuralZeros,
                           sigmaMax = sigma.max,
                           varsigmaMax = varsigma.max)
          })


test_that("SpecModel works with SpecLikelihoodLN2", {
    SpecModel <- demest:::SpecModel
    restrict <- Values(array(c(NA, -1L, 0L, 1L),
                             dim = c(2, 2),
                             dimnames = list(age = c("0-39", "40+"),
                                             sex = c("Female", "Male"))))
    sz <- Values(array(c(1L, 0L, 0L, 1L),
                       dim = c(2, 2),
                       dimnames = list(age = c("0-39", "40+"),
                                       sex = c("Female", "Male"))))
    concordances <- list(sex = Concordance(data.frame(from = c("F", "M", "Female", "Male"),
                                                      to = c("Female", "Male", "Female", "Male"))))
    spec.inner <- LN2(restrict = restrict,
                structuralZeros = sz,
                concordances = concordances)
    call <- call("Model",
                 quote(y ~ LN2(restrict = restrict,
                               structuralZeros = sz,
                               concordances = concordances)))
    ans.obtained <- SpecModel(specInner = spec.inner,
                              call = call,
                              nameY = new("Name", "y"),
                              dots = list(),
                              lower = NULL,
                              upper = NULL,
                              priorSD = NULL,
                              jump = NULL,
                              series = NULL,
                              aggregate = NULL)
    expect_true(validObject(ans.obtained))
    expect_is(ans.obtained, "SpecLN2")
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
              concordances <- object@concordances
              mult.sigma <- object@multSigma
              mult.varsigma <- object@multVarsigma
              restrict.all <- object@restrictLN2
              sigma.max <- object@sigmaMax@.Data
              structural.zeros <- object@structuralZeros
              nu.sigma <- object@nuSigma
              nu.varsigma <- object@nuVarsigma
              varsigma.max <- object@varsigmaMax@.Data
              metadataY <- y@metadata
              struc.zero.array <- makeStrucZeroArray(structuralZeros = structural.zeros, 
                                                     y = y) 
              y <- checkAndTidyYForStrucZero(y = y, 
                                             strucZeroArray = struc.zero.array) 
              sY <- stats::sd(log1p(as.numeric(y), na.rm = TRUE))
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
              y.collapsed <- y
              for (i in seq_along(concordances)) {
                  name <- names(concordances)[[i]]
                  if (name %in% names(y)) {
                      concordance <- concordances[[i]]
                      y.collapsed <- collapseCategories(y.collapsed,
                                                        dimension = name,
                                                        concordance = concordance)
                  }
              }
              restrict <- tryCatch(error = function(e) e,
                                   dembase::makeCompatible(x = restrict.all,
                                                           y = y.collapsed,
                                                           subset = TRUE))
              if (inherits(restrict, "error"))
                  stop(gettextf("'%s' and '%s' not compatible : %s",
                                "restrict", "y", restrict$message))
              transform <- makeTransform(x = y,
                                         y = restrict,
                                         subset = FALSE,
                                         concordances = concordances)
              if (inherits(transform, "error"))
                  stop(gettextf("'%s' not compatible with '%s' : %s",
                                "transform", "y", transform$message))
              alpha <- numeric(length = length(restrict))
              resid <- collapse(as.array(log1p(y) - log1p(exposure)),
                                transform = transform)
              for (i in seq_along(alpha)) {
                  if (is.na(restrict[i]))
                      alpha[i] <- stats::rnorm(n = 1L,
                                               mean = resid[i],
                                               sd = sigma@.Data)
                  else if (restrict[i] == -1L)
                      alpha[i] <- rtnorm1(mean = resid[i],
                                          sd = sigma@.Data,
                                          upper = 0,
                                          useC = TRUE)
                  else if (restrict[i] == 0L)
                      alpha[i] <- 0
                  else if (restrict == 1L)
                      alpha[i] <- rtnorm(mean = resid[i],
                                         sd = sigma@.Data,
                                         lower = 0,
                                         useC = TRUE)
                  else
                      stop(gettext("invalid value for '%s' [%s]",
                                   "restrict", restrict[i]))
              }
              n.cell.before <- integer(length = length(restrict))
              for (i in seq_along(n.cell.before)) {
                  n.cell.before[i] <- length(dembase:::getIBefore(i = i,
                                                                  transform = transform,
                                                                  useC = TRUE))
              }
              cellInLik <- rep(TRUE, times = length(theta)) # temporary value
              model <- methods::new("NormalVaryingVarsigmaUnknown",
                                    alphaLN2 = methods::new("ParameterVector", alpha),
                                    ASigma = A.sigma,
                                    AVarsigma = A.varsigma,
                                    call = call,
                                    cellInLik = cellInLik,
                                    metadataY = metadataY,
                                    nCellBeforeLN2 = n.cell.before,
                                    nuSigma = nu.sigma,
                                    nuVarsigma = nu.varsigma,
                                    restrictAllLN2 = restrict.all,
                                    restrictLN2 = restrict,
                                    sigma = sigma,
                                    sigmaMax = sigmaMax,
                                    strucZeroArray = struc.zero.array,
                                    transformLN2 = transform,
                                    varsigma = varsigma,
                                    varsigmaMax = varsigma.max)
              model <- makeCellInLik(model = model,
                                     y = y,
                                     strucZeroArray = struc.zero.array)
              model
          })




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
        transform <- object@transformLN2
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
        transform <- object@transformLN2
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
    nu <- object@nuVarsigma@.Data
    A <- object@AVarsigma@.Data
    max <- object@varsigmaMax@.Data
    cat("   log(y[i] + 1) ~ N(log(exposure[i] + 1) + alpha[j[i]], sdData^2)\n")
    cat("        alpha[i] ~ N*(0, sd^2)\n", sep = "")
    cat("          sdData ~ trunc-half-t(", nu, ", ", sep = "")
    cat(squaredOrNA(A), ", ", format(max, digits = 4), ")\n", sep = "")
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
    cat(name.y, response, " ~ N(log(", exposure, "[i] + 1) + alpha[j[i]], sd^2)\n",
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
    cat(name.y, "[i] ~ N(", exposure, "[i] * mean[i], sd[i]^2)\n", sep = "")
    n.spaces <- max(5L - nchar(name.y), 0L)
    spaces <- rep(" ", n.spaces)
    response <- sprintf("%slog(%s + 1)", spaces, name.y)
    cat(name.y, response, " ~ N(log(", exposure, "[i] + 1) + alpha[j[i]], sd^2)\n",
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
