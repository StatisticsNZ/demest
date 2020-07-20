
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
              alpha.raw.second <- alpha.second / model@sigma@.Data
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
                                    alphaRawLN2 <- alpha.raw.second,
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
              alpha.raw <- alpha / sigma@.Data
              n.cell.before <- rep(0L, times = length(alpha)) # temporary value
              cellInLik <- rep(TRUE, times = length(y)) # temporary value
              model <- methods::new("LN2",
                                    alphaLN2 = alpha,
                                    alphaRawLN2 = alpha.raw,
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


## NO_TESTS
setClass("LN2",
         slots = c(alphaLN2 = "ParameterVector",
                   alphaRawLN2 = "ParameterVector",
                   constraintLN2 = "Values",
                   constraintAllLN2 = "Values",
                   nCellBeforeLN2 = "integer",
                   transformLN2 = "CollapseTransformExtra"),
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
             alphaRawLN2 <- object@alphaRawLN2@.Data
             constraintLN2 <- object@constraintLN2
             constraintAllLN2 <- object@constraintAllLN2
             nCellBeforeLN2 <- object@nCellBeforeLN2
             transformLN2 <- object@transformLN2
             ## 'nCellBeforeLN2' not NA
             if (any(is.na(nCellBeforeLN2))) {
                 return(gettextf("'%s' has missing values",
                                 "nCellBeforeLN2"))
             }
             ## values of 'nCellBeforeLN2' all non-negative
             if (any(nCellBeforeLN2 < 0L))
                 return(gettextf("'%s' has negative values",
                                 "nCellBeforeLN2"))
             ## 'constraintLN2', 'constraintAllLN2' consist
             ## entirely of NAs, -1s, 0s, and 1s
             for (name in c("constraintLN2", "constraintAllLN2")) {
                 value <- methods::slot(object, name)
                 if (!all(value %in% c(NA, -1L, 0L, 1L)))
                     return(gettextf("'%s' has invalid values",
                                     name))
             }
             ## 'alphaLN2' and 'alphaLN2Raw' have same length
             ## 'alphaLN2' and 'constraintLN2' have same length
             if (!identical(length(alphaLN2), length(constraintLN2)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "alphaLN2", "constraintLN2"))
             ## length of 'constraintLN2' less than or equal to length of 'restructAllLN2'
             if (length(constraintLN2) > length(constraintAllLN2))
                 return(gettextf("length of '%s' greater than length of '%s'",
                                 "constraintLN2", "constraintAllLN2"))
             ## 'nCellBeforeLN2' has same length as 'alphaLN2'
             if (!identical(length(nCellBeforeLN2), length(alphaLN2)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "nCellBeforeLN2", "alphaLN2"))
             ## 'alphaLN2' has length implied by 'transformLN2'
             if (!identical(length(alphaLN2), as.integer(prod(transformLN2@dimAfter))))
                 return(gettextf("'%s' and '%s' incompatible",
                                 "alphaLN2", "transformLN2"))
             ## 'alphaLN2' respects constraints in 'constraint'
             for (j in seq_along(constraintLN2)) {
                 cj <- constraintLN2[j]
                 aj <- alphaLN2[j]
                 if (!is.na(cj)) {
                     if (((cj == -1L) && (aj > 0))
                         || ((cj == 0L) && (aj != 0))
                         || ((cj == 1L) && (aj < 0)))
                         return(gettextf("'%s' outside bounds specified in '%s'",
                                         "alphaLN2", "constraintLN2"))
                 }
             }
             TRUE
         })


## TRANSLATED
## HAS_TESTS
updateAlphaLN2 <- function(object, y, exposure, useC = FALSE) {
    ## object
    stopifnot(methods::is(object, "LN2"))
    stopifnot(methods::validObject(object))
    ## y
    stopifnot(identical(length(y), length(object@cellInLik)))
    stopifnot(is.integer(y))
    stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0L))
    ## exposure
    stopifnot(is.integer(exposure))
    stopifnot(!any(is.na(exposure)))
    stopifnot(all(exposure >= 0L))
    ## y and exposure
    stopifnot(identical(length(exposure), length(y)))
    if (useC) {
        .Call(updateAlphaLN2_R, object, y, exposure)
    }
    else {
        alpha <- object@alphaLN2@.Data
        alpha.raw <- object@alphaRawLN2.Data
        cell.in.lik <- object@cellInLik
        n.cell.vec <- object@nCellBeforeLN2
        constraint <- object@constraintLN2@.Data
        transform <- object@transformLN2
        varsigma <- object@varsigma@.Data # variance of log(y + 1)
        sigma <- object@sigma@.Data # variance of alpha
        varsigma.sq <- varsigma^2
        sigma.sq <- sigma^2
        ## calculate the residuals
        resid.vec <- rep(0, times = length(alpha))
        for (i in seq_along(y)) {
            if (cell.in.lik[i]) {
                resid <- log1p(y[i]) - log1p(exposure[i])
                j <- dembase::getIAfter(i = i,
                                        transform = transform)
                resid.vec[j] <- resid.vec[j] + resid
            }
        }
        ## update 'alpha.raw'
        for (j in seq_along(alpha.raw)) {
            constraint.j <- constraint[j]
            update <- is.na(constraint.j) || (constraint.j != 0L)
            if (update) {
                n.cell <- n.cell.vec[j]
                prec.data  <- n.cell / (varsigma.sq / sigma.sq)
                prec.prior <- 1
                V <- 1 / (prec.data + prec.prior)
                scaled.resid <- resid.vec[j] / sigma
                mean.post <- prec.data * V * scaled.resid / n.cell
                sd.post <- sqrt(V)
                if (is.na(constraint.j)) {
                    alpha.raw[j] <- stats::rnorm(n = 1L,
                                                 mean = mean.post,
                                                 sd = sd.post)
                }
                else if (constraint.j == -1L) {
                    alpha[j] <- rtnorm1(mean = mean.post,
                                        sd = sd.post,
                                        lower = -Inf,
                                        upper = 0)
                }
                else if (constraint.j == 1L) {
                    alpha[j] <- rtnorm1(mean = mean.post,
                                        sd = sd.post,
                                        lower = 0,
                                        upper = Inf)
                }
                else
                    stop("invalid value for 'constraint'")
            }
            ## update.alpha
            for (j in seq_along(alpha.raw))
                alpha[j]  <- sigma * alpha.raw[j]
        }
        object@alphaLN2@.Data <- alpha
        object
    }
}









