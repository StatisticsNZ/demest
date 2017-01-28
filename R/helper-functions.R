
## DEMOGRAPHIC OBJECTS #############################################################

## HAS_TESTS
checkAllTermsInFormulaSpecified <- function(formula, namesSpecPriors) {
    labels <- attr(stats::terms(formula), "term.labels")
    not.specified <- setdiff(labels, namesSpecPriors)
    n.not.specified <- length(not.specified)
    if (n.not.specified > 0L) {
        stop(sprintf(ngettext(n.not.specified,
                              "no prior specified for term %s in formula '%s'",
                              "no priors specified for terms %s in formula '%s'"),
                     paste(sQuote(not.specified), collapse = ", "),
                     deparse(formula)))
    }
    NULL
}

## ## HAS_TESTS
## makeFakeBetas <- function(y, formula, specPriors, namesSpecPriors, intercept) {
##     dim.y <- dim(y)
##     dimnames.y <- dimnames(y)
##     names.y <- names(y)
##     metadata.y <- y@metadata
##     checkTermsFromFormulaFound(y = y, formula = formula)
##     checkLengthDimInFormula(y = y, formula = formula)
##     checkAllTermsInFormulaSpecified(formula = formula, namesSpecPriors = namesSpecPriors)
##     betas <- vector(mode = "list", length = length(namesSpecPriors))
##     for (i in seq_along(betas)) {
##         name.split <- strsplit(namesSpecPriors[i], split = ":", fixed = TRUE)[[1L]]
##         margin <- match(name.split, names.y)
##         metadata <- metadata.y[margin]
##         betas[[i]] <- fakeBeta(object = specPriors[[i]], metadata = metadata)
##     }
##     betas <- c(list(intercept), betas)
##     betas
## }

## ## HAS_TESTS
## makeFakeBetasOutput <- function(betas, namesBetas, y) {
##     n <- length(betas)
##     names.y <- names(y)
##     metadata.y <- y@metadata
##     if (n > 1L) {
##         for (i in seq.int(from = 2L, to = n)) {
##             name.split <- strsplit(namesBetas[i], split = ":", fixed = TRUE)[[1L]]
##             margin <- match(name.split, names.y)
##             metadata.beta <- metadata.y[margin]
##             .Data.beta <- array(betas[[i]],
##                                 dim = dim(metadata.beta),
##                                 dimnames = dimnames(metadata.beta))
##             betas[[i]] <- methods::new("Values", .Data = .Data.beta, metadata = metadata.beta)
##         }
##     }
##     names(betas) <- namesBetas
##     betas
## }

## ## HAS_TESTS
## makeFakeSigma <- function(dfPriorSigma, scalePriorSigma) {
##     sigma.has.improper.prior <- dfPriorSigma < 0
##     if (sigma.has.improper.prior)
##         stop(gettextf("'%s' must have an informative prior with function '%s'",
##                       "prior.sd", "fakeData"))
##     rinvchisq1(df = dfPriorSigma, scale = scalePriorSigma)
## }

## HAS_TESTS
makeIteratorBetas <- function(betas, namesBetas, y) {
    n <- length(betas)
    names.y <- names(y)
    dim.y <- dim(y)
    margins <- vector(mode = "list", length = n)
    margins[[1L]] <- 0L
    if (n > 1L) {
        for (i in seq.int(from = 2L, to = n)) {
            name.split <- strsplit(namesBetas[i], split = ":", fixed = TRUE)[[1L]]
            margins[[i]] <- match(name.split, names.y)
        }
    }
    BetaIterator(dim = dim.y, margins = margins)
}

## TRANSLATED
## HAS_TESTS
makeMu <- function(n, betas, iterator, useC = FALSE) {
    ## n
    stopifnot(is.integer(n))
    stopifnot(!is.na(n))
    stopifnot(n > 0L)
    ## betas
    stopifnot(is.list(betas))
    stopifnot(all(sapply(betas, is.numeric)))
    stopifnot(all(sapply(betas, function(x) !any(is.na(x)))))
    ## iterator
    stopifnot(methods::is(iterator, "BetaIterator"))
    if (useC) {
        .Call(makeMu_R, n, betas, iterator)
    }
    else {
        iterator <- resetB(iterator)
        mu <- numeric(n)
        for (i in seq_len(n)) {
            indices <- iterator@indices
            mu[i] <- 0
            for (b in seq_along(betas))
                mu[i] <- mu[i] + betas[[b]][indices[b]]
            iterator <- advanceB(iterator)
        }
        mu
    }
}

## HAS_TESTS
## Redistribute values in 'x' to ensure that none
## exceeds the corresponding value in 'max',
## while respecting 'subtotal'.
maxWithSubtotal <- function(x, max, subtotal) {
    if (!is.integer(x))
        stop(gettextf("'%s' does not have type \"%s\"",
                      "x", "integer"))
    if (any(x < 0, na.rm = TRUE))
        stop(gettextf("'%s' has negative values",
                      "x"))
    if (!is.integer(max))
        stop(gettextf("'%s' does not have type \"%s\"",
                      "max", "integer"))
    if (any(is.na(max)))
        stop(gettextf("'%s' has missing values",
                      "max"))
    if (!identical(length(x), length(max)))
        stop(gettextf("'%s' and '%s' have different lengths",
                      "x", "max"))
    if (!identical(length(subtotal), 1L))
        stop(gettextf("'%s' does not have length %d",
                      "subtotal", 1L))
    if (!is.integer(subtotal))
        stop(gettextf("'%s' does not have type \"%s\"",
                      "subtotal", "integer"))
    if (is.na(subtotal))
        stop(gettextf("'%s' is missing",
                      "subtotal"))
    if (sum(max) < subtotal)
        stop(gettextf("'%s' and '%s' incompatible",
                      "max", "subtotal"))
    has.missing <- any(is.na(x))
    if (has.missing) {
        if (sum(x, na.rm = TRUE) > subtotal)
            stop(gettextf("'%s' and '%s' incompatible",
                          "x", "subtotal"))
        is.more.than.max <- !is.na(x) & (x > max)
        x[is.more.than.max] <- max[is.more.than.max]
        x
    }
    else {
        if (sum(x) != subtotal)
            stop(gettextf("'%s' and '%s' incompatible",
                          "x", "subtotal"))
        else if (sum(max) == subtotal)
            max
        else {
            is.more.than.max <- x > max
            total.redistribute <- sum(x[is.more.than.max]) - sum(max[is.more.than.max])
            x[is.more.than.max] <- max[is.more.than.max]
            for (i in seq_len(total.redistribute)) {
                is.less.than.max <- x < max  ## recalculate each iteration
                ## the following two lines are slightly awkward, but are necessary
                ## because 'sample' assumes that if the first argument has length 1, it
                ## is the length of the vector to choose values from
                prob <- ifelse(is.less.than.max, max - x, 0)
                i.add <- sample(seq_along(x), size = 1L, prob = prob)
                x[i.add] <- x[i.add] + 1L
            }
            x
        }
    }
}




## SPECIFICATIONS ##################################################################

## NO_TESTS
checkAndTidyDFVectors <- function(df) {
    checkPositiveNumericVector(x = df,
                               name = "df")
    if (length(x) == 0L)
        stop(gettextf("'%s' has length %d",
                      name, 0L))
    as.double(df)
}

## NO_TESTS
checkScaleVectors <- function(scale) {
    if (!is.list(scale))
        stop(gettextf("'%s' has class \"%s\"",
                      "scale", class(scale)))
    if (length(scale) == 0L)
        stop(gettextf("'%s' has length %d",
                      "scale", 0L))
    if (!all(sapply(scale, methods::is, "HalfT")))
        stop(gettextf("'%s' has elements not of class \"%s\"",
                      "scale", "HalfT"))
    NULL
}

    


## HAS_TESTS
checkLowerOrUpper <- function(value,
                              name = c("lower", "upper"),
                              distribution = c("Binomial", "Normal", "Poisson")) {
    name <- match.arg(name)
    distribution <- match.arg(distribution)
    is.lower <- identical(name, "lower")
    if (!identical(length(value), 1L))
        stop(gettextf("'%s' does not have length 1",
                      name))
    if (!is.numeric(value))
        stop(gettextf("'%s' is non-numeric",
                      name))
    if (is.na(value))
        stop(gettextf("'%s' is missing",
                      name))
    if (identical(distribution, "Binomial")) {
        if (identical(name, "lower")) {
            if (value < 0)
                stop(gettextf("'%s' is less than %d",
                              name, 0L))
        }
        else {
            if (value > 1)
                stop(gettextf("'%s' is greater than %d",
                              name, 1L))
        }
    }
    if (identical(distribution, "Poisson")) {
        if (is.lower) {
            if (value < 0)
                stop(gettextf("'%s' is less than %d",
                              name, 0L))
        }
    }
    NULL
}    

## HAS_TESTS
checkLowerAndUpper <- function(lower, upper,
                               distribution = c("Binomial", "Normal", "Poisson")) {
    distribution <- match.arg(distribution)
    checkLowerOrUpper(value = lower, name = "lower", distribution = distribution)
    checkLowerOrUpper(value = upper, name = "upper", distribution = distribution)
    if (lower >= upper)
        stop(gettextf("'%s' is not less than '%s'",
                      "lower", "upper"))
    NULL
}

## HAS_TESTS
checkAndTidyJump <- function(jump) {
    if (is.null(jump)) {
        methods::new("Scale", 0.1)
    }
    else {        
        ## 'jump' has length 1
        if (!identical(length(jump), 1L))
            stop(gettextf("'%s' does not have length %d", "jump", 1L))
        ## 'jump' is not missing
        if (is.na(jump))
            stop(gettextf("'%s' is missing", "jump"))
        ## 'jump' is numeric
        if (!is.numeric(jump))
            stop(gettextf("'%s' is not numeric",
                          "jump"))
        jump <- as.double(jump)
        ## 'jump' is non-negative
        if (jump < 0)
            stop(gettextf("'%s' is negative", "jump"))
        methods::new("Scale", jump)
    }
}

## HAS_TESTS
checkAndTidyMaxAttempt <- function(maxAttempt) {
    if (!identical(length(maxAttempt), 1L))
        stop(gettextf("'%s' does not have length %d",
                      "maxAttempt", 1L))
    if (!is.numeric(maxAttempt))
        stop(gettextf("'%s' is non-numeric",
                      "maxAttempt"))
    if (is.na(maxAttempt))
        stop(gettextf("'%s' is missing",
                      "maxAttempt"))
    if (round(maxAttempt) != maxAttempt)
        stop(gettextf("'%s' has a non-integer value",
                      "maxAttempt"))
    if (maxAttempt < 1L)
        stop(gettextf("'%s' is non-positive",
                      "maxAttempt"))
    as.integer(round(maxAttempt))
}

## HAS_TESTS
checkAndTidyMeanOrProb <- function(object, name = "mean") {
    ## is numeric
    if (!is.numeric(object))
        stop(gettextf("'%s' is not numeric",
                      name))
    ## has length 1
    if (!identical(length(object), 1L))
        stop(gettextf("'%s' does not have length %d", name, 1L))
    ## is not missing
    if (is.na(object))
        stop(gettextf("'%s' is missing", "mean"))
    object <- as.double(object)
    object
}

## HAS_TESTS
## assume that have already checked that formula has response
extractResponse <- function(formula, separateNames = FALSE) {
    if (!hasResponse(formula))
        stop(gettextf("formula '%s' does not have a response",
                      deparse(formula)))
    ans <- formula[[2L]]
    ans <- deparse(ans)
    if (separateNames) {
        ans <- stats::as.formula(paste("~", ans))
        ans <- rownames(attr(stats::terms(ans), "factor"))
    }
    ans
}

## HAS_TESTS
hasResponse <- function(formula)
    identical(attr(stats::terms(formula), "response"), 1L)

## NO_TESTS
checkCovariateData <- function(x, name) {
    ## Do not check for variables or NAs
    ## to avoid raising errors for variables
    ## or records that are not subsequently
    ## used.  Check within initialPrior
    ## instead.
    if (!is.data.frame(x))
        stop(gettextf("'%s' has class \"%s\"",
                      name, class(x)))
    NULL
}

## HAS_TESTS
checkCovariateFormula <- function(formula) {
    if (!hasResponse(formula))
        stop(gettextf("formula '%s' does not include a response",
                      deparse(formula)))
    if (!identical(deparse(formula[[2L]]), "mean"))
        stop(gettextf("response in formula '%s' is not '%s'",
                      deparse(formula), "mean"))
    has.intercept <- identical(attr(stats::terms(formula), "intercept"), 1L)
    if (!has.intercept)
        stop(gettextf("formula '%s' does not include an intercept",
                      deparse(formula)))
    if (identical(length(attr(stats::terms(formula), "term.labels")), 0L))
        stop(gettextf("formula '%s' does not include any predictors (other than the intercept)",
                      deparse(formula)))
    NULL
}

## NO_TESTS
checkLogical <- function(x, name) {
    if (!identical(length(x), 1L))
        stop(gettextf("'%s' does not have length %d",
                      name, 1L))
    if (!is.logical(x))
        stop(gettextf("'%s' does not have type \"%s\"",
                      name, "logical"))
    if (is.na(x))
        stop(gettextf("'%s' is missing",
                      name))
    NULL
}

## NO_TESTS
checkModelMatrix <- function(formula, data, contrastsArg) {
    formula.no.response <- formula[-2L]
    contrasts.arg <- if (identical(contrastsArg, list())) NULL else contrastsArg
    return.value <- tryCatch(stats::model.matrix(object = formula.no.response,
                                                 data = data,
                                                 contrasts.arg = contrasts.arg),
                             error = function(e) e)
    if (methods::is(return.value, "error"))
        stop(gettextf("problem constructing model matrix from formula '%s' : %s",
                      deparse(formula), return.value$message))
    NULL
}

## NO_TESTS
checkPositiveInteger <- function(x, name) {
    ## 'x' has length 1
    if (!identical(length(x), 1L))
        stop(gettextf("'%s' does not have length %d",
                      name, 1L))
    ## 'x' is not missing
    if (is.na(x))
        stop(gettextf("'%s' is missing",
                      name))
    ## 'x' is numeric
    if (!is.numeric(x))
        stop(gettextf("'%s' is non-numeric",
                      name))
    ## 'x' is an integer
    if (!isTRUE(all.equal(x, round(x))))
        stop(gettextf("'%s' is not an integer",
                      name))
    ## 'x' is positive
    if (x <= 0)
        stop(gettextf("'%s' is non-positive",
                      name))
    NULL
}

## NO_TESTS
checkPositiveNumeric <- function(x, name) {
    ## 'x' has length 1
    if (!identical(length(x), 1L))
        stop(gettextf("'%s' does not have length %d",
                      name, 1L))
    ## 'x' is not missing
    if (is.na(x))
        stop(gettextf("'%s' is missing",
                      name))
    ## 'x' is numeric
    if (!is.numeric(x))
        stop(gettextf("'%s' is not numeric",
                      name))
    ## 'x' is positive
    if (x <= 0)
        stop(gettextf("'%s' is non-positive",
                      name))
    NULL
}

## NO_TESTS
checkPositiveNumericVector <- function(x, name) {
    ## 'x' has no missing values
    if (any(is.na(x)))
        stop(gettextf("'%s' has missing values",
                      name))
    ## 'x' is numeric
    if (!is.numeric(x))
        stop(gettextf("'%s' is not numeric",
                      name))
    ## 'x' is positive
    if (any(x <= 0))
        stop(gettextf("'%s' has non-positive values",
                      name))
    NULL
}

## NO_TESTS
checkAndTidyAlongDLM <- function(along) {
    if (is.null(along)) {
        as.character(NA)
    }
    else {
        ## 'along' has length 1
        if (!identical(length(along), 1L))
            stop(gettextf("'%s' does not have length %d",
                          "along", 1L))
        ## 'along' has type "character"
        if (!is.character(along))
            stop(gettextf("'%s' does not have type \"%s\"",
                          "along", "character"))
        ## 'along' is not missing
        if (is.na(along))
            stop(gettextf("'%s' is missing",
                          "along"))
        ## 'along' is not blank
        if (!nzchar(along))
            stop(gettextf("'%s' is blank",
                          "along"))
        along
    }
}

## NO_TESTS
checkAndTidyLogicalFlag <- function(x, name) {
    checkLogical(x = x, name = name)
    methods::new("LogicalFlag", x)
}

## NO_TESTS
checkAndTidyMult <- function(mult, scale, nameScale) {
    checkPositiveNumeric(x = mult, name = "mult")
    mult <- as.double(mult)
    non.default.mult <- !isTRUE(all.equal(mult, 1.0))
    non.default.scale <- !is.na(scale@.Data)
    if (non.default.scale && non.default.mult)
        warning(gettextf("'%s' argument ignored when '%s' argument supplied",
                         "mult", nameScale))
    methods::new("Scale", mult)
}

## NO_TESTS
checkAndTidyNorm <- function(x, name) {
    if (is.null(x))
        Norm()
    else {
        x
    }
}

## HAS_TESTS
checkAndTidyNSeason <- function(n) {
    checkPositiveInteger(x = n, name = "n")
    n <- as.integer(n)
    if (n < 2L)
        stop(gettextf("'%s' is less than %d",
                      "n", 2L))
    methods::new("Length", n)
}

## NO_TESTS
checkAndTidyNu <- function(x, name) {
    checkPositiveNumeric(x = x, name = name)
    x <- as.double(x)
    methods::new("DegreesFreedom", x)
}

## NO_TESTS
checkAndTidyPhi <- function(phi) {
    checkPositiveNumeric(x = phi, name = "damp")
    if (phi > 1)
        stop(gettextf("'%s' is greater than %d",
                      "damp", 1L))
    as.double(phi)
}

## NO_TESTS
checkAndTidyPhiMinMax <- function(min, max) {
    checkPositiveNumeric(x = min, name = "min")
    checkPositiveNumeric(x = max, name = "max")
    if (max > 1)
        stop(gettextf("'%s' is greater than %d",
                      "max", 1L))
    min <- as.double(min)
    max <- as.double(max)
    if (min >= max)
        stop(gettextf("'%s' is not less than '%s'",
                      "min", "max"))
    c(min, max)
}

## NO_TESTS
checkAndTidyScaleMax <- function(x, name, nu, A) {
    if (is.null(x)) {
        x <- as.double(NA)
        x <- methods::new("SpecScale", x)
        if (is.na(A))
            x
        else {
            x <- makeScaleMax(scaleMax = x,
                              A = A,
                              nu = nu)
            methods::new("SpecScale", x@.Data)
        }
    }
    else {
        checkPositiveNumeric(x = x, name = name)
        x <- as.double(x)
        methods::new("SpecScale", x)
    }
}

## NO_TESTS
checkAndTidySpecScale <- function(x, name) {
    if (is.null(x))
        x <- as.double(NA)
    else {
        checkPositiveNumeric(x = x, name = name)
        x <- as.double(x)
    }
    methods::new("SpecScale", x)
}

## HAS_TESTS
checkAndTidySeries <- function(series) {
    if (!is.null(series)) {
        if (!is.character(series))
            stop(gettextf("'%s' does not have type \"%s\"",
                          "series", "character"))
        if (!identical(length(series), 1L))
            stop(gettextf("'%s' does not have length %d",
                          "series", 1L))
        if (!nzchar(series))
            stop(gettextf("'%s' is blank",
                          "series"))
        methods::new("SpecName", series)
    }
    else
        methods::new("SpecName", as.character(NA))
}


    
## INITIAL VALUES - PRIORS ###################################################        

## HAS_TESTS            
initialCov <- function(object, beta, metadata, sY) {
    AEtaCoef <- object@AEtaCoef
    AEtaIntercept <- object@AEtaIntercept
    contrastsArg <- object@contrastsArg
    data <- object@data
    formula <- object@formula
    multEtaCoef <- object@multEtaCoef
    nuEtaCoef <- object@nuEtaCoef
    AEtaCoef <- makeAHalfT(A = AEtaCoef,
                           metadata = metadata,
                           sY = sY,
                           mult = multEtaCoef)
    AEtaIntercept <- makeAIntercept(A = AEtaIntercept, sY = sY)
    Z <- makeZ(formula = formula[-2L],
               data = data,
               metadata = metadata,
               contrastsArg = contrastsArg)
    P <- makeP(Z)
    UEtaCoef <- makeU(nu = nuEtaCoef, A = AEtaCoef, n = P - 1L)
    eta <- makeEta(beta = beta, UEtaCoef = UEtaCoef)
    list(AEtaCoef = AEtaCoef,
         AEtaIntercept = AEtaIntercept,
         contrastsArg = contrastsArg,
         eta = eta,
         formula = formula,
         nuEtaCoef = nuEtaCoef,
         P = P,
         UEtaCoef = UEtaCoef,
         Z = Z)
}

## HAS_TESTS            
initialCovPredict <- function(prior, data, metadata) {
    formula <- prior@formula
    contrastsArg <- prior@contrastsArg
    Z <- makeZ(formula = formula[-2L],
               data = data,
               metadata = metadata,
               contrastsArg = contrastsArg)
    list(Z = Z)
}

## HAS_TESTS
initialDLMAll <- function(object, beta, metadata, sY, ...) {
    AAlpha <- object@AAlpha
    ATau <- object@ATau
    along <- object@along
    multAlpha <- object@multAlpha
    multTau <- object@multTau
    nuAlpha <- object@nuAlpha
    nuTau <- object@nuTau
    omegaAlphaMax <- object@omegaAlphaMax
    phi <- object@phi
    phiKnown <- object@phiKnown
    minPhi <- object@minPhi
    maxPhi <- object@maxPhi
    tauMax <- object@tauMax
    dim <- dim(metadata)
    J <- makeJ(beta)
    ATau <- makeAHalfT(A = ATau,
                       metadata = metadata,
                       sY = sY,
                       mult = multTau)
    tauMax <- makeScaleMax(scaleMax = tauMax,
                           A = ATau,
                           nu = nuTau)
    tau <- makeScale(A = ATau,
                     nu = nuTau,
                     scaleMax = tauMax)
    if (is.na(along))
        along <- NULL
    iAlong <- dembase::checkAndTidyAlong(along = along,
                                         metadata = metadata,
                                         numericDimScales = TRUE)
    K <- makeK(dim = dim, iAlong = iAlong)
    L <- makeL(dim = dim, iAlong = iAlong)
    dim.alpha.delta <- dim
    dim.alpha.delta[iAlong] <- dim.alpha.delta[iAlong] + 1L
    iteratorState <- AlongIterator(dim = dim.alpha.delta,
                                   iAlong = iAlong)
    iteratorV <- AlongIterator(dim = dim,
                               iAlong = iAlong)
    AAlpha <- makeAHalfT(A = AAlpha,
                         metadata = metadata,
                         sY = sY,
                         mult = multAlpha)
    omegaAlphaMax <- makeScaleMax(scaleMax = omegaAlphaMax,
                                  A = AAlpha,
                                  nu = nuAlpha)        
    omegaAlpha <- makeScale(A = AAlpha,
                            nu = nuAlpha,
                            scaleMax = omegaAlphaMax)
    alphaDLM <- makeStateDLM(K = K, L = L)
    phi <- makePhi(phi = phi,
                   phiKnown = phiKnown,
                   minPhi = minPhi,
                   maxPhi = maxPhi)                             
    list(AAlpha = AAlpha,
         ATau = ATau,
         alphaDLM = alphaDLM,
         iAlong = iAlong,
         iteratorState = iteratorState,
         iteratorV = iteratorV,
         J = J,
         K = K,
         L = L,
         minPhi = minPhi,
         maxPhi = maxPhi,
         nuAlpha = nuAlpha,
         nuTau = nuTau,
         omegaAlpha = omegaAlpha,
         omegaAlphaMax = omegaAlphaMax,
         phi = phi,
         phiKnown = phiKnown,
         tau = tau,
         tauMax = tauMax)
}

## HAS_TESTS
initialDLMAllPredict <- function(prior, metadata, name, along) {
    alpha.old <- prior@alphaDLM
    J.old <- prior@J
    K.old <- prior@K@.Data
    iterator.state.old <- prior@iteratorState
    i.along.old <- prior@iAlong
    J <- makeJPredict(metadata)
    i.along.new <- dembase::checkAndTidyAlong(along = along,
                                              metadata = metadata,
                                              numericDimScales = TRUE)
    if (!identical(i.along.new, i.along.old))
        stop(gettextf("\"%s\" dimension of prediction does not match \"%s\" dimension of prior for '%s'",
                      "along", "along", name))
    dim <- dim(metadata)
    K.new <- makeK(dim = dim, iAlong = i.along.new)
    L <- makeL(dim = dim, iAlong = i.along.new)
    dim.alpha.delta <- dim
    dim.alpha.delta[i.along.new] <- dim.alpha.delta[i.along.new] + 1L
    iterator.state.new <- AlongIterator(dim = dim.alpha.delta,
                                        iAlong = i.along.new)
    iterator.v <- AlongIterator(dim = dim,
                                iAlong = i.along.new)
    alpha.new <- makeStateDLM(K = K.new, L = L)
    list(alphaDLM = alpha.new,
         iteratorState = iterator.state.new,
         iteratorStateOld = iterator.state.old,
         iteratorV = iterator.v,
         J = J,
         JOld = J.old,
         K = K.new,
         L = L)
}

## HAS_TESTS
initialDLMNoTrend <- function(object, metadata, sY) {
    along <- object@along
    dim <- dim(metadata)
    if (is.na(along))
        along <- NULL
    i.along <- dembase::checkAndTidyAlong(along = along,
                                         metadata = metadata,
                                         numericDimScales = TRUE)
    K <- makeK(dim = dim, iAlong = i.along)
    L <- makeL(dim = dim, iAlong = i.along)
    mNoTrend <- makeMNoTrend(K = K)
    m0NoTrend <- makeM0NoTrend(L = L)
    CNoTrend <- makeCNoTrend(K = K, sY = sY)
    aNoTrend <- makeANoTrend(K = K)
    RNoTrend <- makeRNoTrend(K = K)
    list(aNoTrend = aNoTrend,
         CNoTrend = CNoTrend,
         mNoTrend = mNoTrend,
         m0NoTrend = m0NoTrend,
         RNoTrend = RNoTrend)
}

## HAS_TESTS
initialDLMNoTrendPredict <- function(prior, metadata) {
    alpha.old <- prior@alphaDLM@.Data
    iterator.old <- prior@iteratorState
    i.along <- prior@iAlong
    dim <- dim(metadata)
    K <- makeK(dim = dim, iAlong = i.along)
    L <- makeL(dim = dim, iAlong = i.along)
    mNoTrend <- makeMNoTrend(K = K)
    m0NoTrend <- makeM0NoTrend(L = L)
    CNoTrend <- makeCNoTrend(K = K, C0 = 0)
    aNoTrend <- makeANoTrend(K = K)
    RNoTrend <- makeRNoTrend(K = K)
    iterator.new <- AlongIterator(dim = dim,
                                  iAlong = i.along)
    list(aNoTrend = aNoTrend,
         CNoTrend = CNoTrend,
         mNoTrend = mNoTrend,
         m0NoTrend = m0NoTrend,
         RNoTrend = RNoTrend)
}

## HAS_TESTS
initialDLMWithTrend <- function(object, beta, metadata, sY, lAll) {
    ADelta <- object@ADelta
    ADelta0 <- object@ADelta0
    meanDelta0 <- object@meanDelta0
    along <- object@along
    multDelta <- object@multDelta
    multDelta0 <- object@multDelta0
    nuDelta <- object@nuDelta
    omegaDeltaMax <- object@omegaDeltaMax
    dim <- dim(metadata)
    J <- makeJ(beta)
    ADelta <- makeAHalfT(A = ADelta,
                         metadata = metadata,
                         sY = sY,
                         mult = multDelta)
    ADelta0 <- makeAHalfT(A = ADelta0,
                          metadata = metadata,
                          sY = sY,
                          mult = multDelta0)
    omegaDeltaMax <- makeScaleMax(scaleMax = omegaDeltaMax,
                                  A = ADelta,
                                  nu = nuDelta)        
    omegaDelta <- makeScale(A = ADelta,
                            nu = nuDelta,
                            scaleMax = omegaDeltaMax)
    if (is.na(along))
        along <- NULL
    iAlong <- dembase::checkAndTidyAlong(along = along,
                                         metadata = metadata,
                                         numericDimScales = TRUE)
    K <- makeK(dim = dim,
               iAlong = iAlong)
    L <- makeL(dim = dim,
               iAlong = iAlong)
    deltaDLM <- makeStateDLM(K = K, L = L)
    mWithTrend <- makeMWithTrend(K = K)
    m0WithTrend <- makeM0WithTrend(L = L,
                                   meanDelta0 = meanDelta0)
    CWithTrend <- makeCWithTrend(K = K,
                                 sY = sY,
                                 ADelta0 = ADelta0)
    aWithTrend <- makeAWithTrend(K = K)
    RWithTrend <- makeRWithTrend(K = K)
    UC <- makeUC(K)
    DC <- makeDC(CWithTrend = CWithTrend)
    DCInv <- makeDCInv(DC)
    UR <- makeUR(K)
    DRInv <- makeDRInv(K)
    omegaAlpha <- lAll$omegaAlpha
    phi <- lAll$phi
    WSqrt <- makeWSqrt(omegaAlpha = omegaAlpha,
                       omegaDelta = omegaDelta)
    WSqrtInvG <- makeWSqrtInvG(omegaAlpha = omegaAlpha,
                               omegaDelta = omegaDelta,
                               phi = phi)
    GWithTrend <- makeGWithTrend(phi = phi)
    list(ADelta = ADelta,
         ADelta0 = ADelta0,
         aWithTrend = aWithTrend,
         CWithTrend = CWithTrend,
         DC = DC,
         DCInv = DCInv,
         DRInv = DRInv,
         deltaDLM = deltaDLM,
         GWithTrend = GWithTrend,
         mWithTrend = mWithTrend,
         m0WithTrend = m0WithTrend,
         meanDelta0 = meanDelta0,
         nuDelta = nuDelta,
         omegaDelta = omegaDelta,
         omegaDeltaMax = omegaDeltaMax,
         RWithTrend = RWithTrend,
         UC = UC,
         UR = UR,
         WSqrt = WSqrt,
         WSqrtInvG = WSqrtInvG)
}

## HAS_TESTS
initialDLMWithTrendPredict <- function(prior, metadata) {
    alpha.old <- prior@alphaDLM@.Data
    delta.old <- prior@deltaDLM@.Data
    iterator.old <- prior@iteratorState
    i.along <- prior@iAlong
    dim <- dim(metadata)
    K <- makeK(dim = dim, iAlong = i.along)
    L <- makeL(dim = dim, iAlong = i.along)
    deltaDLM <- makeStateDLM(K = K, L = L)
    mWithTrend <- makeMWithTrend(K = K)
    m0WithTrend <- makeM0WithTrend(L = L)
    C0 <- matrix(0, nrow = 2, ncol = 2)
    CWithTrend <- makeCWithTrend(K = K, C0 = C0)
    aWithTrend <- makeAWithTrend(K = K)
    RWithTrend <- makeRWithTrend(K = K)
    UC <- makeUC(K)
    DC <- makeDC(CWithTrend = CWithTrend)
    DCInv <- makeDCInv(DC)
    UR <- makeUR(K)
    DRInv <- makeDRInv(K)
    iterator.new <- AlongIterator(dim = dim,
                                  iAlong = i.along)
    vecOld <- lapply(seq_along(alpha.old), function(i) c(alpha.old[i], delta.old[i]))
    list(aWithTrend = aWithTrend,
         CWithTrend = CWithTrend,
         DC = DC,
         DCInv = DCInv,
         DRInv = DRInv,
         deltaDLM = deltaDLM,
         mWithTrend = mWithTrend,
         m0WithTrend = m0WithTrend,
         RWithTrend = RWithTrend,
         UC = UC,
         UR = UR)
}

## HAS_TESTS
initialDLMSeason <- function(object, beta, metadata, sY) {
    along <- object@along
    ASeason <- object@ASeason
    multSeason <- object@multSeason
    nSeason <- object@nSeason
    nuSeason <- object@nuSeason
    omegaSeasonMax <- object@omegaSeasonMax
    dim <- dim(metadata)
    J <- makeJ(beta)
    if (is.na(along))
        along <- NULL
    iAlong <- dembase::checkAndTidyAlong(along = along,
                                         metadata = metadata,
                                         numericDimScales = TRUE)
    K <- makeK(dim = dim, iAlong = iAlong)
    L <- makeL(dim = dim, iAlong = iAlong)
    ASeason <- makeAHalfT(A = ASeason,
                          metadata = metadata,
                          sY = sY,
                          mult = multSeason)
    omegaSeasonMax <- makeScaleMax(scaleMax = omegaSeasonMax,
                                   A = ASeason,
                                   nu = nuSeason)        
    omegaSeason <- makeScale(A = ASeason,
                             nu = nuSeason,
                             scaleMax = omegaSeasonMax)
    mSeason <- makeMSeason(K = K, nSeason = nSeason)
    m0Season <- makeM0Season(L = L, nSeason = nSeason)
    CSeason <- makeCSeason(K = K, nSeason = nSeason, ASeason = ASeason)
    aSeason <- makeASeason(K = K, nSeason = nSeason)
    RSeason <- makeRSeason(K = K, nSeason = nSeason)
    s <- makeSeasonDLM(K = K, L = L, nSeason = nSeason)
    list(ASeason = ASeason,
         aSeason = aSeason,
         CSeason = CSeason,
         mSeason = mSeason,
         m0Season = m0Season,
         nSeason = nSeason,
         nuSeason = nuSeason,
         omegaSeason = omegaSeason,
         omegaSeasonMax = omegaSeasonMax,
         RSeason = RSeason,
         s = s)
}

## HAS_TESTS
initialDLMSeasonPredict <- function(prior, metadata) {
    s.old <- prior@s@.Data
    iterator.old <- prior@iteratorState
    i.along <- prior@iAlong
    n.season <- prior@nSeason
    dim <- dim(metadata)
    K <- makeK(dim = dim, iAlong = i.along)
    L <- makeL(dim = dim, iAlong = i.along)
    mSeason <- makeMSeason(K = K, nSeason = n.season)
    m0Season <- makeM0Season(L = L, nSeason = n.season)
    C0 <- rep(0, times = n.season)
    CSeason <- makeCSeason(K = K, nSeason = n.season, C0 = C0)
    aSeason <- makeASeason(K = K, nSeason = n.season)
    RSeason <- makeRSeason(K = K, nSeason = n.season)
    s.new <- makeSeasonDLM(K = K, L = L, nSeason = n.season)
    iterator.new <- AlongIterator(dim = dim,
                                  iAlong = i.along)
    list(aSeason = aSeason,
         CSeason = CSeason,
         mSeason = mSeason,
         m0Season = m0Season,
         RSeason = RSeason,
         s = s.new)
}

## HAS_TESTS
initialRobust <- function(object, lAll) {
    nuBeta <- object@nuBeta
    J <- lAll$J
    ATau <- lAll$ATau
    UBeta <- makeU(nu = nuBeta, A = ATau, n = J)
    list(nuBeta = nuBeta,
         UBeta = UBeta)
}


## HAS_TESTS
initialRobustPredict <- function(prior, metadata) {
    ATau <- prior@ATau
    nuBeta <- prior@nuBeta
    J <- makeJPredict(metadata)
    UBeta <- makeU(nu = nuBeta, A = ATau, n = J)
    list(UBeta = UBeta)
}


## NO_TESTS
makeAMove <- function(A, metadata, sY, mult) {
    if (is.na(A)) {
        d <- length(metadata)
        ans <- (0.5)^d # not d-1
        if (!is.null(sY))
            ans <- sY * ans
        ans <- mult * ans
    }
    else
        ans <- A
    methods::new("Scale", ans)
}

## NO_TESTS
makeAHalfT <- function(A, metadata, sY, mult) {
    if (is.na(A)) {
        d <- length(metadata)
        ans <- (0.5)^(d - 1L)
        if (!is.null(sY))
            ans <- sY * ans
        ans <- mult * ans
    }
    else
        ans <- A
    methods::new("Scale", ans)
}

## NO_TESTS
makeAIntercept <- function(A, sY) {
    if (is.na(A)) {
        ans <- 10
        if (!is.null(sY))
            ans <- sY * ans
        methods::new("Scale", ans)
    }
    else
        methods::new("Scale", A)
}

## NO_TESTS
makeASigma <- function(A, sY, isSpec = FALSE) {
    if (is.na(A)) {
        ans <- 1
        if (!is.null(sY))
            ans <- sY * ans
    }
    else
        ans <- A
    if (isSpec)
        methods::new("SpecScale", ans)
    else
        methods::new("Scale", ans)
}

## NO_TESTS
makeEta <- function(beta, UEtaCoef) {
    P <- length(UEtaCoef) + 1L
    mean <- c(mean(beta), rep(0, times = P - 1L))
    sd <- c(1, sqrt(UEtaCoef))
    ans <- stats::rnorm(n = P, sd = sd)
    methods::new("ParameterVector", ans)
}

## NO_TESTS
makeAlphaMove <- function(beta, indexClassAlpha, nElementClassAlpha) {
    n.alpha <- length(nElementClassAlpha)
    ans <- double(length = n.alpha)
    for (j in seq_along(indexClassAlpha)) {
        k <- indexClassAlpha[j]
        if (k > 0L)
            ans[k] <- ans[k] + beta[j]
    }
    ans <- ans / nElementClassAlpha
    methods::new("ParameterVector", ans)
}    

## need to call 'alignPair' on 'y' at some point
## NO_TESTS
makeIndexClassAlpha <- function(classes, metadata) {
    name.term <- paste(names(metadata), collapse = ":")
    .Data <- array(0L,
                   dim = dim(metadata),
                   dimnames = dimnames(metadata))
    template.beta <- methods::new("Values",
                                  .Data = .Data,
                                  metadata = metadata)
    classes <- tryCatch(dembase::makeCompatible(x = classes,
                                                y = template.beta,
                                                subset = TRUE,
                                                check = TRUE),
                        error = function(e) e)
    if (methods::is(classes, "error"))
        stop(gettextf("problem aligning '%s' with interaction term '%s' : %s",
                      "cl", name.term, classes$message))
    as.integer(classes@.Data)
}        

## NO_TESTS
makeJ <- function(beta) {
    methods::new("Length", length(beta))
}

## NO_TESTS
makeJPredict <- function(metadata) {
    dim <- dim(metadata)
    length <- prod(dim)
    length <- as.integer(length)
    methods::new("Length", length)
}


    
## NO_TESTS
makeP <- function(Z) {
    ans <- ncol(Z)
    methods::new("Length", ans)
}

## NO_TESTS
makeScale <- function(A, nu, scaleMax) {
    max <- scaleMax@.Data
    ans <- rinvchisq1(df = nu, scale = A^2)
    if (ans > max)
        ans <- stats::runif(n = 1,
                            min = 0,
                            max = min(2 * A@.Data, max))
    methods::new("Scale", ans)
}

## NO_TESTS
makeScaleMax <- function(scaleMax, A, nu, isSpec = FALSE) {
    kPScaleMax <- 0.999
    if (is.na(scaleMax))
        scaleMax <- qhalft(p = kPScaleMax,
                           df = nu@.Data,
                           scale = A@.Data)
    if (isSpec)
        methods::new("SpecScale", scaleMax)
    else
        methods::new("Scale", scaleMax)
}        

## Expands factors into dummy variables, and then standardises as
## described in Gelman, A., Jakulin, A., Pittau, M. G., and Su, Y.-S.
## (2008). A weakly informative default prior distribution
## for logistic and other regression models.
## The Annals of Applied Statistics, pages 1360–1383.
makeStandardizedVariables <- function(formula, inputs, namePrior, contrastsArg) {
    if (identical(contrastsArg, list()))
        contrastsArg <- NULL
    ans <- tryCatch(stats::model.matrix(object = formula,
                                        data = inputs,
                                        contrasts.arg = contrastsArg),
                    error = function(e) e)
    if (methods::is(ans, "error"))
        stop(gettextf("problem constructing model matrix from formula '%s' in prior for '%s' : %s",
                      deparse(formula), namePrior, ans$message))
    which.term <- attr(ans, "assign")
    terms <- stats::terms(formula)
    factors <- attr(terms, "factors")
    order.term <- attr(terms, "order")
    for (j in seq_len(ncol(ans))[-1L]) {
        v <- ans[ , j]
        i.term <- which.term[j]
        is.main.effect <- order.term[i.term] == 1L
        if (is.main.effect) {
            is.binary <- isTRUE(all.equal(sort(unique(v)), 0:1))
            if (is.binary)
                v <- v - mean(v)
            else
                v <- (v - mean(v)) / (2 * stats::sd(v))
        }
        else {
            i.main.effect.contributes <- which(factors[ , i.term] == 1L) + 1L
            v <- ans[, i.main.effect.contributes, drop = FALSE]
            v <- apply(v, MARGIN = 1L, FUN = prod)
        }
        ans[ , j] <- v
    }
    array(ans, dim = dim(ans), dimnames = dimnames(ans))
}


## NO_TESTS
makeTauExchFixedIntercept <- function(tau, sY) {
    if (is.na(tau)) {
        if (is.null(sY))
            ans <- 10
        else
            ans <- 10 * sY
    }
    else
        stop(gettextf("prior for intercept but '%s' is not %s",
                      "tau", "NA"))
    ans <- methods::new("Scale", ans)
}

## NO_TESTS
makeTauExchFixedNonIntercept <- function(tau, sY, mult) {
    if (is.na(tau)) {
        if (is.null(sY))
            ans <- 1
        else
            ans <- sY
        ans <- mult * ans
    }
    else
        ans <- tau
    ans <- methods::new("Scale", ans)
}

## NO_TESTS
makeU <- function(nu, A, n) {
    ans <- double(length = n)
    for (i in seq_len(n))
        ans[i] <- rinvchisq1(df = nu, scale = A^2)
    methods::new("VarTDist", ans)
}

## NO_TESTS
makeMNoTrend <- function(K, m0 = NULL) {
    ans <- replicate(n = K + 1L,
                     0.0,
                     simplify = FALSE)
    if (!is.null(m0))
        ans[[1L]] <- m0
    methods::new("FFBSList", ans)
}

## NO_TESTS
makeM0NoTrend <- function(L) {
    ans <- replicate(n = L,
                     0.0,
                     simplify = FALSE)
    methods::new("FFBSList", ans)
}

## NO_TESTS
makeMSeason <- function(K, nSeason, m0 = NULL) {
    ans <- replicate(n = K + 1L,
                     rep(0, times = nSeason),
                     simplify = FALSE)
    if (!is.null(m0))
        ans[[1L]] <- m0
    methods::new("FFBSList", ans)
}

## NO_TESTS
makeM0Season <- function(L, nSeason) {
    ans <- replicate(n = L,
                     rep(0, times = nSeason),
                     simplify = FALSE)
    methods::new("FFBSList", ans)
}

## NO_TESTS
makeMWithTrend <- function(K, m0 = NULL) {
    ans <- replicate(n = K + 1L,
                     c(0, 0),
                     simplify = FALSE)
    if (!is.null(m0))
        ans[[1L]] <- m0
    methods::new("FFBSList", ans)
}

## NO_TESTS
makeM0WithTrend <- function(L, meanDelta0 = NULL) {
    if (is.null(meanDelta0))
        meanDelta0 <- 0
    else
        meanDelta0 <- meanDelta0@.Data
    ans <- replicate(n = L,
                     c(0, meanDelta0),
                     simplify = FALSE)
    methods::new("FFBSList", ans)
}

## NO_TESTS
makeCNoTrend <- function(K, C0 = NULL, sY) {
    ans <- replicate(n = K + 1L,
                     1.0,
                     simplify = FALSE)
    if (is.null(C0)) {
        A0 <- makeAIntercept(A = NA, sY = sY)
        A0 <- as.double(A0)
        C0 <- A0^2
    }
    ans[[1L]] <- C0
    methods::new("FFBSList", ans)
}

## NO_TESTS
## elements of C are vectors, not matrices
makeCSeason <- function(K, nSeason, ASeason, C0 = NULL) {
    if (is.null(C0)) {
        A <- ASeason@.Data
        C0 <- rep(A^2, times = nSeason)
    }
    ans <- replicate(n = K + 1L,
                     C0,
                     simplify = FALSE)
    methods::new("FFBSList", ans)
}

## NO_TESTS
makeCWithTrend <- function(K, C0 = NULL, sY, ADelta0) {
    if (is.null(C0)) {
        AAlpha <- makeAIntercept(A = NA, sY = sY)
        ADelta <- ADelta0@.Data
        C0 <- c(AAlpha^2, ADelta^2)
        C0 <- diag(C0,
                   nrow = 2L,
                   ncol = 2L)
    }
    head <- list(C0)
    tail <- replicate(n = K,
                      diag(2L),
                      simplify = FALSE)
    ans <- c(head, tail)
    methods::new("FFBSList", ans)
}

## NO_TESTS
makeANoTrend <- function(K) {
    ans <- replicate(n = K,
                     0.0,
                     simplify = FALSE)
    methods::new("FFBSList", ans)
}

## NO_TESTS
makeASeason <- function(K, nSeason) {
    ans <- replicate(n = K,
                     rep(0, times = nSeason),
                     simplify = FALSE)
    methods::new("FFBSList", ans)
}

## NO_TESTS
makeAWithTrend <- function(K) {
    ans <- replicate(n = K,
                     c(0, 0),
                     simplify = FALSE)
    methods::new("FFBSList", ans)
}

## NO_TESTS
makeDC <- function(CWithTrend) {
    K.plus.1 <- length(CWithTrend)
    ans <- replicate(n = K.plus.1,
                     diag(2L),
                     simplify = FALSE)
    ans[[1L]] <- sqrt(CWithTrend[[1L]])
    methods::new("FFBSList", ans)
}

## NO_TESTS
makeDCInv <- function(DC) {
    for (i in seq_along(DC))
        diag(DC[[i]]) <- 1 / diag(DC[[i]])
    DC
}

## NO_TESTS
makeDRInv <- function(K) {
    ans <- replicate(n = K,
                     diag(2L),
                     simplify = FALSE)
    methods::new("FFBSList", ans)
}

## NO_TESTS
makeRNoTrend <- function(K) {
    ans <- replicate(n = K,
                     1.0,
                     simplify = FALSE)
    methods::new("FFBSList", ans)
}

## NO_TESTS
makeRSeason <- function(K, nSeason) {
    ans <- replicate(n = K,
                     rep(1.0, times = nSeason),
                     simplify = FALSE)
    methods::new("FFBSList", ans)
}

## NO_TESTS
makeRWithTrend <- function(K) {
    ans <- replicate(n = K,
                     diag(2L),
                     simplify = FALSE)
    methods::new("FFBSList", ans)
}

## NO_TESTS
makeK <- function(dim, iAlong) {
    ans <- dim[iAlong]
    methods::new("Length", ans)
}

## NO_TESTS
makeL <- function(dim, iAlong) {
    ans <- prod(dim[-iAlong])
    ans <- as.integer(ans)
    methods::new("Length", ans)
}

## NO_TESTS
makeSeasonDLM <- function(K, L, nSeason) {
    n <- (K + 1L) * L
    ans <- replicate(n = n,
                     stats::rnorm(n = nSeason),
                     simplify = FALSE)
    methods::new("FFBSList", ans)
}

## NO_TESTS
makeStateDLM <- function(K, L) {
    n <- (K + 1L) * L
    ans <- stats::rnorm(n = n)
    methods::new("ParameterVector", ans)
}

## NO_TESTS
makePhi <- function(phi, phiKnown, minPhi, maxPhi) {
    if (phiKnown)
        phi
    else
        stats::runif(n = 1L, min = minPhi, max = maxPhi)
}

## NO_TESTS
makeGWithTrend <- function(phi) {
    ans <- matrix(c(1, 0, 1, phi), nrow = 2, ncol = 2)
    methods::new("NumericMatrixSquare", ans)
}

## NO_TESTS
makeWSqrt <- function(omegaAlpha, omegaDelta) {
    ans <- matrix(c(omegaAlpha, 0, 0, omegaDelta), nrow = 2L, ncol = 2L)
    methods::new("NumericMatrixSquare", ans)
}

## NO_TESTS
makeWSqrtInvG <- function(omegaAlpha, omegaDelta, phi) {
    ans <- matrix(0, nrow = 2L, ncol = 2L)
    ans[1L] <- 1 / omegaAlpha
    ans[3L] <- 1 / omegaAlpha
    ans[4L] <- phi / omegaDelta
    methods::new("NumericMatrixSquare", ans)
}

## NO_TESTS
makeUC <- function(K) {
    ans <- replicate(n = K + 1L,
                     diag(2L),
                     simplify = FALSE)
    methods::new("FFBSList", ans)
}

## NO_TESTS
makeUR <- function(K) {
    ans <- replicate(n = K,
                     diag(2L),
                     simplify = FALSE)
    methods::new("FFBSList", ans)
}

makeZ <- function(formula, data, metadata, contrastsArg) {
    namePrior <- paste(names(metadata), collapse = ":")
    ## make required response
    dimnames <- dimnames(metadata)
    response.required <- expand.grid(dimnames)
    response.required <- do.call(paste, response.required)
    ## make response from 'data'
    i.response <- match(names(metadata), names(data), nomatch = 0L)
    unmatched <- i.response == 0L
    if (any(unmatched))
        stop(gettextf("could not find variable '%s' in covariate data for prior '%s'",
                      names(metadata)[unmatched][1L], namePrior))
    response.obtained <- data[i.response]
    response.obtained <- do.call(paste, response.obtained)
    ## get indices for rows of 'data'
    i.row <- match(response.required, response.obtained, nomatch = 0L)
    unmatched <- i.row == 0L
    if (any(unmatched)) {
        first.unmatched <- response.required[unmatched][1L]
        stop(gettextf("no covariate data for element '%s' in prior for '%s'",
                      first.unmatched, namePrior))
    }
    ## make 'inputs' - variables that covariates formed from
    input.names <- rownames(attr(stats::terms(formula), "factors"))
    inputs <- data[input.names]
    inputs <- inputs[i.row, , drop = FALSE]
    ## make Z
    makeStandardizedVariables(formula = formula,
                              inputs = inputs,
                              namePrior = namePrior,
                              contrastsArg = contrastsArg)
}


## INITIAL VALUES - MODELS ##################################################################

## HAS_TESTS
DimScaleIsRegular <- function(x) {
    if (!methods::is(x, "Points") && !methods::is(x, "Intervals"))
        stop(gettextf("'%s' has class \"%s\"",
                      "x", class(x)))
    dimvalues <- x@dimvalues
    dimvalues <- dimvalues[is.finite(dimvalues)]
    n <- length(dimvalues)
    if (n < 3L)
        TRUE
    else {
        steps <- diff(dimvalues)
        all(steps[1L] == steps[-1L])
    }
}

## HAS_TESTS
addAgCertain <- function(object, aggregate, defaultWeights) {
    theta <- object@theta
    metadata.y <- object@metadataY
    value <- aggregate@valueAg
    weight <- aggregate@weightAg
    metadata.ag <- aggregate@metadataAg
    concordances.ag <- aggregate@concordancesAg
    .Data.theta.obj <- array(0,
                             dim = dim(metadata.y),
                             dimnames = dimnames(metadata.y))
    theta.obj <- methods::new("Counts",
                              .Data = .Data.theta.obj,
                              metadata = metadata.y)
    if (is.null(metadata.ag))
        ag.obj <- 0
    else {
        .Data.ag.obj <- array(0,
                              dim = dim(metadata.ag),
                              dimnames = dimnames(metadata.ag))
        ag.obj <- methods::new("Counts",
                               .Data = .Data.ag.obj,
                               metadata = metadata.ag)
    }
    transform <- dembase::makeTransform(x = theta.obj,
                                        y = ag.obj,
                                        subset = TRUE,
                                        concordances = concordances.ag)
    transform <- dembase::makeCollapseTransformExtra(transform)
    weight <- makeWeightAg(weight = weight,
                           default = defaultWeights,
                           model = object,
                           thetaObj = theta.obj,
                           transform = transform,
                           values = value)
    value <- as.double(value)
    mu <- rep(0, times = length(theta))
    for (k in seq_along(value)) {
        i.th <- dembase::getIBefore(k, transform = transform, useC = TRUE)
        sum.wt <- sum(weight[i.th])
        if (sum.wt > 0)
            theta[i.th] <- value[k] / sum.wt
    }
    value <- methods::new("ParameterVector", value)
    class <- paste0(class(object), "AgCertain")
    slotsToExtract <- methods::new(class)@slotsToExtract
    iMethodModel <- methods::new(class)@iMethodModel
    list(theta = theta,
         value = value,
         weight = weight,
         transform = transform,
         metadata = metadata.ag,
         mu = mu,
         slotsToExtract = slotsToExtract,
         iMethodModel = iMethodModel)
}

## HAS_TESTS
addAgNormal <- function(object, aggregate, defaultWeights) {
    theta <- object@theta
    metadata.y <- object@metadataY
    mean <- aggregate@valueAg # 'value' from Spec becomes 'mean' in object
    scale <- aggregate@scaleAg
    sd <- aggregate@sdAg
    weight <- aggregate@weightAg
    metadata.ag <- aggregate@metadataAg
    concordances.ag <- aggregate@concordancesAg
    .Data.theta.obj <- array(0,
                             dim = dim(metadata.y),
                             dimnames = dimnames(metadata.y))
    theta.obj <- methods::new("Counts", .Data = .Data.theta.obj, metadata = metadata.y)
    if (is.null(metadata.ag))
        ag.obj <- 0
    else {
        .Data.ag.obj <- array(0,
                              dim = dim(metadata.ag),
                              dimnames = dimnames(metadata.ag))
        ag.obj <- methods::new("Counts", .Data = .Data.ag.obj, metadata = metadata.ag)
    }
    transform <- dembase::makeTransform(x = theta.obj,
                                        y = ag.obj,
                                        subset = TRUE,
                                        concordances = concordances.ag)
    transform <- dembase::makeCollapseTransformExtra(transform)
    weight <- makeWeightAg(weight = weight,
                           default = defaultWeights,
                           model = object,
                           thetaObj = theta.obj,
                           transform = transform,
                           values = mean)
    value <- array(weight * theta, dim = dim(metadata.y))
    value <- dembase::collapse(value, transform = transform)
    value <- as.double(value)
    value <- methods::new("ParameterVector", value)
    mean <- as.double(mean)
    mean <- methods::new("ParameterVector", mean)
    sd <- as.double(sd)
    sd <- methods::new("ScaleVec", sd)
    mu <- rep(0, times = length(theta))
    class <- paste0(class(object), "AgNormal")
    slotsToExtract <- methods::new(class)@slotsToExtract
    iMethodModel <- methods::new(class)@iMethodModel
    list(value = value,
         mean = mean,
         scale = scale,
         sd = sd,
         weight = weight,
         transform = transform,
         metadata = metadata.ag,
         mu = mu,
         slotsToExtract = slotsToExtract,
         iMethodModel = iMethodModel)
}

## HAS_TESTS
addAgPoisson <- function(object, aggregate, defaultWeights) {
    theta <- object@theta
    metadata.y <- object@metadataY
    mean <- aggregate@valueAg # 'value' from Spec becomes 'mean' in object
    scale <- aggregate@scaleAg
    metadata.ag <- aggregate@metadataAg
    concordances.ag <- aggregate@concordancesAg
    .Data.theta.obj <- array(0,
                             dim = dim(metadata.y),
                             dimnames = dimnames(metadata.y))
    theta.obj <- methods::new("Counts", .Data = .Data.theta.obj, metadata = metadata.y)
    if (is.null(metadata.ag))
        ag.obj <- 0
    else {
        .Data.ag.obj <- array(0,
                              dim = dim(metadata.ag),
                              dimnames = dimnames(metadata.ag))
        ag.obj <- methods::new("Counts", .Data = .Data.ag.obj, metadata = metadata.ag)
    }
    transform <- dembase::makeTransform(x = theta.obj,
                                        y = ag.obj,
                                        subset = TRUE,
                                        concordances = concordances.ag)
    transform <- dembase::makeCollapseTransformExtra(transform)
    weight <- makeWeightAg(weight = NULL,
                           default = defaultWeights,
                           model = object,
                           thetaObj = theta.obj,
                           transform = transform,
                           values = mean)
    value <- array(weight * theta, dim = dim(metadata.y))
    value <- dembase::collapse(value, transform = transform)
    value <- as.double(value)
    value <- methods::new("ParameterVector", value)
    exposure <- dembase::collapse(defaultWeights@.Data, transform = transform)
    exposure <- as.double(exposure)
    if (!all(exposure > 0))
        stop(gettext("exposure term in Poisson aggregate model contains non-positive values"))
    exposure <- methods::new("ScaleVec", exposure)
    mean <- as.double(mean)
    mean <- methods::new("ParameterVector", mean)
    mu <- rep(0, times = length(theta))
    class <- paste0(class(object), "AgPoisson")
    slotsToExtract <- methods::new(class)@slotsToExtract
    iMethodModel <- methods::new(class)@iMethodModel
    list(value = value,
         mean = mean,
         scale = scale,
         weight = weight,
         exposure = exposure,
         transform = transform,
         metadata = metadata.ag,
         mu = mu,
         slotsToExtract = slotsToExtract,
         iMethodModel = iMethodModel)
}
        
## HAS_TESTS
addAgFun <- function(object, aggregate, defaultWeights) {
    theta <- object@theta
    metadata.y <- object@metadataY
    mean <- aggregate@valueAg # 'value' from Spec becomes 'mean' in object
    sd <- aggregate@sdAg
    weight <- aggregate@weightAg
    metadata.ag <- aggregate@metadataAg
    fun.ag <- aggregate@funAg
    concordances.ag <- aggregate@concordancesAg
    .Data.theta.obj <- array(0,
                             dim = dim(metadata.y),
                             dimnames = dimnames(metadata.y))
    theta.obj <- methods::new("Counts",
                              .Data = .Data.theta.obj,
                              metadata = metadata.y)
    if (is.null(metadata.ag))
        ag.obj <- 0
    else {
        .Data.ag.obj <- array(0,
                              dim = dim(metadata.ag),
                              dimnames = dimnames(metadata.ag))
        ag.obj <- methods::new("Counts",
                               .Data = .Data.ag.obj,
                               metadata = metadata.ag)
    }
    transform <- dembase::makeTransform(x = theta.obj,
                                        y = ag.obj,
                                        subset = TRUE,
                                        concordances = concordances.ag)
    transform <- dembase::makeCollapseTransformExtra(transform)
    weight <- makeWeightAg(weight = weight,
                           default = defaultWeights,
                           model = object,
                           thetaObj = theta.obj,
                           transform = transform,
                           values = mean)
    metadata.args <- dembase::makeMetaDataSubarraysBefore(metadata = metadata.y,
                                                          transform = transform)
    n.value <- length(mean)
    x.args <- vector(mode = "list", length = n.value)
    weights.args <- vector(mode = "list", length = n.value)
    value <- double(length = n.value)
    for (i in seq_len(n.value)) {
        i.y <- getIBefore(i = i,
                          transform = transform,
                          useC = TRUE)
        metadata.i <- metadata.args[[i]]
        .Data.x <- theta[i.y]
        .Data.weights <- weight[i.y]
        .Data.x <- array(.Data.x,
                         dim = dim(metadata.i),
                         dimnames = dimnames(metadata.i))
        .Data.weights <- array(.Data.weights,
                               dim = dim(metadata.i),
                               dimnames = dimnames(metadata.i))
        x.args[[i]] <- methods::new("Values",
                                    .Data = .Data.x,
                                    metadata = metadata.i)
        weights.args[[i]] <- methods::new("Counts",
                                          .Data = .Data.weights,
                                          metadata = metadata.i)
        val <- tryCatch(fun.ag(x = x.args[[i]], 
                               weights = weights.args[[i]]),
                        error = function(e) e)
        if (methods::is(val, "error"))
            stop(gettextf("error applying '%s' : %s",
                          "FUN", val$message))
        if (!is.numeric(val))
            stop(gettextf("return value from '%s' has class \"%s\"",
                          "FUN", class(val)))
        if (!identical(length(val), 1L))
            stop(gettextf("return value from '%s' has length %d",
                          "FUN", length(val)))
        value[i] <- val
    }
    value <- methods::new("ParameterVector", value)
    mean <- as.double(mean)
    mean <- methods::new("ParameterVector", mean)
    sd <- as.double(sd)
    sd <- methods::new("ScaleVec", sd)
    class <- paste0(class(object), "AgFun")
    slotsToExtract <- methods::new(class)@slotsToExtract
    iMethodModel <- methods::new(class)@iMethodModel
    list(value = value,
         mean = mean,
         sd = sd,
         metadata = metadata.ag,
         transform = transform,
         funAg = fun.ag,
         xArgs = x.args,
         weightsArgs = weights.args,
         slotsToExtract = slotsToExtract,
         iMethodModel = iMethodModel)
}

## HAS_TESTS
addAgLife <- function(object, aggregate, defaultWeights) {
    theta <- object@theta
    metadata.y <- object@metadataY
    mean <- aggregate@valueAg # 'value' from Spec becomes 'mean' in object
    sd <- aggregate@sdAg
    ax <- aggregate@axAg
    metadata.ag <- aggregate@metadataAg
    concordances.ag <- aggregate@concordancesAg
    names.y <- names(metadata.y)
    dimtypes.y <- dimtypes(metadata.y, use.names = FALSE)
    DimScales.y <- DimScales(metadata.y, use.names = FALSE)
    i.age.y <- match("age", dimtypes.y, nomatch = 0L)
    has.age.y <- i.age.y > 0L
    if (!has.age.y)
        stop(gettextf("'%s' does not have a dimension with %s \"%s\"",
                      "y", "dimtype", "age"))
    name.age <- names.y[i.age.y]
    DimScale.age <- DimScales.y[[i.age.y]]
    .Data.theta.obj <- array(0,
                             dim = dim(metadata.y),
                             dimnames = dimnames(metadata.y))
    theta.obj <- methods::new("Counts",
                              .Data = .Data.theta.obj,
                              metadata = metadata.y)
    if (is.null(metadata.ag)) {
        names.ag <- NULL
        dimtypes.ag <- NULL
        DimScales.ag <- NULL
    }
    else {
        names.ag <- names(metadata.ag)
        dimtypes.ag <- dimtypes(metadata.ag, use.names = FALSE)
        DimScales.ag <- DimScales(metadata.ag, use.names = FALSE)
    }    
    names.mx <- c(name.age, names.ag)
    dimtypes.mx <- c("age", dimtypes.ag)
    DimScales.mx <- c(list(DimScale.age), DimScales.ag)
    metadata.mx <- new("MetaData",
                       nms = names.mx,
                       dimtypes = dimtypes.mx,
                       DimScales = DimScales.mx)
    .Data.mx.obj <- array(0,
                          dim = dim(metadata.mx),
                          dimnames = dimnames(metadata.mx))
    mx.obj <- new("Values",
                  .Data = .Data.mx.obj,
                  metadata = metadata.mx)
    transform <- tryCatch(dembase::makeTransform(x = theta.obj,
                                                 y = mx.obj,
                                                 subset = TRUE,
                                                 concordances = concordances.ag,
                                                 check = TRUE),
                          error = function(e) e)
    if (methods::is(transform, "error"))
        stop(gettextf("'%s' not compatible with '%s' from '%s' : %s",
                      "y", "value", "AgLife", transform$message))
    numerator.mx <- theta * defaultWeights
    numerator.mx <- numerator.mx@.Data
    denominator.mx <- defaultWeights@.Data
    numerator.mx <- collapse(numerator.mx,
                             transform = transform)
    denominator.mx <- collapse(denominator.mx,
                               transform = transform)
    mx <- numerator.mx / denominator.mx
    mx <- array(mx,
                dim = dim(metadata.mx),
                dimnames = dimnames(metadata.mx))
    mx <- new("Values",
              .Data = mx,
              metadata = metadata.mx)
    if (is.null(ax))
        ax <- makeAxStart(mx) # mx must have metadata
    ax <- expandAx(ax = ax,
                   object = mx) # mx must have metadata
    dv.age <- DimScale.age@dimvalues
    nx <- diff(dv.age)
    mx <- as.double(mx)
    ax <- as.double(ax)
    transform <- dembase::makeCollapseTransformExtra(transform)
    value <- double(length = length(mean))
    nAge <- length(nx)
    seq.iAge0 <- seq.int(from = 1L,
                         by = nAge,
                         to = length(mx))
    for (i in seq_along(seq.iAge0)) {
        iAge0 <- seq.iAge0[i]
        value[i] <- makeLifeExpBirth(mx = mx,
                                     nx = nx,
                                     ax = ax,
                                     iAge0 = iAge0,
                                     nAge= nAge)
    }
    value <- methods::new("ParameterVector", value)
    mean <- as.double(mean)
    mean <- methods::new("ParameterVector", mean)
    sd <- as.double(sd)
    sd <- methods::new("ScaleVec", sd)
    nAge <- new("Length", nAge)
    class <- paste0(class(object), "AgLife")
    slotsToExtract <- methods::new(class)@slotsToExtract
    iMethodModel <- methods::new(class)@iMethodModel
    list(value = value,
         mean = mean,
         sd = sd,
         metadataAg = metadata.ag,
         transform = transform,
         metadataMx = metadata.mx,
         mx = mx,
         ax = ax,
         nx = nx,
         nAge = nAge,
         slotsToExtract = slotsToExtract,
         iMethodModel = iMethodModel)
}

## HAS_TESTS
alignDatasetsToObservation <- function(datasets, observation) {
    ans <- vector(mode = "list", length = length(observation))
    names.datasets <- names(datasets)
    for (i in seq_along(observation)) {
        obs <- observation[[i]]
        name.y <- obs@nameY
        i.dataset <- match(name.y, names.datasets, nomatch = 0L)
        has.dataset <- i.dataset > 0L
        if (has.dataset)
            ans[[i]] <- datasets[[i.dataset]]
        else
            stop(gettextf("'%s' has a model for '%s' but '%s' does not have a dataset called '%s'",
                          "observation", name.y, "datasets", name.y))
    }
    names(ans) <- sapply(observation, function(x) methods::slot(x, "nameY"))
    ans
}

## HAS_TESTS
checkAndTidyDatasets <- function(datasets) {
    names.datasets <- names(datasets)
    if (!is.list(datasets))
        stop(gettextf("'%s' has class \"%s\"",
                      "datasets", class(datasets)))
    if (identical(length(datasets), 0L))
        stop(gettextf("'%s' has length %d",
                      "datasets", 0L))
    ans <- vector(mode = "list", length = length(datasets))
    for (i in seq_along(ans)) {
        dataset <- datasets[[i]]
        if (!methods::is(dataset, "Counts"))
            stop(gettextf("dataset '%s' has class \"%s\"",
                          names.datasets[i], class(dataset)))
        if (any(round(dataset) != dataset, na.rm = TRUE))
            stop(gettextf("dataset '%s' has non-integer values",
                          names.datasets[i]))
        if (any(dataset < 0, na.rm = TRUE))
            stop(gettextf("dataset '%s' has negative values",
                          names.datasets[i]))
        dataset <- dembase::toInteger(dataset)
        ans[[i]] <- dataset
    }
    names(ans) <- names.datasets
    ans
}

## HAS_TESTS
checkAndTidyExposure <- function(exposure, y) {
    if (is.null(exposure))
        return(NULL)
    if (!methods::is(exposure, "Counts"))
        stop(gettextf("'%s' has class \"%s\"",
                      "exposure", class(exposure)))
    exposure <- dembase::makeCompatible(x = exposure, y = y, subset = TRUE)
    if (any(exposure[!is.na(exposure)] < 0))
        stop(gettextf("'%s' has negative values",
                      "exposure"))
    if (all(exposure[!is.na(exposure)] == 0))
        stop(gettextf("'%s' has no non-zero values",
                      "exposure"))
    if (any(is.na(exposure) > is.na(y)))
        stop(gettextf("'%s' has missing values in places where '%s' does not",
                      "exposure", "y"))
    exposure
}

## HAS_TESTS
checkAndTidySDAg <- function(sd, value, metadata) {
    length.sd <- length(sd)
    length.value <- length(value)
    if (methods::is(sd, "DemographicArray")) {
        ## 'sd' and 'value' have same length
        if (!identical(length.sd, length.value))
            stop(gettextf("'%s' and '%s' have different lengths",
                          "sd", "value"))
        ## 'sd' has same metadata as 'value'
        metadata.sd <- sd@metadata
        if (!isTRUE(all.equal(metadata.sd, metadata)))
            stop(gettextf("'%s' and '%s' have different metadata",
                          "sd", "value"))
        ## 'sd' has no missing values"
        if (any(is.na(sd)))
            stop(gettextf("'%s' has missing values",
                          "sd"))
        ## 'sd' has no negative values
        if (any(sd < 0))
            stop(gettextf("'%s' has negative values",
                          "sd"))
        sd <- as.double(sd@.Data)
    }
    else if (methods::is(sd, "numeric")) {
        ## 'sd' has length 1
        if (!identical(length.sd, 1L))
            stop(gettextf("'%s' is numeric but does not have length %d",
                          "sd", 1L))
        ## 'sd' is not missing
        if (is.na(sd))
            stop(gettextf("'%s' is missing",
                          "sd"))
        ## 'sd' is non-negative
        if (sd < 0)
            stop(gettextf("'%s' is negative",
                          "sd"))
        sd <- as.double(sd)
        sd <- rep(sd, times = length.value)
    }
    else {
        stop(gettextf("'%s' has class \"%s\"",
                      "sd", class(sd)))
    }
    methods::new("ScaleVec", sd)
}

## HAS_TESTS
checkAndTidyWeights <- function(weights, y) {
    if (is.null(weights))
        return(NULL)
    if (!methods::is(weights, "Counts"))
        stop(gettextf("'%s' has class \"%s\"",
                      "weights", class(weights)))
    weights <- dembase::makeCompatible(x = weights, y = y, subset = TRUE)
    weights <- dembase::toDouble(weights)
    if (any(weights[!is.na(weights)] < 0))
        stop(gettextf("'%s' has negative values",
                      "weights"))
    if (any(is.na(weights) > is.na(y)))
        stop(gettextf("'%s' has missing values in places where '%s' does not",
                      "weights", "y"))
    weights
}

## HAS_TESTS
## Used with Normal, so does not check for Counts, negatives, or non-integer
checkAndTidyY <- function(y, impute = FALSE) {
    ## 'y' is DemographicArray
    if (!methods::is(y, "DemographicArray"))
        stop(gettextf("'%s' has class \"%s\"",
                      "y", class(y)))
    ## 'y' has no zero-length dimensions
    is.zero <- dim(y) == 0L
    if (any(is.zero))
        stop(gettextf("dimension \"%s\" of '%s' has length %d",
                      names(y)[is.zero][1L], "y", dim(y)[is.zero][1L]))
    ## 'y' does not have iteration dimension
    is.iter <- dembase::dimtypes(y) == "iteration"
    if (any(is.iter))
        stop(gettextf("dimension \"%s\" of '%s' has dimtype \"%s\"",
                      names(y)[is.iter], "y", "iteration"))
    ## 'y' does not have quantile dimension
    is.quantile <- dembase::dimtypes(y) == "quantile"
    if (any(is.quantile))
        stop(gettextf("dimension \"%s\" of '%s' has dimtype \"%s\"",
                      names(y)[is.quantile], "y", "quantile"))
    ## 'y' has at least 2 non-missing values
    if (sum(!is.na(y)) < 2L)
        stop(gettextf("'%s' has fewer than %d non-missing values",
                      "y", 2L))
    ## return 'y'
    y
}

## HAS_TESTS
checkAxAg <- function(ax, value) {
    if (is.null(ax))
        return(ax)
    if (length(ax) == 0L)
        stop(gettextf("'%s' has length %d",
                      "ax", 0L))
    if (!methods::is(ax, "Values"))
        stop(gettextf("'%s' has class \"%s\"",
                      "ax", class(ax)))
    names <- names(ax)
    dim <- dim(ax)
    dimtypes <- dimtypes(ax, use.names = FALSE)
    DimScales <- DimScales(ax, use.names = FALSE)
    i.age <- match("age", dimtypes, nomatch = 0L)
    has.age <- i.age > 0L
    if (!has.age)
        stop(gettextf("'%s' does not have a dimension with %s \"%s\"",
                      "ax", "dimtype", "age"))
    DimScale.age <- DimScales[[i.age]]
    if (!methods::is(DimScale.age, "Intervals"))
        stop(gettextf("dimension of '%s' with %s \"%s\" does not have %s \"%s\"",
                      "ax", "dimtype", "age", "dimscale", "Intervals"))
    n.dim <- length(dim)
    if (n.dim > 1L) {
        if (!methods::is(value, "DemographicArray"))
            stop(gettextf("'%s' is not a demographic array, but '%s' has more than one dimension",
                          "value", "ax"))
        metadata.ax.no.age <- new("MetaData",
                                  nms = names[-i.age],
                                  dimtypes = dimtypes[-i.age],
                                  DimScales = DimScales[-i.age])
        .Data.ax.no.age <- array(0,
                                 dim = dim(metadata.ax.no.age),
                                 dimnames = dimnames(metadata.ax.no.age))
        ax.no.age <- methods::new("Values",
                                  .Data = .Data.ax.no.age,
                                  metadata = metadata.ax.no.age)
        return.value <- tryCatch(dembase::canMakeCompatible(x = ax.no.age,
                                                            y = value,
                                                            subset = TRUE),
                                 error = function(e) e)
        if (methods::is(return.value, "error"))
            stop(gettextf("'%s' and '%s' not compatible : %s",
                          "ax", "value", return.value$message))
    }
    NULL
}

## HAS_TESTS
checkConcordances <- function(concordances) {
    ## 'concordances' is a list
    if (!is.list(concordances))
        stop(gettextf("'%s' has class \"%s\"",
                      "concordances", class(concordances)))
    if (identical(length(concordances), 0L))
        return(NULL)
    names <- names(concordances)
    ## all elements of 'concordances' have class "ManyToOne"
    if (!all(sapply(concordances, methods::is, "ManyToOne")))
        return(gettextf("'%s' has elements not of class \"%s\"",
                        "concordances"))
    ## 'concordances' has names
    if (is.null(names))
        stop(gettextf("'%s' does not have names",
                      "concordances"))
    ## no duplicated names for 'concordances'
    if (any(duplicated(names)))
        stop(gettextf("'%s' has duplicate names",
                      "concordances"))
    NULL
}

## HAS_TESTS
checkForMarginalTerms <- function(formula) {
    terms <- stats::terms(formula)
    intercept <- attr(terms, "intercept")
    if (!identical(intercept, 1L))
        stop(gettextf("formula '%s' does not include an intercept",
                      deparse(formula)))
    factor <- attr(terms, "factor")
    if (length(factor) > 0L) {
        col.has.2 <- apply(factor, 2, function(x) any(x == 2L)) ## at most one TRUE
        if (any(col.has.2)) {
            term.without.marginal <- colnames(factor)[col.has.2]
            is.1 <- factor[ , col.has.2] == 1L
            missing.marginal <- rownames(factor)[is.1]
            missing.marginal <- paste(missing.marginal, collapse = ":")
            stop(gettextf("term '%s' is marginal to term '%s' but is not included in formula '%s'",
                          missing.marginal, term.without.marginal, deparse(formula)))
        }
    }
    else
        NULL
}

## HAS_TESTS
checkFormulaMu <- function(formula) {
    correct.length <- identical(length(formula), 3L)
    if (!correct.length)
        stop(gettextf("'%s' is not a valid formula",
                      deparse(formula)))
    if (!identical(deparse(formula[[2L]]), "mean"))
        stop(gettextf("formula '%s' does not have response '%s'",
                      deparse(formula), "mean"))
    NULL
}

## HAS_TESTS
checkFilename <- function(filename, name = "filename") {
    ## filename is character
    if (!is.character(filename))
        stop(gettextf("'%s' does not have type \"%s\"",
                      name, "character"))
    ## 'filename' has length 1
    if (!identical(length(filename), 1L))
        stop(gettextf("'%s' does not have length %d",
                      name, 1L))
    ## 'filename' is not missing
    if (is.na(filename))
        stop(gettextf("'%s' is missing",
                      name))
    NULL
}

## HAS_TESTS
checkFunAg <- function(FUN) {
    if (!is.function(FUN))
        stop(gettextf("'%s' has class \"%s\"",
                      "FUN", class(FUN)))
    formals.obtained <- formals(FUN)
    formals.expected <- formals(function(x, weights) NULL)
    if (!identical(formals.obtained, formals.expected))
        stop(gettextf("'%s' does not have formal arguments '%s' and '%s'",
                      "FUN", "x", "weights"))
    NULL
}

## HAS_TESTS
checkLengthDimInFormula <- function(y, formula, minLength = 2L) {
    terms.without.response <- stats::terms(formula[-2L])
    names.terms <- rownames(attr(terms.without.response, "factors"))
    lengths <- dim(y)[match(names.terms, names(y))]
    too.short <- lengths < minLength
    if (any(too.short))
        stop(gettextf("dimension \"%s\" is used in formula '%s' but has length %d",
                      names.terms[too.short][1L],
                      deparse(formula),
                      dim(y)[too.short][1L]))
    NULL
}

## HAS_TESTS
checkNamesDatasets <- function(datasets) {
    names.datasets <- names(datasets)
    if (is.null(names.datasets))
        stop(gettextf("'%s' does not have names",
                      "datasets"))
    if (any(is.na(names.datasets)))
        stop(gettextf("names for '%s' has missing values",
                      "datasets"))
    if (!all(nzchar(names.datasets)))
        stop(gettextf("names for '%s' has blanks",
                      "datasets"))
    if (any(duplicated(names.datasets)))
        stop(gettextf("names for '%s' has duplicates",
                      "datasets"))
    NULL
}

## HAS_TESTS
checkNumberElementsBetas <- function(betas, y) {
    n.elements.betas <- sum(sapply(betas, length))
    n.elements.y <- length(y)
    if (n.elements.betas >= n.elements.y)
        stop(gettextf("terms predicting cell means have a combined total of %d elements but data only has %d elements : try a model with fewer terms",
                      n.elements.betas, n.elements.y))
    NULL
}

## HAS_TESTS
checkObservation <- function(observation, needsNonDefaultSeriesArg = FALSE) {
    ## 'observation' is a list
    if (!is.list(observation))
        stop(gettextf("'%s' has class \"%s\"",
                      "observation", class(observation)))
    ## 'observation' has at least one element
    if (identical(length(observation), 0L))
        stop(gettextf("'%s' has length %d",
                      "observation", 0L))
    for (i in seq_along(observation)) {
        obs <- observation[[i]]
        ## all elements have class "SpecModel"
        if (!methods::is(obs, "SpecModel"))
            stop(gettextf("element %d of '%s' has class \"%s\"",
                          i, "observation", class(obs)))
        ## element has name
        if (is.na(obs@nameY@.Data) || !nzchar(obs@nameY@.Data))
            stop(gettextf("element %d of '%s' has no name for response variable",
                          i, "observation"))
        ## specification of model is valid
        return.value <- tryCatch(methods::validObject(obs),
                                 error = function(e) e)
        if (methods::is(return.value, "error"))
            stop(gettextf("error in observation model for '%s' : %s",
                          obs@nameY@.Data, return.value$message))
        if (needsNonDefaultSeriesArg) {
            ## 'series' argument supplied if needed
            if (identical(obs@series@.Data, "y"))
                stop(gettextf("'%s' argument not supplied in observation model for '%s'",
                              "series", obs@nameY))
        }
        else {
            ## no 'series' argument supplied if not needed
            if (!identical(obs@series@.Data, "y"))
                warning(gettextf("non-default argument for '%s' in observation model for '%s' ignored",
                                 "series", obs@nameY))
        }
    }
    NULL
}

## HAS_TESTS
checkSpecWeightAg <- function(weights, metadata) {
    if (is.null(weights))
        NULL
    else {
        if (!methods::is(weights, "Counts"))
            stop(gettextf("'%s' has class \"%s\"",
                          "weights", class(weights)))
        if (any(weights < 0, na.rm = TRUE))
            stop(gettextf("'%s' has negative values",
                          "weights"))
        if (!is.null(metadata)) {
            .Data <- array(1L,
                           dim = dim(metadata),
                           dimnames = dimnames(metadata))
            y <- methods::new("Values", .Data = .Data, metadata = metadata)
            return.value <- tryCatch(dembase::canMakeCompatible(x = weights,
                                                                y = y,
                                                                subset = TRUE),
                                     error = function(e) e)
            if (methods::is(return.value, "error"))
                stop(gettextf("'%s' and '%s' not compatible : %s",
                              "weights", "value", return.value$message))
        }
    }
    NULL
}

## HAS_TESTS
checkTermsFromFormulaFound <- function(y, formula) {
    terms.without.response <- stats::terms(formula[-2L])
    names.terms <- rownames(attr(terms.without.response, "factors"))
    not.in.y <- !(names.terms %in% names(y))
    n.not.in.y <- sum(not.in.y)
    if (n.not.in.y > 0L)
        stop(sprintf(ngettext(n.not.in.y,
                              "dimension %s from formula '%s' not found",
                              "dimensions %s from formula '%s' not found"),
                     paste(dQuote(names.terms[not.in.y]), collapse = ", "),
                     deparse(formula)))
    NULL
}

## HAS_TESTS
## Function 'loglm' arranges and names 'beta' according to order of
## dimensions used in data.  Normally functions using a formula
## arrange and name terms according to the order they appear in the formula.
## 'convertToFormulaOrder' converts from 'loglm' format to standard format.
convertToFormulaOrder <- function(betas, formulaMu) {
    n.beta <- length(betas)
    terms <- stats::terms(formulaMu)
    factors <- attr(terms, "factors")
    dims.in.formula <- rownames(factors)
    ## when a term is an array, permute the array and change the name
    ## so that dimensions occur in the same order that they do in 'formula'
    for (i in seq_along(betas)) {
        beta <- betas[[i]]
        if (is.array(beta)) {
            names.dims <- names(dimnames(beta))
            perm <- order(match(names.dims, dims.in.formula))
            beta <- aperm(beta, perm = perm)
            betas[[i]] <- beta
            names(betas)[i] <- paste(names(dimnames(beta)), collapse = ":")
        }
    }
    ## reorder terms so that they appear in right order
    term.labels <- attr(terms, "term.labels")
    term.labels <- c("(Intercept)", term.labels)
    betas <- betas[match(term.labels, names(betas))]
    betas
}




## need to have Cross default for orig-dest or parent-child
## HAS_TESTS
defaultPrior <- function(beta, metadata) {
    dim <- dim(metadata)
    names <- names(metadata)
    dimtypes <- dembase::dimtypes(metadata, use.names = FALSE)
    DimScales <- dembase::DimScales(metadata, use.names = FALSE)
    n.beta <- length(beta)
    name.term <- paste(names, collapse = ":")
    if (n.beta < 2L)
        stop(gettextf("'%s' for \"%s\" has length %d",
                      "beta", name.term, n.beta))
    if (n.beta != prod(dim))
        stop(gettextf("length of '%s' for \"%s\" [%d] not equal to product of dimensions [%d]",
                      "beta", name.term, n.beta, prod(dim)))
    is.main.effect <- identical(length(dim), 1L)
    i.time <- match("time", dimtypes, nomatch = 0L)
    i.age <- match("age", dimtypes, nomatch = 0L)
    has.time <- i.time > 0L
    has.age <- i.age > 0L
    n.beta <- length(beta)
    if (n.beta <= 2L) {
        ExchFixed()
    }
    else {
        if (is.main.effect) {
            if (has.time) {
                DLM()
            }
            else
                Exch()
        }
        else {
            if (has.time) {
                along <- names[i.time]
                DLM(along = along, trend = NULL)
            }
            else
                Exch()
        }
    }
}


## HAS_TESTS
formulaIsInterceptOnly <- function(formula) {
    terms <- stats::terms(formula)
    identical(attr(terms, "factors"), integer(0)) &&
        identical(attr(terms, "intercept"), 1L)
}

## HAS_TESTS
imputeCountsInternal <- function(object, max = NULL) {
    has.subtotals <- methods::is(object, "HasSubtotals")
    if (has.subtotals) {
        subtotals <- object@subtotals
        metadata.subtotals <- object@metadataSubtotals
        transform.subtotals <- object@transformSubtotals
    }
    if (any(is.na(object))) {
        object <- impute(object, max = max)
        if (has.subtotals)
            object <- methods::new("CountsWithSubtotalsInternal",
                                   .Data = object@.Data,
                                   metadata = object@metadata,
                                   subtotals = subtotals,
                                   metadataSubtotals = metadata.subtotals,
                                   transformSubtotals = transform.subtotals)
    }
    object
}

## HAS_TESTS
initialObservation <- function(observation, datasets, y, transforms) {
    ans <- vector(mode = "list", length = length(observation))
    for (i in seq_along(ans)) {
        ans[[i]] <- initialModel(object = observation[[i]],
                                 y = datasets[[i]],
                                 exposure = dembase::collapse(y, transform = transforms[[i]]))
    }
    ans
}

## HAS_TESTS
jitterBetas <- function(betas) {
    kIntercept <- 0.1
    kTerms <- 0.25
    betas[[1L]] <- stats::rnorm(n = 1L,
                         mean = betas[[1L]],
                         sd = kIntercept * abs(betas[[1L]]))
    if (length(betas) > 1L) {
        for (i in seq_along(betas)[-1L]) {
            betas[[i]] <- stats::rnorm(n = length(betas[[i]]),
                                mean = betas[[i]],
                                sd = kTerms * stats::sd(betas[[i]]))
        }
    }
    betas
}

## HAS_TESTS
makeDims <- function(dim, margins) {
    ans <- list(0L)
    if (length(margins) > 0L) {
        extra <- lapply(margins[-1L], function(x) dim[x])
        ans <- c(ans, extra)
    }
    ans
}

## HAS_TESTS
makeIAlong <- function(along, metadata) {
    kContinuousDimtypes <- c("age", "cohort", "time")
    continuous.labels <- paste(dQuote(kContinuousDimtypes), collapse = ", ")
    names <- names(metadata)
    dimtypes <- dembase::dimtypes(metadata, use.names = FALSE)
    DimScales <- dembase::DimScales(metadata, use.names = FALSE)
    is.continuous <- dimtypes %in% kContinuousDimtypes
    if (is.null(along)) {
        n.continuous <- sum(is.continuous)
        if (n.continuous == 0L)
            stop(gettextf("cannot use random walk prior when no dimensions have dimtype %s",
                          continuous.labels))
        if (n.continuous >= 2L)
            stop(gettextf("more than one dimension with dimtype %s, but '%s' not specified",
                          continuous.labels, "along"))
        along <- which(is.continuous)
    }
    else {
        along <- match(along, names, nomatch = 0L)
        if (along == 0L)
            stop(gettextf("'%s' outside valid range", "along"))
        dimtype.along <- dimtypes[along]
        if (!(dimtype.along %in% kContinuousDimtypes))
            stop(gettextf("cannot have random walk along dimension \"%s\" because dimension has dimtype \"%s\"",
                          names[along], dimtypes[along]))
    }
    DimScale.along <- DimScales[[along]]
    steps <- dembase::stepLengths(DimScale.along)
    steps <- steps[is.finite(steps)]
    if (length(steps) > 1L) {
        all.equal <- all(mapply(identical, steps[1L], steps[-1L]))
        if (!all.equal)
            stop(gettextf("cannot have random walk along dimension \"%s\" because steps irregular",
                          names[along]))
    }
    along
}

## HAS_TESTS
makeLinearBetas <- function(theta, formula) {
    checkForMarginalTerms(formula)
    data <- data.frame(expand.grid(dimnames(theta)), mean = as.numeric(theta))
    names(data)[length(data)] <- extractResponse(formula)
    mod <- stats::lm(formula, data = data)
    mod <- stats::aov(mod)
    if (mod$rank > 1L) {
        ans <- stats::model.tables(mod)$tables
        one.dim <- sapply(ans, function(x) identical(length(dim(x)), 1L))
        ans[one.dim] <- lapply(ans[one.dim], as.numeric)
        ans[!one.dim] <- lapply(ans[!one.dim], as.array)
    }
    else
        ans <- list()
    ans <- c(list("(Intercept)" = mean(theta)), ans)
    ans
}

## HAS_TESTS
makeMargins <- function(betas, y) {
    names.betas <- names(betas)
    n.beta <- length(betas)
    dimnames.y <- dimnames(y)
    names.y <- names(dimnames.y)
    ans <- vector(mode = "list", length = n.beta)
    ans[[1L]] <- 0L
    if (n.beta > 1L) {
        for (i in seq.int(from = 2L, to = n.beta)) {
            names.dims <- names(dimnames(betas[[i]]))
            if (is.null(names.dims))
                ans[[i]] <- match(names.betas[i], names.y)
            else
                ans[[i]] <- match(names.dims, names.y)
        }
    }
    ans
}

## HAS_TESTS
makeNamesEta <- function(spec) {
    formula <- spec@innerFormula
    terms <- stats::terms(formula)
    ans <- attr(terms, "term.labels")
    has.intercept <- identical(attr(terms, "intercept"), 1L)
    if (has.intercept)
        ans <- c("(Intercept)", ans)
    ans
}

## HAS_TESTS
makeNamesSpecsPriors <- function(dots) {
    if (length(dots) > 0L)
        sapply(dots, extractResponse)
    else
        character()
}

## HAS_TESTS
## priors follow order implied by formula - not order implied by metadata
makePriors <- function(betas, specs, namesSpecs, margins, y, sY) {
    n.beta <- length(betas)
    ans <- vector(mode = "list", length = n.beta)
    names.betas <- names(betas)
    dim.y <- dim(y)
    metadata.y <- y@metadata
    for (i in seq_len(n.beta)) {
        beta <- betas[[i]]
        beta <- as.double(beta)
        name <- names.betas[i]
        if (any(is.na(beta)))
            stop(gettextf("'%s' for \"%s\" has missing values",
                          "beta", name))
        margin <- margins[[i]]
        if (identical(margin, 0L))
            metadata <- NULL
        else
            metadata <- metadata.y[margin]
        is.intercept <- i == 1L
        if (is.intercept)
            spec <- ExchFixed()
        else {
            i.spec <- match(name, namesSpecs, nomatch = 0L)
            has.spec <- i.spec > 0L
            if (has.spec)
                spec <- specs[[i.spec]]
            else
                spec <- defaultPrior(beta = beta, metadata = metadata)
        }
        ans[[i]] <- initialPrior(object = spec,
                                 beta = beta,
                                 metadata = metadata,
                                 sY = sY)
    }
    ans
}

## HAS_TESTS
makeSigmaInitialPoisson <- function(y, exposure = NULL) {
    kMinimumValue <- 0.01
    y <- as.numeric(y)
    is.missing <- is.na(y)
    if (any(is.missing)) {
        ## 'y' has at least 1 non-missing value
        if (all(is.missing))
            stop(gettextf("'%s' has no non-missing values",
                          "y"))
        y <- y[!is.missing]
        exposure <- exposure[!is.missing]
        Recall(y = y, exposure = exposure)
    }
    ## 'y' is finite
    if (any(is.infinite(y)))
        stop(gettextf("'%s' has non-finite values",
                      "y"))
    ## 'y' not all 0
    if (all(y == 0))
        stop(gettextf("'%s' has no non-zero values",
                      "y"))
    if (is.null(exposure))
        theta <- y
    else {
        exposure <- as.numeric(exposure)
        ## 'exposure' has no missing values
        if (any(is.na(exposure)))
            stop(gettextf("'%s' has missing values",
                          "exposure"))
        ## 'exposure' is finite
        if (any(is.infinite(exposure)))
            stop(gettextf("'%s' has non-finite values",
                          "exposure"))
        ## 'exposure' not all 0
        if (all(exposure == 0))
            stop(gettextf("'%s' has no non-zero values",
                          "exposure"))
        non.zero <- exposure > 0
        theta <- y[non.zero] / exposure[non.zero]
    }
    if (length(theta) > 1L) {
        theta <- 0.5 * (theta + mean(theta))
        log.theta <- log(theta)
        ans <- stats::sd(log.theta)
        ans <- max(ans, kMinimumValue)
        ans
    }
    else
        1.0
}

## NO_TESTS
makeSpecsPriors <- function(dots) {
    ans <- vector(mode = "list", length = length(dots))
    for (i in seq_along(dots)) {
        formula <- dots[[i]]
        if (!methods::is(formula, "formula"))
            stop(gettextf("'%s' is not a formula",
                          deparse(substitute(formula))))
        if (!hasResponse(formula))
            stop(gettextf("formula '%s' does not include a response",
                          deparse(formula)))
        right.hand.side <- formula[[3L]]
        ans[[i]] <- eval(right.hand.side,
                         envir = environment(formula))
    }
    ans
}

## HAS_TESTS
makeTransformsYToDatasets <- function(y, nameY = "y", datasets) {
    names.datasets <- names(datasets)
    ans <- vector(mode = "list", length = length(datasets))
    for (i in seq_along(ans)) {
        dataset <- datasets[[i]]
        return.value <- tryCatch(dembase::canMakeCompatible(x = y, y = dataset, subset = TRUE),
                                 error = function(e) e)
        if (methods::is(return.value, "error"))
            stop(gettextf("unable to collapse '%s' to make it compatible with dataset '%s' : %s",
                          nameY, names.datasets[i], return.value$message))
        transform <- dembase::makeTransform(x = y, y = dataset, subset = TRUE, check = FALSE)
        transform <- dembase::makeCollapseTransformExtra(transform)
        ans[[i]] <- transform
    }
    ans
}

## HAS_TESTS
makeValueAndMetaDataAg <- function(value) {
    if (methods::is(value, "DemographicArray")) {
        if (any(is.na(value)))
            stop(gettextf("'%s' has missing values",
                          "value"))
        metadata <- value@metadata
        value <- as.double(value@.Data)
    }
    else if (is.numeric(value)) {
        if (!identical(length(value), 1L))
            stop(gettextf("'%s' does not have length %d",
                          "value", 1L))
        if (is.na(value))
            stop(gettextf("'%s' is missing",
                          "value"))
        metadata <- NULL
        value <- as.double(value)
    }
    else
        stop(gettextf("'%s' has class \"%s\"",
                      "value", class(value)))
    value <- methods::new("ParameterVector", value)
    list(value = value, metadata = metadata)
}

## HAS_TESTS
makeWeightAg <- function(weight, default, model, thetaObj, transform, values) {
    if (is.null(weight)) {
        if (is.null(default))
            stop(gettext("no aggregate weights supplied, and no default weights"))
        else
            ans <- default
    }
    else {
        ans <- tryCatch(dembase::makeCompatible(x = weight,
                                                y = thetaObj,
                                                subset = TRUE),
                        error = function(e) e)
        if (methods::is(ans, "error"))
            stop(gettextf("%s not compatible with '%s' : %s",
                          "aggregate weight", "y", ans$message))
    }
    ans <- as.double(ans)
    for (i in seq_along(ans)) {
        k <- dembase::getIAfter(i,
                                transform = transform,
                                useC = TRUE)
        if (k == 0L)
            ans[i] <- NA
        else {
            if (is.na(ans[i]))
                stop(gettextf("element %d of '%s' is needed for aggregate values but is missing",
                              i, "weightAg"))
        }
    }
    normalise <- is.null(weight) && !methods::is(model, "PoissonVaryingNotUseExp")
    if (normalise) {
        for (k in seq_along(values)) {
            i.wt <- dembase::getIBefore(k, transform = transform, useC = TRUE)
            wt <- ans[i.wt]
            sum.wt <- sum(wt)
            if (sum.wt > 0)
                wt <- wt / sum(wt)
            ans[i.wt] <- wt
        }
    }
    ans
}

    

## RANDOM VARIATES #################################################################

## TRANSLATED
## HAS_TESTS
dpoibin1 <- function(x, size, prob, log = FALSE, useC = FALSE) {
    for (name in c("x", "size")) {
        value <- get(name)
        if (!identical(length(value), 1L))
            stop(gettextf("'%s' does not have length %d",
                          name, 1L))
        if (!is.integer(value))
            stop(gettextf("'%s' does not have type \"%s\"",
                          name, "integer"))
        if (is.na(value))
            stop(gettextf("'%s' is missing",
                          name))
        if (value < 0L)
            stop(gettextf("'%s' is negative", name))
    }
    if (!identical(length(prob), 1L))
        stop(gettextf("'%s' does not have length %d",
                      "prob", 1L))
    if (!is.double(prob))
        stop(gettextf("'%s' does not have type \"%s\"",
                      "prob", "double"))
    if (is.na(prob))
        stop(gettextf("'%s' is missing",
                      "prob"))
    if (prob < 0)
        stop(gettextf("'%s' is negative", "prob"))
    if (prob > 1)
        stop(gettextf("'%s' is greater than %d",
                      "prob", 1L))
    if (!is.logical(log))
        stop(gettextf("'%s' does not have type \"%s\"", "log", "logical"))
    if (!identical(length(log), 1L))
        stop(gettextf("'%s' does not have length %d", "log", 1L))
    if (is.na(log))
        stop(gettextf("'%s' is missing", "log"))
    if (useC) {
        .Call(dpoibin1_R, x, size, prob, log)
    }
    else {
        kThreshold <- 1000
        lambda <- (1 - prob) * size
        if (x > kThreshold) {
            mean.binom <- prob * size
            var.binom <- prob * (1 - prob) * size
            mean.pois <- lambda
            var.pois <- lambda
            mean <- mean.binom + mean.pois
            var <- var.binom + var.pois
            sd <- sqrt(var)
            ans <- stats::dnorm(x, mean = mean, sd = sd, log = log)
        }
        else {
            limit <- min(x, size)
            ans <- 0
            for (i in seq.int(from = 0L, to = limit)) {
                prob.binom <- stats::dbinom(i, size = size, prob = prob)
                prob.pois <- stats::dpois(x - i, lambda = lambda)
                ans <- ans + prob.binom * prob.pois
            }
            if (log)
                ans <- log(ans)
        }
        ans
    }
}

## TRANSLATED
## HAS_TESTS
invlogit1 <- function(x, useC = FALSE) {
    if (!identical(length(x), 1L))
        stop(gettextf("'%s' does not have length %d",
                      "x", 1L))
    if (!is.numeric(x))
        stop(gettextf("'%s' is non-numeric",
                      "x"))
    if (is.na(x))
        stop(gettextf("'%s' is missing",
                      "x"))
    if (useC) {
        .Call(invlogit1_R, x)
    }
    else {
        if (x > 0)
            1 / (1 + exp(-x))
        else
            exp(x) / (1 + exp(x))
    }
}

## TRANSLATED
## HAS_TESTS
## Generate one random deviate from a categorical distribution
## with cumulative probability given by 'cumProb'.
## Algorithm from Ripley (1987) Stochastic Simulation, p71
rcateg1 <- function(cumProb, useC = FALSE) {
    stopifnot(is.double(cumProb))
    stopifnot(length(cumProb) > 0L)
    stopifnot(!any(is.na(cumProb)))
    stopifnot(all(cumProb > 0))
    if (length(cumProb) > 1L)
        stopifnot(all(diff(cumProb) >= 0))
    stopifnot(all.equal(cumProb[length(cumProb)], 1))
    if (useC) {
        .Call(rcateg1_R, cumProb)
    }
    else {
        u <- stats::runif(n = 1L)
        i <- 1L
        while (cumProb[i] <= u)
            i <- i + 1L
        i
    }
}

#' The half-t distribution.
#'
#' Density, distribution function, quantile function and random
#' generation for the halt-t distribution.
#'
#' The half-t distribution is also known as the folded-t distribution.
#'
#' If \eqn{X} has a \emph{t} distribution with degrees of freedom
#' \eqn{v}, location 0, and scale \code{s}, then \eqn{|X|}
#' has a half-\emph{t} distribution with degrees of freedom \eqn{v}
#' and scale \code{s}.
#'
#' Internally, the functions all call the corresponding functions
#' for the \code{\link[=TDist]{t distribution}}.
#' 
#' @param x Vector of quantiles.
#' @param p Vector of quantiles.
#' @param q Vector of probabilities.
#' @param n Number of observations.
#' @param df Degrees of freedom. Positive. Can be non-integer,
#'     and can be \code{Inf}.
#' @param scale Dispersion parameter.
#'
#' @return \code{dhalft} gives the density, \code{phalft} gives
#'     the distribution function, \code{qhalft} gives the
#'     quantile function, and \code{rhalft} generates random
#'     deviates.
#'
#' @seealso Function \code{\link{plotHalfT}} plots density and distribution
#' functions for half-\emph{t} distributions.
#'
#' @references
#' Based on Brazauskas, V., and Kleefeld, A. (2011) Folded and log-folded-t
#' distributions as models for insurance loss data.
#' \emph{Scandinavian Actuarial Journal} 59-74.
#'
#' @examples
#' dhalft(x = 0.5, df = 7, scale = 0.5)
#' qhalft(p = 0.9, df = 4)
#' phalft(q = 0.5, df = 7, scale = 2)
#' rhalft(n = 5, df = 30)
#' @name halft-distn
#' @aliases rhalft, qhalft, phalft, dhalft
NULL

## HAS_TESTS
#' @rdname halft-distn
#' @export
rhalft <- function(n, df, scale = 1) {
    if (!is.numeric(scale))
        stop(gettextf("'%s' is non-numeric",
                      "scale"))
    if (any(scale[!is.na(scale)] <= 0))
        stop(gettextf("'%s' is non-positive",
                      "scale"))
    ans <- stats::rt(n = n, df = df)
    scale * abs(ans)
}

## HAS_TESTS        
#' @rdname halft-distn
#' @export
qhalft <- function(p, df, scale = 1) {
    if (!is.numeric(scale))
        stop(gettextf("'%s' is non-numeric",
                      "scale"))
    if (any(scale[!is.na(scale)] <= 0))
        stop(gettextf("'%s' is non-positive",
                      "scale"))
    p <- (p + 1) / 2
    ans <- stats::qt(p = p,
                     df = df)
    scale * ans
}

## HAS_TESTS
#' @rdname halft-distn
#' @export
phalft <- function(q, df, scale = 1) {
    if (!is.numeric(scale))
        stop(gettextf("'%s' is non-numeric",
                      "scale"))
    if (any(scale[!is.na(scale)] <= 0))
        stop(gettextf("'%s' is non-positive",
                      "scale"))
    q <- q / scale
    ans <- stats::pt(q = q,
                     df = df)
    2 * (ans - 0.5)
}

## HAS_TESTS
#' @rdname halft-distn
#' @export
dhalft <- function(x, df, scale = 1) {
    if (!is.numeric(scale))
        stop(gettextf("'%s' is non-numeric",
                      "scale"))
    if (any(scale[!is.na(scale)] <= 0))
        stop(gettextf("'%s' is non-positive",
                      "scale"))
    x <- x / scale
    (2/scale) * stats::dt(x = x, df = df, log = FALSE)
}

#' Plot the half-t distribution.
#'
#' Plot the density or distribution function of a
#' \code{\link[=halft-distn]{half-t}} distribution.
#'
#' @inheritParams halft-distn
#' @param max A quantile, defaulting to 0.999.  The x-axis for the plot
#' extends from 0 to this quantile.
#' @param density Whether to plot the density function (the default) or the
#' distribution function.
#' @param add Whether to add to the current plot.
#' @param \dots Other arguments, passed to functions \code{\link[graphics]{plot}}
#' and \code{\link[graphics]{lines}}.
#'
#' @examples
#' plotHalfT()
#' plotHalfT(df = 4, add = TRUE, col = "red")
#' plotHalfT(df = 4, scale = 1.1, add = TRUE, col = "blue")
#' @export
plotHalfT <- function(df = 7, scale = 1, max = 0.999,
                      density = TRUE, add = FALSE, ...) {
    xmax <- qhalft(p = max, df = df, scale = scale)
    x <- seq(from = 0, to = xmax, length.out = 500)
    if (density) {
        density <- dhalft(x = x, df = df, scale = scale)
        if (add)
            graphics::lines(x = x, y = density, ...)
        else
            graphics::plot(x = x, y = density, type = "l", ...)
    }
    else {
        prob <- phalft(q = x, df = df, scale = scale)
        if (add)
            graphics::lines(x = x, y = prob, ...)
        else
            graphics::plot(x = x, y = prob, type = "l", ...)
    }
}



## HAS_TESTS
rinvchisq1 <- function(df, scale, useC = FALSE) {
    stopifnot(is.double(df))
    stopifnot(identical(length(df), 1L))
    stopifnot(!is.na(df))
    stopifnot(df > 0)
    stopifnot(is.double(scale))
    stopifnot(identical(length(scale), 1L))
    stopifnot(!is.na(scale))
    stopifnot(scale > 0)
    if (useC) {
        .Call(rinvchisq1_R, df, scale)
    }
    else {
        X <- stats::rchisq(n = 1, df = df)
        df * scale / X
    }
}

## TRANSLATED
## HAS_TESTS
rmvnorm1 <- function(mean, var, useC = FALSE) {
    ## mean
    stopifnot(is.double(mean))
    stopifnot(!any(is.na(mean)))
    ## var
    stopifnot(is.matrix(var))
    stopifnot(is.double(var))
    stopifnot(nrow(var) == ncol(var))
    stopifnot(all(diag(var) >= 0))
    stopifnot(all.equal(var, t(var)))
    ## mean and var
    stopifnot(ncol(var) == length(mean))
    if (useC) {
        .Call(rmvnorm1_R, mean, var)
    }
    else {
        n <- length(mean)
        R <- chol(var)
        z <- stats::rnorm(n = n)
        mean + drop(R %*% z)
    }
}

## TRANSLATED
## HAS_TESTS
rmvnorm2 <- function(mean, var, useC = FALSE) {
    ## mean
    stopifnot(is.double(mean))
    stopifnot(identical(length(mean), 2L))
    stopifnot(!any(is.na(mean)))
    ## var
    stopifnot(identical(dim(var), c(2L, 2L)))
    stopifnot(is.double(var))
    stopifnot(all(diag(var) >= 0))
    stopifnot(all.equal(var, t(var)))
    if (useC) {
        .Call(rmvnorm2_R, mean, var)
    }
    else {
        kTolerance <- -1e-6
        mean1 <- mean[1L]
        sd1 <- sqrt(var[1L])
        ans1 <- stats::rnorm(n = 1L, mean = mean1, sd = sd1)
        mean2 <- mean[2L] + (var[2L] / var[1L]) * (ans1 - mean1)
        var2 <- var[4L] - (var[3]^2 / var[1L])
        if (var2 < 0) {
            if (var2 < kTolerance)
                stop("'var' is invalid")
            else
                var2 <- 0
        }
        sd2 <- sqrt(var2)
        ans2 <- stats::rnorm(n = 1L, mean = mean2, sd = sd2)
        c(ans1, ans2)
    }
}    

## TRANSLATED
## HAS_TESTS
rnormTruncated <- function(n, mean, sd, lower, upper, tolerance = 1e-5, maxAttempt,
                           uniform = TRUE, useC = FALSE) {
    ## n
    stopifnot(identical(length(n), 1L))
    stopifnot(is.integer(n))
    stopifnot(n > 0L)
    ## mean
    stopifnot(identical(length(mean), n))
    stopifnot(is.double(mean))
    stopifnot(!any(is.na(mean)))
    ## sd
    stopifnot(identical(length(sd), n))
    stopifnot(is.double(sd))
    stopifnot(!any(is.na(sd)))
    stopifnot(all(sd >= 0))
    ## lower
    stopifnot(is.double(lower))
    stopifnot(identical(length(lower), 1L))
    stopifnot(!is.na(lower))
    ## upper
    stopifnot(is.double(upper))
    stopifnot(identical(length(upper), 1L))
    stopifnot(!is.na(upper))
    ## tolerance
    stopifnot(is.double(tolerance))
    stopifnot(identical(length(tolerance), 1L))
    stopifnot(!is.na(tolerance))
    stopifnot(tolerance >= 0)
    ## maxAttempt
    stopifnot(identical(length(maxAttempt), 1L))
    stopifnot(is.integer(maxAttempt))
    stopifnot(!is.na(maxAttempt))
    stopifnot(maxAttempt > 0L)
    ## uniform
    stopifnot(identical(length(uniform), 1L))
    stopifnot(is.logical(uniform))
    stopifnot(!is.na(uniform))
    ## lower, upper
    stopifnot((lower + tolerance) < (upper - tolerance))
    if (useC) {
        .Call(rnormTruncated_R, n, mean, sd, lower, upper, tolerance, maxAttempt, uniform)
    }
    else {
        ans <- double(n)
        for (i in seq_len(n)) {
            found <- FALSE
            n.attempt <- 0L
            while (!found && (n.attempt < maxAttempt)) {
                n.attempt <- n.attempt + 1L
                prop.value <- stats::rnorm(n = 1L, mean = mean[i], sd = sd[i])
                found <- (prop.value > (lower + tolerance)) && (prop.value < (upper - tolerance))
            }
            if (found)
                ans[i] <- prop.value
            else {
                if (uniform) {
                    if (lower + tolerance > mean[i]) {
                        lower.unif <- lower + tolerance
                        upper.unif <- min(lower + tolerance + sd[i], upper - tolerance)
                    }
                    else {
                        upper.unif <- upper - tolerance
                        lower.unif <- max(upper - tolerance - sd[i], lower + tolerance)
                    }
                    ans[i] <- stats::runif(n = 1L,
                                           min = lower.unif,
                                           max = upper.unif)
                }
                else
                    stop("failed to generate value within specified range")
            }
        }
        ans
    }
}

## TRANSLATED
## HAS_TESTS
## modified from code in package 'TruncatedNormal'.
## which uses alorithm from Z. I. Botev (2015),
## "The Normal Law Under Linear Restrictions:
##  Simulation and Estimation via Minimax Tilting", submitted to JRSS(B)
rtnorm1 <- function(mean = 0, sd = 1, lower = -Inf, upper = Inf,
                    useC = FALSE) {
    ## mean
    stopifnot(identical(length(mean), 1L))
    stopifnot(is.double(mean))
    stopifnot(!is.na(mean))
    ## sd
    stopifnot(identical(length(sd), 1L))
    stopifnot(is.double(sd))
    stopifnot(!is.na(sd))
    stopifnot(sd >= 0)
    ## lower
    stopifnot(identical(length(lower), 1L))
    stopifnot(is.double(lower))
    stopifnot(!is.na(lower))
    ## upper
    stopifnot(identical(length(upper), 1L))
    stopifnot(is.double(upper))
    stopifnot(!is.na(upper))
    ## lower, upper
    stopifnot(lower < upper)
    if (useC) {
        .Call(rtnorm1_R, mean, sd, lower, upper)
    }
    else {
        ## set threshold for switching between methods - in C this can be done via macro
        a <- 0.4
        tol <- 2.05
        ## standardized variables
        l <- (lower - mean)/sd
        u <- (upper - mean)/sd
        if (l > a) {
            c <- l^2 / 2
            f <- expm1(c - u^2 / 2)
            x <- c - log(1 + stats::runif(n = 1L) * f); # sample using Rayleigh
            ## keep list of rejected
            while (stats::runif(n = 1L)^2 * x > c) { # while there are rejections
                x <- c - log(1 + stats::runif(n = 1L) * f)
            }
            ans <- sqrt(2*x) # this Rayleigh transform can be delayed till the end
        }
        else if (u < (-a)) {
            c <- (-u)^2 / 2
            f <- expm1(c - (-l)^2 / 2)
            x <- c-log(1+stats::runif(n = 1L)*f); # sample using Rayleigh
            ## keep list of rejected
            while (stats::runif(n = 1L)^2 * x > c) { # while there are rejections
                x <- c - log(1 + stats::runif(n = 1L) * f)
            }
            ans <- (-1)*sqrt(2*x) # this Rayleigh transform can be delayed till the end
        }
        else {
            if (abs(u-l) > tol) { # abs(u-l)>tol, uses accept-reject
                x <- stats::rnorm(n = 1L) # sample from standard normal
                while (x < l | x > u) { # while there are rejections
                    x <- stats::rnorm(n = 1L)
                } 
                ans <- x         
            }
            else { # abs(u-l)<tol, uses inverse-transform
                pl <- stats::pnorm(q = l)
                pu <- stats::pnorm(q = u)
                u <- stats::runif(n = 1L)
                trans <- pl + (pu - pl) * u
                ans <- stats::qnorm(p = trans)
            }      
        }
        ans * sd + mean
    }
}

## TRANSLATED
## HAS_TESTS
## If no upper limit, 'upper' is NA_integer_
rpoisTrunc1 <- function(lambda, lower, upper, maxAttempt, useC = FALSE) {
    ## lambda
    stopifnot(is.double(lambda))
    stopifnot(identical(length(lambda), 1L))
    stopifnot(!is.na(lambda))
    stopifnot(lambda >= 0)
    ## lower
    stopifnot(is.integer(lower))
    stopifnot(identical(length(lower), 1L))
    stopifnot(!is.na(lower))
    ## upper
    stopifnot(is.integer(upper))
    stopifnot(identical(length(upper), 1L))
    ## maxAttempt
    stopifnot(is.integer(maxAttempt))
    stopifnot(identical(length(maxAttempt), 1L))
    stopifnot(!is.na(maxAttempt))
    stopifnot(maxAttempt > 0L)
    ## lower, upper
    stopifnot(is.na(upper) || (lower <= upper))
    if (useC) {
        .Call(rpoisTrunc1_R, lambda, lower, upper, maxAttempt)
    }
    else {
        finite.upper <- !is.na(upper)
        if (finite.upper && (lower == upper))
            return(lower)
        n.attempt <- 0L
        found <- FALSE
        if (finite.upper) {
            while (!found && (n.attempt < maxAttempt)) {
                n.attempt <- n.attempt + 1L
                prop.value <- stats::rpois(n = 1L, lambda = lambda)
                found <- (prop.value >= lower) && (prop.value <= upper)
            }
        }
        else {
            while (!found && (n.attempt < maxAttempt)) {
                n.attempt <- n.attempt + 1L
                prop.value <- stats::rpois(n = 1L, lambda = lambda)
                found <- prop.value >= lower
            }
        }
        if (found)
            as.integer(prop.value)
        else
            NA_integer_
    }
}

## ALONG ITERATOR ################################################################

## TRANSLATED
## HAS_TESTS
centerA <- function(vec, iterator, useC = FALSE) {
    stopifnot(is.double(vec))
    stopifnot(methods::is(iterator, "AlongIterator"))
    methods::validObject(iterator)
    indices <- iterator@indices
    nWithin <- iterator@nWithin
    nBetween <- iterator@nBetween
    stopifnot(identical(length(vec), as.integer(length(indices) * nWithin * nBetween)))
    if (useC) {
        .Call(centerA_R, vec, iterator)
    }
    else {
        indices <- iterator@indices
        nWithin <- iterator@nWithin
        nBetween <- iterator@nBetween
        length.ans <- length(indices) * nWithin * nBetween
        iterator <- resetA(iterator)
        ans <- numeric(length = length.ans)
        n.classifying <- length.ans / length(indices)
        for (i in seq_len(n.classifying)) {
            indices <- iterator@indices
            ans[indices] <- vec[indices] - mean(vec[indices])
            iterator <- advanceA(iterator)
        }
        ans
    }
}




## UPDATING ###########################################################################

## DOES_NOT_NEED_TESTS
checkUpdateBetaAndPriorBeta <- function(prior, vbar, n, sigma) {
    ## prior
    stopifnot(methods::validObject(prior))
    ## vbar
    stopifnot(is.double(vbar))
    stopifnot(!any(is.na(vbar)))
    ## n
    stopifnot(is.integer(n))
    stopifnot(identical(length(n), 1L))
    stopifnot(!is.na(n))
    stopifnot(n > 0L)
    ## sigma
    stopifnot(is.double(sigma))
    stopifnot(identical(length(sigma), 1L))
    stopifnot(!is.na(sigma))
    stopifnot(sigma > 0)
    NULL
}

## TRANSLATED but C code for ICAR and Cross not tested
## HAS_TESTS
## ADD TESTS FOR ICAR AND Cross WHEN CLASSES FINISHED
betaHat <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "Prior"))
    stopifnot(methods::is(prior, "ComponentFlags"))
    if (useC) {
        .Call(betaHat_R, prior)
    }
    else {
        J <- prior@J@.Data
        has.alpha.cross <- prior@hasAlphaMove@.Data
        has.alpha.dlm <- prior@hasAlphaDLM@.Data
        has.alpha.icar <- prior@hasAlphaICAR@.Data
        has.covariates <- prior@hasCovariates@.Data
        has.season <- prior@hasSeason@.Data
        ans <- rep(0, times = J)
        if (has.alpha.cross) {
            alpha.cross <- prior@alphaCross@.Data
            indices.cross <- prior@indicesCross
            for (j in seq_len(J)) {
                index.cross <- indices.cross[j]
                if (is.infinite(index.cross))
                    ans[j] <- if (index.cross < 0) -Inf else Inf
                else
                    ans[j] <- ans[j] + alpha.cross[index.cross]
            }
        }
        if (has.alpha.dlm) {
            alpha.dlm <- prior@alphaDLM@.Data
            K <- prior@K@.Data
            L <- prior@L@.Data
            iterator.alpha <- prior@iteratorState
            iterator.v <- prior@iteratorV
            iterator.alpha <- resetA(iterator.alpha)
            iterator.v <- resetA(iterator.v)
            for (l in seq_len(L)) {
                indices.alpha <- iterator.alpha@indices
                indices.v <- iterator.v@indices
                for (k in seq_len(K)) {
                    i.alpha <- indices.alpha[k + 1L]
                    i.ans <- indices.v[k]
                    ans[i.ans] <- ans[i.ans] + alpha.dlm[i.alpha]
                }
                iterator.alpha <- advanceA(iterator.alpha)
                iterator.v <- advanceA(iterator.v)
            }
        }
        if (has.alpha.icar) {
            alpha.icar <- prior@alphaICAR@.Data
            ans <- ans + alpha.icar
        }
        if (has.covariates) {
            Z <- unname(prior@Z)
            eta <- prior@eta@.Data
            ans <- ans + drop(Z %*% eta)
        }
        if (has.season) {
            s <- prior@s@.Data
            K <- prior@K@.Data
            L <- prior@L@.Data
            iterator.s <- prior@iteratorState
            iterator.v <- prior@iteratorV
            iterator.s <- resetA(iterator.s)
            iterator.v <- resetA(iterator.v)
            for (l in seq_len(L)) {
                indices.s <- iterator.s@indices
                indices.v <- iterator.v@indices
                for (k in seq_len(K)) {
                    i.s <- indices.s[k + 1L]
                    i.ans <- indices.v[k]
                    ans[i.ans] <- ans[i.ans] + s[[i.s]][1L]
                }
                ## changed these 2 lines - JAH 18/4/2016
                iterator.s <- advanceA(iterator.s)
                iterator.v <- advanceA(iterator.v)
            }
        }
        ans
    }
}

## ## NO_TESTS
## betaHatAlphaCross <- function(prior, useC = TRUE) {
##     stopifnot(methods::is(prior, "Prior"))
##     stopifnot(methods::is(prior, "ComponentFlags"))
##     stopifnot(prior@hasAlphaMove)
##     if (useC) {
##         .Call(betaHatAlphaCross_R, prior)
##     }
##     else {
##         ans <- rep(0, times = J)
##         alpha <- prior@alphaCross@.Data
##         indices <- prior@indicesCross
##         for (j in seq_len(J)) {
##             i.alpha <- indices[j]
##             if (is.infinite(i.alpha))
##                 ans[j] <- if (i.alpha < 0) -Inf else Inf
##             else
##                 ans[j] <- alpha[i.alpha]
##         }
##         ans
##     }
## }

## TRANSLATED
## HAS TESTS
betaHatAlphaDLM <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "Prior"))
    stopifnot(methods::is(prior, "ComponentFlags"))
    stopifnot(prior@hasAlphaDLM)
    if (useC) {
        .Call(betaHatAlphaDLM_R, prior)
    }
    else {
        J <- prior@J@.Data
        ans <- rep(0, times = J)
        alpha.dlm <- prior@alphaDLM@.Data
        K <- prior@K@.Data
        L <- prior@L@.Data
        iterator.alpha <- prior@iteratorState
        iterator.v <- prior@iteratorV
        iterator.alpha <- resetA(iterator.alpha)
        iterator.v <- resetA(iterator.v)
        for (l in seq_len(L)) {
            indices.alpha <- iterator.alpha@indices
            indices.v <- iterator.v@indices
            for (k in seq_len(K)) {
                i.alpha <- indices.alpha[k + 1L]
                i.ans <- indices.v[k]
                ans[i.ans] <- ans[i.ans] + alpha.dlm[i.alpha]
            }
            iterator.alpha <- advanceA(iterator.alpha)
            iterator.v <- advanceA(iterator.v)
        }
        ans
    }
}

## ## NO_TESTS
## betaHatAlphaICAR <- function(prior, useC = FALSE) {
##     stopifnot(methods::is(prior, "Prior"))
##     stopifnot(methods::is(prior, "ComponentFlags"))
##     stopifnot(prior@hasAlphaICAR)
##     if (useC) {
##         .Call(betaHatICAR_R, prior)
##     }
##     else {
##         prior@alphaICAR@.Data
##     }
## }


## TRANSLATED
## HAS_TESTS
betaHatCovariates <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "Prior"))
    stopifnot(methods::is(prior, "ComponentFlags"))
    stopifnot(prior@hasCovariates)
    if (useC) {
        .Call(betaHatCovariates_R, prior)
    }
    else {
        Z <- unname(prior@Z)
        eta <- prior@eta@.Data
        drop(Z %*% eta)
    }
}

## TRANSLATED
## HAS_TESTS
betaHatSeason <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "Prior"))
    stopifnot(methods::is(prior, "ComponentFlags"))
    stopifnot(prior@hasSeason)
    if (useC) {
        .Call(betaHatSeason_R, prior)
    }
    else {
        J <- prior@J@.Data
        ans <- rep(0, times = J)
        s <- prior@s@.Data
        K <- prior@K@.Data
        L <- prior@L@.Data
        iterator.s <- prior@iteratorState
        iterator.v <- prior@iteratorV
        iterator.s <- resetA(iterator.s)
        iterator.v <- resetA(iterator.v)
        for (l in seq_len(L)) {
            indices.s <- iterator.s@indices
            indices.v <- iterator.v@indices
            for (k in seq_len(K)) {
                i.s <- indices.s[k + 1L]
                i.ans <- indices.v[k]
                ans[i.ans] <- ans[i.ans] + s[[i.s]][1L]
            }
            iterator.s <- advanceA(iterator.s)
            iterator.v <- advanceA(iterator.v)
        }
        ans
    }
}

## TRANSLATED
## modified from pseudocode from https://en.wikipedia.org/wiki/Newton%27s_method
## If the function finds the root within 'kMaxIter' iterations, it
## returns this root.  If the derivative of 'f' is near 0, the function
## returns -1.0.  If the function fails to find a root, it returns -99.0.
## The root must be between 'min' and 'max'.
findOneRootLogPostSigmaNorm <- function(sigma0, z, A, nu, V, n, min, max,
                                        useC = FALSE) {
    ## 'sigma0'
    stopifnot(identical(length(sigma0), 1L))
    stopifnot(is.double(sigma0))
    stopifnot(!is.na(sigma0))
    stopifnot(sigma0 > 0)
    ## 'z'
    stopifnot(identical(length(z), 1L))
    stopifnot(is.double(z))
    stopifnot(!is.na(z))
    ## 'A'
    stopifnot(identical(length(A), 1L))
    stopifnot(is.double(A))
    stopifnot(!is.na(A))
    stopifnot(A > 0)
    ## 'nu'
    stopifnot(identical(length(nu), 1L))
    stopifnot(is.double(nu))
    stopifnot(!is.na(nu))
    stopifnot(nu > 0)
    ## 'V'
    stopifnot(identical(length(V), 1L))
    stopifnot(is.double(V))
    stopifnot(!is.na(V))
    stopifnot(V > 0)
    ## 'n'
    stopifnot(identical(length(n), 1L))
    stopifnot(is.integer(n))
    stopifnot(!is.na(n))
    stopifnot(n > 0L)
    ## 'min'
    stopifnot(identical(length(min), 1L))
    stopifnot(is.double(min))
    stopifnot(!is.na(min))
    ## 'max'
    stopifnot(identical(length(max), 1L))
    stopifnot(is.double(max))
    stopifnot(!is.na(max))
    ## 'min' and 'max'
    stopifnot(max > min)
    if (useC) {
        .Call(findOneRootLogPostSigmaNorm_R, sigma0, z, A, nu, V, n, min, max)
    }
    else {
        kTolerance <- 1e-7           ## C version can
        kEpsilonMax <- 1e-14         ## use macros to
        kEpsilonBoundaries <- 1e-30  ## set these
        kMaxIter <- 1000L     
        for (i in seq_len(kMaxIter)) {
            f <- -n*log(sigma0) - V/(2*sigma0^2) - ((nu + 1)/2) * log(sigma0^2 + nu*A^2) - z
            fprime <- -n/sigma0 + V/(sigma0^3) - ((nu + 1)*sigma0) / (sigma0^2 + nu*A^2)
            deriv_near_zero <- abs(fprime) < kEpsilonMax
            if (deriv_near_zero)
                return(-1.0)
            sigma1 <- sigma0 - f / fprime
            sigma1 <- max(sigma1, min + kEpsilonBoundaries)
            sigma1 <- min(sigma1, max - kEpsilonBoundaries)
            if (sigma1 - sigma0 > 1)
                sigma1 <- sigma0 + 1
            sigma1.equals.sigma0 <- abs(sigma1 - sigma0) < kTolerance * abs(sigma1)
            if (sigma1.equals.sigma0)
                return(sigma1)
            sigma0 <- sigma1
        }
        ## reached maximum iterations without finding root
        -99.0
    }
}

## TRANSLATED
## modified from pseudocode from https://en.wikipedia.org/wiki/Newton%27s_method
## If the function finds the root within 'kMaxIter' iterations, it
## returns this root.  If the derivative of 'f' is near 0, the function
## returns -1.0.  If the function fails to find a root, it returns -99.0.
## The root must be between 'min' and 'max'.  Note that this function
## uses a different 'f' and 'fprime' from function 'findOneRootLogPosSigmaNorm'.
findOneRootLogPostSigmaRobust <- function(sigma0, z, A, nuBeta, nuTau, V, n, min, max,
                                          useC = FALSE) {
    ## 'sigma0'
    stopifnot(identical(length(sigma0), 1L))
    stopifnot(is.double(sigma0))
    stopifnot(!is.na(sigma0))
    stopifnot(sigma0 > 0)
    ## 'z'
    stopifnot(identical(length(z), 1L))
    stopifnot(is.double(z))
    stopifnot(!is.na(z))
    ## 'A'
    stopifnot(identical(length(A), 1L))
    stopifnot(is.double(A))
    stopifnot(!is.na(A))
    stopifnot(A > 0)
    ## 'nuBeta'
    stopifnot(identical(length(nuBeta), 1L))
    stopifnot(is.double(nuBeta))
    stopifnot(!is.na(nuBeta))
    stopifnot(nuBeta > 0)
    ## 'nuTau'
    stopifnot(identical(length(nuTau), 1L))
    stopifnot(is.double(nuTau))
    stopifnot(!is.na(nuTau))
    stopifnot(nuTau > 0)
    ## 'V'
    stopifnot(identical(length(V), 1L))
    stopifnot(is.double(V))
    stopifnot(!is.na(V))
    stopifnot(V > 0)
    ## 'n'
    stopifnot(identical(length(n), 1L))
    stopifnot(is.integer(n))
    stopifnot(!is.na(n))
    stopifnot(n > 0L)
    ## 'min'
    stopifnot(identical(length(min), 1L))
    stopifnot(is.double(min))
    stopifnot(!is.na(min))
    ## 'max'
    stopifnot(identical(length(max), 1L))
    stopifnot(is.double(max))
    stopifnot(!is.na(max))
    ## 'min' and 'max'
    stopifnot(max > min)
    if (useC) {
        .Call(findOneRootLogPostSigmaRobust_R, sigma0, z, A, nuBeta, nuTau, V, n, min, max)
    }
    else {
        kTolerance <- 1e-7           ## C version can
        kEpsilonMax <- 1e-14         ## use macros to
        kEpsilonBoundaries <- 1e-30  ## set these
        kMaxIter <- 1000L     
        for (i in seq_len(kMaxIter)) {
            f <- n*nuBeta*log(sigma0) - (nuBeta/2)*(sigma0^2)*V - ((nuTau+1)/2)*log(sigma0^2 + nuTau*A^2) - z
            fprime <- n*nuBeta/sigma0 - nuBeta*sigma0*V - ((nuTau + 1)*sigma0)/(sigma0^2 + nuTau*A^2)
            deriv_near_zero <- abs(fprime) < kEpsilonMax
            if (deriv_near_zero)
                return(-1.0)
            sigma1 <- sigma0 - f / fprime
            sigma1 <- max(sigma1, min + kEpsilonBoundaries)
            sigma1 <- min(sigma1, max - kEpsilonBoundaries)
            if (sigma1 - sigma0 > 1)
                sigma1 <- sigma0 + 1
            sigma1.equals.sigma0 <- abs(sigma1 - sigma0) < kTolerance * abs(sigma1)
            if (sigma1.equals.sigma0)
                return(sigma1)
            sigma0 <- sigma1
        }
        ## reached maximum iterations without finding root
        -99.0
    }
}

## TRANSLATED
## HAS_TESTS
getV <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "Prior"))
    stopifnot(methods::is(prior, "IsRobustMixin"))
    stopifnot(methods::validObject(prior))
    if (useC) {
        .Call(getV_R, prior)
    }
    else {
        is.robust <- prior@isRobust@.Data
        if (is.robust)
            prior@UBeta@.Data
        else {
            J <- prior@J@.Data
            tau <- prior@tau@.Data
            rep(tau^2, times = J)
        }
    }
}

## READY_TO_TRANSLATE
## HAS_TESTS
logPostPhiMix <- function(phi, level, meanLevel, nAlong, indexClassMax, omega,
                          useC = FALSE) {
    ## 'phi'
    stopifnot(identical(length(phi), 1L))
    stopifnot(is.double(phi))
    stopifnot(!is.na(phi))
    stopifnot(abs(phi) <= 1)
    ## 'level'
    stopifnot(is.double(level))
    stopifnot(!any(is.na(level)))
    ## 'meanLevel'
    stopifnot(identical(length(meanLevel), 1L))
    stopifnot(is.double(meanLevel))
    stopifnot(!is.na(meanLevel))
    ## 'nAlong'
    stopifnot(identical(length(nAlong), 1L))
    stopifnot(is.integer(nAlong))
    stopifnot(!is.na(nAlong))
    stopifnot(nAlong >= 2L)
    ## 'indexClassMax'
    stopifnot(identical(length(indexClassMax), 1L))
    stopifnot(is.integer(indexClassMax))
    stopifnot(!is.na(indexClassMax))
    stopifnot(indexClassMax > 0L)
    ## 'omega'
    stopifnot(identical(length(omega), 1L))
    stopifnot(is.double(omega))
    stopifnot(!is.na(omega))
    stopifnot(omega > 0)
    ## 'level', 'nAlong', 'indexClassMax'
    stopifnot(length(level) >= nAlong * indexClassMax)
    if (useC) {
        .Call(logPostPhiMix_R, phi, level, meanLevel, nAlong, indexClassMax, omega)
    }
    else {
        if (abs(phi) < 1) {
            ratio <- meanLevel / (1 - phi)
            ans.first <- 0
            for (i.class in seq_len(indexClassMax)) {
                i.wt.first <- (i.class - 1L) * nAlong + 1L
                level.first <- level[i.wt.first]
                ans.first <- ans.first + (level.first - ratio)^2
            }
            ans.first <- (1 - phi^2) * ans.first
            ans.rest <- 0
            for (i.class in seq_len(indexClassMax)) {
                for (i.along in seq.int(from = 2L, to = nAlong)) {
                    i.wt.curr <- (i.class - 1L) * nAlong + i.along
                    i.wt.prev <- i.wt.curr - 1L
                    level.curr <- level[i.wt.curr]
                    level.prev <- level[i.wt.prev]
                    ans.rest <-  ans.rest + (level.curr - meanLevel - phi * level.prev)^2
                }
            }
            (ans.first + ans.rest) / (-2 * omega^2)
        }
        else {
            0.0001
        }
    }
}

## READY_TO_TRANSLATE
## HAS_TESTS
logPostPhiFirstOrderMix <- function(phi, level, meanLevel, nAlong, indexClassMax, omega,
                                    useC = FALSE) {
    ## 'phi'
    stopifnot(identical(length(phi), 1L))
    stopifnot(is.double(phi))
    stopifnot(!is.na(phi))
    stopifnot(abs(phi) <= 1)
    ## 'level'
    stopifnot(is.double(level))
    stopifnot(!any(is.na(level)))
    ## 'meanLevel'
    stopifnot(identical(length(meanLevel), 1L))
    stopifnot(is.double(meanLevel))
    stopifnot(!is.na(meanLevel))
    ## 'nAlong'
    stopifnot(identical(length(nAlong), 1L))
    stopifnot(is.integer(nAlong))
    stopifnot(!is.na(nAlong))
    stopifnot(nAlong >= 2L)
    ## 'indexClassMax'
    stopifnot(identical(length(indexClassMax), 1L))
    stopifnot(is.integer(indexClassMax))
    stopifnot(!is.na(indexClassMax))
    stopifnot(indexClassMax > 0)
    ## 'omega'
    stopifnot(identical(length(omega), 1L))
    stopifnot(is.double(omega))
    stopifnot(!is.na(omega))
    stopifnot(omega > 0)
    ## 'level', 'nAlong', 'indexClassMax'
    stopifnot(length(level) >= nAlong * indexClassMax)
    if (useC) {
        .Call(logPostPhiFirstOrderMix_R, phi, level, meanLevel, nAlong, indexClassMax, omega)
    }
    else {
        if(abs(phi) < 1) {
            ans.first <- 0
            ratio <- meanLevel / (1 - phi)
            for (i.class in seq_len(indexClassMax)) {
                i.wt.first <- (i.class - 1L) * nAlong + 1L
                level.first <- level[i.wt.first]
                ans.first <- ans.first + (level.first - ratio) * (phi * level.first + ratio)
            }
            ans.rest <- 0
            for (i.class in seq_len(indexClassMax)) {
                for (i.along in seq.int(from = 2L, to = nAlong)) {
                    i.wt.curr <- (i.class - 1L) * nAlong + i.along
                    i.wt.prev <- i.wt.curr - 1L
                    level.curr <- level[i.wt.curr]
                    level.prev <- level[i.wt.prev]
                    ans.rest <- ans.rest + level.prev * (level.curr - meanLevel - phi * level.prev)
                }
            }
            (ans.first + ans.rest) / omega^2
        }
        else {
            0.0001
        }
    }
}





## TRANSLATED
## HAS_TESTS
## assume first dimension of array that
## mx is obtained from is age
makeLifeExpBirth <- function(mx, nx, ax, iAge0, nAge,
                             useC = FALSE) {
    ## mx
    stopifnot(is.double(mx))
    stopifnot(!any(is.na(mx)))
    stopifnot(all(mx >= 0))
    ## nx
    stopifnot(is.double(nx))
    stopifnot(!any(is.na(nx)))
    stopifnot(all(nx > 0))
    ## ax
    stopifnot(is.double(ax))
    stopifnot(!any(is.na(ax)))
    stopifnot(all(ax >= 0))
    ## iAge0
    stopifnot(is.integer(iAge0))
    stopifnot(length(iAge0) == 1L)
    stopifnot(iAge0 >= 1L)
    ## nAge
    stopifnot(is.integer(nAge))
    stopifnot(length(nAge) == 1L)
    stopifnot(nAge >= 1L)
    ## mx and ax
    stopifnot(length(ax) == length(mx))
    ## ax and nx
    stopifnot(all(ax <= rep(nx, length.out = length(ax))))
    ## mx and iAge0
    stopifnot(iAge0 <= length(mx))
    ## mx and nAge
    stopifnot(nAge <= length(mx))    
    if (useC) {
        .Call(makeLifeExpBirth_R, mx, nx, ax, iAge0, nAge)
    }
    else {
        ans <- 0
        lx.i <- 1
        for (i in seq_len(nAge - 1L)) {
            mx.i <- mx[iAge0 + i - 1L]
            nx.i <- nx[i]
            ax.i <- ax[iAge0 + i - 1L]
            qx.i <- nx.i * mx.i / (1 + (nx.i - ax.i) * mx.i)
            lx.iplus1 <- lx.i * (1 - qx.i)
            Lx.i <- lx.iplus1 * nx.i + (lx.i - lx.iplus1) * ax.i
            ans <- ans + Lx.i
            lx.i <- lx.iplus1
        }
        mx.i <- mx[iAge0 + nAge - 1L]
        Lx.i <- lx.i / mx.i
        ans <- ans + Lx.i
        ans
    }
}

## TRANSLATED
## HAS_TESTS
makeVBar <- function(object, iBeta, g, useC = FALSE) {
    ## object
    stopifnot(methods::is(object, "Varying"))
    stopifnot(methods::validObject(object))
    ## iBeta
    stopifnot(is.integer(iBeta))
    stopifnot(identical(length(iBeta), 1L))
    stopifnot(!is.na(iBeta))
    stopifnot(iBeta %in% seq_along(object@betas))
    ## g
    stopifnot(is.function(g))
    if (useC) {
        .Call(makeVBar_R, object, iBeta) ## g not passed
    }
    else {
        theta <- object@theta
        betas <- object@betas
        iterator <- object@iteratorBetas
        beta <- betas[[iBeta]]
        iterator <- resetB(iterator)
        ans <- rep(0, times = length(beta))
        i.other.betas <- seq_along(betas)[-iBeta]
        for (i.mu in seq_along(theta)) {
            indices <- iterator@indices
            pos.ans <- indices[iBeta]
            ans[pos.ans] <- ans[pos.ans] + g(theta[i.mu])
            for (i.other.beta in i.other.betas) {
                other.beta <- betas[[i.other.beta]]
                pos.other.beta <- indices[i.other.beta]
                ans[pos.ans] <- ans[pos.ans] - other.beta[pos.other.beta]
            }
            iterator <- advanceB(iterator)
        }
        ans <- ans * length(ans) / length(theta)
        ans
    }
}

## TRANSLATED
## HAS_TESTS
safeLogProp_Binomial <- function(logit.th.new, logit.th.other.new,
                                 logit.th.old, logit.th.other.old,
                                 scale, weight, weight.other,
                                 useC = FALSE) {
    for (name in c("logit.th.new", "logit.th.other.new",
                   "logit.th.old", "logit.th.other.old",
                   "scale", "weight", "weight.other")) {
        value <- get(name)
        stopifnot(identical(length(value), 1L))
        stopifnot(is.double(value))
        stopifnot(!is.na(value))
    }
    stopifnot(scale > 0)
    if (useC) {
        .Call(safeLogProp_Binomial_R, logit.th.new, logit.th.other.new,
              logit.th.old, logit.th.other.old, scale,
              weight, weight.other)
    }
    else {
        if (abs(logit.th.new) > abs(logit.th.other.new)) {
            if (logit.th.new > 0) {
                outside <- logit.th.new
                coef.first <- (exp(-2 * logit.th.new)
                               + 2 * exp(-logit.th.new)
                               + 1)
                coef.second <- (exp(-logit.th.other.new - logit.th.new)
                                + 2 * exp(-logit.th.new)
                                + exp(logit.th.other.new - logit.th.new))
            }
            else {
                outside <- -logit.th.new
                coef.first <- (1
                               + 2 * exp(logit.th.new)
                               + exp(2 * logit.th.new))
                coef.second <- (exp(-logit.th.other.new + logit.th.new)
                                + 2 * exp(logit.th.new)
                                + exp(logit.th.other.new + logit.th.new))
            }
        }
        else {
            if (logit.th.other.new > 0) {
                outside <- logit.th.other.new
                coef.first <- (exp(-logit.th.new - logit.th.other.new)
                               + 2 * exp(-logit.th.other.new)
                               + exp(logit.th.new - logit.th.other.new))
                coef.second <- (exp(-2 * logit.th.other.new)
                                + 2 * exp(-logit.th.other.new)
                                + 1)
            }
            else {
                outside <- -logit.th.other.new
                coef.first <- (exp(-logit.th.new + logit.th.other.new)
                               + 2 * exp(logit.th.other.new)
                               + exp(logit.th.new + logit.th.other.new))
                coef.second <- (1
                                + 2 * exp(logit.th.other.new)
                                + exp(2 * logit.th.other.new))
            }
        }
        dens.first <- stats::dnorm(x = logit.th.new,
                            mean = logit.th.old,
                            sd = scale)
        dens.second <- stats::dnorm(x = logit.th.other.new,
                             mean = logit.th.other.old,
                             sd = scale)
        weight.ratio <- abs(weight / weight.other)
        outside + log(coef.first * dens.first
                      + weight.ratio * coef.second * dens.second)
    }
}

## TRANSLATED
## HAS_TESTS
## This function only protect against overflow due to 'theta' being too small.
## It does not protect against inaccuracies due to 'theta' being too large.
## This doesn't seem worthwhile when the chance of 'theta' being too large
## is small (typically 'theta' will be a rate), and the consequences of the
## errors are not too bad (a coefficient being 0 rather than just above 0.)
safeLogProp_Poisson <- function(log.th.new, log.th.other.new,
                                log.th.old, log.th.other.old,
                                scale, weight, weight.other,
                                useC = FALSE) {
    for (name in c("log.th.new", "log.th.other.new",
                   "log.th.old", "log.th.other.old",
                   "scale", "weight", "weight.other")) {
        value <- get(name)
        stopifnot(identical(length(value), 1L))
        stopifnot(is.double(value))
        stopifnot(!is.na(value))
    }
    stopifnot(scale > 0)
    if (useC) {
        .Call(safeLogProp_Poisson_R, log.th.new, log.th.other.new,
              log.th.old, log.th.other.old, scale,
              weight, weight.other)
    }
    else {
        if ((log.th.new < log.th.other.new) && (log.th.new < 0)) {
            outside <- -log.th.new
            coef.first <- 1
            coef.second <- exp(-log.th.other.new + log.th.new)
        }
        else if ((log.th.other.new < log.th.new) && (log.th.other.new < 0)) {
            outside <- -log.th.other.new
            coef.first <- exp(-log.th.new + log.th.other.new)
            coef.second <- 1
        }
        else {
            outside <- 0
            coef.first <- exp(-log.th.new)
            coef.second <- exp(-log.th.other.new)
        }
        dens.first <- stats::dnorm(x = log.th.new, mean = log.th.old, sd = scale)
        dens.second <- stats::dnorm(x = log.th.other.new, mean = log.th.other.old, sd = scale)
        weight.ratio <- abs(weight / weight.other)
        outside + log(coef.first * dens.first
                      + weight.ratio * coef.second * dens.second)
    }
}


## PREDICTING ######################################################################

## HAS_TESTS
checkDataPredict <- function(data) {
    if (is.null(data))
        return(NULL)
    ## has class 'list'
    if (!is.list(data))
        stop(gettextf("'%s' has class \"%s\"",
                      "data", class(data)))
    names.data <- names(data)
    ## has names
    if (is.null(names.data))
        stop(gettextf("'%s' does not have names",
                      "data"))
    ## names have no missing values
    if (any(is.na(names.data)))
        stop(gettextf("names for '%s' have missing values",
                      "data"))
    ## names have no blanks
    if (!all(nzchar(names.data)))
        stop(gettextf("names for '%s' have blanks",
                      "data"))
    ## names have no duplicates
    if (any(duplicated(names.data)))
        stop(gettextf("names for '%s' have duplicates",
                      "data"))
    ## all elements are data.frames
    for (i in seq_along(data))
        if (!methods::is(data[[i]], "data.frame"))
            stop(gettextf("item \"%s\" from '%s' has class \"%s\"",
                          names.data[i], "data", class(data[[i]])))
    NULL
}

## HAS_TESTS
initialModelPredictHelper <- function(model, along, labels, n, offsetModel,
                                      covariates) {
    theta.old <- model@theta
    metadata.first <- model@metadataY
    betas <- model@betas
    priors.betas <- model@priorsBetas
    names.betas <- model@namesBetas
    margins <- model@margins
    dims <- model@dims
    i.method.model.first <- model@iMethodModel
    n.beta <- length(betas)
    metadata.pred <- makeMetadataPredict(metadata = metadata.first,
                                         along = along,
                                         labels = labels,
                                         n = n)
    theta <- rep(mean(theta.old), times = prod(dim(metadata.pred)))
    beta.is.predicted <- logical(length = n.beta)
    for (i in seq_len(n.beta)) {
        margin <- margins[[i]]
        beta.is.predicted[i] <- along %in% margin
        if (beta.is.predicted[[i]]) {
            metadata.pred.i <- metadata.pred[margin]
            dim.i <- dim(metadata.pred.i)
            J <- prod(dim.i)
            covariates.i <- covariates[[names.betas[i]]]
            dims[[i]] <- dim.i
            betas[[i]] <- rep(0, length = J)
            along.margin <- match(along, margin)
            priors.betas[[i]] <- initialPriorPredict(prior = priors.betas[[i]],
                                                     data = covariates.i,
                                                     metadata = metadata.pred.i,
                                                     name = names.betas[i],
                                                     along = along.margin)
        }
        else {
            J <- length(betas[[i]])
            J <- methods::new("Length", J)
            priors.betas[[i]] <- methods::new("TimeInvariant", J = J)
        }
    }
    names.predicted <- names.betas[beta.is.predicted]
    names.covariates <- names(covariates)
    for (name in names.covariates) {
        if (!(name %in% names.betas))
            stop(gettextf("'%s' includes data for '%s', but '%s' is not a term in the model",
                          "covariates", name, name))
        if (!name %in% names.predicted)
            stop(gettextf("'%s' includes data for '%s', but '%s' is not predicted",
                          "covariates", name, name))
    }
    dim <- dim(metadata.pred)
    iterator.betas <- BetaIterator(dim = dim, margins = margins)
    offsets.betas <- makeOffsetsBetas(model, offsetModel = offsetModel)
    offsets.priors.betas <- makeOffsetsPriorsBetas(model, offsetModel = offsetModel)
    offsets.sigma <- makeOffsetsSigma(model, offsetModel = offsetModel)
    i.method.model <- i.method.model.first + 100L
    list(theta = theta,
         metadataY = metadata.pred,
         betas = betas,
         priorsBetas = priors.betas,
         iteratorBetas = iterator.betas,
         dims = dims,
         betaIsPredicted = beta.is.predicted,
         offsetsBetas = offsets.betas,
         offsetsPriorsBetas = offsets.priors.betas,
         offsetsSigma = offsets.sigma,
         iMethodModel = i.method.model)         
}    


## HAS_TESTS
lengthValues <- function(object) {
    if (is.numeric(object))
        length(object)
    else if (is.list(object)) {
        if (length(object) > 0L)
            sum(sapply(object, lengthValues))
        else
            0L
    }
    else {
        slots.to.extract <- object@slotsToExtract
        if (length(slots.to.extract) > 0L)
            sum(sapply(slots.to.extract, function(name) lengthValues(methods::slot(object, name))))
        else
            0L
    }
}

## HAS_TESTS
makeMetadataPredict <- function(metadata, along, labels, n) {
    names <- names(metadata)
    dimtypes <- dembase::dimtypes(metadata, use.names = FALSE)
    dimscales <- dimscales(metadata, use.names = FALSE)
    DimScales <- dembase::DimScales(metadata, use.names = FALSE)
    if (!is.null(labels)) {
        if (!is.null(n))
            warning(gettextf("'%s' ignored when '%s' provided",
                             "n", "labels"))
        labels <- as.character(labels)
        DimScale.pred <- dembase::inferDimScale(dimtype = dimtypes[along],
                                       dimscale = dimscales[along],
                                       labels = labels,
                                       name = names[along])
    }
    else {
        if (!is.null(n))
            DimScale.pred <- dembase::incrementDimScale(DimScales[[along]],
                                               n = n)
        else
            stop(gettextf("must supply '%s' or '%s' argument",
                          "labels", "n"))
    }
    DimScales[[along]] <- DimScale.pred
    methods::new("MetaData",
        nms = names,
        dimtypes = dimtypes,
        DimScales = DimScales)
}

## HAS_TESTS
makeOffsetsBetas <- function(model, offsetModel) {
    slots.to.extract <- model@slotsToExtract
    first <- offsetModel
    for (name in slots.to.extract) {
        if (identical(name, "betas")) {
            betas <- methods::slot(model, name)
            n.beta <- length(betas)
            ans <- vector(mode = "list", length = n.beta)
            for (i in seq_len(n.beta)) {
                last <- first + length(betas[[i]]) - 1L
                ans[[i]] <- methods::new("Offsets", c(first, last))
                first <- last + 1L
            }
            return(ans)
        }
        else {
            slot <- methods::slot(model, name)
            increment <- lengthValues(slot)
            first <- first + increment
        }
    }
    stop(gettextf("slot \"%s\" not found",
                  "betas"))
}

## HAS_TESTS
makeOffsetsPriorsBetas <- function(model, offsetModel) {
    slots.to.extract <- model@slotsToExtract
    first <- offsetModel
    for (name in slots.to.extract) {
        if (identical(name, "priorsBetas")) {
            priors <- methods::slot(model, name)
            n.beta <- length(priors)
            ans <- vector(mode = "list", length = n.beta)
            for (i in seq_len(n.beta)) {
                n.values <- lengthValues(priors[[i]])
                if (n.values > 0L) {
                    last <- first + n.values - 1L
                    ans[[i]] <- methods::new("Offsets", c(first, last))
                    first <- last + 1L
                }
                else
                    ans[i] <- list(NULL)
            }
            return(ans)
        }
        else {
            slot <- methods::slot(model, name)
            increment <- lengthValues(slot)
            first <- first + increment
        }
    }
    stop(gettextf("slot \"%s\" not found",
                  "priorsBetas"))
}

## HAS_TESTS
makeOffsetsSigma <- function(model, offsetModel) {
    slots.to.extract <- model@slotsToExtract
    first <- offsetModel
    for (name in slots.to.extract) {
        if (identical(name, "sigma")) {
            ans <- methods::new("Offsets", c(first, first))
            return(ans)
        }
        else {
            slot <- methods::slot(model, name)
            increment <- lengthValues(slot)
            first <- first + increment
        }
    }
    stop(gettextf("slot \"%s\" not found",
                  "sigma"))
}

## HAS_TESTS
makeOffsetsVarsigma <- function(model, offsetModel) {
    slots.to.extract <- model@slotsToExtract
    first <- offsetModel
    for (name in slots.to.extract) {
        if (identical(name, "varsigma")) {
            ans <- methods::new("Offsets", c(first, first))
            return(ans)
        }
        else {
            slot <- methods::slot(model, name)
            increment <- lengthValues(slot)
            first <- first + increment
        }
    }
    stop(gettextf("slot \"%s\" not found",
                  "varsigma"))
}

## TRANSLATED
## HAS_TESTS
predictAlphaDLMNoTrend <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "DLM"))
    stopifnot(methods::is(prior, "NoTrendMixin"))
    stopifnot(methods::validObject(prior))
    if (useC) {
        .Call(predictAlphaDLMNoTrend_R, prior)
    }
    else {
        K <- prior@K@.Data
        L <- prior@L@.Data
        alpha <- prior@alphaDLM@.Data # numeric vector length (K+1)L
        phi <- prior@phi
        omega <- prior@omegaAlpha@.Data
        iterator <- prior@iteratorState
        iterator <- resetA(iterator)
        for (l in seq_len(L)) {
            indices <- iterator@indices
            for (i in seq_len(K)) {
                k.curr <- indices[i + 1L]
                k.prev <- indices[i]
                mean <- phi * alpha[k.prev]
                alpha[k.curr] <- stats::rnorm(n = 1L,
                                       mean = mean,
                                       sd = omega)
            }
            iterator <- advanceA(iterator)
        }
        prior@alphaDLM@.Data <- alpha
        prior
    }
}

## TRANSLATED
## HAS_TESTS
predictAlphaDeltaDLMWithTrend <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "DLM"))
    stopifnot(methods::is(prior, "WithTrendMixin"))
    stopifnot(methods::validObject(prior))
    if (useC) {
        .Call(predictAlphaDeltaDLMWithTrend_R, prior)
    }
    else {
        K <- prior@K@.Data
        L <- prior@L@.Data
        alpha <- prior@alphaDLM@.Data # numeric vector length (K+1)L
        delta <- prior@deltaDLM@.Data # numeric vector length (K+1)L
        phi <- prior@phi
        omega.alpha <- prior@omegaAlpha@.Data
        omega.delta <- prior@omegaDelta@.Data
        iterator <- prior@iteratorState
        iterator <- resetA(iterator)
        for (l in seq_len(L)) {
            indices <- iterator@indices
            for (i in seq_len(K)) {
                k.curr <- indices[i + 1L]
                k.prev <- indices[i]
                mean.delta <- phi * delta[k.prev]
                delta[k.curr] <- stats::rnorm(n = 1L,
                                              mean = mean.delta,
                                              sd = omega.delta)
                mean.alpha <- alpha[k.prev] + delta[k.prev]
                alpha[k.curr] <- stats::rnorm(n = 1L,
                                              mean = mean.alpha,
                                              sd = omega.alpha)
            }
            iterator <- advanceA(iterator)
        }
        prior@alphaDLM@.Data <- alpha
        prior@deltaDLM@.Data <- delta
        prior
    }
}

## TRANSLATED
## HAS_TESTS
## This is ugly, but writing the function in a proper
## object-oriented way seemed wrong, since the best approach
## in C would probably be to write a single function.
predictBeta <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "Prior"))
    stopifnot(methods::validObject(prior))
    if (useC) {
        .Call(predictBeta_R, prior)
    }
    else {
        J <- prior@J@.Data
        ## I'm assuming the C code will use iMethodPrior
        ## to identify special cases. At present we only
        ## have one special case, but we will add at least
        ## one more ("Known").  If we forget a special case
        ## functions 'betaHat' and 'getV' below will almost
        ## certainly pick it up.
        is.exch.fixed <- methods::is(prior, "ExchFixed") 
        if (is.exch.fixed) {
            sd <- prior@tau@.Data
            stats::rnorm(n = J, mean = 0, sd = sd)
        }
        else {
            mean <- betaHat(prior)
            var <- getV(prior)
            sd <- sqrt(var)
            stats::rnorm(n = J, mean = mean, sd = sd)
        }
    }
}

## TRANSLATED
## HAS_TESTS
predictBetas <- function(object, useC = FALSE) {
    stopifnot(methods::is(object, "Varying"))
    stopifnot(methods::is(object, "BetaIsPredicted"))
    methods::validObject(object)
    if (useC) {
        .Call(predictBetas_R, object)
    }
    else {
        betas <- object@betas
        priors <- object@priorsBetas
        beta.is.predicted <- object@betaIsPredicted
        for (i in seq_along(betas)) {
            if (beta.is.predicted[i])
                betas[[i]] <- predictBeta(priors[[i]])
        }
        object@betas <- betas
        object
    }
}

## HAS_TESTS
predictOneChain <- function(combined, tempfileOld, tempfileNew,
                            lengthIter, nIteration, nUpdate) {
    con <- file(tempfileNew, open = "wb")
    on.exit(close(con))
    for (iteration in seq_len(nIteration)) {
        combined <- predictCombined(object = combined,
                                    nUpdate = nUpdate,
                                    filename = tempfileOld,
                                    lengthIter = lengthIter,
                                    iteration = iteration,
                                    useC = TRUE)
        values <- extractValues(combined)
        writeBin(object = values, con = con)
    }
    combined
}

## TRANSLATED
## HAS_TESTS
predictPriorsBetas <- function(object, useC = FALSE) {
    stopifnot(methods::is(object, "Varying"))
    stopifnot(methods::is(object, "BetaIsPredicted"))
    methods::validObject(object)
    if (useC) {
        .Call(predictPriorsBetas_R, object)
    }
    else {
        priors <- object@priorsBetas
        beta.is.predicted <- object@betaIsPredicted
        for (i in seq_along(priors)) {
            if (beta.is.predicted[i])
                priors[[i]] <- predictPrior(prior = priors[[i]])
        }
        object@priorsBetas <- priors
        object
    }
}

## TRANSLATED
## HAS_TESTS
predictSeason <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "DLM"))
    stopifnot(methods::is(prior, "SeasonMixin"))
    stopifnot(methods::validObject(prior))
    if (useC) {
        .Call(predictSeason_R, prior)
    }
    else {
        K <- prior@K@.Data
        L <- prior@L@.Data
        s <- prior@s@.Data # list length (K+1)L
        n.season <- prior@nSeason@.Data
        omega <- prior@omegaSeason@.Data
        iterator <- prior@iteratorState
        iterator <- resetA(iterator)
        for (l in seq_len(L)) {
            indices <- iterator@indices
            for (i in seq_len(K)) {
                k.curr <- indices[i + 1L]
                k.prev <- indices[i]
                mean <- s[[k.prev]][n.season]
                s[[k.curr]][1L] <- stats::rnorm(n = 1L,
                                                mean = mean,
                                                sd = omega)
                for (j in seq.int(from = 2L, to = n.season))
                    s[[k.curr]][j] <- s[[k.prev]][j - 1L]
            }
            iterator <- advanceA(iterator)
        }
        prior@s@.Data <- s
        prior
    }
}


## TRANSLATED
## HAS_TESTS
predictUBeta <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "Prior"))
    stopifnot(methods::validObject(prior))
    stopifnot(prior@isRobust@.Data)
    if (useC) {
        .Call(predictUBeta_R, prior)
    }
    else {
        J <- prior@J@.Data
        nu <- prior@nuBeta@.Data
        tau <- prior@tau@.Data
        U <- prior@UBeta@.Data
        scale <- tau^2
        for (j in seq_len(J))
            U[j] <- rinvchisq1(df = nu, scale = scale)
        prior@UBeta@.Data <- U
        prior
    }
}



## redistributeNotSpecified <- function(y, notSpec, notSpecRedist, transform, model) {
##     for (i in seq_along(y)) {
##         i.not.spec <- dembase::getIBefore(i = i,
##                                             transform = transform,
##                                             useC = TRUE)
##         can.have.not.spec <- i.not.spec > 0L
##         if (can.have.not.spec) {
##             i.oth <- getIOther(i = i,
##                                transform = transform,
##                                useC = TRUE)
##             not.spec.redist.curr <- notSpecRedist[i]
##             not.spec.redist.curr.oth <- notSpecRedit[i.oth]
##             sum.not.spec.redist <- not.spec.redist.curr + not.spec.redist.curr.oth
##             if (sum.not.spec.redist == 0L)
##         }
##     }
## }    
              
    

    


## HAS_TESTS
splitFile <- function(filename, nChain, nIteration, lengthIter) {
    con.read <- file(filename, "rb")
    on.exit(close(con.read))
    n.row.file <- nIteration / nChain
    if (n.row.file != round(n.row.file))
        stop(gettextf("'%s' is not a multiple of '%s'",
                      "nIteration", "nChain"))
    size.results <- readBin(con = con.read, what = "integer", n = 1L)
    for (j in seq_len(size.results))
        readBin(con = con.read, what = "raw", n = 1L)
    for (i in seq_len(nChain)) {
        tempfile <- paste(filename, i, sep = "_")
        con.write <- file(tempfile, "wb")
        for (j in seq_len(n.row.file)) {
            object <- readBin(con = con.read, what = "double", n = lengthIter)
            writeBin(object, con = con.write)
        }
        close(con.write)
    }
    paste(filename, seq_len(nChain), sep = "_")
}

## TRANSLATED
## HAS_TESTS
transferAlphaDelta0 <- function(state, values, offset, iteratorNew, iteratorOld,
                                useC = FALSE) {
    ## state
    stopifnot(is.double(state))
    stopifnot(!any(is.na(state)))
    ## values
    stopifnot(is.double(values))
    stopifnot(!any(is.na(values)))
    ## offset
    stopifnot(is.integer(offset))
    stopifnot(identical(length(offset), 1L))
    stopifnot(!is.na(offset))
    ## iteratorNew
    stopifnot(methods::is(iteratorNew, "AlongIterator"))
    ## iteratorOld
    stopifnot(methods::is(iteratorOld, "AlongIterator"))
    ## offset, values
    stopifnot(offset %in% seq_along(values))
    if (useC) {
        .Call(transferAlphaDelta0_R, state, values, offset, iteratorNew, iteratorOld)
    }
    else {    
        iterator.values <- resetA(iteratorOld)
        iterator.state <- resetA(iteratorNew)
        n <- iterator.values@nWithin * iterator.values@nBetween
        Kplus1.values <- length(iterator.values@indices)
        for (i in seq_len(n)) {
            indices.values <- iterator.values@indices
            indices.values <- indices.values + offset - 1L
            indices.state <- iterator.state@indices
            i.first.state <- indices.state[1L]
            i.last.values <- indices.values[Kplus1.values]
            state[i.first.state] <- values[i.last.values]
            iterator.values <- advanceA(iterator.values)
            iterator.state <- advanceA(iterator.state)
        }
        state
    }
}

## TRANSLATED
## HAS_TESTS
transferParamBetas <- function(model, filename, lengthIter, iteration,
                               useC = FALSE) {
    ## model
    stopifnot(methods::is(model, "BetaIsPredicted"))
    ## filename
    stopifnot(is.character(filename))
    ## lengthIter
    stopifnot(is.integer(lengthIter))
    stopifnot(identical(length(lengthIter), 1L))
    stopifnot(!is.na(lengthIter))
    stopifnot(lengthIter > 0L)
    ## iteration
    stopifnot(is.integer(iteration))
    stopifnot(identical(length(iteration), 1L))
    stopifnot(!is.na(iteration))
    stopifnot(iteration > 0L)
    if (useC) {
        .Call(transferParamBetas_R, model, filename, lengthIter, iteration)
    }
    else {
        betas <- model@betas
        beta.is.predicted <- model@betaIsPredicted
        offsets.betas <- model@offsetsBetas
        n.beta <- length(betas)
        for (i in seq_len(n.beta)) {
            if (!beta.is.predicted[[i]]) {
                offsets <- offsets.betas[[i]]
                betas[[i]] <- getOneIterFromFile(filename = filename,
                                                 first = offsets[1L],
                                                 last = offsets[2L],
                                                 lengthIter = lengthIter,
                                                 iteration = iteration,
                                                 useC = FALSE)
            }
        }
        model@betas <- betas
        model
    }
}

## TRANSLATED
## HAS_TESTS
transferParamPriorsBetas <- function(model, filename, lengthIter, iteration,
                                     useC = FALSE) {
    ## model
    stopifnot(methods::is(model, "Betas"))
    ## filename
    stopifnot(is.character(filename))
    ## lengthIter
    stopifnot(is.integer(lengthIter))
    stopifnot(identical(length(lengthIter), 1L))
    stopifnot(!is.na(lengthIter))
    stopifnot(lengthIter > 0L)
    ## iteration
    stopifnot(is.integer(iteration))
    stopifnot(identical(length(iteration), 1L))
    stopifnot(!is.na(iteration))
    stopifnot(iteration > 0L)
    if (useC) {
        .Call(transferParamPriorsBetas_R, model, filename, lengthIter, iteration)
    }
    else {
        priors <- model@priorsBetas
        offsets.priors <- model@offsetsPriorsBetas
        is.predicted <- model@betaIsPredicted  ## NEW
        n.beta <- length(priors)
        for (i in seq_len(n.beta)) {
            if (is.predicted[i]) {  ## NEW
                offsets <- offsets.priors[[i]]
                if (!is.null(offsets)) {
                    first <- offsets[1L]
                    last <- offsets[2L]
                    values <- getOneIterFromFile(filename = filename,
                                                 first = first,
                                                 last = last,
                                                 lengthIter = lengthIter,
                                                 iteration = iteration)
                    priors[[i]] <- transferParamPrior(prior = priors[[i]],
                                                      values = values)
                }
            } ## NEW
        }
        model@priorsBetas <- priors
        model
    }
}

## TRANSLATED
## HAS_TESTS
transferParamSigma <- function(model, filename, lengthIter, iteration, useC = FALSE) {
    ## model
    stopifnot(methods::is(model, "SigmaMixin"))
    ## filename
    stopifnot(is.character(filename))
    ## lengthIter
    stopifnot(is.integer(lengthIter))
    stopifnot(identical(length(lengthIter), 1L))
    stopifnot(!is.na(lengthIter))
    stopifnot(lengthIter > 0L)
    ## iteration
    stopifnot(is.integer(iteration))
    stopifnot(identical(length(iteration), 1L))
    stopifnot(!is.na(iteration))
    stopifnot(iteration > 0L)
    if (useC) {
        .Call(transferParamSigma_R, model, filename, lengthIter, iteration)
    }
    else {
        offsets <- model@offsetsSigma
        first <- offsets[1L]
        last <- offsets[2L]
        sigma <- getOneIterFromFile(filename = filename,
                                    first = first,
                                    last = last,
                                    lengthIter = lengthIter,
                                    iteration = iteration)
        model@sigma@.Data <- sigma
        model
    }
}

## TRANSLATED
## HAS_TESTS
transferParamVarsigma <- function(model, filename, lengthIter, iteration, useC = FALSE) {
    ## model
    stopifnot(methods::is(model, "VarsigmaMixin"))
    ## filename
    stopifnot(is.character(filename))
    ## lengthIter
    stopifnot(is.integer(lengthIter))
    stopifnot(identical(length(lengthIter), 1L))
    stopifnot(!is.na(lengthIter))
    stopifnot(lengthIter > 0L)
    ## iteration
    stopifnot(is.integer(iteration))
    stopifnot(identical(length(iteration), 1L))
    stopifnot(!is.na(iteration))
    stopifnot(iteration > 0L)
    if (useC) {
        .Call(transferParamVarsigma_R, model, filename, lengthIter, iteration)
    }
    else {
        offsets <- model@offsetsVarsigma
        first <- offsets[1L]
        last <- offsets[2L]
        varsigma <- getOneIterFromFile(filename = filename,
                                       first = first,
                                       last = last,
                                       lengthIter = lengthIter,
                                       iteration = iteration)
        model@varsigma@.Data <- varsigma
        model
    }
}

## TRANSLATED
## HAS_TESTS
transferSeason0 <- function(s, nSeason, values, offset, iteratorNew, iteratorOld,
                            useC = FALSE) {
    ## s
    stopifnot(is.list(s))
    stopifnot(all(sapply(s, is.double)))
    stopifnot(all(sapply(s, function(x) !any(is.na(x)))))
    ## nSeason
    stopifnot(is.integer(nSeason))
    stopifnot(identical(length(nSeason), 1L))
    ## values
    stopifnot(is.double(values))
    stopifnot(!any(is.na(values)))
    ## offset
    stopifnot(is.integer(offset))
    stopifnot(identical(length(offset), 1L))
    stopifnot(!is.na(offset))
    ## iteratorNew
    stopifnot(methods::is(iteratorNew, "AlongIterator"))
    ## iteratorOld
    stopifnot(methods::is(iteratorOld, "AlongIterator"))
    ## s, nSeason
    stopifnot(all(sapply(s, length) == nSeason))
    ## s, values
    stopifnot(length(s) < length(values))
    ## offset, values
    stopifnot(offset %in% seq_along(values))
    if (useC) {
        .Call(transferSeason0_R, s, nSeason, values, offset, iteratorNew, iteratorOld)
    }
    else {    
        iterator.values <- resetA(iteratorOld)
        iterator.s <- resetA(iteratorNew)
        n <- iterator.values@nWithin * iterator.values@nBetween
        Kplus1.values <- length(iterator.values@indices)
        for (i in seq_len(n)) {
            indices.values <- iterator.values@indices
            indices.s <- iterator.s@indices
            i.first.s <- indices.s[1L]
            i.last.values <- (indices.values[Kplus1.values] - 1L) * nSeason + offset
            for (j in seq_len(nSeason))
                s[[i.first.s]][j] <- values[i.last.values + j - 1L]
            iterator.values <- advanceA(iterator.values)
            iterator.s <- advanceA(iterator.s)
        }
        s
    }
}


## MONITORING ######################################################################

## JAH note: I am not sure if we translate this.  At present the
## write_to_file function in C combines extracting and writing and
## I think is much more efficient like that for C purposes even though
## it is a bit ugly from a design point of view.  What I'd really like
## is a more efficient way of getting at the slots to extract in C.
## HAS_TESTS
extractValues <- function(object) {
    if (is.numeric(object)) {
        object
    }
    else if (is.list(object)) {
        unlist(lapply(object, extractValues))
    }
    else {
        slots.to.extract <- object@slotsToExtract
        n <- length(slots.to.extract)
        if (n > 0L) {
            ans <- vector(mode = "list", length = n)
            for (i in seq_along(ans)) {
                obj <- methods::slot(object, slots.to.extract[i])
                ans[[i]] <- extractValues(obj)
            }
            unlist(ans)
        }
        else
            numeric()
    }
}

## HAS_TESTS
## Helper function for 'sweepAllMargins'.
sweepMargins <- function(x, margins) {
    dim <- dim(x)
    s <- seq_along(dim)
    seqs <- lapply(dim, seq_len)
    ones <- lapply(dim, function(times) rep(1L, times = times))
    for (margin in margins) {
        along <- s[-margin]
        indices <- replace(seqs, list = along,  values = ones[along])
        dims <- match(s, margin, nomatch = 0L)
        dim.margin <- dim[margin]
        collapse <- methods::new("CollapseTransform",
                        indices = indices,
                        dims = dims,
                        dimBefore = dim,
                        dimAfter = dim.margin)
        extend <- methods::new("ExtendTransform",
                      indices = indices,
                      dims = dims,
                      dimBefore = dim.margin,
                      dimAfter = dim)
        collapsed <- dembase::collapse(x, transform = collapse)
        means <- collapsed / prod(dim[along])
        means <- extend(means, transform = extend)
        x <- x - means
    }
    x
}



## UPDATING COUNTS ##################################################################

## 'diffLogLik' has special provisions for infinite values.
## log-likelihood of -Inf can occur fairly frequently with
## sparse data.  When the observation model is binomial, it occurs
## whenever the cell in 'dataset' has a higher value than the
## sum of the corresponding cell(s) from 'y'.  When the
## observation model is Poisson or a Poisson-binomial mixture,
## it occurs whenever the cell in 'dataset' has a value > 0
## but the corresponding cell(s)
## in 'y' are all 0. Whether testing and allowing for an
## early exit from the function speeds things up depends on the
## sparseness of the data and the speed of testing
## vs calculating likelihoods.  However, I quite like the
## idea of explicitly testing to emphasize that we need
## to deal with -Inf.

## TRANSLATED
## HAS_TESTS
diffLogLik <- function(yProp, y, indicesY, observation,
                       datasets, transforms, useC = FALSE) {
    ## yProp
    stopifnot(is.integer(yProp))
    stopifnot(!any(is.na(yProp)))
    stopifnot(all(yProp >= 0))
    ## y
    stopifnot(methods::is(y, "Counts"))
    stopifnot(is.integer(y))
    stopifnot(!any(is.na(y)))
    stopifnot(all(y >= 0))
    ## indicesY
    stopifnot(is.integer(indicesY))
    stopifnot(!any(is.na(indicesY)))
    stopifnot(all(indicesY >= 1L))
    ## observation
    stopifnot(is.list(observation))
    stopifnot(all(sapply(observation, methods::is, "Model")))
    stopifnot(all(sapply(observation, methods::is, "UseExposure")))
    ## datasets
    stopifnot(is.list(datasets))
    stopifnot(all(sapply(datasets, methods::is, "Counts")))
    stopifnot(all(sapply(datasets, is.integer)))
    stopifnot(all(sapply(datasets, function(x) all(x[!is.na(x)] >= 0))))
    ## transforms
    stopifnot(is.list(transforms))
    stopifnot(all(sapply(transforms, methods::is, "CollapseTransformExtra")))
    ## yProp and indicesY
    stopifnot(identical(length(yProp), length(indicesY)))
    ## y and indicesY
    stopifnot(all(indicesY <= length(y)))
    ## y and transforms
    for (i in seq_along(transforms))
        stopifnot(identical(transforms[[i]]@dimBefore, dim(y)))
    ## observation and datasets
    stopifnot(identical(length(observation), length(datasets)))
    ## observation and transforms
    stopifnot(identical(length(observation), length(transforms)))
    ## datasets and transforms
    for (i in seq_along(datasets))
        stopifnot(identical(transforms[[i]]@dimAfter, dim(datasets[[i]])))
    if (useC) {
        .Call(diffLogLik_R, yProp, y, indicesY, observation,
              datasets, transforms)
    }
    else {
        ans <- 0
        n.element.indices.y <- length(indicesY)
        n.dataset <- length(datasets)
        i.element.indices.y <- 1L
        ans.infinite <- FALSE
        while ((i.element.indices.y <= n.element.indices.y) && !ans.infinite) {
            i.cell.y <- indicesY[i.element.indices.y]
            if (yProp[i.element.indices.y] != y[i.cell.y]) {
                i.dataset <- 1L
                while ((i.dataset <= n.dataset) && !ans.infinite) {
                    transform <- transforms[[i.dataset]]
                    i.cell.dataset <- dembase::getIAfter(i.cell.y, transform = transform)
                    if (i.cell.dataset > 0L) {
                        dataset <- datasets[[i.dataset]]
                        ## THE FOLLOWING LINE AND THE TEST FOR CELL OBSERVED ARE NEW
                        cell.observed <- !is.na(dataset[i.cell.dataset])
                        if (cell.observed) {
                            model <- observation[[i.dataset]]
                            i.contrib.to.cell <- dembase::getIShared(i = i.cell.y, transform = transform)
                            collapsed.y.curr <- sum(y[i.contrib.to.cell])
                            diff.prop.curr <- yProp[i.element.indices.y] - y[i.cell.y]
                            collapsed.y.prop <- collapsed.y.curr + diff.prop.curr
                            log.lik.prop <- logLikelihood(model = model,
                                                          count = collapsed.y.prop,
                                                          dataset = dataset,
                                                          i = i.cell.dataset)
                            if (is.infinite(log.lik.prop)) {
                                ans <- -Inf
                                ans.infinite <- TRUE
                                break
                            }
                            log.lik.curr <- logLikelihood(model = model,
                                                          count = collapsed.y.curr,
                                                          dataset = dataset,
                                                          i = i.cell.dataset)
                            ans <- ans + log.lik.prop - log.lik.curr
                        }
                    }
                    i.dataset <- i.dataset + 1L
                }
            }
            i.element.indices.y <- i.element.indices.y + 1L
        }
        ans
    }
}

## TRANSLATED
## HAS_TESTS
## Calling function should test that dataset[i] is not missing
logLikelihood_Binomial <- function(model, count, dataset, i, useC = FALSE) {
    ## model
    stopifnot(methods::is(model, "Model"))
    stopifnot(methods::is(model, "Binomial"))
    ## count
    stopifnot(identical(length(count), 1L))
    stopifnot(is.integer(count))
    stopifnot(!is.na(count))
    stopifnot(count >= 0L)
    ## dataset
    stopifnot(is.integer(dataset))
    stopifnot(all(dataset[!is.na(dataset)] >= 0L))
    ## i
    stopifnot(identical(length(i), 1L))
    stopifnot(is.integer(i))
    stopifnot(!is.na(i))
    stopifnot(i >= 1L)
    ## dataset and i
    stopifnot(i <= length(dataset))
    stopifnot(!is.na(dataset@.Data[i]))
    ## model and dataset
    stopifnot(identical(length(model@theta), length(dataset)))
    ## model and i
    stopifnot(i <= length(model@theta))
    if (useC) {
        .Call(logLikelihood_Binomial_R, model, count, dataset, i)
    }
    else {
        x <- dataset[[i]]
        prob <- model@theta[i]
        stats::dbinom(x = x, size = count, prob = prob, log = TRUE)
    }
}


## TRANSLATED
## HAS_TESTS
## Calling function should test that dataset[i] is not missing
logLikelihood_Poisson <- function(model, count, dataset, i, useC = FALSE) {
    ## model
    stopifnot(methods::is(model, "Model"))
    stopifnot(methods::is(model, "UseExposure"))
    ## count
    stopifnot(identical(length(count), 1L))
    stopifnot(is.integer(count))
    stopifnot(!is.na(count))
    stopifnot(count >= 0L)
    ## dataset
    stopifnot(is.integer(dataset))
    stopifnot(all(dataset[!is.na(dataset)] >= 0L))
    ## i
    stopifnot(identical(length(i), 1L))
    stopifnot(is.integer(i))
    stopifnot(!is.na(i))
    stopifnot(i >= 1L)
    ## dataset and i
    stopifnot(i <= length(dataset))
    stopifnot(!is.na(dataset@.Data[i]))
    ## model and dataset
    stopifnot(identical(length(model@theta), length(dataset)))
    ## model and i
    stopifnot(i <= length(model@theta))
    if (useC) {
        .Call(logLikelihood_Poisson_R, model, count, dataset, i)
    }
    else {
        x <- dataset[[i]]
        rate <- model@theta[i]
        lambda <- rate * count
        stats::dpois(x = x, lambda = lambda, log = TRUE)
    }
}

## TRANSLATED
## HAS_TESTS
## Calling function should test that dataset[i] is not missing
logLikelihood_PoissonBinomialMixture <- function(model, count, dataset, i, useC = FALSE) {
    ## model
    stopifnot(methods::is(model, "Model"))
    stopifnot(methods::is(model, "PoissonBinomialMixture"))
    ## count
    stopifnot(identical(length(count), 1L))
    stopifnot(is.integer(count))
    stopifnot(!is.na(count))
    stopifnot(count >= 0L)
    ## dataset
    stopifnot(is.integer(dataset))
    stopifnot(all(dataset[!is.na(dataset)] >= 0L))
    ## i
    stopifnot(identical(length(i), 1L))
    stopifnot(is.integer(i))
    stopifnot(!is.na(i))
    stopifnot(i >= 1L)
    ## dataset and i
    stopifnot(i <= length(dataset))
    stopifnot(!is.na(dataset@.Data[i]))
    if (useC) {
        .Call(logLikelihood_PoissonBinomialMixture_R, model, count, dataset, i)
    }
    else {
        x <- dataset[[i]]
        prob <- model@prob
        dpoibin1(x = x, size = count, prob = prob, log = TRUE)
    }
}


## TRANSLATED
## HAS_TESTS
## Given the position of a cell, and the transform from the object
## to the subtotals, 'makeIOther' returns the position of another cell
## that belongs to the same subtotal.  If the cell is the only cell
## included in the subtotal, then a value of 0L is returned. If the
## cell does not belong to any subtotal, a value of -1L is returned.
makeIOther <- function(i, transform, useC = FALSE) {
    ## i
    stopifnot(identical(length(i), 1L))
    stopifnot(is.integer(i))
    stopifnot(!is.na(i))
    stopifnot(i >= 1L)
    ## transform
    stopifnot(methods::is(transform, "CollapseTransformExtra"))
    ## i and transform
    stopifnot(i <= prod(transform@dimBefore))
    if (useC) {
        .Call(makeIOther_R, i, transform)
    }
    else {
        i.shared <- dembase::getIShared(i = i, transform = transform)
        n.shared <- length(i.shared)
        if (n.shared == 0L)
            -1L
        else if (n.shared == 1L)
            0L
        else {
            ## draw pair in way that can be replicated in C
            which.self <- which(i.shared == i)
            ## select 'which.shared' from 1, ..., n.shared-1
            which.shared <- as.integer(stats::runif(n = 1L) * (n.shared - 1L)) + 1L
            if (which.shared == n.shared) ## in case stats::runif(n = 1L) == 1.0
                which.shared = n.shared - 1L;
            ## if which.shared < which.self, leave as is; otherwise add 1
            if (which.shared >= which.self)
                which.shared <- which.shared + 1L
            i.shared[which.shared]
        }
    }
}



## ESTIMATION #######################################################################


## HAS_TESTS
joinFiles <- function(filenamesFirst, filenamesLast) {
    kLength <- 10000
    if (length(filenamesFirst) != length(filenamesLast))
        stop(gettextf("'%s' and '%s' have different lengths",
                      "filenamesFirst", "filenamesLast"))
    for (i in seq_along(filenamesFirst)) {
        con.append <- file(filenamesFirst[i], "ab")
        con.read <- file(filenamesLast[i], "rb")
        finished <- FALSE
        while (!finished) {
            object <- readBin(con = con.read, what = "double", n = kLength)
            writeBin(object, con = con.append)
            finished <- length(object) < kLength
        }
        close(con.append)
        close(con.read)
        unlink(filenamesLast[i])
    }
    NULL
}

## NO_TESTS
estimateOneChain <- function(combined, seed, tempfile, nBurnin, nSim, nThin,
                             continuing, ...) {
    ## set seed if continuing
    if (!is.null(seed))
        assign(".Random.seed", seed, envir = .GlobalEnv)
    ## burnin
    combined <- updateCombined(combined, nUpdate = nBurnin, useC = TRUE)
    ## production
    con <- file(tempfile, open = "wb")
    n.prod <- nSim %/% nThin
    for (i in seq_len(n.prod)) {
        combined <- updateCombined(combined, nUpdate = nThin, useC = TRUE)
        values <- extractValues(combined)
        writeBin(values, con = con)
    }
    close(con)
    ## return final state
    combined
}

## estimate one Chain with a max number of updates in any one .Call
estimateOneChainNew <- function(combined, seed, tempfile, nBurnin, nSim, nThin,
                             continuing, ...) {
    nUpdatesMax <- 200L ## this should really be something like a parameter to the function
    ## set seed if continuing
    if (!is.null(seed))
        assign(".Random.seed", seed, envir = .GlobalEnv)
    ## burnin
    ##combined <- updateCombined(combined, nUpdate = nBurnin, useC = TRUE)
    nLoops <- nBurnin %/% nUpdatesMax
    for (i in seq_len(nLoops)) {
        combined <- updateCombined(combined, nUpdate = nUpdatesMax, useC = TRUE)
    }
    ## and any final ones
    nLeftOver <- nBurnin - nLoops * nUpdatesMax
    combined <- updateCombined(combined, nUpdate = nLeftOver, useC = TRUE)
    ## production
    con <- file(tempfile, open = "wb")
    n.prod <- nSim %/% nThin
    for (i in seq_len(n.prod)) {
        nLoops <- nThin %/% nUpdatesMax
        for (i in seq_len(nLoops)) {
           combined <- updateCombined(combined, nUpdate = nUpdatesMax, useC = TRUE)
        }
        ## and any final ones
        nLeftOver <- nThin - nLoops * nUpdatesMax
        combined <- updateCombined(combined, nUpdate = nLeftOver, useC = TRUE)
        #combined <- updateCombined(combined, nUpdate = nThin, useC = TRUE)
        values <- extractValues(combined)
        writeBin(values, con = con)
    }
    close(con)
    ## return final state
    combined
}


## estimateOneChainInC <- function(combined, tempfile, nBurnin, nSim, nThin,
##                              continuing = FALSE) {
##     return.value <- tryCatch(.Call(estimateOneChain_R, combined, tempfile, nBurnin, nSim, nThin, continuing),
##                                          error = function(e) e)
##     if (methods::is(return.value, "error")) {
##         print("error")
##         print(return.value)
##         NULL
##     }
##     else {
##         return.value
##     }
## }

## estimateOneChainAllInR <- function(combined, tempfile, nBurnin, nSim, nThin,
##                              continuing = FALSE, nAttempt, wait) {
##     con <- file(tempfile, open = "wb")
##     on.exit(close(con))
##     n.prod <- nSim %/% nThin
##     if (nBurnin > 0L)
##         combined <- updateCombined(combined, nUpdate = nBurnin - 1L)
##     for (i in seq_len(n.prod)) {
##         ## using R for updateCombined
##         if ((nBurnin == 0L) && (i == 1L) && !continuing)
##             combined <- updateCombined(combined, nUpdate = nThin - 1L)
##         else
##             combined <- updateCombined(combined, nUpdate = nThin)
##         values <- extractValues(combined)
##         wrote.values <- FALSE
##         for (attempt in seq_len(nAttempt)) {
##             return.value <- tryCatch(writeBin(values, con = con),
##                                      error = function(e) e)
##             if (methods::is(return.value, "error")) {
##                 if (attempt < nAttempt)
##                     Sys.sleep(time = wait)
##             }
##             else {
##                 wrote.values <- TRUE
##                 break
##             }
##         }
##         if (!wrote.values)
##             break
##     }
##     ## if completed successfully, return final combined; otherwise return NULL
##     if (wrote.values)
##         combined
##     else {
##         warning(gettextf("a chain has failed because unable to write to file \"%\"",
##                          tempfile))
##         NULL
##     }
## }

## HAS_TESTS
finalMessage <- function(filename, verbose) {
    if (verbose)
        message(gettextf("results contained in file \"%s\"", filename))
    else
        invisible(NULL)
}

## HAS_TESTS
makeControlArgs <- function(call, parallel) {
    ## call is 'call'
    if (!is.call(call))
        stop(gettextf("'%s' does not have class \"%s\"",
                      "call", "call"))
    ## parallel is logical
    if (!is.logical(parallel))
        stop(gettextf("'%s' does not have type \"%s\"",
                      "parallel", "logical"))
    ## 'parallel' has length 1
    if (!identical(length(parallel), 1L))
        stop(gettextf("'%s' does not have length %d",
                      "parallel", 1L))
    ## 'parallel' is not missing
    if (is.na(parallel))
        stop(gettextf("'%s' is missing",
                      "parallel"))
    list(call = call,
         parallel = parallel,
         lengthIter = NULL)
}

## HAS_TESTS
makeMCMCArgs <- function(nBurnin, nSim, nChain, nThin) {
    for (name in c("nBurnin", "nSim", "nChain", "nThin")) {
        value <- get(name)
        ## length 1
        if (!identical(length(value), 1L))
            stop(gettextf("'%s' does not have length %d",
                          name, 1L))
        ## is not missing
        if (is.na(value))
            stop(gettextf("'%s' is missing",
                          name))
        ## is numeric
        if (!is.numeric(value))
            stop(gettextf("'%s' is non-numeric",
                          name))
        ## is integer
        if (round(value) != value)
            stop(gettextf("'%s' has non-integer value",
                          name))
        assign(name, value = as.integer(value))
    }
    ## 'nBurnin', nSim non-negative
    for (name in c("nBurnin", "nSim")) {
        value <- get(name)
        if (value < 0L)
            stop(gettextf("'%s' is negative",
                          name))
    }
    ## 'nChain', 'nThin' positive
    for (name in c("nChain", "nThin")) {
        value <- get(name)
        if (value < 1L)
            stop(gettextf("'%s' is less than %d",
                          name, 1L))
    }
    ## nThin <= nSim if nSim positive
    if ((nSim > 0L) && (nThin > nSim))
        stop(gettextf("'%s' is greater than '%s'",
                      "nThin", "nSim"))
    list(nBurnin = nBurnin,
         nSim = nSim,
         nChain = nChain,
         nThin = nThin)
}




## CONSTRUCT RESULTS ####################################################################

## HAS_TESTS
## Should perhaps be turned into methods,
## but it seems like overkill
changeInPos <- function(object) {
    if (methods::is(object, "SkeletonOne"))
        1L
    else if (methods::is(object, "SkeletonMany"))
        object@last - object@first + 1L
    else if (is.list(object)) {
        total <- 0L
        for (i in seq_along(object))
            total <- total + Recall(object[[i]])
        total
    }
    else
        0L
}

## HAS_TESTS
indicesShow <- function(iterator, nSeason = NULL) {
    if (!methods::is(iterator, "AlongIterator"))
        stop(gettextf("'%s' has class \"%s\"",
                      "iterator", class(iterator)))
    n.within <- iterator@nWithin
    n.between <- iterator@nBetween
    n.indices <- length(iterator@indices)
    n <- n.within * n.between
    ans <- vector(mode = "list", length = n)
    iterator <- resetA(iterator)
    for (i in seq_len(n)) {
        indices <- iterator@indices
        indices <- indices[-1]
        if (!is.null(nSeason))
            indices <- (indices - 1L) * nSeason + 1L
        ans[[i]] <- indices
        iterator <- advanceA(iterator, useC = TRUE)
    }
    ans <- unlist(ans)
    ans <- sort(ans)
    ans
}

## HAS_TESTS
makeOutputMCMC <- function(mcmcArgs, finalCombineds) {
    if (!identical(length(finalCombineds), mcmcArgs$nChain))
        stop(gettextf("length of '%s' [%d] not equal to '%s' argument [%d]",
                      "finalCombineds", length(finalCombineds), "nChain", mcmcArgs$nChain))
    n.sim <- mcmcArgs$nSim
    n.thin <- mcmcArgs$nThin
    n.chain <- mcmcArgs$nChain
    n.iteration <- as.integer(n.chain * (n.sim %/% n.thin))
    c(nBurnin = mcmcArgs$nBurnin,
      nSim = n.sim,
      nChain = n.chain,
      nThin = n.thin,
      nIteration = n.iteration)
}

## HAS_TESTS
makeOutputPriorCoef <- function(Z, pos) {
    names.eta <- colnames(Z)
    first <- pos
    ## values written to file include intercept,
    ## even though metadata does not include
    last <- pos + length(names.eta) - 1L
    dimvalues <- names.eta[-1L] # omit intercept
    DimScales <- list(methods::new("Categories", dimvalues = dimvalues))
    metadata <- methods::new("MetaData",
                    nms = "coef",
                    dimtypes = "state",
                    DimScales = DimScales)
    methods::new("SkeletonCovariates",
        first = first,
        last = last,
        metadata = metadata)
}

## NO_TESTS
makeOutputPriorDamp <- function(pos) {
    Skeleton(first = pos)
}

## HAS_TESTS
makeOutputPriorScale <- function(pos) {
    Skeleton(first = pos)
}

## HAS_TESTS
makeOutputLevelDLM <- function(iterator, metadata, nSeason, iAlong, pos, firstSeason) {
    indices <- iterator@indices
    n.within <- iterator@nWithin
    n.between <- iterator@nBetween
    length <- length(indices) * n.within * n.between
    first <- pos
    last <- pos + length - 1L
    indices.show <- indicesShow(iterator,
                                nSeason = NULL)
    has.season.effect <- nSeason > 1L
    if (has.season.effect) {
        length.season <- length * nSeason
        last.season <- firstSeason + length.season - 1L
    }
    else {
        if (!identical(firstSeason, 0L))
            stop(gettextf("'%s' equals %d but '%s' does not equal %d",
                          "nSeason", 1L, "firstSeason", 0L))
        last.season <- 0L
    }
    methods::new("SkeletonLevelDLM",
                 first = first,
                 last = last,
                 firstSeason = firstSeason,
                 lastSeason = last.season,
                 indicesShow = indices.show,
                 iAlong = iAlong,
                 nSeason = nSeason,
                 metadata = metadata)
}

## HAS_TESTS
makeOutputTrendDLM <- function(iterator, metadata, pos) {
    indices <- iterator@indices
    n.within <- iterator@nWithin
    n.between <- iterator@nBetween
    length <- length(indices) * n.within * n.between
    first <- pos
    last <- pos + length - 1L
    indices.show <- indicesShow(iterator,
                                nSeason = NULL)
    methods::new("SkeletonTrendDLM",
                 first = first,
                 last = last,
                 indicesShow = indices.show,
                 metadata = metadata)
}

## HAS_TESTS
makeOutputSeasonDLM <- function(iterator, metadata, nSeason, iAlong, pos) {
    indices <- iterator@indices
    n.within <- iterator@nWithin
    n.between <- iterator@nBetween
    length <- length(indices) * n.within * n.between * nSeason
    first <- pos
    last <- pos + length - 1L
    indices.show <- indicesShow(iterator,
                                nSeason = nSeason)
    methods::new("SkeletonSeasonDLM",
                 first = first,
                 last = last,
                 indicesShow = indices.show,
                 iAlong = iAlong,
                 nSeason = nSeason,
                 metadata = metadata)
}

## HAS_TESTS
makeResultsFile <- function(filename, results, tempfiles) {
    kLength <- 10000
    con.write <- file(filename, open = "wb")
    on.exit(close(con.write))
    results <- serialize(results, connection = NULL)
    size.results <- length(results)
    writeBin(size.results, con = con.write)
    writeBin(results, con = con.write)
    for (i in seq_along(tempfiles)) {
        con.read <- file(tempfiles[i], "rb")
        finished <- FALSE
        while (!finished) {
            object <- readBin(con = con.read, what = "double", n = kLength)
            writeBin(object, con = con.write)
            finished <- length(object) < kLength
        }
        close(con.read)
        unlink(tempfiles[i])
    }
    NULL
}

## HAS_TESTS
makeResultsModelEst <- function(finalCombineds, mcmcArgs, controlArgs, seed) {
    combined <- finalCombineds[[1L]]
    model <- combined@model
    y <- combined@y
    has.exposure <- methods::is(combined, "HasExposure")
    exposure <- if (has.exposure) combined@exposure else NULL
    mcmc <- makeOutputMCMC(mcmcArgs = mcmcArgs,
                           finalCombineds = finalCombineds)
    n.sim <- mcmc[["nSim"]]
    final <- finalCombineds
    names(final) <- paste0("chain", seq_along(final))
    pos <- 1L
    if (n.sim > 0L) {
        output.model <- makeOutputModel(model = model, pos = pos, mcmc = mcmc)
        pos <- pos + changeInPos(output.model)
        if (any(is.na(y)))
            output.y <- SkeletonMissingData(y,
                                            model = model,
                                            outputModel = output.model,
                                            exposure = exposure)
        else
            output.y <- y
    }
    else {
        output.model <- list()
        output.y <- y
    }
    if (has.exposure) {
        methods::new("ResultsModelExposureEst",
                     model = output.model,
                     y = output.y,
                     exposure = exposure,
                     mcmc = mcmc,
                     control = controlArgs,
                     seed = seed,
                     final = final)
    }
    else
        methods::new("ResultsModelEst",
                     model = output.model,
                     y = output.y,
                     mcmc = mcmc,
                     control = controlArgs,
                     seed = seed,
                     final = final)
}

## HAS_TESTS
makeResultsModelPred <- function(finalCombineds, mcmcArgs, controlArgs, seed) {
    combined <- finalCombineds[[1L]]
    model <- combined@model
    y <- combined@y
    mcmc <- makeOutputMCMC(mcmcArgs = mcmcArgs,
                           finalCombineds = finalCombineds)
    final <- finalCombineds
    names(final) <- paste0("chain", seq_along(final))
    output.model <- makeOutputModel(model = model, pos = 1L, mcmc = mcmc)
    mcmc <- mcmc["nIteration"]
    methods::new("ResultsModelPred",
        model = output.model,
        mcmc = mcmc,
        control = controlArgs,
        seed = seed,
        final = final)
}

## HAS_TESTS
makeResultsCounts <- function(finalCombineds, mcmcArgs, controlArgs, seed) {
    combined <- finalCombineds[[1L]]
    model <- combined@model
    y <- combined@y
    observation <- combined@observation
    datasets <- combined@datasets
    names.datasets <- combined@namesDatasets
    transforms <- combined@transforms
    mcmc <- makeOutputMCMC(mcmcArgs = mcmcArgs,
                           finalCombineds = finalCombineds)
    n.sim <- mcmc[["nSim"]]
    pos <- 1L
    if (n.sim > 0L) {
        output.model <- makeOutputModel(model = model, pos = pos, mcmc = mcmc)
        pos <- pos + changeInPos(output.model)
    }
    else
        model <- list()
    output.y <- Skeleton(y, first = pos)
    pos <- pos + changeInPos(output.y)
    has.exposure <- methods::is(combined, "HasExposure")
    if (has.exposure)
        exposure <- combined@exposure
    output.observation <- vector(mode = "list", length = length(observation))
    if (n.sim > 0L) {
        for (i in seq_along(observation)) {
            output.observation[[i]] <- makeOutputModel(model = observation[[i]],
                                                       pos = pos,
                                                       mcmc = mcmc)
            pos <- pos + changeInPos(output.observation[[i]])
        }
        for (i in seq_along(datasets)) {
            if (any(is.na(datasets[[i]])))
                datasets[[i]] <-
                    SkeletonMissingDataset(object = datasets[[i]],
                                           model = observation[[i]],
                                           outputModel = output.observation[[i]],
                                           transformComponent = transforms[[i]],
                                           skeletonComponent = output.y)
        }
    }
    names(output.observation) <- names.datasets
    names(datasets) <- names.datasets
    final <- finalCombineds
    names(final) <- paste("chain", seq_along(final), sep = "")
    if (has.exposure) {
        methods::new("ResultsCountsExposureEst",
            model = output.model,
            y = output.y,
            exposure = exposure,
            observation = output.observation,
            datasets = datasets,
            mcmc = mcmc,
            control = controlArgs,
            seed = seed,
            final = final)
    }
    else
        methods::new("ResultsCountsEst",
            model = output.model,
            y = output.y,
            observation = output.observation,
            datasets = datasets,
            mcmc = mcmc,
            control = controlArgs,
            seed = seed,
            final = final)
}


## PRINTING ##########################################################################

expandTermsMod <- function(names) {
    ans <- "(Intercept)"
    if (length(names) > 1L) {
        other.terms <- names[-1L]
        other.terms <- paste0(other.terms, "[j[i]]")
        other.terms <- paste(other.terms, collapse = " + ")
        ans <- paste(ans, other.terms, sep = " + ")
    }
    ans
}

expandTermsSpec <- function(f) {
    terms <- terms(f)
    term.labels <- attr(terms, "term.labels")
    ans <- paste0(term.labels, "[j[i]]")
    ans <- paste(ans, collapse = " + ")
    ans
}

printAggregateEqns <- function(object) {
    printAgValEqns(object)
    printAgAccuracyEqns(object)
}

printAggregateSpecEqns <- function(object) {
    aggregate <- object@aggregate
    printSpecAggregateEqns(aggregate)
}

printBinomialLikEqns <- function(object) {
    formulaMu <- object@formulaMu
    terms <- expandTermsSpec(formulaMu)
    cat("            y[i] ~ binomial(exposure[i], prob[i])\n")
    cat("  logit(prob[i]) ~ N(", terms, ", sd^2)\n", sep = "")
}

printBinomialModEqns <- function(object) {
    call <- object@call
    lower <- object@lower
    upper <- object@upper
    names <- object@namesBetas
    series <- call$series
    has.series <- !is.null(series)
    name.y <- deparse(call$formula[[2L]])
    name.y <- sprintf("%13s", name.y)
    lower <- invlogit1(lower)
    upper <- invlogit1(upper)
    terms <- expandTermsMod(names)
    if (has.series)
        exposure <- series
    else
        exposure <- "exposure"
    cat(name.y, "[i] ~ binomial(", exposure, "[i], prob[i])", sep = "")
    if ((0 < lower) || (upper < 1))
        cat(",  ", format(lower, digits = 4), "< prob[i] <", format(upper, digits = 4))
    cat("\n")
    cat("  logit(prob[i]) ~ N(", terms, ", sd^2)\n", sep = "")
}

printBinomialSpecEqns <- function(object) {
    formulaMu <- object@formulaMu
    nameY <- object@nameY
    series <- object@series@.Data
    lower <- object@lower
    upper <- object@upper
    has.series <- !is.na(series)
    name.y <- sprintf("%13s", nameY)
    if (has.series)
        exposure <- series        
    else
        exposure <- "exposure"
    terms <- expandTermsSpec(formulaMu)
    cat(name.y, "[i] ~ binomial(", exposure, "[i], prob[i])", sep = "")
    if ((0 < lower) || (upper < 1))
        cat(",  ", format(lower, digits = 4), "< prob[i] <", format(upper, digits = 4))
    cat("\n")
    cat("  logit(prob[i]) ~ N(", terms, ", sd^2)\n", sep = "")
}

printCovariatesEqns <- function(object) {
    AEtaIntercept <- object@AEtaIntercept
    AEtaCoef <- object@AEtaCoef
    nuEtaCoef <- object@nuEtaCoef
    cat("    covariate[j] ~ (Intercept) + data[j,] * coef\n")
    cat("     (Intercept) ~ N(0, ", squaredOrNA(AEtaIntercept), ")\n", sep = "")
    cat("         coef[p] ~ t(", nuEtaCoef, ", 0, ", squaredOrNA(AEtaCoef), ")\n",
        sep = "")
}

printCovariatesDLMEqns <- function(object, isMain) {
    AEtaIntercept <- object@AEtaIntercept
    AEtaCoef <- object@AEtaCoef
    nuEtaCoef <- object@nuEtaCoef
    if (isMain)
        cat("    covariate[j] ~ (Intercept) + data[j,] * coef\n")
    else
        cat("  covariate[k,l] ~ (Intercept) + data[k,l,] * coef\n")
    cat("     (Intercept) ~ N(0, ", squaredOrNA(AEtaIntercept), ")\n", sep = "")
    cat("         coef[p] ~ t(", nuEtaCoef, ", 0, ", squaredOrNA(AEtaCoef), ")\n",
        sep = "")
}

printDLMEqns <- function(object, name, order, hasTrend, hasSeason, hasCovariates) {
    is.main <- order == 1L
    if (is.null(name))
        name <- "parameter"
    if (is.main) {
        name <- sprintf("%13s", name, sep = "")
        cat(name, "[j] ~ level[j] + ", sep = "")
    }
    else {
        name <- sprintf("%11s", name, sep = "")
        cat(name, "[k,l] ~ level[k,l] + ", sep = "")
    }                  
    if (hasSeason) {
        if (is.main)
            cat("season[j] + ")
        else
            cat("season[k,l] + ")
    }
    if (hasCovariates) {
        if (is.main)
            cat("covariates[j] + ")
        else
            cat("covariates[k,l] + ")
    }
    if (is.main)
        cat("error[j]\n")
    else
        cat("error[k,l]\n")
    printLevelTrendEqns(object = object,
                        isMain = is.main,
                        hasTrend = hasTrend)
    if (hasSeason)
        printSeasonEqns(object = object,
                        isMain = is.main)
    if (hasCovariates)
        printCovariatesDLMEqns(object = object,
                               isMain = is.main)
    printErrorDLMEqns(object = object,
                      isMain = is.main)
}

printErrorDLMEqns <- function(object, isMain) {
    nuTau <- object@nuTau
    A <- object@ATau
    max <- object@tauMax
    is.robust <- methods::is(object, "SpecRobustMixin")
    if (is.robust) {
        nuBeta <- object@nuBeta
        if (isMain)
            cat("        error[j] ~ t(", nuBeta, ", 0, scaleError^2)\n", sep = "")
        else
            cat("      error[k,l] ~ t(", nuBeta, ", 0, scaleError^2)\n", sep = "")
    }
    else {
        if (isMain)
            cat("        error[j] ~ N(0, scaleError^2)\n")
        else
            cat("      error[k,l] ~ N(0, scaleError^2)\n")
    }
    cat("      scaleError ~ trunc-half-t(", nuTau, ", ", sep = "")
    cat(squaredOrNA(A), ", ", format(max, digits = 4), ")\n", sep = "")
}

printErrorEqns <- function(object) {
    nuTau <- object@nuTau
    A <- object@ATau
    max <- object@tauMax
    is.robust <- methods::is(object, "SpecRobustMixin")
    if (is.robust) {
        nuBeta <- object@nuBeta
        cat("        error[j] ~ t(", nuBeta, ", 0, scaleError^2)\n", sep = "")
    }
    else
        cat("        error[j] ~ N(0, scaleError^2)\n")
    cat("      scaleError ~ trunc-half-t(", nuTau, ", ", sep = "")
    cat(squaredOrNA(A), ", ", format(max, digits = 4), ")\n", sep = "")
}

printExchEqns <- function(object, name, hasCovariates) {
    if (is.null(name))
        name <- "parameter"
    name <- sprintf("%13s", name) 
    cat(name, "[j] ~ ", sep = "")
    if (hasCovariates)
        cat("covariate[j] + ")
    cat("error[j]\n")
    if (hasCovariates)
        printCovariatesEqns(object)
    printErrorEqns(object)
}

printExchFixedEqns <- function(object, name) {
    sd <- object@tau@.Data
    if (is.null(name))
        name <- "parameter"
    name <- sprintf("%13s", name) 
    cat(name, "[j] ~ N(0, ", squaredOrNA(sd), ")\n", sep = "")
}

printJump <- function(object) {
    aggregate <- object@aggregate
    scale.theta <- stringScaleTheta(object)
    scale.ag <- stringScaleAg(aggregate)
    print.scale.theta <- nzchar(scale.theta)
    print.scale.ag <- nzchar(scale.ag)
    if (print.scale.theta || print.scale.ag) {
        cat("\njump:\n")
        if (print.scale.theta)
            cat(scale.theta)
        if (print.scale.ag)
            cat(scale.ag)
    }
    else
        invisible()
}

printJumpAg <- function(object) {
    jump <- object@scaleAg
    value <- object@valueAg
    weight <- object@weightAg
    value.is.scalar <- identical(length(value), 1L)
    weight.is.null <- is.null(weight)
    if (!value.is.scalar || !weight.is.null)
        cat("\n")
    cat("jump:", jump, "\n")
}

printLevelTrendEqns <- function(object, isMain, hasTrend) {
    AAlpha <- object@AAlpha
    omegaAlphaMax <- object@omegaAlphaMax
    nuAlpha <- object@nuAlpha
    phi <- object@phi
    phi.known <- object@phiKnown
    min.phi <- object@minPhi
    max.phi <- object@maxPhi
    is.spec <- methods::is(object, "SpecPrior")
    if (hasTrend) {
        ADelta0 <- object@ADelta0@.Data
        ADelta <- object@ADelta@.Data
        meanDelta0 <- object@meanDelta0@.Data
        nuDelta <- object@nuDelta@.Data
        omegaDeltaMax <- object@omegaDeltaMax@.Data
        if (is.spec)
            AAlpha0 <- NA
        else {
            DC <- object@DC@.Data
            AAlpha0 <- DC[[1L]][1L]
        }
    }
    else {
        if (is.spec)
            AAlpha0 <- NA
        else {
            C <- object@CNoTrend@.Data
            AAlpha0 <- sqrt(C[[1L]])
        }
    }
    show.damp <- !phi.known || (phi < 1)
    if (hasTrend) {
        if (isMain) {
            cat("        level[j] ~ level[j-1] + trend[j-1] + errorLevel[j]\n")
            cat("        trend[j] ~ ")
        }
        else {
            cat("      level[k,l] ~ level[k-1,l] + trend[k-1,l] + errorLevel[k,l]\n")
            cat("      trend[k,l] ~ ")
        }
        if (show.damp)
            cat("damp * ")
        if (isMain)
            cat("trend[j-1] + errorTrend[j]\n")
        else
            cat("trend[k-1,l] + errorTrend[k,l]\n")
    }
    else {
        if (isMain)
            cat("        level[j] ~ ")
        else
            cat("      level[k,l] ~ ")
        if (show.damp)
            cat("damp * ")
        if (isMain)
            cat("level[j-1] + errorLevel[j]\n")
        else
            cat("level[k-1,l] + errorLevel[k,l]\n")
    }
    if (isMain) {
        cat("        level[0] ~ N(0, ", squaredOrNA(AAlpha0), ")\n", sep = "")
        if (hasTrend) {
            cat("        trend[0] ~ N(", meanDelta0, ", ", sep = "")
            cat(squaredOrNA(ADelta0), ")\n", sep = "")
        }
    }
    else {
        cat("      level[0,l] ~ N(0, ", squaredOrNA(AAlpha0), ")\n", sep = "")
        if (hasTrend) {
            cat("      trend[0,l] ~ N(", meanDelta0, ", ", sep = "")
            cat(squaredOrNA(ADelta0), ")\n", sep = "")
        }
    }
    if (show.damp) {
        if (phi.known)
            cat("            damp =",
                format(phi, digits = 4),
                "\n")
        else
            cat("            damp ~ Unif(",
                format(min.phi, digits = 4),
                ", ",
                format(max.phi, digits = 4), ")\n",
                sep = "")
    }
    if (isMain)
        cat("   errorLevel[j] ~ N(0, scaleLevel^2)\n")
    else
        cat(" errorLevel[k,l] ~ N(0, scaleLevel^2)\n")
    if (hasTrend) {
        if (isMain)
            cat("   errorTrend[j] ~ N(0, scaleTrend^2)\n")
        else
            cat(" errorTrend[k,l] ~ N(0, scaleTrend^2)\n")
    }
    cat("      scaleLevel ~ trunc-half-t(", nuAlpha, ", ", sep = "")
    cat(squaredOrNA(AAlpha),
        ", ",
        format(omegaAlphaMax, digits = 4),
        ")\n",
        sep = "")
    if (hasTrend) {
        cat("      scaleTrend ~ trunc-half-t(", nuDelta, ", ", sep = "")
        cat(squaredOrNA(ADelta),
            ", ",
            format(omegaDeltaMax, digits = 4),
            ")\n",
            sep = "")
    }
}        

printNormalVarsigmaKnownLikEqns <- function(object) {
    formulaMu <- object@formulaMu
    varsigma <- object@varsigma
    terms <- expandTermsSpec(formulaMu)
    cat("            y[i] ~ N(mean[i], ", varsigma, "^2 / weights[i])\n", sep = "")
    cat("         mean[i] ~ N(", terms, ", sd^2)\n", sep = "")
}

printNormalVarsigmaUnknownLikEqns <- function(object) {
    formulaMu <- object@formulaMu
    nu <- object@nuVarsigma
    A <- object@AVarsigma
    max <- object@varsigmaMax
    terms <- expandTermsSpec(formulaMu)
    cat("            y[i] ~ N(mean[i], sdData^2 / weights[i])\n", sep = "")
    cat("         mean[i] ~ N(", terms, ", sd^2)\n", sep = "")
    cat("          sdData ~ trunc-half-t(", nu, ", ", sep = "")
    cat(squaredOrNA(A), ", ", format(max, digits = 4), ")\n", sep = "")
}

printNormalVarsigmaKnownModEqns <- function(object) {
    call <- object@call
    lower <- object@lower
    upper <- object@upper
    names <- object@namesBetas
    varsigma <- object@varsigma@.Data
    series <- call$series
    has.series <- !is.null(series)
    name.y <- deparse(call$formula[[2L]])
    name.y <- sprintf("%13s", name.y)
    terms <- expandTermsMod(names)
    cat(name.y, "[i] ~ N(mean[i], ", varsigma, "^2 / weight[i])", sep = "")
    if (is.finite(lower) || is.finite(upper))
        cat(",  ", format(lower, digits = 4), "< mean[i] <", format(upper, digits = 4))
    cat("\n")
    cat("         mean[i] ~ N(", terms, ", sd^2)\n", sep = "")
}

printNormalVarsigmaKnownSpecEqns <- function(object) {
    formulaMu <- object@formulaMu
    varsigma <- object@varsigma
    nameY <- object@nameY
    lower <- object@lower
    upper <- object@upper
    name.y <- sprintf("%13s", nameY)
    terms <- expandTermsSpec(formulaMu)
    cat(name.y, "[i] ~ N(mean[i], ", varsigma, "^2 / weight[i])", sep = "")
    if (is.finite(lower) || is.finite(upper))
        cat(",  ", format(lower, digits = 4), "< mean[i] <", format(upper, digits = 4))
    cat("\n")
    cat("         mean[i] ~ N(", terms, ", sd^2)\n", sep = "")
}

printNormalVarsigmaUnknownModEqns <- function(object) {
    call <- object@call
    lower <- object@lower
    upper <- object@upper
    names <- object@namesBetas
    nu <- object@nuVarsigma
    A <- object@AVarsigma
    max <- object@varsigmaMax
    series <- call$series
    has.series <- !is.null(series)
    name.y <- deparse(call$formula[[2L]])
    name.y <- sprintf("%13s", name.y)
    terms <- expandTermsMod(names)
    cat("            y[i] ~ N(mean[i], sdData^2 / weights[i])", sep = "")
    if (is.finite(lower) || is.finite(upper))
        cat(",  ", format(lower, digits = 4), "< mean[i] <", format(upper, digits = 4))
    cat("\n")
    cat("         mean[i] ~ N(", terms, ", sd^2)\n", sep = "")
    cat("          sdData ~ trunc-half-t(", nu, ", ", sep = "")
    cat(squaredOrNA(A), ", ", format(max, digits = 4), ")\n", sep = "")
}

printNormalVarsigmaUnknownSpecEqns <- function(object) {
    formulaMu <- object@formulaMu
    nu <- object@nuVarsigma
    A <- object@AVarsigma
    max <- object@varsigmaMax
    nameY <- object@nameY
    lower <- object@lower
    upper <- object@upper
    name.y <- sprintf("%13s", nameY)
    terms <- expandTermsSpec(formulaMu)
    cat("            y[i] ~ N(mean[i], sdData^2 / weights[i])", sep = "")
    if (is.finite(lower) || is.finite(upper))
        cat(",  ", format(lower, digits = 4), "< mean[i] <", format(upper, digits = 4))
    cat("\n")
    cat("         mean[i] ~ N(", terms, ", sd^2)\n", sep = "")
    cat("          sdData ~ trunc-half-t(", nu, ", ", sep = "")
    cat(squaredOrNA(A), ", ", format(max, digits = 4), ")\n", sep = "")
}

printPoissonBinomialLikEqns <- function(object) {
    prob <- object@prob
    cat("y[i] ~ Poisson-binomial(exposure[i], prob[i])\n")
}

printPoissonBinomialSpecEqns <- function(object) {
    call <- object@call
    prob <- object@prob
    series <- call$series
    has.series <- !is.null(series)
    name.y <- deparse(call$formula[[2L]])
    name.y <- sprintf("%13s", nameY)
    if (has.series)
        exposure <- series        
    else
        exposure <- "exposure"
    cat(name.y, "[i] ~ Poisson-binomial(", exposure, "[i], prob[i])\n", sep = "")
}

printPoissonBinomialSpecEqns <- function(object) {
    prob <- object@prob
    nameY <- object@nameY
    series <- object@series@.Data
    has.series <- !is.na(series)
    name.y <- sprintf("%13s", nameY)
    if (has.series)
        exposure <- series        
    else
        exposure <- "exposure"
    cat(name.y, "[i] ~ Poisson-binomial(", exposure, "[i], prob[i])\n", sep = "")
}

printPoissonLikEqns <- function(object) {
    formulaMu <- object@formulaMu
    terms <- expandTermsSpec(formulaMu)
    cat("            y[i] ~ Poisson(rate[i] * exposure[i])\n")
    cat("    log(rate[i]) ~ N(", terms, ", sd^2)\n", sep = "")
    cat("                 -------- or --------\n")
    cat("            y[i] ~ Poisson(count[i])\n")
    cat("   log(count[i]) ~ N(", terms, ", sd^2)\n", sep = "")
}

printPoissonModEqns <- function(object) {
    call <- object@call
    lower <- object@lower
    upper <- object@upper
    names <- object@namesBetas
    uses.exposure <- methods::is(object, "UseExposure")
    series <- call$series
    has.series <- !is.null(series)
    name.y <- deparse(call$formula[[2L]])
    name.y <- sprintf("%13s", name.y)
    lower <- exp(lower)
    upper <- exp(upper)
    terms <- expandTermsMod(names)
    if (uses.exposure) {
        if (has.series)
            exposure <- series
        else
            exposure <- "exposure"
        cat(name.y, "[i] ~ Poisson(rate[i] * ", exposure, "[i])", sep = "")
        if (lower > 0 || is.finite(upper))
            cat(",  ", format(lower, digits = 4), "< rate[i] <", format(upper, digits = 4))
        cat("\n")
        cat("    log(rate[i]) ~ N(", terms, ", sd^2)\n", sep = "")
    }
    else {
        cat(name.y, "[i] ~ Poisson(count[i])", sep = "")
        if ((lower > 0) || (is.finite(upper)))
            cat(",  ", format(lower, digits = 4), "< count[i] <", format(upper, digits = 4))
        cat("\n")
        cat("   log(count[i]) ~ N(", terms, ", sd^2)\n", sep = "")
    }
}


printPoissonSpecEqns <- function(object) {
    formulaMu <- object@formulaMu
    nameY <- object@nameY
    series <- object@series@.Data
    lower <- object@lower
    upper <- object@upper
    has.series <- !is.na(series)
    name.y <- sprintf("%13s", nameY)
    if (has.series)
        exposure <- series
    else
        exposure <- "exposure"
    terms <- expandTermsSpec(formulaMu)
    cat(name.y, "[i] ~ Poisson(rate[i] * ", exposure, "[i])", sep = "")
    if (lower > 0 || is.finite(upper))
        cat(",  ", format(lower, digits = 4), "< rate[i] <", format(upper, digits = 4))
    cat("\n")
    cat("    log(rate[i]) ~ N(", terms, ", sd^2)\n", sep = "")
    if (!has.series) {
        cat("                 -------- or --------\n")
        cat("            y[i] ~ Poisson(count[i])")
        if (lower > 0 || is.finite(upper))
            cat(",  ", format(lower, digits = 4), "< count[i] <", format(upper, digits = 4))
        cat("\n")
        cat("   log(count[i]) ~ N(", terms, ", sd^2)  \n", sep = "")
    }
}

printPriorsEqns <- function(object) {
    stopifnot(methods::is(object, "Varying"))
    priors <- object@priorsBetas
    names <- object@namesBetas
    margins <- object@margins
    metadata.y <- object@metadataY
    prior.intercept <- priors[[1L]]
    printPriorIntercept(prior.intercept)
    n <- length(priors)
    if (n > 2L) {
        cat("\n")
        for (i in seq.int(from = 2L, to = n)) {
            prior <- priors[[i]]
            name <- names[[i]]
            mar <- margins[[i]]
            order <- length(mar)
            printPriorEqns(object = prior,
                           name = name,
                           order = order)
            if (i < n)
                cat("\n")
        }
    }
}

printSDAg <- function(object) {
    sd <- object@sdAg@.Data
    metadata <- object@metadataAg
    sd.is.scalar <- identical(length(sd), 1L)
    if (sd.is.scalar)
        cat("sd:", format(sd, digits = 4), "\n")
    else {
        sd <- array(sd,
                    dim = dim(metadata),
                    dimnames = dimnames(metadata))
        cat("\nsd:\n")
        print(sd, digits = 4)
    }
}

printSDEqns <- function(object) {
    nu <- object@nuSigma
    A <- object@ASigma
    max <- object@sigmaMax
    cat("              sd ~ trunc-half-t(", nu, ", ", sep = "")
    cat(squaredOrNA(A), ", ", format(max, digits = 4), ")\n", sep = "")
}

printSeasonEqns <- function(object, isMain) {
    n <- object@nSeason
    nu <- object@nuSeason
    A <- object@ASeason
    max <- object@omegaSeasonMax
    if (isMain) {
        cat("       season[j] ~ season[j-", n, "] + errorSeason[j]\n", sep = "")
        cat("  errorSeason[j] ~ N(0, scaleSeason^2)\n")
    }
    else {
        cat("     season[k,l] ~ season[k-", n, ",l] + errorSeason[k,l]\n", sep = "")
        cat("errorSeason[k,l] ~ N(0, scaleSeason^2)\n")
    }
    cat("     scaleSeason ~ trunc-half-t(", nu, ", ", sep = "")
    cat(squaredOrNA(A), ", ", format(max, digits = 4), ")\n", sep = "")
}

printSpecAggregateEqns <- function(object) {
    aggregate <- object@aggregate
    printSpecAgValEqns(object = object,
                       aggregate = aggregate)
    printSpecAgAccuracyEqns(aggregate)
}

printSpecsPriorsEqns <- function(object) {
    formulaMu <- object@formulaMu
    terms <- terms(formulaMu)
    term.labels <- attr(terms, "term.labels")
    term.orders <- attr(terms, "order")
    specs <- object@specsPriors
    names <- object@namesSpecsPriors
    i.name <- 0L
    n.name <- length(names)
    for (i.term in seq_along(term.labels)) {
        label <- term.labels[i.term]
        i.spec <- match(label, names, nomatch = 0L)
        has.label <- i.spec > 0L
        if (has.label) {
            i.name <- i.name + 1L
            spec <- specs[[i.spec]]
            name <- term.labels[i.term]
            order <- term.orders[i.term]
            printPriorEqns(object = specs[[i.spec]],
                           name = name,
                           order = order)
            cat("\n")
        }
    }
}

printWeightAg <- function(object) {
    weight <- object@weightAg
    metadata <- object@metadataAg
    if (!is.null(weight)) {
        cat("\nweights:\n")
        print(weight@.Data)
    }              
}

printValueAg <- function(object) {
    value <- object@valueAg@.Data
    metadata <- object@metadataAg
    cat("\n")
    if (is.null(metadata))
        cat("value:", value, "\n")
    else {
        value <- array(value,
                       dim = dim(metadata),
                       dimnames = dimnames(metadata))
        cat("value:\n")
        print(value, digits = 4)
    }
}

squaredOrNA <- function(x) {
    if (is.na(x))
        x
    else {
        if (isTRUE(all.equal(x, 1.0)))
            format(x, digits = 4)
        else
            paste0(format(x, digits = 4), "^2")
    }
}



## INSPECT RESULTS ###################################################################

## HAS_TESTS
MCMCDemographic <- function(object, sample = NULL, nChain, nThin = 1L) {
    if (!methods::is(object, "DemographicArray"))
        stop(gettextf("'%s' has class \"%s\"",
                      "object", class(object)))
    kDefaultSize <- 25L
    .Data <- object@.Data
    dim <- dim(object)
    n.dim <- length(dim)
    i.iter <- match("iteration", dembase::dimtypes(object), nomatch = 0L)
    if (identical(i.iter, 0L))
        stop(gettextf("no dimension with dimtype \"%s\"", "iteration"))
    n.data <- length(.Data)
    n.iter <- dim[i.iter]
    n.slice <- n.data / n.iter  ## number of values in one iteration
    if (is.null(sample)) {
        if (kDefaultSize >= n.slice)
            sample <- seq_len(n.slice)
        else
            sample <- sort(sample.int(n.slice, size = kDefaultSize))
    }
    else {
        if (!all(sample %in% seq_len(n.slice)))
            stop(gettextf("'%s' has values outside the valid range", "sample"))
    }
    for (name in c("nChain", "nThin")) {
        value <- get(name)
        if (!is.numeric(value))
            stop(gettextf("'%s' does not have type \"%s\"", name, "numeric"))
        if (!identical(length(value), 1L))
            stop(gettextf("'%s' does not have length %d", name, 1L))
        if (is.na(value))
            stop(gettextf("'%s' is missing", name))
        if (value < 1L)
            stop(gettextf("'%s' is less than %d", name, 1L))
        assign(name, as.integer(value))
    }
    if (!identical(n.iter %% nChain, 0L))
        stop(gettextf("number of iterations is not divisible by '%s'", "nChain"))
    if (!identical(i.iter, n.dim)) {
        s <- seq_len(n.dim)
        .Data <- aperm(.Data, perm = c(s[-i.iter], i.iter))
    }
    .Data <- array(.Data, dim = c(n.slice, n.iter / nChain, nChain))
    .Data <- .Data[sample, , , drop = FALSE]
    makeColnames <- function(x, y) outer(x, y, FUN = paste, sep = ".")
    colnames <- Reduce(makeColnames, dimnames(object)[-i.iter])
    colnames <- colnames[sample]
    makeChainIntoMCMC <- function(i) {
        chain <- matrix(.Data[ , , i], nrow = nrow(.Data))
        chain <- t(chain)
        colnames(chain) <- colnames
        coda::mcmc(chain, thin = nThin)
    }
    l <- lapply(seq_len(nChain), makeChainIntoMCMC)
    coda::mcmc.list(l)
}

## HAS_TESTS
addIterationsToTransform <- function(transform, nIter) {
    ## transform
    stopifnot(methods::is(transform, "CollapseTransform"))
    ## nIteration
    stopifnot(is.integer(nIter))
    stopifnot(identical(length(nIter), 1L))
    stopifnot(!is.na(nIter))
    stopifnot(nIter > 0L)
    indices <- transform@indices
    dims <- transform@dims
    dimBefore <- transform@dimBefore
    dimAfter <- transform@dimAfter
    indices.ans <- c(indices, list(seq_len(nIter)))
    dims.ans <- c(dims, length(dimAfter) + 1L)
    dimBefore.ans <- c(dimBefore, nIter)
    dimAfter.ans <- c(dimAfter, nIter)
    methods::new("CollapseTransform",
        indices = indices.ans,
        dims = dims.ans,
        dimBefore = dimBefore.ans,
        dimAfter = dimAfter.ans)
}

## HAS_TESTS
calculateDF <- function(dim) {
    if (!is.integer(dim))
        stop(gettextf("'%s' does not have type \"%s\"",
                      "dim", "integer"))
    n <- length(dim)
    if (n == 0L)
        stop(gettextf("'%s' has length %d",
                      "dim", 0L))
    if (any(is.na(dim)))
        stop(gettextf("'%s' has missing values",
                      "dim"))
    if (any(dim < 2L))
        stop(gettextf("'%s' has values less than %d",
                      "dim", 2L))
    if (n == 1L)
        dim - 1L
    else {
        s <- seq_len(n)
        margins <- lapply(s[-n], function(m) utils::combn(s, m, simplify = FALSE))
        margins <- unlist(margins, recursive = FALSE)
        ans <- as.integer(prod(dim)) - 1L
        for (i in seq_along(margins))
            ans <- ans - Recall(dim[margins[[i]]])
        ans
    }
}


## HAS_TESTS
centerPolyGammaTrend <- function(object) {
    .Data <- object@.Data
    metadata <- object@metadata
    means <- apply(.Data[1L, , ], MARGIN = 2L, mean)
    .Data[1L, , ] <- sweep(.Data[1L, , ], MARGIN = 2L, means)
    methods::new("Values",
        .Data = .Data,
        metadata = metadata)
}

## HAS_TESTS
checkProbs <- function(probs) {
    if (!is.numeric(probs))
        stop(gettextf("'%s' is non-numeric",
                      "probs"))
    if (length(probs) == 0L)
        stop(gettextf("'%s' has length %d",
                      "probs", 0L))
    if (any(is.na(probs)))
        stop(gettextf("'%s' has missing values",
                      "probs"))
    if (any(duplicated(probs)))
        stop(gettextf("'%s' has duplicates",
                      "probs"))
    if (any(probs < 0))
        stop(gettextf("'%s' has negative values",
                      "probs"))
    if (any(probs > 1))
        stop(gettextf("'%s' has values greater than %d",
                      "probs", 1L))
    NULL
}

## HAS_TESTS
checkAndTidyTotalOrSampled <- function(x, model, ySampled, name) {
    if (!methods::is(x, "Counts"))
        stop(gettextf("'%s' has class \"%s\"",
                      name, class(x)))
    if (any(is.na(x)))
        stop(gettextf("'%s' has missing values",
                      name))
    if (any(x < 0L))
        stop(gettextf("'%s' has negative values",
                      name))
    x <- castPopnOrSampled(x = x, model = model, name = name)
    x <- tryCatch(dembase::makeCompatible(x = x, y = ySampled),
                  error = function(e) e)
    if (methods::is(x, "error"))
        stop(gettextf("'%s' and '%s' incompatible : %s",
                      name, "y", x$message))
    x
}

## HAS_TESTS
checkMax <- function(max) {
    if (!is.null(max)) {
        if (!identical(length(max), 1L))
            stop(gettextf("'%s' does not have length %d",
                          "max", 1L))
        if (!is.numeric(max))
            stop(gettextf("'%s' does not have type \"%s\"",
                          "max", "numeric"))
        if (is.na(max))
            stop(gettextf("'%s' is missing",
                          "max"))
        if (!isTRUE(all.equal(round(max), max)))
            stop(gettextf("'%s' is not an integer",
                          "max"))
        if (max < 1L)
            stop(gettextf("'%s' is less than %d",
                          "max", 1L))
    }
    NULL
}

## HAS_TESTS
excludeFromList <- function(object, exclude = character()) {
    if (!is.list(object))
        stop(gettextf("'%s' has class \"%s\"",
                      "object", class(object)))
    for (i in seq_along(object)) {
        if (is.list(object[[i]]))
            object[[i]] <- Recall(object[[i]], exclude = exclude)
        else {
            if (names(object)[i] %in% exclude)
                object[i] <- list(NULL)
        }
    }
    object
}

## HAS_TESTS
fetchResultsObject <- function(filename) {
    con <- file(filename, open = "rb")
    on.exit(close(con))
    size.results <- readBin(con = con, what = "integer", n = 1L)
    results <- readBin(con = con, what = "raw", n = size.results)
    unserialize(results)
}

## HAS_TESTS
fetchInner <- function(object, nameObject, where, iterations,
                       filename, lengthIter, nIteration,
                       listsAsSingleItems, shift = TRUE,
                       impute = FALSE) {
    n.where <- length(where)
    if (n.where == 0L) {
        if (is.list(object)) {
            if (nameObject %in% listsAsSingleItems)
                object
            else {
                choices <- names(object)
                raiseMultipleChoicesError(choices)
            }
        }
        else
            fetchResults(object = object,
                         nameObject = nameObject,
                         filename = filename,
                         iterations = iterations,
                         nIteration = nIteration,
                         lengthIter = lengthIter,
                         shift = shift,
                         impute = impute)
    }
    else {
        if (is.list(object) && !(nameObject %in% listsAsSingleItems)) {
            choices <- names(object)
            name <- where[1L]
            i <- charmatch(name, choices, nomatch = -1L)
            if (i > 0L) {
                name <- choices[i]
                Recall(object = object[[i]],
                       nameObject = name,
                       where = where[-1L],
                       iterations = iterations,
                       filename = filename,
                       lengthIter = lengthIter,
                       nIteration = nIteration,
                       listsAsSingleItems = listsAsSingleItems,
                       shift = shift,
                       impute = impute)
            }
            else if (i == 0L)
                raiseMultipleMatchesError(target = name, choices = choices)
            else
                raiseNotFoundError(target = name, choices = choices)
        }
        else
            raiseOvershotError(nameObject = nameObject, where = where)
    }
}

## HAS_TESTS
## assume that 'where' valid
fetchSkeleton <- function(object, where) {
    choices <- methods::slotNames(object)
    name <- where[1L]
    i <- charmatch(name, choices)
    name <- choices[i]
    fetchSkeletonInner(methods::slot(object, name), where = where[-1L])
}

## HAS_TESTS
fetchSkeletonInner <- function(object, where) {
    n.where <- length(where)
    if (n.where == 0L)
        object
    else {
        choices <- names(object)
        name <- where[1L]
        i <- charmatch(name, choices)
        name <- choices[i]
        Recall(object = object[[i]], where = where[-1L])
    }
}

## HAS_TESTS
finiteSDInner <- function(filename, model, where, probs, iterations) {
    if (!methods::is(model, "Betas"))
        return(NULL)
    checkProbs(probs)
    names.betas <- model@namesBetas
    dims <- model@dims
    n.beta <- length(names.betas)
    if (n.beta == 1L)
        return(NULL)
    ## calculate SS and df for betas (other than intercept)
    SS <- vector(mode = "list", length = n.beta - 1L)
    df <- vector(mode = "list", length = n.beta - 1L)
    for (i in seq_len(n.beta - 1L)) {
        name.beta <- names.betas[i + 1L]
        where.beta <- c(where, "prior", name.beta)
        beta <- fetch(filename,
                      where = where.beta,
                      iterations = iterations)
        n.iter <- dim(beta)[length(dim(beta))]
        beta <- matrix(beta@.Data, ncol = n.iter)
        SS[[i]] <- colSums(beta^2)
        df[[i]] <- calculateDF(dims[[i + 1L]])
    }
    ## calculate sd
    sd <- vector(mode = "list", length = n.beta - 1L)
    for (i in seq_along(sd))
        sd[[i]] <- sqrt(SS[[i]] / df[[i]])
    ## calculate quantiles
    .Data = lapply(sd, stats::quantile, probs = probs)
    ## assemble answer
    .Data <- do.call(rbind, .Data)
    dn <- list(term = names.betas[-1L],
               quantile = paste0(100 * probs, "%"))
    dimnames(.Data) <- dn
    metadata <- methods::new("MetaData",
                    nms = names(dn),
                    dimtypes = c("state", "quantile"),
                    DimScales = list(methods::new("Categories", dimvalues = dn[[1L]]),
                        methods::new("Quantiles", dimvalues = probs)))
    df <- unlist(df)
    methods::new("FiniteSD",
        .Data = .Data,
        metadata = metadata,
        df = df)
}

## HAS_TESTS
foldMCMCList <- function(l) {
    if (!coda::is.mcmc.list(l))
        stop(gettextf("'%s' has class \"%s\"",
                      "l", class(l)))
    nrow.old <- nrow(l[[1L]])
    if (nrow.old < 2L)
        stop(gettextf("'%s' has fewer than %d rows",
                      "l", 2L))
    nrow.new <- trunc(nrow.old / 2)
    n <- length(l)
    ans <- vector(mode = "list", length = 2L * n)
    rows.first <- seq_len(nrow.new)
    rows.second <- seq(to = nrow.old, length = nrow.new)
    thin <- coda::thin(l[[1L]])
    for (i.old in seq_len(n)) {
        i.new.first <- (i.old - 1L) * 2L + 1L
        i.new.second <- i.new.first + 1L
        old <- l[[i.old]]
        first <- old[rows.first, , drop = FALSE]
        second <- old[rows.second, , drop = FALSE]
        first <- coda::mcmc(first, thin = thin)
        second <- coda::mcmc(second, thin = thin)
        ans[[i.new.first]] <- first
        ans[[i.new.second]] <- second
    }
    coda::mcmc.list(ans)
}    

#' Obtain potential scale reduction factors (Rhats).
#'
#' Extract potential scale reduction factors (Rhats) from an object of
#' class \code{\linkS4class{SummaryResults}}. See the documentation for
#' \code{\link{fetchSummary}} for details.
#' 
#' @param object An object of class \code{\linkS4class{SummaryResults}}.
#' 
#' @return A named numeric vector.
#'
#' @seealso  \code{\link{fetchSummary}}
#'
#' @examples
#' library(demdata)
#' deaths <- Counts(round(VADeaths2))
#' popn <- Counts(VAPopn)
#' filename <- tempfile()
#' estimateModel(Model(y ~ Poisson(mean ~ sex)),
#'               y = deaths,
#'               exposure = popn,
#'               filename = filename,
#'               nBurnin = 2,
#'               nSim = 20,
#'               nChain = 2,
#'               parallel = FALSE)
#' summary.deaths <- fetchSummary(filename)
#' gelmanDiag(summary.deaths)
#' @export
gelmanDiag <- function(object) {
    if (!methods::is(object, "SummaryResults"))
        stop(gettextf("'%s' has class \"%s\"",
                      "object", class(object)))
    if (methods::.hasSlot(object, "gelmanDiag"))
        object@gelmanDiag
    else
        NULL
}

## TRANSLATED
## HAS_TESTS
## Assume all arguments have been checked
## - 'filename' is a string;
## - 'first', 'last', and 'lengthIter' are all positive integer scalars,
##    such that 'first' <= 'last' <= 'lengthIter';
## - 'iterations' is vector of integers of length >= 1, in order, with no duplicates
## Note that the function defaults to useC is TRUE (rather than FALSE like most
## functions) since it meant to be called from within R.
getDataFromFile <- function(filename, first, last, lengthIter,
                            iterations, useC = TRUE) {
    if (useC) {
        .Call(getDataFromFile_R, filename, first, last, lengthIter, iterations)
    }
    else {
        n.iter <- length(iterations)
        length.data <- last - first + 1L
        length.gap <- lengthIter - length.data
        ans <- double(length = n.iter * length.data)
        con <- file(filename, open = "rb")
        on.exit(close(con))
        ## find out size of results object - stored in first position ## NEW
        size.results <- readBin(con = con, what = "integer", n = 1L)  ## NEW
        ## skip over results object ## NEW
        for (j in seq_len(size.results))  ## NEW
            readBin(con = con, what = "raw", n = 1L)  ## NEW
        ## go to start of first iteration
        n.skip <- iterations[1L] - 1L
        for (i in seq_len(n.skip))
            for (j in seq_len(lengthIter))
                readBin(con = con, what = "double", n = 1L)
        ## go along iteration to start of data
        for (j in seq_len(first - 1L))
            readBin(con = con, what = "double", n = 1L)
        ## read data
        pos <- 1L
        for (j in seq_len(length.data)) {
            ans[pos] <- readBin(con = con, what = "double", n = 1L)
            pos <- pos + 1L
        }
        ## read remaining lines, if any
        if (n.iter > 1L) {
            for (i in seq.int(from = 2L, to = n.iter)) {
                ## move to start of next block of data
                n.skip <- iterations[i] - iterations[i - 1L] - 1L
                for (j in seq_len(n.skip * lengthIter + length.gap))
                    readBin(con = con, what = "double", n = 1L)
                ## read data
                for (j in seq_len(length.data)) {
                    ans[pos] <- readBin(con = con, what = "double", n = 1L)
                    pos <- pos + 1L
                }
            }
        }
        ans
    }
}

## TRANSLATED
## HAS_TESTS
## Single-iteration version of 'getDataFromFile'.
## Assume all arguments have been checked.
## - 'filename' is a string;
## - 'first', 'last', and 'lengthIter' are all positive integer scalars,
##    such that 'first' <= 'last' <= 'lengthIter';
getOneIterFromFile <- function(filename, first, last, lengthIter, iteration,
                               useC = FALSE) {
    if (useC) {
        .Call(getOneIterFromFile_R, filename, first, last, lengthIter,
              iteration)
    }
    else {
        length.data <- last - first + 1L
        ans <- double(length = length.data)
        con <- file(filename, open = "rb")
        on.exit(close(con))
        ## go to start of first iteration
        n.skip <- iteration - 1L
        ## go to start of first iteration
        for (i in seq_len(n.skip))
            for (j in seq_len(lengthIter))
                readBin(con = con, what = "double", n = 1L)
        ## go along iteration to start of data
        for (j in seq_len(first - 1L))
            readBin(con = con, what = "double", n = 1L)
        ## read data
        for (j in seq_len(length.data))
            ans[j] <- readBin(con = con, what = "double", n = 1L)
        ans
    }
}

## HAS_TESTS
giveListElementsLongNames <- function(object, names = NULL) {
    if (!is.list(object))
        stop(gettextf("'%s' has class \"%s\"",
                      "object", class(object)))
    for (i in seq_along(object)) {
        name <- names(object)[i]
        combined.name <- paste(c(names, name), collapse = ".")
        if (is.list(object[[i]]))
            object[[i]] <- Recall(object[[i]], names = combined.name)
        else
            names(object)[i] <- combined.name
    }
    object
}

## HAS_TESTS
isTimeVarying <- function(filenameEst, filenamePred, where) {
    obj.est <- fetch(filenameEst,
                     where = where,
                     iterations = 1L,
                     normalize = FALSE,
                     impute = FALSE)
    obj.pred <- fetch(filenamePred,
                      where = where,
                      iterations = 1L,
                      normalize = FALSE,
                      impute = FALSE)
    metadata.est <- obj.est@metadata
    metadata.pred <- obj.pred@metadata
    !identical(metadata.est, metadata.pred)
}

## HAS_TESTS
jump <- function(object) {
    where.jump <- whereJump(object)
    if (is.null(where.jump))
        NULL
    else {
        ans <- sapply(where.jump,
                      function(w) fetch(object, where = w))
        ans <- matrix(ans, ncol = 1L)
        colnames(ans) <- "value"
        rownames(ans) <- sapply(where.jump, paste, collapse = ".")
        ans
    }
}

## HAS_TESTS
listsAsSingleItems <- function()
    c("control", "final", "seed")

## HAS_TESTS
makeAutocorr <- function(object) {
    if (!methods::is(object, "mcmc.list"))
        stop(gettextf("'%s' has class \"%s\"",
                      class(object)))
    cor <- coda::autocorr(object, lags = 1, relative = FALSE)
    several.var <- ncol(object[[1L]]) > 1L
    if (several.var) {
        cor <- lapply(cor, drop)
        cor <- lapply(cor, diag)
    }
    cor <- unlist(cor)
    mean(abs(cor))
}


## NO_TESTS
makeContentsList <- function(object, where, max) {
    listsAsSingleItems <- listsAsSingleItems()
    n.where <- length(where)
    choices <- methods::slotNames(object)
    depth <- 1L
    if (n.where == 0L) {
        ans <- vector(mode = "list", length = length(choices))
        for (i in seq_along(ans)) {
            name <- choices[i]
            ans[i] <- list(makeContentsListInner(object = methods::slot(object, name),
                                                 nameObject = name,
                                                 where = character(),
                                                 max = max,
                                                 depth = depth,
                                                 listsAsSingleItems = listsAsSingleItems))
        }
        names(ans) <- choices
        ans
    }
    else {
        name <- where[1L]
        i <- charmatch(name, choices, nomatch = -1L)
        if (i > 0L) {
            name <- choices[i]
            makeContentsListInner(object = methods::slot(object, name),
                                  nameObject = name,
                                  where = where[-1L],
                                  max = max,
                                  depth = depth,
                                  listsAsSingleItems = listsAsSingleItems)
        }
        else if (i == 0L)
            raiseMultipleMatchesError(target = name, choices = choices)
        else
            raiseNotFoundError(target = name, choices = choices)
    }
}

## NO_TESTS
makeContentsListInner <- function(object, nameObject, where, max, depth, listsAsSingleItems) {
    n.where <- length(where)
    depth.ok <- is.null(max) || (depth < max)
    normal.list <- !(nameObject %in% listsAsSingleItems)
    if (n.where == 0L) {
        if (is.list(object) && depth.ok && normal.list) {
            ans <- vector(mode = "list", length = length(object))
            names <- names(object)
            depth <- depth + 1L
            for (i in seq_along(ans))
                ans[i] <- list(Recall(object = object[[i]],
                                      nameObject = names[i],
                                      where = character(),
                                      max = max,
                                      depth = depth,
                                      listsAsSingleItems = listsAsSingleItems))
            names(ans) <- names
            ans
        }
        else
            nameObject
    }
    else {
        if (is.list(object) && normal.list) {
            choices <- names(object)
            name <- where[1L]
            i <- charmatch(name, choices, nomatch = -1L)
            if (i > 0L) {
                if (depth.ok) {
                    name <- choices[i]
                    depth <- depth + 1L
                    Recall(object = object[[i]],
                           nameObject = name,
                           where = where[-1L],
                           max = max,
                           depth = depth,
                           listsAsSingleItems = listsAsSingleItems)
                }
                else
                    name
            }
            else if (i == 0L)
                raiseMultipleMatchesError(target = name, choices = choices)
            else
                raiseNotFoundError(target = name, choices = choices)
        }
        else
            raiseOvershotError(nameObject = nameObject, where = where)
    }
}

## HAS_TESTS
makeGelmanDiag <- function(object, filename) {
    if (!methods::is(object, "Results"))
        stop(gettextf("'%s' has class \"%s\"",
                      "object", class(object)))
    if (identical(dembase::nIteration(object), 0L))
        numeric()
    l <- fetchMCMC(filename)
    if (is.null(l))
        numeric()
    n <- length(l)
    ans <- numeric(length = n)
    for (i in seq_len(n)) {
        mcmc.list.i <- l[[i]]
        mcmc.list.i <- foldMCMCList(mcmc.list.i)
        ans.i <- coda::gelman.diag(mcmc.list.i,
                                   autoburnin = FALSE,
                                   multivariate = FALSE)
        ans.i <- max(ans.i$psrf[, "Point est."])
        ans[i] <- ans.i
    }
    names(ans) <- names(l)
    ans
}

## HAS_TESTS
makeMCMCPriorsBetas <- function(priors, names) {
    ans <- lapply(priors, whereEstimated)
    times <- sapply(ans, length)
    names <- rep(names, times = times)
    ans <- unlist(ans)
    if (length(ans) > 0L)
        mapply(c,
               "hyper",
               names,
               ans,
               SIMPLIFY = FALSE,
               USE.NAMES = FALSE)
    else
        NULL        
}

## HAS_TESTS
makeMetropolis <- function(object, filename) {
    if (!methods::is(object, "Results"))
        stop(gettextf("'%s' has class \"%s\"",
                      "object", class(object)))
    if (identical(dembase::nIteration(object), 0L))
        return(NULL)
    where.jump <- whereMetropStat(object, FUN = whereJump)
    if (identical(where.jump, list(NULL)))
        return(NULL)
    where.acceptance <- whereMetropStat(object, FUN = whereAcceptance)
    where.autocorr <- whereMetropStat(object, FUN = whereAutocorr)
    jump <- sapply(where.jump, function(where) fetch(filename, where))
    acceptance <- lapply(where.acceptance, function(where) fetch(filename, where))
    acceptance <- sapply(acceptance, mean)
    autocorr <- lapply(where.autocorr, function(where) fetchMCMC(filename, where))
    autocorr <- sapply(autocorr, makeAutocorr)
    ans <- data.frame(jump, acceptance, autocorr)
    rownames <- sapply(where.autocorr, function(x) paste(x, collapse = "."))
    rownames(ans) <- rownames
    ans
}

## HAS_TESTS
makeParameters <- function(object, filename) {
    if (!methods::is(object, "Results"))
        stop(gettextf("'%s' has class \"%s\"",
                      "object", class(object)))
    kProbs <- c(0.025, 0.5, 0.975)
    n.iter <- dembase::nIteration(object)
    if (n.iter == 0L)
        return(NULL)
    else if (n.iter <= 25L)
        iterations <- seq_len(n.iter)
    else
        iterations <- sample(n.iter, size = 25L)
    where.est <- whereMetropStat(object, whereEstimated)
    n.where <- length(where.est)
    if (n.where == 0L)
        return(NULL)
    quantile <- vector(mode = "list", length = n.where)
    length <- integer(length = n.where)
    for (i in seq_len(n.where)) {
        where <- where.est[[i]]
        estimates <- fetch(filename,
                           where = where,
                           iterations = iterations,
                           impute = FALSE)
        estimates <- as.double(estimates)
        quantile[[i]] <- stats::quantile(estimates,
                                         probs = kProbs,
                                         na.rm = TRUE)
        length[i] <- c(length = length(estimates) %/% length(iterations))
    }
    colnames <- c(names(quantile[[1L]]), "length")
    rownames <- sapply(where.est, paste, collapse = ".")
    quantile <- do.call(rbind, quantile)
    ans <- data.frame(quantile, length)
    rownames(ans) <- rownames
    colnames(ans) <- colnames
    ans
}

#' Extract information on Metropolis-Hastings updates.
#' 
#' Given an object of class \code{\linkS4class{SummaryResults}}, extract
#' the standard deviation of the proposal density, the proportion of
#' proposals accepted, and autocorrelation, for Metropolis-Hastings updates.
#' See the documentation for \code{\link{fetchSummary}} for details.
#'
#' @param object An object of class \code{\linkS4class{SummaryResults}}.
#' 
#' @return If Metropolis-Hastings updates where carried out, a matrix;
#' otherwise \code{NULL}.
#'
#' @seealso \code{\link{fetchSummary}}
#' 
#' @examples
#' library(demdata)
#' deaths <- Counts(round(VADeaths2))
#' popn <- Counts(VAPopn)
#' filename <- tempfile()
#' estimateModel(Model(y ~ Poisson(mean ~ age)),
#'               y = deaths,
#'               exposure = popn,
#'               filename = filename,
#'               nBurnin = 2,
#'               nSim = 5,
#'               nChain = 2)
#' summary.est <- fetchSummary(filename)
#' metropolis(summary.est)
#' @export
metropolis <- function(object) {
    if (!methods::is(object, "SummaryResults"))
        stop(gettextf("'%s' has class \"%s\"",
                      "object", class(object)))
    object@metropolis
}


#' Extract summaries of parameter estimates from a SummaryResults object.
#' 
#' Given an object of class \code{\linkS4class{SummaryResults}}, extract
#' a data.frame containing summaries of parameter estimates.
#' See the documentation for \code{\link{fetchSummary}} for details
#' of the summaries.
#' 
#' @param object An object of class \code{\linkS4class{SummaryResults}}.
#' 
#' @return A data.frame.
#'
#' @seealso \code{\link{fetchSummary}}
#'
#' @examples
#' deaths <- demdata::VADeaths2
#' popn <- demdata::VAPopn
#' deaths <- round(deaths)
#' deaths <- Counts(deaths)
#' popn <- Counts(popn)
#' filename <- tempfile()
#' model <- Model(y ~ Poisson(mean ~ age + sex),
#'                jump = 0.5)
#' estimateModel(model = model,
#'               y = deaths,
#'               exposure = popn,
#'               filename = filename,
#'               nBurnin = 50,
#'               nSim = 50,
#'               nChain = 2,
#'               parallel = FALSE)
#' summary.est <- fetchSummary(filename)
#' parameters(summary.est)
#' @export
parameters <- function(object) {
    if (!methods::is(object, "SummaryResults"))
        stop(gettextf("'%s' has class \"%s\"",
                      "object", class(object)))
    object@parameters
}

## HAS_TESTS
raiseMultipleChoicesError <- function(choices) {
    stop(sprintf(ngettext(length(choices),
                          "'%s' stops before end of hierarchy : remaining choice is %s",
                          "'%s' stops before end of hierarchy : remaining choices are %s"),
                  "where", paste(sQuote(choices), collapse = ", ")))
}

## HAS_TESTS
raiseMultipleMatchesError <- function(target, choices) {
    stop(gettextf("'%s' partially matches two or more of %s",
                  target,
                  paste(sQuote(choices), collapse = ", ")))
}


## HAS_TESTS
raiseNotFoundError <- function(target, choices) {
    n.choices <- length(choices)
    stop(sprintf(ngettext(n.choices,
                          "'%s' not found : only choice is %s",
                          "'%s' not found : choices are %s"),
                 target, paste(sQuote(choices), collapse = ", ")))
}

## HAS_TESTS
raiseOvershotError <- function(nameObject, where) {
    stop(sprintf(ngettext(length(where),
                          "hierarchy only extends to '%s' : '%s' has additional term %s",
                          "hierarchy only extends to '%s' : '%s' has additional terms %s"),
                 nameObject,
                 "where",
                 paste(dQuote(where), collapse = ", ")))
}

## HAS_TESTS
seasonalNormalizingFactor <- function(season, nSeason, iAlong, nIteration, metadata) {
    n <- length(season)
    n.other <- n / (nSeason * nIteration)
    dim.season <- c(nSeason, n.other, nIteration)
    season <- array(season, dim = dim.season)
    A <- colMeans(season)
    dim.A <- c(dim(metadata), nIteration)
    n.along <- dim.A[iAlong]
    dim.A <- replace(dim.A,
                     list = iAlong,
                     values = n.along + 1L)
    A <- array(A, dim = dim.A)
    A[slice.index(A, MARGIN = iAlong) > 1L]
}


## NO_TESTS
showContentsList <- function(l, nIndent = 2L) {
    kIndent <- 2L
    for (i in seq_along(l)) {
        item <- l[[i]]
        spaces <- paste(rep(" ", times = nIndent), collapse = "")
        if (is.list(item)) {
            name <- names(l)[i]
            cat(spaces, name, "\n", sep = "")
            Recall(item, nIndent = nIndent + kIndent)
        }
        else
            cat(spaces, item, "\n", sep = "")
    }
}

## HAS_TESTS
centerAlong <- function(object, iAlong) {
    if (!methods::is(object, "Values"))
        stop(gettextf("'%s' has class \"%s\"",
                      "object", class(object)))
    if (!is.integer(iAlong))
        stop(gettextf("'%s' does not have type \"%s\"",
                      "iAlong", "integer"))
    if (!identical(length(iAlong), 1L))
        stop(gettextf("'%s' does not have length %d",
                      "iAlong", 1L))
    if (is.na(iAlong))
        stop(gettextf("'%s' is missing",
                      "iAlong"))
    .Data <- object@.Data
    metadata <- object@metadata
    dim <- dim(object)
    n <- length(dim)
    s <- seq_len(n)
    if (!(iAlong %in% s))
        stop(gettextf("'%s' does not specify a dimension of '%s'",
                      "iAlong", "object"))
    if (n > 1L) {
        margin <- s[-iAlong]
        means <- apply(.Data, MARGIN = margin, FUN = mean)
        .Data <- sweep(.Data, MARGIN = margin, STATS = means)
    }
    else
        .Data <- .Data - mean(.Data)
    methods::new("Values", .Data = .Data, metadata = metadata)
}

## HAS_TESTS
## assumes that 'est' and 'pred' are time-varying
combineEstPredHelper <- function(est, pred) {
    .Data.est <- est@.Data
    .Data.pred <- pred@.Data
    metadata.est <- est@metadata
    metadata.pred <- pred@metadata
    dim.est <- dim(metadata.est)
    dim.pred <- dim(metadata.pred)
    names.est <- names(metadata.est)
    names.pred <- names(metadata.pred)
    dimtypes.est <- dembase::dimtypes(metadata.est, use.names = FALSE)
    dimtypes.pred <- dembase::dimtypes(metadata.pred, use.names = FALSE)
    DimScales.est <- dembase::DimScales(metadata.est, use.names = FALSE)
    DimScales.pred <- dembase::DimScales(metadata.pred, use.names = FALSE)
    if (!identical(names.est, names.pred))
        stop(gettextf("results from '%s' and '%s' have different '%s'",
                      "est", "pred", "names"))
    if (!identical(dimtypes.est, dimtypes.pred))
        stop(gettextf("results from '%s' and '%s' have different '%s'",
                      "est", "pred", "dimtypes"))
    DimScales.different <- !mapply(identical,
                                   x = DimScales.est,
                                   y = DimScales.pred)
    if (sum(DimScales.different) != 1L)
        stop(gettextf("results from '%s' and '%s' have incompatible dimensions or '%s'",
                      "est", "pred", "dimscales"))
    i.along <- which(DimScales.different)
    DimScale.est <- DimScales.est[[i.along]]
    DimScale.pred <- DimScales.pred[[i.along]]
    DimScale <- concatDimScaleFirstSecond(first = DimScale.est,
                                          second = DimScale.pred,
                                          name = names.est[i.along])
    DimScales <- replace(DimScales.est,
                         list = i.along,
                         values = list(DimScale))
    metadata <- methods::new("MetaData",
                    nms = names.est,
                    dimtypes = dimtypes.est,
                    DimScales = DimScales)
    s <- seq_along(dim.est)
    perm <- c(s[-i.along], i.along)
    .Data.est <- aperm(.Data.est, perm = perm)
    .Data.pred <- aperm(.Data.pred, perm = perm)
    .Data <- array(c(.Data.est, .Data.pred),
                   dim = dim(metadata)[perm],
                   dimnames = dimnames(metadata)[perm])
    .Data <- aperm(.Data, perm = match(s, perm))
    list(.Data = .Data, metadata = metadata)
}

## HAS_TESTS
flattenList <- function(object) {
  if (!is.list(object))
    stop(gettextf("'%s' has class \"%s\"",
                  "object", class(object)))
  ans <- list()
  for (i in seq_along(object)) {
    if (is.list(object[[i]]))
      ans <- c(ans, Recall(object[[i]]))
    else
      ans <- c(ans, object[i])
  }
  ans
}

## HAS_TESTS
trimNULLsFromList <- function(object) {
  if (!is.list(object))
    stop(gettextf("'%s' has class \"%s\"",
                  "object", class(object)))
  onlyNULL <- function(x) all(is.null(unlist(x)))
  i <- 1L
  while (i <= length(object)) {
    if (onlyNULL(object[[i]]))
      object[[i]] <- NULL
    else {
      if (is.list(object[[i]]))
        object[[i]] <- Recall(object[[i]])
      i <- i + 1L
    }
  }
  object
}


## DEMOGRAPHIC ACCOUNTS ###################################################

## functions for getting cell positions in other components,
## given a mapping, in file 'mapping-functions.R'

## TRANSLATED
## HAS_TESTS
chooseICellComp <- function(description, useC = FALSE) {
    stopifnot(methods::is(description, "DescriptionComp"))
    if (useC) {
        .Call(chooseICellComp_R, description)
    }
    else {
        length <- description@length
        i <- as.integer(stats::runif(n = 1L) * length) # C-style
        if (i == length) # just in case
            i <- length - 1L
        i <- i + 1L # R-style
        i
    }
}

## TRANSLATED
## HAS_TESTS
chooseICellOutInPool <- function(description, useC = FALSE) { 
    stopifnot(methods::is(description, "DescriptionPool"))
    if (useC) {
        .Call(chooseICellOutInPool_R, description) 
    }
    else {
        step.direction <- description@stepDirection
        n.between.vec <- description@nBetweenVec
        step.between.vec <- description@stepBetweenVec
        n.within.vec <- description@nWithinVec
        step.within.vec <- description@stepWithinVec
        n.dim.between <- length(n.between.vec)
        n.dim.within <- length(n.within.vec)
        i.out <- 1L  # assume 'outs' come before 'ins' 
        i.in <- 1L + step.direction
        for (d in seq_len(n.dim.between)) {
            n.between <- n.between.vec[d]  # guaranteed > 1
            step.between <- step.between.vec[d]
            i.between.out <- as.integer(stats::runif(n = 1L) * n.between) # C-style  
            if (i.between.out == n.between) # just in case
                i.between.out <- n.between - 1L
            i.between.in <- as.integer(stats::runif(n = 1L) * (n.between - 1L)) # C-style
            if (i.between.in == n.between - 1L) # just in case
                i.between.in <- n.between - 2L
            if (i.between.in >= i.between.out)
                i.between.in <- i.between.in + 1L
            i.out <- i.out + i.between.out * step.between
            i.in <- i.in + i.between.in * step.between
        }
        for (d in seq_len(n.dim.within)) {
            n.within <- n.within.vec[d]
            step.within <- step.within.vec[d]
            i.within <- as.integer(stats::runif(n = 1L) * n.within) # C-style
            if (i.within == n.within) # just in case
                i.within <- n.within - 1L
            i.out <- i.out + i.within * step.within
            i.in <- i.in + i.within * step.within
        }
        c(i.out, i.in)
    }
}

## TRANSLATED
## HAS_TESTS
chooseICellPopn <- function(description, useC = FALSE) {
    stopifnot(methods::is(description, "DescriptionPopn"))
    if (useC) {
        .Call(chooseICellPopn_R, description)
    }
    else {
        length <- description@length
        n.time <- description@nTime
        step.time <- description@stepTime
        n.initial <- length %/% n.time
        i <- as.integer(stats::runif(n = 1L) * n.initial) # C-style
        if (i == n.initial) # just in case
            i <- n.initial - 1L
        i <- i %% step.time + (i %/% step.time) * (n.time * step.time) # C-style
        i <- i + 1L  # R-style
        i
    }
}

## TRANSLATED
## HAS_TESTS
## Assumes that population and accession have identical dimensions,
## except that time dimension for accession is one shorter than
## time dimension for population
getIAccNextFromPopn <- function(i, description, useC = FALSE) {
    ## 'i'
    stopifnot(is.integer(i))
    stopifnot(identical(length(i), 1L))
    stopifnot(!is.na(i))
    stopifnot(i >= 1L)
    ## 'description'
    stopifnot(methods::is(description, "DescriptionPopn"))
    stopifnot(description@hasAge)
    ## 'i' and 'description'
    stopifnot(i <= description@length)
    if (useC) {
        .Call(getIAccNextFromPopn_R, i, description)
    }
    else {            
        step.time <- description@stepTime
        n.time.popn <- description@nTime
        step.age.popn <- description@stepAge
        n.age <- description@nAge
        n.time.acc <- n.time.popn - 1L
        i.time <- (((i - 1L) %/% step.time) %% n.time.popn) + 1L ## R-style
        if (i.time < n.time.popn) {
            i.acc <- (((i - i.time) %/% (step.time * n.time.popn)) * (step.time * n.time.acc)
                      + ((i - i.time) %% (step.time * n.time.popn))
                      + i.time) ## R-style
            i.age <- (((i - 1L) %/% step.age.popn) %% n.age) + 1L ## R-style
            if (i.age < n.age) {
                if (step.age.popn < step.time)
                    step.age.acc <- step.age.popn
                else
                    step.age.acc <- (step.age.popn * n.time.acc) %/% n.time.popn
                i.acc + step.age.acc
            }
            else
                i.acc
        }
        else
            0L
    }
}


## TRANSLATED
## HAS_TESTS
getIPopnNextFromPopn <- function(i, description, useC = FALSE) {
    ## 'i'
    stopifnot(is.integer(i))
    stopifnot(identical(length(i), 1L))
    stopifnot(!is.na(i))
    stopifnot(i >= 1L)
    ## 'description'
    stopifnot(methods::is(description, "DescriptionPopn"))
    ## 'i' and 'description'
    stopifnot(i <= description@length)
    if (useC) {
        .Call(getIPopnNextFromPopn_R, i, description)
    }
    else {
        step.time <- description@stepTime
        n.time <- description@nTime
        i.time <- (((i - 1L) %/% step.time) %% n.time) + 1L # R-style
        if (i.time < n.time) {
            ans <- i + step.time
            has.age <- description@hasAge
            if (has.age) {
                step.age <- description@stepAge
                n.age <- description@nAge
                i.age <- (((i - 1L) %/% step.age) %% n.age) + 1L # R-style
                if (i.age < n.age)
                    ans <- ans + step.age
            }
            ans
        }
        else
            0L
    }
}

## TRANSLATED
## HAS_TESTS
getMinValCohort <- function(i, series, iterator, useC = FALSE) {  
    ## 'i'
    stopifnot(is.integer(i))
    stopifnot(identical(length(i), 1L))
    stopifnot(!is.na(i))
    stopifnot(i >= 1L)
    ## 'series'
    stopifnot(is.integer(series))
    stopifnot(!any(is.na(series)))
    ## 'iterator'
    stopifnot(methods::is(iterator, "CohortIteratorAccessionPopulation"))
    ## 'i' and 'series'
    stopifnot(i <= length(series))
    if (useC) {
        .Call(getMinValCohort_R, i, series, iterator)
    }
    else {              
        ans <- series[i]
        iterator <- resetCAP(iterator, i = i)  
        while (!iterator@finished) {
            iterator <- advanceCAP(iterator)
            i <- iterator@i
            ans <- min(series[i], ans)
        }
        ans
    }
}

## HAS_TESTS
makeIteratorCAP <- function(dim, iTime, iAge) {
    n.time <- dim[iTime]
    step.time <- 1L
    for (d in seq_len(iTime - 1L))
        step.time <- step.time * dim[d]
    has.age <- iAge > 0L
    if (has.age) {
        n.age <- dim[iAge]
        step.age <- 1L
        for (d in seq_len(iAge - 1L))
            step.age <- step.age * dim[d]
        i.age <- 1L
    }
    else {
        n.age <- as.integer(NA)
        step.age <- as.integer(NA)
        i.age <- as.integer(NA)
    }
    finished <- n.time == 1L
    methods::new("CohortIteratorAccessionPopulation",
        i = 1L,
        nTime = n.time,
        stepTime = step.time,
        iTime = 1L,
        hasAge = has.age,
        nAge = n.age,
        stepAge = step.age,
        iAge = i.age,
        finished = finished)
}

## HAS_TESTS
makeIteratorCC <- function(dim, iTime, iAge, iTriangle) {
    n.time <- dim[iTime]
    step.time <- 1L
    for (d in seq_len(iTime - 1L))
        step.time <- step.time * dim[d]
    has.age <- iAge > 0L
    if (has.age) {
        n.age <- dim[iAge]
        step.age <- 1L
        for (d in seq_len(iAge - 1L))
            step.age <- step.age * dim[d]
        i.age <- 1L
        step.triangle <- 1L
        for (d in seq_len(iTriangle - 1L))
            step.triangle <- step.triangle * dim[d]
        i.triangle <- 1L
    }
    else {
        n.age <- as.integer(NA)
        step.age <- as.integer(NA)
        i.age <- as.integer(NA)
        step.triangle <- as.integer(NA)
        i.triangle <- as.integer(NA)
    }
    finished <- n.time == 1L
    methods::new("CohortIteratorComponent",
        i = 1L,
        nTime = n.time,
        stepTime = step.time,
        iTime = 1L,
        hasAge = has.age,
        nAge = n.age,
        stepAge = step.age,
        iAge = i.age,
        stepTriangle = step.triangle,
        iTriangle = i.triangle,
        finished = finished)
}

## HAS_TESTS
makeIteratorCODPCP <- function(dim, iTime, iAge, iTriangle, iMultiple) {
    n.time <- dim[iTime]
    step.time <- 1L
    for (d in seq_len(iTime - 1L))
        step.time <- step.time * dim[d]
    has.age <- iAge > 0L
    if (has.age) {
        n.age <- dim[iAge]
        step.age <- 1L
        for (d in seq_len(iAge - 1L))
            step.age <- step.age * dim[d]
        i.age <- 1L
        step.triangle <- 1L
        for (d in seq_len(iTriangle - 1L))
            step.triangle <- step.triangle * dim[d]
        i.triangle <- 1L
    }
    else {
        n.age <- as.integer(NA)
        step.age <- as.integer(NA)
        i.age <- as.integer(NA)
        step.triangle <- as.integer(NA)
        i.triangle <- as.integer(NA)
    }
    increment <- vector(mode = "list", length = length(iMultiple))
    for (j in seq_along(iMultiple)) {
        i.m <- iMultiple[j]
        step <- 1L
        for (d in seq_len(i.m - 1L))
            step <- step * dim[d]
        increment[[j]] <- seq.int(from = 0L, by = step, length.out = dim[i.m])
    }
    increment <- expand.grid(increment)
    increment <- Reduce(f = "+", x = increment)
    i <- 1L
    length.vec <- length(increment)
    i.vec <- i + increment
    finished <- n.time == 1L
    methods::new("CohortIteratorOrigDestParChPool",
        i = i,
        nTime = n.time,
        stepTime = step.time,
        iTime = 1L,
        hasAge = has.age,
        nAge = n.age,
        stepAge = step.age,
        iAge = i.age,
        stepTriangle = step.triangle,
        iTriangle = i.triangle,
        iVec = i.vec,
        lengthVec = length.vec,
        increment = increment,
        finished = finished)
}



    

