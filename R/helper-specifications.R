

## HAS_TESTS
addInfantToData <- function(metadata, data) {
    names <- names(metadata)
    dimtypes <- dimtypes(metadata,
                         use.names = FALSE)
    DimScales <- DimScales(metadata,
                           use.names = FALSE)
    namePrior <- paste(names, collapse = ":")
    is.age.main.effect <- identical(dimtypes, "age")
    if (!is.age.main.effect)
        stop(gettextf("cannot add \"%s\" covariate to prior '%s' because '%s' is not a main effect for age",
                      "infant", namePrior, namePrior))
    DimScale <- DimScales[[1L]]
    labels <- labels(DimScale)
    n.age <- length(DimScale)
    if (!methods::is(DimScale, "Intervals"))
        stop(gettextf("cannot make \"%s\" covariate, because dimension with %s \"%s\" has %s \"%s\"",
                      "infant", "dimtype", "age", "dimscale", class(DimScale)))
    if (n.age < 2L)
        stop(gettextf("cannot make \"%s\" covariate, because dimension with %s \"%s\" has length %d",
                      "infant", "dimtype", "age", n.age))
    dimvalues <- DimScale@dimvalues
    if (!isTRUE(all.equal(dimvalues[1:2], 0:1)))
        stop(gettextf("can't make '%s' indicator variable, because first age group for dimension with %s \"%s\" is not \"%d\"",
                      "infant", "dimtype", "age", 0L))
    infant <- c(1L, rep(0L, times = n.age - 1L))
    name.age <- names
    if (length(data) == 0L) {
        data <- data.frame(labels, infant)
        names(data) <- c(name.age, "infant")
    }
    else {
        i.name.age <- match(name.age, names(data), nomatch = 0L)
        has.name.age <- i.name.age > 0L
        if (!has.name.age)
            stop(gettextf("could not find variable '%s' in covariate data for prior '%s'",
                          name.age, namePrior))
        name.infant <- make.unique(c(names(data), "infant"))[length(data) + 1L]
        data[[name.infant]] <- infant[match(data[[i.name.age]], labels)]
    }
    data
}

## NO_TESTS
checkAndTidyIndexClassMaxMix <- function(maxComponents) {
    checkPositiveInteger(x = maxComponents,
                         name = "maxComponents")
    maxComponents <- as.integer(maxComponents)
    if (maxComponents < 2L)
        stop(gettextf("'%s' is less than %d",
                      "maxComponents", 2L))
    methods::new("Counter", maxComponents)    
}

checkAndTidyLevelComponentWeightMinMax <- function(minAR2, maxAR2) {
    for (name in c("minAR2", "maxAR2")) {
        value <- get(name)
        ## 'minAR2', 'maxAR2' have length 1
        if (!identical(length(value), 1L))
            stop(gettextf("'%s' does not have length %d",
                            name, 1L))
        ## 'minAR2', 'maxAR2' are numeric
        if (!is.numeric(value))
            stop(gettextf("'%s' is non-numeric",
                            name))
        ## 'minAR2', 'maxAR2' are not missing
        if (is.na(value))
            stop(gettextf("'%s' is missing",
                            name))
    }
    ## minAR2 < maxAR2
    if (minAR2 >= maxAR2)
        stop(gettextf("'%s' is greater than or equal to '%s'",
                        "minAR2", "maxAR2"))
    minAR2 <- as.double(minAR2)
    maxAR2 <- as.double(maxAR2)
    list(minLevelComponentWeight = minAR2,
         maxLevelComponentWeight = maxAR2)
}

## HAS_TESTS
checkAndTidyStructuralZeros <- function(structuralZeros) {
    if (is.null(structuralZeros))
        NULL
    else if (identical(structuralZeros, "diag"))
        new("Values")
    else if (methods::is(structuralZeros, "Values")) {
        length(structuralZeros) > 0L
        if (any(is.na(structuralZeros)))
            stop(gettextf("'%s' has missing values",
                          "structuralZeros"))
        if (!any(structuralZeros == 0L))
            stop(gettextf("'%s' does not contain any zeros",
                          "structuralZeros"))
        structuralZeros
    }
    else {
        stop(gettextf("'%s' has class \"%s\"",
                      "structuralZeros", class(structuralZeros)))
    }
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
checkAndTidyYForStrucZero <- function(y, strucZeroArray) {
    if (!is.integer(y@.Data))
        stop(gettextf("'%s' does not have type \"%s\"",
                      "y", "integer"))
    should.be.struc.zero <- strucZeroArray@.Data == 0L
    is.na.or.0 <- is.na(y@.Data) | (y@.Data == 0L)
    is.invalid <- should.be.struc.zero & !is.na.or.0
    if (any(is.invalid)) {
        i.first.invalid <- which(is.invalid)[1L]
        all.labels <- expand.grid(dimnames(y))
        label <- all.labels[i.first.invalid, ]
        label[] <- lapply(label, as.character)
        label <- paste(label, collapse = ", ")
        stop(gettextf("cell '[%s]' of '%s' is a structural zero but has value %d",
                      label, "y", y@.Data[[i.first.invalid]]))
    }
    y[should.be.struc.zero] <- 0L # fix up any NAs
    y
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
    if (!is.null(x)) {
        ## Do not check for variables or NAs
        ## to avoid raising errors for variables
        ## or records that are not subsequently
        ## used.  Check within initialPrior
        ## instead.
        if (!is.data.frame(x))
            stop(gettextf("'%s' has class \"%s\"",
                          name, class(x)))
    }
    NULL
}

## NO_TESTS
checkCovariateFormula <- function(formula) {
    if (!is.null(formula)) {
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
    }
    NULL
}

## NO_TESTS
checkInfant <- function(infant) {
    if (!is.logical(infant))
        stop(gettextf("'%s' does not have type \"%s\"",
                      "infant", "logical"))
    if (!identical(length(infant), 1L))
        stop(gettextf("'%s' does not have length %d",
                      "infant", 1L))
    if (is.na(infant))
        stop(gettextf("'%s' is missing",
                      "infant"))
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
    if (!is.null(formula)) {
        formula.no.response <- formula[-2L]
        contrasts.arg <- if (identical(contrastsArg, list())) NULL else contrastsArg
        return.value <- tryCatch(stats::model.matrix(object = formula.no.response,
                                                     data = data,
                                                     contrasts.arg = contrasts.arg),
                                 error = function(e) e)
        if (methods::is(return.value, "error"))
            stop(gettextf("problem constructing model matrix from formula '%s' : %s",
                          deparse(formula), return.value$message))
    }
    NULL
}

## NO_TESTS
checkNonNegativeNumeric <- function(x, name) {
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
    ## 'x' is non-negative
    if (x < 0)
        stop(gettextf("'%s' is negative",
                      name))
    NULL
}


## NO_TESTS
checkAndTidyParameterVector <- function(x, name) {
    ## 'x' has no missing values
    if (any(is.na(x)))
        stop(gettextf("'%s' has missing values",
                      name))
    ## 'x' is numeric
    if (!is.numeric(x))
        stop(gettextf("'%s' is not numeric",
                      name))
    x <- as.numeric(x)
    new("ParameterVector", x)
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
checkPositiveIntegerVector <- function(x, name) {
    ## 'x' has no missing values
    if (any(is.na(x)))
        stop(gettextf("'%s' has missing values",
                      name))
    ## 'x' is numeric
    if (!is.numeric(x))
        stop(gettextf("'%s' is non-numeric",
                      name))
    ## 'x' is integer
    if (!isTRUE(all.equal(x, round(x))))
        stop(gettextf("'%s' has non-integer values",
                      name))
    ## 'x' is positive
    if (any(x <= 0))
        stop(gettextf("'%s' has non-positive values",
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
checkAndTidyMultVec <- function(mult, scale, nameMult, nameScale) {
    checkPositiveNumericVector(x = mult, name = nameMult)
    mult <- as.double(mult)
    n.mult <- length(mult)
    n.scale <- length(scale)
    if (!identical(n.mult, n.scale)) {
        if (identical(n.mult, 1L))
            mult <- rep(mult, times = n.scale)
        else if (identical(n.scale, 1L))
            scale <- rep(scale, times = n.mult)
        else
            stop(gettextf("'%s' has length %d and '%s' has length %d",
                          nameScale, n.scale, nameMult, n.mult))
    }
    non.default.mult <- mult != 1
    non.default.scale <- !is.na(scale)
    if (any(non.default.mult & non.default.scale))
        warning(gettextf("'%s' argument ignored for elements where '%s' argument supplied",
                         nameMult, nameScale))
    methods::new("ScaleVec", mult)
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
checkAndTidyNuVec <- function(x, name) {
    checkPositiveNumericVector(x = x, name = name)
    x <- as.double(x)
    methods::new("DegreesFreedomVector", x)
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
    checkNonNegativeNumeric(x = min, name = "min")
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

## NO_TESTS
checkAndTidySpecScaleVec <- function(x, name) {
    if (is.null(x))
        x <- as.double(NA)
    else {
        checkPositiveNumericVector(x = x, name = name)
        x <- as.double(x)
    }
    methods::new("SpecScaleVec", x)
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


    
