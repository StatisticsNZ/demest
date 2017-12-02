

## HAS_TESTS
## priors follow order implied by formula - not order implied by metadatax
makePriors <- function(betas, specs, namesSpecs, margins, y, sY, strucZeroArray) {
    n.beta <- length(betas)
    ans <- vector(mode = "list", length = n.beta)
    names.betas <- names(betas)
    dim.y <- dim(y)
    s <- seq_along(dim.y)
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
        is.saturated <- identical(sort(margin), s)
        i.spec <- match(name, namesSpecs, nomatch = 0L)
        has.spec <- i.spec > 0L
        if (has.spec)
            spec <- specs[[i.spec]]
        else
            spec <- defaultPrior(beta = beta,
                                 metadata = metadata)
        ans[[i]] <- initialPrior(object = spec,
                                 beta = beta,
                                 metadata = metadata,
                                 sY = sY,
                                 isSaturated = is.saturated,
                                 strucZeroArray = strucZeroArray)
    }
    ans
}



checkAndTidyStructuralZeros <- function(structuralZeros) {
    if (is.null(structuralZeros))
        NULL
    else if (identical(structuralZeros), "diag")
        data.frame()
    else if (is.data.frame(structuralZeros)) {
        if (nrow(structuralZeros) == 0L)
            stop(gettextf("'%s' has %d rows",
                          "structuralZeros", 0L))
        structuralZeros[] <- lapply(structuralZeros, as.character)
        if (any(is.na(structuralZeros)))
            stop(gettextf("'%s' has missing values",
                          "structuralZeros"))
        if (any(!nzchar(unlist(structuralZeros))))
            stop(gettextf("'%s' has blanks",
                          "structuralZeros"))
        if (any(duplicated(structuralZeros)))
            stop(gettextf("'%s' contains duplicate rows",
                          "structuralZeros"))
        structuralZeros
    }
    else {
        stop(gettextf("'%s' has class \"%s\"",
                      "structuralZeros", class(structuralZeros)))
    }
}

setClass("StructuralZerosMixin",
         slots = c(structuralZeros = "dataframeOrNULL"),
         contains = "VIRTUAL",
         validity = function(object) {
             structuralZeros <- object@structuralZeros
             if (is.null(structuralZeros))
                 TRUE
             else if (identical(structuralZeros, data.frame()))
                 TRUE
             else {
                 if (nrow(structuralZeros) == 0L)
                     return(gettextf("'%s' has %d rows",
                                     "structuralZeros", 0L))
                 if (!all(sapply(structuralZeros, is.character)))
                     return(gettextf("'%s' has columns not of type \"%s\"",
                                     "structuralZeros", "character"))
                 if (any(is.na(structuralZeros)))
                     stop(gettextf("'%s' has missing values",
                                   "structuralZeros"))
                 if (any(!nzchar(unlist(structuralZeros))))
                     stop(gettextf("'%s' has blanks",
                                   "structuralZeros"))
                 if (any(duplicated(structuralZeros)))
                     stop(gettextf("'%s' contains duplicate rows",
                                   "structuralZeros"))
                 TRUE
             }
         })


## TRANSLATED
## HAS_TESTS
## Includes case where 'y' has subtotals.
## Subtotals can only be used with PoissonVarying models.
updateTheta_PoissonVaryingUseExp <- function(object, y, exposure, useC = FALSE) {
    ## object
    stopifnot(methods::is(object, "PoissonVaryingUseExp"))
    stopifnot(methods::validObject(object))
    ## y
    stopifnot(identical(length(y), length(object@theta)))
    stopifnot(is.integer(y))
    stopifnot(all(y@.Data[!is.na(y@.Data)] >= 0L))
    ## exposure
    stopifnot(is.double(exposure))
    stopifnot(all(exposure@.Data[!is.na(exposure@.Data)] >= 0))
    ## y and exposure
    stopifnot(identical(length(exposure), length(y)))
    stopifnot(all(is.na(exposure) <= is.na(y)))
    stopifnot(all(y@.Data[!is.na(y@.Data)][exposure[!is.na(y)] == 0] == 0L))
    if (methods::is(y, "HasSubtotals")) {
        for (i in seq_along(exposure))
            if (is.na(exposure[i]) && (dembase::getIAfter(i,
                                                          transform = y@transformSubtotals,
                                                          check = FALSE,
                                                          useC = TRUE) > 0L))
                stop("exposure is missing for cell in subtotal")
    }
    if (useC) {
        .Call(updateTheta_PoissonVaryingUseExp_R, object, y, exposure)
    }
    else {
        y.has.subtotals <- methods::is(y, "HasSubtotals")
        if (y.has.subtotals) {
            subtotals <- y@subtotalsNet
            transform <- y@transformSubtotals
        }
        theta <- object@theta
        box.cox.param <- object@boxCoxParam
        cell.in.lik <- object@cellInLik ## NEW
        scale <- object@scaleTheta
        scale.multiplier <- object@scaleThetaMultiplier
        lower <- object@lower
        upper <- object@upper
        tolerance <- object@tolerance
        sigma <- object@sigma@.Data
        betas <- object@betas
        iterator <- object@iteratorBetas
        max.attempt <- object@maxAttempt
        n.failed.prop.theta <- 0L
        n.accept.theta <- 0L
        iterator <- resetB(iterator)
        scale <- scale * scale.multiplier
        for (i in seq_along(theta)) {
            is.struc.zero <- !cell.in.lik[i] && !is.na(y[i]) && (y[i] == 0L) ## NEW
            if (!is.struc.zero) { ## NEW
                indices <- iterator@indices
                mu <- 0
                for (b in seq_along(betas))
                    mu <- mu + betas[[b]][indices[b]]
                found.prop <- FALSE
                attempt <- 0L
                y.is.missing <- is.na(y[i])
                if (y.is.missing && y.has.subtotals) {
                    i.after <- dembase::getIAfter(i = i,
                                                  transform = transform,
                                                  check = FALSE,
                                                  useC = TRUE)
                    use.subtotal <- i.after > 0L
                }
                else
                    use.subtotal <- FALSE
                draw.straight.from.prior <- y.is.missing && !use.subtotal
                if (draw.straight.from.prior) {
                    mean <- mu
                    sd <- sigma
                }
                else {
                    th.curr <- theta[i]
                    if (box.cox.param > 0)
                        tr.th.curr <- (th.curr ^ box.cox.param - 1) / box.cox.param # ('tr' short for 'transformed')
                    else
                        tr.th.curr <- log(th.curr)
                    mean <- tr.th.curr
                    if (y.is.missing)
                        sd <- scale / scale.multiplier
                    else
                        sd <- scale / sqrt(1 + y[i])
                }
                while (!found.prop && (attempt < max.attempt)) {
                    attempt <- attempt + 1L
                    tr.th.prop <- stats::rnorm(n = 1L, mean = mean, sd = sd)
                    found.prop <- ((tr.th.prop > lower + tolerance)
                        && (tr.th.prop < upper - tolerance))
                }
                if (found.prop) {
                    if (box.cox.param > 0)
                        th.prop <- (box.cox.param * tr.th.prop + 1) ^ (1 / box.cox.param)
                    else
                        th.prop <- exp(tr.th.prop)
                    if (draw.straight.from.prior)
                        theta[i] <- th.prop
                    else {
                        if (use.subtotal) {
                            subtotal <- subtotals[i.after]
                            i.shared <- dembase::getIShared(i = i, transform = transform)
                            lambda.curr <- 0
                            for (i.s in i.shared) {
                                if (is.na(y[i.s]))
                                    lambda.curr <- lambda.curr + theta[i.s] * exposure[i.s]
                            }
                            lambda.prop <- lambda.curr + (th.prop - th.curr) * exposure[i]
                            log.lik.prop <- stats::dpois(x = subtotal, lambda = lambda.prop, log = TRUE)
                            log.lik.curr <- stats::dpois(x = subtotal, lambda = lambda.curr, log = TRUE)
                        }
                        else {
                            log.lik.prop <- stats::dpois(y[i], lambda = th.prop * exposure[i], log = TRUE)
                            log.lik.curr <- stats::dpois(y[i], lambda = th.curr * exposure[i], log = TRUE)
                        }
                        log.dens.prop <- stats::dnorm(x = tr.th.prop, mean = mu, sd = sigma, log = TRUE)
                        log.dens.curr <- stats::dnorm(x = tr.th.curr, mean = mu, sd = sigma, log = TRUE)
                        log.diff <- log.lik.prop + log.dens.prop - log.lik.curr - log.dens.curr
                        accept <- (log.diff >= 0) || (stats::runif(n = 1L) < exp(log.diff))
                        if (accept) {
                            n.accept.theta <- n.accept.theta + 1L
                            theta[i] <- th.prop
                        }
                    }
                }
                else
                    n.failed.prop.theta <- n.failed.prop.theta + 1L
            } ## NEW
            iterator <- advanceB(iterator)
        }
        object@theta <- theta
        object@nFailedPropTheta@.Data <- n.failed.prop.theta
        object@nAcceptTheta@.Data <- n.accept.theta
        object
    }
}

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
              structural.zeros <- object@structuralZeros ## NEW
              box.cox.param <- object@boxCoxParam
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
              struc.zero.array <- makeStrucZeroArray(structuralZeros = structural.zeros, ## NEW
                                                     y = y) ## NEW
              y <- checkAndTidyYForStrucZero(y = y, ## NEW
                                             strucZeroArray = struc.zero.array) ## NEW
              has.exposure <- !is.null(exposure)
              if (has.exposure && !use.expose)
                  stop(gettextf("'%s' argument supplied, but model '%s' does not use exposure",
                                "exposure", deparse(call[[2L]])))
              if (!has.exposure && use.expose)
                  stop(gettextf("model '%s' uses exposure, but no '%s' argument supplied",
                                deparse(call[[2L]]), "exposure"))
              is.obs <- is.na(y@.Data) & (struc.zero.array != 0L) ## NEW
              if (any(is.obs)) ##
                  mean.y.obs <- mean(y@.Data[is.obs]) ## NEW
              else
                  mean.y.obs <- 0.5
              shape <- ifelse(is.obs, 0.5 * mean.y.obs + 0.5 * y@.Data, mean.y.obs) ## NEW
              if (has.exposure) {
                  mean.expose.obs <- mean(exposure[is.obs])
                  rate <- ifelse(is.obs, 0.5 * mean.expose.obs + 0.5 * exposure, mean.expose.obs) ## NEW
              }
              else
                  rate <- 1
              if (has.exposure)
                  scale.theta.multiplier <- sqrt(mean.y.obs + 1)
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
              sigma <- methods::new("Scale", sigma)
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
              theta[struc.zero.array == 0L] <- 0 ## NEW
              names.betas <- names(betas)
              margins <- makeMargins(betas = betas, y = y)
              priors.betas <- makePriors(betas = betas,
                                         specs = specs.priors,
                                         namesSpecs = names.specs.priors,
                                         margins = margins,
                                         y = y,
                                         sY = sY,
                                         strucZeroArray = struc.zero.array) ## NEW
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
              betas <- jitterBetas(betas = betas, ## NEW 
                                   priors = priors) ## NEW
              iterator.betas <- BetaIterator(dim = dim, margins = margins)
              dims <- makeDims(dim = dim, margins = margins)
              class <- if (has.exposure) "PoissonVaryingUseExp" else "PoissonVaryingNotUseExp"
              cellInLik <- rep(TRUE, times = length(theta))
              model <- methods::new(class,
                                    call = call,
                                    theta = theta,
                                    cellInLik = cellInLik,
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
                                     strucZerosArray = struc.zeros.array) ## NEW
              model
          })



setClass("AllStrucZeroMixin",
         slots = c(allStrucZero = "logical"),
         contains = "VIRTUAL",
         validity = function(object) {
             allStrucZero <- object@allStrucZero
             ## no missing values
             if (any(is.na(allStrucZero)))
                 return(gettextf("'%s' has missing values",
                                 "allStrucZero"))
             ## not all TRUE
             if (all(allStrucZero))
                 return(gettext("'%s' all %s",
                                "allStrucZero", "TRUE"))
             TRUE
         })


setClass("AllStrucZeroExchMixin",
         contains = c("VIRTUAL",
                      "AllStrucZeroMixin")
         validity = function(object) {
             allStrucZero <- object@allStrucZero
             J <- object@J@.Data
             if (!identical(length(allStrucZero), J))
                 return(gettextf("'%s' does not have length '%s'",
                                 "allStrucZero", "J"))
             TRUE
         })

setClass("AllStrucZeroDLMMixin",
         contains = c("VIRTUAL",
                      "AllStrucZeroMixin")
         validity = function(object) {
             allStrucZero <- object@allStrucZero
             L <- object@L@.Data
             if (!identical(length(allStrucZero), L))
                 return(gettextf("'%s' does not have length '%s'",
                                 "allStrucZero", "L"))
             TRUE
         })






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
        label <- all.labels[i, ]
        label <- paste(label, collapse = ", ")
        label <- paste0("[", label, "]")
        stop(gettextf("cell '%s' is structural zero but has value %d",
                      label, y@.Data[[i.first.invalid]]))
    }
    y[should.be.struc.zero] <- 0L # fix up any NAs
    y
}


makeStrucZeroArray <- function(structuralZeros, y) {
    if (is.null)
        makeStrucZeroArrayNULL(y)
    if (identical(structuralZeros, data.frame()))
        makeStrucZeroArrayDiag(y)
    else if (is.data.frame(stucturalZeros))
        makeStrucZeroArrayGeneral(structuralZeros = structuralZeros,
                                  y  = y)
}

makeStrucZeroArrayNULL <- function(y) {
    .Data <- array(1L,
                   dim = dim(y),
                   dimnames = dimnames(y))
    metadata <- y@metadata
    new("Counts",
        .Data = .Data,
        metadata = metadata)
}
        
makeStrucZeroArrayDiag <- function(y) {
    metadata <- y@metadata
    names <- names(y)
    dimtypes <- dimtypes(y, use.names = FALSE)
    i.orig <- grep("origin", dimtypes)
    has.orig <- length(i.orig) > 0L
    if (!has.orig)
        stop(gettextf("'%s' has no dimensions with %s \"%s\"",
                      "y", "origin"))
    names.orig <- names[i.orig]
    base <- sub("_orig$", "", names.orig)
    dembase::pairAligned(y, base = base)
    .Data <- array(1L,
                   dim = dim(y),
                   dimnames = dimnames(y))
    names.dest <- sprintf("%s_dest", base)
    i.dest <- match(names.dest, names)
    for (i in seq_along(i.orig)) {
        is.diag <- slice.index(y, MARGIN = i.orig[i]) == slice.index(y, MARGIN = i.dest[i])
        .Data[is.diag] <- 0L
    }
    new("Counts",
        .Data = .Data,
        metadata = metadata)
}

makeStrucZeroArrayGeneral <- function(structuralZeros, y) {
    names.zero <- names(structuralZeros)
    names.y <- names(y)
    dim.y <- dim(y)
    dimnames.y <- dimnames(y)
    vec.indices <- vector(model = "list", length = length(structuralZeros))
    for (i.zero in seq_along(structuralZeros)) {
        name.zero <- names.zero[[i]]
        i.y <- match(name.zero, names.y, nomatch = 0L)
        not.found <- i.y == 0L
        if (not.found)
            stop(gettextf("'%s' does not have a dimension called \"%s\"",
                          "y", name.zero))
        labels.zero <- structuralZeros[[name.zero]]
        labels.y <- dimnames.y[[i.y]]
        i.zero.to.y <- match(labels.zero, labels.y, nomatch = 0L)
        in.zero.not.in.y <- i.zero.to.y == 0L
        if (any(in.zero.not.in.y))
            stop(gettextf("dimension \"%s\" of '%s' does not have element called \"%s\"",
                          name.zero, "y", labels.zero[in.zero.not.in.y][1L]))
        in.zero <- which(labels.y %in% labels.zero)
        vec.indices[[i.zero]] <- slice.index(y, MARGIN = i.y) %in% in.zero
    }
    is.struc.zero <- Reduce(`&`, vec.indices)
    .Data <- array(1L,
                   dim = dim.y,
                   dimnames = dimnames.y)
    .Data[is.struc.zero] <- 0L
    new("Counts",
        .Data = .Data,
        metadata = metadata)
}

        

makeAllStrucZeroExch <- function(metadata) {
    .Data.prior <- array(0L,
                         dim = dim(metadata),
                         dimnames = dimnames(metadata))
    array.prior <- new("Counts",
                       .Data = .Data.prior,
                       metadata = metadata)
    array.zero <- tryCatch(makeCompatible(y = strucZeroArray,
                                          x = array.prior,
                                          subset = FALSE,
                                          check = TRUE),
                           error = function(e) e)
    if (methods::is(array.zero, "error"))
        stop(gettextf("problem assigning structural zeros to prior '%s' : %s",
                      paste(names(metadata), collapse = ":"), array.zero$message))
    as.logical(array.zero@.Data == 0L)
}

makeAllStrucZeroDLM <- function(metadata, iAlong) {
    name.prior <- paste(names(metadata), collapse = ":")
    metadata.along <- metadata[iAlong]
    metadata.within <- metadata[-iAlong]
    .Data.along <- array(0L,
                         dim = dim(metadata.along),
                         dimnames = dimnames(metadata.along))
    .Data.within <- array(0L,
                          dim = dim(metadata.within),
                          dimnames = dimnames(metadata.within))
    array.along <- new("Counts",
                       .Data = .Data.along,
                       metadata = metadata.along)
    array.within <- new("Counts",
                        .Data = .Data.within,
                        metadata = metadata.within)
    array.zero.along <- tryCatch(makeCompatible(y = strucZeroArray,
                                                x = array.along,
                                                subset = FALSE,
                                                check = TRUE),
                                 error = function(e) e)
    array.zero.within <- tryCatch(makeCompatible(y = strucZeroArray,
                                                 x = array.within,
                                                 subset = FALSE,
                                                 check = TRUE),
                                  error = function(e) e)
    if (methods::is(array.zero.along, "error") || methods::is(array.zero.within, "error"))
        stop(gettextf("problem assigning structural zeros to prior '%s' : %s",
                      name.prior, array.zero$message))
    along.is.zero <- as.logical(array.zero.along@.Data == 0L)
    within.is.zero <- as.logical(array.zero.within@.Data == 0L)
    if (any(along.is.zero)) {
        labels <- dimnames(metadata.along)[[1L]]
        i.first.zero <- which(along.is.zero)[1L]
        name.along <- names(metadata.along)
        stop(gettextf("element '%s' of '%s' dimension [\"%s\"] for prior '%s' has not data because of structural zeros",
                      labels[i.first.zero], "along", name.along, name.prior))
    }
    within.is.zero
}


setMethod("makeStrucZero",
          signature(prior = "Mix"),
          function(prior, strucZeroArray, metadata) {
              stop("sorry - mixture prior cannot deal with with structural zeros yet")
          })



makeZ <- function(formula, data, metadata, contrastsArg, infant) {
    namePrior <- paste(names(metadata), collapse = ":")
    ## infant
    if (infant@.Data) {
        data <- addInfantToData(metadata = metadata,
                                data = data)
        if (length(formula) == 0L)
            formula <- ~ infant
        else {
            name.infant <- names(data)[length(data)]
            formula <- deparse(formula)
            formula <- paste(formula, name.infant, sep = " + ")
            formula <- stats::as.formula(formula)
        }
    }
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



## Expands factors into dummy variables, and then standardises as
## described in Gelman, A., Jakulin, A., Pittau, M. G., and Su, Y.-S.
## (2008). A weakly informative default prior distribution
## for logistic and other regression models.
## The Annals of Applied Statistics, pages 1360–1383.
makeStandardizedVariables <- function(formula, inputs, namePrior, contrastsArg, isStrucZero) { ## NEW
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
    ans[isStructZero , ] <- NA ## NEW
    for (j in seq_len(ncol(ans))[-1L]) {
        v <- ans[ , j]
        i.term <- which.term[j]
        is.main.effect <- order.term[i.term] == 1L
        if (is.main.effect) {
            is.binary <- isTRUE(all.equal(sort(unique(v)), 0:1))
            if (is.binary)
                v <- v - mean(v, na.rm = TRUE) ## NEW
            else
                v <- (v - mean(v, na.rm = TRUE)) / (2 * stats::sd(v, na.rm = TRUE)) ## NEW
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




## TRANSLATED
## HAS_TESTS (INCLUDING FOR MIX)
## ADD TESTS FOR ICAR AND Cross WHEN CLASSES FINISHED
betaHat <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "Prior") || methods::is(prior, "FakePrior"))
    stopifnot(methods::is(prior, "ComponentFlags"))
    if (useC) {
        .Call(betaHat_R, prior)
    }
    else {
        J <- prior@J@.Data
        ## has.alpha.cross <- prior@hasAlphaMove@.Data
        has.alpha.dlm <- prior@hasAlphaDLM@.Data
        has.alpha.icar <- prior@hasAlphaICAR@.Data
        has.alpha.mix <- prior@hasAlphaMix@.Data
        has.covariates <- prior@hasCovariates@.Data
        has.season <- prior@hasSeason@.Data
        is.struc.zero <- prior@isStrucZero ## NEW
        ans <- rep(0, times = J)
        ## if (has.alpha.cross) {
        ##     alpha.cross <- prior@alphaCross@.Data
        ##     indices.cross <- prior@indicesCross
        ##     for (j in seq_len(J)) {
        ##         index.cross <- indices.cross[j]
        ##         if (is.infinite(index.cross))
        ##             ans[j] <- if (index.cross < 0) -Inf else Inf
        ##         else
        ##             ans[j] <- ans[j] + alpha.cross[index.cross]
        ##     }
        ## }
        if (has.alpha.dlm) {
            alpha.dlm <- prior@alphaDLM@.Data
            K <- prior@K@.Data
            L <- prior@L@.Data
            iterator.alpha <- prior@iteratorState
            iterator.v <- prior@iteratorV
            iterator.alpha <- resetA(iterator.alpha)
            iterator.v <- resetA(iterator.v)
            for (l in seq_len(L)) {
                if (!is.struc.zero[i]) { ## NEW
                    indices.alpha <- iterator.alpha@indices
                    indices.v <- iterator.v@indices
                    for (k in seq_len(K)) {
                        i.alpha <- indices.alpha[k + 1L]
                        i.ans <- indices.v[k]
                        ans[i.ans] <- ans[i.ans] + alpha.dlm[i.alpha]
                    }
                } ## NEW
                iterator.alpha <- advanceA(iterator.alpha)
                iterator.v <- advanceA(iterator.v)
            }
        }
        if (has.alpha.icar) {
            alpha.icar <- prior@alphaICAR@.Data
            ans <- ans + alpha.icar
        }
        if (has.alpha.mix) { 
            alpha.mix <- prior@alphaMix@.Data 
            ans <- ans + alpha.mix 
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


setMethod("updateBetaAndPriorBeta",
          signature(prior = "ExchFixed"),
          function(prior, vbar, n, sigma, useC = FALSE, useSpecific = FALSE) {
              checkUpdateBetaAndPriorBeta(prior = prior,
                                          vbar = vbar,
                                          n = n,
                                          sigma = sigma)
              if (useC) {
                  if (useSpecific)
                      .Call(updateBetaAndPriorBeta_ExchFixed_R, prior, vbar, n, sigma)
                  else
                      .Call(updateBetaAndPriorBeta_R, prior, vbar, n, sigma)
              }
              else {
                  J <- prior@J@.Data
                  is.saturated <- prior@isSaturated@.Data
                  if (is.saturated)
                      beta <- rep(0, times = J)
                  else {
                      tau <- prior@tau@.Data
                      is.struc.zero <- prior@isStrucZero
                      J <- prior@J
                      prec.prior <- 1 / tau^2
                      for (i in seq_len(J)) {
                          if (!is.struc.zero[i]) {
                              prec.data <- n[i] / sigma^2
                              var <- 1 / (prec.data + prec.prior) 
                              mean <- prec.data * vbar * var
                              sd <- sqrt(var)
                              beta[i] <- stats::rnorm(n = 1L, mean = mean, sd = sd)
                          }
                      }
                  }
                  list(beta, prior)
              }
          })


updateBeta <- function(prior, vbar, n, sigma, useC = FALSE) {
    checkUpdateBetaAndPriorBeta(prior = prior,
                                vbar = vbar,
                                n = n,
                                sigma = sigma)
    stopifnot(methods::is(prior, "ComponentFlags"))
    if (useC) {
        .Call(updateBeta_R, prior, vbar, n, sigma)
    }
    else {
        J <- prior@J@.Data
        is.struct.zero <- prior@isStrucZero
        v <- getV(prior)
        beta.hat <- betaHat(prior)
        ans <- numeric(length = J)
        for (i in seq_len(J)) {
            if (is.struct.zero)
                ans[i] <- 0
            else {
                prec.data <- n[i] / sigma^2 ## now a vector
                prec.prior <- 1 / v[i]
                var <- 1 / (prec.data + prec.prior)
                mean <- (prec.data * vbar[i] + prec.prior * beta.hat[i]) * var
                sd <- sqrt(var)
                ans[i] <- stats::rnorm(n = 1L, mean = mean, sd = sd)
            }
        }
        ans
    }
}


updateTauNorm <- function(prior, beta, useC = FALSE) {
    ## prior
    stopifnot(methods::is(prior, "Prior"))
    stopifnot(methods::is(prior, "NormMixin"))
    ## beta
    stopifnot(is.double(beta))
    stopifnot(!any(is.na(beta)))
    ## prior and beta
    stopifnot(identical(length(beta), as.integer(prior@J)))
    if (useC) {
        .Call(updateTauNorm_R, prior, beta)
    }
    else {
        J <- prior@J@.Data
        tau <- prior@tau@.Data
        tauMax <- prior@tauMax@.Data
        A <- prior@ATau@.Data
        nu <- prior@nuTau@.Data
        is.struc.zero <- prior@isStrucZero
        beta.hat <- betaHat(prior)
        V <- 0
        n <- 0
        for (i in seq_len(J)) {
            if (!is.struct.zero[i]) {
                n <- n + 1L
                V <- V + (beta - beta.hat[i])^2
            }
        }
        tau <- updateSDNorm(sigma = tau,
                            A = A,
                            nu = nu,
                            V = V,
                            n = n,
                            max = tauMax)
        successfully.updated <- tau > 0
        if (successfully.updated)
            prior@tau@.Data <- tau
        prior
    }
}


updateTauRobust <- function(prior, useC = FALSE) {
    ## prior
    stopifnot(methods::is(prior, "Prior"))
    stopifnot(methods::is(prior, "RobustMixin"))
    if (useC) {
        .Call(updateTauRobust_R, prior)
    }
    else {
        J <- prior@J@.Data
        UBeta <- prior@UBeta@.Data
        nuBeta <- prior@nuBeta@.Data
        tau <- prior@tau@.Data
        tauMax <- prior@tauMax@.Data
        A <- prior@ATau@.Data
        nuTau <- prior@nuTau@.Data
        is.struc.zero <- prior@isStrucZero
        V <- 0
        n <- 0L
        for (i in seq_len(J)) {
            if (!is.struct.zero[i]) {
                V <- V + (1 / UBeta[i])
                n <- n + 1L
            }
        }
        tau <- updateSDRobust(sigma = tau,
                              A = A,
                              nuBeta = nuBeta,
                              nuTau = nuTau,
                              V = V,
                              n = n,
                              max = tauMax)
        successfully.updated <- tau > 0
        if (successfully.updated)
            prior@tau@.Data <- tau
        prior
    }
}


updateUBeta <- function(prior, beta, useC = FALSE) {
    ## prior
    stopifnot(methods::validObject(prior))
    stopifnot(methods::is(prior, "RobustMixin"))
    ## beta
    stopifnot(is.double(beta))
    stopifnot(identical(length(beta), as.integer(prior@J)))
    stopifnot(!any(is.na(beta)))
    if (useC) {
        .Call(updateUBeta_R, prior, beta)
    }
    else {
        J <- prior@J@.Data
        U <- prior@UBeta@.Data
        nu <- prior@nuBeta@.Data
        tau <- prior@tau@.Data
        is.struc.zero <- prior@isStrucZero
        beta.hat <- betaHat(prior)
        df <- nu + 1
        for (i in seq_len(J)) {
            if (!is.struc.zero[i]) {
                scale <- (nu * tau^2 + (beta - beta.hat[i])^2) / df
                U[i] <- rinvchisq1(df = df, scale = scale[i])
            }
        }
        prior@UBeta@.Data <- U
        prior
    }
}

updateAlphaDLMNoTrend <- function(prior, betaTilde, useC = FALSE) {
    ## prior
    stopifnot(methods::is(prior, "NoTrendMixin"))
    stopifnot(methods::validObject(prior))
    ## betaTilde
    stopifnot(is.double(betaTilde))
    stopifnot(!any(is.na(betaTilde)))
    stopifnot(identical(length(betaTilde), prior@J@.Data))
    if (useC) {
        .Call(updateAlphaDLMNoTrend_R, prior, betaTilde)
    }
    else {    
        K <- prior@K@.Data
        L <- prior@L@.Data
        alpha <- prior@alphaDLM@.Data # numeric vector length (K+1)L
        m <- prior@mNoTrend@.Data     # list length K+1
        m0 <- prior@m0NoTrend@.Data   # list length L
        C <- prior@CNoTrend@.Data     # list length K+1
        a <- prior@aNoTrend@.Data     # list length K
        R <- prior@RNoTrend@.Data     # list length K
        phi <- prior@phi
        phi.sq <- phi^2
        omega <- prior@omegaAlpha@.Data
        omega.sq <- omega^2
        is.struc.zero <- object@isStrucZero ## NEW
        v <- getV(prior)              # numeric vector length KL
        tolerance <- prior@tolerance@.Data
        iterator.a <- prior@iteratorState
        iterator.v <- prior@iteratorV
        iterator.a <- resetA(iterator.a)
        iterator.v <- resetA(iterator.v)
        for (l in seq_len(L)) {
            if (!is.struc.zero[l]) { ## NEW
                indices.a <- iterator.a@indices
                indices.v <- iterator.v@indices
                m[[1L]] <- m0[[l]]
                ## forward filter
                for (i in seq_len(K)) {
                    a[[i]] <- phi * m[[i]]
                    R[[i]] <- phi.sq * C[[i]] + omega.sq
                    q <- R[[i]] + v[indices.v[i]]
                    e <- betaTilde[indices.v[i]] - a[[i]]
                    A <- R[[i]] / q
                    m[[i + 1L]] <- a[[i]] + A * e
                    C[[i + 1L]] <- R[[i]] - A^2 * q
                }
                ## draw gamma_K
                alpha[indices.a[K + 1L]] <- stats::rnorm(n = 1L,
                                                         mean = m[[K + 1L]], 
                                                         sd = sqrt(C[[K + 1L]]))
                ## backward sample
                ## for (i in seq.int(from = K - 1L, to = non.stationary)) { # if nonstationary, alpha0 = 0
                for (i in seq.int(from = K - 1L, to = 0L)) { # if nonstationary, alpha0 = 0
                    if ((i > 0L) || (C[[1L]] > tolerance)) {
                        B <- C[[i + 1L]] * phi / R[[i + 1L]]
                        m.star <- m[[i + 1L]] + B * (alpha[indices.a[i + 2L]] - a[[i + 1L]])
                        C.star <- C[[i + 1L]] - B^2 * R[[i + 1L]]
                        alpha[indices.a[i + 1L]] <- stats::rnorm(n = 1L, mean = m.star, sd = sqrt(C.star))
                    }
                }
            } ## NEW
            iterator.a <- advanceA(iterator.a)
            iterator.v <- advanceA(iterator.v)
        }
        prior@alphaDLM@.Data <- alpha
        prior
    }
}



## TRANSLATED (AGAIN 16/7/2017)
## HAS_TESTS
updateAlphaDeltaDLMWithTrend <- function(prior, betaTilde, useC = FALSE) {
    ## prior
    stopifnot(methods::is(prior, "WithTrendMixin"))
    stopifnot(methods::validObject(prior))
    ## betaTilde
    stopifnot(is.double(betaTilde))
    stopifnot(!any(is.na(betaTilde)))
    ## 'prior' and 'betaTilde'
    stopifnot(identical(length(betaTilde), prior@J@.Data))
    if (useC) {
        .Call(updateAlphaDeltaDLMWithTrend_R, prior, betaTilde)
    }
    else {    
        K <- prior@K@.Data
        L <- prior@L@.Data
        alpha <- prior@alphaDLM@.Data  # numeric length (K+1)L
        delta <- prior@deltaDLM@.Data  # numeric length (K+1)L
        G <- prior@GWithTrend@.Data  # 2x2 matrix
        m <- prior@mWithTrend@.Data # list length K+1
        m0 <- prior@m0WithTrend@.Data # list length L
        C <- prior@CWithTrend@.Data # list length K+1
        a <- prior@aWithTrend@.Data # list length K
        W.sqrt <- prior@WSqrt@.Data # matrix 2X2
        W.sqrt.inv.G <- prior@WSqrtInvG@.Data # matrix 2X2
        UC <- prior@UC@.Data  # list length K+1
        DC <- prior@DC@.Data  # list length K+1
        DC.inv <- prior@DCInv@.Data # list length K+1
        UR <- prior@UR@.Data  # list length K
        DR.inv <- prior@DRInv@.Data
        has.level <- prior@hasLevel@.Data 
        phi <- prior@phi ## scalar
        omega.alpha <- prior@omegaAlpha@.Data ## scalar
        omega.delta <- prior@omegaDelta@.Data ## scalar
        is.struc.zero <- object@isStrucZero ## NEW
        v <- getV(prior) ## numeric length KL
        iterator.ad <- prior@iteratorState
        iterator.v <- prior@iteratorV
        iterator.ad <- resetA(iterator.ad)
        iterator.v <- resetA(iterator.v)
        for (l in seq_len(L)) {
            if (!is.struc.zero[l]) { ## NEW
                indices.ad <- iterator.ad@indices
                indices.v <- iterator.v@indices
                m[[1L]] <- m0[[l]]
                ## forward filter
                for (i in seq_len(K)) {
                    M.R <- rbind(DC[[i]] %*% t(UC[[i]]) %*% t(G),
                                 W.sqrt)
                    svd.R <- svd(M.R, nu = 0)
                    UR[[i]] <- svd.R$v
                    DR.inv.diag <- 1 / svd.R$d
                    DR.inv.diag[is.infinite(DR.inv.diag)] <- 0
                    DR.inv[[i]][c(1L, 4L)] <- DR.inv.diag
                    M.C <- rbind(UR[[i]][c(1L, 3L)] / sqrt(v[indices.v[i]]),
                                 DR.inv[[i]])
                    svd.C <- svd(M.C, nu = 0)
                    UC[[i + 1L]] <- UR[[i]] %*% svd.C$v
                    DC.inv.diag <- svd.C$d
                    DC.diag <- 1/DC.inv.diag
                    DC.diag[is.infinite(DC.diag)] <- 0
                    DC.inv[[i + 1L]][c(1L, 4L)] <- DC.inv.diag
                    DC[[i + 1L]][c(1L, 4L)] <- DC.diag
                    a[[i]] <- drop(G %*% m[[i]])
                    e <- betaTilde[indices.v[i]] - a[[i]][1L]
                    C[[i + 1L]] <- UC[[i + 1L]] %*% DC[[i + 1L]] %*% DC[[i + 1L]] %*% t(UC[[i + 1L]])
                    A <- C[[i + 1L]][1:2] / v[indices.v[i]]
                    m[[i + 1L]] <- a[[i]] + A * e
                }
                ## draw final gamma, delta
                sqrt.C <- UC[[K + 1L]] %*% DC[[K + 1L]]
                z <- stats::rnorm(n = 2L)
                theta <- m[[K + 1L]] + drop(sqrt.C %*% z)
                alpha[indices.ad[K + 1L]] <- theta[1L]
                delta[indices.ad[K + 1L]] <- theta[2L]
                ## backward smooth
                for (i in seq.int(from = K - 1L, to = 0L)) {
                    if (!has.level) {
                        if ((i == 0L) && is.infinite(DC.inv[[1L]][1L])) {
                            delta[indices.ad[1L]] <- alpha[indices.ad[2L]] - alpha[indices.ad[1L]]
                        }
                        else  {
                            C.inv <- UC[[i + 1L]] %*% DC.inv[[i + 1L]] %*% DC.inv[[i + 1L]] %*% t(UC[[i + 1L]])
                            sigma.inv.1 <- C.inv[1L]
                            sigma.inv.2 <- C.inv[2L]
                            sigma.inv.3 <- C.inv[3L]
                            sigma.inv.4 <- C.inv[4L] + phi^2 / omega.delta^2
                            determinant <- sigma.inv.1 * sigma.inv.4 - sigma.inv.2 * sigma.inv.3
                            sigma.1 <- sigma.inv.4 / determinant
                            sigma.2 <- -1 * sigma.inv.3 / determinant
                            sigma.3 <- -1 * sigma.inv.2 / determinant
                            sigma.4 <- sigma.inv.1 / determinant
                            mu.inner.1 <- C.inv[1L] * m[[i + 1L]][1L] + C.inv[3L] * m[[i + 1L]][2L]
                            mu.inner.2 <- (C.inv[2L] * m[[i + 1L]][1L] + C.inv[4L] * m[[i + 1L]][2L]
                                + phi * delta[indices.ad[i + 2L]] / omega.delta^2)
                            mu.1 <- sigma.1 * mu.inner.1 + sigma.3 * mu.inner.2
                            mu.2 <- sigma.2 * mu.inner.1 + sigma.4 * mu.inner.2
                            mu.star.1 <- mu.1
                            mu.star.2 <- mu.1 + mu.2
                            sigma.star.1 <- sigma.1
                            sigma.star.2 <- sigma.1 + sigma.2
                            sigma.star.3 <- sigma.1 + sigma.3
                            sigma.star.4 <- sigma.1 + sigma.2 + sigma.3 + sigma.4
                            rho.star.sq <- sigma.star.2 * sigma.star.3 / (sigma.star.1 * sigma.star.4)
                            mean.alpha <- (mu.star.1 + sqrt(rho.star.sq * sigma.star.1 / sigma.star.4)
                                * (alpha[indices.ad[i + 2L]] - mu.star.2))
                            var.alpha <- (1 - rho.star.sq) * sigma.star.1
                            alpha.curr <- stats::rnorm(n = 1L,
                                                       mean = mean.alpha,
                                                       sd = sqrt(var.alpha))
                            delta.curr <- alpha[indices.ad[i + 2L]] - alpha.curr
                            alpha[indices.ad[i + 1L]] <- alpha.curr
                            delta[indices.ad[i + 1L]] <- delta.curr
                        }
                    }
                    else {
                        if ((i == 0L) && is.infinite(DC.inv[[1L]][1L])) {
                            prec.delta.0 <- DC.inv[[1L]][4L] 
                            prec.alpha <- 1 / omega.alpha^2
                            prec.delta.1 <- phi^2 / omega.delta^2
                            var.delta.curr <- 1 / (prec.delta.0 + prec.alpha + prec.delta.1)
                            mean.delta.curr <- var.delta.curr * (prec.delta.0 * m[[1L]][2L] + prec.alpha * alpha[indices.ad[2L]]
                                + prec.delta.1 * delta[indices.ad[2L]] / phi)
                            delta.curr <- rnorm(n = 1L,
                                                mean = mean.delta.curr,
                                                sd = sqrt(var.delta.curr))
                            delta[indices.ad[1L]] <- delta.curr
                        }
                        else {
                            R.inv <- (UR[[i + 1L]] %*% DR.inv[[i + 1L]]
                                %*% DR.inv[[i + 1L]] %*% t(UR[[i + 1L]]))
                            B <- C[[i + 1L]] %*% t(G) %*% R.inv
                            M.C.star <- rbind(W.sqrt.inv.G,
                                              DC.inv[[i + 1L]] %*% t(UC[[i + 1L]]))
                            svd.C.star <- svd(M.C.star, nu = 0)
                            UC.star <- svd.C.star$v
                            DC.star <- 1 / svd.C.star$d
                            DC.star[is.infinite(DC.star)] <- 0
                            DC.star <- diag(DC.star, nrow = 2L)
                            sqrt.C.star <- UC.star %*% DC.star
                            theta.prev <- c(alpha[indices.ad[i + 2L]], delta[indices.ad[i + 2L]])
                            m.star <- m[[i + 1L]] + drop(B %*% (theta.prev - a[[i + 1L]]))
                            z <- stats::rnorm(n = 2L)
                            theta.curr <- m.star + drop(sqrt.C.star %*% z)
                            alpha[indices.ad[i + 1L]] <- theta.curr[1L]
                            delta[indices.ad[i + 1L]] <- theta.curr[2L]
                        }
                    }
                }
            } ## NEW
            iterator.ad <- advanceA(iterator.ad)
            iterator.v <- advanceA(iterator.v)
        }
        prior@alphaDLM@.Data <- alpha
        prior@deltaDLM@.Data <- delta
        prior
    }
}


updatePhi <- function(prior, withTrend, useC = FALSE) {
    ## 'prior'
    stopifnot(methods::is(prior, "DLM"))
    stopifnot(methods::validObject(prior))
    ## 'withTrend'
    stopifnot(is.logical(withTrend))
    stopifnot(identical(length(withTrend), 1L))
    stopifnot(!is.na(withTrend))
    ## 'prior' and 'withTrend'
    stopifnot((withTrend && methods::is(prior, "WithTrendMixin"))
              || (!withTrend && methods::is(prior, "NoTrendMixin")))
    if (useC) {
        .Call(updatePhi_R, prior, withTrend)
    }
    else {
        phi.known <- prior@phiKnown@.Data
        if (phi.known)
            return(prior)
        phi.curr <- prior@phi
        K <- prior@K@.Data
        L <- prior@L@.Data
        if (withTrend) {
            state <- prior@deltaDLM@.Data
            omega <- prior@omegaDelta@.Data
        }
        else {
            state <- prior@alphaDLM@.Data
            omega <- prior@omegaAlpha@.Data
        }
        phi.min <- prior@minPhi@.Data
        phi.max <- prior@maxPhi@.Data
        shape1 <- prior@shape1Phi@.Data
        shape2 <- prior@shape2Phi@.Data
        is.struc.zero <- prior@isStrucZero ## new
        iterator <- prior@iteratorState
        iterator <- resetA(iterator)
        numerator <- 0
        denominator <- 0
        for (l in seq_len(L)) {
            if (!is.struc.zero[l]) { ## New
                indices <- iterator@indices
                for (i in seq_len(K)) {
                    k.curr <- indices[i + 1]
                    k.prev <- indices[i]
                    numerator <- numerator + state[k.curr] * state[k.prev]
                    denominator <- denominator + (state[k.prev])^2
                }
            } ## New
            iterator <- advanceA(iterator)
        }
        mean <- numerator / denominator
        sd <- omega / sqrt(denominator)
        phi.prop <- rtnorm1(mean = mean,
                            sd = sd,
                            lower = phi.min,
                            upper = phi.max)
        ## proposal density and likelihood cancel
        phi.prop.tr <- (phi.prop - phi.min) / (phi.max - phi.min)
        phi.curr.tr <- (phi.curr - phi.min) / (phi.max - phi.min)
        log.dens.prop <- stats::dbeta(x = phi.prop.tr,
                                      shape1 = shape1,
                                      shape2 = shape2,
                                      log = TRUE)
        log.dens.curr <- stats::dbeta(x = phi.curr.tr,
                                      shape1 = shape1,
                                      shape2 = shape2,
                                      log = TRUE)
        log.diff <- log.dens.prop - log.dens.curr
        accept <- (log.diff >= 0) || (stats::runif(1L) < exp(log.diff))
        if (accept)
            prior@phi <- phi.prop
        prior
    }
}

updateOmegaAlpha <- function(prior, withTrend, useC = FALSE) {
    ## 'prior'
    stopifnot(methods::is(prior, "DLM"))
    stopifnot(methods::validObject(prior))
    ## 'withTrend'
    stopifnot(is.logical(withTrend))
    stopifnot(identical(length(withTrend), 1L))
    stopifnot(!is.na(withTrend))
    ## 'prior' and 'withTrend'
    stopifnot((withTrend && methods::is(prior, "WithTrendMixin"))
              || (!withTrend && methods::is(prior, "NoTrendMixin")))
    if (useC) {
        .Call(updateOmegaAlpha_R, prior, withTrend)
    }
    else {
        if (withTrend) {                          
            has.level <- prior@hasLevel@.Data     
            if (!has.level)                       
                return(prior)                     
        }                                         
        J <- prior@J@.Data
        K <- prior@K@.Data
        L <- prior@L@.Data
        alpha <- prior@alphaDLM@.Data
        omega <- prior@omegaAlpha@.Data
        omegaMax <- prior@omegaAlphaMax@.Data
        A <- prior@AAlpha@.Data
        nu <- prior@nuAlpha@.Data
        if (withTrend)
            delta <- prior@deltaDLM@.Data
        else
            phi <- prior@phi
        iterator <- prior@iteratorState
        is.struc.zero <- prior@isStrucZero ## NEW
        iterator <- resetA(iterator)
        V <- 0
        n <- 0L ## NEW?
        for (l in seq_len(L)) {
            if (!is.struc.zero[l]) { ## NEW
                indices <- iterator@indices
                for (i in seq_len(K)) {
                    k.curr <- indices[i + 1]
                    k.prev <- indices[i]
                    if (withTrend)
                        V <- V + (alpha[k.curr] - alpha[k.prev] - delta[k.prev])^2
                    else
                        V <- V + (alpha[k.curr] - phi * alpha[k.prev])^2
                    n <- n + 1L ## NEW?
                }
                iterator <- advanceA(iterator)
            } ## NEW
        }
        omega <- updateSDNorm(sigma = omega,
                              A = A,
                              nu = nu,
                              V = V,
                              n = n,
                              max = omegaMax)
        successfully.updated <- omega > 0
        if (successfully.updated)
            prior@omegaAlpha@.Data <- omega
        prior
    }
}


## TRANSLATED
## HAS_TESTS
updateOmegaDelta <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "WithTrendMixin"))
    stopifnot(methods::validObject(prior))
    if (useC) {
        .Call(updateOmegaDelta_R, prior)
    }
    else {
        J <- prior@J@.Data
        K <- prior@K@.Data
        L <- prior@L@.Data
        delta <- prior@deltaDLM@.Data
        phi <- prior@phi
        omega <- prior@omegaDelta@.Data
        omegaMax <- prior@omegaDeltaMax@.Data
        A <- prior@ADelta@.Data
        nu <- prior@nuDelta@.Data
        iterator <- prior@iteratorState
        is.struc.zero <- prior@isStrucZero ## NEW
        iterator <- resetA(iterator)
        V <- 0
        n <- 0L ## NEW
        for (l in seq_len(L)) {
            if (!is.struc.zero[l]) { ## NEW
                indices <- iterator@indices
                for (i in seq_len(K)) {
                    k.curr <- indices[i + 1]
                    k.prev <- indices[i]
                    V <- V + (delta[k.curr] - phi * delta[k.prev])^2
                    n <- n + 1L ## NEW
                }
                iterator <- advanceA(iterator)
            } ## NEW
        }
        omega <- updateSDNorm(sigma = omega,
                              A = A,
                              nu = nu,
                              V = V,
                              n = n, ## NEW
                              max = omegaMax)
        successfully.updated <- omega > 0
        if (successfully.updated)
            prior@omegaDelta@.Data <- omega
        prior
    }
}





## TRANSLATED
## HAS_TESTS
updateSeason <- function(prior, betaTilde, useC = FALSE) {
    ## prior
    stopifnot(methods::is(prior, "SeasonMixin"))
    stopifnot(methods::validObject(prior))
    ## betaTilde
    stopifnot(is.double(betaTilde))
    stopifnot(!any(is.na(betaTilde)))
    if (useC) {
        .Call(updateSeason_R, prior, betaTilde)
    }
    else {    
        K <- prior@K@.Data
        L <- prior@L@.Data
        n.season <- prior@nSeason@.Data 
        s <- prior@s@.Data       # length (K+1)L list of vectors of length nSeason
        m <- prior@mSeason@.Data # length K+1 list of vectors of length nSeason
        m0 <- prior@m0Season@.Data # length L list of vectors of length nSeason
        C <- prior@CSeason@.Data # length K+1 list of vectors (not matrices) of length nSeason
        a <- prior@aSeason@.Data # length K list of vectors of length nSeason
        R <- prior@RSeason@.Data # length K list of vectors (not matrices) of length nSeason
        v <- getV(prior)         # numeric vector of length KL
        omega <- prior@omegaSeason@.Data
        omega.sq <- omega^2
        iterator.s <- prior@iteratorState
        iterator.v <- prior@iteratorV
        is.struc.zero <- prior@isStrucZero ## NEW
        iterator.s <- resetA(iterator.s)
        iterator.v <- resetA(iterator.v)
        for (l in seq_len(L)) {
            if (!is.struc.zero[l]) { ## NEW
                indices.s <- iterator.s@indices
                indices.v <- iterator.v@indices
                m[[1L]] <- m0[[l]]
                ## forward filter
                for (i in seq_len(K)) {
                    j <- indices.v[i]
                    for (i.n in seq_len(n.season - 1L)) {
                        a[[i]][i.n + 1L] <- m[[i]][i.n]
                        R[[i]][i.n + 1L] <- C[[i]][i.n]
                    }
                    a[[i]][1L] <- m[[i]][n.season]
                    R[[i]][1L] <- C[[i]][n.season] + omega^2
                    q <- R[[i]][1L] + v[j]
                    e <- betaTilde[j] - a[[i]][1L]
                    Ae1 <- R[[i]][1L] * e / q
                    m[[i + 1L]] <- a[[i]]
                    m[[i + 1L]][1L] <- m[[i + 1L]][1L] + Ae1
                    AAq1 <- (R[[i]][1L])^2 / q
                    C[[i + 1L]] <- R[[i]]
                    C[[i + 1L]][1L] <- C[[i + 1L]][1L] - AAq1 
                }                
                ## draw final value for 's'
                for (i.n in seq_len(n.season)) {
                    i.curr <- indices.s[K + 1L]
                    mean <- m[[K + 1L]][i.n]
                    sd <- sqrt(C[[K + 1L]][i.n])
                    s[[i.curr]][i.n] <- stats::rnorm(n = 1L, mean = mean, sd = sd)
                }
                ## backward smooth
                for (i in seq.int(from = K, to = 1L)) {
                    i.prev <- indices.s[i + 1L]
                    i.curr <- indices.s[i]
                    s[[i.curr]][-n.season] <- s[[i.prev]][-1L]
                    lambda <- C[[i]][n.season] / (C[[i]][n.season] + omega.sq)
                    mean <- lambda * s[[i.prev]][1L] + (1 - lambda) * m[[i]][n.season]
                    sd <- sqrt(lambda) * omega
                    s[[i.curr]][n.season] <- stats::rnorm(n = 1L, mean = mean, sd = sd)
                }
                iterator.s <- advanceA(iterator.s)
                iterator.v <- advanceA(iterator.v)
            } ## NEW
        }
        prior@s@.Data <- s
        prior
    }
}


## TRANSLATED
## HAS_TESTS
updateOmegaSeason <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "SeasonMixin"))
    stopifnot(methods::validObject(prior))
    if (useC) {
        .Call(updateOmegaSeason_R, prior)
    }
    else {
        J <- prior@J@.Data
        K <- prior@K@.Data
        L <- prior@L@.Data
        s <- prior@s@.Data
        n.season <- prior@nSeason@.Data
        omega <- prior@omegaSeason@.Data
        omegaMax <- prior@omegaSeasonMax@.Data
        A <- prior@ASeason@.Data
        nu <- prior@nuSeason@.Data
        iterator <- prior@iteratorState
        is.struc.zero <- prior@isStrucZero ## NEW
        iterator <- resetA(iterator)
        V <- 0
        n <- 0L ## NEW
        for (l in seq_len(L)) {
            if (!is.struc.zero[l]) { ## NEW
                indices <- iterator@indices
                for (i in seq_len(K)) {
                    i.curr <- indices[i + 1L]
                    i.prev <- indices[i]
                    curr <- s[[i.curr]][1L]
                    prev <- s[[i.prev]][n.season]
                    V <- V + (curr - prev)^2
                    n <- n + 1L ## NEW
                }
                iterator <- advanceA(iterator)
            } ## NEW
        }
        omega <- updateSDNorm(sigma = omega,
                              A = A,
                              nu = nu,
                              V = V,
                              n = n, ## NEW
                              max = omegaMax)
        successfully.updated <- omega > 0
        if (successfully.updated)
            prior@omegaSeason@.Data <- omega
        prior
    }
}
