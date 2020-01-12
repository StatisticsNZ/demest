

setClass("AlphaLN2Mixin",
         slots = c(alpha = "numeric",
                   transformAlpha = "CollapseTransform",
                   truncation = "integer"),
         contains = "VIRTUAL",
         validity = function(object) {
             theta <- object@theta
             thetaTransformed <- object@thetaTransformed
             logPostTheta <- object@logPostTheta
             metadataY <- object@metadataY
             ## 'theta' is double
             if (!is.double(theta))
                 return(gettextf("'%s' does not have type \"%s\"",
                                 "theta", "double"))
             ## 'thetaTransformed' is double
             if (!is.double(thetaTransformed))
                 return(gettextf("'%s' does not have type \"%s\"",
                                 "thetaTransformed", "double"))
             ## 'thetaTransformed' and 'theta' have same length
             if (!identical(length(theta), length(thetaTransformed)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "theta", "thetaTransformed"))
             ## dimensions of 'metadataY' consistent with length of 'theta'
             if (!identical(as.integer(prod(dim(metadataY))), length(theta)))
                 return(gettextf("dimensions of '%s' inconsistent with length of '%s'",
                                 "metadataY", "theta"))
             TRUE
         })


setClass("LN2",
         slots = c(alphaLN2 = "numeric",
                   alphaIsZeroLN2 = "logical",
                   metadataAlphaLN2 = "MetaData",
                   transformAlphaLN2 = "CollapseTransform",
                   truncationLN2 = "integer"),
         contains = c("Model",
                      "ASigmaMixin",
                      "CellInLikMixin",
                      "MaxAttemptMixin",
                      "NuSigmaMixin",
                      "SigmaMaxMixin",
                      "SigmaMixin"))



setMethod("updateModelUseExp",
          signature(object = "LN2"),
          function(object, y, exposure, useC = FALSE, useSpecific = FALSE) {
              ## object
              stopifnot(methods::validObject(object))
              ## y
              stopifnot(identical(length(y), length(object@theta)))
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
                  object <- updateAlphaLN2(object,
                                           y = y,
                                           exposure = exposure)
                  object <- updateSigmaLN2(object)
                  object <- updateSDAlphaLN2(object)
                  object
              }
          })


updateAlphaLN2 <- function(object, y, exposure) {
    alpha <- objecty@alphaLN2
    alpha.is.zero <- object@alphaIsZeroLN2
    cell.in.lik <- object@cellInLik
    n.cell.vec <- object@nCellLN2
    transform <- object@transformAlphaLN2
    truncation <- object@truncationLN2
    sigma <- object@sigmaLN2
    sd <- object@sdAlphaLN2
    for (i.alpha in seq_along(alpha)) {
        if (!alpha.is.zero[i.alpha]) {
            i.y.vec <- dembase::getIBefore(i = i.alpha,
                                           transform = transform)
            n.cell <- n.cell.vec[i.alpha]
            trunc <- truncation[i.alpha]
            prec.data  <- n.cell / sigma^2
            prec.prior <- 1 / sd^2
            V <- 1 / (prec.data + prec.prior)
            sum.y <- sum(log1p(y[i.y.vec]))
            sum.expose  <- sum(log1p(exposure[i.y.vec]))
            m <- prec.data * V * (sum.y - sum.expose) / n.cell.vec
            sd <- sqrt(V)
            if (trunc == -1L) {
                alpha[i.alpha] <- rtnorm1(mean = m,
                                          sd = sd,
                                          lower = -Inf,
                                          upper = 0)
            }
            else if (trunc == 0L) {
                alpha[i.alpha] <- stats::rnorm(n = 1L,
                                               mean = m,
                                               sd = sd)
            }
            else {
                alpha[i.alpha] <- rtnorm1(mean = m,
                                          sd = sd,
                                          lower = 0,
                                          upper = Inf)
            }
        }
    }
    object@alphaLN2 <- alpha
    object
}    
        
updateSigmaLN2 <- function(object, y, exposure, useC = FALSE) {
    ## object
    stopifnot(methods::is(object, "LN2"))
    stopifnot(methods::validObject(object))
    ## ALSO NEED TO CHECK Y AND EXPOSURE
    if (useC) {
        .Call(updateSigmaLN2_R, object)
    }
    else {
        sigma <- object@sigma@.Data
        sigma.max <- object@sigmaMax@.Data
        A <- object@ASigma@.Data
        nu <- object@nuSigma@.Data
        alpha <- objecty@alphaLN2
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

updateSDAlphaN2 <- function(object, useC = FALSE) {
    ## object
    stopifnot(methods::is(object, "LN2"))
    stopifnot(methods::validObject(object))
    if (useC) {
        .Call(updateSDAlphaLN2_R, object)
    }
    else {
        alpha <- object@alphaLN2@.Data
        alpha.is.zero <- object@alphaIsZeroLN2@.Data
        SD <- object@sdAlphaLN2@.Data
        sd.max <- object@sdAlphaLN2@.Data
        A <- object@ASDAlphaLN2@.Data
        nu <- object@nuSDAlphaLN2@.Data
        V <- 0
        n <- 0L
        for (i in seq_along(alpha)) {
            if (!alpha.is.zero) {
                V <- V + alpha[i]^2
                n <- n + 1L
            }
        }
        sigma <- updateSDNorm(sigma = sd,
                              A = A,
                              nu = nu,
                              V = V,
                              n = n,
                              max = sd.max)
        successfully.updated <- sd > 0
        if (successfully.updated)
            object@sdAlphaLN2@.Data <- sd
        object
    }
}

