
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
    ans <- c("(Intercept)", ans)
    ans <- paste(ans, collapse = " + ")
    ans
}

printAggregateEqns <- function(object) {
    if (methods::extends(class(object), "Aggregate")) {
        printAgValEqns(object)
        printAgAccuracyEqns(object)
    }
    else
        invisible()
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
    name.y <- deparse(call$formula[[2L]])
    if (is.null(series)) {
        if (identical(name.y, "y"))
            exposure <- "exposure"
        else
            exposure <- "y"
    }
    else
        exposure <- series
    name.y <- sprintf("%13s", name.y)
    lower <- invlogit1(lower)
    upper <- invlogit1(upper)
    terms <- expandTermsMod(names)
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

printCMPLikEqns <- function(object) {
    formulaMu <- object@formulaMu
    useExpose <- object@useExpose@.Data
    mean <- object@meanLogNuCMP@.Data
    sd <- object@sdLogNuCMP@.Data
    terms <- expandTermsSpec(formulaMu)
    if (useExpose) {
        cat("              y[i] ~ CMP(rate[i] * exposure[i], dispersion[i])\n")
        cat("      log(rate[i]) ~ N(", terms, ", sd^2)\n", sep = "")
    }
    else {
        cat("              y[i] ~ CMP(count[i], dispersion[i])\n")
        cat("     log(count[i]) ~ N(", terms, ", sd^2)\n", sep = "")
    }
    cat("log(dispersion[i]) ~ N(", mean, ", ", squaredOrNA(sd), ")\n", sep = "")
}

printCMPModEqns <- function(object) {
    call <- object@call
    mean <- object@meanLogNuCMP@.Data
    sd <- object@sdLogNuCMP@.Data
    lower <- object@lower
    upper <- object@upper
    names <- object@namesBetas
    uses.exposure <- methods::is(object, "UseExposure")
    series <- call$series
    name.y <- deparse(call$formula[[2L]])
    if (is.null(series)) {
        if (identical(name.y, "y"))
            exposure <- "exposure"
        else
            exposure <- "y"
    }
    else
        exposure <- series
    name.y <- sprintf("%15s", name.y)
    lower <- exp(lower)
    upper <- exp(upper)
    terms <- expandTermsMod(names)
    if (uses.exposure) {
        cat(name.y, "[i] ~ CMP(rate[i] * ", exposure, "[i])", sep = "")
        if (lower > 0 || is.finite(upper))
            cat(",  ", format(lower, digits = 4), "< rate[i] <", format(upper, digits = 4))
        cat("\n")
        cat("      log(rate[i]) ~ N(", terms, ", sd^2)\n", sep = "")
    }
    else {
        cat(name.y, "[i] ~ CMP(count[i])", sep = "")
        if ((lower > 0) || (is.finite(upper)))
            cat(",  ", format(lower, digits = 4), "< count[i] <", format(upper, digits = 4))
        cat("\n")
        cat("     log(count[i]) ~ N(", terms, ", sd^2)\n", sep = "")
    }
    cat("log(dispersion[i]) ~ N(", mean, ", ", squaredOrNA(sd), ")\n", sep = "")
}

printCMPSpecEqns <- function(object) {
    formulaMu <- object@formulaMu
    nameY <- object@nameY
    series <- object@series@.Data
    lower <- object@lower
    upper <- object@upper
    useExpose <- object@useExpose@.Data
    mean <- object@meanLogNuCMP@.Data
    sd <- object@sdLogNuCMP@.Data
    has.series <- !is.na(series)
    name.y <- sprintf("%15s", nameY)
    terms <- expandTermsSpec(formulaMu)
    if (useExpose) {
        if (has.series)
            exposure <- series
        else
            exposure <- "exposure"
        cat(name.y, "[i] ~ CMP(rate[i] * ", exposure, "[i], dispersion[i])", sep = "")
        if (lower > 0 || is.finite(upper))
            cat(",  ", format(lower, digits = 4), "< rate[i] <", format(upper, digits = 4))
        cat("\n")
        cat("      log(rate[i]) ~ N(", terms, ", sd^2)\n", sep = "")
    }
    else {
        cat("              y[i] ~ CMP(count[i], dispersion[i])")
        if (lower > 0 || is.finite(upper))
            cat(",  ", format(lower, digits = 4), "< count[i] <", format(upper, digits = 4))
        cat("\n")
        cat("     log(count[i]) ~ N(", terms, ", sd^2)  \n", sep = "")
    }
    cat("log(dispersion[i]) ~ N(", mean, ", ", squaredOrNA(sd), ")\n", sep = "")
}


printCovariatesEqns <- function(object) {
    AEtaCoef <- object@AEtaCoef@.Data
    meanEtaCoef <- object@meanEtaCoef@.Data
    nuEtaCoef <- object@nuEtaCoef@.Data
    n <- length(nuEtaCoef)
    cat("    covariate[j] ~ (Intercept) + data[j,] * coef\n")
    if (n == 1L)
        cat("            coef ~ t(", nuEtaCoef, ", ", meanEtaCoef, ", ", squaredOrNA(AEtaCoef), ")\n",
            sep = "")
    else
        cat("            coef ~ t([", paste(nuEtaCoef, collapse = ","), "], [",
            paste(meanEtaCoef, collapse = ","), "], [",
            paste(sapply(AEtaCoef, squaredOrNA), collapse = ","), "])\n",
            sep = "")
}

printCovariatesDLMEqns <- function(object, isMain) {
    AEtaCoef <- object@AEtaCoef@.Data
    meanEtaCoef <- object@meanEtaCoef@.Data
    nuEtaCoef <- object@nuEtaCoef@.Data
        n <- length(nuEtaCoef)
    if (isMain)
        cat("    covariate[j] ~ (Intercept) + data[j,] * coef\n")
    else
        cat("  covariate[k,l] ~ (Intercept) + data[k,l,] * coef\n")
    if (n == 1L)
        cat("            coef ~ t(", nuEtaCoef, ", ", meanEtaCoef, ", ", squaredOrNA(AEtaCoef), ")\n",
            sep = "")
    else
        cat("            coef ~ t([", paste(nuEtaCoef, collapse = ","), "], [",
            paste(meanEtaCoef, collapse = ","), "], [",
            paste(sapply(AEtaCoef, squaredOrNA), collapse = ","), "])\n",
            sep = "")
}

printDLMEqns <- function(object, name, order, hasTrend, hasSeason, hasCovariates) {
    is.main <- order == 1L
    if (is.null(name))
        name <- "parameter"
    if (is.main) {
        name <- sprintf("%13s", name, sep = "")
        cat(name, "[j] = level[j] + ", sep = "")
    }
    else {
        name <- sprintf("%11s", name, sep = "")
        cat(name, "[k,l] = level[k,l] + ", sep = "")
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
    A <- object@ATau@.Data
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
    A <- object@ATau@.Data
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
    cat(name, "[j] = ", sep = "")
    if (hasCovariates)
        cat("covariate[j] + ")
    cat("error[j]\n")
    if (hasCovariates)
        printCovariatesEqns(object)
    printErrorEqns(object)
}

printExchFixedEqns <- function(object, name) {
    mean <- object@mean@.Data
    sd <- object@tau@.Data
    if (is.null(name))
        name <- "parameter"
    name <- sprintf("%13s", name) 
    cat(name, "[j] ~ N(", mean, ", ", squaredOrNA(sd), ")\n", sep = "")
}

printJump <- function(object) {
    aggregate <- object@aggregate
    scale.theta <- stringScaleTheta(object)
    scale.ag <- stringScaleAg(aggregate)
    print.scale.theta <- nzchar(scale.theta)
    print.scale.ag <- nzchar(scale.ag)
    if (print.scale.theta || print.scale.ag) {
        cat("\nMH jump:\n")
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
    if (methods::.hasSlot(object, "weightAg"))
        has.weight <- !is.null(object@weightAg)
    else
        has.weight <- FALSE
    value.is.scalar <- identical(length(value), 1L)
    if (!value.is.scalar || has.weight)
        cat("\n")
    cat("MH jump:", jump, "\n")
}

printKnownEqns <- function(object, name) {
    is.uncertain <- (methods::is(object, "SpecKnownUncertain")
        || methods::is(object, "KnownUncertain"))
    if (is.null(name))
        name <- "parameter"
    name <- sprintf("%13s", name)
    if (is.uncertain)
        cat(name, "[j] ~ N(mean[j], sd[j]^2)\n", sep = "")
    else
        cat(name, "[j] = mean[j]\n", sep = "")
}

printLN2LikEqns  <- function(object) {
    updateVarsigma <- object@updateVarsigmaLN2@.Data
    varsigma <- object@varsigmaLN2@.Data
    hasHalfT <- object@varsigmaLN2HasHalfT@.Data
    nuVarsigma <- object@nuVarsigma@.Data
    AVarsigma <- object@AVarsigma@.Data
    varsigmaMax <- object@varsigmaMax@.Data
    cat("   log(y[i] + 1) ~ N(log(exposure[i] + 1) + alpha[j[i]], ")
    if (updateVarsigma)
        cat("sdData^2)\n")
    else
        cat(format(varsigma, digits = 4), "^2)\n", sep = "")
    cat("        alpha[i] ~ N*(0, sdAlpha^2)\n", sep = "")
    if (updateVarsigma) {
        if (hasHalfT) {
            cat("          sdData ~ trunc-half-t(", nuVarsigma, ", ", sep = "")
            cat(squaredOrNA(AVarsigma), ", ", format(varsigmaMax, digits = 4), ")\n", sep = "")
        }
        else {
            cat("          sdData ~ trunc-inv-chi-sq(", nuVarsigma, ", ", sep = "")
            cat(format(AVarsigma, digits = 4), ", ", format(varsigmaMax, digits = 4), ")\n", sep = "")
        }
    }
}

printLN2ModEqns <- function(object) {
    call <- object@call
    series <- call$series
    ASigma <- object@ASigma@.Data
    sigmaMax <- object@sigmaMax
    nuSigma <- object@nuSigma
    updateVarsigma <- object@updateVarsigmaLN2@.Data
    hasHalfT <- object@varsigmaLN2HasHalfT@.Data
    varsigma <- object@varsigmaLN2@.Data    
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
    cat(name.y, response, " ~ N(log(", exposure, "[i] + 1) + alpha[j[i]], ", sep = "")
    if (updateVarsigma)
        cat("sdData^2)\n")
    else
        cat(format(varsigma, digits = 4), "^2)\n")
    cat("      alpha[j] ~ N*(0, sdAlpha^2)")
    if (updateVarsigma) {
        if (hasHalfT) {
            cat("      sdData ~ trunc-half-t(", nuVarsigma, ", ", sep = "")
            cat(squaredOrNA(AVarsigma),
                ", ",
                format(varsigmaMax, digits = 4),
                ")\n",
                sep = "")
        }
        else {
            cat("      sdData ~ trunc-inv-chi-sq(", nuVarsigma, ", ", sep = "")
            cat(format(AVarsigma, digits = 4),
                ", ",
                format(varsigmaMax, digits = 4),
                ")\n",
                sep = "")
        }
    }
    cat("             sd ~ trunc-half-t(", nuSigma, ", ", sep = "")
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
    updateVarsigma <- object@updateVarsigmaLN2@.Data
    hasHalfT <- object@varsigmaLN2HasHalfT@.Data
    varsigma <- object@varsigmaLN2@.Data    
    varsigmaMax <- object@varsigmaMax
    nuVarsigma <- object@nuVarsigma
    nameY <- object@nameY
    has.series <- !is.na(series)
    name.y <- deparse(call$formula[[2L]])
    if (has.series)
        exposure <- series        
    else
        exposure <- "exposure"
    n.spaces <- max(5L - nchar(name.y), 0L)
    spaces <- paste(rep(" ", n.spaces), collapse = "")
    response <- sprintf("%slog(%s + 1)", spaces, name.y)
    cat(response, " ~ N(log(", exposure, "[i] + 1) + alpha[j[i]], ", sep = "")
    if (updateVarsigma)
        cat("sdData^2)\n")
    else
        cat(format(varsigma, digits = 4), "^2)\n", sep = "")
    cat("      alpha[j] ~ N*(0, sd^2)\n")
    if (updateVarsigma) {
        if (hasHalfT) {
            cat("        sdData ~ trunc-half-t(", nuVarsigma, ", ", sep = "")
            cat(squaredOrNA(AVarsigma),
                ", ",
                format(varsigmaMax, digits = 4),
                ")\n",
                sep = "")
        }
        else {
            cat("        sdData ~ trunc-inv-chi-sq(", nuVarsigma, ", ", sep = "")
            cat(format(AVarsigma, digits = 4),
                ", ",
                format(varsigmaMax, digits = 4),
                ")\n",
                sep = "")
        }
    }
    cat("    sd ~ trunc-half-t(", nuSigma, ", ", sep = "")
    cat(squaredOrNA(ASigma),
        ", ",
        format(sigmaMax, digits = 4),
        ")\n",
        sep = "")
}



printLevelTrendEqns <- function(object, isMain, hasTrend) {
    AAlpha <- object@AAlpha@.Data
    omegaAlphaMax <- object@omegaAlphaMax
    nuAlpha <- object@nuAlpha
    phi <- object@phi
    phi.known <- object@phiKnown
    min.phi <- object@minPhi
    max.phi <- object@maxPhi
    shape1 <- object@shape1Phi@.Data
    shape2 <- object@shape2Phi@.Data
    is.spec <- methods::is(object, "SpecPrior")
    if (hasTrend) {
        has.level <- object@hasLevel@.Data
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
        has.level <- FALSE
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
            cat("        level[j] = level[j-1] + trend[j-1]")
            if (has.level)
                cat(" + errorLevel[j]\n")
            else
                cat("\n")
            cat("        trend[j] = ")
        }
        else {
            cat("      level[k,l] = level[k-1,l] + trend[k-1,l]")
            if (has.level)
                cat("+ errorLevel[k,l]\n")
            else
                cat("\n")
            cat("      trend[k,l] = ")
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
            cat("        level[j] = ")
        else
            cat("      level[k,l] = ")
        if (show.damp)
            cat("damp * ")
        if (isMain)
            cat("level[j-1] + errorLevel[j]\n")
        else
            cat("level[k-1,l] + errorLevel[k,l]\n")
    }
    if (isMain) {
        if (!hasTrend || (hasTrend && has.level))
            cat("        level[0] ~ N(0, ", squaredOrNA(AAlpha0), ")\n", sep = "")
        else
            cat("        level[0] = 0\n")
        if (hasTrend) {
            cat("        trend[0] ~ N(", meanDelta0, ", ", sep = "")
            cat(squaredOrNA(ADelta0), ")\n", sep = "")
        }
    }
    else {
        if (!hasTrend || (hasTrend && has.level))
            cat("      level[0,l] ~ N(0, ", squaredOrNA(AAlpha0), ")\n", sep = "")
        else
            cat("      level[0,l] = 0\n")
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
        else {
            cat("   dampTransform = (damp-",
                format(min.phi, digits = 4),
                ")/(",
                format(max.phi, digits = 4),
                "-",
                format(min.phi, digits = 4),
                ")\n",
                sep = "")
            cat("   dampTransform ~ Beta(",
                format(shape1, digits = 4),
                ",",
                format(shape2, digits = 4),
                ")\n",
                sep = "")
        }
    }
    if (isMain) {
        if (!hasTrend || (hasTrend && has.level))
            cat("   errorLevel[j] ~ N(0, scaleLevel^2)\n")
    }
    else {
        if (!hasTrend || (hasTrend && has.level))
            cat(" errorLevel[k,l] ~ N(0, scaleLevel^2)\n")
    }
    if (hasTrend) {
        if (isMain)
            cat("   errorTrend[j] ~ N(0, scaleTrend^2)\n")
        else
            cat(" errorTrend[k,l] ~ N(0, scaleTrend^2)\n")
    }
    if (!hasTrend || (hasTrend && has.level)) {
        cat("      scaleLevel ~ trunc-half-t(", nuAlpha, ", ", sep = "")
        cat(squaredOrNA(AAlpha),
            ", ",
            format(omegaAlphaMax, digits = 4),
            ")\n",
            sep = "")
    }
    if (hasTrend) {
        cat("      scaleTrend ~ trunc-half-t(", nuDelta, ", ", sep = "")
        cat(squaredOrNA(ADelta),
            ", ",
            format(omegaDeltaMax, digits = 4),
            ")\n",
            sep = "")
    }
}

printMixEqns <- function(object, name, hasCovariates) {
    AVectors <- object@AVectorsMix@.Data
    nuVectors <- object@nuVectorsMix@.Data
    omegaVectorsMax <- object@omegaVectorsMaxMix@.Data
    priorMean <- object@priorMeanLevelComponentWeightMix@.Data
    priorSD <- object@priorSDLevelComponentWeightMix@.Data
    AComponentWeight <- object@AComponentWeightMix@.Data
    nuComponentWeight <- object@nuComponentWeightMix@.Data
    omegaComponentWeightMax <- object@omegaComponentWeightMaxMix@.Data
    ALevelComponentWeight <- object@ALevelComponentWeightMix@.Data
    nuLevelComponentWeight <- object@nuLevelComponentWeightMix@.Data
    omegaLevelComponentWeightMax <- object@omegaLevelComponentWeightMaxMix@.Data
    phi <- object@phi
    phi.known <- object@phiKnown
    min.phi <- object@minPhi
    max.phi <- object@maxPhi
    shape1 <- object@shape1Phi@.Data
    shape2 <- object@shape2Phi@.Data
    if (is.null(name))
        name <- "parameter"
    name <- sprintf("%11s", name, sep = "")
    cat(name, "[k,l] ~ Mix(components, weights, ", sep = "")
    if (hasCovariates)
        cat("covariates, ")
    cat("error)\n")
    cat("       component ~ N(0, scaleComponent^2)\n")
    cat("  scaleComponent ~ trunc-half-t(", nuVectors, ", ", sep = "")
    cat(squaredOrNA(AVectors),
        ", ",
        format(omegaVectorsMax, digits = 4),
        ")\n",
        sep = "")
    cat("     weight[k,h] = g(level1[k,1], ..., level1[k,h])\n")
    cat("     level1[k,h] = level2[k,h] + error1[k,h]\n")
    cat("     level2[k,h] = mean + damp * level2[k-1,h] + error2[k,h]\n")
    cat("     level2[1,h] ~ N(", priorMean, "/(1-damp), ", sep = "")
    cat(priorSD, "/sqrt(1-damp^2))\n", sep = "")
    cat("            mean ~ N(", priorMean, ", ", squaredOrNA(priorSD), ")\n", sep = "")
    if (phi.known)
        cat("            damp =",
            format(phi, digits = 4),
            "\n")
    else {
        cat("   dampTransform = (damp-",
            format(min.phi, digits = 4),
            ")/(",
            format(max.phi, digits = 4),
            "-",
            format(min.phi, digits = 4),
            ")\n",
            sep = "")
        cat("   dampTransform ~ Beta(",
            format(shape1, digits = 4),
            ",",
            format(shape2, digits = 4),
            ")\n",
            sep = "")
    }
    cat("          error1 ~ N(0, scale1^2)\n")
    cat("          error2 ~ N(0, scale2^2)\n")
    cat("          scale1 ~ trunc-half-t(", nuComponentWeight, ", ", sep = "")
    cat(squaredOrNA(AComponentWeight),
        ", ",
        format(omegaComponentWeightMax, digits = 4),
        ")\n",
        sep = "")
    cat("          scale2 ~ trunc-half-t(", nuLevelComponentWeight, ", ", sep = "")
    cat(squaredOrNA(ALevelComponentWeight),
        ", ",
        format(omegaLevelComponentWeightMax, digits = 4),
        ")\n",
        sep = "")
    if (hasCovariates)
        printCovariatesDLMEqns(object = object,
                               isMain = FALSE)
    printErrorDLMEqns(object,
                      isMain = FALSE)
}

printNormalFixedLikEqns <- function(object) {
    useExpose <- object@useExpose@.Data
    if (useExpose)
        cat("            y[i] ~ Normal(exposure[i] * mean[i], sd[i]^2)\n")
    else
        cat("            y[i] ~ Normal(mean[i], sd[i]^2)\n")
}

printNormalFixedModEqns <- function(object) {
    call <- object@call
    uses.exposure <- methods::is(object, "UseExposure")
    series <- call$series
    name.y <- deparse(call$formula[[2L]])
    if (is.null(series)) {
        if (identical(name.y, "y"))
            exposure <- "exposure"
        else
            exposure <- "y"
    }
    else
        exposure <- series
    name.y <- sprintf("%13s", name.y)
    if (uses.exposure)
        cat(name.y, "[i] ~ Normal(", exposure, "[i] * mean[i], sd[i]^2)\n", sep = "")
    else
        cat(name.y, "Normal(mean[i], sd[i]^2)\n", sep = "")
}

printNormalFixedSpecEqns <- function(object) {
    series <- object@series@.Data
    call <- object@call
    nameY <- object@nameY
    useExpose <- object@useExpose@.Data
    has.series <- !is.na(series)
    name.y <- deparse(call$formula[[2L]])
    name.y <- sprintf("%13s", nameY)
    if (useExpose) {
        if (has.series)
            exposure <- series        
        else
            exposure <- "exposure"
        cat(name.y, "[i] ~ Normal(", exposure, "[i] * mean[i], sd[i]^2)\n", sep = "")
    }
    else
        cat("            y[i] ~ Normal(mean[i], sd[i]^2)\n")
}

printNormalVarsigmaKnownLikEqns <- function(object) {
    formulaMu <- object@formulaMu
    varsigma <- object@varsigma
    varsigmaSetToZero <- object@varsigmaSetToZero@.Data
    terms <- expandTermsSpec(formulaMu)
    if (varsigmaSetToZero)
        cat("            y[i] = mean[i]\n", sep = "")
    else
        cat("            y[i] ~ N(mean[i], ", varsigma, "^2 / weights[i])\n", sep = "")
    cat("         mean[i] ~ N(", terms, ", sd^2)\n", sep = "")
}

printNormalVarsigmaUnknownLikEqns <- function(object) {
    formulaMu <- object@formulaMu
    nu <- object@nuVarsigma@.Data
    A <- object@AVarsigma@.Data
    max <- object@varsigmaMax@.Data
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
    varsigmaSetToZero <- object@varsigmaSetToZero@.Data
    series <- call$series
    has.series <- !is.null(series)
    name.y <- deparse(call$formula[[2L]])
    name.y <- sprintf("%13s", name.y)
    terms <- expandTermsMod(names)
    if (varsigmaSetToZero)
        cat(name.y, "[i] = mean[i]", sep = "")
    else
        cat(name.y, "[i] ~ N(mean[i], ", varsigma, "^2 / weight[i])", sep = "")
    if (is.finite(lower) || is.finite(upper))
        cat(",  ", format(lower, digits = 4), "< mean[i] <", format(upper, digits = 4))
    cat("\n")
    cat("         mean[i] ~ N(", terms, ", sd^2)\n", sep = "")
}

printNormalVarsigmaKnownSpecEqns <- function(object) {
    formulaMu <- object@formulaMu
    varsigma <- object@varsigma
    varsigmaSetToZero <- object@varsigmaSetToZero@.Data
    nameY <- object@nameY
    lower <- object@lower
    upper <- object@upper
    name.y <- sprintf("%13s", nameY)
    terms <- expandTermsSpec(formulaMu)
    if (varsigmaSetToZero)
        cat(name.y, "[i] = mean[i]", sep = "")
    else
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
    nu <- object@nuVarsigma@.Data
    A <- object@AVarsigma@.Data
    max <- object@varsigmaMax@.Data
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
    nu <- object@nuVarsigma@.Data
    A <- object@AVarsigma@.Data
    max <- object@varsigmaMax@.Data
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

printPoissonBinomialModEqns <- function(object) {
    call <- object@call
    prob <- object@prob
    series <- call$series
    name.y <- deparse(call$formula[[2L]])
    if (is.null(series)) {
        if (identical(name.y, "y"))
            exposure <- "exposure"
        else
            exposure <- "y"
    }
    else
        exposure <- series
    name.y <- sprintf("%13s", name.y)
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
    useExpose <- object@useExpose@.Data
    structuralZeros <- object@structuralZeros
    terms <- expandTermsSpec(formulaMu)
    if (useExpose) {
        cat("            y[i] ~ Poisson(rate[i] * exposure[i])\n")
        cat("    log(rate[i]) ~ N(", terms, ", sd^2)\n", sep = "")
    }
    else {
        cat("            y[i] ~ Poisson(count[i])\n")
        cat("   log(count[i]) ~ N(", terms, ", sd^2)\n", sep = "")
    }
    cat("\nhas structural zeros: ", !is.null(structuralZeros), "\n", sep = "")
}

printPoissonModEqns <- function(object) {
    call <- object@call
    lower <- object@lower
    upper <- object@upper
    names <- object@namesBetas
    uses.exposure <- methods::is(object, "UseExposure")
    series <- call$series
    name.y <- deparse(call$formula[[2L]])
    if (is.null(series)) {
        if (identical(name.y, "y"))
            exposure <- "exposure"
        else
            exposure <- "y"
    }
    else
        exposure <- series
    name.y <- sprintf("%13s", name.y)
    lower <- exp(lower)
    upper <- exp(upper)
    terms <- expandTermsMod(names)
    if (uses.exposure) {
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
    useExpose <- object@useExpose@.Data
    structuralZeros <- object@structuralZeros
    has.series <- !is.na(series)
    name.y <- sprintf("%13s", nameY)
    terms <- expandTermsSpec(formulaMu)
    if (useExpose) {
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
        cat("            y[i] ~ Poisson(count[i])")
        if (lower > 0 || is.finite(upper))
            cat(",  ", format(lower, digits = 4), "< count[i] <", format(upper, digits = 4))
        cat("\n")
        cat("   log(count[i]) ~ N(", terms, ", sd^2)  \n", sep = "")
    }
    cat("\nhas structural zeros: ", !is.null(structuralZeros), "\n", sep = "")
}

printRound3LikEqns <- function(object) {
    cat("            y[i] ~ round3(exposure[i])\n")
}


printRound3ModEqns <- function(object) {
    call <- object@call
    series <- call$series
    name.y <- deparse(call$formula[[2L]])
    if (is.null(series)) {
        if (identical(name.y, "y"))
            exposure <- "exposure"
        else
            exposure <- "y"
    }
    else
        exposure <- series
    name.y <- sprintf("%13s", name.y)
    cat(name.y, "[i] ~ round3(", exposure, "[i])\n", sep = "")
}

printRound3SpecEqns <- function(object) {
    nameY <- object@nameY
    series <- object@series@.Data
    has.series <- !is.na(series)
    name.y <- sprintf("%13s", nameY)
    if (has.series)
        exposure <- series        
    else
        exposure <- "exposure"
    cat(name.y, "[i] ~ round3(", exposure, "[i])\n", sep = "")
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
    if (n >= 2L) {
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
    if (methods::.hasSlot(object, "priorsBetas")) {
        priors <- object@priorsBetas
        is.saturated <- any(sapply(priors, function(x) x@isSaturated@.Data))
    }
    else
        is.saturated <- FALSE
    if (is.saturated)
        invisible()
    else {
        nu <- object@nuSigma@.Data
        A <- object@ASigma@.Data
        max <- object@sigmaMax@.Data
        cat("              sd ~ trunc-half-t(", nu, ", ", sep = "")
        cat(squaredOrNA(A), ", ", format(max, digits = 4), ")\n", sep = "")
    }
}

printSeasonEqns <- function(object, isMain) {
    n <- object@nSeason@.Data
    nu <- object@nuSeason@.Data
    A <- object@ASeason@.Data
    max <- object@omegaSeasonMax@.Data
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

printTFixedLikEqns <- function(object) {
    df <- object@nu@.Data
    useExpose <- object@useExpose@.Data
    if (useExpose)
        cat("            y[i] ~ t(", df, ", exposure[i] * location[i], scale[i]^2)\n", sep = "")
    else
        cat("            y[i] ~ t(", df, ", location[i], scale[i]^2)\n", sep = "")
}


printTFixedModEqns <- function(object) {
    call <- object@call
    df <- object@nu@.Data
    uses.exposure <- methods::is(object, "UseExposure")
    series <- call$series
    name.y <- deparse(call$formula[[2L]])
    if (is.null(series)) {
        if (identical(name.y, "y"))
            exposure <- "exposure"
        else
            exposure <- "y"
    }
    else
        exposure <- series
    name.y <- sprintf("%13s", name.y)
    if (uses.exposure)
        cat(name.y, "[i] ~ t(", df, ", ", exposure, "[i] * location[i], scale[i]^2)\n", sep = "")
    else
        cat(name.y, "t(", df, ", location[i], scale[i]^2)\n", sep = "")
}

printTFixedSpecEqns <- function(object) {
    series <- object@series@.Data
    df <- object@nu@.Data
    call <- object@call
    nameY <- object@nameY
    useExpose <- object@useExpose@.Data
    has.series <- !is.na(series)
    name.y <- deparse(call$formula[[2L]])
    name.y <- sprintf("%13s", nameY)
    if (useExpose) {
        if (has.series)
            exposure <- series        
        else
            exposure <- "exposure"
        cat(name.y, "[i] ~ t(", df, ", ", exposure, "[i] * location[i], scale[i]^2)\n", sep = "")
    }
    else
        cat("            y[i] ~ t(", df, ", location[i], scale[i]^2)\n", sep = "")
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

printZeroEqns <- function(name) {
    if (is.null(name))
        name <- "parameter"
    name <- sprintf("%13s", name)
    cat(name, "[j] = 0\n", sep = "")
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


