
## Mixin class describing specific slots in priors

setClass("aNoTrendMixin",
         slots = c(aNoTrend = "FFBSList"),
         contains = "VIRTUAL",
         validity = function(object) {
             aNoTrend <- object@aNoTrend
             K <- object@K
             ## 'aNoTrend' has length K
             if (!identical(length(aNoTrend), as.integer(K)))
                 return(gettextf("'%s' does not have length '%s'",
                                 "aNoTrend", "K"))
             ## elements of 'aNoTrend' have length 1L
             if (!identical(length(aNoTrend[[1L]]), 1L))
                 return(gettextf("elements of '%s' do not have length %d",
                                 "aNoTrend", 1L))
             TRUE
         })

setClass("aWithTrendMixin",
         slots = c(aWithTrend = "FFBSList"),
         contains = "VIRTUAL",
         validity = function(object) {
             aWithTrend <- object@aWithTrend
             K <- object@K
             ## 'aWithTrend' has length K
             if (!identical(length(aWithTrend), as.integer(K)))
                 return(gettextf("'%s' does not have length '%s'",
                                 "aWithTrend", "K"))
             ## elements of 'aWithTrend' have length 2
             if (!identical(length(aWithTrend[[1L]]), 2L))
                 return(gettextf("elements of '%s' doe not have length %d",
                                 "aWithTrend", 2L))
             TRUE
         })


setClass("aSeasonMixin",
         slots = c(aSeason = "FFBSList"),
         contains = "VIRTUAL",
         validity = function(object) {
             aSeason <- object@aSeason
             K <- object@K
             nSeason <- object@nSeason
             ## 'aSeason' has length K
             if (!identical(length(aSeason), as.integer(K)))
                 return(gettextf("'%s' does not have length '%s'",
                                 "aSeason", "K"))
             ## elements of 'aSeason'  have length 'nSeason'
             if (!identical(length(aSeason[[1L]]), as.integer(nSeason)))
                 return(gettextf("elements of '%s' doe not have length '%s'",
                                 "aWithTrend", "nSeason"))
             TRUE
         })

setClass("AAlphaMixin",
         slots = c(AAlpha = "Scale"),
         contains = "VIRTUAL")

setClass("AMoveMixin",
         slots = c(AMove = "Scale"),
         contains = "VIRTUAL")

setClass("ADeltaMixin",
         slots = c(ADelta = "Scale"),
         contains = "VIRTUAL")

setClass("AEtaCoefMixin",
         slots = c(AEtaCoef = "Scale"),
         contains = "VIRTUAL")

setClass("AEtaInterceptMixin",
         slots = c(AEtaIntercept = "Scale"),
         contains = "VIRTUAL")

setClass("AKnownVecMixin",
         slots = c(AKnownVec = "ScaleVec"),
         contains = "VIRTUAL",
         validity = function(object) {
             AKnownVec <- object@AKnownVec
             J <- object@J
             ## 'AKnownVec' has length 'J'
             if (!identical(length(AKnownVec), as.integer(J)))
                 return(gettextf("'%s' does not have length '%s'",
                                 "AKnownVec", "J"))
             TRUE
         })

setClass("ASeasonMixin",
         slots = c(ASeason = "Scale"),
         contains = "VIRTUAL")

setClass("ATauMixin",
         slots = c(ATau = "Scale"),
         contains = "VIRTUAL")

setClass("AlongMixin",
         slots = c(along = "character"),
         contains = "VIRTUAL",
         validity = function(object) {
             along <- object@along
             ## 'along' has length 1
             if (!identical(length(along), 1L))
                 return(gettextf("'%s' does not have length %d",
                                 "along", 1L))
             if (!is.na(along)) {
                 ## 'along' not blank
                 if (!nzchar(along))
                     return(gettextf("'%s' is blank",
                                     "along"))
             }
             TRUE
         })

setClass("AlphaMoveMixin",
         slots = c(alphaMove = "ParameterVector"),
         contains = "VIRTUAL")

setClass("AlphaDLMMixin",
         slots = c(alphaDLM = "ParameterVector"),
         contains = "VIRTUAL",
         validity = function(object) {
             K <- object@K@.Data
             L <- object@L@.Data
             alphaDLM <- object@alphaDLM
             ## 'alphaDLM' has length '(K+1)L'
             if (!identical(length(alphaDLM), (K + 1L) * L))
                 return(gettextf("'%s' does not have length '%s'",
                                 "alphaDLM", "(K+1)L"))
             TRUE
         })

setClass("AlphaICARMixin",
         slots = c(alphaICAR = "ParameterVector"),
         contains = "VIRTUAL",
         validity = function(object) {
             J <- object@J@.Data
             alphaICAR <- object@alphaICAR
             ## 'alphaICAR' has length 'J'
             if (!identical(length(alphaICAR), J))
                 return(gettextf("'%s' does not have length '%s'",
                                 "alphaICAR", "J"))
             TRUE
         })

setClass("AlphaKnownMixin",
         slots = c(alphaKnown = "ParameterVector"),
         contains = "VIRTUAL",
         validity = function(object) {
             alphaKnown <- object@alphaKnown
             J <- object@J
             ## 'alphaKnown' has length 'J'
             if (!identical(length(alphaKnown), as.integer(J)))
                 return(gettextf("'%s' does not have length '%s'",
                                 "alphaKnown", "J"))
             TRUE
         })

setClass("CNoTrendMixin",
         slots = c(CNoTrend = "FFBSList"),
         contains = "VIRTUAL",
         validity = function(object) {
             CNoTrend <- object@CNoTrend
             K <- object@K
             ## 'CNoTrend' has length K+1
             if (!identical(length(CNoTrend), as.integer(K) + 1L))
                 return(gettextf("'%s' does not have length %s+1",
                                 "CNoTrend", "K"))
             ## elements of 'CNoTrend' have length 1L
             if (!identical(length(CNoTrend[[1L]]), 1L))
                 return(gettextf("elements of '%s' do not have length '%d'",
                                 "CNoTrend", 1L))
             TRUE
         })

setClass("CWithTrendMixin",
         slots = c(CWithTrend = "FFBSList"),
         contains = "VIRTUAL",
         validity = function(object) {
             CWithTrend <- object@CWithTrend
             K <- object@K
             ## 'CWithTrend' has length K+1
             if (!identical(length(CWithTrend), as.integer(K) + 1L))
                 return(gettextf("'%s' does not have length %s+1",
                                 "CWithTrend", "K"))
             ## elements of 'CWithTrend' are 2x2 matrices
             if (!all(sapply(CWithTrend, function(x) identical(dim(x), c(2L, 2L)))))
                 return(gettextf("elements of '%s' are not 2x2 matrices",
                                 "CWithTrend"))
             TRUE
         })

setClass("CSeasonMixin",
         slots = c(CSeason = "FFBSList"),
         contains = "VIRTUAL",
         validity = function(object) {
             CSeason <- object@CSeason
             K <- object@K
             nSeason <- object@nSeason
             ## 'CSeason' has length K+1
             if (!identical(length(CSeason), as.integer(K) + 1L))
                 return(gettextf("'%s' does not have length %s+1",
                                 "CSeason", "K"))
             ## elements of 'CSeason' are length-nSeason vectors (not matrices)
             if (!all(sapply(CSeason, length) == nSeason))
                 return(gettextf("elements of '%s' do not have length '%s'",
                                 "CSeason", "nSeason"))
             TRUE
         })

setClass("ClassesMixin",
         slots = c(classes = "Values"),
         contains = "VIRTUAL",
         validity = function(object) {
             classes <- object@classes
             ## 'classes' does not have length 0
             if (identical(length(classes), 0L))
                 return(gettextf("'%s' has length %d",
                                 "classes", 0L))
             ## 'classes' has at least one dimension with
             ## dimtype "origin"
             dimtypes <- dimtypes(classes, use.names = FALSE)
             if (!("origin" %in% dimtypes))
                 return(gettextf("'%s' does not have dimension with dimtype \"%s\"",
                                 "classes", "origin"))
             ## 'classes' has no missing values
             if (any(is.na(classes)))
                 return(gettextf("'%s' has missing values",
                                 "classes"))
             ## 'classes' is integer
             if (!is.integer(classes))
                 return(gettextf("'%s' does not have type \"%s\"",
                                 "classes", "integer"))
             ## minimum value for 'classes' is 0 or 1
             if (!(min(classes) %in% 0:1))
                 return(gettextf("minimum value for '%s' is not %d or %d",
                                 "classes", 0L, 1L))
             ## 'classes' must have at least 2 distinct values
             elements.classes <- unique(as.integer(classes@.Data))
             if (length(elements.classes) < 2L)
                 return(gettextf("'%s' must have at least %d distinct values",
                                 "classes", 2L))
             ## unique values for 'classes' form unbroken series
             if (any(diff(sort(elements.classes)) != 1L))
                 return(gettextf("unique values of '%s' must be consecutive numbers",
                                 "classes"))
             TRUE
         })

setClass("ContrastsArgMixin",
         slots = c(contrastsArg = "list"),
         contains = "VIRTUAL")

setClass("DataMixin",
         slots = c(data = "data.frame"),
         contains = "VIRTUAL",
         validity = function(object) {
             data <- object@data
             ## 'data' has no missing values
             if (any(is.na(data)))
                 return(gettextf("'%s' has missing values",
                                 "data"))
             ## 'data' has at least 2 rows
             if (nrow(data) < 2L)
                 return(gettextf("'%s' has fewer than 2 rows",
                                 "data"))
             ## 'data' has at least 1 column
             if (ncol(data) < 1L)
                 return(gettextf("'%s' has 0 columns",
                                 "data"))
             TRUE
         })

setClass("DeltaDLMMixin",
         slots = c(deltaDLM = "ParameterVector"),
         contains = "VIRTUAL",
         validity = function(object) {
             K <- object@K@.Data
             L <- object@L@.Data
             deltaDLM <- object@deltaDLM
             ## 'deltaDLM' has length '(K+1)L'
             if (!identical(length(deltaDLM), (K + 1L) * L))
                 return(gettextf("'%s' does not have length '%s'",
                                 "deltaDLM", "(K+1)L"))
             TRUE
         })

setClass("EtaMixin",
         slots = c(eta = "ParameterVector"),
         contains = "VIRTUAL",
         validity = function(object) {
             P <- object@P
             eta <- object@eta
             ## 'eta' has length 'P'
             if (!identical(length(eta), as.integer(P)))
                 return(gettextf("'%s' does not have length '%s'",
                                 "eta", "P"))
             TRUE
         })

setClass("FormulaMixin",
         slots = c(formula = "formula"),
         contains = "VIRTUAL",
         validity = function(object) {
             formula <- object@formula
             ## 'formula' includes a response
             if (!hasResponse(formula))
                 return(gettextf("formula '%s' does not include a response",
                                 deparse(formula)))
             ## response in 'formula' is "mean"
             if (!identical(deparse(formula[[2L]]), "mean"))
                 return(gettextf("response in formula '%s' is not '%s'",
                                 deparse(formula), "mean"))
             ## 'formula' has intercept
             has.intercept <- identical(attr(stats::terms(formula), "intercept"), 1L)
             if (!has.intercept)
                 return(gettextf("formula '%s' does not include an intercept",
                                 deparse(formula)))
             ## 'formula' has at least one predictor, other than intercept
             if (identical(length(attr(stats::terms(formula), "term.labels")), 0L))
                 return(gettextf("formula '%s' does not include any predictors (other than the intercept)",
                                 deparse(formula)))
             TRUE
         })

setClass("GWithTrendMixin",
         slots = c(GWithTrend = "DLMMatrix"),
         contains = "VIRTUAL",
         validity = function(object) {
             GWithTrend <- object@GWithTrend
             ## 'GWithTrend' has 2 rows
             if (!identical(nrow(GWithTrend), 2L))
                 return(gettextf("'%s' does not have %d rows",
                                 "GWithTrend", 2L))
             TRUE
         })

setClass("HasAlphaMoveMixin",
         slots = c(hasAlphaMove = "LogicalFlag"),
         contains = "VIRTUAL")

setClass("HasAlphaDLMMixin",
         slots = c(hasAlphaDLM = "LogicalFlag"),
         contains = "VIRTUAL")

setClass("HasAlphaICARMixin",
         slots = c(hasAlphaICAR = "LogicalFlag"),
         contains = "VIRTUAL")

setClass("HasCovariatesMixin",
         slots = c(hasCovariates = "LogicalFlag"),
         contains = "VIRTUAL")

setClass("HasSeasonMixin",
         slots = c(hasSeason = "LogicalFlag"),
         contains = "VIRTUAL")

setClass("IAlongMixin",
         slots = c(iAlong = "integer"),
         contains = "VIRTUAL",
         validity = function(object) {
             iAlong <- object@iAlong
             ## 'iAlong' has length 1
             if (!identical(length(iAlong), 1L))
                 return(gettextf("'%s' does not have length %d",
                                 "iAlong", 1L))
             ## 'iAlong' is not missing
             if (is.na(iAlong))
                 return(gettextf("'%s' is missing",
                                 "iAlong"))
             ## 'iAlong' is positive
             if (iAlong <= 0)
                 return(gettextf("'%s' is non-positive",
                                 "iAlong"))
             TRUE
         })

setClass("IMethodPrior",
         slots = c(iMethodPrior = "integer"),
         contains = "VIRTUAL")

setClass("IndexClassAlphaMixin",
         slots = c(indexClassAlpha = "integer"),
         contains = "VIRTUAL",
         validity = function(object) {
             indexClassAlpha <- object@indexClassAlpha
             alphaMove <- object@alphaMove
             J <- object@J
             ## 'indexClassAlpha' has no missing values
             if (any(is.na(indexClassAlpha)))
                 return(gettextf("'%s' has missing values",
                                 "indexClassAlpha"))
             ## 'indexClassAlpha' has length 'J'
             if (!identical(length(indexClassAlpha), as.integer(J)))
                 return(gettextf("'%s' does not have length '%s'",
                                 "indexClassAlpha", "J"))
             ## non-zero indices form index for elements of 'alphaMove'
             indices.nonzero <- indexClassAlpha[indexClassAlpha != 0L]
             indices.obtained <- sort(unique(indices.nonzero))
             indices.expected <- seq_along(alphaMove)
             if (!identical(indices.obtained, indices.expected))
                 return(gettextf("'%s' has missing or redundant indices",
                                 "indexClassAlpha"))
             TRUE
         })

setClass("IsRobustMixin",
         slots = c(isRobust = "LogicalFlag"),
         contains = "VIRTUAL")

setClass("IteratorStateMixin",
         slots = c(iteratorState = "AlongIterator"),
         contains = "VIRTUAL",
         validity = function(object) {
             iterator <- object@iteratorState
             K <- object@K@.Data
             L <- object@L@.Data
             ## 'indices' have length K+1
             if (!identical(length(iterator@indices), K + 1L))
                 return(gettextf("'%s' does not have length %s",
                                 "indices", "K+1"))
             ## 'nWithin' times 'nBetween' equals 'L'
             L.expected <- iterator@nBetween * iterator@nWithin
             if (!identical(L.expected, L))
                 return(gettextf("'%s' times '%s' for '%s' does not equal %s",
                                 "nWithin", "nBetween", "iterator", "L"))
             TRUE
         })

setClass("IteratorStateOldMixin",
         slots = c(iteratorStateOld = "AlongIterator"),
         contains = "VIRTUAL",
         validity = function(object) {
             iterator <- object@iteratorStateOld
             J.old <- object@JOld@.Data
             L <- object@L@.Data
             K.old <- J.old %/% L
             ## 'indices' have length K.old + 1
             if (!identical(length(iterator@indices), K.old + 1L))
                 return(gettextf("'%s' does not have length %s",
                                 "indices", "K.old + 1"))
             ## 'nWithin' times 'nBetween' equals 'L'
             L.expected <- iterator@nBetween * iterator@nWithin
             if (!identical(L.expected, L))
                 return(gettextf("'%s' times '%s' for '%s' does not equal %s",
                                 "nWithin", "nBetween", "iterator", "L"))
             TRUE
         })

setClass("IteratorVMixin",
         slots = c(iteratorV = "AlongIterator"),
         contains = "VIRTUAL",
         validity = function(object) {
             iterator <- object@iteratorV
             K <- object@K@.Data
             L <- object@L@.Data
             ## 'indices' have length K
             if (!identical(length(iterator@indices), K))
                 return(gettextf("'%s' does not have length %s",
                                 "indices", "K"))
             ## 'nWithin' times 'nBetween' equals 'L'
             L.expected <- iterator@nBetween * iterator@nWithin
             if (!identical(L.expected, L))
                 return(gettextf("'%s' times '%s' for '%s' does not equal %s",
                                 "nWithin", "nBetween", "iterator", "L"))
             TRUE
         })

setClass("JMixin",
         slots = c(J = "Length"),
         contains = "VIRTUAL")

setClass("JOldMixin",
         slots = c(JOld = "Length"),
         contains = "VIRTUAL")

setClass("KLMixin",
         slots = c(K = "Length",
                        L = "Length"),
         contains = "VIRTUAL",
         validity = function(object) {
             J <- object@J@.Data
             K <- object@K@.Data
             L <- object@L@.Data
             ## J = KL
             if (!identical(K * L, J))
                 return(gettextf("'%s' not equal to '%s' times '%s'",
                                 "J", "K", "L"))
             TRUE
         })

setClass("MNoTrendMixin",
         slots = c(mNoTrend = "FFBSList"),
         contains = "VIRTUAL",
         validity = function(object) {
             mNoTrend <- object@mNoTrend
             K <- object@K@.Data
             ## 'mNoTrend' has length K+1
             if (!identical(length(mNoTrend), K + 1L))
                 return(gettextf("'%s' does not have length %s+1",
                                 "mNoTrend", "K"))
             ## elements of 'mNoTrend' have length 1L
             if (!identical(length(mNoTrend[[1L]]), 1L))
                 return(gettextf("elements of '%s' do not have length '%d'",
                                 "mNoTrend", 1L))
             TRUE
         })

setClass("MWithTrendMixin",
         slots = c(mWithTrend = "FFBSList"),
         contains = "VIRTUAL",
         validity = function(object) {
             mWithTrend <- object@mWithTrend
             K <- object@K@.Data
             ## 'mWithTrend' has length K+1
             if (!identical(length(mWithTrend), K + 1L))
                 return(gettextf("'%s' does not have length %s+1",
                                 "mWithTrend", "K"))
             ## elements of 'mWithTrend' have length 2
             if (!identical(length(mWithTrend[[1L]]), 2L))
                 return(gettextf("elements of '%s' do not have length '%d'",
                                 "mWithTrend", 2L))
             TRUE
         })

setClass("MSeasonMixin",
         slots = c(mSeason = "FFBSList"),
         contains = "VIRTUAL",
         validity = function(object) {
             mSeason <- object@mSeason
             K <- object@K@.Data
             nSeason <- object@nSeason
             ## 'mSeason' has length K+1
             if (!identical(length(mSeason), K + 1L))
                 return(gettextf("'%s' does not have length %s+1",
                                 "mSeason", "K"))
             ## elements of 'mSeason' have length 'nSeason'
             if (!identical(length(mSeason[[1L]]), as.integer(nSeason)))
                 return(gettextf("elements of '%s' do not have length '%s'",
                                 "mSeason", "nSeason"))
             TRUE
         })

setClass("M0NoTrendMixin",
         slots = c(m0NoTrend = "FFBSList"),
         contains = "VIRTUAL",
         validity = function(object) {
             m0NoTrend <- object@m0NoTrend
             L <- object@L@.Data@.Data
             ## 'm0NoTrend' has length L
             if (!identical(length(m0NoTrend), L))
                 return(gettextf("'%s' does not have length %s",
                                 "m0NoTrend", "L"))
             ## elements of 'm0NoTrend' have length 1L
             if (!identical(length(m0NoTrend[[1L]]), 1L))
                 return(gettextf("elements of '%s' do not have length '%d'",
                                 "m0NoTrend", 1L))
             TRUE
         })

setClass("M0WithTrendMixin",
         slots = c(m0WithTrend = "FFBSList"),
         contains = "VIRTUAL",
         validity = function(object) {
             m0WithTrend <- object@m0WithTrend
             L <- object@L@.Data
             ## 'm0WithTrend' has length L
             if (!identical(length(m0WithTrend), L))
                 return(gettextf("'%s' does not have length %s",
                                 "m0WithTrend", "L"))
             ## elements of 'm0WithTrend' have length 2
             if (!identical(length(m0WithTrend[[1L]]), 2L))
                 return(gettextf("elements of '%s' do not have length '%d'",
                                 "m0WithTrend", 2L))
             TRUE
         })

setClass("M0SeasonMixin",
         slots = c(m0Season = "FFBSList"),
         contains = "VIRTUAL",
         validity = function(object) {
             m0Season <- object@m0Season
             L <- object@L@.Data
             nSeason <- object@nSeason@.Data
             ## 'm0Season' has length L
             if (!identical(length(m0Season), L))
                 return(gettextf("'%s' does not have length %s",
                                 "m0Season", "L"))
             ## elements of 'm0Season' have length 'nSeason'
             if (!identical(length(m0Season[[1L]]), nSeason))
                 return(gettextf("elements of '%s' do not have length '%s'",
                                 "m0Season", "nSeason"))
             TRUE
         })

setClass("MeanMixin",
         slots = c(mean = "Parameter"),
         contains = "VIRTUAL")

setClass("MeanDelta0Mixin",
         slots = c(meanDelta0 = "Parameter"),
         prototype = prototype(meanDelta0 = new("Parameter", 0)),
         contains = "VIRTUAL")

setClass("MultMixin",
         slots = c(mult = "Scale"),
         contains = "VIRTUAL")

setClass("MultAlphaMixin",
         slots = c(multAlpha = "Scale"),
         contains = "VIRTUAL")

setClass("MultDeltaMixin",
         slots = c(multDelta = "Scale"),
         contains = "VIRTUAL")

setClass("MultDelta0Mixin",
         slots = c(multDelta0 = "Scale"),
         contains = "VIRTUAL")

setClass("MultEtaCoefMixin",
         slots = c(multEtaCoef = "Scale"),
         contains = "VIRTUAL")

setClass("MultMoveMixin",
         slots = c(multMove = "Scale"),
         contains = "VIRTUAL")

setClass("MultSeasonMixin",
         slots = c(multSeason = "Scale"),
         contains = "VIRTUAL")

setClass("MultTauMixin",
         slots = c(multTau = "Scale"),
         contains = "VIRTUAL")

setClass("NElementClassAlphaMixin",
         slots = c(nElementClassAlpha = "integer"),
         contains = "VIRTUAL",
         validity = function(object) {
             nElementClassAlpha <- object@nElementClassAlpha
             alphaMove <- object@alphaMove
             indexClassAlpha <- object@indexClassAlpha
             ## no missing values
             if (any(is.na(nElementClassAlpha)))
                 return(gettextf("'%s' has missing values",
                                 "nElementClassAlpha"))
             ## same number of elements as 'alphaMove'
             if (!identical(length(nElementClassAlpha), length(alphaMove)))
                 return(gettextf("'%s' and '%s' have different lengths",
                                 "nElementClassAlpha", "alphaMove"))
             ## consistent with 'indexClassAlpha'
             index.expected <- sort(unique(indexClassAlpha[indexClassAlpha != 0L]))
             if (!identical(indexClassAlpha, index.expected))
                 return(gettextf("'%s' and '%s' inconsistent",
                                 "nElementClassAlpha", "indexClassAlpha"))
             TRUE
         })

setClass("NSeasonMixin",
         slots = c(nSeason = "Length"),
         contains = "VIRTUAL",
         validity = function(object) {
             nSeason <- object@nSeason
             ## at least 2 seasons
             if (nSeason < 2L)
                 return(gettextf("'%s' is less than %d",
                                 "nSeason", 2L))
             TRUE
         })

setClass("NuMixin",
         slots = c(nu = "DegreesFreedom"),
         contains = "VIRTUAL")

setClass("NuAlphaMixin",
         slots = c(nuAlpha = "DegreesFreedom"),
         contains = "VIRTUAL")

setClass("NuBetaMixin",
         slots = c(nuBeta = "DegreesFreedom"),
         contains = "VIRTUAL")

setClass("NuDeltaMixin",
         slots = c(nuDelta = "DegreesFreedom"),
         contains = "VIRTUAL")

setClass("NuEtaCoefMixin",
         slots = c(nuEtaCoef = "DegreesFreedom"),
         contains = "VIRTUAL")

setClass("NuSeasonMixin",
         slots = c(nuSeason = "DegreesFreedom"),
         contains = "VIRTUAL")

setClass("NuTauMixin",
         slots = c(nuTau = "DegreesFreedom"),
         contains = "VIRTUAL")

setClass("OmegaAlphaMaxMixin",
         slots = c(omegaAlphaMax = "Scale"),
         contains = "VIRTUAL",
         validity = function(object) {
             omegaAlpha <- object@omegaAlpha@.Data
             omegaAlphaMax <- object@omegaAlphaMax@.Data
             if (omegaAlpha > omegaAlphaMax)
                 return(gettextf("'%s' is greater than '%s'",
                                 "omegaAlpha", "omegaAlphaMax"))
             TRUE
         })

setClass("OmegaAlphaMixin",
         slots = c(omegaAlpha = "Scale"),
         contains = "VIRTUAL")

setClass("OmegaDeltaMaxMixin",
         slots = c(omegaDeltaMax = "Scale"),
         contains = "VIRTUAL",
         validity = function(object) {
             omegaDelta <- object@omegaDelta@.Data
             omegaDeltaMax <- object@omegaDeltaMax@.Data
             if (omegaDelta > omegaDeltaMax)
                 return(gettextf("'%s' is greater than '%s'",
                                 "omegaDelta", "omegaDeltaMax"))
             TRUE
         })

setClass("OmegaDeltaMixin",
         slots = c(omegaDelta = "Scale"),
         contains = "VIRTUAL")

setClass("OmegaSeasonMaxMixin",
         slots = c(omegaSeasonMax = "Scale"),
         contains = "VIRTUAL",
         validity = function(object) {
             omegaSeason <- object@omegaSeason@.Data
             omegaSeasonMax <- object@omegaSeasonMax@.Data
             if (omegaSeason > omegaSeasonMax)
                 return(gettextf("'%s' is greater than '%s'",
                                 "omegaSeason", "omegaSeasonMax"))
             TRUE
         })

setClass("OmegaSeasonMixin",
         slots = c(omegaSeason = "Scale"),
         contains = "VIRTUAL")

setClass("PMixin",
         slots = c(P = "Length"),
         contains = "VIRTUAL",
         validity = function(object) {
             P <- object@P@.Data
             ## 'P' is 2 or more
             if (P < 2L)
                 return(gettextf("'%s' is less than %d",
                                 "P", 2L))
         })

setClass("PhiMixin",
         slots = c(phi = "numeric"),
         contains = "VIRTUAL",
         validity = function(object) {
             phi <- object@phi
             ## 'phi' has length 1
             if (!identical(length(phi), 1L))
                 return(gettextf("'%s' does not have length %d",
                                 "phi", 1L))
             ## 'phi' has type "double"
             if (!is.double(phi))
                 return(gettextf("'%s' does not have type \"%s\"",
                                 "phi", "double"))
             ## 'phi' is not missing
             if (is.na(phi))
                 return(gettext("'%s' is missing",
                                "phi"))
             ## 'phi' is non-negative
             if (phi < 0)
                 return(gettext("'%s' is negative",
                                "phi"))
             ## 'phi' is less than or equal to 1
             if (phi > 1)
                 return(gettext("'%s' is greater than %d",
                                "phi", 1L))
             TRUE
         })


setClass("PhiKnownMixin",
         slots = c(phiKnown = "LogicalFlag"),
         contains = "VIRTUAL")

setClass("PhiMinMaxMixin",
         slots = c(minPhi = "numeric",
                        maxPhi = "numeric"),
         contains = "VIRTUAL",
         validity = function(object) {
             minPhi <- object@minPhi
             maxPhi <- object@maxPhi
             for (name in c("minPhi", "maxPhi")) {
                 value <- methods::slot(object, name) 
                 ## 'minPhi', 'maxPhi' have length 1
                 if (!identical(length(value), 1L))
                     return(gettextf("'%s' does not have length %d",
                                     name, 1L))
                 ## 'minPhi', 'maxPhi' are double
                 if (!is.double(value))
                     return(gettextf("'%s' does not have type \"%s\"",
                                     name, "double"))
                 ## 'minPhi', 'maxPhi' are not missing
                 if (is.na(value))
                     return(gettextf("'%s' is missing",
                                     name))
             }
             ## minPhi >= 0
             if (minPhi < 0)
                 return(gettextf("'%s' is less than %d",
                                 "minPhi", 0L))
             ## maxPhi <= 1
             if (maxPhi > 1)
                 return(gettextf("'%s' is greater than %d",
                                 "maxPhi", 1L))
             ## minPhi < maxPhi
             if (minPhi >= maxPhi)
                 return(gettextf("'%s' greater than or equal to '%s'",
                                 "minPhi", "maxPhi"))
             TRUE
         })

setClass("RNoTrendMixin",
         slots = c(RNoTrend = "FFBSList"),
         contains = "VIRTUAL",
         validity = function(object) {
             RNoTrend <- object@RNoTrend
             K <- object@K
             ## 'RNoTrend' has length K
             if (!identical(length(RNoTrend), as.integer(K)))
                 return(gettextf("'%s' does not have length '%s'",
                                 "RNoTrend", "K"))
             ## elements of 'RNoTrend' have length 1L
             if (!identical(length(RNoTrend[[1L]]), 1L))
                 return(gettextf("elements of '%s' do not have length '%d'",
                                 "RNoTrend", 1L))
             TRUE
         })

setClass("RWithTrendMixin",
         slots = c(RWithTrend = "FFBSList"),
         contains = "VIRTUAL",
         validity = function(object) {
             RWithTrend <- object@RWithTrend
             K <- object@K
             ## 'RWithTrend' has length K
             if (!identical(length(RWithTrend), as.integer(K)))
                 return(gettextf("'%s' does not have length '%s'",
                                 "RWithTrend", "K"))
             ## elements of 'RWithTrend' are 2x2 matrices
             if (!all(sapply(RWithTrend, function(x) identical(dim(x), c(2L, 2L)))))
                 return(gettextf("elements of '%s' are not 2x2 matrices",
                                 "RWithTrend"))
             TRUE
         })

setClass("RSeasonMixin",
         slots = c(RSeason = "FFBSList"),
         contains = "VIRTUAL",
         validity = function(object) {
             RSeason <- object@RSeason
             K <- object@K
             nSeason <- object@nSeason
             ## 'RSeason' has length K
             if (!identical(length(RSeason), as.integer(K)))
                 return(gettextf("'%s' does not have length '%s'",
                                 "RSeason", "K"))
             ## elements of 'RSeason' are length-nSeason vectors (not matrices)
             if (!all(sapply(RSeason, length) == nSeason))
                 return(gettextf("elements of '%s' do not have length '%s'",
                                 "RSeason", "nSeason"))
             TRUE
         })

setClass("SlotsToExtract",
         slots = c(slotsToExtract = "character"),
         contains = "VIRTUAL")

setClass("SMixin",
         slots = c(s = "FFBSList"),
         contains = "VIRTUAL",
         validity = function(object) {
             s <- object@s@.Data
             K <- object@K@.Data
             L <- object@L@.Data
             nSeason <- object@nSeason@.Data
             ## 's' has length '(K+1)L'
             if (!identical(length(s), (K + 1L) * L))
                 return(gettextf("'%s' does not have length '%s'",
                                 "s", "(K+1)L"))
             ## elements of 's' have length 'nSeason'
             if (!identical(length(s[[1L]]), nSeason))
                 return(gettextf("elements of '%s' do not have length '%s'",
                                 "s", "nSeason"))
             TRUE
         })

setClass("SpecAMixin",
         slots = c(A = "SpecScale"),
         contains = "VIRTUAL")

setClass("SpecAAlphaMixin",
         slots = c(AAlpha = "SpecScale"),
         contains = "VIRTUAL")

setClass("SpecADeltaMixin",
         slots = c(ADelta = "SpecScale"),
         contains = "VIRTUAL")

setClass("SpecADelta0Mixin",
         slots = c(ADelta0 = "SpecScale"),
         contains = "VIRTUAL")

setClass("SpecAEtaCoefMixin",
         slots = c(AEtaCoef = "SpecScale"),
         contains = "VIRTUAL")

setClass("SpecAEtaInterceptMixin",
         slots = c(AEtaIntercept = "SpecScale"),
         contains = "VIRTUAL")

setClass("SpecAMoveMixin",
         slots = c(AMove = "SpecScale"),
         contains = "VIRTUAL")

setClass("SpecASeasonMixin",
         slots = c(ASeason = "SpecScale"),
         contains = "VIRTUAL")

setClass("SpecATauMixin",
         slots = c(ATau = "SpecScale"),
         contains = "VIRTUAL")

setClass("SpecOmegaAlphaMaxMixin",
         slots = c(omegaAlphaMax = "SpecScale"),
         contains = "VIRTUAL")

setClass("SpecOmegaDeltaMaxMixin",
         slots = c(omegaDeltaMax = "SpecScale"),
         contains = "VIRTUAL")

setClass("SpecOmegaSeasonMaxMixin",
         slots = c(omegaSeasonMax = "SpecScale"),
         contains = "VIRTUAL")

setClass("SpecPhiMixin",
         slots = c(phi = "numeric"),
         validity = function(object) {
             phi <- object@phi
             ## 'phi' has length 1
             if (!identical(length(phi), 1L))
                 return(gettextf("'%s' does not have length %d",
                                 "phi", 1L))
             ## 'phi' has type "double"
             if (!is.double(phi))
                 return(gettextf("'%s' does not have type \"%s\"",
                                 "phi", "double"))
             if (!is.na(phi)) {
                 ## if 'phi' is not missing: 'phi' is non-negative
                 if (phi < 0)
                     return(gettext("'%s' is negative",
                                    "phi"))
                 ## if 'phi' is not missing: 'phi' is less than or equal to 1
                 if (phi > 1)
                     return(gettext("'%s' is greater than %d",
                                    "phi", 1L))
             }
             TRUE
         })

setClass("SpecScaleMaxMixin",
         slots = c(scaleMax = "SpecScale"),
         contains = "VIRTUAL")

setClass("SpecTauMixin",
         slots = c(tau = "SpecScale"),
         contains = "VIRTUAL")

setClass("SpecTauMaxMixin",
         slots = c(tauMax = "SpecScale"),
         contains = "VIRTUAL")

setClass("TauMaxMixin",
         slots = c(tauMax = "Scale"),
         contains = "VIRTUAL",
         validity = function(object) {
             tau <- object@tau
             tauMax <- object@tauMax
             if (tau > tauMax)
                 return(gettextf("'%s' is greater than '%s'",
                                 "tau", "tauMax"))
             TRUE
         })

setClass("TauMixin",
         slots = c(tau = "Scale"),
         contains = "VIRTUAL")

setClass("TauScaledMixin",
         slots = c(tauScaled = "Scale"),
         contains = "VIRTUAL")

setClass("UBetaMixin",
         slots = c(UBeta = "VarTDist"),
         contains = "VIRTUAL",
         validity = function(object) {
             J <- object@J
             UBeta <- object@UBeta
             ## 'UBeta' has length 'J'
             if (!identical(length(UBeta), as.integer(J)))
                 return(gettextf("'%s' does not have length '%s'",
                                 "UBeta", "J"))
             TRUE
         })

setClass("UEtaCoefMixin",
         slots = c(UEtaCoef = "VarTDist"),
         contains = "VIRTUAL",
         validity = function(object) {
             P <- object@P@.Data
             UEtaCoef <- object@UEtaCoef
             ## 'UEtaCoef' has length P-1
             if (!identical(length(UEtaCoef), P - 1L))
                 return(gettextf("'%s' does not have length %s-1",
                                 "UEtaCoef", "P"))
             TRUE
         })

setClass("UCDCMixin",
         slots = c(UC = "FFBSList",
                        DC = "FFBSList",
                        DCInv = "FFBSList"),
         contains = "VIRTUAL",
         validity = function(object) {
             K <- object@K@.Data
             for (name in c("UC", "DC", "DCInv")) {
                 value <- methods::slot(object, name)
                 ## 'UC', 'DC', 'DCInv' have length K+1
                 if (!identical(length(value), K + 1L))
                     return(gettextf("'%s' does not have length %s+1",
                                     name, "K"))
                 ## elements of 'UC', 'DC', 'DCInv' are 2x2 matrices
                 if (!all(sapply(value, function(x) identical(dim(x), c(2L, 2L)))))
                     return(gettextf("elements of '%s' are not 2x2 matrices",
                                     name))
             }
             TRUE
         })

setClass("URDRMixin",
         slots = c(UR = "FFBSList",
                        DRInv = "FFBSList"),
         contains = "VIRTUAL",
         validity = function(object) {
             K <- object@K@.Data
             for (name in c("UR", "DRInv")) {
                 value <- methods::slot(object, name)
                 ## 'UR', 'DRInv' have length K
                 if (!identical(length(value), K))
                     return(gettextf("'%s' does not have length %s",
                                     name, "K"))
                 ## elements of 'UR', 'DRInv' are 2x2 matrices
                 if (!all(sapply(value, function(x) identical(dim(x), c(2L, 2L)))))
                     return(gettextf("elements of '%s' are not 2x2 matrices",
                                     name))
             }
             TRUE
         })


setClass("WSqrtMixin",
         slots = c(WSqrt = "DLMMatrix",
                        WSqrtInvG = "DLMMatrix"),
         contains = "VIRTUAL",
         validity = function(object) {
             WSqrt <- object@WSqrt
             WSqrtInvG <- object@WSqrtInvG
             for (name in c("WSqrt", "WSqrtInvG")) {
                 value <- methods::slot(object, name)
                 ## 'WSqrt', 'WSqrtInvG' have 2 rows
                 if (!identical(nrow(value), 2L))
                     return(gettextf("'%s' does not have %d rows",
                                     name, 2L))
             }
             ## 'WSqrt' is a  diagonal matrix
             if (!isTRUE(all.equal(WSqrt[c(2L, 3L)], c(0, 0))))
                 return(gettextf("'%s' is not diagonal",
                                 "WSqrt"))
             ## second element of 'WSqrtInvG' is 0
             if (!isTRUE(all.equal(WSqrtInvG[2L], 0)))
                 return(gettextf("second element of '%s' is not %d",
                                 "WSqrtInvG", 0L))
             TRUE
         })

## Do not require that nrows >= ncols, since we have
## an informative prior on the coefficients.
setClass("ZMixin",
         slots = c(Z = "matrix"),
         contains = "VIRTUAL",
         validity = function(object) {
             J <- object@J
             P <- object@P
             Z <- object@Z
             ## 'Z' is double
             if (!is.double(Z))
                 return(gettextf("model matrix '%s' does not have type \"%s\"",
                                 "Z", "double"))
             ## 'Z' has no missing values
             if (any(is.na(Z)))
                 return(gettextf("model matrix '%s' has missing values",
                                 "Z"))
             ## 'Z' has 'J' rows
             if (!identical(nrow(Z), as.integer(J)))
                 return(gettextf("'%s' does not have '%s' rows",
                                 "Z", "J"))
             ## 'Z' has 'P' columns
             if (!identical(ncol(Z), as.integer(P)))
                 return(gettextf("'%s' does not have '%s' columns",
                                 "Z", "P"))
             ## first column all 1s
             first <- as.numeric(Z[, 1L])
             if (!isTRUE(all.equal(first, rep(1, times = J))))
                 return(gettextf("first column of '%s' is not a vector of 1s",
                                 "Z"))
             TRUE
         })

setClass("ZetaMixin",
         slots = c(zeta = "numeric"),
         contains = "VIRTUAL",
         validity = function(object) {
             zeta <- object@zeta
             ## 'zeta' has type "double"
             if (!is.double(zeta))
                 return(gettextf("'%s' does not have type \"%s\"",
                                 "zeta", "double"))
             ## 'zeta' is length 1
             if (!identical(length(zeta), 1L))
                 return(gettextf("'%s' does not have length %d",
                                 "zeta", 1L))
             ## 'zeta' is not missing
             if (is.na(zeta))
                 return(gettextf("'%s' is missing",
                                 "zeta"))
             TRUE
         })

