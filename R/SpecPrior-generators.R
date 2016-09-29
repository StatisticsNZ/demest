
## NO_TESTS
#' Specify covariates in a prior for a main effect or interaction.
#'
#' Most priors for main effects or interactions, such as age effects or
#' age-sex interactions, can include a covariate term.  The covariate
#' term is specified using function \code{Covariates}.
#'
#' The \code{formula} argument is a standard R \code{\link[stats]{formula}},
#' though with the restriction that the response must be \code{mean}.
#' Interactions are specified in the usual way, so that, for instance,
#'
#' \code{mean ~ age * sex + time}
#'
#' \code{mean ~ age + sex + age:sex + time}
#'
#' \code{mean ~ (age + sex)^2 + time}
#'
#' are all equivalent.
#'
#' The \code{data} argument must include labels for the main effect or
#' interaction being modelled, in addition to the covariate data itself.
#' For example, the \code{data} argument for region effect with a
#' \code{formula} argument of \code{mean ~ income} could be
#'
#' \tabular{lrr}{
#'  \tab region \tab income \cr
#' 1 \tab A \tab 15000 \cr
#' 2 \tab B \tab 22000 \cr
#' 3 \tab C \tab 18000
#' }
#'
#' The \code{data} argument for region:time interaction with a
#' \code{formula} argument of \code{mean ~ income} could be
#' 
#' \tabular{lrrr}{
#'  \tab region \tab time \tab income \cr
#' 1 \tab A \tab 2000 \tab 15000 \cr
#' 2 \tab B \tab 2000 \tab 22000 \cr
#' 3 \tab C \tab 2000 \tab 18000 \cr
#' 4 \tab A \tab 2010 \tab 17000 \cr
#' 5 \tab B \tab 2010 \tab 23000 \cr
#' 6 \tab C \tab 2010 \tab 17000
#' }
#'
#' Any columns in \code{data} that are not referred to by the \code{formula}
#' argument and that do do not serve as labels for the main effect or
#' interaction are ignored.  Similarly, redundant rows are ignored.
#'
#' Internally, covariates are standardized to have mean 0 and standard
#' deviation 0.5, as described in Gelman et al (2014, Section 16.3).
#' Coefficients for the standardized covariates are assumed to be drawn
#' from \emph{t} distributions centered on 0.  These distributions contain
#' scale parameters that determine the degree to which coefficient estimates
#' are shrunk towards 0.  The rules for choosing default values for the scale
#' parameters are described in the' documentation for \code{\link{HalfT}}.
#' Shrinking coefficient' estimates towards 0 protects against over-fitting.
#'
#' The intercept term is assumed to have a diffuse normal distribution centered
#' at 0.  The rules for choosing default values for the standard deviation are
#' described in the documentation for \code{\link{Norm}}.
#' 
#' The help for \code{\link[stats]{model.matrix}} contains a discussion of
#' contrasts.  With Bayesian models that have informative priors, such as the
#' t priors used by \code{Covariates}, all levels of a factor can be included
#' in the model. See below for an example. 
#' 
#' @param formula A \code{\link[stats]{formula}} with response \code{mean}.
#' @param data A data.frame containing covariate data.
#' @param contrastsArg A named list, the elements which are matrices
#'     or names of contrasts functions.
#' @param intercept An object of class \code{\linkS4class{Norm}}.
#' @param coef An object of class \code{\link{HalfT}}.
#'
#' @return An object of class \code{\linkS4class{Covariates}}
#'
#' @references
#' Gelman, A., Carlin, J.B., Stern, H.S. and Rubin, D.B., 2014.
#' \emph{Bayesian Ddata Analysis. Third Edition.} Boca Raton, FL, USA:
#' Chapman & Hall/CRC.
#'
#' @seealso Priors constructed with \code{\link{Exch}} and
#' \code{\link{DLM}} both allow for covariates.
#'
#' @examples
## covariates for a region main effect
#' reg.data <- data.frame(region = LETTERS[1:10],
#'                        income = c(20, 31, 15, 7, 24, 8, 22, 14, 21, 17),
#'                        area = rep(c("Urban", "Rural"), each = 5))
#' Covariates(mean ~ income + area,
#'            data = reg.data)
#'
#' ## override the default settings for
#' ## intercept and coefficients
#' Covariates(mean ~ income + area,
#'            data = reg.data,
#'            intercept = Norm(scale = 5),
#'            coef = HalfT(scale = 0.25))
#'
#' ## override the default 'treatment' contrast
#' contrasts.arg <- list(area = diag(2))
#' Covariates(mean ~ income + area,
#'            data = reg.data,
#'            contrastsArg = contrasts.arg)
#'
#' ## covariate data for an age:sex interaction
#' agesex.data <- data.frame(age = rep(c("0-14", "15-29", "30+"),
#'                                     times = 2),
#'                           sex = rep(c("Female", "Male"),
#'                                     each = 3),
#'                           weight = c(78, 94, 83, 84, 62, 75))
#' Covariates(mean ~ weight,
#'            data = agesex.data)
#' @export 
Covariates <- function(formula, data, contrastsArg = list(),
                       intercept = Norm(),
                       coef = HalfT()) {
    checkCovariateFormula(formula)
    checkCovariateData(x = data, name = "data")
    checkModelMatrix(formula = formula,
                     data = data,
                     contrastsArg = contrastsArg)
    if (!methods::is(intercept, "Norm"))
        stop(gettextf("'%s' has class \"%s\"",
                      "intercept", class(intercept)))
    if (!methods::is(coef, "HalfT"))
        stop(gettextf("'%s' has class \"%s\"",
                      "coef", class(coef)))
    AEtaIntercept <- intercept@A
    AEtaCoef <- coef@A
    multEtaCoef <- coef@mult
    nuEtaCoef <- coef@nu
    methods::new("Covariates",
                 AEtaCoef = AEtaCoef,
                 AEtaIntercept = AEtaIntercept,
                 contrastsArg = contrastsArg,
                 data = data,
                 formula = formula,
                 multEtaCoef = multEtaCoef,
                 nuEtaCoef = nuEtaCoef)
}


## NO_TESTS
#' Specify the amount of damping in a DLM prior.
#'
#' By default, the level term in a local level model and the trend term in a
#' linear trend model are 'damped'.  The level term or trend term are pulled
#' towards 0.  The amount of damping can be specified by the user, or can
#' be estimated from the data.
#'
#' With a prior for a main effect, in a local level model, the level term has
#' the form
#'
#' \code{level[j] ~ damp * level[j-1] + errorLevel[j]},
#'
#' and in a linear trend model the trend term has the form
#'
#' \code{level[j] ~ damp * level[j-1] + errorLevel[j]}.
#'
#' With a prior for an interaction, the level term has the form
#'
#' \code{level[k,l] ~ damp * level[k-1,l] + errorLevel[k,l]}
#'
#' and
#'
#' \code{level[k,l] ~ damp * level[k-1,l] + errorLevel[k,l]}.
#'
#' (See the documentation for function \code{\link{DLM}} for
#' an explanation of the \code{k,l} subscripts.)
#'
#' Values of for \code{damp} are restricted to the range \code{0 < damp <= 1}.
#' In linear trend models, including a damping term with a value near 1
#' typically results in more accurate forecasts (Hyndman et al 2008).  There
#' are exceptions, however: for instance, damping of the trend for the time
#' effect is probably not appropriate in mortality forecasts for developed
#' countries (Oeppen and Vaupel 2002).  Damping is also not necessary
#' appropriate in local level models.
#'
#' The user set the level of damping by providing a value for the
#' \code{coef} argument.  Alternatively, an appropriate value can be inferred
#' from the data, using a uniform prior, with limits set by arguments
#' \code{min} and \code{max}.  The default for \code{min} and \code{max}
#' are based on function \code{ets} in package \pkg{forecast}.
#'
#' Setting the \code{damp} argument to \code{NULL} in function
#' \code{\link{DLM}} turns off damping.
#'
#' @param coef A number between 0 and 1.
#' @param min A number between 0 and 1.
#' @param max A number between \code{min} and 1.
#'
#' @return An object of class \code{\linkS4class{Damp}}.
#'
#' @seealso \code{Damp} is used in calls to function \code{\link{DLM}}
#'
#' @references
#' Hyndman, R., Koehler, A. B., Ord, J. K., & Snyder, R. D. (2008).
#' \emph{Forecasting with' exponential smoothing: the state space approach}.
#' Springer.
#'
#' Oeppen, J., & Vaupel, J. W. (2002). Broken limits to life expectancy.
#' \emph{Science}, 296(5570), 1029-1031.
#' 
#' @examples
#' ## default
#' Damp()
#'
#' ## known value
#' Damp(coef = 0.95)
#'
#' ## estimate, but restrict to values between 0.85 and 0.95
#' Damp(min = 0.85, max = 0.95)
#' @export
Damp <- function(coef = NULL, min = 0.8, max = 0.98) {
    phi.known <- !is.null(coef)
    if (phi.known) {
        if (methods::hasArg(min))
            warning(gettextf("'%s' is ignored when '%s' is non-%s",
                             "min", "coef", "NULL"))
        if (methods::hasArg(max))
            warning(gettextf("'%s' is ignored when '%s' is non-%s",
                             "max", "coef", "NULL"))
        phi <- checkAndTidyPhi(coef)
        methods::new("DampKnown",
            phi = phi)
    }
    else {
        min.max <- checkAndTidyPhiMinMax(min = min, max = max)
        methods::new("DampUnknown",
            minPhi = min.max[1L],
            maxPhi = min.max[2L])
    }
}

#' Specify a Dynamic Linear Model (DLM) prior.
#'
#' Specify a prior for main effects or interactions in which units that are
#' neighbours are more strongly correlated than units that are distant
#' from each other. Dynamic linear models were developed for time
#' series, but are often appropriate for non-time dimensions, such as age.
#'
#' When applied to a main effect, the prior specified by \code{DLM} has the
#' form
#' 
#'\code{parameter[j] = level[j] + season[j] + covariates[j] + error[j].}
#'
#' When applied to an interaction, a DLM prior contains a separate series
#' along one dimension (typically a time dimension) for each combination
#' of values for the remaining series.  Using \code{k} to index location
#' on the "along" dimension, and \code{l} to index location on the remaining
#' dimensions, the prior has the form
#' 
#' \code{
#' parameter[k,l] = level[k,l] + season[k,l] + covariates[k,l] + error[k,l].
#' }
#'
#' For instance, if \code{parameter} is a region:time interaction involving 10
#' regions and 30 periods, and the "along" dimension is time, then
#' \code{k=1,...,30} and \code{l=1,...,10}.  If \code{parameters} is an
#' age:sex:region interaction involving 101 age groups, 2 sexes, and 10
#' regions, and the "along" dimension is age, then \code{k=1,...,101} and
#' \code{l = 1,...,20}.
#' 
#' Season and covariate terms are specified using functions \code{\link{Season}}
#' and \code{\link{Covariates}}.  Both are optional.  Error terms are 
#' specified using  function \code{\link{Error}}.
#'
#' @section The "along" dimension:
#' 
#' If the "along" dimension is not specified, then \code{DLM} looks
#' for a dimension with \code{\link[dembase]{dimtype}}
#' \code{"time"}.  If it does not find one, it looks for a dimension with
#' dimtype \code{"age"}.
#' 
#' A dimension with dimtype \code{"state"} can be used as an \code{"along"}
#' dimension.  Doing so probably make senses if the dimension measures
#' a qualitative variable such as income or years of schooling.  It probably
#' does not makes sense if the dimension measures a qualitative variable
#' such as region or occupation.
#'
#' @section The local level model:
#'
#' In a local level model, the level term follows a random walk.  With
#' a main effect,
#' 
#' \code{level[j] = damp * level[j-1] + errorLevel[j],}
#'
#' and with an interaction,
#'
#' \code{level[k,l] = damp * level[k-1,l] + errorLevel[k,l],}
#'
#' where \code{0 < damp <= 1}.  The documentaion for function
#' \code{\link{Damp}} describes the role played by the \code{damp} term.
#'
#' @section The linear trend model:
#'
#' In a linear level model, the level term follows a random walk but with
#' an underlying trend.  The trend term itself follows a random walk, implying
#' that it can change magnitude or direction.  With a main effect,
#'
#' \code{level[j] = level[j-1] + trend[j-1] + errorLevel[j]}
#' \code{trend[j] = trend[j-1] + errorTrend[j],}
#'
#' and with an interaction,
#'
#' \code{level[k,l] = level[k-1,l] + trend[k-1,l] + errorLevel[k,l]}
#' \code{trend[k,l] = damp * trend[k-1,l] + errorTrend[k,l].}
#' 
#' where \code{0 < damp <= 1}.  The documentaion for function
#' \code{\link{Damp}} describe the role played by the \code{damp} term.
#'
#' @section Error terms:
#' 
#' The error term for the level, \code{errorLevel}, has the form
#'
#' \code{errorLevel[j] ~ N(0, scaleLevel^2)}
#'
#' or
#'
#' \code{errorLevel[k,l] ~ N(0, scaleLevel^2)}.
#'
#' Similarly, the error term for the trend, \code{errorTrend}, has the form
#' 
#' \code{errorLevel[j] ~ N(0, scaleLevel^2)}
#'
#' or
#'
#' \code{errorLevel[k,l] ~ N(0, scaleLevel^2)}.
#'
#' \code{scaleLevel} and \code{scaleTrend} both have half-t priors.
#' Non-default versions of these can be specified using function 
#' \code{\link{HalfT}}.
#'
#' @param along Name of a dimension.  
#' @param level An object of class \code{\linkS4class{HalfT}}
#' specifying the prior for \code{errorLevel}.
#' @param trend An object of class \code{\linkS4class{HalfT}}
#' (the default) or \code{NULL}.  If \code{trend} is \code{NULL},
#' then \code{DLM} produces local level model; otherwise
#' \code{DLM} produces a linear trend model.
#' @param damp An object of class \code{\linkS4class{Damp}}
#' (the default) or \code{NULL}.  If \code{damp} is \code{NULL}
#' the level or trend terms are not damped.
#' @param season An object of class \code{\linkS4class{Season}}
#' or \code{NULL} (the default).  If \code{season} is
#' \code{NULL}, the prior does not include a seasonal effect.
#' @param covariates An object of class
#' \code{\linkS4class{Covariates}} or \code{NULL}
#' (the default).  If \code{covariates} is
#' \code{NULL}, the prior does not include covariates.
#' @param error An object of class \code{\link{Error}}.
#'
#' @return An object of class \code{\linkS4class{SpecDLM}}.
#'
#' @seealso \code{\link{ExchFixed}} and \code{\link{Exch}} specify exchangeable
#' in which the order of the units conveys no information about the
#' correlations between them.  \code{DLM}, \code{\link{ExchFixed}},
#' and \code{\link{Exch}} are usually called as part of a call
#' to function \code{\link{Model}}.
#'
#' @references
#' Petris, G., Petrone, S., & Campagnoli, P. (2009). \emph{Dynamic Linear
#' Models with R}. Springer.
#' 
#' Prado, R., & West, M. (2010). \emph{Time Series: Modeling, Computation,
#' and Inference}. CRC Press.
#'
#' @examples
#' ## damped linear trend model with no seasonal effect
#' ## and no covariates
#' DLM()
#'
#' ## damped local level model with no seasonal effect
#' ## and no covariates
#' DLM(trend = NULL)
#'
#' ## local level model with no damping, no seasonal effect,
#' ## and no covariates
#' DLM(trend = NULL, damp = NULL)
#'
#' ## damped linear trend model with seasonal effect
#' ## and no covariates
#' DLM(season = Season(n = 4))
#'
#' ## linear trend model with covariates, but no damping
#' ## and no seasonal effects
#' gdp.data <- data.frame(year = 2000:2016,
#'                        gdp = rnorm(17))
#' gdp.covariates <- Covariates(mean ~ gdp, data = gdp.data)
#' DLM(damp = NULL, covariates = gdp.covariates)
#'
#' ## robustified damped linear trend model
#' DLM(error = Error(robust = TRUE))
#' @export
DLM <- function(along = NULL, level = HalfT(), trend = HalfT(),
                damp = Damp(), season = NULL, covariates = NULL,
                error = Error()) {
    ## along
    along <- checkAndTidyAlongDLM(along)
    ## level
    if (!methods::is(level, "HalfT"))
        stop(gettextf("'%s' has class \"%s\"",
                      "level", class(level)))
    AAlpha <- level@A
    multAlpha <- level@mult
    nuAlpha <- level@nu
    omegaAlphaMax <- level@scaleMax
    ## trend
    if (methods::is(trend, "HalfT")) {
        has.trend <- TRUE
        ADelta <- trend@A
        multDelta <- trend@mult
        nuDelta <- trend@nu
        omegaDeltaMax <- trend@scaleMax
    }
    else {
        if (is.null(trend))
            has.trend <- FALSE
        else
            stop(gettextf("'%s' has class \"%s\"",
                          "trend", class(trend)))
    }
    ## damp
    if (!methods::is(damp, "Damp")) {
        if (is.null(damp))
            damp <- Damp(coef = 1)
        else
            stop(gettextf("'%s' has class \"%s\"",
                          "damp", class(damp)))
    }
    phiKnown <- methods::is(damp, "DampKnown")
    if (phiKnown) {
        minPhi <- 0
        maxPhi <- 1
        phi <- damp@phi
    }
    else {
        minPhi <- damp@minPhi
        maxPhi <- damp@maxPhi
        phi <- as.double(NA)
    }
    phiKnown <- methods::new("LogicalFlag", phiKnown)
    ## season
    if (methods::is(season, "Season")) {
        has.season <- TRUE
        ASeason <- season@ASeason
        multSeason <- season@multSeason
        nSeason <- season@nSeason
        nuSeason <- season@nuSeason
        omegaSeasonMax <- season@omegaSeasonMax
    }
    else {
        if (is.null(season))
            has.season <- FALSE
        else
            if (!methods::is(season, "Season"))
                stop(gettextf("'%s' has class \"%s\"",
                              "season", class(season)))
    }            
    ## covariates
    if (methods::is(covariates, "Covariates")) {
        has.covariates <- TRUE
        AEtaCoef <- covariates@AEtaCoef
        AEtaIntercept <- covariates@AEtaIntercept
        contrastsArg <- covariates@contrastsArg
        data <- covariates@data
        formula <- covariates@formula
        multEtaCoef <- covariates@multEtaCoef
        nuEtaCoef <- covariates@nuEtaCoef
    }
    else {
        if (is.null(covariates))
            has.covariates <- FALSE
        else
            stop(gettextf("'%s' has class \"%s\"",
                          "covariates", class(covariates)))
    }    
    ## error
    if (!methods::is(error, "Error"))
        stop(gettextf("'%s' has class \"%s\"",
                      "error", class(error)))
    ATau <- error@ATau
    multTau <- error@multTau
    nuTau <- error@nuTau
    tauMax <- error@tauMax
    is.robust <- methods::is(error, "ErrorRobust")
    if (is.robust)
        nuBeta <- error@nuBeta
    ## return
    if (!has.trend && !has.season && !has.covariates && !is.robust) {
        methods::new("SpecDLMNoTrendNormZeroNoSeason",
                     AAlpha = AAlpha,
                     ATau = ATau,
                     along = along,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     multAlpha = multAlpha,
                     multTau = multTau,
                     nuAlpha = nuAlpha,
                     nuTau = nuTau,
                     omegaAlphaMax = omegaAlphaMax,
                     phi = phi,
                     phiKnown = phiKnown,
                     tauMax = tauMax)
    }
    else if (has.trend && !has.season && !has.covariates && !is.robust) {
        methods::new("SpecDLMWithTrendNormZeroNoSeason",
                     AAlpha = AAlpha,
                     ADelta = ADelta,
                     ATau = ATau,
                     along = along,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     multAlpha = multAlpha,
                     multDelta = multDelta,
                     multTau = multTau,
                     nuAlpha = nuAlpha,
                     nuDelta = nuDelta,
                     nuTau = nuTau,
                     omegaAlphaMax = omegaAlphaMax,
                     omegaDeltaMax = omegaDeltaMax,
                     phi = phi,
                     phiKnown = phiKnown,
                     tauMax = tauMax)
    }
    else if (!has.trend && has.season && !has.covariates && !is.robust) {
        methods::new("SpecDLMNoTrendNormZeroWithSeason",
                     AAlpha = AAlpha,
                     ASeason = ASeason,
                     ATau = ATau,
                     along = along,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     multAlpha = multAlpha,
                     multSeason = multSeason,
                     multTau = multTau,
                     nSeason = nSeason,
                     nuAlpha = nuAlpha,
                     nuSeason = nuSeason,
                     nuTau = nuTau,
                     omegaAlphaMax = omegaAlphaMax,
                     phi = phi,
                     phiKnown = phiKnown,
                     omegaSeasonMax = omegaSeasonMax,
                     tauMax = tauMax)
    }
    else if (has.trend && has.season && !has.covariates && !is.robust) {
        methods::new("SpecDLMWithTrendNormZeroWithSeason",
                     AAlpha = AAlpha,
                     ADelta = ADelta,
                     ASeason = ASeason,
                     ATau = ATau,
                     along = along,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     multAlpha = multAlpha,
                     multDelta = multDelta,
                     multSeason = multSeason,
                     multTau = multTau,
                     nSeason = nSeason,
                     nuAlpha = nuAlpha,
                     nuDelta = nuDelta,
                     nuSeason = nuSeason,
                     nuTau = nuTau,
                     omegaAlphaMax = omegaAlphaMax,
                     omegaDeltaMax = omegaDeltaMax,
                     omegaSeasonMax = omegaSeasonMax,
                     phi = phi,
                     phiKnown = phiKnown,
                     tauMax = tauMax)
    }
    else if (!has.trend && !has.season && has.covariates && !is.robust) {
        methods::new("SpecDLMNoTrendNormCovNoSeason",
                     AAlpha = AAlpha,
                     AEtaCoef = AEtaCoef,
                     AEtaIntercept = AEtaIntercept,
                     ATau = ATau,
                     along = along,
                     contrastsArg = contrastsArg,
                     data = data,
                     formula = formula,
                     multAlpha = multAlpha,
                     multEtaCoef = multEtaCoef,
                     multTau = multTau,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     nuAlpha = nuAlpha,
                     nuEtaCoef = nuEtaCoef,
                     nuTau = nuTau,
                     omegaAlphaMax = omegaAlphaMax,
                     phi = phi,
                     phiKnown = phiKnown,
                     tauMax = tauMax)
    }
    else if (has.trend && !has.season && has.covariates && !is.robust) {
        methods::new("SpecDLMWithTrendNormCovNoSeason",
                     AAlpha = AAlpha,
                     ADelta = ADelta,
                     AEtaCoef = AEtaCoef,
                     AEtaIntercept = AEtaIntercept,
                     ATau = ATau,
                     along = along,
                     contrastsArg = contrastsArg,
                     data = data,
                     formula = formula,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     multAlpha = multAlpha,
                     multDelta = multDelta,
                     multEtaCoef = multEtaCoef,
                     multTau = multTau,
                     nuAlpha = nuAlpha,
                     nuDelta = nuDelta,
                     nuEtaCoef = nuEtaCoef,
                     nuTau = nuTau,
                     omegaAlphaMax = omegaAlphaMax,
                     omegaDeltaMax = omegaDeltaMax,
                     phi = phi,
                     phiKnown = phiKnown,
                     tauMax = tauMax)
    }
    else if (!has.trend && has.season && has.covariates && !is.robust) {
        methods::new("SpecDLMNoTrendNormCovWithSeason",
                     AAlpha = AAlpha,
                     AEtaCoef = AEtaCoef,
                     AEtaIntercept = AEtaIntercept,
                     ASeason = ASeason,
                     ATau = ATau,
                     along = along,
                     contrastsArg = contrastsArg,
                     data = data,
                     formula = formula,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     multAlpha = multAlpha,
                     multEtaCoef = multEtaCoef,
                     multSeason = multSeason,
                     multTau = multTau,
                     nSeason = nSeason,
                     nuAlpha = nuAlpha,
                     nuEtaCoef = nuEtaCoef,
                     nuSeason = nuSeason,
                     nuTau = nuTau,
                     omegaAlphaMax = omegaAlphaMax,
                     omegaSeasonMax = omegaSeasonMax,
                     phi = phi,
                     phiKnown = phiKnown,
                     tauMax = tauMax)
    }
    else if (has.trend && has.season && has.covariates && !is.robust) {
        methods::new("SpecDLMWithTrendNormCovWithSeason",
                     AAlpha = AAlpha,
                     ADelta = ADelta,
                     AEtaCoef = AEtaCoef,
                     AEtaIntercept = AEtaIntercept,
                     ASeason = ASeason,
                     ATau = ATau,
                     along = along,
                     contrastsArg = contrastsArg,
                     data = data,
                     formula = formula,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     multAlpha = multAlpha,
                     multDelta = multDelta,
                     multEtaCoef = multEtaCoef,
                     multSeason = multSeason,
                     multTau = multTau,
                     nSeason = nSeason,
                     nuAlpha = nuAlpha,
                     nuDelta = nuDelta,
                     nuEtaCoef = nuEtaCoef,
                     nuSeason = nuSeason,
                     nuTau = nuTau,
                     omegaAlphaMax = omegaAlphaMax,
                     omegaDeltaMax = omegaDeltaMax,
                     omegaSeasonMax = omegaSeasonMax,
                     phi = phi,
                     phiKnown = phiKnown,
                     tauMax = tauMax)
    }
    else if (!has.trend && !has.season && !has.covariates && is.robust) {
        methods::new("SpecDLMNoTrendRobustZeroNoSeason",
                     AAlpha = AAlpha,
                     ATau = ATau,
                     along = along,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     multAlpha = multAlpha,
                     multTau = multTau,
                     nuAlpha = nuAlpha,
                     nuBeta = nuBeta,
                     nuTau = nuTau,
                     omegaAlphaMax = omegaAlphaMax,
                     phi = phi,
                     phiKnown = phiKnown,
                     tauMax = tauMax)
    }
    else if (has.trend && !has.season && !has.covariates && is.robust) {
        methods::new("SpecDLMWithTrendRobustZeroNoSeason",
                     AAlpha = AAlpha,
                     ADelta = ADelta,
                     ATau = ATau,
                     along = along,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     multAlpha = multAlpha,
                     multDelta = multDelta,
                     multTau = multTau,
                     nuAlpha = nuAlpha,
                     nuBeta = nuBeta,
                     nuDelta = nuDelta,
                     nuTau = nuTau,
                     omegaAlphaMax = omegaAlphaMax,
                     omegaDeltaMax = omegaDeltaMax,
                     phi = phi,
                     phiKnown = phiKnown,
                     tauMax = tauMax)
    }
    else if (!has.trend && has.season && !has.covariates && is.robust) {
        methods::new("SpecDLMNoTrendRobustZeroWithSeason",
                     AAlpha = AAlpha,
                     ASeason = ASeason,
                     ATau = ATau,
                     along = along,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     multAlpha = multAlpha,
                     multSeason = multSeason,
                     multTau = multTau,
                     nSeason = nSeason,
                     nuAlpha = nuAlpha,
                     nuBeta = nuBeta,
                     nuSeason = nuSeason,
                     nuTau = nuTau,
                     omegaAlphaMax = omegaAlphaMax,
                     omegaSeasonMax = omegaSeasonMax,
                     phi = phi,
                     phiKnown = phiKnown,
                     tauMax = tauMax)
    }
    else if (has.trend && has.season && !has.covariates && is.robust) {
        methods::new("SpecDLMWithTrendRobustZeroWithSeason",
                     AAlpha = AAlpha,
                     ADelta = ADelta,
                     ASeason = ASeason,
                     ATau = ATau,
                     along = along,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     multAlpha = multAlpha,
                     multDelta = multDelta,
                     multSeason = multSeason,
                     multTau = multTau,
                     nSeason = nSeason,
                     nuAlpha = nuAlpha,
                     nuBeta = nuBeta,
                     nuDelta = nuDelta,
                     nuSeason = nuSeason,
                     nuTau = nuTau,
                     omegaAlphaMax = omegaAlphaMax,
                     omegaDeltaMax = omegaDeltaMax,
                     omegaSeasonMax = omegaSeasonMax,
                     phi = phi,
                     phiKnown = phiKnown,
                     tauMax = tauMax)
    }
    else if (!has.trend && !has.season && has.covariates && is.robust) {
        methods::new("SpecDLMNoTrendRobustCovNoSeason",
                     AAlpha = AAlpha,
                     AEtaCoef = AEtaCoef,
                     AEtaIntercept = AEtaIntercept,
                     ATau = ATau,
                     along = along,
                     contrastsArg = contrastsArg,
                     data = data,
                     formula = formula,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     multAlpha = multAlpha,
                     multEtaCoef = multEtaCoef,
                     multTau = multTau,
                     nuAlpha = nuAlpha,
                     nuBeta = nuBeta,
                     nuEtaCoef = nuEtaCoef,
                     nuTau = nuTau,
                     omegaAlphaMax = omegaAlphaMax,
                     phi = phi,
                     phiKnown = phiKnown,
                     tauMax = tauMax)
    }
    else if (has.trend && !has.season && has.covariates && is.robust) {
        methods::new("SpecDLMWithTrendRobustCovNoSeason",
                     AAlpha = AAlpha,
                     ADelta = ADelta,
                     AEtaCoef = AEtaCoef,
                     AEtaIntercept = AEtaIntercept,
                     ATau = ATau,
                     along = along,
                     contrastsArg = contrastsArg,
                     data = data,
                     formula = formula,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     multAlpha = multAlpha,
                     multDelta = multDelta,
                     multEtaCoef = multEtaCoef,
                     multTau = multTau,
                     nuAlpha = nuAlpha,
                     nuBeta = nuBeta,
                     nuDelta = nuDelta,
                     nuEtaCoef = nuEtaCoef,
                     nuTau = nuTau,
                     omegaAlphaMax = omegaAlphaMax,
                     omegaDeltaMax = omegaDeltaMax,
                     phi = phi,
                     phiKnown = phiKnown,
                     tauMax = tauMax)
    }
    else if (!has.trend && has.season && has.covariates && is.robust) {
        methods::new("SpecDLMNoTrendRobustCovWithSeason",
                     AAlpha = AAlpha,
                     AEtaCoef = AEtaCoef,
                     AEtaIntercept = AEtaIntercept,
                     ASeason = ASeason,
                     ATau = ATau,
                     along = along,
                     contrastsArg = contrastsArg,
                     data = data,
                     formula = formula,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     multAlpha = multAlpha,
                     multEtaCoef = multEtaCoef,
                     multSeason = multSeason,
                     multTau = multTau,
                     nSeason = nSeason,
                     nuAlpha = nuAlpha,
                     nuBeta = nuBeta,
                     nuEtaCoef = nuEtaCoef,
                     nuSeason = nuSeason,
                     nuTau = nuTau,
                     omegaAlphaMax = omegaAlphaMax,
                     omegaSeasonMax = omegaSeasonMax,
                     phi = phi,
                     phiKnown = phiKnown,
                     tauMax = tauMax)
    }
    else if (has.trend && has.season && has.covariates && is.robust) {
        methods::new("SpecDLMWithTrendRobustCovWithSeason",
                     AAlpha = AAlpha,
                     ADelta = ADelta,
                     AEtaCoef = AEtaCoef,
                     AEtaIntercept = AEtaIntercept,
                     ASeason = ASeason,
                     ATau = ATau,
                     along = along,
                     contrastsArg = contrastsArg,
                     data = data,
                     formula = formula,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     multAlpha = multAlpha,
                     multDelta = multDelta,
                     multEtaCoef = multEtaCoef,
                     multSeason = multSeason,
                     multTau = multTau,
                     nSeason = nSeason,
                     nuAlpha = nuAlpha,
                     nuBeta = nuBeta,
                     nuDelta = nuDelta,
                     nuEtaCoef = nuEtaCoef,
                     nuSeason = nuSeason,
                     nuTau = nuTau,
                     omegaAlphaMax = omegaAlphaMax,
                     omegaDeltaMax = omegaDeltaMax,
                     omegaSeasonMax = omegaSeasonMax,
                     phi = phi,
                     phiKnown = phiKnown,
                     tauMax = tauMax)
    }
}

## NO_TESTS
#' Specify the error term in a prior for a main effect or interaction.
#' 
#' Priors for main effects or interactions, such as age effects or
#' age-sex interactions, typically include an error term.  The error
#' terms can be specified using  function \code{Error}.
#'
#' If \code{robust = FALSE} (the default), then the error term has a normal
#' distribution,
#' 
#'   \code{error[j] ~ N(0, scale^2)}.
#'
#' The \code{scale} parameter has a half-\emph{t} prior, which is described in the
#' documentation for function \code{\link{HalfT}}.
#'
#' If \code{robust = TRUE}, then, by default, the error term has a \emph{t}
#' distribution with 4 degrees of freedom,
#' 
#'   \code{error[j] ~ t(4, 0, scale^2)}.
#'
#' Users can supply alternative values for the degrees of freedom.
#' The \code{scale} parameter has a half-\emph{t} prior, described
#' in the documentation for \code{\link{HalfT}}.
#'
#' @param robust Whether to use a \emph{t} distribution.  Defaults to
#' \code{FALSE}.
#' @param df The degrees of freedom for the \emph{t} distribution.
#' Defaults to 4.
#' @param scale Prior for the \code{scale} parameter.  An object of class
#' \code{\linkS4class{HalfT}}.
#'
#' @return An object of class \code{\linkS4class{Error}}
#'
#' @examples
#' Error()
#' Error(robust = TRUE)
#' Error(robust = TRUE, df = 10)
#' Error(scale = HalfT(scale = 0.5, max = 1))
#' Error(scale = HalfT(mult = 0.5))
#' @export
Error <- function(robust = FALSE, df = 4, scale = HalfT()) {
    checkLogical(x = robust, name = "robust")
    if (!methods::is(scale, "HalfT"))
        stop(gettextf("'%s' has class \"%s\"",
                      scale, class(scale)))
    ATau <- scale@A
    multTau <- scale@mult
    nuTau <- scale@nu
    tauMax <- scale@scaleMax
    if (robust) {
        nuBeta <- checkAndTidyNu(x = df, name = "df")         
        methods::new("ErrorRobust",
                     ATau = ATau,
                     multTau = multTau,
                     nuBeta = nuBeta,
                     nuTau = nuTau,
                     tauMax = tauMax)
    }
    else {
        if (methods::hasArg(df))
            warning(gettextf("'%s' is ignored when '%s' is %s",
                             "df", "robust", "FALSE"))
        methods::new("ErrorNorm",
                     ATau = ATau,
                     multTau = multTau,
                     nuTau = nuTau,
                     tauMax = tauMax)
    }
}

## NO_TESTS
#' Specify an exchangeable prior.
#'
#' Specify a prior for main effects or interactions in which the units are
#' exchangeable.  Units are exchangeable if the labels or ordering of the
#' units does not provide any information about their distribution.
#'
#' The prior specified by \code{Exch} has the form
#'
#' \code{parameter[j] = covariates[j] + error[j].}
#'
#' Including a covariates term can make the assumption of exchangeability
#' more plausible.  For instance, treating regional effects as exchangeable
#' is more plausible after the region effects have been adjusted for
#' differences in regions' incomes and population sizes.  A covariates term
#' can be specified using function \code{\link{Covariates}}.
#'
#' Errors can have a normal or \emph{t} distribution.  The error term is
#' specified using function \code{\link{Error}}.
#'
#' @inheritParams DLM
#'
#' @return An object of class \code{\linkS4class{SpecExch}}.
#'
#' @seealso \code{\link{ExchFixed}} specifies a simpler
#' exchangeable prior.  \code{\link{DLM}} specifies a
#' non-exchangeable prior.
#'
#' @examples
#' Exch()
#' Exch(error = Error(robust = TRUE))
#' Exch(error = Error(scale = HalfT(scale = 0.5)))
#'
#' ## with covariates
#' data <- data.frame(region = letters[1:10],
#'                    income = rnorm(10))
#' Exch(covariates = Covariates(mean ~ income, data = data))
#' @export
Exch <- function(covariates = NULL, error = Error()) {
    ## covariates
    has.covariates <- !is.null(covariates)
    if (has.covariates) {
        if (!methods::is(covariates, "Covariates"))
            stop(gettextf("'%s' has class",
                          "covariates", class(covariates)))
        AEtaCoef <- covariates@AEtaCoef
        AEtaIntercept <- covariates@AEtaIntercept
        contrastsArg <- covariates@contrastsArg
        data <- covariates@data
        formula <- covariates@formula
        multEtaCoef <- covariates@multEtaCoef
        nuEtaCoef <- covariates@nuEtaCoef
    }
    ## error
    if (!methods::is(error, "Error"))
        stop(gettextf("'%s' has class \"%s\"",
                      "error", class(error)))
    ATau <- error@ATau
    multTau <- error@multTau
    nuTau <- error@nuTau
    tauMax <- error@tauMax
    is.robust <- methods::is(error, "ErrorRobust")
    if (is.robust)
        nuBeta <- error@nuBeta
    ## return
    if (!is.robust && !has.covariates) {
        methods::new("SpecExchNormZero",
                     ATau = ATau,
                     multTau = multTau,
                     nuTau = nuTau,
                     tauMax = tauMax)
    }
    else if (is.robust && !has.covariates) {
        methods::new("SpecExchRobustZero",
                     ATau = ATau,
                     multTau = multTau,
                     nuBeta = nuBeta,
                     nuTau = nuTau,
                     tauMax = tauMax)
    }
    else if (!is.robust && has.covariates) {
        methods::new("SpecExchNormCov",
                     AEtaCoef = AEtaCoef,
                     AEtaIntercept = AEtaIntercept,
                     ATau = ATau,
                     contrastsArg = contrastsArg,
                     data = data,
                     formula = formula,
                     multEtaCoef = multEtaCoef,
                     multTau = multTau,
                     nuEtaCoef = nuEtaCoef,
                     nuTau = nuTau,
                     tauMax = tauMax)
    }
    else {
        methods::new("SpecExchRobustCov",
                     AEtaCoef = AEtaCoef,
                     AEtaIntercept = AEtaIntercept,
                     ATau = ATau,
                     contrastsArg = contrastsArg,
                     data = data,
                     formula = formula,
                     multEtaCoef = multEtaCoef,
                     multTau = multTau,
                     nuBeta = nuBeta,
                     nuEtaCoef = nuEtaCoef,
                     nuTau = nuTau,
                     tauMax = tauMax)
    }
}

## NO_TESTS
#' Specify a simple exchangeable prior.
#'
#' A simpler exchangeable prior than \code{\link{Exch}}.  The units are
#' assumed to be drawn from a normal distribution with no covariates and
#' a known standard deviation.
#'
#' The prior specified by \code{ExchFixed} has the form
#'
#' \code{parameter[j] ~ N(0, sd^2).}
#'
#' If a value for \code{sd} is not supplied by the user, then a value is
#' generated when function \code{\link{estimateModel}},
#' \code{\link{estimateCounts}}, or \code{\link{estimateAccount}} is called.
#' Let \eqn{s} be the standard deviation of data \code{y}, or of \code{log(y)}
#' in the case of a Poisson model without exposure, and let \eqn{m} be
#' the \code{mult} argument.  The default value for \code{sd} is then
#' 
#' \tabular{ll}{
#'   \strong{Model} \tab \strong{Default} \cr
#'   Poisson with exposure \tab \eqn{m} \cr
#'   Poisson without exposure \tab  \eqn{ms} \cr
#'   binomial \tab \eqn{m} \cr
#'   normal \tab  \eqn{ms}.
#' }
#'
#' These value are designed to be weakly informative, in that they favour
#' values for \code{sd} that are demographically plausible.
#' 
#' \code{ExchFixed} can be useful a dimension with few elements, such as sex
#' dimension, where there is insufficient information to justify more
#' complicated priors.
#'
#' @param sd Standard deviation.  Optional.
#' @param mult Multiplier applied to \code{sd}, if \code{sd}
#' is generated automatically.  Defaults to 1.
#'
#' @return An object of class \code{\linkS4class{SpecExchFixed}}.
#'
#' @seealso \code{\link{Exch}} specifies a more complicated
#' exchangeable prior.
#'
#' @examples
#' ## default standard deviation
#' ExchFixed()
#' ## user-specified standard deviation
#' ExchFixed(sd = 0.5)
#' ## reduce the size of the automatically-generated 'sd'
#' ExchFixed(mult = 0.5)
#' ## increase the size of the automatically-generated 'sd'
#' ExchFixed(mult = 2)
#' @export
ExchFixed <- function(sd = NULL, mult = 1) {
    tau <- checkAndTidySpecScale(x = sd, name = "sd")
    multTau <- checkAndTidyMult(mult = mult,
                                scale = tau,
                                nameScale = "sd")
    methods::new("SpecExchFixed",
                 tau = tau,
                 multTau = multTau)
}

## NO_TESTS
#' Specify a half-t distribution.
#'
#' If \code{x} has a \emph{t} distribution, then \code{abs(x)} has a
#' half-\emph{t} distribution, also known as a folder \emph{t}
#' distribution.  In package \pkg{demest}, most standard deviation or
#' scale parameters have half-\emph{t} priors.
#'
#' A half-\emph{t} distribution with degrees of freedom \eqn{n} and
#' scale \eqn{A} has density
#'
#' \deqn{p(y) \propto ((y^2)/n + A^2)^(-(n+1)/2).}
#' 
#' Setting a maximum value that the standard deviation or scale
#' parameter can take (and so specifying a \emph{truncated} half-\emph{t}
#' distribution) can reduce numerical problems, and dramatically
#' improve convergence.  By default, the maximum value is set to the 0.999
#' quantile.Users can specify alternative values, including infinity.
#'
#' \code{HalfT} is typically used to specify the prior for
#' a main effect or interaction.  In this case, if a value for \code{scale}
#' is not specified, a default value is determined when function
#' \code{\link{estimateModel}}, \code{\link{estimateCounts}}, or
#' \code{\link{estimateAccount}} is called. Let \eqn{s} be the standard
#' deviation of data \eqn{y}, or of \code{log(y)} in the case of a
#' Poisson model without exposure. Let \eqn{d} be the degree of an
#' interaction: for instance, an interaction between age and sex has
#' degree 2, and an interaction between age, sex, and region has degree 3.
#' Let \eqn{m} be the \code{mult} argument. The default value for \code{scale}
#' is then
#' 
#' \tabular{lll}{
#'   \strong{Model} \tab \strong{Term} \tab \strong{Default} \cr
#'   Poisson with exposure \tab Main effect \tab \eqn{m} \cr 
#'   Poisson with exposure \tab Interaction of degree \eqn{d} \tab \eqn{m 0.5^{d-1}} \cr
#'   Poisson without exposure \tab Main effect \ \eqn{ms} \cr
#'   Poisson without exposure \tab Interaction of degree \eqn{d} \tab \eqn{ms 0.5^{d-1}} \cr
#'   binomial \tab Main effect \tab \eqn{m} \cr
#'   binomial \tab Interaction of degree \eqn{d} \tab \eqn{m 0.5^{d-1}} \cr
#'   normal \tab Main effect \tab \eqn{ms} \cr
#'   normal \tab Interaction of degree \eqn{d} \tab \eqn{ms 0.5^{d-1}}.
#' }
#'
#' @param df Degrees of freedom of the half-\emph{t} distribution.
#' A positive number, defaulting to 7
#' @param scale Scale parameter for the  half-\emph{t} distribution.
#' A positive number.
#' @param max A positive number.  If finite, the half-\emph{t}
#' distribution is truncated at this point.
#' @param mult Multiplier applied to \code{scale} if \code{scale}
#' is generated automatically.  Defaults to 1.
#'
#' @return Object of class \code{\linkS4class{HalfT}}.
#'
#' @references
#' Brazauskas, V., and Kleefeld, A. (2011) Folded and log-folded-t
#' distributions as models for insurance loss data.
#' \emph{Scandinavian Actuarial Journal} 59-74.
#'
#' Gelman, A. (2006). Prior distributions for variance parameters in
#' hierarchical models (comment on article by Browne and Draper).
#' \emph{Bayesian Analysis} 1, 515-534.
#'
#' Gelman, A., Carlin, J.B., Stern, H.S. and Rubin, D.B., 2014.
#' \emph{Bayesian Ddata Analysis. Third Edition.} Boca Raton, FL, USA:
#' Chapman & Hall/CRC. Section 16.3.
#' 
#' @seealso \code{\link{Error}}, \code{\link{Exch}}, \code{\link{Exch}}
#'
#' @examples
#' HalfT()
#' HalfT(scale = 0.5)
#' HalfT(df = 1, scale = 10)
#' HalfT(scale = 0.5, max = 4)
#' HalfT(mult = 0.5)
#' HalfT(mult = 2)
#' @export
HalfT <- function(df = 7, scale = NULL, max = NULL, mult = 1) {
    nu <- checkAndTidyNu(x = df, name = "df")
    A <- checkAndTidySpecScale(x = scale,
                               name = "scale")
    scaleMax <- checkAndTidyScaleMax(x = max,
                                     name = "max",
                                     nu = nu,
                                     A = A)
    mult <- checkAndTidyMult(mult = mult,
                             scale = A,
                             nameScale = "scale")
    methods::new("HalfT",
                 A = A,
                 mult = mult,
                 nu = nu,
                 scaleMax = scaleMax)
}

Classes <- function(cl, scale = NULL, mult = 1) {
    if (!methods::is(cl, "Values"))
        stop(gettextf("'%s' has class \"%s\"",
                      "cl", class(cl)))
    dimtypes <- dimtypes(cl, use.names = FALSE)
    if (!("origin" %in% dimtypes))
        stop(gettextf("'%s' does not have dimension with dimtype \"%s\"",
                      "cl", "origin"))
    .Data <- cl@.Data
    if (length(.Data) == 0L)
        stop(gettextf("'%s' has length %d",
                      "cl", 0L))
    if (any(is.na(.Data)))
        stop(gettextf("'%s' has missing values",
                      "cl"))
    if (any(round(.Data) != .Data))
        stop(gettextf("'%s' has non-integer values",
                      "cl"))
    .Data <- as.integer(.Data)
    if (any(.Data) < 0L)
        stop(gettextf("'%s' has negative values",
                      "cl"))
    unique.cl <- unique(as.integer(.Data))
    n.cl <- length(unique.cl)
    if (n.cl == 1L)
        stop(gettextf("'%s' contains only one distinct value",
                      "cl"))
    min.val <- min(unique.cl)
    if (!(min.val %in% 0:1))
        stop(gettextf("minimum value for '%s' must be %d or %d",
                      "cl", 0L, 1L))
    if (any(diff(sort(unique.cl)) != 1L))
        stop(gettextf("distinct values from '%s' must be consecutive integers",
                      "cl"))
    cl <- toInteger(cl)
    A <- checkAndTidySpecScale(x = scale,
                               name = "scale")
    mult <- checkAndTidyMult(mult = mult,
                             scale = A,
                             nameScale = "scale")
    methods::new("Classes",
                 classes = cl,
                 A = A,
                 mult = mult)                 
}

Move <- function(classes, covariates = NULL, error = Error()) {
    ## classes
    if (!methods::is(classes, "Classes"))
        stop(gettextf("'%s' has class \"%s\"",
                      "classes", class(classes)))
    AMove <- classes@A
    multMove <- classes@mult
    classes <- classes@classes
    ## covariates
    has.covariates <- !is.null(covariates)
    if (has.covariates) {
        if (!methods::is(covariates, "Covariates"))
            stop(gettextf("'%s' has class",
                          "covariates", class(covariates)))
        AEtaCoef <- covariates@AEtaCoef
        AEtaIntercept <- covariates@AEtaIntercept
        contrastsArg <- covariates@contrastsArg
        data <- covariates@data
        formula <- covariates@formula
        multEtaCoef <- covariates@multEtaCoef
        nuEtaCoef <- covariates@nuEtaCoef
    }
    ## error
    if (!methods::is(error, "Error"))
        stop(gettextf("'%s' has class \"%s\"",
                      "error", class(error)))
    ATau <- error@ATau
    multTau <- error@multTau
    nuTau <- error@nuTau
    tauMax <- error@tauMax
    is.robust <- methods::is(error, "ErrorRobust")
    if (is.robust)
        nuBeta <- error@nuBeta
    ## return
    if (!is.robust && !has.covariates) {
        methods::new("SpecMoveNormZero",
                     classes = classes,
                     AMove = AMove,
                     ATau = ATau,
                     multMove = multMove,
                     multTau = multTau,
                     nuTau = nuTau,
                     tauMax = tauMax)
    }
    else if (is.robust && !has.covariates) {
        methods::new("SpecMoveRobustZero",
                     classes = classes,
                     AMove = AMove,
                     ATau = ATau,
                     multMove = multMove,
                     multTau = multTau,
                     nuBeta = nuBeta,
                     nuTau = nuTau,
                     tauMax = tauMax)
    }
    else if (!is.robust && has.covariates) {
        methods::new("SpecMoveNormCov",
                     classes = classes,
                     AEtaCoef = AEtaCoef,
                     AEtaIntercept = AEtaIntercept,
                     AMove = AMove,
                     ATau = ATau,
                     contrastsArg = contrastsArg,
                     data = data,
                     formula = formula,
                     multEtaCoef = multEtaCoef,
                     multMove = multMove,
                     multTau = multTau,
                     nuEtaCoef = nuEtaCoef,
                     nuTau = nuTau,
                     tauMax = tauMax)
    }
    else {
        methods::new("SpecMoveRobustCov",
                     classes = classes,
                     AEtaCoef = AEtaCoef,
                     AEtaIntercept = AEtaIntercept,
                     AMove = AMove,
                     ATau = ATau,
                     contrastsArg = contrastsArg,
                     data = data,
                     formula = formula,
                     multEtaCoef = multEtaCoef,
                     multMove = multMove,
                     multTau = multTau,
                     nuBeta = nuBeta,
                     nuEtaCoef = nuEtaCoef,
                     nuTau = nuTau,
                     tauMax = tauMax)
    }
}
    

## NO_TESTS
#' Specify a normal distribution centered at 0.
#'
#' Specify a normal distribution with mean equal to 0 and standard
#' deviation equal to \code{scale}.
#'
#' \code{Norm} is used to specify the prior for the intercept in
#' function \code{\link{Covariates}}.  If a value for \code{scale}
#' is not specified, a default value is determined when function
#' \code{\link{estimateModel}}, \code{\link{estimateCounts}}, or
#' \code{\link{estimateAccount}} is called.  Let \eqn{s} be the standard
#' deviation of data \eqn{y}, or of \code{log(y)} in the case of a Poisson
#' model without exposure.  Then the default for \code{scale} is
#' determined as follows.
#' 
#' \tabular{ll}{
#'   \strong{Model} \tab \strong{Default} \cr
#'   Poisson with exposure \tab \eqn{10} \cr
#'   Poisson without exposure \tab  \eqn{10s} \cr
#'   binomial \tab \eqn{10} \cr
#'   normal \tab  \eqn{10s}
#' }
#'
#' @param scale Standard deviation for the normal distribution.
#' A positive number.
#'
#' @return An object of class \code{\linkS4class{Norm}}.
#'
#' @seealso \code{Norm} is used in calls to function \code{\link{Covariates}}.
#'
#' @examples
#' Norm()
#' Norm(scale = 0.5)
#' @export
Norm <- function(scale = NULL) {
    A <- checkAndTidySpecScale(x = scale, name = "scale")
    methods::new("Norm",
                 A = A)
}


## NO_TESTS
#' Specify a seasonal effect in a DLM prior.
#'
#' A \code{\link{DLM}} prior can contain a seasonal effect, which
#' is specified using function \code{Season}. Season effects can
#' can change over time.
#'
#' In a prior for a main effect, the seasonal effect has the form
#'
#' \code{s[j] = s[j-n] + errorSeason[j],}
#'
#' where \code{n} is the number of seasons.
#'
#' In a prior for an interaction, the seasonal effect has the form
#' 
#' \code{s[k,l] = s[k-n,l] + errorSeason[k,l].}
#'
#' (See the documentation for function \code{\link{DLM}} for
#' an explanation of the \code{k,l} subscripts.)
#'
#' The error term has the form
#'
#' \code{errorSeason[j] ~ N(0, scaleSeason^2)}
#'
#' or 
#'
#' \code{errorSeason[k,l] ~ N(0, scaleSeason^2)}.
#'
#' Standard deviation \code{scaleSeason} has a half-\emph{t} prior,
#' which can be specified using function \code{\link{HalfT}}.
#
#' Season effects are not fixed, but instead evolve over time.  The speed
#' with which season effects change is governed by \code{scaleSeason}.
#'
#' @param n Number of seasons.  An integer greater than 1.
#' @param scale Prior for the \code{scaleSeason} parameter. An object of class
#' \code{\linkS4class{HalfT}}.
#'
#' @return An object of class \code{\linkS4class{Season}}
#'
#' @seealso \code{Season} supplies an option argument
#' to function \code{\link{DLM}}.
#'
#' @examples
#' Season(n = 4)
#' Season(n = 2, scale = HalfT(scale = 3))
#' @export
Season <- function(n, scale = HalfT()) {
    n <- checkAndTidyNSeason(n)
    ASeason <- scale@A
    multSeason <- scale@mult
    nuSeason <- scale@nu
    omegaSeasonMax <- scale@scaleMax
    methods::new("Season",
                 ASeason = ASeason,
                 multSeason = multSeason,
                 nSeason = n,
                 nuSeason = nuSeason,
                 omegaSeasonMax = omegaSeasonMax)
}




    
