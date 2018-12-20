


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

## NO_TESTS
#' Specify priors for the components in a Mix prior.
#'
#' In normal usage, it is better to accept the default prior
#' for the components than to specify it explicitly.
#' End users are therefore unlikely to need this function.
#'
#' A \code{\link{Mix}} prior treats an interaction as a mixture of
#' normal distributions.  The means for these normal distributions
#' are formed by multiplying a component for each dimension
#' other than the 'along' dimension (typically the time dimension).
#' Each element of these components has a normal prior with mean
#' 0 and standard deviation \code{scaleComponent}.  The prior
#' for \code{scaleComponent} can be set using the \code{scale}
#' argument.
#'
#' Let \eqn{s} be the standard deviation of data \eqn{y},
#' or of \code{log(y)} in the case of a Poisson model without
#' exposure.  Let \eqn{m} be the \code{mult} argument. Then
#' the default value for scale parameter in the half-t distribution is
#'
#' \tabular{ll}{
#'   \strong{Model} \tab \strong{Default} \cr
#'   Poisson with exposure  \tab \eqn{m 0.5} \cr
#'   Poisson without exposure \tab \eqn{ms 0.5} \cr
#'   binomial  \tab \eqn{m 0.5} \cr
#'   normal \tab \eqn{ms 0.5}
#' }
#'
#' @param scale An object of class \code{\link{HalfT}}.
#'
#' @return An object of class \code{\linkS4class{Components}}.
#'
#' @seealso Mixture priors are specified using \code{\link{Mix}}.
#' The weights in a mixture prior are specified using
#' \code{\link{Weights}}.
#'
#' @examples
#' Components()
#' ## A tighter prior on 'scaleComponent'
#' Components(scale = HalfT(scale = 0.25))
#' 
#' @export
Components <- function(scale = HalfT()) {
    if (!methods::is(scale, "HalfT"))
        stop(gettextf("'%s' has class \"%s\"",
                      scale, class(scale)))
    AVectorsMix <- scale@A
    multVectorsMix <- scale@mult
    nuVectorsMix <- scale@nu
    omegaVectorsMaxMix <- scale@scaleMax
    methods::new("Components",
                 AVectorsMix = AVectorsMix,
                 multVectorsMix = multVectorsMix,
                 nuVectorsMix = nuVectorsMix,
                 omegaVectorsMaxMix = omegaVectorsMaxMix)
}



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
#' When modelling mortality rates with data that include the "infant"
#' age group (ie age 0), it is good practice to include an indicator
#' variable for this age group, since its mortality rates are much
#' higher than subsequent age groups.  If \code{infant} is \code{TRUE},
#' then an indicator is set up automatically.
#'
#' \code{infant = TRUE} can only be used when specifying the prior
#' for a main effect for age.  If \code{infant} is \code{TRUE},
#' then \code{formula} and \code{data} can also be supplied,
#' but do not have to be.
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
#' The intercept term is assumed to have a diffuse normal distribution.
#' The rules for choosing default values for the standard deviation are
#' described in the documentation for \code{\link{Norm}}. \code{mean}
#' must equal 0 (the default).
#' 
#' The help for \code{\link[stats]{model.matrix}} contains a discussion of
#' contrasts.  With Bayesian models that have informative priors, such as the
#' t priors used by \code{Covariates}, all levels of a factor can be included
#' in the model. See below for an example. 
#' 
#' @param formula A \code{\link[stats]{formula}} with response \code{mean}.
#' @param data A data.frame containing covariate data.
#' @param infant Logical. Whether to add an 'infant' indicator to the
#' covariates specified by \code{formula}. Defaults to \code{FALSE}.
#' @param contrastsArg A named list, the elements which are matrices
#'     or names of contrasts functions.
#' @param intercept An object of class \code{\linkS4class{Norm}}.
#' @param coef An object of class \code{\link{TDist}}.
#'
#' @return An object of class \code{\linkS4class{Covariates}}
#'
#' @references
#' Gelman, A., Carlin, J.B., Stern, H.S. and Rubin, D.B., 2014.
#' \emph{Bayesian Data Analysis. Third Edition.} Boca Raton, FL, USA:
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
#'            intercept = Norm(sd = 5),
#'            coef = TDist(scale = 0.25))
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
#'
#' ## 'infant' indicator
#' Covariates(infant = TRUE)
#' @export 
Covariates <- function(formula = NULL, data = NULL, infant = FALSE,
                       contrastsArg = list(), intercept = Norm(),
                       coef = TDist()) {
    checkCovariateFormula(formula)
    checkCovariateData(x = data, name = "data")
    checkModelMatrix(formula = formula,
                     data = data,
                     contrastsArg = contrastsArg)
    checkInfant(infant)
    if (is.null(formula) & !is.null(data))
        stop(gettextf("'%s' supplied but '%s' not supplied",
                      "data", "formula"))
    if (!is.null(formula) && is.null(data))
        stop(gettextf("'%s' supplied but '%s' not supplied",
                      "formula", "data"))
    if (is.null(formula) && is.null(data)) {
        if (!infant)
            stop(gettextf("'%s' and '%s' not supplied, and '%s' is %s",
                          "formula", "data", "infant", "FALSE"))
        data <- methods::new("data.frame")
        formula <- methods::new("formula")
    }
    if (!methods::is(intercept, "Norm"))
        stop(gettextf("'%s' has class \"%s\"",
                      "intercept", class(intercept)))
    if (!methods::is(coef, "TDist"))
        stop(gettextf("'%s' has class \"%s\"",
                      "coef", class(coef)))
    mean <- intercept@mean@.Data
    if (!isTRUE(all.equal(mean, 0)))
        stop(gettextf("'%s' in '%s' does not equal %d",
                      "mean", "intercept", 0L))
    AEtaIntercept <- intercept@A
    AEtaCoef <- coef@AEtaCoef
    meanEtaCoef <- coef@meanEtaCoef
    multEtaCoef <- coef@multEtaCoef
    nuEtaCoef <- coef@nuEtaCoef
    infant <- methods::new("LogicalFlag", infant)
    methods::new("Covariates",
                 AEtaCoef = AEtaCoef,
                 AEtaIntercept = AEtaIntercept,
                 contrastsArg = contrastsArg,
                 data = data,
                 formula = formula,
                 infant = infant,
                 meanEtaCoef = meanEtaCoef,
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
#' and in a linear trend model, the trend term has the form
#'
#' \code{trend[j] ~ damp * trend[j-1] + errorTrend[j]}.
#'
#' With a prior for an interaction, in a local level model, the level term
#' has the form
#'
#' \code{level[k,l] ~ damp * level[k-1,l] + errorLevel[k,l]}.
#'
#' and in linear trend model, the trend term has the form
#'
#' \code{trend[k,l] ~ damp * trend[k-1,l] + errorTrend[k,l]}
#'
#' (See the documentation for function \code{\link{DLM}} for
#' an explanation of the \code{k,l} subscripts.)
#'
#' Values of for \code{damp} are restricted to the range \code{0 <= damp <= 1}.
#' In linear trend models, including a damping term with a value near 1
#' typically results in more accurate forecasts (Hyndman et al 2008).  There
#' are exceptions, however: for instance, damping of the trend for the time
#' effect is probably not appropriate in mortality forecasts for developed
#' countries (Oeppen and Vaupel 2002).  Damping is also not necessary
#' appropriate in local level models.
#'
#' The user can set the level of damping by providing a value for the
#' \code{coef} argument.  Alternatively, an appropriate value can be inferred
#' from the data, using a beta prior on the transformed parameter
#' \code{(damp - min)/(max - min)}.  The beta prior is specified
#' using parameters \code{shape1} and \code{shape1}.  The default
#' values give a boundary-avoiding prior, confined to the range \code{(min, max)},
#' with \code{min} defaulting to 0.8 and \code{max} defaulting to \code{1}.
#' (See Gelman et al 2014, pp313-318, for a definition
#' of boundary-avoiding priors.) Setting \code{shape1 = 1} and \code{shape2 = 1}
#' gives a uniform prior on the range \code{(min, max)}.
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
#'
#' ## uniform prior on the range (0, 1)
#' Damp(min = 0, max = 1, shape1 = 1, shape2 = 1)
#'
#' ## informative prior favouring high values, but
#' ## not ruling out any value between 0 and 1
#' Damp(min = 0, max = 1, shape1 = 9, shape2 = 1)
#' @export
Damp <- function(coef = NULL, shape1 = 2, shape2 = 2, min = 0.8, max = 1) {
    phi.known <- !is.null(coef)
    if (phi.known) {
        if (methods::hasArg(shape1))
            warning(gettextf("'%s' is ignored when '%s' is non-%s",
                             "shape1", "coef", "NULL"))
        if (methods::hasArg(shape2))
            warning(gettextf("'%s' is ignored when '%s' is non-%s",
                             "shape2", "coef", "NULL"))
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
        checkPositiveNumeric(x = shape1,
                             name = "shape1")
        checkPositiveNumeric(x = shape2,
                             name = "shape2")
        shape1 <- as.double(shape1)
        shape2 <- as.double(shape2)
        shape1 <- methods::new("Scale", shape1)
        shape2 <- methods::new("Scale", shape2)
        min.max <- checkAndTidyPhiMinMax(min = min, max = max)
        methods::new("DampUnknown",
                     minPhi = min.max[1L],
                     maxPhi = min.max[2L],
                     shape1Phi = shape1,
                     shape2Phi = shape2)
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
#' If \code{level} is \code{NULL}, then the model does not
#' include \code{errorLevel}.  This can be a useful simplification
#' in situations where there is a trend in the data, but the full
#' linear trend model is not converging properly.
#' 
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
#' @param level An object of class \code{\linkS4class{Level}}
#' specifying the prior for \code{errorLevel} or \code{NULL}.
#' \code{level} can be \code{NULL} only when \code{trend} is
#' non-\code{NULL}.
#' @param trend An object of class \code{\linkS4class{Trend}}
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
DLM <- function(along = NULL, level = Level(), trend = Trend(),
                damp = Damp(), season = NULL, covariates = NULL,
                error = Error()) {
    ## along
    along <- checkAndTidyAlongDLM(along)
    ## level
    if (methods::is(level, "Level")) {
        has.level <- TRUE
        AAlpha <- level@AAlpha
        multAlpha <- level@multAlpha
        nuAlpha <- level@nuAlpha
        omegaAlphaMax <- level@omegaAlphaMax
    }
    else {
        if (is.null(level)) {
            has.level <- FALSE
            level <- Level()
            AAlpha <- level@AAlpha
            multAlpha <- level@multAlpha
            nuAlpha <- level@nuAlpha
            omegaAlphaMax <- level@omegaAlphaMax
        }
        else {
            stop(gettextf("'%s' has class \"%s\"",
                          "level", class(level)))
        }
    }
    has.level <- methods::new("LogicalFlag", has.level)
    ## trend
    if (methods::is(trend, "Trend")) {
        has.trend <- TRUE
        ADelta <- trend@ADelta
        ADelta0 <- trend@ADelta0
        meanDelta0 <- trend@meanDelta0
        multDelta <- trend@multDelta
        multDelta0 <- trend@multDelta0
        nuDelta <- trend@nuDelta
        omegaDeltaMax <- trend@omegaDeltaMax
    }
    else {
        if (is.null(trend))
            has.trend <- FALSE
        else
            stop(gettextf("'%s' has class \"%s\"",
                          "trend", class(trend)))
    }
    if (!has.level && !has.trend)
        stop(gettextf("'%s' and '%s' are both %s",
                      "level", "trend", "NULL"))
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
        shape1Phi <- methods::new("Scale", 1)
        shape2Phi <- methods::new("Scale", 1)
        phi <- damp@phi
    }
    else {
        minPhi <- damp@minPhi
        maxPhi <- damp@maxPhi
        shape1Phi <- damp@shape1Phi
        shape2Phi <- damp@shape2Phi
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
        infant <- covariates@infant
        meanEtaCoef <- covariates@meanEtaCoef
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
                     shape1Phi = shape1Phi,
                     shape2Phi = shape2Phi,
                     tauMax = tauMax)
    }
    else if (has.trend && !has.season && !has.covariates && !is.robust) {
        methods::new("SpecDLMWithTrendNormZeroNoSeason",
                     AAlpha = AAlpha,
                     ADelta = ADelta,
                     ADelta0 = ADelta0,
                     ATau = ATau,
                     along = along,
                     hasLevel = has.level,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     meanDelta0 = meanDelta0,
                     multAlpha = multAlpha,
                     multDelta = multDelta,
                     multDelta0 = multDelta0,
                     multTau = multTau,
                     nuAlpha = nuAlpha,
                     nuDelta = nuDelta,
                     nuTau = nuTau,
                     omegaAlphaMax = omegaAlphaMax,
                     omegaDeltaMax = omegaDeltaMax,
                     phi = phi,
                     phiKnown = phiKnown,
                     shape1Phi = shape1Phi,
                     shape2Phi = shape2Phi,
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
                     shape1Phi = shape1Phi,
                     shape2Phi = shape2Phi,
                     tauMax = tauMax)
    }
    else if (has.trend && has.season && !has.covariates && !is.robust) {
        methods::new("SpecDLMWithTrendNormZeroWithSeason",
                     AAlpha = AAlpha,
                     ADelta = ADelta,
                     ADelta0 = ADelta0,
                     ASeason = ASeason,
                     ATau = ATau,
                     along = along,
                     hasLevel = has.level,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     meanDelta0 = meanDelta0,
                     multAlpha = multAlpha,
                     multDelta = multDelta,
                     multDelta0 = multDelta0,
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
                     shape1Phi = shape1Phi,
                     shape2Phi = shape2Phi,
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
                     infant = infant,
                     multAlpha = multAlpha,
                     meanEtaCoef = meanEtaCoef,
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
                     shape1Phi = shape1Phi,
                     shape2Phi = shape2Phi,
                     tauMax = tauMax)
    }
    else if (has.trend && !has.season && has.covariates && !is.robust) {
        methods::new("SpecDLMWithTrendNormCovNoSeason",
                     AAlpha = AAlpha,
                     ADelta = ADelta,
                     ADelta0 = ADelta0,
                     AEtaCoef = AEtaCoef,
                     AEtaIntercept = AEtaIntercept,
                     ATau = ATau,
                     along = along,
                     contrastsArg = contrastsArg,
                     data = data,
                     formula = formula,
                     hasLevel = has.level,
                     infant = infant,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     meanDelta0 = meanDelta0,
                     multAlpha = multAlpha,
                     multDelta = multDelta,
                     multDelta0 = multDelta0,
                     meanEtaCoef = meanEtaCoef,
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
                     shape1Phi = shape1Phi,
                     shape2Phi = shape2Phi,
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
                     infant = infant,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     multAlpha = multAlpha,
                     meanEtaCoef = meanEtaCoef,
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
                     shape1Phi = shape1Phi,
                     shape2Phi = shape2Phi,
                     tauMax = tauMax)
    }
    else if (has.trend && has.season && has.covariates && !is.robust) {
        methods::new("SpecDLMWithTrendNormCovWithSeason",
                     AAlpha = AAlpha,
                     ADelta = ADelta,
                     ADelta0 = ADelta0,
                     AEtaCoef = AEtaCoef,
                     AEtaIntercept = AEtaIntercept,
                     ASeason = ASeason,
                     ATau = ATau,
                     along = along,
                     contrastsArg = contrastsArg,
                     data = data,
                     formula = formula,
                     hasLevel = has.level,
                     infant = infant,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     meanDelta0 = meanDelta0,
                     multAlpha = multAlpha,
                     multDelta = multDelta,
                     multDelta0 = multDelta0,
                     meanEtaCoef = meanEtaCoef,
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
                     shape1Phi = shape1Phi,
                     shape2Phi = shape2Phi,
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
                     shape1Phi = shape1Phi,
                     shape2Phi = shape2Phi,
                     tauMax = tauMax)
    }
    else if (has.trend && !has.season && !has.covariates && is.robust) {
        methods::new("SpecDLMWithTrendRobustZeroNoSeason",
                     AAlpha = AAlpha,
                     ADelta = ADelta,
                     ADelta0 = ADelta0,
                     ATau = ATau,
                     along = along,
                     hasLevel = has.level,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     meanDelta0 = meanDelta0,
                     multAlpha = multAlpha,
                     multDelta = multDelta,
                     multDelta0 = multDelta0,
                     multTau = multTau,
                     nuAlpha = nuAlpha,
                     nuBeta = nuBeta,
                     nuDelta = nuDelta,
                     nuTau = nuTau,
                     omegaAlphaMax = omegaAlphaMax,
                     omegaDeltaMax = omegaDeltaMax,
                     phi = phi,
                     phiKnown = phiKnown,
                     shape1Phi = shape1Phi,
                     shape2Phi = shape2Phi,
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
                     shape1Phi = shape1Phi,
                     shape2Phi = shape2Phi,
                     tauMax = tauMax)
    }
    else if (has.trend && has.season && !has.covariates && is.robust) {
        methods::new("SpecDLMWithTrendRobustZeroWithSeason",
                     AAlpha = AAlpha,
                     ADelta = ADelta,
                     ADelta0 = ADelta0,
                     ASeason = ASeason,
                     ATau = ATau,
                     along = along,
                     hasLevel = has.level,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     meanDelta0 = meanDelta0,
                     multAlpha = multAlpha,
                     multDelta = multDelta,
                     multDelta0 = multDelta0,
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
                     shape1Phi = shape1Phi,
                     shape2Phi = shape2Phi,
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
                     infant = infant,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     multAlpha = multAlpha,
                     meanEtaCoef = meanEtaCoef,
                     multEtaCoef = multEtaCoef,
                     multTau = multTau,
                     nuAlpha = nuAlpha,
                     nuBeta = nuBeta,
                     nuEtaCoef = nuEtaCoef,
                     nuTau = nuTau,
                     omegaAlphaMax = omegaAlphaMax,
                     phi = phi,
                     phiKnown = phiKnown,
                     shape1Phi = shape1Phi,
                     shape2Phi = shape2Phi,
                     tauMax = tauMax)
    }
    else if (has.trend && !has.season && has.covariates && is.robust) {
        methods::new("SpecDLMWithTrendRobustCovNoSeason",
                     AAlpha = AAlpha,
                     ADelta = ADelta,
                     ADelta0 = ADelta0,
                     AEtaCoef = AEtaCoef,
                     AEtaIntercept = AEtaIntercept,
                     ATau = ATau,
                     along = along,
                     contrastsArg = contrastsArg,
                     data = data,
                     formula = formula,
                     hasLevel = has.level,
                     infant = infant,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     meanDelta0 = meanDelta0,
                     multAlpha = multAlpha,
                     multDelta = multDelta,
                     multDelta0 = multDelta0,
                     meanEtaCoef = meanEtaCoef,
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
                     shape1Phi = shape1Phi,
                     shape2Phi = shape2Phi,
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
                     infant = infant,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     multAlpha = multAlpha,
                     meanEtaCoef = meanEtaCoef,
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
                     shape1Phi = shape1Phi,
                     shape2Phi = shape2Phi,
                     tauMax = tauMax)
    }
    else if (has.trend && has.season && has.covariates && is.robust) {
        methods::new("SpecDLMWithTrendRobustCovWithSeason",
                     AAlpha = AAlpha,
                     ADelta = ADelta,
                     ADelta0 = ADelta0,
                     AEtaCoef = AEtaCoef,
                     AEtaIntercept = AEtaIntercept,
                     ASeason = ASeason,
                     ATau = ATau,
                     along = along,
                     contrastsArg = contrastsArg,
                     data = data,
                     formula = formula,
                     hasLevel = has.level,
                     infant = infant,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     meanDelta0 = meanDelta0,
                     multAlpha = multAlpha,
                     multDelta = multDelta,
                     multDelta0 = multDelta0,
                     meanEtaCoef = meanEtaCoef,
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
                     shape1Phi = shape1Phi,
                     shape2Phi = shape2Phi,
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
        infant <- covariates@infant
        meanEtaCoef <- covariates@meanEtaCoef
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
                     infant = infant,
                     meanEtaCoef = meanEtaCoef,
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
                     infant = infant,
                     meanEtaCoef = meanEtaCoef,
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
#' \code{parameter[j] ~ N(mean, sd^2).}
#'
#' \code{mean} defaults to 0. In most cases, non-zero means are ignored,
#' with a warning, when the model is generated. The exception is when
#' \code{ExchFixed} is being used to specify an intercept term,
#' when generating fake data.
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
#' @param mean Mean. Optional.
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
ExchFixed <- function(mean = 0, sd = NULL, mult = 1) {
    mean <- checkAndTidyMeanOrProb(mean, name = "mean")
    mean <- new("Parameter", mean)
    tau <- checkAndTidySpecScale(x = sd, name = "sd")
    multTau <- checkAndTidyMult(mult = mult,
                                scale = tau,
                                nameScale = "sd")
    methods::new("SpecExchFixed",
                 mean = mean,
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

#' Specify the prior for the initial value of the trend term in a DLM prior.
#'
#' Specify the prior for the initial value of the trend term, typically as
#' part of a call to function \code{\link{Trend}}.
#'
#' In the linear trend version of a DLM prior, the trend term has the form
#'
#' \code{trend[0] ~ N(mean, sd^2)}
#' \code{trend[j] ~ damp * trend[j-1] + errorLevel[j], j = 1,...,J}.
#'
#' By default, the mean in the prior for \code{trend[0]} defaults to 0.
#' The default for the \code{sd} is set using the same rules as the
#' default for the scale term in a \code{\link{HalfT}} prior.
#'
#' @inheritParams HalfT
#' @param mean The mean of the prior distribution.  Defaults to 0.
#' @param sd The standard deviation of the prior distribution.
#'
#' @return An object of class \code{\linkS4class{Initial}}.
#'
#' @seealso \code{Initial} is usually called as part of a call to function
#' \code{\link{Trend}}.
#'
#' @examples
#' Initial()
#' Initial(mean = 0.1, sd = 0.3)
#' Initial(mean = 0.02, mult = 0.1)
#' @export
Initial <- function(mean = 0, sd = NULL, mult = 1) {
    mean <- checkAndTidyMeanOrProb(object = mean,
                                   name = "mean")
    mean <- methods::new("Parameter", mean)
    A <- checkAndTidySpecScale(x = sd,
                               name = "sd")
    mult <- checkAndTidyMult(mult = mult,
                             scale = A,
                             nameScale = "sd")
    methods::new("Initial",
                 A = A,
                 mean = mean,
                 mult = mult)
}



#' Specify a prior where the mean varies but is treated as known.
#'
#' Specify a 'Known' prior.  The prior comes in two forms.  With the first
#' form, the corresponding main effect or interaction equals the mean
#' exactly,
#' \code{parameter[j] = mean[j]}.
#' With the second form,
#' \code{parameter[j] ~ N(mean[j], sd[j])}.
#' The second form is obtained by supplying a \code{sd} argument
#' to function \code{Known}.
#' 
#' Internally, the \code{mean} and \code{sd} arguments are permuted and
#' subsetted to make them compatible with the corresponding main effect or
#' interaction.  Thus, for example, if \code{mean} has values for regions
#' \code{A}, \code{B}, and \code{C}, but data \code{y} only includes
#' regions \code{A} and \code{B}, then \code{C} is silently ignored.
#'
#' The original \code{mean} and \code{sd} arguments are, however,
#' stored, and can be used for prediction.  For example, we might
#' supply a \code{mean} argument with values for years 2005,
#' 2010, 2015, 2020, and 2025, then call \code{\link{estimateModel}}
#' with data for 2005, 2010, and 2015, and finally call
#' \code{\link{predictModel}} to obtain forecasts for 2020 and 2025.
#' 
#' @param mean The mean of the prior distribution.  An object of
#' class \code{\link[dembase:Values-class]{Values}}.
#' @param sd The standard deviation of the prior distribution.
#' If omitted, it is assumed to be 0.  \code{sd} can be an object
#' of class \code{\link[dembase:Values-class]{Values}}, or it can be
#' a single number, in which case it is applied to all
#' elements of \code{mean}.
#'
#' @return An object of class \code{\linkS4class{SpecKnown}}.
#'
#' @seealso \code{ExchFixed} creates a normal prior centered at 0.
#'
#' @examples
#' mean <- ValuesOne(c(0.1, 0.2, 0.3),
#'                   labels = c("A", "B", "C"),
#'                   name = "region")
#' sd <- ValuesOne(c(0.02, 0.03, 0.02),
#'                   labels = c("A", "B", "C"),
#'                   name = "region")
#' ## No uncertainty
#' Known(mean)
#'
#' ## Some uncertainty
#' Known(mean = mean, sd = sd)
#'
#' ## Single standard deviation
#' Known(mean = mean, sd = 0.05)
#' @export
Known <- function(mean, sd = 0) {
    ## 'mean' is "Values"
    if (!methods::is(mean, "Values"))
        stop(gettextf("'%s' has class \"%s\"",
                      "mean", class(mean)))
    metadata <- mean@metadata
    ## 'metadata' does not have any dimensions with dimtype "iteration"
    if ("iteration" %in% dimtypes(metadata))
        stop(gettextf("'%s' has dimension with dimtype \"%s\"",
                      "mean", "iteration"))
    ## 'metadata' does not have any dimensions with dimtype "quantile"
    if ("quantile" %in% dimtypes(metadata))
        stop(gettextf("'%s' has dimension with dimtype \"%s\"",
                      "mean", "quantile"))
    ## 'mean' has no missing values
    if (any(is.na(mean)))
        stop(gettextf("'%s' has missing values",
                      "mean"))
    ## mean has length of 2 or more
    if (length(mean) < 2L)
        stop(gettextf("'%s' has length %d",
                      "mean", length(mean)))
    alphaKnown <- as.double(mean)
    alphaKnown <- methods::new("ParameterVector", alphaKnown)
    if (identical(sd, 0)) {
        methods::new("SpecKnownCertain",
            alphaKnown = alphaKnown,
            metadata = metadata)
    }
    else {
        if (methods::is(sd, "Values")) {
            ## 'sd' is compatible with 'mean'
            sd <- tryCatch(dembase::makeCompatible(x = sd, y = mean, subset = TRUE),
                           error = function(e) e)
            if (methods::is(sd, "error"))
                stop(gettextf("'%s' and '%s' not compatible : %s",
                              "sd", "mean", sd$message))
            sd <- as.double(sd)
            ## 'sd' has no missing values
            if (any(is.na(sd)))
                stop(gettextf("'%s' has missing values",
                              "sd"))
            ## 'sd' has no negative values
            if (any(sd < 0))
                stop(gettextf("'%s' has negative values",
                              "sd"))
        }
        else if (methods::is(sd, "numeric")) {
            sd <- as.double(sd)
            ## 'sd' has length 1
            if (!identical(length(sd), 1L))
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
            sd <- rep(sd, times = length(mean))
        }
        else { ## sd is Values or numeric
            stop(gettextf("'%s' has class \"%s\"",
                          "sd", class(sd)))
        }
        sd <- methods::new("ScaleVec", sd)
        methods::new("SpecKnownUncertain",
            alphaKnown = alphaKnown,
            AKnownVec = sd,
            metadata = metadata)
    }
}


#' Specify the level term in a DLM prior.
#'
#' Specify the level term in a \code{\link{DLM}} prior.  Currently the only
#' part of the level term that can be specified is the prior for the
#' standard deviation of the innovations.
#'
#' @param scale An object of class \code{\linkS4class{HalfT}}.
#'
#' @return An object of class \code{\linkS4class{Level}}
#'
#' @seealso \code{Level} is usually called as part of a call to
#' function \code{\link{DLM}}.
#'
#' @examples
#' Level()
#' Level(scale = HalfT(scale = 0.5))
#' @export
Level <- function(scale = HalfT()) {
    if (!methods::is(scale, "HalfT"))
        stop(gettextf("'%s' has class \"%s\"",
                      scale, class(scale)))
    AAlpha <- scale@A
    multAlpha <- scale@mult
    nuAlpha <- scale@nu
    omegaAlphaMax <- scale@scaleMax
    methods::new("Level",
                 AAlpha = AAlpha,
                 multAlpha = multAlpha,
                 nuAlpha = nuAlpha,
                 omegaAlphaMax = omegaAlphaMax)
}



## NO_TESTS
#' Specify a Mix prior.
#'
#' A \code{Mix} prior is used to model an iteraction that includes
#' an "along" dimension (typically, but not necessarily, time),
#' and additional dimensions.  Values for the additional
#' dimensions evolve, generally smoothly, across successive values of the
#' "along" dimension.  A typical application is modelling age-structures
#' that change gradually over time.
#'
#' The prior is modified from a model developed by Kunihama and Dunson
#' (2013).  The model is non-parametric, meaning that it extracts
#' structure from the data with minimal guidance from the user.  It is
#' designed for use with sparse data.
#'
#' @inheritParams DLM
#' @param components An object of class \code{\linkS4class{Components}}.
#' @param weights An object of class \code{\linkS4class{Weights}}.
#' @param maxComponents The maximum number of components. Defaults to 10.
#'
#' @references
#' Kunihama, T and Dunson, DB. 2013. Bayesian modeling of temporal dependence in
#' large sparse contingency tables.  \emph{Journal of the American Statistical
#' Association}, 108(504): 1324-1338.
#'
#' @seealso Priors for the components can be specified using
#' function \code{\link{Components}}, and weights (ie mixing parameters)
#' can be specified using function \code{\link{Weights}}.  However,
#' in normal usage, it is typically best to stick with the defaults.
#' 
#' @examples
#' Mix()
#' @export
Mix <- function(along = NULL,
                components = Components(),
                weights = Weights(),
                covariates = NULL,
                error = Error(),
                maxComponents = 10) {
    ## along
    along <- checkAndTidyAlongDLM(along)
    ## components
    if (!methods::is(components, "Components"))
        stop(gettextf("'%s' has class \"%s\"",
                      "components", class(components)))
    AVectorsMix <- components@AVectorsMix
    multVectorsMix <- components@multVectorsMix
    nuVectorsMix <- components@nuVectorsMix
    omegaVectorsMaxMix <- components@omegaVectorsMaxMix
    ## weights
    if (!methods::is(weights, "Weights"))
        stop(gettextf("'%s' has class \"%s\"",
                      "weights", class(weights)))
    AComponentWeightMix <- weights@AComponentWeightMix
    ALevelComponentWeightMix <- weights@ALevelComponentWeightMix
    minLevelComponentWeight <- weights@minLevelComponentWeight
    maxLevelComponentWeight <- weights@maxLevelComponentWeight
    maxPhi <- weights@maxPhi
    minPhi <- weights@minPhi
    multComponentWeightMix <- weights@multComponentWeightMix
    multLevelComponentWeightMix <- weights@multLevelComponentWeightMix
    nuComponentWeightMix <- weights@nuComponentWeightMix
    nuLevelComponentWeightMix <- weights@nuLevelComponentWeightMix
    omegaComponentWeightMaxMix <- weights@omegaComponentWeightMaxMix
    omegaLevelComponentWeightMaxMix <- weights@omegaLevelComponentWeightMaxMix
    phi <- weights@phi
    phiKnown <- weights@phiKnown
    priorMeanLevelComponentWeightMix <- weights@priorMeanLevelComponentWeightMix
    priorSDLevelComponentWeightMix <- weights@priorSDLevelComponentWeightMix
    shape1Phi <- weights@shape1Phi
    shape2Phi <- weights@shape2Phi
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
        infant <- covariates@infant
        meanEtaCoef <- covariates@meanEtaCoef
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
    ## indexClassMax
    indexClassMaxMix <- checkAndTidyIndexClassMaxMix(maxComponents)
    ## return
    if (!is.robust && !has.covariates) {
        methods::new("SpecMixNormZero",
                     AComponentWeightMix = AComponentWeightMix,
                     ALevelComponentWeightMix = ALevelComponentWeightMix,
                     ATau = ATau,
                     AVectorsMix = AVectorsMix,
                     along = along,
                     indexClassMaxMix = indexClassMaxMix,
                     minLevelComponentWeight = minLevelComponentWeight,
                     maxLevelComponentWeight = maxLevelComponentWeight,
                     minPhi = minPhi,
                     maxPhi = maxPhi,
                     multComponentWeightMix = multComponentWeightMix,
                     multLevelComponentWeightMix = multLevelComponentWeightMix,
                     multTau = multTau,
                     multVectorsMix = multVectorsMix,
                     nuComponentWeightMix = nuComponentWeightMix,
                     nuLevelComponentWeightMix = nuLevelComponentWeightMix,
                     nuTau = nuTau,
                     nuVectorsMix = nuVectorsMix,
                     omegaComponentWeightMaxMix = omegaComponentWeightMaxMix,
                     omegaLevelComponentWeightMaxMix = omegaLevelComponentWeightMaxMix,
                     omegaVectorsMaxMix = omegaVectorsMaxMix,
                     phi = phi,
                     phiKnown = phiKnown,
                     priorMeanLevelComponentWeightMix = priorMeanLevelComponentWeightMix,
                     priorSDLevelComponentWeightMix = priorSDLevelComponentWeightMix,
                     shape1Phi = shape1Phi,
                     shape2Phi = shape2Phi,
                     tauMax = tauMax)
    }
    else
        stop("Mix prior with covariates and/or robust errors not implemented yet")
}
    

## NO_TESTS
#' Specify a normal distribution.
#'
#' Specify a normal distribution with mean \code{mean}
#' and standard deviation \code{sd}.
#'
#' The default for \code{mean} is always 0, but the default
#' for \code{sd} depends on context.
#'
#' \code{Norm} is used to specify the prior for the intercept in
#' function \code{\link{Covariates}}.  If a value for \code{sd}
#' is not specified, a default value is determined when function
#' \code{\link{estimateModel}}, \code{\link{estimateCounts}}, or
#' \code{\link{estimateAccount}} is called.  Let \eqn{s} be the standard
#' deviation of data \eqn{y}, or of \code{log(y)} in the case of a Poisson
#' model without exposure.  Then the default for \code{sd} is
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
#' {Norm} is also used to specify a prior (on the log scale) for
#' the dispersion parameter in a \code{\link{CMP}} model. In this
#' case \code{sd} defaults to 1.
#'
#' @param mean Mean.
#' @param sd Standard deviation. A positive number.
#'
#' @return An object of class \code{\linkS4class{Norm}}.
#'
#' @seealso \code{Norm} is used in calls to function \code{\link{Covariates}}.
#'
#' @examples
#' Norm()
#' Norm(sd = 0.5)
#' Norm(mean = 0.5, sd = 1.5)
#' @export
Norm <- function(mean = 0, sd = NULL) {
    mean <- checkAndTidyMeanOrProb(mean, name = "mean")
    mean <- new("Parameter", mean)
    A <- checkAndTidySpecScale(x = sd, name = "scale")
    methods::new("Norm",
                 mean = mean,
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


## NO_TESTS
#' Specify a vector of independent t-distributed variables
#'
#' Specify a vector of \code{n} t-distributed variables, each
#' of which has degrees of freedom \code{df[i]},
#' mean \code{mean[i]}, and scale \code{scale[i]}.
#'
#' @param df Degrees of freedom. A vector with length equal to
#' 1 or to the number of variables required. Defaults to 4.
#' @param mean Mean parameter. A vector with length equal to 1
#' or to the number of variables required. Defaults to 0.
#' @param scale Scale parameter.  A vector with length equal to
#' 1 or to the number of variables required. Defaults to 1.
#'
#' @return Object of class \code{\linkS4class{SpecTDist}}.
#'
#' @seealso \code{\link{Covariates}}
#'
#' @examples
#' TDist()
#' TDist(mean = c(-1, 0, 0))
#' TDistn(df = c(4, 4, 7),
#'        mean = c(-1, 0.2, 0.1),
#'        scale = c(1, 2, 1))
#' @export
TDist <- function(df = 7, mean = 0, scale = NULL, mult = 1) {
    nu <- checkAndTidyNuVec(x = df,
                            name = "df")
    n.nu <- length(nu@.Data)
    if (n.nu == 0L)
        stop(gettextf("'%s' has length %d",
                      "df", 0L))
    mean <- checkAndTidyParameterVector(x = mean,
                                        name = "mean")
    n.mean <- length(mean@.Data)
    if (n.mean == 0L)
        stop(gettextf("'%s' has length %d",
                      "mean", 0L))
    A <- checkAndTidySpecScaleVec(x = scale,
                                  name = "scale")
    n.A <- length(A@.Data)
    if (n.A == 0L)
        stop(gettextf("'%s' has length %d",
                      "mean", 0L))
    mult <- checkAndTidyMultVec(mult = mult,
                                scale = A,
                                nameMult = "mult",
                                nameScale = "scale")
    n.mult <- length(mult@.Data)
    max.len <- max(n.nu, n.mean, n.A, n.mult)
    if (max.len > 1L) {
        nu.invalid <- (n.nu != 1L) && (n.nu != max.len)
        mean.invalid <- (n.mean != 1L) && (n.mean != max.len)
        A.invalid <- (n.A != 1L) && (n.A != max.len)
        mult.invalid <- (n.mult != 1L) && (n.mult != max.len)
        if (nu.invalid || mean.invalid || A.invalid || mult.invalid)
            stop(gettextf("'%s', '%s', '%s', and '%s' have inconsistent lengths",
                          "df", "mean", "scale", "mult"))
        nu@.Data <- rep_len(nu@.Data, length.out = max.len)
        mean@.Data <- rep_len(mean@.Data, length.out = max.len)
        A@.Data <- rep_len(A@.Data, length.out = max.len)
        mult@.Data <- rep_len(mult@.Data, length.out = max.len)
    }
    methods::new("TDist",
                 AEtaCoef = A,
                 meanEtaCoef = mean,
                 multEtaCoef = mult,
                 nuEtaCoef = nu)
}




#' Specify the trend term in a DLM prior.
#'
#' Specify the prior for the initial values, and the prior for the standard
#' deviation of the innovations in a \code{\link{DLM}} prior.
#'
#' @param initial An object of class \code{\linkS4class{Initial}}.
#' @param scale An object of class \code{\linkS4class{HalfT}}.
#'
#' @return An object of class \code{\linkS4class{Trend}}
#'
#' @seealso \code{Trend} is usually called as part of a call to
#' function \code{\link{DLM}}.
#'
#' @examples
#' Trend()
#' Trend(initial = Initial(mean = 0.05, sd = 0.1),
#'       scale = HalfT(scale = 0.5))
#' @export
Trend <- function(initial = Initial(), scale = HalfT()) {
    if (!methods::is(initial, "Initial"))
        stop(gettextf("'%s' has class \"%s\"",
                      initial, class(initial)))
    ADelta0 <- initial@A
    meanDelta0 <- initial@mean
    multDelta0 <- initial@mult
    if (!methods::is(scale, "HalfT"))
        stop(gettextf("'%s' has class \"%s\"",
                      scale, class(scale)))
    ADelta <- scale@A
    multDelta <- scale@mult
    nuDelta <- scale@nu
    omegaDeltaMax <- scale@scaleMax
    methods::new("Trend",
                 ADelta = ADelta,
                 ADelta0 = ADelta0,
                 meanDelta0 = meanDelta0,
                 multDelta = multDelta,
                 multDelta0 = multDelta0,
                 nuDelta = nuDelta,
                 omegaDeltaMax = omegaDeltaMax)
}


## NO_TESTS
#' Specify priors for the weights in a Mix prior.
#'
#' In normal usage, it is better to accept the default priors
#' for the weights than to specify them explicitly.
#' End users are therefore unlikely to need this function.
#'
#' A \code{\link{Mix}} prior treats an interaction as a mixture of
#' normal distributions.  The weights, also referred as mixture
#' parameters, evolve over time.  The evolution of the weight for
#' each component is governed by an auto-regressive time series model.
#' Specifically, transformed values of the weights are modelled
#' using
#'
#'   \code{level1[k,h] = level2[k,h] + error1[k,h]}
#' 
#'   \code{level2[k,h] = mean + damp * level2[k-1,h] + error2[k,h]}
#'
#' \code{mean} has a normal prior.
#'
#' \code{damp} has the boundary-avoiding prior described in
#' Gelman et al (2004) p316-317.  This prior is equivalent to assuming
#' a Beta(2, 2) prior on the transformed parameter (\code{damp}+0.5)/2.
#'
#' \code{error1} and \code{error2} both have half-t priors. These
#' priors have the defaults described in \code{\link{HalfT}}.
#'
#' @inheritParams DLM
#' @param mean The mean of the prior for \code{mean}. Defaults to 0.
#' @param sd The standard deviation of the prior for \code{mean}.
#' Defaults to 1.
#' @param damp An object of class \code{\linkS4class{Damp}}. 
#' @param scale1 An object of class \code{\linkS4class{HalfT}} defining
#' the prior for \code{error1}.
#' @param scale2 An object of class \code{\linkS4class{HalfT}} defining
#' the prior for \code{error2}.
#'
#' @return An object of class \code{\linkS4class{Weights}}.
#' 
#' @seealso Mixture priors are specified using \code{\link{Mix}}.
#' The weights in a mixture prior are specified using
#' \code{\link{Weights}}.
#' 
#' @export
Weights <- function(mean = 0, sd = 1, damp = Damp(), scale1 = HalfT(), scale2 = HalfT()) {
    minAR2 <- -4; maxAR2 <- 4 # no longer letting users specify
    priorMeanLevelComponentWeightMix <- checkAndTidyMeanOrProb(object = mean,
                                                               name = "mean")
    checkPositiveNumeric(x = sd,
                         name = "sd")
    priorMeanLevelComponentWeightMix = methods::new("Parameter",
                                                    priorMeanLevelComponentWeightMix)
    priorSDLevelComponentWeightMix <- as.double(sd)
    priorSDLevelComponentWeightMix <- methods::new("Scale",
                                                   priorSDLevelComponentWeightMix)
    if (!methods::is(scale1, "HalfT"))
        stop(gettextf("'%s' has class \"%s\"",
                      "scale1", class(scale1)))
    if (!methods::is(scale2, "HalfT"))
        stop(gettextf("'%s' has class \"%s\"",
                      "scale2", class(scale2)))
    if (!methods::is(damp, "Damp")) {
        stop(gettextf("'%s' has class \"%s\"",
                      "damp", class(damp)))
    }
    phiKnown <- methods::is(damp, "DampKnown")
    if (phiKnown) {
        minPhi <- 0
        maxPhi <- 1
        shape1Phi <- methods::new("Scale", 1)
        shape2Phi <- methods::new("Scale", 1)
        phi <- damp@phi
        if (isTRUE(all.equal(phi, 1)))
            stop(gettextf("'%s' equals %d",
                          "damp", 1L))
    }
    else {
        minPhi <- damp@minPhi
        maxPhi <- damp@maxPhi
        shape1Phi <- damp@shape1Phi
        shape2Phi <- damp@shape2Phi
        phi <- as.double(NA)
    }
    phiKnown <- methods::new("LogicalFlag", phiKnown)
    l <- checkAndTidyLevelComponentWeightMinMax(minAR2 = minAR2,
                                                maxAR2 = maxAR2)
    minLevelComponentWeight <- l$minLevelComponentWeight
    maxLevelComponentWeight <- l$maxLevelComponentWeight
    maxAR2 <- l$maxAR2
    AComponentWeightMix <- scale1@A
    ALevelComponentWeightMix <- scale2@A
    multComponentWeightMix <- scale1@mult
    multLevelComponentWeightMix <- scale2@mult
    nuComponentWeightMix <- scale1@nu
    nuLevelComponentWeightMix <- scale2@nu
    omegaComponentWeightMaxMix <- scale1@scaleMax
    omegaLevelComponentWeightMaxMix <- scale2@scaleMax
    methods::new("Weights",
                 AComponentWeightMix = AComponentWeightMix,
                 ALevelComponentWeightMix = ALevelComponentWeightMix,
                 maxLevelComponentWeight = maxLevelComponentWeight,
                 maxPhi = maxPhi,
                 minLevelComponentWeight = minLevelComponentWeight,
                 minPhi = minPhi,
                 multComponentWeightMix = multComponentWeightMix,
                 multLevelComponentWeightMix = multLevelComponentWeightMix,
                 nuComponentWeightMix = nuComponentWeightMix,
                 nuLevelComponentWeightMix = nuLevelComponentWeightMix,
                 omegaComponentWeightMaxMix = omegaComponentWeightMaxMix,
                 omegaLevelComponentWeightMaxMix = omegaLevelComponentWeightMaxMix,
                 phi = phi,
                 phiKnown = phiKnown,
                 priorMeanLevelComponentWeightMix = priorMeanLevelComponentWeightMix,
                 priorSDLevelComponentWeightMix = priorSDLevelComponentWeightMix,
                 shape1Phi = shape1Phi,
                 shape2Phi = shape2Phi)
}









#' Specify a prior that sets all terms to zero.
#'
#' Specify a prior for a main effect or interaction in which 
#' \code{parameter[j] = 0}.
#'
#' In some cases, as with a dimension representing upper and lower
#' Lexis triangles, setting all elements of the main effect or interaction
#' to zero may make substantive sense.  In other cases, a \code{Zero} prior
#' to reduce the number of terms being estimated, while conforming
#' to that requirement from \code{\link{Model}} that all terms marginal
#' to an interaction be included in a model.
#' 
#' @param mean The mean of the prior distribution.  An object of
#'
#' @return An object of class \code{\linkS4class{Zero}}.
#'
#' @seealso \code{Known} can be used to create a prior that
#' fixes the main effect or interaction at values other than 0.
#'
#' @examples
#' Zero()
#' @export  
Zero <- function() {
    methods::new("SpecZero")
}
