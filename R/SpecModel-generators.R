

#' Specify the prior for the dispersion parameter in a CMP model.
#'
#' Specify the prior for the dispersion parameter (often denoted
#' 'nu') in a CMP or COM-Poisson model. The dispersion parameter
#' is assumed to have a distribution
#' \deqn{log \nu_i \sim N(\mu_i, \sigma^2)}
#' \deqn{\mu_i \sim N(M, S^2)}
#' \deqn{\sigma \sim trunc-half-t(df, A, max)}
#'
#' The \code{mean} argument is used to specify the second equation,
#' and the \code{scale} argument is used to specify the third.
#' \code{M} in the second equation defaults to 0, and \code{S}
#' defaults to 1. \code{df} in the third equation defaults to 7.
#' \code{A} defaults to 1 if the CMP model uses exposure,
#' and a function of the variation in log(y) otherwise.
#' 
#' @param mean An object of class \code{\linkS4class{Norm}}, specifying
#' the prior for the mean of the (logged) dispersion parameter.
#' @param scale An object of class \code{\linkS4class{HalfT}},
#' specifying the prior for the standard deviation of the (logged)
#' dispersion parameter.
#'
#' @return An object of class \code{\linkS4class{Dispersion}}.
#'
#' @seealso CMP models are specified with function \code{\link{CMP}}.
#'
#' @examples
#' Dispersion()
#' Dispersion(mean = Norm(mean = -1, sd = 0.1))
#' Dispersion(mean = Norm(mean = -1, sd = 0.1),
#'            scale = HalfT(scale = 0.1))
#' @export
Dispersion <- function(mean = Norm(), scale = HalfT()) {
    ## mean
    if (!methods::is(mean, "Norm"))
        stop(gettextf("'%s' has class \"%s\"",
                      mean, class(mean)))
    meanMeanLogNuCMP <- mean@mean
    sdMeanLogNuCMP <- mean@A@.Data
    if (is.na(sdMeanLogNuCMP))
        sdMeanLogNuCMP <- new("Scale", 1)
    else
        sdMeanLogNuCMP <- new("Scale", sdMeanLogNuCMP)
    ## scale
    if (!methods::is(scale, "HalfT"))
        stop(gettextf("'%s' has class \"%s\"",
                      scale, class(scale)))
    ASDLogNuCMP <- scale@A
    multSDLogNuCMP <- scale@mult
    nuSDLogNuCMP <- scale@nu
    sdMaxLogNuCMP <- scale@scaleMax@.Data
    sdMaxLogNuCMP <- makeScaleMax(scaleMax = sdMaxLogNuCMP,
                                  A = ASDLogNuCMP,
                                  nu = nuSDLogNuCMP,
                                  isSpec = TRUE)
    methods::new("Dispersion",
                 meanMeanLogNuCMP = meanMeanLogNuCMP,
                 sdMeanLogNuCMP = sdMeanLogNuCMP,
                 ASDLogNuCMP = ASDLogNuCMP,
                 multSDLogNuCMP = multSDLogNuCMP,
                 nuSDLogNuCMP = nuSDLogNuCMP,
                 sdMaxLogNuCMP = sdMaxLogNuCMP)
}



#' Specify first two levels of hierarchical model.
#'
#' Specify the likelihood and part of the prior for a
#' Poisson, binomial, or normal hierarchical model.
#'
#' Specify a likelihood and prior of the form
#' \deqn{y_i \sim Poisson(\gamma_i n_i)}
#' \deqn{log(\gamma_i) \sim N(x_i \beta, \sigma_i),}
#'
#' \deqn{y_i \sim Poisson(\gamma_i)}
#' \deqn{log(\gamma_i) \sim N(x_i \beta, \sigma_i),}
#'
#' \deqn{y_i \sim binomial(n_i, \gamma_i)}
#' \deqn{logit(\gamma_i) \sim N(x_i \beta, \sigma_i),}
#' or
#' \deqn{y_i \sim normal(\gamma_i, \phi^2 / w_i)}
#' \deqn{\gamma_i \sim N(x_i \beta, \sigma_i)}.
#' 
#' Subscript \eqn{i} denotes a cell within a multiway array,
#' such as an array with dimensions age, sex, and time.  In
#' Poisson and binomial models, \eqn{y_i} is a count.
#' The \eqn{n_i} term is an exposure in the case of Poisson
#' models and a sample size in the case of binomial models.
#' It is not supplied in calls to function \code{Poisson}
#' or \code{Binomial}.  In normal models \eqn{y_i}
#' is a cell-specific value such as a mean,
#' and \eqn{w_i} is a cell-specific weight.  Weights
#' are not supplied in calls to function \code{Normal}.
#' Vector \eqn{\beta} contains main effects
#' and interactions, such as age effects, time effects,
#' and age-region interactions. Vector \eqn{x_i} is the
#' \eqn{i}th row from the design matrix \eqn{X}.
#'
#' The main effects and interactions are specified
#' via the \code{formula} argument. For instance,
#' 
#'   \code{mean ~ age * sex + time}
#' 
#' specifies a model with age, sex, and time main effects,
#' and an age-sex interaction.
#'
#' The main effects and interactions in a hierarchical model are only weakly
#' identified: see the documentation for function \code{\link{fetch}} for
#' details.
#'
#' If a model has two or more levels, the second level
#' typically contains more than just main effects
#' and interactions.  For instance, the second level of
#' Poisson, binomial, and normal hierarchical models
#' contains a variance term.  The remaining parts of
#' the second level, such as the variances, as well
#' as any higher levels, are specified
#' in calls to function \code{\link{Model}}, or to
#' functions \code{\link{estimateModel}},
#' \code{\link{estimateCounts}}, or \code{\link{estimateAccount}}.
#'
#' @param formula A \code{\link[stats]{formula}} with response
#' \code{mean} and names of dimensions of demographic
#' series or dataset being modelled.
#' @param sd Standard deviation in the likelihood for the
#' normal model.  If a value is not supplied,
#' it is estimated from the data.
#' @param priorSD An object of class \code{HalfT} specifying
#' a non-default prior for the standard deviation in the
#' likelihood for the normal model.
#' @param useExpose Whether the model includes an
#' exposure term. Defaults to \code{TRUE}.
#'
#' @return An object of class \code{\linkS4class{SpecLikelihood}}.
#'
#' @seealso
#' Functions \code{Poisson}, \code{Binomial}, and \code{Normal} are
#' used as part of a call to function \code{\link{Model}}.
#'
#' @examples
#' ## age effect, sex effect, age-sex interaction,
#' ## and time effect
#' Poisson(mean ~ age * sex + time)
#'
#' ## same model, but without exposure term
#' Poisson(mean ~ age * sex + time, useExpose = FALSE)
#'
#' ## use formula notation to specify second-order interactions
#' Binomial(mean ~ (age + sex + region)^2)
#'
#' Normal(mean ~ age + education + income)
#' ## specify the exact value of the standard deviation
#' Normal(mean ~ age + education + income,
#'        sd = 0.3)
#' ## specify a non-default prior for the standard deviation
#' Normal(mean ~ age + education + income,
#'        priorSD = HalfT(scale = 100))
#'
#' @name likelihood
#' @aliases Binomial Normal Poisson
NULL

## HAS_TESTS
#' @rdname likelihood
#' @export
Poisson <- function(formula, useExpose = TRUE, boxcox = 0) {
    checkFormulaMu(formula)
    checkForMarginalTerms(formula)
    useExpose <- checkAndTidyLogicalFlag(x = useExpose,
                                         name = "useExpose")
    methods::new("SpecLikelihoodPoisson",
                 formulaMu = formula,
                 useExpose = useExpose,
                 boxCoxParam = boxcox)
}

## HAS_TESTS
#' @rdname likelihood
#' @export
Binomial <- function(formula) {
    checkFormulaMu(formula)
    checkForMarginalTerms(formula)
    methods::new("SpecLikelihoodBinomial",
                 formulaMu = formula)
}

## HAS_TESTS
#' @rdname likelihood
#' @export
Normal <- function(formula, sd = NULL, priorSD = HalfT()) {
    checkFormulaMu(formula)
    checkForMarginalTerms(formula)
    if (is.null(sd)) {
        if (!methods::is(priorSD, "HalfT"))
            stop(gettextf("'%s' has class \"%s\"",
                          "priorSD", class(priorSD)))
        AVarsigma <- priorSD@A
        nuVarsigma <- priorSD@nu
        varsigmaMax <- priorSD@scaleMax
        methods::new("SpecLikelihoodNormalVarsigmaUnknown",
                     formulaMu = formula,
                     AVarsigma = AVarsigma,
                     nuVarsigma = nuVarsigma,
                     varsigmaMax = varsigmaMax)
    }
    else {
        checkPositiveNumeric(x = sd, name = "sd")
        varsigma <- methods::new("Scale", as.double(sd))
        methods::new("SpecLikelihoodNormalVarsigmaKnown",
                     formulaMu = formula,
                     varsigma = varsigma)
    }
}

## HAS_TESTS
#' Specify a model based on a normal distribution with known
#' means and standard deviations.
#'
#' Specify a model of the form
#'   \deqn{y_i = N(mean[i], sd[i])}
#' or
#'   \deqn{y_i = N(exposure[i] * mean[i], sd[i])}.
#'
#' Among other things, the model is useful as a data model for
#' surveys.  In such cases, \code{exposure} represents the
#' true underlying counts, \code{mean} represents coverage rates,
#' \code{sd} represents standard errors for the coverage rates,
#' and \code{y} represents the observered counts.  If the model
#' is being used to represent a census post-enumeration survey,
#' for instance, then \code{exposure} would be the true population,
#' \code{mean} the coverage rates as estimated from the survey,
#' \code{se} the survey-based standard errors, and \code{y} the
#' census counts.
#'
#' Subscript \eqn{i} denotes a cell within a classification
#' defined by variables such as age, sex, and time.
#' For instance cell \eqn{i} might be 30-34 year old females
#' in 2020.
#'
#' @inheritParams likelihood
#' @param mean An object of class \code{\link[dembase:Values-class]{Values}}
#' holding means.
#' @param sd The standard deviation of the means. \code{sd} can be an object
#' of class \code{\link[dembase:Values-class]{Values}}, or it can be
#' a single number, in which case it is applied to all
#' elements of \code{mean}.
#'
#' @return An object of class \code{\linkS4class{SpecLikelihoodNormalFixed}}.
#' 
#' @seealso \code{NormalFixed} is typically used as
#' part of a call to function \code{\link{Model}}.
#' More flexible normal models can be specified using
#' \code{\link{Normal}}.
#'
#' @examples
#' mean <- Values(array(c(0.95, 0.96, 0.95, 0.94),
#'                      dim = c(2, 2),
#'                      dimnames = list(age = c("0-39", "40+"),
#'                                      sex = c("Female", "Male"))))
#' sd <- Values(array(c(0.05, 0.08, 0.05, 0.04),
#'                    dim = c(2, 2),
#'                    dimnames = list(age = c("0-39", "40+"),
#'                                    sex = c("Female", "Male"))))
#' NormalFixed(mean = mean, sd = sd)
#' NormalFixed(mean = mean, sd = 0.1)
#' NormalFixed(mean = mean, sd = 0.1, useExpose = FALSE)
#' @export
NormalFixed <- function(mean, sd, useExpose = TRUE) {
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
    mean.param <- as.double(mean)
    mean.param <- new("ParameterVector", mean.param)
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
    sd <- new("ScaleVec", sd)
    useExpose <- checkAndTidyLogicalFlag(x = useExpose,
                                         name = "useExpose")
    new("SpecLikelihoodNormalFixed",
        mean = mean.param,
        sd = sd,
        metadata = metadata,
        useExpose = useExpose)
}


## HAS_TESTS
#' Specify a model for a single demographic series or
#' dataset.
#'
#' The likelihood and, if the model has a second level,
#' main effects and interactions from that level, are
#' specified via functions such as \code{\link{Poisson}},
#' \code{\link{Binomial}}, \code{\link{Normal}}, or
#' \code{\link{PoissonBinomial}}.  \code{Model} is used
#' to specify the remaining parts of the model.
#'
#' The \code{\dots} argument is used to specify non-default
#' hyper-priors.  Each main effect or interaction
#' specified via functions such as \code{\link{Poisson}}
#' has a default prior, which is determined from information such
#' as the {\link[dembase]{dimtype}} of the associated dimensions
#' of the series or dataset.  Since the characteristics of
#' the series or dataset are not known until functions
#' \code{\link{estimateModel}}, \code{\link{estimateCounts}},
#' or \code{\link{estimateAccount}} have been called, the
#' the process of choosing the default is not completed until
#' the \code{estimate} functions are called.
#'
#' The \code{\dots} argument can be used to specify
#' non-default priors.  This is done by through expressions
#' such as
#'
#' \code{region ~ Exch(error = Error(robust = TRUE))}
#'
#' or
#'
#' \code{region:time ~ DLM(trend = HalfT(scale = 0.2))}
#'
#' where \code{region} and \code{time} are names of dimensions
#' in the dataset, and \code{\link{Exch}} and \code{\link{DLM}}
#' are functions for construction hyper-priors.
#'
#' The \code{priorSD} argument refers to the standard
#' deviation at the second level of the model (if there
#' is a second level.)  It should not be confused with the
#' \code{priorsSD} argument to function \code{\link{Normal}},
#' which refers to the standard deviation in the likelihood.
#'
#' The \code{lower} and \code{upper} arguments can be used
#' to constrain the range of the rate or count parameters
#' in the likelihood for Poisson models, the probability
#' parameters in the likelihood for binomial models,
#' or the mean parameters in the likelihood for normal models.
#' These constraints may reflect substantive features
#' of the application: for instance, in a normal model
#' it may make sense to constrain the means to be non-negative.
#' However, setting \code{lower} to a value slightly above 0
#' may also help resolve numerical problems in binomial
#' models were many estimated probabilities are close to 0,
#' and setting \code{upper} to a value slightly below 1 may
#' resolve problems when estimated probabilities are close to
#' 1.
#'
#' Printing the object created by \code{Model}, typically by typing
#' the name of the object at the console, shows the specification.
#' The \code{trunc-half-t(df, s^2, max)} in the printed results refers to a
#' truncated \code{\link[=halft-distn]{half-t}} distribution with \code{df}
#' degrees of freedom, scale \code{s^2}, and maximum value \code{max}.
#' 
#' @param formula A \code{\link[stats]{formula}} describing
#'     the likelihood, and possibly parts of the  prior.
#'     Constructed using functions such as \code{\link{Poisson}}.
#' @param \dots  Non-default hyper-priors, in models that
#'     include hyper-priors.  Constructed using functions
#'     such as \code{\link{Exch}} and \code{\link{DLM}}.
#' @param priorSD An object of class \code{HalfT} specifying
#'      the prior for the prior-level standard deviation.
#' @param lower A lower bound for estimates of data-level
#'    means or probabilities (the \eqn{gamma_i}).
#' @param upper An upper bound for estimates of data-level
#'    means or probabilities (the \eqn{gamma_i}).
#' @param jump The standard deviation of the proposal density
#'    for Metropolis-Hasting updates.
#' @param series The name of the demographic series (eg
#'     \code{"population"} or \code{"births"}) that
#'     is being modelled. Only needed when the model is to
#'     be used in a call to \code{\link{estimateAccount}}.
#' @param aggregate An object of class \code{\linkS4class{SpecAggregate}}.
#'
#' @examples
#' ## model where all hyper-priors follow defaults
#' Model(y ~ Poisson(mean ~ age * sex + age * time))
#'
#' ## override default hyper-prior for age effect
#' Model(y ~ Poisson(mean ~ age * sex + age * time),
#'       age ~ DLM(trend = NULL))
#'
#' ## impose lower and upper bounds.
#' Model(y ~ Binomial(mean ~ age * region + time),
#'       lower = 0.001, upper = 0.999)
#'
#' ## increase size of Metropolis-Hastings steps
#' Model(y ~ Poisson(mean ~ age * sex + age * time),
#'       jump = 0.2)
#'
#' ## data model
#' Model(reg.births ~ PoissonBinomial(prob = 0.98),
#'       series = "births")
#'
#' overall.av <- AgNormal(value = 0.3, sd = 0.01)
#' Model(y ~ Binomial(mean ~ region + sex),
#'       aggregate = overall.av)
#' @export
Model <- function(formula, ..., lower = NULL, upper = NULL,
                  priorSD = NULL, jump = NULL, series = NULL,
                  aggregate = NULL) {
    kValidDistributions <- c("Poisson", "Binomial", "Normal",
                             "PoissonBinomial", "NormalFixed")
    call <- match.call()
    dots <- list(...)
    correct.length <- identical(length(formula), 3L)
    if (!correct.length)
        stop(gettextf("'%s' is not a valid formula for the likelihood",
                      deparse(formula)))
    name.y <- extractResponse(formula)
    y.has.non.standard.name <- !identical(name.y, "y")
    name.y <- methods::new("Name", name.y)
    if (y.has.non.standard.name && is.null(series))
        series <- "y"
    right.hand.side <- formula[[3L]]
    ## use explicit list of valid distributions to give
    ## more meaningful error message, including catching
    ## inappropriate use of valid functions such as
    ## functions specifying priors
    distribution <- deparse(right.hand.side[[1L]])
    if (!(distribution %in% kValidDistributions))
        stop(gettextf("'%s' is not a valid distribution",
                      distribution))
    spec.inner <- eval(right.hand.side, envir = environment(formula))
    SpecModel(specInner = spec.inner,
              call = call,
              nameY = name.y,
              dots = dots,
              lower = lower,
              upper = upper,
              priorSD = priorSD,
              jump = jump,
              series = series,
              aggregate = aggregate)
}

## HAS_TESTS
#' Specify a model based on a Poisson-binomial mixture.
#'
#' Specify a model of the form
#'   \deqn{y_i = U_i + V_i}
#'   \deqn{U_i \sim Poisson((1-p) n_i)}
#'   \deqn{V_i \sim binomial(n_i, p)}.
#'
#' The model is useful mainly as a way of representing
#' the relationship between a true set of counts (the
#' \eqn{n_i}) and measurements of those counts (the
#' \eqn{y_i}.)  For instance, \eqn{n_i} could be true
#' counts of births, and \eqn{y_i} could be births
#' recorded in an accurate births registration system.
#'
#' Subscript \eqn{i} denotes a cell within a classification
#' defined by variables such as age, sex, and time.
#' For instance cell \eqn{i} might be 30-34 year old females
#' in 2020.
#'
#' Higher values of \eqn{p} imply greater accuracy;
#' \eqn{p} can be interpreted as the probability that
#' a person or event is enumerated in the correct
#' cell \eqn{i}.
#'
#' One limitation of the model is that it does not allow
#' for the possibility that \eqn{y_i > 0} when
#' \eqn{n_i = 0}. In other words, it does not allow for the
#' possibility that a person or event is erroneously
#' enumerated in a cell that has no people or events.
#'
#' @param prob The probability that a person or event
#' is correctly enumerated. A number between 0 and 1,
#' and typically close to 1.
#'
#' @return An object of class \code{\linkS4class{SpecLikelihood}}.
#' 
#' @seealso \code{PoissonBinomial} is typically used as
#' part of a call to function \code{\link{Model}}.
#'
#' @examples
#' PoissonBinomial(prob = 0.98)
#' @export
PoissonBinomial <- function(prob) {
    prob <- checkAndTidyMeanOrProb(prob, name = "prob")
    if (prob < 0)
        stop(gettextf("'%s' is less than %d",
                      "prob", 0L))
    if (prob > 1)
        stop(gettextf("'%s' is greter than %d",
                      "prob", 1L))
    methods::new("SpecLikelihoodPoissonBinomialMixture",
        prob = prob)
}


## SpecModel ############################################################

## HAS_TESTS
setMethod("SpecModel",
          signature(specInner = "SpecLikelihoodBinomial"),
          function(specInner, call, nameY, dots, 
                   lower, upper, priorSD, jump, series, aggregate) {
              formula.mu <- specInner@formulaMu
              specs.priors <- makeSpecsPriors(dots)
              names.specs.priors <- makeNamesSpecsPriors(dots)
              if (is.null(lower))
                  lower <- 0
              if (is.null(upper))
                  upper <- 1
              checkLowerAndUpper(lower = lower,
                                 upper = upper,
                                 distribution = "Binomial")
              if (is.null(priorSD))
                  priorSD <- HalfT()
              else {
                  if (!methods::is(priorSD, "HalfT"))
                      stop(gettextf("'%s' has class \"%s\"",
                                    "priorSD", class(priorSD)))
              }
              A.sigma <- priorSD@A
              nu.sigma <- priorSD@nu
              sigma.max <- priorSD@scaleMax
              A.sigma <- makeASigma(A = A.sigma,
                                    sY = NULL,
                                    isSpec = TRUE)
              sigma.max <- makeScaleMax(scaleMax = sigma.max,
                                        A = A.sigma,
                                        nu = nu.sigma,
                                        isSpec = TRUE)
              scale.theta <- checkAndTidyJump(jump)
              series <- checkAndTidySeries(series)
              if (is.null(aggregate))
                  aggregate <- methods::new("SpecAgPlaceholder")
              else {
                  if (methods::is(aggregate, "SpecAgPoisson"))
                      stop(gettextf("Poisson model for accuracy of aggregate values cannot be combined with %s likelihood",
                                    "binomial"))
              }
              methods::new("SpecBinomialVarying",
                           ASigma = A.sigma,
                           call = call,
                           formulaMu = formula.mu,
                           lower = lower,
                           specsPriors = specs.priors,
                           namesSpecsPriors = names.specs.priors,
                           nameY = nameY,
                           nuSigma = nu.sigma,
                           scaleTheta = scale.theta,
                           series = series,
                           sigmaMax = sigma.max,
                           upper = upper,
                           aggregate = aggregate)
          })

## HAS_TESTS
setMethod("SpecModel",
          signature(specInner = "SpecLikelihoodNormalVarsigmaKnown"),
          function(specInner, call, nameY, dots, lower, upper,
                   priorSD, jump, series, aggregate) {
              formula.mu <- specInner@formulaMu
              varsigma <- specInner@varsigma
              specs.priors <- makeSpecsPriors(dots)
              names.specs.priors <- makeNamesSpecsPriors(dots)
              if (is.null(lower))
                  lower <- -Inf
              if (is.null(upper))
                  upper <- Inf
              checkLowerAndUpper(lower = lower,
                                 upper = upper,
                                 distribution = "Normal")
              if (is.null(priorSD))
                  priorSD <- HalfT()
              else {
                  if (!methods::is(priorSD, "HalfT"))
                      stop(gettextf("'%s' has class \"%s\"",
                                    "priorSD", class(priorSD)))
              }
              A.sigma <- priorSD@A
              nu.sigma <- priorSD@nu
              sigma.max <- priorSD@scaleMax
              if (is.null(aggregate) && !is.null(jump))
                  warning(gettextf("'%s' is ignored in Normal model when '%s' is %s",
                                   "jump", "aggregate", "NULL"))
              scale.theta <- checkAndTidyJump(jump) # needed for aggregate models
              series <- checkAndTidySeries(series)
              if (is.null(aggregate))
                  aggregate <- methods::new("SpecAgPlaceholder")
              else {
                  if (methods::is(aggregate, "SpecAgPoisson"))
                      stop(gettextf("Poisson model for accuracy of aggregate values cannot be combined with %s likelihood",
                                    "normal"))
              }
              methods::new("SpecNormalVaryingVarsigmaKnown",
                           ASigma = A.sigma,
                           call = call,
                           formulaMu = formula.mu,
                           lower = lower,
                           specsPriors = specs.priors,
                           namesSpecsPriors = names.specs.priors,
                           nameY = nameY,
                           nuSigma = nu.sigma,
                           scaleTheta = scale.theta,
                           series = series,
                           sigmaMax = sigma.max,
                           upper = upper,
                           varsigma = varsigma,
                           aggregate = aggregate)
          })

## HAS_TESTS
setMethod("SpecModel",
          signature(specInner = "SpecLikelihoodNormalVarsigmaUnknown"),
          function(specInner, call, nameY, dots, lower, upper,
                   priorSD, jump, series, aggregate) {
              formula.mu <- specInner@formulaMu
              A.varsigma <- specInner@AVarsigma
              nu.varsigma <- specInner@nuVarsigma
              varsigma.max <- specInner@varsigmaMax
              specs.priors <- makeSpecsPriors(dots)
              names.specs.priors <- makeNamesSpecsPriors(dots)
              if (is.null(lower))
                  lower <- -Inf
              if (is.null(upper))
                  upper <- Inf
              checkLowerAndUpper(lower = lower,
                                 upper = upper,
                                 distribution = "Normal")
              if (is.null(priorSD))
                  priorSD <- HalfT()
              else {
                  if (!methods::is(priorSD, "HalfT"))
                      stop(gettextf("'%s' has class \"%s\"",
                                    "priorSD", class(priorSD)))
              }
              A.sigma <- priorSD@A
              nu.sigma <- priorSD@nu
              sigma.max <- priorSD@scaleMax
              if (is.null(aggregate) && !is.null(jump))
                  warning(gettextf("'%s' is ignored in Normal model when '%s' is %s",
                                   "jump", "aggregate", "NULL"))
              scale.theta <- checkAndTidyJump(jump) # needed for aggregate models
              series <- checkAndTidySeries(series)
              if (is.null(aggregate))
                  aggregate <- methods::new("SpecAgPlaceholder")
              else {
                  if (methods::is(aggregate, "SpecAgPoisson"))
                      stop(gettextf("Poisson model for accuracy of aggregate values cannot be combined with %s likelihood",
                                    "normal"))
              }
              methods::new("SpecNormalVaryingVarsigmaUnknown",
                           ASigma = A.sigma,
                           AVarsigma = A.varsigma,
                           call = call,
                           formulaMu = formula.mu,
                           lower = lower,
                           specsPriors = specs.priors,
                           namesSpecsPriors = names.specs.priors,
                           nameY = nameY,
                           nuSigma = nu.sigma,
                           nuVarsigma = nu.varsigma,
                           scaleTheta = scale.theta,
                           series = series,
                           sigmaMax = sigma.max,
                           upper = upper,
                           varsigmaMax = varsigma.max,
                           aggregate = aggregate)
          })

## HAS_TESTS
setMethod("SpecModel",
          signature(specInner = "SpecLikelihoodPoisson"),
          function(specInner, call, nameY, dots, lower, upper,
                   priorSD, jump, series, aggregate) {
              formula.mu <- specInner@formulaMu
              useExpose <- specInner@useExpose
              boxCoxParam <- specInner@boxCoxParam
              specs.priors <- makeSpecsPriors(dots)
              names.specs.priors <- makeNamesSpecsPriors(dots)
              if (is.null(lower))
                  lower <- 0
              if (is.null(upper))
                  upper <- Inf
              checkLowerAndUpper(lower = lower,
                                 upper = upper,
                                 distribution = "Poisson")
              if (is.null(priorSD))
                  priorSD <- HalfT()
              else {
                  if (!methods::is(priorSD, "HalfT"))
                      stop(gettextf("'%s' has class \"%s\"",
                                    "priorSD", class(priorSD)))
              }
              A.sigma <- priorSD@A
              nu.sigma <- priorSD@nu
              sigma.max <- priorSD@scaleMax
              scale.theta <- checkAndTidyJump(jump)
              series <- checkAndTidySeries(series)
              has.series <- !is.na(series@.Data)
              if (has.series) {
                  A.sigma <- makeASigma(A = A.sigma,
                                        sY = NULL,
                                        isSpec = TRUE)
                  sigma.max <- makeScaleMax(scaleMax = sigma.max,
                                            A = A.sigma,
                                            nu = nu.sigma,
                                            isSpec = TRUE)
              }
              if (is.null(aggregate))
                  aggregate <- methods::new("SpecAgPlaceholder")
              methods::new("SpecPoissonVarying",
                           ASigma = A.sigma,
                           boxCoxParam = boxCoxParam,
                           call = call,
                           formulaMu = formula.mu,
                           lower = lower,
                           specsPriors = specs.priors,
                           namesSpecsPriors = names.specs.priors,
                           nameY = nameY,
                           nuSigma = nu.sigma,
                           scaleTheta = scale.theta,
                           series = series,
                           sigmaMax = sigma.max,
                           upper = upper,
                           useExpose = useExpose,
                           aggregate = aggregate)
          })

## HAS_TESTS
setMethod("SpecModel",
          signature(specInner = "SpecLikelihoodPoissonBinomialMixture"),
          function(specInner, call, nameY, dots, lower, upper,
                   priorSD, jump, series, aggregate) {
              prob <- specInner@prob
              if (length(dots) > 0L)
                  stop(gettextf("priors specified, but distribution is %s",
                                "Poisson-binomial mixture"))
              for (name in c("lower", "upper", "priorSD", "jump", "aggregate")) {
                  value <- get(name)
                  if (!is.null(value))
                      stop(gettextf("'%s' specified, but distribution is %s",
                                    name, "Poisson-binomial mixture"))
              }
              series <- checkAndTidySeries(series)
              methods::new("SpecPoissonBinomialMixture",
                           call = call,
                           nameY = nameY,
                           series = series,
                           prob = prob)
          })

## HAS_TESTS
setMethod("SpecModel",
          signature(specInner = "SpecLikelihoodNormalFixed"),
          function(specInner, call, nameY, dots, lower, upper,
                   priorSD, jump, series, aggregate) {
              mean <- specInner@mean
              sd <- specInner@sd
              metadata <- specInner@metadata
              useExpose <- specInner@useExpose
              if (length(dots) > 0L)
                  stop(gettextf("priors specified, but distribution is %s",
                                "NormalFixed"))
              for (name in c("lower", "upper", "priorSD", "jump", "aggregate")) {
                  value <- get(name)
                  if (!is.null(value))
                      stop(gettextf("'%s' specified, but distribution is %s",
                                    name, "NormalFixed"))
              }
              series <- checkAndTidySeries(series)
              methods::new("SpecNormalFixed",
                           call = call,
                           nameY = nameY,
                           series = series,
                           mean = mean,
                           sd = sd,
                           metadata = metadata,
                           useExpose = useExpose)
          })


## SpecAggregate #########################################################

#' Specify aggregate values.
#'
#' Specify values for aggregations of low-level parameters.
#' Aggregate values can be used to provide extra information to a model,
#' beyond the information contains in the main dataset.  For instance,
#' aggregate values can be used implement benchmarks or incorporate
#' expert judgements.
#'
#' Let \eqn{\gamma_i} be a rate, count, probability, or mean for cell \eqn{i}.
#' For instance, \eqn{\gamma_i} could be the prevalence of obesity in a
#' particular combination of age, educational status, and region, or it
#' could be an age-sex-specific mortality rate during a given period.  
#' The \eqn{\gamma_i} are underlying parameters that are not observed
#' directly.
#'
#' Let \eqn{\psi_j} be a more aggregate parameter describing the same
#' phenomenon as the \eqn{\gamma_i}.  For instance, \eqn{\psi_j} could
#' be the average prevalence of obesity in region \eqn{j}, or life expectancy
#' for sex \eqn{j}.  Like the \eqn{\gamma_i}, the \eqn{\psi_j} are not
#' observed directly.
#'
#' Typically, \eqn{\psi_j} is a weighted sum of the associated \eqn{\gamma_i},
#' that is,
#'
#'   \deqn{\psi_j = \sum b_{ij} \gamma_i,}
#'
#' where \eqn{b_{ij} > 0} if \eqn{\gamma_i} is associated with \eqn{\psi_j},
#' and 0 otherwise.  For instance, if \eqn{\gamma_i} describes obesity
#' prevalence for a subpopulation in region \eqn{j}, then \eqn{b_{ij} > 0},
#' and if it describes obesity prevanece in another region, then
#' \eqn{b_{ij} = 0}.
#'
#' However, more complicated relationships between the \eqn{\psi_j} and
#' \eqn{\gamma_j} are also permitted.  In the most general case,
#' 
#'   \deqn{\psi_j = f(B, \gamma),}
#'
#' where \eqn{B} is a matrix of \eqn{b_{ij}}, and \eqn{f} is an arbitrary
#' function.  For instance, \eqn{f} could be a (non-linear) function that takes
#' a vector of age-specific mortality rates and returns life expectancy.
#'
#' Let \eqn{m_j} be an estimate, prediction, or elicited value for
#' aggregate parameter \eqn{\psi_j}.  For instance, \eqn{m_j} could be a
#' previously published estimate of obesity prevalence in region \eqn{j},
#' or it could be an expert's life expectancy forecast.  In contrast to the
#' \eqn{\gamma_i} and \eqn{\psi_j}, the \eqn{m_j} are observed. The \eqn{m_j}
#' are 'aggregate values'.
#'
#' Aggregate values are treated as data, and placed in the likelihood.  To do
#' so, a sub-model specifying the relationship between the \eqn{m_j} and
#' \eqn{\psi_j} is required.  A sub-model
#'
#'   \deqn{p(m_j | \psi_j),}
#'
#' is, in effect, a model for the accuracy of the \eqn{m_j}.
#'
#' Different choices for the relationship between (i) the \eqn{\gamma_i} and
#' \eqn{\psi_j}, and (ii) the \eqn{\psi_j} and \eqn{m_j} are appropriate
#' for different applications.  The combinations that are currently available
#' in \code{demest} are documented below.
#' 
#' Default values for the \eqn{b_{ij}} vary according to the model being used:
#'
#' \tabular{ll}{
#'    Model \tab Default \cr
#'    Poisson with exposure \tab \code{exposure} argument, normalised to sum to
#' 1 for each \eqn{j}. \cr
#'    Poisson without exposure \tab All weights equal to 1. \cr
#'    Binomial \tab \code{exposure} argument, normalised to sum to 1 for each
#' \eqn{j}. \cr
#'    Normal \tab \code{weights} argument (which defaults to 1).
#' }
#'
#' The \code{concordances} argument is needed when \code{values} has categories
#' that are collapsed versions of categories of \code{weights}, or the
#' underlying rates, probabilities, or means.  For instance, \code{values}
#' might be specified at the state level, while the rates are estimated at
#' the county level.  The mapping between the original and collapsed
#' categories is known as a \code{\link[classconc]{Concordance}}.
#' 
#' @section \code{AgCertain}:
#'
#' The aggregate parameters are weighted sums or means of the disaggregated
#' parameters,
#'
#'     \deqn{\psi_j = \sum b_{ij} \gamma_i,}
#'
#' and the aggregate values are treated as error-free,
#'
#'     \deqn{m_j = \psi_j.}
#'
#' Although it is seldom realistic to treat an aggregate parameter as known
#' with certainty, there can be pragmatic reasons for doing so.  For
#' instance, statistical agencies sometimes require that disaggregated
#' estimates agree exactly with previously-published aggregate estimates.
#' (Within the literature on small area estimation, this practice is known
#' as 'benchmarking'.)  With \code{AgCertain}, agreement is guaranteed.
#' For instance, new estimates of obesity by age, educational status, and
#' region can be made to agree with existing estimates of obesity by region.
#' 
#' @section \code{AgNormal}:
#'
#' The aggregate parameters are weighted sums or means of the disaggregated
#' parameters,
#'
#'     \deqn{\psi_j = \sum b_{ij} \gamma_i.}
#'
#' However, in contrast to \code{AgCertain}, the aggregate parameters are
#' assumed to be observed with error.  The errors have normal distributions,
#' with mean 0 and standard deviation \eqn{s_j}, so that
#'
#'     \deqn{m_j ~ N(\psi_j, s_j^2).}
#'
#' One possible application for \code{AgNormal} is 'inexact' benchmarking,
#' where the disaggregated parameters are pulled towards the benchmarks, but
#' complete agreement is not required.  Another application is where expert
#' judgements are treated as fallible.
#'
#' @section \code{AgPoisson}:
#'
#' \code{AgPoisson} is used only with Poisson models that contain an exposure
#' term.  The aggregate parameters are rates, obtained using
#' 
#'     \deqn{\psi_j = \sum b_{ij} \gamma_i,}
#'
#' where the \eqn{b_{ij}} are proportional to exposures.  Let \eqn{n_j} be
#' exposure term associated with \eqn{\psi_i}.  The expected count implied by
#' \eqn{\psi_j} is then \eqn{\psi_j n_j}.  The expected count is implied by
#' aggregate value \eqn{m_j} is \eqn{m_j n_j}.  The two expected counts are
#' related by
#'
#'     \deqn{m_j n_j ~ Poisson(\psi_j n_j).}
#'
#' @section \code{AgFun}:
#'
#' The aggregate parameters are obtained from the disaggregated parameters
#' through a user-defined function \eqn{f}.  Let \eqn{\gamma_{[j]}} denote the
#' vector of \eqn{\gamma_i} associated with aggregate parameter \eqn{\psi_j}.
#' Similarly let \eqn{b_{[j]}} denote the vector of \eqn{b_{ij}} associated
#' with \eqn{\psi_j}.  Then
#'
#'     \deqn{\psi_j = f(\gamma_{[j]}, b_{[j]}).}
#'
#' \code{AgFun} uses the same model as \code{AgNormal} for the accuracy of the
#' accuracy of the \eqn{m_j},
#'
#'     \deqn{m_j ~ N(\psi_j, s_j^2).}
#'
#' User-supplied function \code{FUN} must take two arguments, called \code{x}
#' and \code{weights}, and return a numeric vector of length 1.  The \code{x}
#' argument is for the \eqn{\gamma_{[j]}} and the \code{weights} argument is
#' for the \eqn{b_{[j]}}.  The values for \code{x} supplied to \code{FUN}
#' during estimation have class \code{\link[dembase:Values]{Values-class}}
#' and the values for \code{weights} have class
#' \code{\link[dembase:Counts]{Counts-class}}.  Function \code{FUN} can
#' take advantage of the metadata attached to \code{x} and \code{weights}:
#' see below for an example.
#' 
#' @param value The aggregate value or values.  A single number, or, if there
#' are multiple values, an object of class
#' \code{\linkS4class{DemographicArray}}.
#' @param sd Standard deviation(s) for errors. If \code{value} is a single
#' number, then \code{sd} must be a single number; otherwise \code{sd} must be
#' an object of class \code{\linkS4class{DemographicArray}}.
#' @param weights An object of class \code{\linkS4class{Counts}} holding
#' weights to be used when aggregating. Optional.
#' @param concordances A named list of objects of class
#' \code{\link[classconc]{ManyToOne}}.
#' @param FUN A function taking arguments called \code{x} and \code{weights}
#' and returning a single number.  See below for details.
#' @param jump The standard deviation of the proposal density used in
#' Metropolis-Hastings updates.
#'
#' @return An object of class \code{\linkS4class{SpecAggregate}}.
#'
#' @seealso Aggregate values are typically specified as part of a call
#' to function \code{\link{Model}}.
#'
#' @examples
#' ## Overall value of 0.8 known with certainty
#' AgCertain(0.8)
#'
#' ## Separate values for females and males known
#' ## with certainty
#' value <- ValuesOne(c(0.5, 1.1),
#'                    labels = c("Female", "Male"),
#'                    name = "sex")
#' AgCertain(value)
#'
#' ## Non-default weights
#' weights <- Counts(array(c(0.6, 0.3, 0.2, 0.4, 0.2, 0.3),
#'                         dim = c(2, 3),
#'                         dimnames = list(sex = c("Female", "Male"),
#'                                         region = c("A", "B", "C"))))
#' AgCertain(value = value, weights = weights)
#' 
#' ## Overall value of 0.8, with all errors having
#' ## standard deviation of 0.1
#' AgNormal(value = 0.8, sd = 0.1)
#'
#' ## Aggregate values and errors that vary by sex
#' sd <- ValuesOne(c(0.15, 0.25),
#'                 labels = c("Female", "Male"),
#'                 name = "sex")
#' AgNormal(value = value, sd = sd)
#'
#' ## Non-default standard deviation for proposal density
#' AgNormal(value = value, sd = sd, jump = 0.02)
#'
#' ## Poisson model
#' AgPoisson(value)
#'
#' ## TODO - AgFun
#' @name Aggregate
NULL

## HAS_TESTS
#' @rdname Aggregate
#' @export
AgCertain <- function(value, weights = NULL, concordances = list()) {
    l <- makeValueAndMetaDataAg(value)
    valueAg <- l$value
    metadataAg <- l$metadata
    checkSpecWeightAg(weights = weights,
                      metadata = metadataAg)
    checkConcordances(concordances)
    methods::new("SpecAgCertain",
                 metadataAg = metadataAg,
                 valueAg = valueAg,
                 weightAg = weights,
                 concordancesAg = concordances)
}

## HAS_TESTS
#' @rdname Aggregate
#' @export
AgNormal <- function(value, sd, weights = NULL, concordances = list(), jump = NULL) {
    l <- makeValueAndMetaDataAg(value)
    valueAg <- l$value
    metadataAg <- l$metadata
    checkSpecWeightAg(weights = weights,
                      metadata = metadataAg)
    sdAg <- checkAndTidySDAg(sd = sd,
                             value = value,
                             metadata = metadataAg)
    checkConcordances(concordances)
    scaleAg <- checkAndTidyJump(jump)
    methods::new("SpecAgNormal",
                 metadataAg = metadataAg,
                 scaleAg = scaleAg,
                 sdAg = sdAg,
                 valueAg = valueAg,
                 weightAg = weights,
                 concordancesAg = concordances)
}

## HAS_TESTS
#' @rdname Aggregate
#' @export
AgPoisson <- function(value, concordances = list(), jump = NULL) {
    l <- makeValueAndMetaDataAg(value)
    valueAg <- l$value
    metadataAg <- l$metadata
    checkConcordances(concordances)
    scaleAg <- checkAndTidyJump(jump)
    methods::new("SpecAgPoisson",
                 metadataAg = metadataAg,
                 scaleAg = scaleAg,
                 valueAg = valueAg,
                 concordancesAg = concordances)
}

## HAS_TESTS
#' @rdname Aggregate
#' @export
AgFun <- function(value, sd, FUN, weights = NULL, concordances = list()) {
    l <- makeValueAndMetaDataAg(value)
    valueAg <- l$value
    metadataAg <- l$metadata
    sdAg <- checkAndTidySDAg(sd = sd,
                             value = value,
                             metadata = metadataAg)
    checkFunAg(FUN)
    checkSpecWeightAg(weights = weights,
                      metadata = metadataAg)
    checkConcordances(concordances)
    methods::new("SpecAgFun",
                 funAg = FUN,
                 metadataAg = metadataAg,
                 sdAg = sdAg,
                 valueAg = valueAg,
                 weightAg = weights,
                 concordancesAg = concordances)
}

## HAS_TESTS
#' @rdname Aggregate
#' @export
AgLife <- function(value, sd, ax = NULL,
                   concordances = list()) {
    checkAxAg(ax = ax,
              value = value)
    if (methods::is(value, "DemographicArray")) {
        dimtypes <- dimtypes(value, use.names = FALSE)
        if ("age" %in% dimtypes)
            stop(gettextf("'%s' has a dimension with %s \"%s\"",
                          "values", "dimtype", "age"))
    }
    l <- makeValueAndMetaDataAg(value)
    valueAg <- l$value
    metadataAg <- l$metadata
    sdAg <- checkAndTidySDAg(sd = sd,
                             value = value,
                             metadata = metadataAg)
    checkConcordances(concordances)
    methods::new("SpecAgLife",
                 metadataAg = metadataAg,
                 sdAg = sdAg,
                 axAg = ax,
                 valueAg = valueAg,
                 concordancesAg = concordances)
}

