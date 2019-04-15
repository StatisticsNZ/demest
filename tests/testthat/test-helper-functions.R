
context("helper-functions")

n.test <- 5
test.identity <- FALSE
test.extended <- FALSE


## DEMOGRAPHIC OBJECTS #############################################################

test_that("checkAllTermsInFormulaSpecified works", {
    checkAllTermsInFormulaSpecified <- demest:::checkAllTermsInFormulaSpecified
    formula <- mean ~ age * sex
    namesSpecPriors <- c("age", "sex", "age:sex")
    expect_identical(checkAllTermsInFormulaSpecified(formula = formula,
                                                     namesSpecPriors = namesSpecPriors),
                     NULL)
    expect_error(checkAllTermsInFormulaSpecified(formula = formula,
                                                 namesSpecPriors = c("age", "sex")),
                 sprintf("no prior specified for term %s in formula 'mean ~ age \\* sex",
                         sQuote("age:sex")))
    expect_error(checkAllTermsInFormulaSpecified(formula = formula,
                                                 namesSpecPriors = "age"),
                 sprintf("no priors specified for terms %s in formula 'mean ~ age \\* sex",
                         paste(sQuote(c("sex", "age:sex")), collapse = ", ")))
})

test_that("listAllSubsets works", {
    listAllSubsets <- demest:::listAllSubsets
    ans.obtained <- listAllSubsets(n = 1L, max = 0L)
    ans.expected <- list()
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- listAllSubsets(n = 2L, max = 0L)
    ans.expected <- list()
    expect_identical(ans.obtained, ans.expected)    
    ans.obtained <- listAllSubsets(n = 2L, max = 1L)
    ans.expected <- list(1L, 2L)
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- listAllSubsets(n = 3L, max = 1L)
    ans.expected <- list(1L, 2L, 3L)
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- listAllSubsets(n = 3L, max = 2L)
    ans.expected <- list(1L, 2L, 3L, 1:2, c(1L, 3L), 2:3)
    expect_identical(ans.obtained, ans.expected)
    expect_error(listAllSubsets(n = 3L, max = 3L),
                 "'max' greater than or equal to 'n'")
})

test_that("makeIteratorBetas works", {
    makeIteratorBetas <- demest:::makeIteratorBetas
    BetaIterator <- demest:::BetaIterator
    y <- Counts(array(1:24,
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), region = c("a", "b", "c"), age = 0:3)))
    betas <- list(2, rnorm(3), rnorm(4), rnorm(12))
    namesBetas = c("(Intercept)", "region", "age", "region:age")
    ans.obtained <- makeIteratorBetas(betas = betas, namesBetas = namesBetas, y = y)
    ans.expected <- BetaIterator(dim = dim(y), margins = list(0L, 2L, 3L, 2:3))
    expect_identical(ans.obtained, ans.expected)
})

test_that("R version of makeMu works", {
    makeMu <- demest:::makeMu
    BetaIterator <- demest:::BetaIterator
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        betas <- list("(Intercept)" = rnorm(1), a = rnorm(3),
                      b = rnorm(4), c = rnorm(5),
                      ab = rnorm(12), bc = rnorm(20), abc = rnorm(60))
        iterator <- BetaIterator(dim  = 3:5,
                                 margins = list(0L, 1L, 2L, 3L, 1:2, 2:3, 1:3))
        ans.obtained <- makeMu(n = 60L, betas = betas, iterator = iterator)
        ans.expected <- (betas[[1]]
                         + betas[[2]]
                         + rep(betas[[3]], each = 3)
                         + rep(betas[[4]], each = 12)
                         + betas[[5]]
                         + rep(betas[[6]], each = 3)
                         + betas[[7]])
        expect_identical(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of makeMu give same answer", {
    makeMu <- demest:::makeMu
    BetaIterator <- demest:::BetaIterator
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        betas <- list("(Intercept)" = rnorm(1), a = rnorm(3), b = rnorm(4), c = rnorm(5),
                      ab = rnorm(12), ac = rnorm(15), bc = rnorm(20), abc = rnorm(60))
        zetas <- rnorm(8)
        iterator <- BetaIterator(dim  = 3:5,
                                 margins = list(0L, 1L, 2L, 3L, 1:2, c(1L, 3L),
                                 2:3, 1:3))
        ans.R <- makeMu(n = 60L,
                        betas = betas,
                        iterator = iterator,
                        useC = TRUE)
        ans.C <- makeMu(n = 60L,
                        betas = betas,
                        iterator = iterator,
                        useC = FALSE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("maxWithSubtotal works", {
    maxWithSubtotal <- demest:::maxWithSubtotal
    ## 'x' has missing values
    x <- c(1:9, NA)
    expect_equal(maxWithSubtotal(x, max = rep(5L, length(x)), subtotal = 50L),
                 c(1:5, rep(5L, 4), NA))
    x <- as.integer(NA)
    expect_equal(maxWithSubtotal(x, max = 5L, subtotal = 5L),
                 x)
    ## 'x' has no missing values and sum(x) is equal to sum(max)
    expect_equal(maxWithSubtotal(x = 1:10, max = 10:1, subtotal = 55L),
                 10:1)
    ## 'x' has no missing values and sum(x) is less than sum(max) - all integer
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x <- 1:10
        max <- 12:3
        subtotal <- 55L
        ans.obtained <- maxWithSubtotal(x = x, max = max, subtotal = subtotal)
        expect_true(all(ans.obtained <= max))
        expect_true(sum(ans.obtained) == subtotal)
    }
    expect_equal(maxWithSubtotal(x = 4L, max = 5L, subtotal = 4L),
                 4L)
    expect_error(maxWithSubtotal(x = c(0.1, 1:9), max = 5L, subtotal = 4L),
                 "'x' does not have type \"integer\"")
    expect_error(maxWithSubtotal(x = c(-1L, 1:9), max = 1:10, subtotal = 55L),
                 "'x' has negative values")
    expect_error(maxWithSubtotal(x = 1:10, max = 5, subtotal = 50L),
                 "'max' does not have type \"integer\"")
    expect_error(maxWithSubtotal(x = 1:10, max = c(1:9, NA), subtotal = 55),
                 "'max' has missing values")
    expect_error(maxWithSubtotal(x = 1:9, max = 1:10, subtotal = 55),
                 "'x' and 'max' have different lengths")
    expect_error(maxWithSubtotal(x = 1:10, max = 10:1, subtotal = c(55, 55)),
                 "'subtotal' does not have length 1")
    expect_error(maxWithSubtotal(x = 1:10, max = rep(5L, 10), subtotal = 50),
                 "'subtotal' does not have type \"integer\"")
    expect_error(maxWithSubtotal(x = 1:10, max = 10:1, subtotal = as.integer(NA)),
                 "'subtotal' is missing")
    expect_error(maxWithSubtotal(x = 1:10, max = 10:1, subtotal = 100L),
                 "'max' and 'subtotal' incompatible")
    expect_error(maxWithSubtotal(x = c(1:9, NA), max = 10:1, subtotal = 10L),
                 "'x' and 'subtotal' incompatible")
    expect_error(maxWithSubtotal(x = 1:10, max = 10:1, subtotal = 54L),
                 "'x' and 'subtotal' incompatible")
})



## SPECIFICATIONS ##################################################################

test_that("addInfantToData works", {
    addInfantToData <- demest:::addInfantToData
    ## 'data' blank
    metadata <- new("MetaData",
                    nms = "age",
                    dimtypes = "age",
                    DimScales = list(new("Intervals", dimvalues = c(0, 1, 5, 10, Inf))))
    data <- new("data.frame")
    ans.obtained <- addInfantToData(metadata = metadata,
                                    data = data)
    ans.expected <- data.frame(age = c("0", "1-4", "5-9", "10+"),
                               infant = c(1L, 0L, 0L, 0L))
    expect_identical(ans.obtained, ans.expected)
    ## 'data' has values
    metadata <- new("MetaData",
                    nms = "age",
                    dimtypes = "age",
                    DimScales = list(new("Intervals", dimvalues = c(0, 1, 5, 10, Inf))))
    data <- data.frame(income = 1:4,
                       age = c("0", "1-4", "5-9", "10+"))
    ans.obtained <- addInfantToData(metadata = metadata,
                                    data = data)
    ans.expected <- data.frame(income = 1:4,
                               age = c("0", "1-4", "5-9", "10+"),
                               infant = c(1L, 0L, 0L, 0L))
    expect_identical(ans.obtained, ans.expected)
    ## 'data' includes 'infant' column
    metadata <- new("MetaData",
                    nms = "age",
                    dimtypes = "age",
                    DimScales = list(new("Intervals", dimvalues = c(0, 1, 5, 10, Inf))))
    data <- data.frame(income = 1:4,
                       age = c("0", "1-4", "5-9", "10+"),
                       infant = 4:1)
    ans.obtained <- addInfantToData(metadata = metadata,
                                    data = data)
    ans.expected <- data.frame(income = 1:4,
                               age = c("0", "1-4", "5-9", "10+"),
                               infant = 4:1,
                               infant.1 = c(1L, 0L, 0L, 0L))
    expect_identical(ans.obtained, ans.expected)
})


test_that("addInfantToData throws appropriate errors", {
    addInfantToData <- demest:::addInfantToData
    ## not main effect
    metadata <- new("MetaData",
                    nms = c("age", "sex"),
                    dimtypes = c("age", "sex"),
                    DimScales = list(new("Intervals", dimvalues = c(0, 1, 5, 10, Inf)),
                                     new("Sexes", dimvalues = c("Female", "Male"))))
    data <- new("data.frame")
    expect_error(addInfantToData(metadata = metadata,
                                 data = data),
                 "cannot add \"infant\" covariate to prior 'age\\:sex' because 'age\\:sex' is not a main effect for age")
    ## dimension has Points dimscale
    metadata <- new("MetaData",
                    nms = "age", 
                    dimtypes = "age",
                    DimScales = list(new("Points", dimvalues = c(0, 1, 5))))
    data <- new("data.frame")
    expect_error(addInfantToData(metadata = metadata,
                                 data = data),
                 "cannot make \"infant\" covariate, because dimension with dimtype \"age\" has dimscale \"Points\"")
    ## dimension too short
    metadata <- new("MetaData",
                    nms = "age", 
                    dimtypes = "age",
                    DimScales = list(new("Intervals", dimvalues = c(0, 1))))
    data <- new("data.frame")
    expect_error(addInfantToData(metadata = metadata,
                                 data = data),
                 "cannot make \"infant\" covariate, because dimension with dimtype \"age\" has length 1")
    ## no age dimension in data
    metadata <- new("MetaData",
                    nms = "age",
                    dimtypes = "age",
                    DimScales = list(new("Intervals", dimvalues = c(0, 1, 5, 10, Inf))))
    data <- data.frame(income = 1:4,
                       wrong = c("0", "1-4", "5-9", "10+"),
                       infant = 4:1)
    expect_error(addInfantToData(metadata = metadata,
                                 data = data),
                 "could not find variable 'age' in covariate data for prior 'age'")
})


test_that("checkAndTidyLevelComponentWeightMinMax works", {
    checkAndTidyLevelComponentWeightMinMax <- demest:::checkAndTidyLevelComponentWeightMinMax
    expect_identical(checkAndTidyLevelComponentWeightMinMax(minAR2 = -4,
                                                            maxAR2 = 4),
                     list(minLevelComponentWeight = -4,
                          maxLevelComponentWeight = 4))
    expect_identical(checkAndTidyLevelComponentWeightMinMax(minAR2 = -4L,
                                                            maxAR2 = Inf),
                     list(minLevelComponentWeight = -4,
                          maxLevelComponentWeight = Inf))
    expect_error(checkAndTidyLevelComponentWeightMinMax(minAR2 = -4L,
                                                        maxAR2 = c(1, 2)),
                 "'maxAR2' does not have length 1")
    expect_error(checkAndTidyLevelComponentWeightMinMax(minAR2 = "-4",
                                                        maxAR2 = 4),
                 "'minAR2' is non-numeric")
    expect_error(checkAndTidyLevelComponentWeightMinMax(minAR2 = -4L,
                                                        maxAR2 = as.numeric(NA)),
                 "'maxAR2' is missing")
    expect_error(checkAndTidyLevelComponentWeightMinMax(minAR2 = -4,
                                                        maxAR2 = -4),
                 "'minAR2' is greater than or equal to 'maxAR2'")
})

test_that("checkAndTidyStructuralZeros works", {
    checkAndTidyStructuralZeros <- demest:::checkAndTidyStructuralZeros
    ans.obtained <- checkAndTidyStructuralZeros(NULL)
    ans.expected <- NULL
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- checkAndTidyStructuralZeros("diag")
    ans.expected <- new("Values")
    expect_identical(ans.obtained, ans.expected)
    x <- Values(matrix(c(0, 1, 2, 0),
                       nr = 2,
                       dimnames = list(reg_orig = c("a", "b"), reg_dest = c("a", "b"))))
    ans.obtained <- checkAndTidyStructuralZeros(x)
    ans.expected <- x
    expect_identical(ans.obtained, ans.expected)
    x.wrong <- x
    x.wrong[2] <- NA
    expect_error(checkAndTidyStructuralZeros(x.wrong),
                 "'structuralZeros' has missing values")
    x.wrong <- x
    x.wrong[c(1, 4)] <- 2
    expect_error(checkAndTidyStructuralZeros(x.wrong),
                 "'structuralZeros' does not contain any zeros")
    expect_error(checkAndTidyStructuralZeros("wrong"),
                 "'structuralZeros' has class \"character\"")
})

test_that("checkAndTidyYForStrucZero works", {
    checkAndTidyYForStrucZero <- demest:::checkAndTidyYForStrucZero
    y <- Counts(matrix(c(0L, 1L, 2L, 0L),
                       nr = 2,
                       dimnames = list(reg_orig = c("a", "b"), reg_dest = c("a", "b"))))
    strucZeroArray <- Counts(matrix(c(0L, 1L, 1L, 0L),
                                    nr = 2,
                                    dimnames = list(reg_orig = c("a", "b"), reg_dest = c("a", "b"))))
    ans.obtained <- checkAndTidyYForStrucZero(y = y, strucZeroArray = strucZeroArray)
    ans.expected <- y
    expect_identical(ans.obtained, ans.expected)
    y <- Counts(matrix(c(NA, 1L, 2L, NA),
                       nr = 2,
                       dimnames = list(reg_orig = c("a", "b"), reg_dest = c("a", "b"))))
    strucZeroArray <- Counts(matrix(c(0L, 1L, 1L, 0L),
                                    nr = 2,
                                    dimnames = list(reg_orig = c("a", "b"), reg_dest = c("a", "b"))))
    ans.obtained <- checkAndTidyYForStrucZero(y = y, strucZeroArray = strucZeroArray)
    ans.expected <- y
    ans.expected[c(1, 4)] <- 0L
    expect_identical(ans.obtained, ans.expected)
    y.wrong <- Counts(matrix(c(NA, 1L, 2L, 1L),
                       nr = 2,
                       dimnames = list(reg_orig = c("a", "b"), reg_dest = c("a", "b"))))
    strucZeroArray <- Counts(matrix(c(0L, 1L, 1L, 0L),
                                    nr = 2,
                                    dimnames = list(reg_orig = c("a", "b"), reg_dest = c("a", "b"))))
    expect_error(checkAndTidyYForStrucZero(y = y.wrong, strucZeroArray = strucZeroArray),
                 "cell '\\[b, b\\]' of 'y' is a structural zero but has value 1")
})


test_that("checkLowerOrUpper works", {
    checkLowerOrUpper <- demest:::checkLowerOrUpper
    expect_identical(checkLowerOrUpper(value = 0.1,
                                       name = "lower",
                                       distribution = "Binomial"),
                     NULL)
    expect_identical(checkLowerOrUpper(value = Inf,
                                       name = "upper",
                                       distribution = "Poisson"),
                     NULL)
    expect_identical(checkLowerOrUpper(value = -Inf,
                                       name = "lower",
                                       distribution = "Normal"),
                     NULL)
    expect_error(checkLowerOrUpper(value = c(0, 0),
                                   name = "lower",
                                   distribution = "Binomial"),
                 "'lower' does not have length 1")
    expect_error(checkLowerOrUpper(value = "100",
                                   name = "upper",
                                   distribution = "Poisson"),
                 "'upper' is non-numeric")
    expect_error(checkLowerOrUpper(value = as.numeric(NA),
                                   name = "lower",
                                   distribution = "Normal"),
                 "'lower' is missing")
    expect_error(checkLowerOrUpper(value = -1,
                                   name = "lower",
                                   distribution = "Binomial"),
                 "'lower' is less than 0")
    expect_error(checkLowerOrUpper(value = 1.5,
                                   name = "upper",
                                   distribution = "Binomial"),
                 "'upper' is greater than 1")
    expect_error(checkLowerOrUpper(value = -0.0000001,
                                   name = "lower",
                                   distribution = "Poisson"),
                 "'lower' is less than 0")
})

test_that("checkLowerAndUpper works", {
    checkLowerAndUpper <- demest:::checkLowerAndUpper
    expect_identical(checkLowerAndUpper(lower = 0.1,
                                        upper = 0.9,
                                        distribution = "Binomial"),
                     NULL)
    expect_identical(checkLowerAndUpper(lower = 0.1,
                                        upper = Inf,
                                        distribution = "Poisson"),
                     NULL)
    expect_identical(checkLowerAndUpper(lower = -Inf,
                                        upper = 100,
                                        distribution = "Normal"),
                     NULL)
    expect_error(checkLowerAndUpper(lower = c(0, 0),
                                    upper = 1,
                                    distribution = "Binomial"),
                 "'lower' does not have length 1")
    expect_error(checkLowerAndUpper(lower = 0,
                                    upper = "100",
                                    distribution = "Poisson"),
                 "'upper' is non-numeric")
    expect_error(checkLowerAndUpper(lower = as.numeric(NA),
                                    upper = 100,
                                    distribution = "Normal"),
                 "'lower' is missing")
    expect_error(checkLowerAndUpper(lower = 0,
                                    upper = 0,
                                    distribution = "Binomial"),
                 "'lower' is not less than 'upper")
    expect_error(checkLowerAndUpper(lower = -1,
                                    upper = 0,
                                    distribution = "Binomial"),
                 "'lower' is less than 0")
    expect_error(checkLowerAndUpper(lower = 0.5,
                                    upper = 1.5,
                                    distribution = "Binomial"),
                 "'upper' is greater than 1")
    expect_error(checkLowerAndUpper(lower = -0.0000001,
                                    upper = Inf,
                                    distribution = "Poisson"),
                 "'lower' is less than 0")
})
    
test_that("initialDLMAll works", {
    initialDLMAll <- demest:::initialDLMAll
    set.seed(100)
    ## sY is NULL
    spec <- DLM(trend = NULL)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(2, 10),
                                   dimnames = list(sex = c("f", "m"),
                                                   time = 2001:2010)),
                             dimscales = c(time = "Points"))
    l <- initialDLMAll(spec,
                       beta = beta,
                       metadata = metadata,
                       sY = NULL,
                       isSaturated = FALSE,
                       margin = 2L,
                       strucZeroArray = strucZeroArray)
    expect_identical(l$AAlpha, new("Scale", 1.0))
    expect_identical(l$ATau, new("Scale", 1.0))
    expect_identical(l$alphaDLM, new("ParameterVector", rep(0, 11L)))
    expect_identical(l$iAlong, 1L)
    expect_identical(l$iteratorState@indices, 1:11)
    expect_identical(l$iteratorV@indices, 1:10)
    expect_identical(l$J, new("Length", 10L))
    expect_identical(l$K, new("Length", 10L))
    expect_identical(l$L, new("Length", 1L))
    expect_identical(l$minPhi, 0.8)
    expect_identical(l$maxPhi, 1)
    expect_identical(l$shape1Phi, new("Scale", 2))
    expect_identical(l$shape2Phi, new("Scale", 2))
    expect_identical(l$nuAlpha, new("DegreesFreedom", 7.0))
    expect_identical(l$nuTau, new("DegreesFreedom", 7.0))
    expect_identical(length(l$omegaAlpha), 1L)
    expect_identical(l$omegaAlphaMax, new("Scale", qhalft(0.999, 7, 1)))
    expect_false(l$phiKnown@.Data)
    expect_identical(length(l$tau), 1L)
    expect_identical(l$tauMax, new("Scale", qhalft(0.999, 7, 1)))
    expect_identical(l$allStrucZero, rep(FALSE, 10))
    expect_false(l$alongAllStrucZero)
    ## sY is 100
    spec <- DLM(trend = NULL)
    beta <- rnorm(10, mean = 100)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(2, 10),
                                   dimnames = list(sex = c("f", "m"),
                                                   time = 2001:2010)),
                             dimscales = c(time = "Points"))
    l <- initialDLMAll(spec,
                       beta = beta,
                       metadata = metadata,
                       sY = 100,
                       isSaturated = TRUE,
                       margin = 2L,
                       strucZeroArray = strucZeroArray)
    expect_identical(l$ATau, new("Scale", 100))
    expect_identical(l$AAlpha, new("Scale", 100))
    expect_identical(l$omegaAlphaMax, new("Scale", qhalft(0.999, 7, 100)))
    expect_identical(l$tauMax, new("Scale", qhalft(0.999, 7, 100)))
    ## mult is 0.5
    spec <- DLM(level = Level(scale = HalfT(mult = 0.5)),
                trend = NULL,
                error = Error(scale = HalfT(mult = 0.5)))
    beta <- rnorm(10, mean = 100)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(2, 10),
                                   dimnames = list(sex = c("f", "m"),
                                                   time = 2001:2010)),
                             dimscales = c(time = "Points"))
    l <- initialDLMAll(spec,
                       beta = beta,
                       metadata = metadata,
                       sY = NULL,
                       isSaturated = TRUE,
                       margin = 2L,
                       strucZeroArray = strucZeroArray)
    expect_identical(l$ATau, new("Scale", 0.5))
    expect_identical(l$AAlpha, new("Scale", 0.5))
    expect_identical(l$omegaAlphaMax, new("Scale", qhalft(0.999, 7, 0.5)))
    expect_identical(l$tauMax, new("Scale", qhalft(0.999, 7, 0.5)))
    ## hasLevel is FALSE
    spec <- DLM(level = NULL)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(2, 10),
                                   dimnames = list(sex = c("f", "m"),
                                                   time = 2001:2010)),
                             dimscales = c(time = "Points"))
    l <- initialDLMAll(spec,
                       beta = beta,
                       metadata = metadata,
                       sY = NULL,
                       isSaturated = FALSE,
                       margin = 2L,
                       strucZeroArray = strucZeroArray)
    expect_identical(l$omegaAlpha@.Data, 0)
})

test_that("initialDLMAllPredict works", {
    initialDLMAllPredict <- demest:::initialDLMAllPredict
    initialPrior <- demest:::initialPrior
    set.seed(100)
    ## main effect
    spec <- DLM(trend = NULL)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = TRUE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 5,
                                   dimnames = list(time = 2011:2015)),
                             dimscales = c(time = "Points"))
    l <- initialDLMAllPredict(prior = prior,
                              metadata = metadata.new,
                              name = "time",
                              along = "time",
                              margin = 1L,
                              strucZeroArray = strucZeroArray)
    expect_identical(length(l$alphaDLM), 6L)
    expect_identical(l$iteratorState@indices, 1:6)
    expect_identical(l$iteratorStateOld@indices, 1:11)
    expect_identical(l$iteratorV@indices, 1:5)
    expect_identical(l$J, new("Length", 5L))
    expect_identical(l$JOld, new("Length", 10L))
    expect_identical(l$K, new("Length", 5L))
    expect_identical(l$L, new("Length", 1L))
    ## interaction
    spec <- DLM()
    beta <- rnorm(50)
    metadata <- new("MetaData",
                    nms = c("region", "time"),
                    dimtypes = c("state", "time"),
                    DimScales = list(new("Categories", dimvalues = letters[1:5]),
                                     new("Points", dimvalues = 1:10)))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(5, 10),
                                   dimnames = list(region = letters[1:5],
                                                   time = 1:10)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = TRUE,
                          margin = 1:2,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "DLMWithTrendNormZeroNoSeason")
    metadata.new <- new("MetaData",
                        nms = c("region", "time"),
                        dimtypes = c("state", "time"),
                        DimScales = list(new("Categories", dimvalues = letters[1:5]),
                            new("Points", dimvalues = 11:15)))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(5, 5),
                                   dimnames = list(region = letters[1:5],
                                                   time = 11:15)),
                             dimscales = c(time = "Points"))
    beta.new <- rnorm(25)
    l <- initialDLMAllPredict(prior = prior,
                              metadata = metadata.new,
                              along = "time",
                              margin = 1:2,
                              strucZeroArray = strucZeroArray)
    expect_identical(length(l$alphaDLM), 30L)
    expect_identical(l$iteratorState@indices, seq.int(from = 1, by = 5, length = 6))
    expect_identical(l$iteratorStateOld@indices, seq.int(from = 1, by = 5, length = 11))
    expect_identical(l$iteratorV@indices, seq.int(from = 1, by = 5, length = 5))
    expect_identical(l$J, new("Length", 25L))
    expect_identical(l$JOld, new("Length", 50L))
    expect_identical(l$K, new("Length", 5L))
    expect_identical(l$L, new("Length", 5L))
})

test_that("initialDLMNoTrend works", {
    initialDLMNoTrend <- demest:::initialDLMNoTrend
    set.seed(100)
    spec <- DLM(trend = NULL)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    l <- initialDLMNoTrend(spec,
                           metadata = metadata,
                           sY = NULL)
    expect_identical(length(l$aNoTrend), 10L)
    expect_identical(length(l$CNoTrend), 11L)
    expect_identical(length(l$mNoTrend), 11L)
    expect_identical(length(l$m0NoTrend), 1L)
    expect_identical(length(l$RNoTrend), 10L)
    expect_true(all(sapply(l$aNoTrend, length) == 1L))
    expect_true(all(sapply(l$CNoTrend, length) == 1L))
    expect_true(all(sapply(l$mNoTrend, length) == 1L))
    expect_true(all(sapply(l$m0NoTrend, length) == 1L))
    expect_true(all(sapply(l$RNoTrend, length) == 1L))
    expect_identical(l$CNoTrend[[1L]], 100)
    ## phi is 1, known
    spec <- DLM(trend = NULL, damp = NULL)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    l <- initialDLMNoTrend(spec,
                           metadata = metadata,
                           sY = NULL)
    expect_identical(l$CNoTrend[[1L]], 100)
})

test_that("initialDLMNoTrendPredict works", {
    initialDLMNoTrendPredict <- demest:::initialDLMNoTrendPredict
    initialPrior <- demest:::initialPrior
    set.seed(100)
    ## main effect
    spec <- DLM(trend = NULL)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          margin = 1L,
                          strucZeroArray = strucZeroArray,
                          isSaturated = TRUE)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    l <- initialDLMNoTrendPredict(prior = prior,
                                  metadata = metadata.new)
    expect_identical(length(l$aNoTrend), 5L)
    expect_identical(length(l$CNoTrend), 6L)
    expect_identical(length(l$mNoTrend), 6L)
    expect_identical(length(l$m0NoTrend), 1L)
    expect_identical(length(l$RNoTrend), 5L)
    expect_identical(l$CNoTrend[[1]], 0)
    ## interaction
    spec <- DLM(trend = NULL)
    beta <- rnorm(50)
    metadata <- new("MetaData",
                    nms = c("region", "time"),
                    dimtypes = c("state", "time"),
                    DimScales = list(new("Categories", dimvalues = letters[1:5]),
                        new("Points", dimvalues = 1:10)))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(5, 10),
                                   dimnames = list(region = letters[1:5],
                                                   time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          margin = 1:2,
                          strucZeroArray = strucZeroArray,
                          isSaturated = FALSE)
    expect_is(prior, "DLMNoTrendNormZeroNoSeason")
    metadata.new <- new("MetaData",
                        nms = c("region", "time"),
                        dimtypes = c("state", "time"),
                        DimScales = list(new("Categories", dimvalues = letters[1:5]),
                            new("Points", dimvalues = 11:15)))
    beta.new <- rnorm(25)
    l <- initialDLMNoTrendPredict(prior = prior,
                                  metadata = metadata.new)
    expect_identical(length(l$aNoTrend), 5L)
    expect_identical(length(l$CNoTrend), 6L)
    expect_identical(length(l$mNoTrend), 6L)
    expect_identical(length(l$m0NoTrend), 5L)
    expect_identical(length(l$RNoTrend), 5L)
    expect_identical(l$CNoTrend[[1]], 0)
})

test_that("initialDLMWithTrend works", {
    initialDLMWithTrend <- demest:::initialDLMWithTrend
    initialDLMAll <- demest:::initialDLMAll
    set.seed(100)
    spec <- DLM()
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    lAll <- initialDLMAll(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          margin = 1L,
                          strucZeroArray = strucZeroArray,
                          isSaturated = TRUE)
    l <- initialDLMWithTrend(spec,
                             beta = beta,
                             metadata = metadata,
                             sY = NULL,
                             lAll = lAll)
    expect_identical(l$ADelta, new("Scale", 1.0))
    expect_identical(length(l$aWithTrend), 10L)
    expect_identical(length(l$CWithTrend), 11L)
    expect_identical(l$CWithTrend[[1]], matrix(c(100, 0, 0, 1), nr = 2))
    expect_identical(length(l$DC), 11L)
    expect_identical(length(l$DCInv), 11L)
    expect_identical(length(l$DRInv), 10L)
    expect_identical(length(l$deltaDLM), 11L)
    expect_identical(length(l$mWithTrend), 11L)
    expect_identical(length(l$m0WithTrend), 1L)
    expect_identical(l$nuDelta, new("DegreesFreedom", 7))
    expect_identical(length(l$omegaDelta), 1L)
    expect_identical(l$omegaDeltaMax, new("Scale", qhalft(0.999, 7, 1)))
    expect_identical(length(l$RWithTrend), 10L)
    expect_identical(length(l$UC), 11L)
    expect_identical(length(l$UR), 10L)
    expect_identical(dim(l$WSqrt), c(2L, 2L))
    expect_identical(dim(l$WSqrtInvG), c(2L, 2L))
    expect_true(all(sapply(l$aWithTrend, length) == 2L))
    expect_true(all(sapply(l$CWithTrend, length) == 4L))
    expect_true(all(sapply(l$DC, length) == 4L))
    expect_true(all(sapply(l$DCInv, length) == 4L))
    expect_identical(l$DCInv[[1]], matrix(c(0.1, 0, 0, 1), nr = 2))
    expect_true(all(sapply(l$DRInv, length) == 4L))        
    expect_true(all(sapply(l$mWithTrend, length) == 2L))
    expect_true(all(sapply(l$m0WithTrend, length) == 2L))
    expect_true(all(sapply(l$RWithTrend, length) == 4L))
    expect_true(all(sapply(l$UC, length) == 4L))
    expect_true(all(sapply(l$UR, length) == 4L))
    expect_identical(l$ADelta0, new("Scale", 1))
    expect_identical(l$meanDelta0, new("Parameter", 0))
    ## mult = 0.5
    spec <- DLM(trend = Trend(scale = HalfT(mult = 0.5)))
    l <- initialDLMWithTrend(spec,
                             beta = beta,
                             metadata = metadata,
                             sY = NULL,
                             lAll = lAll)
    expect_identical(l$ADelta, new("Scale", 0.5))
    expect_identical(l$omegaDeltaMax, new("Scale", qhalft(0.999, 7, 0.5)))
    ## informative delta0
    spec <- DLM(trend = Trend(initial = Initial(mean = 0.05, sd = 0.1)))
    l <- initialDLMWithTrend(spec,
                             beta = beta,
                             metadata = metadata,
                             sY = NULL,
                             lAll = lAll)
    expect_equal(l$CWithTrend[[1]], diag(c(100, 0.01)))
    expect_identical(l$UC[[1]], diag(2))
    expect_identical(l$DC[[1]], diag(c(10, 0.1)))
    expect_identical(l$DCInv[[1]], diag(c(0.1, 10)))
    expect_identical(l$m0WithTrend[[1]], c(0, 0.05))
    expect_identical(l$ADelta0, new("Scale", 0.1))
    expect_identical(l$meanDelta0, new("Parameter", 0.05))
    ## level is NULL
    spec <- DLM(level = NULL)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    lAll <- initialDLMAll(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          strucZeroArray = strucZeroArray,
                          margin = 1L,
                          isSaturated = TRUE)
    l <- initialDLMWithTrend(spec,
                             beta = beta,
                             metadata = metadata,
                             sY = NULL,
                             lAll = lAll)
    expect_identical(l$hasLevel, new("LogicalFlag", FALSE))
    expect_true(is.finite(l$DCInv[[1]][1]))
})

test_that("initialDLMWithTrendPredict works", {
    initialDLMWithTrendPredict <- demest:::initialDLMWithTrendPredict
    initialPrior <- demest:::initialPrior
    set.seed(100)
    ## main effect
    spec <- DLM()
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          strucZeroArray = strucZeroArray,
                          margin = 1L,
                          isSaturated = TRUE)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    l <- initialDLMWithTrendPredict(prior = prior,
                                    metadata = metadata.new)
    expect_identical(length(l$aWithTrend), 5L)
    expect_identical(length(l$CWithTrend), 6L)
    expect_identical(length(l$DC), 6L)
    expect_identical(length(l$DCInv), 6L)
    expect_identical(length(l$DRInv), 5L)
    expect_identical(length(l$deltaDLM), 6L)
    expect_identical(length(l$mWithTrend), 6L)
    expect_identical(length(l$m0WithTrend), 1L)
    expect_identical(length(l$RWithTrend), 5L)
    expect_identical(l$CWithTrend[[1]], matrix(0, nr = 2, nc = 2))
    ## interaction
    spec <- DLM()
    beta <- rnorm(50)
    metadata <- new("MetaData",
                    nms = c("region", "time"),
                    dimtypes = c("state", "time"),
                    DimScales = list(new("Categories", dimvalues = letters[1:5]),
                        new("Points", dimvalues = 1:10)))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(5, 10),
                                   dimnames = list(region = letters[1:5],
                                                   time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          strucZeroArray = strucZeroArray,
                          margin = 1:2,
                          isSaturated = TRUE)
    expect_is(prior, "DLMWithTrendNormZeroNoSeason")
    metadata.new <- new("MetaData",
                        nms = c("region", "time"),
                        dimtypes = c("state", "time"),
                        DimScales = list(new("Categories", dimvalues = letters[1:5]),
                            new("Points", dimvalues = 11:15)))
    beta.new <- rnorm(25)
    l <- initialDLMWithTrendPredict(prior = prior,
                                    metadata = metadata.new)
    expect_identical(length(l$aWithTrend), 5L)
    expect_identical(length(l$CWithTrend), 6L)
    expect_identical(length(l$DC), 6L)
    expect_identical(length(l$DCInv), 6L)
    expect_identical(length(l$DRInv), 5L)
    expect_identical(length(l$deltaDLM), 30L)
    expect_identical(length(l$mWithTrend), 6L)
    expect_identical(length(l$m0WithTrend), 5L)
    expect_identical(length(l$RWithTrend), 5L)
    expect_identical(l$CWithTrend[[1]], matrix(0, nr = 2, nc = 2))
})

test_that("initialDLMSeason works", {
    initialDLMSeason <- demest:::initialDLMSeason
    set.seed(100)
    spec <- DLM(season = Season(n = 4))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    l <- initialDLMSeason(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
    expect_identical(l$ASeason, new("Scale", 1.0))
    expect_identical(length(l$aSeason), 10L)
    expect_identical(length(l$CSeason), 11L)
    expect_identical(length(l$mSeason), 11L)
    expect_identical(length(l$m0Season), 1L)
    expect_identical(l$nSeason, new("Length", 4L))
    expect_identical(l$nuSeason, new("DegreesFreedom", 7))
    expect_identical(length(l$omegaSeason), 1L)
    expect_identical(l$omegaSeasonMax, new("Scale", qhalft(0.999, 7, 1)))
    expect_identical(length(l$RSeason), 10L)
    expect_identical(length(l$s), 11L)
    expect_identical(length(l$s[[1L]]), 4L)
    expect_true(all(sapply(l$aSeason, length) == 4L))
    expect_true(all(sapply(l$CSeason, length) == 4L))
    expect_true(all(sapply(l$mSeason, length) == 4L))
    expect_true(all(sapply(l$m0Season, length) == 4L))
    expect_true(all(sapply(l$RSeason, length) == 4L))
    ## mult = 0.5
    spec <- DLM(season = Season(n = 4, scale = HalfT(mult = 0.5)))
    l <- initialDLMSeason(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL)
    expect_identical(l$ASeason, new("Scale", 0.5))
    expect_identical(l$omegaSeasonMax, new("Scale", qhalft(0.999, 7, 0.5)))
})

test_that("initialDLMSeasonPredict works", {
    initialDLMSeasonPredict <- demest:::initialDLMSeasonPredict
    initialPrior <- demest:::initialPrior
    set.seed(100)
    ## main effect
    spec <- DLM(season = Season(n = 4))
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          strucZeroArray = strucZeroArray,
                          margin = 1L,
                          isSaturated = TRUE)
    metadata.new <- new("MetaData",
                        nms = "time",
                        dimtypes = "time",
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    l <- initialDLMSeasonPredict(prior = prior,
                                 metadata = metadata.new)
    expect_identical(length(l$aSeason), 5L)
    expect_identical(length(l$CSeason), 6L)
    expect_identical(length(l$mSeason), 6L)
    expect_identical(length(l$m0Season), 1L)
    expect_identical(length(l$RSeason), 5L)
    expect_identical(length(l$s), 6L)
    expect_identical(l$CSeason[[1]], rep(0, 4))
    ## interaction
    spec <- DLM(season = Season(n = 2))
    beta <- rnorm(50)
    metadata <- new("MetaData",
                    nms = c("region", "time"),
                    dimtypes = c("state", "time"),
                    DimScales = list(new("Categories", dimvalues = letters[1:5]),
                                     new("Points", dimvalues = 1:10)))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(5, 10),
                                   dimnames = list(region = letters[1:5],
                                                   time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          margin = 1:2,
                          strucZeroArray = strucZeroArray,
                          isSaturated = TRUE)
    expect_is(prior, "DLMWithTrendNormZeroWithSeason")
    metadata.new <- new("MetaData",
                        nms = c("region", "time"),
                        dimtypes = c("state", "time"),
                        DimScales = list(new("Categories", dimvalues = letters[1:5]),
                                         new("Points", dimvalues = 11:15)))
    beta.new <- rnorm(25)
    l <- initialDLMSeasonPredict(prior = prior,
                                 metadata = metadata.new)
    expect_identical(length(l$aSeason), 5L)
    expect_identical(length(l$CSeason), 6L)
    expect_identical(length(l$mSeason), 6L)
    expect_identical(length(l$m0Season), 5L)
    expect_identical(length(l$RSeason), 5L)
    expect_identical(length(l$s), 30L)
    expect_identical(l$CSeason[[1]], rep(0, 2))
})

test_that("initialMixAll works", {
    initialMixAll <- demest:::initialMixAll
    initialPrior <- demest:::initialPrior
    set.seed(0)
    ## main effect
    spec <- Mix(weights = Weights(mean = 0))
    beta <- rnorm(400)
    strucZeroArray <- Counts(array(1L,
                                   dim = c(20, 2, 10),
                                   dimnames = list(age = 0:19,
                                                   sex = c("female", "male"),
                                                   time = 2001:2010)),
                             dimscales = c(time = "Points"))
    metadata <- new("MetaData",
                    nms = c("age", "sex", "time"),
                    dimtypes = c("age", "sex", "time"),
                    DimScales = list(new("Intervals", dimvalues = 0:20),
                                     new("Sexes", dimvalues = c("female", "male")),
                                     new("Points", dimvalues = 2001:2010)))
    l <- initialMixAll(spec,
                       beta = beta,
                       metadata = metadata,
                       sY = NULL,
                       isSaturated = TRUE,
                       margin = 1:3,
                       strucZeroArray = strucZeroArray)
    stopifnot(identical(l$allStrucZero, rep(FALSE, 20 * 2 * 10)))
    stopifnot(identical(l$AComponentWeightMix, new("Scale", 0.5)))
    stopifnot(identical(l$ALevelComponentWeightMix, new("Scale", 0.25)))
    stopifnot(identical(l$ATau, new("Scale", 0.25)))
    stopifnot(identical(l$AVectorsMix, new("Scale", 0.25)))
    stopifnot(identical(l$aMix, new("ParameterVector", rep(0, times = 9))))
    stopifnot(identical(length(l$alphaMix@.Data), l$J@.Data))
    stopifnot(all(l$alphaMix@.Data %in% l$prodVectors@.Data))
    stopifnot(identical(l$CMix, new("ParameterVector", rep(1, times = 10))))
    stopifnot(identical(length(l$componentWeightMix), 10L * 10L))
    stopifnot(identical(l$dimBeta, c(20L, 2L, 10L)))
    stopifnot(identical(l$foundIndexClassMaxPossibleMix,
                        new("LogicalFlag", TRUE)))
    stopifnot(identical(l$iAlong, 3L))
    stopifnot(identical(l$indexClassMaxMix, new("Counter", 10L)))
    stopifnot(identical(l$indexClassMaxPossibleMix,
                        new("Counter", max(l$indexClassMix))))
    stopifnot(identical(l$indexClassMaxUsedMix, new("Counter", max(l$indexClassMix))))
    stopifnot(identical(length(l$indexClassMix), 400L))
    stopifnot(all(l$indexClassMix@.Data %in% 1:10))
    stopifnot(identical(l$indexClassProbMix, new("ParameterVector", rep(0, 10))))
    stopifnot(is(l$iteratorProdVector, "MarginIterator"))
    stopifnot(all(sapply(l$iteratorsDimsMix, is, "SliceIterator")))
    stopifnot(identical(length(l$iteratorsDimsMix), 3L))
    stopifnot(identical(l$J@.Data, 400L))
    stopifnot(identical(length(l$latentComponentWeightMix), 400L * 10L))
    lcwm <- matrix(l$latentComponentWeightMix@.Data, nr = 400)
    icm <- l$indexClassMix@.Data
    s <- 1:10
    for (i in 1:400)
        stopifnot(all(lcwm[i, s < icm[i]] < 0))
    for (i in 1:400)
        stopifnot(lcwm[i, s == icm[i]] > 0)
    stopifnot(identical(length(l$latentWeightMix), 400L))
    lwm <- l$latentWeightMix
    wm <- matrix(l$weightMix, nr = 10)
    for (i in 1:400)
        stopifnot(lwm[i] <= wm[(i-1) %/% 40L + 1L, icm[i]])
    stopifnot(identical(length(l$levelComponentWeightMix), 10L * 10L))
    stopifnot(identical(l$mMix, new("ParameterVector", rep(0, 10))))
    stopifnot(identical(l$minLevelComponentWeight, -4))
    stopifnot(identical(l$maxLevelComponentWeight, 4))
    stopifnot(identical(l$nBetaNoAlongMix, 40L))
    stopifnot(identical(l$nuComponentWeightMix, new("DegreesFreedom", 7)))
    stopifnot(identical(l$nuLevelComponentWeightMix, new("DegreesFreedom", 7)))
    stopifnot(identical(l$nuTau, new("DegreesFreedom", 7)))
    stopifnot(identical(l$nuVectorsMix, new("DegreesFreedom", 7)))
    stopifnot(is(l$omegaComponentWeightMaxMix, "Scale"))
    stopifnot(is(l$omegaComponentWeightMix, "Scale"))
    stopifnot(is(l$omegaLevelComponentWeightMaxMix, "Scale"))
    stopifnot(is(l$omegaLevelComponentWeightMix, "Scale"))
    stopifnot(is(l$omegaVectorsMaxMix, "Scale"))
    stopifnot(is(l$omegaVectorsMix, "Scale"))    
    stopifnot(is(l$meanLevelComponentWeightMix, "Parameter"))
    stopifnot(l$phiMix > 0.8 && l$phiMix < 1)
    stopifnot(identical(l$posProdVectors1Mix, 400L))
    stopifnot(identical(l$posProdVectors2Mix, 40L))
    stopifnot(is(l$priorMeanLevelComponentWeightMix, "Parameter"))
    stopifnot(is(l$priorSDLevelComponentWeightMix, "Scale"))
    stopifnot(identical(length(l$prodVectorsMix@.Data), 20L * 2L * 10L))
    stopifnot(identical(l$RMix, new("ParameterVector", rep(1, 9))))
    stopifnot(identical(l$sumsWeightsMix, new("UnitIntervalVec", rep(0, 10))))
    stopifnot(is(l$tau, "Scale"))
    stopifnot(is(l$tauMax, "Scale"))
    stopifnot(identical(sapply(l$vectorsMix, length), c(20L * 10L, 2L * 10L, 0L)))
    stopifnot(is(l$weightMix, "UnitIntervalVec"))
    stopifnot(identical(length(l$weightMix), 10L * 10L))
    stopifnot(identical(l$XXMix, new("ParameterVector", rep(0, 10))))
    stopifnot(identical(l$yXMix, new("ParameterVector", rep(0, 10))))
})

test_that("initialMixAllPredict works", {
    initialMixAllPredict <- demest:::initialMixAllPredict
    initialPrior <- demest:::initialPrior
    set.seed(0)
    ## main effect
    spec <- Mix(weights = Weights(mean = -20))
    beta <- rnorm(400)
    metadata <- new("MetaData",
                    nms = c("age", "sex", "time"),
                    dimtypes = c("age", "sex", "time"),
                    DimScales = list(new("Intervals", dimvalues = 0:20),
                                     new("Sexes", dimvalues = c("female", "male")),
                                     new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(20, 2, 10),
                                   dimnames = dimnames(metadata)),
                             dimscales = c(age = "Intervals", time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = TRUE,
                          margin = 1:3,
                          strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = c("age", "sex", "time"),
                        dimtypes = c("age", "sex", "time"),
                        DimScales = list(new("Intervals", dimvalues = 0:20),
                                         new("Sexes", dimvalues = c("female", "male")),
                                         new("Points", dimvalues = 2011:2030)))    
    strucZeroArray <- Counts(array(1L,
                                   dim = c(20, 2, 20),
                                   dimnames = dimnames(metadata.new)),
                             dimscales = c(age = "Intervals", time = "Points"))
    l <- initialMixAllPredict(prior,
                              metadata = metadata.new,
                              along = 3L,
                              name = "age:sex:time",
                              margin = 1:3,
                              strucZeroArray = strucZeroArray)
    stopifnot(identical(l$aMix, new("ParameterVector", rep(0, times = 19))))
    stopifnot(identical(l$allStrucZero, rep(FALSE, 20*2*20)))
    stopifnot(identical(length(l$alphaMix@.Data), l$J@.Data))
    stopifnot(identical(l$CMix, new("ParameterVector", rep(1, times = 20))))
    stopifnot(identical(length(l$componentWeightMix), 20L * 10L))
    stopifnot(identical(l$latentWeightMix, new("UnitIntervalVec", rep(0, 800))))
    stopifnot(identical(l$dimBeta, c(20L, 2L, 20L)))
    stopifnot(identical(l$iAlong, 3L))
    stopifnot(identical(length(l$indexClassMix), 800L))
    stopifnot(is(l$iteratorProdVector, "MarginIterator"))
    stopifnot(all(sapply(l$iteratorsDimsMix, is, "SliceIterator")))
    stopifnot(identical(length(l$iteratorsDimsMix), 3L))
    stopifnot(identical(l$J@.Data, 800L))
    stopifnot(identical(length(l$latentComponentWeightMix), 800L * 10L))
    stopifnot(identical(length(l$levelComponentWeightMix), 20L * 10L))
    stopifnot(identical(l$mMix, new("ParameterVector", rep(0, 20))))
    stopifnot(identical(l$posProdVectors1Mix, 800L))
    stopifnot(identical(l$posProdVectors2Mix, 40L))
    stopifnot(identical(l$RMix, new("ParameterVector", rep(1, 19))))
    stopifnot(identical(l$sumsWeightsMix, new("UnitIntervalVec", rep(0, 20))))
    stopifnot(is(l$weightMix, "UnitIntervalVec"))
    stopifnot(identical(length(l$weightMix), 20L * 10L))
})

test_that("checkAndTidyMaxAttempt works", {
    checkAndTidyMaxAttempt <- demest:::checkAndTidyMaxAttempt
    expect_error(checkAndTidyMaxAttempt(maxAttempt = 1:2),
                 "'maxAttempt' does not have length 1")
    expect_error(checkAndTidyMaxAttempt(maxAttempt = "wrong"),
                 "'maxAttempt' is non-numeric")
    expect_error(checkAndTidyMaxAttempt(maxAttempt = as.integer(NA)),
                 "'maxAttempt' is missing")
    expect_error(checkAndTidyMaxAttempt(maxAttempt = 1.3),
                 "'maxAttempt' has a non-integer value")
    expect_error(checkAndTidyMaxAttempt(maxAttempt = 0),
                 "'maxAttempt' is non-positive")
})

test_that("initialRobust works", {
    initialRobust <- demest:::initialRobust
    initialDLMAll <- demest:::initialDLMAll
    makeU <- demest:::makeU
    set.seed(100)
    spec <- DLM(error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(2, 10),
                                   dimnames = list(sex = c("f", "m"),
                                                   time = 2001:2010)),
                             dimscales = c(time = "Points"))
    lAll <- initialDLMAll(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    set.seed(1)
    l <- initialRobust(spec,
                       lAll = lAll)
    set.seed(1)
    U <- makeU(nu = spec@nuBeta, A = lAll$ATau, n = lAll$J, allStrucZero = lAll$allStrucZero)
    expect_identical(l$nuBeta, spec@nuBeta)
    expect_identical(l$UBeta, U)
})

test_that("initialRobustPredict works", {
    initialRobustPredict <- demest:::initialRobustPredict
    initialPrior <- demest:::initialPrior
    makeU <- demest:::makeU
    set.seed(100)
    spec <- DLM(error = Error(robust = TRUE))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = 2001:2010)))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(time = 2001:2010)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    metadata.new <- new("MetaData",
                        nms = "time", 
                        dimtypes = "time", 
                        DimScales = list(new("Points", dimvalues = 2011:2015)))
    set.seed(1)
    allStrucZero <- rep(FALSE, 5)
    l <- initialRobustPredict(prior,
                              metadata = metadata.new,
                              allStrucZero = allStrucZero)
    set.seed(1)
    ans.obtained <- list(UBeta = makeU(nu = prior@nuBeta, A = prior@ATau, n = 5, allStrucZero = allStrucZero))
    expect_identical(l, ans.obtained)
})

test_that("checkAndTidyMeanOrProb works", {
    checkAndTidyMeanOrProb <- demest:::checkAndTidyMeanOrProb
    expect_identical(checkAndTidyMeanOrProb(0.5),
                     0.5)
    expect_identical(checkAndTidyMeanOrProb(1L),
                     1.0)
    expect_error(checkAndTidyMeanOrProb("a"),
                 "'mean' is not numeric")
    expect_error(checkAndTidyMeanOrProb(c(1, 1), name = "prob"),
                 "'prob' does not have length 1")
    expect_error(checkAndTidyMeanOrProb(as.numeric(NA)),
                 "'mean' is missing")
})

test_that("checkAndTidyMult works", {
    checkAndTidyMult <- demest:::checkAndTidyMult
    expect_identical(checkAndTidyMult(0.5,
                                      scale = new("SpecScale", NA),
                                      nameScale = "scale"),
                     new("Scale", 0.5))
    expect_identical(checkAndTidyMult(1L,
                                      scale = new("SpecScale", NA),
                                      nameScale = "scale"),
                     new("Scale", 1.0))
    expect_error(checkAndTidyMult("a",
                                  scale = new("SpecScale", NA),
                                  nameScale = "scale"),
                 "'mult' is not numeric")
    expect_error(checkAndTidyMult(c(1, 1),
                                  scale = new("SpecScale", NA),
                                  nameScale = "scale"),
                 "'mult' does not have length 1")
    expect_error(checkAndTidyMult(as.numeric(NA),
                                  scale = new("SpecScale", NA),
                                  nameScale = "scale"),
                 "'mult' is missing")
    expect_error(checkAndTidyMult(0,
                                  scale = new("SpecScale", NA),
                                  nameScale = "scale"),
                 "'mult' is non-positive")
    expect_warning(checkAndTidyMult(mult = 2,
                                    scale = new("SpecScale", 2),
                                    nameScale = "scale"),
                   "'mult' argument ignored when 'scale' argument supplied")
})

test_that("checkAndTidyNSeason works", {
    checkAndTidyNSeason <- demest:::checkAndTidyNSeason
    expect_identical(checkAndTidyNSeason(2),
                     new("Length", 2L))
    expect_identical(checkAndTidyNSeason(2L),
                     new("Length", 2L))
    expect_error(checkAndTidyNSeason(c(1, 1)),
                 "'n' does not have length 1")
    expect_error(checkAndTidyNSeason("a"),
                 "'n' is non-numeric")
    expect_error(checkAndTidyNSeason(as.numeric(NA)),
                 "'n' is missing")
    expect_error(checkAndTidyNSeason(1.3),
                 "'n' is not an integer")
    expect_error(checkAndTidyNSeason(1),
                 "'n' is less than 2")
})

test_that("extractResponse works", {
    extractResponse <- demest:::extractResponse
    expect_identical(extractResponse(age ~ income, separateNames = TRUE),
                     "age")
    expect_identical(extractResponse(age ~ income, separateNames = FALSE),
                     "age")
    expect_identical(extractResponse(age:sex ~ income),
                     "age:sex")
    expect_identical(extractResponse(age:sex ~ income, separateNames = TRUE),
                    c("age", "sex"))
    expect_identical(extractResponse(age:sex:region ~ income, separateNames = TRUE),
                     c("age", "sex", "region"))
    expect_error(extractResponse( ~ income, separateNames = TRUE),
                 "formula '~income' does not have a response")
})

test_that("hasResponse works", {
    hasResponse <- demest:::hasResponse
    expect_true(hasResponse(age ~ income))
    expect_false(hasResponse(~ income))
})


## RANDOM VARIATES #################################################################


test_that("dpoibin1 works when x < threshold", {
    dpoibin1 <- demest:::dpoibin1
    set.seed(100)
    for (i in 1:10) {
        x <- as.integer(rpois(n = 1, lambda = 10))
        prob <- runif(1, min = 0.5, max = 1)
        size <- as.integer(rpois(n = 1, lambda = 12))
        ans <- sum(dbinom(seq(from = 0L, to = x), size = size, prob = prob, log = FALSE) *
                   dpois(seq(from = x, to = 0L), lambda = size * (1 - prob), log = FALSE))
        expect_equal(dpoibin1(x = x, size = size,  prob = prob), ans)
        expect_equal(dpoibin1(x = x, size = size, prob = prob, useC = TRUE),
                     dpoibin1(x = x, size = size, prob = prob, useC = FALSE))
    }
})

test_that("dpoibin1 works when x > threshold", {
    dpoibin1 <- demest:::dpoibin1
    set.seed(100)
    for (i in 1:10) {
        x <- as.integer(rpois(n = 1, lambda = 10)) + 50L
        prob <- runif(1, min = 0.5, max = 1)
        size <- as.integer(abs(rnorm(1, mean = 12, sd = 2))) + 50L
        mean <- prob * floor(size) + (1 - prob) * size
        sd <- sqrt(prob * (1 - prob) * floor(size) + (1 - prob) * size)
        ans <- dnorm(x, mean = mean, sd = sd)
        expect_equal(dpoibin1(x = x, size = size, prob = prob), ans)
        expect_equal(dpoibin1(x = x, size = size, prob = prob, useC = TRUE),
                     dpoibin1(x = x, size = size, prob = prob, useC = FALSE))
    }
})

test_that("log argument for dpoibin1 works", {
    dpoibin1 <- demest:::dpoibin1
    expect_identical(dpoibin1(x = 10L, size = 11L, prob = 0.98, log = TRUE),
                     log(dpoibin1(x = 10L, size = 11L, prob = 0.98, log = FALSE)))
    expect_identical(dpoibin1(x = 1L, size = 1L, prob = 0.98, log = TRUE),
                     log(dpoibin1(x = 1L, size = 1L, prob = 0.98, log = FALSE)))
    expect_identical(dpoibin1(x = 3000L, size = 3000L, prob = 0.98, log = TRUE),
                     log(dpoibin1(x = 3000L, size = 3000L, prob = 0.98, log = FALSE)))
})

test_that("log argument for dpoibin1 works with C version", {
    dpoibin1 <- demest:::dpoibin1
    expect_equal(dpoibin1(x = 10L, size = 11L, prob = 0.98, log = TRUE, useC = FALSE),
                 (dpoibin1(x = 10L, size = 11L, prob = 0.98, log = TRUE, useC = TRUE)))
    expect_equal(dpoibin1(x = 1L, size = 1L, prob = 0.98, log = TRUE, useC = FALSE),
                 (dpoibin1(x = 1L, size = 1L, prob = 0.98, log = TRUE, useC = TRUE)))
    expect_equal(dpoibin1(x = 3000L, size = 3000L, prob = 0.98, log = TRUE, useC = FALSE),
                 (dpoibin1(x = 3000L, size = 3000L, prob = 0.98, log = TRUE, useC = TRUE)))
})

test_that("dpoibin1 throws correct errors", {
    dpoibin1 <- demest:::dpoibin1
    expect_error(dpoibin1(x = 1:2, prob = 0.98, size = 10L),
                 "'x' does not have length 1")
    expect_error(dpoibin1(x = 1L, size = 1.0, prob = 0.98),
                 "'size' does not have type \"integer\"")
    expect_error(dpoibin1(x = as.integer(NA), size = 10L, prob = 0.98),
                 "'x' is missing")
    expect_error(dpoibin1(x = 10L, prob = 0.98, size = -1L),
                 "'size' is negative")
    expect_error(dpoibin1(x = 10L, prob = c(0.1, 0.1), size = 10L),
                 "'prob' does not have length 1")
    expect_error(dpoibin1(x = 10L, prob = 0L, size = 10L),
                 "'prob' does not have type \"double\"")
    expect_error(dpoibin1(x = 10L, prob = as.numeric(NA), size = 10L),
                 "'prob' is missing")
    expect_error(dpoibin1(x = 10L, prob = -1.1, size = 10L),
                 "'prob' is negative")
    expect_error(dpoibin1(x = 10L, prob = 1.1, size = 10L),
                 "'prob' is greater than 1")
    expect_error(dpoibin1(x = 10L, prob = 0.98, size = 10L, log = c(TRUE, TRUE)),
                 "'log' does not have length 1")
    expect_error(dpoibin1(x = 10L, prob = 0.98, size = 10L, log = NA),
                 "'log' is missing")
})

test_that("invlogit1 gives valid answer", {
    invlogit1 <- demest:::invlogit1
    logit <- function(x) log(x / (1 - x))
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x <- runif(1)
        expect_equal(x, invlogit1(logit(x)))
    }
    expect_equal(invlogit1(1e6), 1)
    expect_equal(invlogit1(-1e6), 0)
    expect_error(invlogit1(1:2),
                 "'x' does not have length 1")
    expect_error(invlogit1("a"),
                 "'x' is non-numeric")
    expect_error(invlogit1(as.numeric(NA)),
                 "'x' is missing")
})


test_that("invlogit1 gives valid answer", {
    invlogit1 <- demest:::invlogit1
    logit <- function(x) log(x / (1 - x))
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x <- runif(1)
        expect_equal(x, invlogit1(logit(x)))
    }
    expect_equal(invlogit1(1e6), 1)
    expect_equal(invlogit1(-1e6), 0)
    expect_error(invlogit1(1:2),
                 "'x' does not have length 1")
    expect_error(invlogit1("a"),
                 "'x' is non-numeric")
    expect_error(invlogit1(as.numeric(NA)),
                 "'x' is missing")
})

test_that("R and C versions of invlogit1 give same answer", {
    invlogit1 <- demest:::invlogit1
    logit <- function(x) log(x / (1 - x)) 
    # input is negative 
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x <- logit(runif(n=1,min=0,max=0.5))
        ans.R <- invlogit1(x, useC = FALSE)
        ans.C <- invlogit1(x, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
    # input is positive 
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x <- logit(runif(n=1,min=0.5,max=1))
        ans.R <- invlogit1(x, useC = FALSE)
        ans.C <- invlogit1(x, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
    #input 0
    {
        seed <- 1
        set.seed(seed)
        x <- 0
        ans.R <- invlogit1(x, useC = FALSE)
        ans.C <- invlogit1(x, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("rhalftTrunc1 gives valid answer", {
    rhalftTrunc1 <- demest:::rhalftTrunc1
    for (seed in seq_len(100 * n.test)) {
        set.seed(seed)
        df <- runif(1, 0.1, 10)
        scale <- runif(1, 0.1, 10)
        max <- runif(1, scale, 10)
        ans <- rhalftTrunc1(df = df, scale = scale, max = max)
        expect_true(ans > 0)
        expect_true(ans < max)
        set.seed(seed + 1)
        ans.obtained <- rhalftTrunc1(df = df, scale = scale, max = Inf)
        set.seed(seed + 1)
        ans.expected <- rhalft(n = 1, df = df, scale = scale)
        expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of rhalftTrunc1 give same answer", {
    rhalftTrunc1 <- demest:::rhalftTrunc1
    for (seed in seq_len(100 * n.test)) {
        set.seed(seed)
        df <- runif(1, 0.1, 10)
        scale <- runif(1, 0.1, 10)
        max <- runif(1, scale, 10)
        set.seed(seed + 1)
        ans.R <- rhalftTrunc1(df = df, scale = scale, max = max, useC = FALSE)
        set.seed(seed + 1)
        ans.C <- rhalftTrunc1(df = df, scale = scale, max = max, useC = TRUE)
        expect_equal(ans.R, ans.C)
    }
})

test_that("rinvchisq1 gives valid answer", {
    rinvchisq1 <- demest:::rinvchisq1
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans1 <- rinvchisq1(df = 4, scale = 3)
        set.seed(seed)
        X <- rchisq(n = 1L, df = 4)
        ans2 <- 4 * 3 / X
        expect_identical(ans1, ans2)
    }
    ## scale is 0
    ans.obtained <- rinvchisq1(df = 4, scale = 0)
    ans.expected <- 0
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of rinvchisq1 give same answer", {
    rinvchisq1 <- demest:::rinvchisq1
    for (seed in seq_len(max(100,n.test*10))) {
        df <- as.double(rpois(n = 1, lambda = 4) + 1)
        scale <- rgamma(n = 1, shape = 1, rate = 0.2)
        set.seed(seed)
        ans.R <- rinvchisq1(df = df, scale = scale, useC = FALSE)
        set.seed(seed)
        ans.C <- rinvchisq1(df = df, scale = scale, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
    ## scale is 0
    ans.R <- rinvchisq1(df = 4, scale = 0, useC = FALSE)
    ans.C <- rinvchisq1(df = 4, scale = 0, useC = TRUE)
    expect_identical(ans.R, ans.C)
})

test_that("rcateg1 gives valid answer", {
    rcateg1 <- demest:::rcateg1
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        cumProb <- c(0.2, 0.3, 0.8, 1)
        ans <- replicate(n = 10000, rcateg1(cumProb = cumProb))
        t <- unname(prop.table(table(ans)))
        expect_equal(cumsum(t), cumProb, tol = 0.02)
        expect_equal(rcateg1(cumProb = 1), 1)
    }
})

test_that("R and C versions of rcateg1 give same answer", {
    rcateg1 <- demest:::rcateg1
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        cumProb <- cumsum(prop.table(runif(n = 5)))
        set.seed(seed + 1)
        ans.R <- rcateg1(cumProb = cumProb, useC = FALSE)
        set.seed(seed + 1)
        ans.C <- rcateg1(cumProb = cumProb, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("rnormTruncated gives valid answer", {
    rnormTruncated <- demest:::rnormTruncated
    for (seed in seq_len(n.test)) {
        ## limits non-finite
        mean <- as.double(1:10)
        sd <-  as.double(10:1)
        set.seed(seed + 1)
        ans.obtained <- rnormTruncated(n = 10L, mean = mean, sd = sd,
                                       lower = -Inf, upper = Inf, maxAttempt = 100L,
                                       tolerance = 1e-5, uniform = TRUE)
        set.seed(seed + 1)
        ans.expected <- rnorm(n = 10L, mean = mean, sd = sd)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
        ## all within range
        ans <- rnormTruncated(n = 10L, mean = mean, sd = sd,
                              lower = -2, upper = 2, maxAttempt = 100L,
                              tolerance = 1e-5, uniform = TRUE)
        expect_true(all(ans > -2))
        expect_true(all(ans < 2))
        ## stops when nAttempt exceeded if uniform is FALSE
        expect_error(rnormTruncated(n = 10L, mean = mean, sd = sd,
                                    lower = 1000000, upper = 1000001, maxAttempt = 5L,
                                    tolerance = 1e-5, uniform = FALSE),
                     "failed to generate value within specified range")
    }
})

test_that("R and C versions of rnormTruncated give same answer", {
    rnormTruncated <- demest:::rnormTruncated
    for (seed in seq_len(n.test)) {
        ## limits non-finite
        mean <- rnorm(10)
        sd <-  rbeta(n = 10, shape1 = 2, shape2 = 2)
        lower <- runif(1, min = -10, max = 0)
        upper <- lower + runif(n = 1, min = 1, max = 20)
        set.seed(seed + 1)
        ans.R <- rnormTruncated(n = 10L, mean = mean, sd = sd,
                                lower = lower, upper = upper, maxAttempt = 1000L,
                                tolerance = 1e-5, uniform = TRUE, useC = FALSE)
        set.seed(seed + 1)
        ans.C <- rnormTruncated(n = 10L, mean = mean, sd = sd,
                                lower = lower, upper = upper, maxAttempt = 1000L,
                                tolerance = 1e-5, uniform = TRUE, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
        ## C version stops when nAttempt exceeded if uniform is FALSE
        expect_error(rnormTruncated(n = 10L, mean = mean, sd = sd,
                                    lower = 1000000, upper = 1000001, maxAttempt = 5L,
                                    tolerance = 1e-5, uniform = FALSE, useC = TRUE),
                     "failed to generate value within specified range")
    }
})

if (test.extended) {
    test_that("rnormIntTrunc1 gives valid answer", {
        rnormIntTrunc1 <- demest:::rnormIntTrunc1
        for (seed in seq_len(n.test)) {
            ## limits non-finite
            for (i in seq(100, 200, 10)) {
                mean <- as.double(i)
                sd <-  sqrt(i)
                set.seed(seed + 1)
                ans.obtained <- rnormIntTrunc1(mean = mean, sd = sd)
                set.seed(seed + 1)
                ans.expected <- rnorm(n = 1L, mean = mean, sd = sd)
                expect_equal(ans.obtained, ans.expected, tol = 0.01)
                expect_true(is.integer(ans.obtained))
            }
            ## all within range
            for (i in seq_len(10)) {
                ans <- rnormIntTrunc1(mean = 0, sd = 50, lower = -200L, upper = 200L)
                expect_true(ans >= -200L)
                expect_true(ans <= 200L)
                expect_true(is.integer(ans))
            }
            ## check distribution
            ans <- numeric(10000)
            for (i in seq_len(10000))
                ans[i] <- rnormIntTrunc1(sd = 100000, lower = 0L)
            true_mean <- 100000/(sqrt(2*pi))/(0.5)
            expect_equal(mean(ans), true_mean, tol = 0.02)
            ans <- numeric(10000)
            for (i in seq_len(10000))
                ans[i] <- rnormIntTrunc1(sd = 100000, upper = 0L)
            expect_equal(mean(ans), -true_mean, tol = 0.02)
        }
    })
}

test_that("R and C versions of rnormIntTrunc1 give same answer", {
    rnormIntTrunc1 <- demest:::rnormIntTrunc1
    for (seed in seq_len(n.test)) {
        ## no limits
        for (i in seq(100, 200, 10)) {
            mean <- as.double(i)
            sd <-  sqrt(i)
            set.seed(seed + 1)
            ans.R <- rnormIntTrunc1(mean = mean, sd = sd)
            set.seed(seed + 1)
            ans.C <- rnormIntTrunc1(mean = mean, sd = sd)
            if (test.identity)
                expect_identical(ans.R, ans.C)
            else
                expect_equal(ans.R, ans.C)
            expect_true(is.integer(ans.C))
        }
        ## upper limit
        for (i in seq(100, 200, 10)) {
            mean <- as.double(i)
            sd <-  sqrt(i)
            set.seed(seed + 1)
            ans.R <- rnormIntTrunc1(mean = mean, sd = sd, upper = 200L)
            set.seed(seed + 1)
            ans.C <- rnormIntTrunc1(mean = mean, sd = sd, upper = 200L)
            if (test.identity)
                expect_identical(ans.R, ans.C)
            else
                expect_equal(ans.R, ans.C)
            expect_true(is.integer(ans.C))
        }
        ## lower limit
        for (i in seq(100, 200, 10)) {
            mean <- as.double(i)
            sd <-  sqrt(i)
            set.seed(seed + 1)
            ans.R <- rnormIntTrunc1(mean = mean, sd = sd, lower = -200L)
            set.seed(seed + 1)
            ans.C <- rnormIntTrunc1(mean = mean, sd = sd, lower = -200L)
            if (test.identity)
                expect_identical(ans.R, ans.C)
            else
                expect_equal(ans.R, ans.C)
            expect_true(is.integer(ans.C))
        }
        ## lower and upper limits
        for (i in seq(100, 200, 10)) {
            mean <- as.double(i)
            sd <-  sqrt(i)
            set.seed(seed + 1)
            ans.R <- rnormIntTrunc1(mean = mean, sd = sd, lower = -200L, upper = 200L)
            set.seed(seed + 1)
            ans.C <- rnormIntTrunc1(mean = mean, sd = sd, lower = -200L, upper = 200L)
            if (test.identity)
                expect_identical(ans.R, ans.C)
            else
                expect_equal(ans.R, ans.C)
            expect_true(is.integer(ans.C))
        }
    }
})

test_that("rtnorm1 gives valid answer", {
    rtnorm1 <- demest:::rtnorm1
    for (seed in seq_len(n.test)) {
        ## limits non-finite
        for (i in seq_len(10)) {
            mean <- as.double(i)
            sd <-  11 - i
            set.seed(seed + 1)
            ans.obtained <- rtnorm1(mean = mean, sd = sd)
            set.seed(seed + 1)
            ans.expected <- rnorm(n = 1L, mean = mean, sd = sd)
            if (test.identity)
                expect_identical(ans.obtained, ans.expected)
            else
                expect_equal(ans.obtained, ans.expected)
        }
        ## all within range
        for (i in seq_len(10)) {
            ans <- rtnorm1(lower = -2, upper = 2)
            expect_true(ans > -2)
            expect_true(ans < 2)
        }
        ## check distribution
        ans <- numeric(1000)
        for (i in seq_len(1000))
            ans[i] <- rtnorm1(lower = 0)
        true_mean <- 1/(sqrt(2*pi))/(0.5)
        expect_equal(round(mean(ans),1), round(true_mean,1))
        ans <- numeric(1000)
        for (i in seq_len(1000))
            ans[i] <- rtnorm1(upper = 0)
        expect_equal(round(mean(ans),1), round(-true_mean,1))
    }
})


test_that("R and C versions of rtnorm1 give same answer", {
    rtnorm1 <- demest:::rtnorm1
    for (seed in seq_len(n.test)) {
        ## limits non-finite
        for (i in seq_len(10)) {
            mean <- as.double(i)
            sd <-  11 - i
            set.seed(seed + 1)
            ans.R <- rtnorm1(mean = mean, sd = sd, useC = FALSE)
            set.seed(seed + 1)
            ans.C <- rtnorm1(mean = mean, sd = sd, useC = TRUE)
            if (test.identity)
                expect_identical(ans.R, ans.C)
            else
                expect_equal(ans.R, ans.C)
        }
        ## limits finite
        for (i in seq_len(10)) {
            mean <- as.double(i)
            sd <-  11 - i
            set.seed(seed + 1)
            ans.R <- rtnorm1(mean = mean, sd = sd, lower = -1, upper = 0.5, useC = FALSE)
            set.seed(seed + 1)
            ans.C <- rtnorm1(mean = mean, sd = sd, lower = -1, upper = 0.5, useC = TRUE)
            if (test.identity)
                expect_identical(ans.R, ans.C)
            else
                expect_equal(ans.R, ans.C)
        }
        ## limits finite, force cases where some l > a
        for (i in seq_len(10)) {
            mean <- as.double(i)
            sd <-  11 - i
            set.seed(seed + 1)
            lower <- mean + i/10*sd + mean + 0.1
            upper <- lower + i/10
            ans.R <- rtnorm1(mean = mean, sd = sd, lower = lower, upper = upper, useC = FALSE)
            set.seed(seed + 1)
            ans.C <- rtnorm1(mean = mean, sd = sd, lower = lower, upper = upper, useC = TRUE)
            if (test.identity)
                expect_identical(ans.R, ans.C)
            else
                expect_equal(ans.R, ans.C)
        }
    }
})

test_that("rpoisTrunc1 gives valid answer", {
    rpoisTrunc1 <- demest:::rpoisTrunc1
    for (seed in seq_len(n.test)) {
        ## no limits
        set.seed(seed)
        lambda <- runif(1, 0, 20)
        set.seed(seed + 1)
        ans.obtained <- rpoisTrunc1(lambda = lambda, lower = 0L, upper = NA_integer_,
                                    maxAttempt = 1L)
        set.seed(seed + 1)
        ans.expected <- rpois(n = 1L, lambda = lambda)
        expect_identical(ans.obtained, ans.expected)
        ## within range
        ans <- rpoisTrunc1(lambda = lambda, lower = 2L, upper = 10L,
                           maxAttempt = 100L)
        expect_true((is.na(ans)) || ((ans >= 2L) && ans <= 10L))
        ## returns 0 if upper = 0
        ans <- rpoisTrunc1(lambda = 1000, lower = -1L, upper = 0L,
                           maxAttempt = 1L)
        expect_identical(ans, 0L)
        ## returns NA_integer_ if failed
        ans <- rpoisTrunc1(lambda = 1000, lower = -1L, upper = 1L,
                           maxAttempt = 1L)
        expect_identical(ans, NA_integer_)
        ## lower is NA gives same answer as lower is 0
        set.seed(seed + 1)
        ans.obtained <- rpoisTrunc1(lambda = lambda, lower = NA_integer_,
                                    upper = 100L, maxAttempt = 100L)
        set.seed(seed + 1)
        ans.expected <- rpoisTrunc1(lambda = lambda, lower = 0L,
                                    upper = 100L, maxAttempt = 100L)
        expect_identical(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of rpoisTrunc1 give same answer", {
    rpoisTrunc1 <- demest:::rpoisTrunc1
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        lambda <- runif(1, 0, 15)
        lower <- as.integer(rpois(n = 1, lambda = 5))
        if (runif(1) < 0.8)
            upper <- lower + as.integer(rpois(1, lambda = 10))
        else
            upper <- NA_integer_
        set.seed(seed + 1)
        ans.R <- rpoisTrunc1(lambda = lambda, lower = lower, upper = upper,
                             maxAttempt = 10L, useC = FALSE)
        set.seed(seed + 1)
        ans.C <- rpoisTrunc1(lambda = lambda, lower = lower, upper = upper,
                             maxAttempt = 10L, useC = TRUE)
        expect_identical(ans.R, ans.C)
        set.seed(seed + 1)
        ans.R <- rpoisTrunc1(lambda = lambda, lower = NA_integer_, upper = upper,
                             maxAttempt = 10L, useC = FALSE)
        set.seed(seed + 1)
        ans.C <- rpoisTrunc1(lambda = lambda, lower = NA_integer_, upper = upper,
                             maxAttempt = 10L, useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
})


test_that("More tests for R and C versions of rpoisTrunc1 give same answer", {
    rpoisTrunc1 <- demest:::rpoisTrunc1
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        lambda <- runif(1, 0, 15)
            lower <- as.integer(rpois(n = 1, lambda = 5))
            upper <- NA_integer_ ## non-finite upper
            set.seed(seed + 1)
            ans.R <- rpoisTrunc1(lambda = lambda, lower = lower, upper = upper,
                                 maxAttempt = 10L, useC = FALSE)
            set.seed(seed + 1)
            ans.C <- rpoisTrunc1(lambda = lambda, lower = lower, upper = upper,
                                 maxAttempt = 10L, useC = TRUE)
            expect_identical(ans.R, ans.C)
            upper <- lower ## upper == lower
            set.seed(seed + 1)
            ans.R <- rpoisTrunc1(lambda = lambda, lower = lower, upper = upper,
                                 maxAttempt = 10L, useC = FALSE)
            set.seed(seed + 1)
            ans.C <- rpoisTrunc1(lambda = lambda, lower = lower, upper = upper,
                                 maxAttempt = 10L, useC = TRUE)
            expect_identical(ans.R, ans.C)
            upper <- lower + as.integer(rpois(1, lambda = 10)) ## upper > lower
            set.seed(seed + 1)
            ans.R <- rpoisTrunc1(lambda = lambda, lower = lower, upper = upper,
                                 maxAttempt = 10L, useC = FALSE)
            set.seed(seed + 1)
            ans.C <- rpoisTrunc1(lambda = lambda, lower = lower, upper = upper,
                                 maxAttempt = 10L, useC = TRUE)
            expect_identical(ans.R, ans.C)            
    }
})

test_that("rhalft works", {
    ## scale = 1
    df <- rpois(10, lambda = 3) + 1
    scale <- runif(10, 0.2, 2)
    set.seed(1)
    ans.obtained <- rhalft(10, df = df, scale = scale)
    set.seed(1)
    ans.expected <- abs(scale * rt(10, df = df))
    expect_equal(ans.obtained, ans.expected)
    ## scale numeric
    expect_error(rhalft(1,  7, "wrong"),
                 "'scale' is non-numeric")
    ## scale positive
    expect_error(rhalft(1, 7, -1),
                 "'scale' is negative")
})

test_that("qhalft works", {
    ## scale = 1
    p <- runif(10)
    df <- rpois(10, lambda = 3) + 1
    ans.obtained <- qhalft(p, df = df, scale = 1)
    ans.expected <- qt(p = (p+1)/2, df = df)
    expect_equal(ans.obtained, ans.expected)
    ## scale = 0.5
    p <- runif(10)
    df <- rpois(10, lambda = 3) + 1
    ans.obtained <- qhalft(p, df = df, scale = 0.5)
    ans.expected <- 0.5 * qt(p = (p+1)/2, df = df)
    expect_equal(ans.obtained, ans.expected)
    ## scale = 0
    p <- runif(10)
    df <- rpois(10, lambda = 3) + 1
    ans.obtained <- qhalft(p, df = df, scale = 0)
    ans.expected <- rep(0, 10)
    expect_equal(ans.obtained, ans.expected)
    ## scale numeric
    expect_error(qhalft(0.5, 7, "wrong"),
                 "'scale' is non-numeric")
    ## scale non-negative
    expect_error(qhalft(0.5, 7, -1),
                 "'scale' is negative")
})

test_that("phalft works", {
    ## scale = 1
    q <- runif(10)
    df <- rpois(10, lambda = 3) + 1
    scale <- runif(10, 0.2, 2)
    ans.obtained <- phalft(q, df = df, scale = scale)
    ans.expected <- 2 * (pt(q = q/scale, df = df) - 0.5)
    expect_equal(ans.obtained, ans.expected)
    ## scale numeric
    expect_error(phalft(0.5, 7, "wrong"),
                 "'scale' is non-numeric")
    ## scale positive
    expect_error(phalft(0.5, 7, -1),
                 "'scale' is negative")
})

test_that("dhalft works", {
    ## scale = 1
    x <- runif(10)
    df <- rpois(10, lambda = 3) + 1
    scale <- runif(10, 0.2, 2)
    ans.obtained <- dhalft(x, df = df, scale = scale)
    ans.expected <- (2/scale) * dt(x = x/scale, df = df)
    expect_equal(ans.obtained, ans.expected)
    ## scale numeric
    expect_error(dhalft(0.5, 7, "wrong"),
                 "'scale' is non-numeric")
    ## scale positive
    expect_error(dhalft(0.5, 7, -1),
                 "'scale' is negative")
})


## ALONG ITERATOR ################################################################


test_that("centerA gives valid answer", {
    centerA <- demest:::centerA
    AlongIterator <- demest:::AlongIterator
    ## dim = 10L, iAlong = 1L
    vec <- rnorm(10)
    expect_identical(centerA(vec, iterator = AlongIterator(dim = 10L, iAlong = 1L)),
                     vec - mean(vec))
    ## dim = c(4L, 5L), iAlong = 1L
    vec <- rnorm(20)
    expect_identical(centerA(vec, iterator = AlongIterator(dim = c(4L, 5L), iAlong = 1L)),
                     as.numeric(apply(array(vec, dim = c(4, 5)), 2, function(x) x - mean(x))))
    ## dim = c(4L, 5L), iAlong = 2L
    vec <- rnorm(20)
    expect_identical(centerA(vec, iterator = AlongIterator(dim = c(4L, 5L), iAlong = 2L)),
                     as.numeric(t(apply(array(vec, dim = c(4, 5)), 1, function(x) x - mean(x)))))
    ## dim = c(4L, 5L, 3L), iAlong = 1L
    vec <- rnorm(60)
    expect_identical(centerA(vec, iterator = AlongIterator(dim = c(4L, 5L, 3L), iAlong = 1L)),
                     as.numeric(apply(array(vec, dim = c(4, 5, 3)),
                                      2:3,
                                      function(x) x - mean(x))))
    ## dim = c(4L, 5L, 3L), iAlong = 2L
    vec <- rnorm(60)
    expect_identical(centerA(vec, iterator = AlongIterator(dim = c(4L, 5L, 3L), iAlong = 2L)),
                     as.numeric(aperm(apply(array(vec, dim = c(4, 5, 3)),
                                            c(1, 3),
                                            function(x) x - mean(x)),
                                      c(2, 1, 3))))
    ## dim = c(4L, 5L, 3L), iAlong = 3L
    vec <- rnorm(60)
    expect_identical(centerA(vec, iterator = AlongIterator(dim = c(4L, 5L, 3L), iAlong = 3L)),
                     as.numeric(aperm(apply(array(vec, dim = c(4, 5, 3)),
                                            1:2,
                                            function(x) x - mean(x)),
                                      c(2, 3, 1))))
})

## sometimes gives equal but not identical, sometimes identical
test_that("R and C versions of centerA give same answer", {
    centerA <- demest:::centerA
    AlongIterator <- demest:::AlongIterator
    n.dim <- round(runif(n = 1, min = 1, max = 4))
    dim <- as.integer(round(runif(n = n.dim, min = 1, max = 10)))
    iAlong <- sample.int(n = n.dim, size = 1)
    vec <- rnorm(n = prod(dim))
    iterator <- AlongIterator(dim = dim, iAlong = iAlong)
    ans.R <- centerA(vec = vec, iterator = iterator, useC = FALSE)
    ans.C <- centerA(vec = vec, iterator = iterator, useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
})





## UPDATING ###########################################################################

test_that("betaHat gives valid answer with prior of class ExchFixed", {
    betaHat <- demest:::betaHat
    initialPrior <- demest:::initialPrior
    ## intercept
    spec <- ExchFixed(mean = -3, sd = 3)
    beta <- rnorm(n = 1)
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = NULL,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 0L,
                          strucZeroArray = 1L)
    expect_is(prior, "ExchFixed")
    ans.obtained <- betaHat(prior)
    ans.expected <- -3
    expect_identical(ans.obtained, ans.expected)
    ## non-intercept
    spec <- ExchFixed(sd = 3)
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(c(0L, rep(1L, 9)),
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "ExchFixed")
    ans.obtained <- betaHat(prior)
    ans.expected <- c(NA, rep(0, 9))
    expect_identical(ans.obtained, ans.expected)    
})

test_that("R and C versions of betaHat give same answer with ExchFixed", {
    betaHat <- demest:::betaHat
    initialPrior <- demest:::initialPrior
    ## intercept
    for (seed in seq_len(n.test)) {
        spec <- ExchFixed(mean = -3, sd = 3)
        beta <- rnorm(n = 1)
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = NULL,
                              sY = NULL,
                              isSaturated = FALSE,
                              margin = 0L,
                              strucZeroArray = 1L)
        expect_is(prior, "ExchFixed")
        ans.R <- betaHat(prior, useC = FALSE)
        ans.C <- betaHat(prior, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
    ## non-intercept
    for (seed in seq_len(n.test)) {
        spec <- ExchFixed(sd = 3)
        beta <- rnorm(10)
        strucZeroArray <- Counts(array(c(0L, rep(1L, 9)),
                                       dim = 10,
                                       dimnames = list(region = letters[1:10])))
        metadata <- new("MetaData",
                        nms = "region",
                        dimtypes = "state",
                        DimScales = list(new("Categories", dimvalues = letters[1:10])))
        prior <- initialPrior(spec,
                              beta = beta,
                              metadata = metadata,
                              sY = NULL,
                              isSaturated = FALSE,
                              margin = 1,
                              strucZeroArray = strucZeroArray)
        expect_is(prior, "ExchFixed")
        ans.R <- betaHat(prior, useC = FALSE)
        ans.C <- betaHat(prior, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("betaHat gives valid answer with prior of class Exch - no covariates", {
    betaHat <- demest:::betaHat
    initialPrior <- demest:::initialPrior
    spec <- Exch()
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    ans.obtained <- betaHat(prior)
    ans.expected <- rep(0, 10)
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of betaHat give same answer with prior of class Exch - no covariates", {
    betaHat <- demest:::betaHat
    initialPrior <- demest:::initialPrior
    spec <- Exch()
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    ans.R <- betaHat(prior, useC = FALSE)
    ans.C <- betaHat(prior, useC = TRUE)
    expect_identical(ans.R, ans.C)
})

test_that("betaHat gives valid answer with prior of class Exch - with covariates", {
    betaHat <- demest:::betaHat
    initialPrior <- demest:::initialPrior
    data <- data.frame(region = letters[10:1], income = rnorm(10))
    spec <- Exch(covariates = Covariates(mean ~ income, data = data))
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    ans.obtained <- betaHat(prior)
    ans.expected <- unname(drop(prior@Z %*% prior@eta@.Data))
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of betaHat give same answer with prior of class Exch - with covariates", {
    betaHat <- demest:::betaHat
    initialPrior <- demest:::initialPrior
    data <- data.frame(region = letters[10:1], income = rnorm(10))
    spec <- Exch(covariates = Covariates(mean ~ income, data = data))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    ans.R <- betaHat(prior, useC = FALSE)
    ans.C <- betaHat(prior, useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
})

test_that("betaHat gives valid answer with prior of class DLM - no season", {
    betaHat <- demest:::betaHat
    initialPrior <- demest:::initialPrior
    spec <- DLM()
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    ans.obtained <- betaHat(prior)
    ans.expected <- prior@alphaDLM@.Data[-1]
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of betaHat give same answer with prior of class DLM - no season", {
    betaHat <- demest:::betaHat
    initialPrior <- demest:::initialPrior
    spec <- DLM()
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    ans.R <- betaHat(prior, useC = FALSE)
    ans.C <- betaHat(prior, useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
})

test_that("betaHat gives valid answer with prior of class DLM - with season", {
    betaHat <- demest:::betaHat
    initialPrior <- demest:::initialPrior
    spec <- DLM(season = Season(n = 12))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    ans.obtained <- betaHat(prior)
    ans.expected <- prior@alphaDLM@.Data[-1] + sapply(prior@s@.Data[-1], function(x) x[1])
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of betaHat give same answer with prior of class DLM - with season", {
    betaHat <- demest:::betaHat
    initialPrior <- demest:::initialPrior
    spec <- DLM(season = Season(n = 12))
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    ans.R <- betaHat(prior, useC = FALSE)
    ans.C <- betaHat(prior, useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
})

test_that("betaHat gives valid answer with prior of class Mix", {
    betaHat <- demest:::betaHat
    initialPrior <- demest:::initialPrior
    beta <- rnorm(200)
    metadata <- new("MetaData",
                    nms = c("time", "reg", "age"),
                    dimtypes = c("time", "state", "age"),
                    DimScales = list(new("Points", dimvalues = 2001:2010),
                                     new("Categories", dimvalues = c("a", "b")),
                                     new("Intervals", dimvalues = as.numeric(0:10))))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(10, 2, 10),
                                   dimnames = list(time = 2001:2010,
                                                   reg = c("a", "b"),
                                                   age = 0:9)),
                             dimscales = c(time = "Points", age = "Intervals"))
    spec <- Mix()
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          margin = 1:3,
                          strucZeroArray = strucZeroArray)
    ans.obtained <- betaHat(prior)
    ans.expected <- prior@alphaMix@.Data
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of betaHat give same answer with prior of class Mix", {
    betaHat <- demest:::betaHat
    initialPrior <- demest:::initialPrior
    beta <- rnorm(200)
    metadata <- new("MetaData",
                    nms = c("time", "reg", "age"),
                    dimtypes = c("time", "state", "age"),
                    DimScales = list(new("Points", dimvalues = 2001:2010),
                                     new("Categories", dimvalues = c("a", "b")),
                                     new("Intervals", dimvalues = as.numeric(0:10))))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(10, 2, 10),
                                   dimnames = list(time = 2001:2010,
                                                   reg = c("a", "b"),
                                                   age = 0:9)),
                             dimscales = c(time = "Points", age = "Intervals"))
    spec <- Mix()
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          multScale = 1,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray,
                          margin = 1:3)
    ans.R <- betaHat(prior, useC = FALSE)
    ans.C <- betaHat(prior, useC = TRUE)
    expect_identical(ans.R, ans.C)
})


test_that("betaHat works with KnownCertain", {
    betaHat <- demest:::betaHat
    initialPrior <- demest:::initialPrior
    mean <- ValuesOne(1:10, labels = letters[1:10], name = "region")
    spec <- Known(mean)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "KnownCertain")
    ans.obtained <- betaHat(prior)
    ans.expected <- as.numeric(mean)
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of betaHat give same answer with KnownCertain", {
    betaHat <- demest:::betaHat
    initialPrior <- demest:::initialPrior
    mean <- ValuesOne(1:10, labels = letters[1:10], name = "region")
    spec <- Known(mean)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "KnownCertain")
    ans.R <- betaHat(prior, useC = FALSE)
    ans.C <- betaHat(prior, useC = TRUE)
    expect_identical(ans.R, ans.C)
})

test_that("betaHat works with KnownUncertain", {
    betaHat <- demest:::betaHat
    initialPrior <- demest:::initialPrior
    mean <- ValuesOne(1:10, labels = letters[1:10], name = "region")
    spec <- Known(mean, sd = 1)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "KnownUncertain")
    ans.obtained <- betaHat(prior)
    ans.expected <- as.numeric(mean)
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of betaHat give same answer with KnownUncertain", {
    betaHat <- demest:::betaHat
    initialPrior <- demest:::initialPrior
    mean <- ValuesOne(1:10, labels = letters[1:10], name = "region")
    spec <- Known(mean, sd = 1)
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "KnownUncertain")
    ans.R <- betaHat(prior, useC = FALSE)
    ans.C <- betaHat(prior, useC = TRUE)
    expect_identical(ans.R, ans.C)
})

## Zero

test_that("betaHat works with Zero", {
    betaHat <- demest:::betaHat
    initialPrior <- demest:::initialPrior
    spec <- Zero()
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "Zero")
    ans.obtained <- betaHat(prior)
    ans.expected <- rep(0, 10)
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of betaHat give same answer with Zero", {
    betaHat <- demest:::betaHat
    initialPrior <- demest:::initialPrior
    spec <- Zero()
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "Zero")
    ans.R <- betaHat(prior, useC = FALSE)
    ans.C <- betaHat(prior, useC = TRUE)
    expect_identical(ans.R, ans.C)
})

test_that("betaHatAlphaDLM works", {
    betaHatAlphaDLM <- demest:::betaHatAlphaDLM
    initialPrior <- demest:::initialPrior
    spec <- DLM()
    beta <- rnorm(20)
    metadata <- new("MetaData",
                    nms = c("sex", "time"),
                    dimtypes = c("sex", "time"),
                    DimScales = list(new("Sexes", dimvalues = c("f", "m")),
                        new("Points", dimvalues = 1:10)))
    strucZeroArray <- Counts(array(1:0,
                                   dim = c(2, 10),
                                   dimnames = list(sex = c("f", "m"),
                                                   time = 1:10)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1:2,
                          strucZeroArray = strucZeroArray)
    ans.obtained <- betaHatAlphaDLM(prior)
    ans.expected <- c(matrix(prior@alphaDLM, nr = 2)[1,-1],
                      rep(0, 10))
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of betaHatAlphaDLM give same answer", {
    betaHatAlphaDLM <- demest:::betaHatAlphaDLM
    initialPrior <- demest:::initialPrior
    spec <- DLM()
    beta <- rnorm(20)
    metadata <- new("MetaData",
                    nms = c("sex", "time"),
                    dimtypes = c("sex", "time"),
                    DimScales = list(new("Sexes", dimvalues = c("f", "m")),
                        new("Points", dimvalues = 1:10)))
    strucZeroArray <- Counts(array(1:0,
                                   dim = c(2, 10),
                                   dimnames = list(sex = c("f", "m"),
                                                   time = 1:10)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          strucZeroArray = strucZeroArray,
                          margin = 1:2)
    ans.R <- betaHatAlphaDLM(prior, useC = FALSE)
    ans.C <- betaHatAlphaDLM(prior, useC = TRUE)
    expect_identical(ans.R, ans.C)
})

test_that("betaHatCovariates works", {
    betaHatCovariates <- demest:::betaHatCovariates
    initialPrior <- demest:::initialPrior
    data <- data.frame(time = rep(1:10, times = 2),
                       sex = rep(c("f", "m"), each = 10),
                       income = rnorm(20))
    spec <- Exch(covariates = Covariates(mean ~ income, data = data))
    beta <- rnorm(20)
    metadata <- new("MetaData",
                    nms = c("time", "sex"),
                    dimtypes = c("time", "sex"),
                    DimScales = list(new("Points", dimvalues = 1:10),
                        new("Sexes", dimvalues = c("f", "m"))))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(2, 10),
                                   dimnames = list(sex = c("f", "m"),
                                                   time = 1:10)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1:2,
                          strucZeroArray = strucZeroArray)
    ans.obtained <- betaHatCovariates(prior)
    ans.expected <- unname(drop(prior@Z %*% prior@eta@.Data))
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of betaHatCovariates give same answer", {
    betaHatCovariates <- demest:::betaHatCovariates
    initialPrior <- demest:::initialPrior
    data <- data.frame(time = rep(1:10, times = 2),
                       sex = rep(c("f", "m"), each = 10),
                       income = rnorm(20))
    spec <- Exch(covariates = Covariates(mean ~ income, data = data))
    beta <- rnorm(20)
    metadata <- new("MetaData",
                    nms = c("time", "sex"),
                    dimtypes = c("time", "sex"),
                    DimScales = list(new("Points", dimvalues = 1:10),
                        new("Sexes", dimvalues = c("f", "m"))))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(2, 10),
                                   dimnames = list(sex = c("f", "m"),
                                                   time = 1:10)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1:2,
                          strucZeroArray = strucZeroArray)
    ans.R <- betaHatCovariates(prior, useC = FALSE)
    ans.C <- betaHatCovariates(prior, useC = TRUE)
    expect_identical(ans.R, ans.C)
})

test_that("betaHatSeason works", {
    betaHatSeason <- demest:::betaHatSeason
    initialPrior <- demest:::initialPrior
    spec <- DLM(season = Season(n = 2))
    beta <- rnorm(20)
    metadata <- new("MetaData",
                    nms = c("sex", "time"),
                    dimtypes = c("sex", "time"),
                    DimScales = list(new("Sexes", dimvalues = c("f", "m")),
                        new("Points", dimvalues = 1:10)))
    strucZeroArray <- Counts(array(1:0,
                                   dim = c(2, 10),
                                   dimnames = list(sex = c("f", "m"),
                                                   time = 1:10)),
                             dimscales = c(time = "Points"))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1:2,
                          strucZeroArray = strucZeroArray)
    ans.obtained <- betaHatSeason(prior)
    ans.expected <- c(matrix(sapply(prior@s, function(x) x[1]), nr = 2)[1,-1],
                      rep(0, 10))
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of betaHatSeason give same answer", {
    betaHatSeason <- demest:::betaHatSeason
    initialPrior <- demest:::initialPrior
    spec <- DLM(season = Season(n = 2))
    beta <- rnorm(20)
    strucZeroArray <- Counts(array(1:0,
                                   dim = c(2, 10),
                                   dimnames = list(sex = c("f", "m"),
                                                   time = 1:10)),
                             dimscales = c(time = "Points"))
    metadata <- new("MetaData",
                    nms = c("sex", "time"),
                    dimtypes = c("sex", "time"),
                    DimScales = list(new("Sexes", dimvalues = c("f", "m")),
                        new("Points", dimvalues = 1:10)))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1:2,
                          strucZeroArray = strucZeroArray)
    ans.R <- betaHatSeason(prior, useC = FALSE)
    ans.C <- betaHatSeason(prior, useC = TRUE)
    expect_identical(ans.R, ans.C)
})

test_that("findOneRootLogPostSigmaNorm works", {
    findOneRootLogPostSigmaNorm <- demest:::findOneRootLogPostSigmaNorm
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        A <- runif(n = 1, min = 0.1, max = 10)
        nu <- 1.0 * max(rpois(n = 1, lambda = 5), 1)
        V <- runif(n = 1, 0.01, 20)
        n <- as.integer(rpois(n = 1, lambda = 10)) + 1L
        sigma <- runif(n = 1, min = 0.001, max = 10)
        sigma.max <- sqrt((V - n*nu*A^2 + sqrt((V - n*nu*A^2)^2 + 4*(n + nu + 1)*V*nu*A^2))
                          / (2*(n + nu + 1)))
        max.right <- if (runif(1) < 0.7) runif(1, sigma.max, 10 * sigma.max) else Inf ## NEW
        ## sigma.left <- runif(n = 1, min = 0.000, max = sigma.max)
        ## sigma.right <- runif(n = 1, min = sigma.max, max = 3*sigma.max)
        sigma.left <- 0.5 * sigma.max ## NEW
        sigma.right <- min(1.5 * sigma.max, (sigma.max + max.right) / 2) ## NEW
        f <- function(sigma) {
            -n*log(sigma) - V/(2*sigma^2) - ((nu + 1)/2) * log(sigma^2 + nu*A^2)
        }
        z <- f(sigma) - rexp(1, 1)
        root.left <- findOneRootLogPostSigmaNorm(sigma0 = sigma.left,
                                                 z = z,
                                                 A = A,
                                                 nu = nu,
                                                 V = V,
                                                 n = n,
                                                 min = 0,
                                                 max = sigma.max,
                                                 useC = FALSE)
        root.right <- findOneRootLogPostSigmaNorm(sigma0 = sigma.right,
                                                  z = z,
                                                  A = A,
                                                  nu = nu,
                                                  V = V,
                                                  n = n,
                                                  min = sigma.max,
                                                  max = max.right, ## NEW
                                                  useC = FALSE)
        if (root.left > 0)
            expect_equal(f(root.left), z)
        if (root.right > 0)
            expect_equal(f(root.right), z)
        ans.at.max <- findOneRootLogPostSigmaNorm(sigma0 = sigma.max,
                                                  z = z,
                                                  A = A,
                                                  nu = nu,
                                                  V = V,
                                                  n = n,
                                                  min = sigma.max,
                                                  max = max.right, ## NEW
                                                  useC = FALSE)
        expect_true(isTRUE(all.equal(ans.at.max, sigma.max))
                    || isTRUE(all.equal(ans.at.max, -1))
                    || isTRUE(all.equal(ans.at.max, root.left))
                    || isTRUE(all.equal(ans.at.max, root.right)))
    }
})

## NOTE: C version of this function can give different results due
## to effect of kEpsilon test for deriv near zero.
test_that("R and C versions of findOneRootLogPostSigmaNorm give same answer", {
    findOneRootLogPostSigmaNorm <- demest:::findOneRootLogPostSigmaNorm
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        A <- runif(n = 1, min = 0.1, max = 10)
        nu <- 1.0 * max(rpois(n = 1, lambda = 5), 1)
        V <- runif(n = 1, 0.01, 10)
        n <- as.integer(rpois(n = 1, lambda = 10)) + 1L
        sigma <- runif(1, 0, 10)
        sigma.max <- sqrt((V - n*nu*A^2 + sqrt((V - n*nu*A^2)^2 + 4*(n + nu + 1)*V*nu*A^2))
                          / (2*(n + nu + 1)))
        f <- function(sigma) {
            -n*log(sigma) - V/(2*sigma^2) - ((nu + 1)/2) * log(sigma^2 + nu*A^2)
        }
        z <- f(sigma) - rexp(n = 1, 1)
        max.right <- if (runif(1) < 0.7) runif(1, sigma.max, 10 * sigma.max) else Inf ## NEW
        sigma.left <- 0.5 * sigma.max ## NEW
        sigma.right <- min(1.5 * sigma.max, (sigma.max + max.right) / 2) ## NEW
        root.left.R <- findOneRootLogPostSigmaNorm(sigma0 = sigma.left,
                                                   z = z,
                                                   A = A,
                                                   nu = nu,
                                                   V = V,
                                                   n = n,
                                                   min = 0,
                                                   max = sigma.max,
                                                   useC = FALSE)
        root.left.C <- findOneRootLogPostSigmaNorm(sigma0 = sigma.left,
                                                   z = z,
                                                   A = A,
                                                   nu = nu,
                                                   V = V,
                                                   n = n,
                                                   min = 0,
                                                   max = sigma.max,
                                                   useC = TRUE)
        root.right.R <- findOneRootLogPostSigmaNorm(sigma0 = sigma.right,
                                                    z = z,
                                                    A = A,
                                                    nu = nu,
                                                    V = V,
                                                    n = n,
                                                    min = sigma.max,
                                                    max = max.right,
                                                    useC = FALSE)
        root.right.C <- findOneRootLogPostSigmaNorm(sigma0 = sigma.right,
                                                    z = z,
                                                    A = A,
                                                    nu = nu,
                                                    V = V,
                                                    n = n,
                                                    min = sigma.max,
                                                    max = max.right,
                                                    ##max = Inf, JAH changed for line above
                                                    useC = TRUE)
        expect_equal(root.left.R, root.left.C)
        expect_equal(root.right.R, root.right.C)
        ans.at.max.R <- findOneRootLogPostSigmaNorm(sigma0 = sigma.max,
                                                    z = z,
                                                    A = A,
                                                    nu = nu,
                                                    V = V,
                                                    n = n,
                                                    min = sigma.max,
                                                    max = max.right, ## NEW
                                                    useC = FALSE)
        ans.at.max.C <- findOneRootLogPostSigmaNorm(sigma0 = sigma.max,
                                                    z = z,
                                                    A = A,
                                                    nu = nu,
                                                    V = V,
                                                    n = n,
                                                    min = sigma.max,
                                                    max = max.right, ## NEW
                                                    useC = TRUE)
        expect_equal(ans.at.max.R, ans.at.max.C)
        expect_true(isTRUE(all.equal(ans.at.max.R, sigma.max))
                    || isTRUE(all.equal(ans.at.max.R, -1))
                    || isTRUE(all.equal(ans.at.max.R, root.left.R))
                    || isTRUE(all.equal(ans.at.max.R, root.right.R)))
    }
})

test_that("findOneRootLogPostSigmaRobust works", {
    findOneRootLogPostSigmaRobust <- demest:::findOneRootLogPostSigmaRobust
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        A <- runif(n = 1, min = 0.1, max = 10)
        nuBeta <- 1.0 * max(rpois(n = 1, lambda = 5), 1)
        nuTau <- 1.0 * max(rpois(n = 1, lambda = 5), 1)
        V <- runif(n = 1, 0.01, 10)
        n <- as.integer(rpois(n = 1, lambda = 10)) + 1L
        H1 <- nuBeta * V
        H2 <- nuBeta * nuTau * V * A^2 + nuTau + 1 - n * nuBeta
        H3 <- -n * nuBeta * nuTau * A^2
        sigma.max <- sqrt((-H2 + sqrt(H2^2 - 4*H1*H3)) / (2*H1))
        f <- function(sigma) {
            n*nuBeta*log(sigma) - (nuBeta/2)*(sigma^2)*V - ((nuTau + 1)/2) * log(sigma^2 + nuTau*A^2)
        }
        sigma <- runif(1, 0.05, 10)
        z <- f(sigma) - rexp(n = 1, 1)
        ## sigma.left <- runif(n = 1, min = 0.005, max = sigma.max)
        ## sigma.right <- runif(n = 1, min = sigma.max, max = 5*sigma.max)
        max.right <- if (runif(1) < 0.7) runif(1, sigma.max, 10 * sigma.max) else Inf ## NEW
        sigma.left <- 0.5 * sigma.max ## NEW
        sigma.right <- min(1.5 * sigma.max, (sigma.max + max.right) / 2) ## NEW
        root.left <- findOneRootLogPostSigmaRobust(sigma0 = sigma.left,
                                                   z = z,
                                                   A = A,
                                                   nuBeta = nuBeta,
                                                   nuTau = nuTau,
                                                   V = V,
                                                   n = n,
                                                   min = 0,
                                                   max = sigma.max,
                                                   useC = FALSE)
        root.right <- findOneRootLogPostSigmaRobust(sigma0 = sigma.right,
                                                    z = z,
                                                    A = A,
                                                    nuBeta = nuBeta,
                                                    nuTau = nuTau,
                                                    V = V,
                                                    n = n,
                                                    min = sigma.max,
                                                    max = max.right,
                                                    useC = FALSE)
        if (root.left > 0)
            expect_equal(f(root.left), z)
        if (root.right > 0)
            expect_equal(f(root.right), z)
        ans.at.max <- findOneRootLogPostSigmaRobust(sigma0 = sigma.max,
                                                    z = z,
                                                    A = A,
                                                    nuBeta = nuBeta,
                                                    nuTau = nuTau,
                                                    V = V,
                                                    n = n,
                                                    min = sigma.max,
                                                    max = max.right,
                                                    useC = FALSE)
        expect_true(isTRUE(all.equal(ans.at.max, sigma.max))
                    || isTRUE(all.equal(ans.at.max, -1))
                    || isTRUE(all.equal(ans.at.max, root.left))
                    || isTRUE(all.equal(ans.at.max, root.right)))
    }
})

## NOTE: C version of this function can give different results due
## to effect of kEpsilon test for deriv near zero.
test_that("R and C versions of findOneRootLogPostSigmaRobust give same answer", {
    findOneRootLogPostSigmaRobust <- demest:::findOneRootLogPostSigmaRobust
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        A <- runif(n = 1, min = 0.1, max = 10)
        nuBeta <- 1.0 * max(rpois(n = 1, lambda = 5), 1)
        nuTau <- 1.0 * max(rpois(n = 1, lambda = 5), 1)
        V <- runif(n = 1, 0.01, 10)
        n <- as.integer(rpois(n = 1, lambda = 10)) + 1L
        H1 <- nuBeta * V
        H2 <- nuBeta * nuTau * V * A^2 + nuTau + 1 - n * nuBeta
        H3 <- -n * nuBeta * nuTau * A^2
        sigma.max <- sqrt((-H2 + sqrt(H2^2 - 4*H1*H3)) / (2*H1))
        f <- function(sigma) {
            n*nuBeta*log(sigma) - (nuBeta/2)*(sigma^2)*V - ((nuTau + 1)/2) * log(sigma^2 + nuTau*A^2)
        }
        sigma <- runif(1, 0.005, 5)
        z <- f(sigma) - rexp(n = 1, 1)
        max.right <- if (runif(1) < 0.7) runif(1, sigma.max, 10 * sigma.max) else Inf ## NEW
        ## sigma.left <- runif(n = 1, min = 0.001, max = sigma.max)
        ## sigma.right <- runif(n = 1, min = sigma.max, max = 10*sigma.max)
        sigma.left <- 0.5 * sigma.max ## NEW
        sigma.right <- min(1.5 * sigma.max, (sigma.max + max.right) / 2) ## NEW
        root.left.R <- findOneRootLogPostSigmaRobust(sigma0 = sigma.left,
                                                     z = z,
                                                     A = A,
                                                     nuBeta = nuBeta,
                                                     nuTau = nuTau,
                                                     V = V,
                                                     n = n,
                                                     min = 0,
                                                     max = sigma.max,
                                                     useC = FALSE)
        root.left.C <- findOneRootLogPostSigmaRobust(sigma0 = sigma.left,
                                                     z = z,
                                                     A = A,
                                                     nuBeta = nuBeta,
                                                     nuTau = nuTau,
                                                     V = V,
                                                     n = n,
                                                     min = 0,
                                                     max = sigma.max,
                                                     useC = TRUE)
        root.right.R <- findOneRootLogPostSigmaRobust(sigma0 = sigma.right,
                                                      z = z,
                                                      A = A,
                                                      nuBeta = nuBeta,
                                                      nuTau = nuTau,
                                                      V = V,
                                                      n = n,
                                                      min = sigma.max,
                                                      max = max.right,
                                                      useC = FALSE)
        root.right.C <- findOneRootLogPostSigmaRobust(sigma0 = sigma.right,
                                                      z = z,
                                                      A = A,
                                                      nuBeta = nuBeta,
                                                      nuTau = nuTau,
                                                      V = V,
                                                      n = n,
                                                      min = sigma.max,
                                                      max = max.right,
                                                      useC = TRUE)
        expect_equal(root.left.R, root.left.C)
        expect_equal(root.right.R, root.right.C)
        ans.at.max <- findOneRootLogPostSigmaRobust(sigma0 = sigma.max,
                                                    z = z,
                                                    A = A,
                                                    nuBeta = nuBeta,
                                                    nuTau = nuTau,
                                                    V = V,
                                                    n = n,
                                                    min = sigma.max,
                                                    max = max.right,
                                                    useC = TRUE)
        expect_true(isTRUE(all.equal(ans.at.max, sigma.max))
                    || isTRUE(all.equal(ans.at.max, -1))
                    || isTRUE(all.equal(ans.at.max, root.left.C))
                    || isTRUE(all.equal(ans.at.max, root.right.C)))
    }
})

test_that("R version of getLogPostMomentum works", {
    getLogPostMomentum <- demest:::getLogPostMomentum
    initializeMomentum <- demest:::initializeMomentum
    initialModel <- demest:::initialModel
    updateModelNotUseExp <- demest:::updateModelNotUseExp
    y <- Counts(array(rpois(n = 20, lambda = 30),
                      dim = 5:4,
                      dimnames = list(age = 0:4, region = letters[1:4])))
    y[1,] <- 0L
    strucZeroArray <- ValuesOne(c(0L, rep(1L, 4)), labels = 0:4, name = "age")
    spec <- Model(y ~ Poisson(mean ~ age + region,
                              useExpose = FALSE,
                              structuralZeros = strucZeroArray),
                  age ~ Exch(),
                  region ~ Zero())
    set.seed(1)
    x <- initialModel(spec, y = y, exposure = NULL)
    x <- updateModelNotUseExp(x, y = y, useC = TRUE)
    x <- initializeMomentum(x)
    ans.obtained <- getLogPostMomentum(x)
    ans.expected <- sum(dnorm(x@momentumBetas[[1]], sd = 1/sqrt(x@variancesBetas[[1]]), log = TRUE)) +
        sum(dnorm(x@momentumBetas[[2]][-1], sd = 1/sqrt(x@variancesBetas[[2]][-1]), log = TRUE))
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of getLogPostMomentum give same answer", {
    getLogPostMomentum <- demest:::getLogPostMomentum
    initializeMomentum <- demest:::initializeMomentum
    initialModel <- demest:::initialModel
    updateModelNotUseExp <- demest:::updateModelNotUseExp
    y <- Counts(array(rpois(n = 20, lambda = 30),
                      dim = 5:4,
                      dimnames = list(age = 0:4, region = letters[1:4])))
    y[1,] <- 0L
    strucZeroArray <- ValuesOne(c(0L, rep(1L, 4)), labels = 0:4, name = "age")
    spec <- Model(y ~ Poisson(mean ~ age + region,
                              useExpose = FALSE,
                              structuralZeros = strucZeroArray),
                  age ~ Exch(),
                  region ~ Zero())
    set.seed(1)
    x <- initialModel(spec, y = y, exposure = NULL)
    x <- updateModelNotUseExp(x, y = y, useC = TRUE)
    ans.R <- getLogPostMomentum(x, useC = FALSE)
    ans.C <- getLogPostMomentum(x, useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
})

test_that("R version of initializeMomentum works", {
    initializeMomentum <- demest:::initializeMomentum
    initialModel <- demest:::initialModel
    updateModelNotUseExp <- demest:::updateModelNotUseExp
    y <- Counts(array(rpois(n = 20, lambda = 30),
                      dim = 5:4,
                      dimnames = list(age = 0:4, region = letters[1:4])))
    y[1,] <- 0L
    strucZeroArray <- ValuesOne(c(0L, rep(1L, 4)), labels = 0:4, name = "age")
    spec <- Model(y ~ Poisson(mean ~ age + region,
                              useExpose = FALSE,
                              structuralZeros = strucZeroArray),
                  age ~ Exch(),
                  region ~ Zero())
    x <- initialModel(spec, y = y, exposure = NULL)
    x <- updateModelNotUseExp(x, y = y, useC = TRUE)
    set.seed(1)
    ans.obtained <- initializeMomentum(x)
    set.seed(1)
    ans.expected <- x
    ans.expected@momentumBetas[[1]] <- rnorm(1, sd = 1/sqrt(x@variancesBetas[[1]]))
    ans.expected@momentumBetas[[2]][-1] <- rnorm(4, sd = 1/sqrt(x@variancesBetas[[2]][-1]))
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of initializeMomentum give same answer", {
    initializeMomentum <- demest:::initializeMomentum
    initialModel <- demest:::initialModel
    updateModelNotUseExp <- demest:::updateModelNotUseExp
    y <- Counts(array(rpois(n = 20, lambda = 30),
                      dim = 5:4,
                      dimnames = list(age = 0:4, region = letters[1:4])))
    y[1,] <- 0L
    strucZeroArray <- ValuesOne(c(0L, rep(1L, 4)), labels = 0:4, name = "age")
    spec <- Model(y ~ Poisson(mean ~ age + region,
                              useExpose = FALSE,
                              structuralZeros = strucZeroArray),
                  age ~ Exch(),
                  region ~ Zero())
    x <- initialModel(spec, y = y, exposure = NULL)
    x <- updateModelNotUseExp(x, y = y, useC = TRUE)
    set.seed(1)
    ans.R <- initializeMomentum(x, useC = FALSE)
    set.seed(1)
    ans.C <- initializeMomentum(x, useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
})

test_that("logPostPhiMix works", {
    logPostPhiMix <- demest:::logPostPhiMix
    phi <- 0.5
    level <- matrix(1*seq(1,6),nrow=2)
    meanLevel <- 0
    nAlong <- 2L
    indexClassMaxMix <- 2L
    omega <- 1
    ans.expected <- ((1-phi^2)*10+1.5^2+2.5^2)/(-2)
    ans.obtained <- logPostPhiMix(phi = phi,
                                       level = level,
                                       meanLevel = meanLevel,
                                       nAlong = nAlong,
                                       indexClassMaxMix = indexClassMaxMix,
                                       omega = omega)
    expect_equal(ans.obtained, ans.expected)
    phi_log_posterior <- function(mu,phi,Lw,alpha,Kstar,sigma2ETA)
    {
        ## 'mu'
        stopifnot(identical(length(mu), 1L))
        stopifnot(is.double(mu))
        ## 'phi'
        stopifnot(identical(length(phi), 1L))
        stopifnot(is.double(phi))
        ## 'alpha'
        stopifnot(is.matrix(alpha))
        stopifnot(sum(is.na(alpha))<1)
        stopifnot(dim(alpha)[2]>=Kstar)
        stopifnot(identical(dim(alpha)[1], Lw))
        ## 'Lw'
        stopifnot(identical(length(Lw), 1L))
        stopifnot(is.integer(Lw))
        stopifnot(Lw > 0)
        ## 'Kstar'
        stopifnot(identical(length(Kstar), 1L))
        stopifnot(is.integer(Kstar))
        stopifnot(Kstar > 0)
        ## 'sigma2ETA'
        stopifnot(identical(length(sigma2ETA), 1L))
        stopifnot(is.double(sigma2ETA))
        stopifnot(sigma2ETA > 0)
        if(abs(phi)<1)
        {
            res0 <- (1-phi^2)/(-2*sigma2ETA)*sum((alpha[1,1:Kstar]-mu/(1-phi))^2)
            res <- res0+1/(-2*sigma2ETA)*sum((alpha[2:Lw,1:Kstar]-mu-phi*alpha[1:(Lw-1),1:Kstar])^2)
        }else{
            res <- 0.0001
        }
        return(res)
    }
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        phi <- runif(1)
        nAlong <- sample(2:10, 1)
        indexClassMaxMix <- sample(2:10, 1)
        level <- matrix(rnorm(nAlong * indexClassMaxMix),
                        nrow = nAlong,
                        ncol = indexClassMaxMix)
        meanLevel <- rnorm(n = 1, sd = 0.1)
        omega <- runif(1)
        ans.obtained <- logPostPhiMix(phi = phi,
                                           level = level,
                                           meanLevel = meanLevel,
                                           nAlong = nAlong,
                                           indexClassMaxMix = indexClassMaxMix,
                                           omega = omega)
        ans.expected <- phi_log_posterior(mu = meanLevel,
                                          phi = phi,
                                          Lw = nAlong,
                                          alpha = level,
                                          Kstar = indexClassMaxMix,
                                          sigma2ETA = omega^2)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of logPostPhiMix give same answer", {
    logPostPhiMix <- demest:::logPostPhiMix
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        phi <- runif(1)
        nAlong <- sample(2:10, 1)
        indexClassMaxMix <- sample(2:10, 1)
        level <- matrix(rnorm(nAlong * indexClassMaxMix),
                        nrow = nAlong,
                        ncol = indexClassMaxMix)
        meanLevel <- rnorm(n = 1, sd = 0.1)
        omega <- runif(1)
        ans.R <- logPostPhiMix(phi = phi,
                                    level = level,
                                    meanLevel = meanLevel,
                                    nAlong = nAlong,
                                    indexClassMaxMix = indexClassMaxMix,
                                    omega = omega,
                                    useC = FALSE)
        ans.C <- logPostPhiMix(phi = phi,
                                    level = level,
                                    meanLevel = meanLevel,
                                    nAlong = nAlong,
                                    indexClassMaxMix = indexClassMaxMix,
                                    omega = omega,
                                    useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("logPostPhiFirstOrderMix works", {
    logPostPhiFirstOrderMix <- demest:::logPostPhiFirstOrderMix
    phi <- 0.5
    level <- matrix(1*seq(1,6),nrow=2)
    meanLevel <- 0.5
    nAlong <- 2L
    indexClassMaxMix <- 2L
    omega <- 1
    ans.expected <- (-1)*(0+4)+(1-0.25)*2*(-0.5)/(0.5^2)*(3-1)+(-2)*((2-0.5-0.5*1)+(4-0.5-1.5)*(3))
    ans.expected <- 1/(-2*omega^2)*ans.expected
    ans.obtained <- logPostPhiFirstOrderMix(phi = phi,
                                            level = level,
                                            meanLevel = meanLevel,
                                            nAlong = nAlong,
                                            indexClassMaxMix = indexClassMaxMix,
                                            omega = omega)
    expect_equal(ans.obtained, ans.expected)
    phi_log_posterior_first_order <- function(mu,phi,Lw,alpha,Kstar,sigma2ETA)
    {
        ## 'mu'
        stopifnot(identical(length(mu), 1L))
        stopifnot(is.double(mu))
        ## 'phi'
        stopifnot(identical(length(phi), 1L))
        stopifnot(is.double(phi))
        ## 'alpha'
        stopifnot(is.matrix(alpha))
        stopifnot(sum(is.na(alpha))<1)
        stopifnot(dim(alpha)[2]>=Kstar)
        stopifnot(identical(dim(alpha)[1], Lw))
        ## 'Lw'
        stopifnot(identical(length(Lw), 1L))
        stopifnot(is.integer(Lw))
        stopifnot(Lw > 0)
        ## 'Kstar'
        stopifnot(identical(length(Kstar), 1L))
        stopifnot(is.integer(Kstar))
        stopifnot(Kstar > 0)
        ## 'sigma2ETA'
        stopifnot(identical(length(sigma2ETA), 1L))
        stopifnot(is.double(sigma2ETA))
        stopifnot(sigma2ETA > 0)
        if(abs(phi)<1)
        {
            res0 <- -2*sum((alpha[1,1:Kstar]-mu/(1-phi))*(phi*alpha[1,1:Kstar]+mu/(1-phi)))
            res <- res0-2*(sum((alpha[1:(Lw-1),1:Kstar])*(alpha[2:Lw,1:Kstar]-mu-phi*alpha[1:(Lw-1),1:Kstar])))
            res <- res*(-1/(2*sigma2ETA))  ## corrected from 'res*(-1/2*sigma2ETA)'
        }else{
            res <- 0.0001
        }
        return(res)
    }
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        phi <- runif(1)
        nAlong <- sample(2:10, 1)
        indexClassMaxMix <- sample(2:10, 1)
        level <- matrix(rnorm(nAlong * indexClassMaxMix),
                        nrow = nAlong,
                        ncol = indexClassMaxMix)
        meanLevel <- rnorm(n = 1, sd = 0.1)
        omega <- runif(1)
        ans.obtained <- logPostPhiFirstOrderMix(phi = phi,
                                                level = level,
                                                meanLevel = meanLevel,
                                                nAlong = nAlong,
                                                indexClassMaxMix = indexClassMaxMix,
                                                omega = omega)
        ans.expected <- phi_log_posterior_first_order(mu = meanLevel,
                                                      phi = phi,
                                                      Lw = nAlong,
                                                      alpha = level,
                                                      Kstar = indexClassMaxMix,
                                                      sigma2ETA = omega^2)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of logPostPhiFirstOrderMix give same answer", {
    logPostPhiFirstOrderMix <- demest:::logPostPhiFirstOrderMix
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        phi <- runif(1)
        nAlong <- sample(2:10, 1)
        indexClassMaxMix <- sample(2:10, 1)
        level <- matrix(rnorm(nAlong * indexClassMaxMix),
                        nrow = nAlong,
                        ncol = indexClassMaxMix)
        meanLevel <- rnorm(n = 1, sd = 0.1)
        omega <- runif(1)
        ans.R <- logPostPhiFirstOrderMix(phi = phi,
                                              level = level,
                                              meanLevel = meanLevel,
                                              nAlong = nAlong,
                                              indexClassMaxMix = indexClassMaxMix,
                                              omega = omega,
                                              useC = FALSE)
        ans.C <- logPostPhiFirstOrderMix(phi = phi,
                                              level = level,
                                              meanLevel = meanLevel,
                                              nAlong = nAlong,
                                              indexClassMaxMix = indexClassMaxMix,
                                              omega = omega,
                                              useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})


test_that("logPostPhiSecondOrderMix works", {
    logPostPhiSecondOrderMix <- demest:::logPostPhiSecondOrderMix
    phi <- 0.5
    level <- matrix(1*seq(1,6),nrow=3)
    meanLevel <- 0.5
    nAlong <- 3L
    indexClassMaxMix <- 2L
    omega <- 1
    ans.expected <- ((-0.5)/0.25*(0.5+1+2+1)+3*(4+2))*(-2)+2*(1+16+4+25)
    ans.expected <- 1/(-2*omega^2)*ans.expected
    ans.obtained <- logPostPhiSecondOrderMix(phi = phi,
                                             level = level,
                                             meanLevel = meanLevel,
                                             nAlong = nAlong,
                                             indexClassMaxMix = indexClassMaxMix,
                                             omega = omega)
    expect_equal(ans.obtained, ans.expected)
    phi_log_posterior_second_order <- function(mu,phi,Lw,alpha,Kstar,sigma2ETA,useC=FALSE)
    {
        ## 'mu'
        stopifnot(identical(length(mu), 1L))
        stopifnot(is.double(mu))
        ## 'phi'
        stopifnot(identical(length(phi), 1L))
        stopifnot(is.double(phi))
        ## stopifnot(abs(phi) <= 1)
        ## 'alpha'
        stopifnot(is.matrix(alpha))
        stopifnot(sum(is.na(alpha))<1)
        stopifnot(dim(alpha)[2]>=Kstar)
        stopifnot(identical(dim(alpha)[1], Lw))
        ## 'Lw'
        stopifnot(identical(length(Lw), 1L))
        stopifnot(is.integer(Lw))
        stopifnot(Lw > 0)
        ## 'Kstar'
        stopifnot(identical(length(Kstar), 1L))
        stopifnot(is.integer(Kstar))
        stopifnot(Kstar > 0)
        ## 'sigma2ETA'
        stopifnot(identical(length(sigma2ETA), 1L))
        stopifnot(is.double(sigma2ETA))
        stopifnot(sigma2ETA > 0)
        if (useC) {
            .Call(phi_log_posterior_second_order_R,mu,phi,Lw,alpha,Kstar,sigma2ETA)
        }
        else {
            if(abs(phi)<1)
            {
                res <- 4*Kstar*mu^2/(1-phi)^3+2*sum((alpha[seq(2,Lw-1),1:Kstar])^2)
                res <- 1/(-2*sigma2ETA)*res
            }else{
                res <- 0.0001
            }
            return(res)
        }
    }
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        phi <- runif(1)
        nAlong <- sample(3:10, 1) ## original version doesn't work for nAlong = 2
        indexClassMaxMix <- sample(2:10, 1) 
        level <- matrix(rnorm(nAlong * indexClassMaxMix),
                        nrow = nAlong,
                        ncol = indexClassMaxMix)
        meanLevel <- rnorm(n = 1, sd = 0.1)
        omega <- runif(1)
        ans.obtained <- logPostPhiSecondOrderMix(phi = phi,
                                                 level = level,
                                                 meanLevel = meanLevel,
                                                 nAlong = nAlong,
                                                 indexClassMaxMix = indexClassMaxMix,
                                                 omega = omega)
        ans.expected <- phi_log_posterior_second_order(mu = meanLevel,
                                                      phi = phi,
                                                      Lw = nAlong,
                                                      alpha = level,
                                                      Kstar = indexClassMaxMix,
                                                      sigma2ETA = omega^2)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of logPostPhiSecondOrderMix give same answer", {
    logPostPhiSecondOrderMix <- demest:::logPostPhiSecondOrderMix
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        phi <- runif(1)
        nAlong <- sample(2:10, 1)
        indexClassMaxMix <- sample(2:10, 1)
        level <- matrix(rnorm(nAlong * indexClassMaxMix),
                        nrow = nAlong,
                        ncol = indexClassMaxMix)
        meanLevel <- rnorm(n = 1, sd = 0.1)
        omega <- runif(1)
        ans.R <- logPostPhiSecondOrderMix(phi = phi,
                                          level = level,
                                          meanLevel = meanLevel,
                                          nAlong = nAlong,
                                          indexClassMaxMix = indexClassMaxMix,
                                          omega = omega,
                                          useC = FALSE)
        ans.C <- logPostPhiSecondOrderMix(phi = phi,
                                          level = level,
                                          meanLevel = meanLevel,
                                          nAlong = nAlong,
                                          indexClassMaxMix = indexClassMaxMix,
                                          omega = omega,
                                          useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("getV gives valid answer with prior of class ExchFixed", {
    getV <- demest:::getV
    initialPrior <- demest:::initialPrior
    spec <- ExchFixed(sd = 3)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(c(0L, rep(1L, 9)),
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "ExchFixed")
    ans.obtained <- getV(prior)
    ans.expected <- c(NA, rep(prior@tau@.Data^2, 9))
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of getV give same answer with prior of class ExchFixed", {
    getV <- demest:::getV
    initialPrior <- demest:::initialPrior
    spec <- ExchFixed(sd = 3)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(c(0L, rep(1L, 9)),
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "ExchFixed")
    ans.R <- getV(prior, useC = FALSE)
    ans.C <- getV(prior, useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
})

test_that("getV gives valid answer with prior of class NormMixin", {
    getV <- demest:::getV
    initialPrior <- demest:::initialPrior
    spec <- Exch()
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    ans.obtained <- getV(prior)
    ans.expected <- rep(prior@tau@.Data^2, 10)
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of getV give same answer with prior of class NormMixin", {
    getV <- demest:::getV
    initialPrior <- demest:::initialPrior
    spec <- Exch()
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    ans.R <- getV(prior, useC = FALSE)
    ans.C <- getV(prior, useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
})

test_that("getV gives valid answer with prior of class RobustMixin", {
    getV <- demest:::getV
    initialPrior <- demest:::initialPrior
    spec <- Exch(error = Error(robust = TRUE))
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    ans.obtained <- getV(prior)
    ans.expected <- prior@UBeta@.Data
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of getV give same answer with prior of class RobustMixin", {
    getV <- demest:::getV
    initialPrior <- demest:::initialPrior
    spec <- Exch(error = Error(robust = TRUE))
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    ans.R <- getV(prior, useC = FALSE)
    ans.C <- getV(prior, useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
})

test_that("getV works with KnownCertain", {
    getV <- demest:::getV
    initialPrior <- demest:::initialPrior
    mean <- ValuesOne(1:10, labels = letters[1:10], name = "region")
    spec <- Known(mean)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "KnownCertain")
    ans.obtained <- getV(prior)
    ans.expected <- rep(0, 10)
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of getV give same answer with KnownCertain", {
    getV <- demest:::getV
    initialPrior <- demest:::initialPrior
    mean <- ValuesOne(1:10, labels = letters[1:10], name = "region")
    spec <- Known(mean)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "KnownCertain")
    ans.R <- getV(prior, useC = FALSE)
    ans.C <- getV(prior, useC = TRUE)
    expect_identical(ans.R, ans.C)
})

test_that("getV works with KnownUncertain", {
    getV <- demest:::getV
    initialPrior <- demest:::initialPrior
    mean <- ValuesOne(1:10, labels = letters[1:10], name = "region")
    spec <- Known(mean, sd = 1.1)
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "KnownUncertain")
    ans.obtained <- getV(prior)
    ans.expected <- rep(1.1^2, 10)
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of getV give same answer with KnownUncertain", {
    getV <- demest:::getV
    initialPrior <- demest:::initialPrior
    mean <- ValuesOne(1:10, labels = letters[1:10], name = "region")
    spec <- Known(mean, sd = 0.2)
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "KnownUncertain")
    ans.R <- getV(prior, useC = FALSE)
    ans.C <- getV(prior, useC = TRUE)
    expect_identical(ans.R, ans.C)
})

test_that("getV works with Zero", {
    getV <- demest:::getV
    initialPrior <- demest:::initialPrior
    spec <- Zero()
    beta <- rnorm(10)
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "Zero")
    ans.obtained <- getV(prior)
    ans.expected <- rep(0, 10)
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of getV give same answer with Zero", {
    getV <- demest:::getV
    initialPrior <- demest:::initialPrior
    spec <- Zero()
    beta <- rnorm(10)
    strucZeroArray <- Counts(array(1L,
                                   dim = 10,
                                   dimnames = list(region = letters[1:10])))
    metadata <- new("MetaData",
                    nms = "region",
                    dimtypes = "state",
                    DimScales = list(new("Categories", dimvalues = letters[1:10])))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = FALSE,
                          margin = 1L,
                          strucZeroArray = strucZeroArray)
    expect_is(prior, "Zero")
    ans.R <- getV(prior, useC = FALSE)
    ans.C <- getV(prior, useC = TRUE)
    expect_identical(ans.R, ans.C)
})

test_that("R version of makeLifeExpBirth works", {
    makeLifeExpBirth <- demest:::makeLifeExpBirth
    makeLifeExp1 <- function(mx1, ax1, nx) {
        n.age <- length(nx)
        qx <- (nx * mx1) / (1 + (nx - ax1) * mx1)
        lx <- rep(1.0, times = n.age)
        for (i in seq.int(from = 2L, to = n.age))
            lx[i] <- lx[i - 1L] * (1 - qx[i - 1L])
        Lx <- numeric(length = n.age)
        for (i in seq.int(from = 1L, to = n.age - 1L))
            Lx[i] <- lx[i + 1] * nx[i] + (lx[i] - lx[i + 1L]) * ax1[i]
        Lx[n.age] <- lx[n.age] / mx1[n.age]
        sum(Lx)
    }
    mx <- rgamma(n = 100, shape = 3, rate = 0.01)/10000
    nx <- c(1, 4, rep(5, 7), Inf)
    ax <- rep_len(x = c(0.1, 1.5, rep(2.5, 8)), 100)
    ans.obtained <- makeLifeExpBirth(mx = mx,
                                     nx = nx,
                                     ax = ax,
                                     iAge0 = 21L,
                                     nAge = 10L)
    ans.expected <- makeLifeExp1(mx1 = mx[21:30],
                                 ax1 = ax[21:30],
                                 nx = nx)
    expect_equal(ans.obtained, ans.expected)
    ans.obtained <- makeLifeExpBirth(mx = mx,
                                     nx = nx,
                                     ax = ax,
                                     iAge0 = 91L,
                                     nAge = 10L)
    ans.expected <- makeLifeExp1(mx1 = mx[91:100],
                                 ax1 = ax[91:100],
                                 nx = nx)
    expect_equal(ans.obtained, ans.expected)
})

test_that("R and C versions of makeLifeExpBirth give same answer", {
    makeLifeExpBirth <- demest:::makeLifeExpBirth
    mx <- rgamma(n = 100, shape = 3, rate = 0.01)/10000
    nx <- c(1, 4, rep(5, 7), Inf)
    ax <- rep_len(x = c(0.1, 1.5, rep(2.5, 8)), 100)
    ans.R <- makeLifeExpBirth(mx = mx,
                              nx = nx,
                              ax = ax,
                              iAge0 = 21L,
                              nAge = 10L,
                              useC = FALSE)
    ans.C <- makeLifeExpBirth(mx = mx,
                              nx = nx,
                              ax = ax,
                              iAge0 = 21L,
                              nAge = 10L,
                              useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
    mx <- rgamma(n = 100, shape = 3, rate = 0.01)/10000
    nx <- c(1, 4, rep(5, 17), Inf)
    ax <- rep_len(x = c(0.1, 1.5, rep(2.5, 18)), 100)
    ans.R <- makeLifeExpBirth(mx = mx,
                              nx = nx,
                              ax = ax,
                              iAge0 = 81L,
                              nAge = 20L,
                              useC = FALSE)
    ans.C <- makeLifeExpBirth(mx = mx,
                              nx = nx,
                              ax = ax,
                              iAge0 = 81L,
                              nAge = 20L,
                              useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
})


## makeVBarAndN #####################################################################

test_that("makeVBarAndN gives valid answer with PoissonVaryingNotUseExp, main effects model, terms in order, no missing", {
    makeVBarAndN <- demest:::makeVBarAndN
    initialModel <- demest:::initialModel
    y <- Counts(array(rpois(n = 20, lambda = 10),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Poisson(mean ~ age + region, useExpose = FALSE))
    x <- initialModel(spec, y = y, exposure = NULL)
    ## iBeta = 1L
    ans.obtained <- makeVBarAndN(x, iBeta = 1L)
    other.betas <- x@betas[[2]] + rep(x@betas[[3]], each = 5)
    g.theta <- log(x@theta)
    ans.expected <- list(sum(g.theta - other.betas) / length(x@theta),
                         20L)
    expect_equal(ans.obtained, ans.expected)
    ## iBeta = 2L
    ans.obtained <- makeVBarAndN(x, iBeta = 2L)
    other.betas <- x@betas[[1]] + rep(x@betas[[3]], each = 5)
    g.theta <- log(x@theta)
    ans <- g.theta - other.betas
    ans.expected <- list(rowMeans(matrix(ans, nrow = 5)),
                         rep(4L, 5))
    expect_equal(ans.obtained, ans.expected)
    ## iBeta = 3L
    ans.obtained <- makeVBarAndN(x, iBeta = 3L)
    other.betas <- x@betas[[1]] + x@betas[[2]]
    g.theta <- log(x@theta)
    ans.expected <- g.theta - other.betas
    ans.expected <- list(colMeans(matrix(ans.expected, nrow = 5)),
                         rep(5L, 4))
    expect_equal(ans.obtained, ans.expected)
})


test_that("R and C versions of makeVBarAndN give same answer with PoissonVaryingNotUseExp, main effects, terms in order, no missing", {
    makeVBarAndN <- demest:::makeVBarAndN
    initialModel <- demest:::initialModel  
    for (seed in seq_len(n.test)) {
        set.seed(seed+1)
        y <- Counts(array(rpois(n = 20, lambda = 10),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = letters[1:4])))
        spec <- Model(y ~ Poisson(mean ~ age + region, useExpose = FALSE))
        x <- initialModel(spec, y = y, exposure = NULL)
        save_x <- x
        for (iBeta in seq.int(from = 1, to = 3)) {
            ans.R <- makeVBarAndN(x, iBeta = iBeta, useC = FALSE)
            ans.C <- makeVBarAndN(x, iBeta = iBeta, useC = TRUE)
            expect_identical(x, save_x)
            if (test.identity)
                expect_identical(ans.R, ans.C)
            else
                expect_equal(ans.R, ans.C)
        }
    }
})

test_that("makeVBarAndN gives valid answer with PoissonVaryingNotUseExp, main effects model, terms in order, has missing", {
    makeVBarAndN <- demest:::makeVBarAndN
    initialModel <- demest:::initialModel
    y <- Counts(array(rpois(n = 20, lambda = 10),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    y[1] <- NA
    spec <- Model(y ~ Poisson(mean ~ age + region, useExpose = FALSE))
    x <- initialModel(spec, y = y, exposure = NULL)
    ## iBeta = 1L
    ans.obtained <- makeVBarAndN(x, iBeta = 1L)
    other.betas <- x@betas[[2]] + rep(x@betas[[3]], each = 5)
    other.betas <- other.betas[-1]
    g.theta <- log(x@theta)
    g.theta <- g.theta[-1]
    ans.expected <- list(sum(g.theta - other.betas) / length(g.theta),
                         19L)
    expect_equal(ans.obtained, ans.expected)
    ## iBeta = 2L
    ans.obtained <- makeVBarAndN(x, iBeta = 2L)
    other.betas <- x@betas[[1]] + rep(x@betas[[3]], each = 5)
    g.theta <- log(x@theta)
    vbar <- matrix(g.theta - other.betas, nr = 5)
    vbar[1] <- 0
    vbar <- rowSums(vbar)
    n <- c(3L, rep(4L, 4))
    vbar <- vbar/n
    ans.expected <- list(vbar, n)
    expect_equal(ans.obtained, ans.expected)
    ## iBeta = 3L
    ans.obtained <- makeVBarAndN(x, iBeta = 3L)
    other.betas <- x@betas[[1]] + x@betas[[2]]
    g.theta <- log(x@theta)
    vbar <- matrix(g.theta - other.betas, nr = 5)
    vbar[1] <- 0
    vbar <- colSums(vbar)
    n <- c(4L, rep(5L, 3))
    vbar <- vbar/n
    ans.expected <- list(vbar, n)
    expect_equal(ans.obtained, ans.expected)
})

test_that("R and C versions of makeVBarAndN give same answer with PoissonVaryingNotUseExp, main effects, terms in order, has missing", {
    makeVBarAndN <- demest:::makeVBarAndN
    initialModel <- demest:::initialModel  
    for (seed in seq_len(n.test)) {
        set.seed(seed+1)
        y <- Counts(array(rpois(n = 20, lambda = 10),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = letters[1:4])))
        y[10] <- NA
        spec <- Model(y ~ Poisson(mean ~ age + region, useExpose = FALSE))
        x <- initialModel(spec, y = y, exposure = NULL)
        save_x <- x
        for (iBeta in seq.int(from = 1, to = 3)) {
            ans.R <- makeVBarAndN(x, iBeta = iBeta, useC = FALSE)
            ans.C <- makeVBarAndN(x, iBeta = iBeta, useC = TRUE)
            expect_identical(x, save_x)
            if (test.identity)
                expect_identical(ans.R, ans.C)
            else
                expect_equal(ans.R, ans.C)
        }
    }
})

test_that("makeVBarAndN gives valid answer with PoissonVaryingNotUseExp, with Box-Cox transform", {
    makeVBarAndN <- demest:::makeVBarAndN
    initialModel <- demest:::initialModel
    y <- Counts(array(rpois(n = 20, lambda = 10),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Poisson(mean ~ age + region,
                              useExpose = FALSE,
                              boxcox = 0.6))
    x <- initialModel(spec, y = y, exposure = NULL)
    ## iBeta = 1L
    ans.obtained <- makeVBarAndN(x, iBeta = 1L)
    other.betas <- x@betas[[2]] + rep(x@betas[[3]], each = 5)
    f.theta <- (x@theta^0.6 - 1)/0.6
    ans.expected <- list(sum(f.theta - other.betas) / length(x@theta),
                         20L)
    expect_equal(ans.obtained, ans.expected)
    ## iBeta = 2L
    ans.obtained <- makeVBarAndN(x, iBeta = 2L)
    other.betas <- x@betas[[1]] + rep(x@betas[[3]], each = 5)
    f.theta <- (x@theta^0.6 - 1)/0.6
    ans <- f.theta - other.betas
    ans.expected <- list(rowMeans(matrix(ans, nrow = 5)),
                         rep(4L, 5))
    expect_equal(ans.obtained, ans.expected)
    ## iBeta = 3L
    ans.obtained <- makeVBarAndN(x, iBeta = 3L)
    other.betas <- x@betas[[1]] + x@betas[[2]]
    f.theta <- (x@theta^0.6 - 1)/0.6
    ans.expected <- f.theta - other.betas
    ans.expected <- list(colMeans(matrix(ans.expected, nrow = 5)),
                         rep(5L, 4))
    expect_equal(ans.obtained, ans.expected)
})

test_that("R and C versions of makeVBarAndN give same answer with PoissonVaryingNotUseExp, Box-Cox transform", {
    makeVBarAndN <- demest:::makeVBarAndN
    initialModel <- demest:::initialModel  
    for (seed in seq_len(n.test)) {
        set.seed(seed+1)
        y <- Counts(array(rpois(n = 20, lambda = 10),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = letters[1:4])))
        y[10] <- NA
        spec <- Model(y ~ Poisson(mean ~ age + region,
                                  useExpose = FALSE,
                                  boxcox = 0.6))
        x <- initialModel(spec, y = y, exposure = NULL)
        save_x <- x
        for (iBeta in seq.int(from = 1, to = 3)) {
            ans.R <- makeVBarAndN(x, iBeta = iBeta, useC = FALSE)
            ans.C <- makeVBarAndN(x, iBeta = iBeta, useC = TRUE)
            expect_identical(x, save_x)
            if (test.identity)
                expect_identical(ans.R, ans.C)
            else
                expect_equal(ans.R, ans.C)
        }
    }
})

test_that("makeVBarAndN gives valid answer with PoissonVaryingNotUseExp, intercept only", {
    makeVBarAndN <- demest:::makeVBarAndN
    initialModel <- demest:::initialModel
    y <- Counts(array(rpois(n = 20, lambda = 10),
                      dim = c(5, 4),
                      dimnames = list(age = 0:4, region = letters[1:4])))
    y[3] <- NA
    spec <- Model(y ~ Poisson(mean ~ 1, useExpose = FALSE))
    x <- initialModel(spec, y = y, exposure = NULL)
    ans.obtained <- makeVBarAndN(x, iBeta = 1L)
    g.theta <- log(x@theta)
    ans.expected <- list(mean(g.theta[-3]),
                         19L)
    expect_equal(ans.obtained, ans.expected)
})

test_that("makeVBarAndN gives valid answer with Binomial, terms out of order", {
    ## dim = c(2, 3, 4), margins = list(0L, 3:2, 2L, 3L)
    makeVBarAndN <- demest:::makeVBarAndN
    initialModel <- demest:::initialModel
    logit <- function(p) log(p / (1 - p))
    exposure <- Counts(array(rpois(n = 24, lambda = 10),
                             dim = 2:4,
                             dimnames = list(sex = c("f", "m"),
                                 age = c("0-4", "5-9", "10+"),
                                 region = letters[1:4])))
    y <- Counts(array(rbinom(n = 24, prob = 0.5, size = exposure),
                dim = dim(exposure),
                dimnames = dimnames(exposure)))
    y[11] <- NA
    spec <- Model(y ~ Binomial(mean ~ age:region + region + age))
    x <- initialModel(spec, y = y, exposure = exposure)
    ## iBeta = 1L
    ans.obtained <- makeVBarAndN(x, iBeta = 1L)
    other.betas <- (rep(matrix(x@betas[[4]], nrow = 3), each = 2)
                    + rep(x@betas[[3]], each = 2)
                    + rep(x@betas[[2]], each = 6))
    g.theta <- logit(x@theta)
    ans.expected <- list(mean(g.theta[-11] - other.betas[-11]),
                         23L)
    expect_equal(ans.obtained, ans.expected) 
    ## iBeta = 2L
    ans.obtained <- makeVBarAndN(x, iBeta = 2L)
    other.betas <- (x@betas[[1]]
                    + rep(x@betas[[3]], each = 2)
                    + rep(x@betas[[4]], each = 2))
    g.theta <- logit(x@theta)
    ans.expected <- g.theta - other.betas
    ans.expected <- matrix(ans.expected, nrow = 6)
    ans.expected[11] <- NA
    ans.expected <- colMeans(ans.expected, na.rm = TRUE)
    ans.expected <- list(ans.expected,
                         c(6L, 5L, 6L, 6L))
    expect_equal(ans.obtained, ans.expected)
    ## iBeta = 3L
    ans.obtained <- makeVBarAndN(x, iBeta = 3L)
    other.betas <- (x@betas[[1]]
                    + rep(x@betas[[2]], each = 6)
                    + rep(x@betas[[4]], each = 2))
    g.theta <- logit(x@theta)
    ans.expected <- g.theta - other.betas
    ans.expected <- array(ans.expected, dim = 2:4)
    ans.expected[11] <- NA
    ans.expected <- apply(ans.expected, 2, mean, na.rm = TRUE)
    ans.expected <- list(ans.expected,
                         c(8L, 8L, 7L))
    expect_equal(ans.obtained, ans.expected)
    ## iBeta = 4L
    ans.obtained <- makeVBarAndN(x, iBeta = 4L)
    other.betas <- (x@betas[[1]]
                    + rep(x@betas[[2]], each = 6)
                    + rep(x@betas[[3]], each = 2))
    g.theta <- logit(x@theta)
    ans.expected <- g.theta - other.betas
    ans.expected <- array(ans.expected, dim = 2:4)
    ans.expected[11] <- NA
    ans.expected <- apply(ans.expected, 2:3, mean, na.rm = TRUE)
    ans.expected <- list(as.numeric(ans.expected),
                         c(rep(2L, 5), 1L, rep(2L, 6)))
    expect_equal(ans.obtained, ans.expected)
})

test_that("makeVBarAndN gives valid answer with Normal, main effects", {
    makeVBarAndN <- demest:::makeVBarAndN
    initialModel <- demest:::initialModel
    y <- Counts(array(rnorm(n = 24),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"),
                          age = c("0-4", "5-9", "10+"),
                          region = letters[1:4])))
    weights <- Counts(array(1,
                            dim = 2:4,
                            dimnames = list(sex = c("f", "m"),
                                age = c("0-4", "5-9", "10+"),
                                region = letters[1:4])))
    spec <- Model(y ~ Normal(mean ~ sex + age))
    x <- initialModel(spec, y = y, weights = weights)
    identity <- function(x) x
    ## iBeta = 1L
    ans.obtained <- makeVBarAndN(x, iBeta = 1L)
    other.betas <- rep(x@betas[[2]], times = 12) + rep(x@betas[[3]], each = 2)
    g.theta <- x@theta
    ans.expected <- mean(g.theta - other.betas)
    ans.expected <- list(ans.expected, 24L)
    expect_equal(ans.obtained, ans.expected)
    ## iBeta = 2L
    ans.obtained <- makeVBarAndN(x, iBeta = 2L)
    other.betas <- rep(x@betas[[1]], times = 24) + rep(x@betas[[3]], each = 2)
    g.theta <- x@theta
    ans.expected <- g.theta - other.betas
    ans.expected <- matrix(ans.expected, nrow = 2)
    ans.expected <- rowMeans(ans.expected)
    ans.expected <- list(ans.expected,
                         c(12L, 12L))
    expect_equal(ans.obtained, ans.expected)
    ## iBeta = 3L
    ans.obtained <- makeVBarAndN(x, iBeta = 3L)
    other.betas <- rep(x@betas[[1]], times = 24) + x@betas[[2]]
    g.theta <- x@theta
    ans.expected <- g.theta - other.betas
    ans.expected <- array(ans.expected, dim = 2:4)
    ans.expected <- apply(ans.expected, 2, mean)
    ans.expected <- list(ans.expected,
                         c(8L, 8L, 8L))
    expect_equal(ans.obtained, ans.expected)
})


test_that("R and C versions of makeVBarAndN give same answer with Poisson, intercept only", {
    makeVBarAndN <- demest:::makeVBarAndN
    initialModel <- demest:::initialModel  
    for (seed in seq_len(n.test)) {
        set.seed(seed+1)
        y <- Counts(array(rpois(n = 20, lambda = 10),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = letters[1:4])))
        y[c(1, 3, 5)] <- NA
        spec <- Model(y ~ Poisson(mean ~ 1, useExpose = FALSE))
        x <- initialModel(spec, y = y, exposure = NULL)
        save_x <- x
        iBeta <- 1L
        ans.R <- makeVBarAndN(x, iBeta = iBeta, useC = FALSE)
        ans.C <- makeVBarAndN(x, iBeta = iBeta, useC = TRUE)
        expect_identical(x, save_x)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

## tests equal but not identical
test_that("R and C versions of makeVBarAndN give same answer with Binomial, terms out of order", {
    makeVBarAndN <- demest:::makeVBarAndN
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed+1)
        logit <- function(p) log(p / (1 - p))
        exposure <- Counts(array(rpois(n = 24, lambda = 10),
                                 dim = 2:4,
                                 dimnames = list(sex = c("f", "m"),
                                     age = c("0-4", "5-9", "10+"),
                                     region = letters[1:4])))
        y <- Counts(array(rbinom(n = 24, prob = 0.5, size = exposure),
                          dim = dim(exposure),
                          dimnames = dimnames(exposure)))
        y[11] <- NA
        spec <- Model(y ~ Binomial(mean ~ age:region + region + age))
        x <- initialModel(spec, y = y, exposure = exposure)
        save_x <- x
        for (iBeta in seq.int(from = 1, to = 4)) {
            ans.R <- makeVBarAndN(x, iBeta = iBeta, useC = FALSE)
            ans.C <- makeVBarAndN(x, iBeta = iBeta, useC = TRUE)
            expect_identical(x, save_x)
            if (test.identity)
                expect_identical(ans.R, ans.C)
            else
                expect_equal(ans.R, ans.C)
        }
    }
})

## tests equal but not identical
test_that("R and C versions of makeVBarAndN give same answer with Normal, main effect only", {
    makeVBarAndN <- demest:::makeVBarAndN
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed+1)
        identity <- function(x) x
        y <- Counts(array(rnorm(n = 24),
                          dim = 2:4,
                          dimnames = list(sex = c("f", "m"),
                              age = c("0-4", "5-9", "10+"),
                              region = letters[1:4])))
        y[21:22] <- NA
        weights <- Counts(array(1,
                                dim = 2:4,
                                dimnames = list(sex = c("f", "m"),
                                    age = c("0-4", "5-9", "10+"),
                                    region = letters[1:4])))
        spec <- Model(y ~ Normal(mean ~ sex + age))
        x <- initialModel(spec, y = y, weights = weights)
        save_x <- x
        for (iBeta in seq.int(from = 1, to = 2)) {
            ans.R <- makeVBarAndN(x, iBeta = iBeta, useC = FALSE)
            ans.C <- makeVBarAndN(x, iBeta = iBeta, useC = TRUE)
            expect_identical(x, save_x)
            if (test.identity)
                expect_identical(ans.R, ans.C)
            else
                expect_equal(ans.R, ans.C)
        }
    }
})




##########################################################################################



test_that("modePhiMix works", {
    modePhiMix <- demest:::modePhiMix
    logPostPhiMix <- demest:::logPostPhiMix
    for (seed in seq_len(n.test)) {
        ## on fourth iteraction, where nAlong = 2, the mode
        ## is at -0.99999, ie -1 + tolerance
        set.seed(seed)
        phi <- runif(1)
        nAlong <- sample(2:10, 1)
        indexClassMaxMix <- sample(2:10, 1)
        level <- matrix(rnorm(nAlong * indexClassMaxMix),
                        nrow = nAlong,
                        ncol = indexClassMaxMix)
        meanLevel <- rnorm(n = 1, sd = 0.1)
        omega <- runif(1)
        tolerance <- 1e-5
        ans.obtained <- modePhiMix(level = level,
                                   meanLevel = meanLevel,
                                   nAlong = nAlong,
                                   indexClassMaxMix = indexClassMaxMix,
                                   omega = omega,
                                   tolerance = tolerance)
        logpost <- function(p)
            logPostPhiMix(phi = p,
                          level = level,
                          meanLevel = meanLevel,
                          nAlong = nAlong,
                          indexClassMaxMix = indexClassMaxMix,
                          omega = omega,
                          useC = TRUE)
        x <- seq(-0.99999, 0.99, length = 1000)
        vals <- sapply(x, logpost)
        expect_true(all(vals <= ans.obtained))
        if (FALSE) { # Graphical check. Creates 'n.text' new devices.
            dev.new()
            plot(vals ~ x, type = "l")
            max.val <- logPostPhiMix(phi = ans.obtained,
                                     level = level,
                                     meanLevel = meanLevel,
                                     nAlong = nAlong,
                                     indexClassMaxMix = indexClassMaxMix,
                                     omega = omega,
                                     useC = TRUE)
            points(x = ans.obtained, y = max.val)
        }
    }
})

test_that("R and C versions of modePhiMix give same answer", {
    modePhiMix <- demest:::modePhiMix
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        nAlong <- sample(2:10, 1)
        indexClassMaxMix <- sample(2:10, 1)
        level <- matrix(rnorm(nAlong * indexClassMaxMix),
                        nrow = nAlong,
                        ncol = indexClassMaxMix)
        meanLevel <- rnorm(n = 1, sd = 0.1)
        omega <- runif(1)
        tolerance <- 1e-5
        ans.R <- modePhiMix(level = level,
                            meanLevel = meanLevel,
                            nAlong = nAlong,
                            indexClassMaxMix = indexClassMaxMix,
                            omega = omega,
                            tolerance = tolerance,
                            useC = FALSE)
        ans.C <- modePhiMix(level = level,
                            meanLevel = meanLevel,
                            nAlong = nAlong,
                            indexClassMaxMix = indexClassMaxMix,
                            omega = omega,
                            tolerance = tolerance,
                            useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})        

test_that("safeLogProp_Binomial gives valid answers", {
    safeLogProp_Binomial <- demest:::safeLogProp_Binomial
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        logit.th.new <- rnorm(1, sd = 3)
        logit.th.other.new <- rnorm(1, sd = 3)
        logit.th.old <- rnorm(1, sd = 3)
        logit.th.other.old <- rnorm(1, sd = 3)
        scale <- runif(1, 0.01, 2)
        weight <- runif(1, 0.1, 2)
        weight.other <- runif(1, 0.1, 2)
        ans.obtained <- safeLogProp_Binomial(logit.th.new = logit.th.new,
                                             logit.th.other.new = logit.th.other.new,
                                             logit.th.old = logit.th.old,
                                             logit.th.other.old = logit.th.other.old,
                                             scale = scale,
                                             weight = weight,
                                             weight.other = weight.other)
        coef1 <- exp(-logit.th.new) + 2 + exp(logit.th.new)
        coef2 <- exp(-logit.th.other.new) + 2 + exp(logit.th.other.new)
        r <- abs(weight / weight.other)
        ans.expected <- log(coef1 * dnorm(logit.th.new, mean = logit.th.old, sd = scale)
                            + r * coef2 * dnorm(logit.th.other.new, mean = logit.th.other.old, sd = scale))
        ## print(c(logit.th.new, logit.th.other.new, logit.th.old, logit.th.other.old, scale))
        ## print(c(ans.obtained, ans.expected))
        expect_equal(ans.obtained, ans.expected)
    }
    ## logit.th.new very large
    max.exp <- 1.0 * .Machine$double.max.exp
    logit.th.new <- max.exp
    logit.th.other.new <- max.exp - 2
    logit.th.old <- rnorm(1, sd = 3)
    logit.th.other.old <- rnorm(1, sd = 3)
    scale <- runif(1, 0.01, 2)
    weight <- runif(1, 0.1, 2)
    weight.other <- runif(1, 0.1, 2)
    ans.obtained <- safeLogProp_Binomial(logit.th.new = logit.th.new,
                                         logit.th.other.new = logit.th.other.new,
                                         logit.th.old = logit.th.old,
                                         logit.th.other.old = logit.th.other.old,
                                         scale = scale,
                                         weight = weight,
                                         weight.other = weight.other)
    coef2 <- exp(2) + 2 + exp(-2)
    r <- abs(weight / weight.other)
    ans.expected <- logit.th.new + log(dnorm(logit.th.new, mean = logit.th.old, sd = scale)
                                       + r * coef2 * dnorm(logit.th.other.new, mean = logit.th.other.old, sd = scale))
    expect_equal(ans.obtained, ans.expected)
    ## logit.th.new very small
    max.exp <- 1.0 * .Machine$double.max.exp
    logit.th.new <- -max.exp
    logit.th.other.new <- max.exp + 2
    logit.th.old <- rnorm(1, sd = 3)
    logit.th.other.old <- rnorm(1, sd = 3)
    scale <- runif(1, 0.01, 2)
    weight <- runif(1, 0.1, 2)
    weight.other <- runif(1, 0.1, 2)
    ans.obtained <- safeLogProp_Binomial(logit.th.new = logit.th.new,
                                         logit.th.other.new = logit.th.other.new,
                                         logit.th.old = logit.th.old,
                                         logit.th.other.old = logit.th.other.old,
                                         scale = scale,
                                         weight = weight,
                                         weight.other = weight.other)
    coef2 <- exp(-2) + 2 + exp(2)
    r <- abs(weight / weight.other)
    ans.expected <- logit.th.new + log(dnorm(logit.th.new, mean = logit.th.old, sd = scale)
                                       + r * coef2 * dnorm(logit.th.other.new, mean = logit.th.other.old, sd = scale))
    expect_equal(ans.obtained, ans.expected)
})

test_that("R and C versions of safeLogProp_Binomial give same answer", {
    safeLogProp_Binomial <- demest:::safeLogProp_Binomial
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        logit.th.new <- rnorm(1, sd = 3)
        logit.th.other.new <- rnorm(1, sd = 3)
        logit.th.old <- rnorm(1, sd = 3)
        logit.th.other.old <- rnorm(1, sd = 3)
        scale <- runif(1, 0.01, 2)
        weight <- runif(1, 0.1, 2)
        weight.other <- runif(1, 0.1, 2)
        ans.R <- safeLogProp_Binomial(logit.th.new = logit.th.new,
                                      logit.th.other.new = logit.th.other.new,
                                      logit.th.old = logit.th.old,
                                      logit.th.other.old = logit.th.other.old,
                                      scale = scale,
                                      weight = weight,
                                      weight.other = weight.other,
                                      useC = FALSE)
        ans.C <- safeLogProp_Binomial(logit.th.new = logit.th.new,
                                      logit.th.other.new = logit.th.other.new,
                                      logit.th.old = logit.th.old,
                                      logit.th.other.old = logit.th.other.old,
                                      scale = scale,
                                      weight = weight,
                                      weight.other = weight.other,
                                      useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
    ## logit.th.new very large
    max.exp <- 1.0 * .Machine$double.max.exp
    logit.th.new <- max.exp
    logit.th.other.new <- max.exp - 2
    logit.th.old <- rnorm(1, sd = 3)
    logit.th.other.old <- rnorm(1, sd = 3)
    scale <- runif(1, 0.01, 2)
    weight <- runif(1, 0.1, 2)
    weight.other <- runif(1, 0.1, 2)
    ans.R <- safeLogProp_Binomial(logit.th.new = logit.th.new,
                                  logit.th.other.new = logit.th.other.new,
                                  logit.th.old = logit.th.old,
                                  logit.th.other.old = logit.th.other.old,
                                  scale = scale,
                                  weight = weight,
                                  weight.other = weight.other,
                                  useC = FALSE)
    ans.C <- safeLogProp_Binomial(logit.th.new = logit.th.new,
                                  logit.th.other.new = logit.th.other.new,
                                  logit.th.old = logit.th.old,
                                  logit.th.other.old = logit.th.other.old,
                                  scale = scale,
                                  weight = weight,
                                  weight.other = weight.other,
                                  useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
    ## logit.th.new very small
    max.exp <- 1.0 * .Machine$double.max.exp
    logit.th.new <- -max.exp
    logit.th.other.new <- max.exp + 2
    logit.th.old <- rnorm(1, sd = 3)
    logit.th.other.old <- rnorm(1, sd = 3)
    scale <- runif(1, 0.01, 2)
    weight <- runif(1, 0.1, 2)
    weight.other <- runif(1, 0.1, 2)
    ans.R <- safeLogProp_Binomial(logit.th.new = logit.th.new,
                                  logit.th.other.new = logit.th.other.new,
                                  logit.th.old = logit.th.old,
                                  logit.th.other.old = logit.th.other.old,
                                  scale = scale,
                                  weight = weight,
                                  weight.other = weight.other,
                                  useC = FALSE)
    ans.C <- safeLogProp_Binomial(logit.th.new = logit.th.new,
                                  logit.th.other.new = logit.th.other.new,
                                  logit.th.old = logit.th.old,
                                  logit.th.other.old = logit.th.other.old,
                                  scale = scale,
                                  weight = weight,
                                  weight.other = weight.other,
                                  useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
})

test_that("safeLogProp_Poisson gives valid answers", {
    safeLogProp_Poisson <- demest:::safeLogProp_Poisson
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        log.th.new <- rnorm(1, sd = 3)
        log.th.other.new <- rnorm(1, sd = 3)
        log.th.old <- rnorm(1, sd = 3)
        log.th.other.old <- rnorm(1, sd = 3)
        scale <- runif(1, 0.01, 2)
        weight <- runif(1, 0.1, 2)
        weight.other <- runif(1, 0.1, 2)
        ans.obtained <- safeLogProp_Poisson(log.th.new = log.th.new,
                                            log.th.other.new = log.th.other.new,
                                            log.th.old = log.th.old,
                                            log.th.other.old = log.th.other.old,
                                            weight = weight,
                                            weight.other = weight.other,
                                            scale = scale)
        coef1 <- exp(-log.th.new)
        coef2 <- exp(-log.th.other.new)
        r <- abs(weight / weight.other)
        ans.expected <- log(coef1 * dnorm(log.th.new, mean = log.th.old, sd = scale)
                            + r * coef2 * dnorm(log.th.other.new, mean = log.th.other.old, sd = scale))
        ## print(c(log.th.new, log.th.other.new, log.th.old, log.th.other.old, scale))
        ## print(c(ans.obtained, ans.expected))
        expect_equal(ans.obtained, ans.expected)
    }
    ## log.th.new very large
    max.exp <- 1.0 * .Machine$double.max.exp
    log.th.new <- max.exp
    log.th.other.new <- max.exp - 2
    log.th.old <- rnorm(1, sd = 3)
    log.th.other.old <- rnorm(1, sd = 3)
    scale <- runif(1, 0.01, 2)
    weight <- runif(1, 0.1, 2)
    weight.other <- runif(1, 0.1, 2)
    ans.obtained <- safeLogProp_Poisson(log.th.new = log.th.new,
                                        log.th.other.new = log.th.other.new,
                                        log.th.old = log.th.old,
                                        log.th.other.old = log.th.other.old,
                                        weight = weight,
                                        weight.other = weight.other,
                                        scale = scale)
    coef1 <- exp(-log.th.new)
    coef2 <- exp(-log.th.other.new)
    r <- abs(weight / weight.other)
    ans.expected <- log(coef1 * dnorm(log.th.new, mean = log.th.old, sd = scale)
                        + r * coef2 * dnorm(log.th.other.new, mean = log.th.other.old, sd = scale))
    expect_equal(ans.obtained, ans.expected)
    ## log.th.new very small
    max.exp <- 1.0 * .Machine$double.max.exp
    log.th.new <- -max.exp
    log.th.other.new <- max.exp + 2
    log.th.old <- rnorm(1, sd = 3)
    log.th.other.old <- rnorm(1, sd = 3)
    scale <- runif(1, 0.01, 2)
    weight <- runif(1, 0.1, 2)
    weight.other <- runif(1, 0.1, 2)
    ans.obtained <- safeLogProp_Poisson(log.th.new = log.th.new,
                                        log.th.other.new = log.th.other.new,
                                        log.th.old = log.th.old,
                                        log.th.other.old = log.th.other.old,
                                        weight = weight,
                                        weight.other = weight.other,
                                        scale = scale)
    coef2 <- exp(-2)
    r <- abs(weight / weight.other)
    ans.expected <- -log.th.new + log(dnorm(log.th.new, mean = log.th.old, sd = scale)
                                     + r * coef2 * dnorm(log.th.other.new, mean = log.th.other.old, sd = scale))
    expect_equal(ans.obtained, ans.expected)
})

test_that("R and C versions of safeLogProp_Poisson give same answer", {
    safeLogProp_Poisson <- demest:::safeLogProp_Poisson
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        log.th.new <- rnorm(1, sd = 3)
        log.th.other.new <- rnorm(1, sd = 3)
        log.th.old <- rnorm(1, sd = 3)
        log.th.other.old <- rnorm(1, sd = 3)
        scale <- runif(1, 0.01, 2)
        weight <- runif(1, 0.1, 2)
        weight.other <- runif(1, 0.1, 2)
        ans.R <- safeLogProp_Poisson(log.th.new = log.th.new,
                                     log.th.other.new = log.th.other.new,
                                     log.th.old = log.th.old,
                                     log.th.other.old = log.th.other.old,
                                     scale = scale,
                                     weight = weight,
                                     weight.other = weight.other,
                                     useC = FALSE)
        ans.C <- safeLogProp_Poisson(log.th.new = log.th.new,
                                     log.th.other.new = log.th.other.new,
                                     log.th.old = log.th.old,
                                     log.th.other.old = log.th.other.old,
                                     weight = weight,
                                     weight.other = weight.other,
                                     scale = scale,
                                     useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
    ## log.th.new very large
    max.exp <- 1.0 * .Machine$double.max.exp
    log.th.new <- max.exp
    log.th.other.new <- max.exp - 2
    log.th.old <- rnorm(1, sd = 3)
    log.th.other.old <- rnorm(1, sd = 3)
    scale <- runif(1, 0.01, 2)
    weight <- runif(1, 0.1, 2)
    weight.other <- runif(1, 0.1, 2)
    ans.R <- safeLogProp_Poisson(log.th.new = log.th.new,
                                 log.th.other.new = log.th.other.new,
                                 log.th.old = log.th.old,
                                 log.th.other.old = log.th.other.old,
                                 scale = scale,
                                 weight = weight,
                                 weight.other = weight.other,
                                 useC = FALSE)
    ans.C <- safeLogProp_Poisson(log.th.new = log.th.new,
                                 log.th.other.new = log.th.other.new,
                                 log.th.old = log.th.old,
                                 log.th.other.old = log.th.other.old,
                                 weight = weight,
                                 weight.other = weight.other,
                                 scale = scale,
                                 useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
    ## log.th.new very small
    max.exp <- 1.0 * .Machine$double.max.exp
    log.th.new <- -max.exp
    log.th.other.new <- max.exp + 2
    log.th.old <- rnorm(1, sd = 3)
    log.th.other.old <- rnorm(1, sd = 3)
    scale <- runif(1, 0.01, 2)
    weight <- runif(1, 0.1, 2)
    weight.other <- runif(1, 0.1, 2)
    ans.R <- safeLogProp_Poisson(log.th.new = log.th.new,
                                 log.th.other.new = log.th.other.new,
                                 log.th.old = log.th.old,
                                 log.th.other.old = log.th.other.old,
                                 weight = weight,
                                 weight.other = weight.other,
                                 scale = scale,
                                 useC = FALSE)
    ans.C <- safeLogProp_Poisson(log.th.new = log.th.new,
                                 log.th.other.new = log.th.other.new,
                                 log.th.old = log.th.old,
                                 log.th.other.old = log.th.other.old,
                                 scale = scale,
                                 weight = weight,
                                 weight.other = weight.other,
                                 useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
})


## MONITORING ######################################################################

test_that("sweepMargins gives vald answer", {
    sweepMargins <- demest:::sweepMargins
    ## dim = c(4, 3), without iteration dimension
    x <- matrix(rnorm(12), nrow = 4)
    x <- x - mean(x)
    margins <- list(1L, 2L)
    ans.obtained <- sweepMargins(x, margins = margins)
    expect_equal(mean(ans.obtained), 0)
    expect_equal(rowMeans(ans.obtained), rep(0, 4))
    expect_equal(colMeans(ans.obtained), rep(0, 3))
    ans.expected <- x - rowMeans(x)
    ans.expected <- ans.expected - rep(colMeans(ans.expected), each = 4)
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
    ## dim = c(4, 3, 5), without iteration dimension
    x <- array(rnorm(60), dim = c(4, 3, 5))
    x <- x - mean(x)
    margins <- list(1L, 2L, 3L, 1:2, c(1L, 3L), 2:3)
    ans.obtained <- sweepMargins(x, margins = margins)
    for (margin in margins) {
        tmp1 <- apply(ans.obtained, margin, mean)
        tmp1 <- as.numeric(tmp1)
        tmp2 <- rep(0, times = length(tmp1))
        expect_equal(tmp1, tmp2)
    }
    ## dim = c(4, 3), dim 2 is iteration
    x <- matrix(rnorm(12), nrow = 4)
    margins <- list(2L)
    ans.obtained <- sweepMargins(x, margins = margins)
    ans.expected <- x - rep(colMeans(x), each = 4)
    expect_equal(ans.obtained, ans.expected)
    ## dim = c(4, 3, 5), dim 3 is iteration
    x <- array(rnorm(60), dim = c(4, 3, 5))
    margins <- list(c(1L, 3L), c(2L, 3L))
    ans.obtained <- sweepMargins(x, margins = margins)
    for (margin in margins) {
        tmp1 <- apply(ans.obtained, margin, mean)
        tmp1 <- as.numeric(tmp1)
        tmp2 <- rep(0, times = length(tmp1))
        expect_equal(tmp1, tmp2)
    }
    ## dim = c(6, 5, 3, 2), dim 1 is  iteration
    x <- array(rnorm(180), dim = c(6, 5, 3, 2))
    margins <- list(c(1L, 2L), c(1L, 3L), c(1L, 4L),
                    c(1L, 2L, 3L), c(1L, 2L, 4L), c(1L, 3L, 4L))
    ans.obtained <- sweepMargins(x, margins = margins)
    for (margin in margins) {
        tmp1 <- apply(ans.obtained, margin, mean)
        tmp1 <- as.numeric(tmp1)
        tmp2 <- rep(0, times = length(tmp1))
        expect_equal(tmp1, tmp2)
    }
})
    


## UPDATING COUNTS ####################################################################

test_that("R version of diffLogLik works", {
    diffLogLik <- demest:::diffLogLik
    initialModel <- demest:::initialModel
    getIAfter <- dembase::getIAfter
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    logLikelihood <- demest:::logLikelihood
    set.seed(100)
    yProp <- as.integer(rpois(n = 1, lambda = 10))
    y <- Counts(array(as.integer(rpois(24, lambda = 10)),
                      dim = c(6, 4),
                      dimnames = list(age = 0:5, reg = letters[1:4])))
    datasets <- list(Counts(array(as.integer(rpois(3, lambda = 20)),
                                  dim = 3,
                                  dimnames = list(reg = letters[1:3]))),
                     Counts(array(as.integer(rpois(18, lambda = 10)),
                                  dim = c(6, 3),
                                  dimnames = list(age = 0:5, reg = letters[1:3]))))
    data.models <- vector("list", 2)
    transforms <- vector("list", 2)
    for (i in 1:2) {
        transforms[[i]] <- makeCollapseTransformExtra(makeTransform(x = y,
                                                             y = datasets[[i]],
                                                             subset = TRUE))
        data.models[[i]] <- initialModel(Model(y ~ Poisson(mean ~ 1)),
                                         y = datasets[[i]],
                                         exposure = dembase::collapse(y, transforms[[i]]))
    }
    ## indicesY has length 1 and cell from y has corresponding cell in datasets
    indicesY <- 1L
    yProp <- y[1L] + 1L
    ans.obtained <- diffLogLik(yProp = yProp,
                               y = y,
                               indicesY = indicesY,
                               dataModels = data.models,
                               datasets = datasets,
                               transforms = transforms)
    ans.expected <- (logLikelihood(model = data.models[[1]],
                                   count = sum(y[1:6]) + 1L,
                                   dataset = datasets[[1]],
                                   i = 1L) -
                     logLikelihood(model = data.models[[1]],
                                   count = sum(y[1:6]),
                                   dataset = datasets[[1]],
                                   i = 1L) +
                     logLikelihood(model = data.models[[2]],
                                   count = y[1] + 1L,
                                   dataset = datasets[[2]],
                                   i = 1L) -
                     logLikelihood(model = data.models[[2]],
                                   count = y[1],
                                   dataset = datasets[[2]],
                                   i = 1L))
    expect_identical(ans.obtained, ans.expected)
    ## indicesY has length 1 and cell from y does not have corresponding cell in datasets
    indicesY <- 19L
    yProp <- y[1L] + 1L
    ans.obtained <- diffLogLik(yProp = yProp,
                               y = y,
                               indicesY = indicesY,
                               dataModels = data.models,
                               datasets = datasets,
                               transforms = transforms)
    ans.expected <- 0
    expect_identical(ans.obtained, ans.expected)
    ## indicesY has length 2 and both cells from y have corresponding cells in datasets
    indicesY <- c(7L, 13L)
    yProp <- y[indicesY] + c(-1L, 1L)
    ans.obtained <- diffLogLik(yProp = yProp,
                               y = y,
                               indicesY = indicesY,
                               dataModels = data.models,
                               datasets = datasets,
                               transforms = transforms)
    ans.expected <- (logLikelihood(model = data.models[[1]],
                                   count = sum(y[7:12]) - 1L,
                                   dataset = datasets[[1]],
                                   i = 2L) -
                     logLikelihood(model = data.models[[1]],
                                   count = sum(y[7:12]),
                                   dataset = datasets[[1]],
                                   i = 2L) +
                     logLikelihood(model = data.models[[2]],
                                   count = y[7] - 1L,
                                   dataset = datasets[[2]],
                                   i = 7L) -
                     logLikelihood(model = data.models[[2]],
                                   count = y[7],
                                   dataset = datasets[[2]],
                                   i = 7L) +
                     logLikelihood(model = data.models[[1]],
                                   count = sum(y[13:18]) + 1L,
                                   dataset = datasets[[1]],
                                   i = 3L) -
                     logLikelihood(model = data.models[[1]],
                                   count = sum(y[13:18]),
                                   dataset = datasets[[1]],
                                   i = 3L) +
                     logLikelihood(model = data.models[[2]],
                                   count = y[13] + 1L,
                                   dataset = datasets[[2]],
                                   i = 13L) -
                     logLikelihood(model = data.models[[2]],
                                   count = y[13],
                                   dataset = datasets[[2]],
                                   i = 13L))
    expect_identical(ans.obtained, ans.expected)
    ## indicesY has length 2 and neither cell has correponding cells in datasets
    indicesY <- c(21L, 22L)
    yProp <- y[indicesY] + c(-1L, 1L)
    ans.obtained <- diffLogLik(yProp = yProp,
                               y = y,
                               indicesY = indicesY,
                               dataModels = data.models,
                               datasets = datasets,
                               transforms = transforms)
    ans.expected <- 0
    expect_identical(ans.obtained, ans.expected)
    ## indicesY has length 2, one cell has data, and other does not
    indicesY <- c(18L, 24L)
    yProp <- y[indicesY] + c(2L, -2L)
    ans.obtained <- diffLogLik(yProp = yProp,
                               y = y,
                               indicesY = indicesY,
                               dataModels = data.models,
                               datasets = datasets,
                               transforms = transforms)
    ans.expected <- (logLikelihood(model = data.models[[1]],
                                   count = sum(y[13:18]) + 2L,
                                   dataset = datasets[[1]],
                                   i = 3L) -
                     logLikelihood(model = data.models[[1]],
                                   count = sum(y[13:18]),
                                   dataset = datasets[[1]],
                                   i = 3L) +
                     logLikelihood(model = data.models[[2]],
                                   count = y[18] + 2L,
                                   dataset = datasets[[2]],
                                   i = 18L) -
                     logLikelihood(model = data.models[[2]],
                                   count = y[18],
                                   dataset = datasets[[2]],
                                   i = 18L))
    expect_identical(ans.obtained, ans.expected)
    ## indicesY has length 1 and proposal has 0 likelihood
    indicesY <- 3L
    stopifnot(datasets[[2]][3] > 0L)
    yProp <- 0L
    ans.obtained <- diffLogLik(yProp = yProp,
                               y = y,
                               indicesY = indicesY,
                               dataModels = data.models,
                               datasets = datasets,
                               transforms = transforms)
    ans.expected <- -Inf
    expect_identical(ans.obtained, ans.expected)
    ## indicesY has length 2 and proposal has 0 likelihood
    indicesY <- c(3L, 9L)
    stopifnot(datasets[[2]][0] > 0L)
    yProp <- c(y[3] + y[9], 0L)
    ans.obtained <- diffLogLik(yProp = yProp,
                               y = y,
                               indicesY = indicesY,
                               dataModels = data.models,
                               datasets = datasets,
                               transforms = transforms)
    ans.expected <- -Inf
    expect_identical(ans.obtained, ans.expected)
    ## dataset has missing value
    indicesY <- 1L
    yProp <- as.integer(rpois(n = 1, lambda = 10))
    y <- Counts(array(as.integer(rpois(24, lambda = 10)),
                      dim = c(6, 4),
                      dimnames = list(age = 0:5, reg = letters[1:4])))
    datasets <- list(Counts(array(as.integer(rpois(24, lambda = 5)),
                                  dim = c(6, 4),
                                  dimnames = list(age = 0:5, reg = letters[1:4]))))
    datasets[[1]][1] <- NA
    transforms <- list(makeCollapseTransformExtra(makeTransform(x = y,
                                                                y = datasets[[1]])))
    data.models <- list(initialModel(Model(y ~ Poisson(mean ~ 1)),
                                     y = datasets[[1]],
                                     exposure = y))
    ans.obtained <- diffLogLik(yProp = yProp,
                               y = y,
                               indicesY = indicesY,
                               dataModels = data.models,
                               datasets = datasets,
                               transforms = transforms)
    ans.expected <- 0
    expect_identical(ans.obtained, ans.expected)
})

## tests equal but not identical
test_that("R and C versions of diffLogLik give same answer, part 1", {
    diffLogLik <- demest:::diffLogLik
    initialModel <- demest:::initialModel
    getIAfter <- dembase::getIAfter
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    logLikelihood <- demest:::logLikelihood
    set.seed(100)
    yProp <- as.numeric(rpois(n = 1, lambda = 10))
    y <- Counts(array(as.integer(rpois(24, lambda = 10)),
                      dim = c(6, 4),
                      dimnames = list(age = 0:5, reg = letters[1:4])))
    datasets <- list(Counts(array(as.integer(rpois(3, lambda = 20)),
                                  dim = 3,
                                  dimnames = list(reg = letters[1:3]))),
                     Counts(array(as.integer(rpois(18, lambda = 10)),
                                  dim = c(6, 3),
                                  dimnames = list(age = 0:5, reg = letters[1:3]))))
    data.models <- vector("list", 2)
    transforms <- vector("list", 2)
    for (i in 1:2) {
        transforms[[i]] <- makeCollapseTransformExtra(makeTransform(x = y,
                                                             y = datasets[[i]],
                                                             subset = TRUE))
        data.models[[i]] <- initialModel(Model(y ~ Poisson(mean ~ 1)),
                                         y = datasets[[i]],
                                         exposure = dembase::collapse(y, transforms[[i]]))
    }
    ## indicesY has length 1
    for (i in seq_along(y)) {
        yProp <- as.integer(rpois(n = 1, lambda = 10))
        ans.R <- diffLogLik(yProp = yProp,
                            y = y,
                            indicesY = i,
                            dataModels = data.models,
                            datasets = datasets,
                            transforms = transforms,
                            useC = FALSE)
        ans.C <- diffLogLik(yProp = yProp,
                            y = y,
                            indicesY = i,
                            dataModels = data.models,
                            datasets = datasets,
                            transforms = transforms,
                            useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
    ## indicesY has length 2
    for (i in 1:18) {
        indicesY = c(i, i + 6L)
        yProp <- as.integer(rmultinom(n = 1, size = sum(y[indicesY]), prob = c(0.5, 0.5)))
        ans.R <- diffLogLik(yProp = yProp,
                            y = y,
                            indicesY = indicesY,
                            dataModels = data.models,
                            datasets = datasets,
                            transforms = transforms,
                            useC = FALSE)
        ans.C <- diffLogLik(yProp = yProp,
                            y = y,
                            indicesY = indicesY,
                            dataModels = data.models,
                            datasets = datasets,
                            transforms = transforms,
                            useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
    ## dataset has missing value
    indicesY <- 1L
    yProp <- as.integer(rpois(n = 1, lambda = 10))
    y <- Counts(array(as.integer(rpois(24, lambda = 10)),
                      dim = c(6, 4),
                      dimnames = list(age = 0:5, reg = letters[1:4])))
    datasets <- list(Counts(array(as.integer(rpois(24, lambda = 5)),
                                  dim = c(6, 4),
                                  dimnames = list(age = 0:5, reg = letters[1:4]))))
    datasets[[1]][1] <- NA
    transforms <- list(makeCollapseTransformExtra(makeTransform(x = y,
                                                                y = datasets[[1]])))
    data.models <- list(initialModel(Model(y ~ Poisson(mean ~ 1)),
                                     y = datasets[[1]],
                                     exposure = y))
    ans.R <- diffLogLik(yProp = yProp,
                               y = y,
                               indicesY = indicesY,
                               dataModels = data.models,
                               datasets = datasets,
                        transforms = transforms,
                        useC = FALSE)
    ans.C <- diffLogLik(yProp = yProp,
                               y = y,
                               indicesY = indicesY,
                               dataModels = data.models,
                               datasets = datasets,
                        transforms = transforms,
                        useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
})

## tests identical (-inf outcome tests)
test_that("R and C versions of diffLogLik give same answer, part 2", {
    diffLogLik <- demest:::diffLogLik
    initialModel <- demest:::initialModel
    getIAfter <- dembase::getIAfter
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    logLikelihood <- demest:::logLikelihood
    safeLogProp_Poisson <- demest:::safeLogProp_Poisson
    set.seed(100)
    yProp <- as.integer(rpois(n = 1, lambda = 10))
    y <- Counts(array(as.integer(rpois(24, lambda = 10)),
                      dim = c(6, 4),
                      dimnames = list(age = 0:5, reg = letters[1:4])))
    datasets <- list(Counts(array(as.integer(rpois(3, lambda = 20)),
                                  dim = 3,
                                  dimnames = list(reg = letters[1:3]))),
                     Counts(array(as.integer(rpois(18, lambda = 10)),
                                  dim = c(6, 3),
                                  dimnames = list(age = 0:5, reg = letters[1:3]))))
    data.models <- vector("list", 2)
    transforms <- vector("list", 2)
    for (i in 1:2) {
        transforms[[i]] <- makeCollapseTransformExtra(makeTransform(x = y,
                                                             y = datasets[[i]],
                                                             subset = TRUE))
        data.models[[i]] <- initialModel(Model(y ~ Poisson(mean ~ 1)),
                                         y = datasets[[i]],
                                         exposure = dembase::collapse(y, transforms[[i]]))
    }
    ## log.th.new very large
    max.exp <- 1.0 * .Machine$double.max.exp
    log.th.new <- max.exp
    log.th.other.new <- max.exp - 2
    log.th.old <- rnorm(1, sd = 3)
    log.th.other.old <- rnorm(1, sd = 3)
    scale <- runif(1, 0.01, 2)
    weight <- runif(1, 0.1, 2)
    weight.other <- runif(1, 0.1, 2)
    ans.obtained <- safeLogProp_Poisson (log.th.new = log.th.new,
                                        log.th.other.new = log.th.other.new,
                                        log.th.old = log.th.old,
                                        log.th.other.old = log.th.other.old,
                                        weight = weight,
                                        weight.other = weight.other,
                                        scale = scale)
    coef1 <- exp(-log.th.new)
    coef2 <- exp(-log.th.other.new)
    r <- abs(weight / weight.other)
    ans.expected <- log(coef1 * dnorm(log.th.new, mean = log.th.old, sd = scale)
                        + r * coef2 * dnorm(log.th.other.new, mean = log.th.other.old, sd = scale))
    expect_equal(ans.obtained, ans.expected)
    ## log.th.new very small
    max.exp <- 1.0 * .Machine$double.max.exp
    log.th.new <- -max.exp
    log.th.other.new <- max.exp + 2
    log.th.old <- rnorm(1, sd = 3)
    log.th.other.old <- rnorm(1, sd = 3)
    scale <- runif(1, 0.01, 2)
    weight <- runif(1, 0.1, 2)
    weight.other <- runif(1, 0.1, 2)
    ans.obtained <- safeLogProp_Poisson(log.th.new = log.th.new,
                                        log.th.other.new = log.th.other.new,
                                        log.th.old = log.th.old,
                                        log.th.other.old = log.th.other.old,
                                        weight = weight,
                                        weight.other = weight.other,
                                        scale = scale)
    coef2 <- exp(-2)
    r <- abs(weight / weight.other)
    ans.expected <- -log.th.new + log(dnorm(log.th.new, mean = log.th.old, sd = scale)
                                     + r * coef2 * dnorm(log.th.other.new, mean = log.th.other.old, sd = scale))
    expect_equal(ans.obtained, ans.expected)
})

test_that("R and C versions of safeLogProp_Poisson give same answer", {
    safeLogProp_Poisson <- demest:::safeLogProp_Poisson
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        log.th.new <- rnorm(1, sd = 3)
        log.th.other.new <- rnorm(1, sd = 3)
        log.th.old <- rnorm(1, sd = 3)
        log.th.other.old <- rnorm(1, sd = 3)
        scale <- runif(1, 0.01, 2)
        weight <- runif(1, 0.1, 2)
        weight.other <- runif(1, 0.1, 2)
        ans.R <- safeLogProp_Poisson(log.th.new = log.th.new,
                                     log.th.other.new = log.th.other.new,
                                     log.th.old = log.th.old,
                                     log.th.other.old = log.th.other.old,
                                     scale = scale,
                                     weight = weight,
                                     weight.other = weight.other,
                                     useC = FALSE)
        ans.C <- safeLogProp_Poisson(log.th.new = log.th.new,
                                     log.th.other.new = log.th.other.new,
                                     log.th.old = log.th.old,
                                     log.th.other.old = log.th.other.old,
                                     weight = weight,
                                     weight.other = weight.other,
                                     scale = scale,
                                     useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
    ## log.th.new very large
    max.exp <- 1.0 * .Machine$double.max.exp
    log.th.new <- max.exp
    log.th.other.new <- max.exp - 2
    log.th.old <- rnorm(1, sd = 3)
    log.th.other.old <- rnorm(1, sd = 3)
    scale <- runif(1, 0.01, 2)
    weight <- runif(1, 0.1, 2)
    weight.other <- runif(1, 0.1, 2)
    ans.R <- safeLogProp_Poisson(log.th.new = log.th.new,
                                 log.th.other.new = log.th.other.new,
                                 log.th.old = log.th.old,
                                 log.th.other.old = log.th.other.old,
                                 scale = scale,
                                 weight = weight,
                                 weight.other = weight.other,
                                 useC = FALSE)
    ans.C <- safeLogProp_Poisson(log.th.new = log.th.new,
                                 log.th.other.new = log.th.other.new,
                                 log.th.old = log.th.old,
                                 log.th.other.old = log.th.other.old,
                                 weight = weight,
                                 weight.other = weight.other,
                                 scale = scale,
                                 useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
    ## log.th.new very small
    max.exp <- 1.0 * .Machine$double.max.exp
    log.th.new <- -max.exp
    log.th.other.new <- max.exp + 2
    log.th.old <- rnorm(1, sd = 3)
    log.th.other.old <- rnorm(1, sd = 3)
    scale <- runif(1, 0.01, 2)
    weight <- runif(1, 0.1, 2)
    weight.other <- runif(1, 0.1, 2)
    ans.R <- safeLogProp_Poisson(log.th.new = log.th.new,
                                 log.th.other.new = log.th.other.new,
                                 log.th.old = log.th.old,
                                 log.th.other.old = log.th.other.old,
                                 weight = weight,
                                 weight.other = weight.other,
                                 scale = scale,
                                 useC = FALSE)
    ans.C <- safeLogProp_Poisson(log.th.new = log.th.new,
                                 log.th.other.new = log.th.other.new,
                                 log.th.old = log.th.old,
                                 log.th.other.old = log.th.other.old,
                                 scale = scale,
                                 weight = weight,
                                 weight.other = weight.other,
                                 useC = TRUE)
    if (test.identity)
        expect_identical(ans.R, ans.C)
    else
        expect_equal(ans.R, ans.C)
})


test_that("logLikelihood_Binomial gives valid answer", {
    logLikelihood_Binomial <- demest:::logLikelihood_Binomial
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        exposure <- Counts(array(20 * rpois(n = 20, lambda = 10),
                                 dim = c(5, 4),
                                 dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        spec <- Model(y ~ Binomial(mean ~ age + region))
        model <- initialModel(spec, y = y, exposure = exposure)
        dataset <- Counts(array(as.integer(rpois(n = 20, lambda = 20)),
                                dim = c(2, 10),
                                dimnames = list(sex = c("f", "m"), age = 0:9)))
        i <- sample.int(20, size = 1)
        count <- as.integer(rpois(n = 1, lambda = dataset[i] * 1.5))
        ans.obtained <- logLikelihood_Binomial(model = model,
                                               count = count,
                                               dataset = dataset,
                                               i = i)
        ans.expected <- dbinom(x = dataset[i], size = count, prob = model@theta[i], log = TRUE)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R, and C versions of logLikelihood_Binomial give same answer", {
    logLikelihood_Binomial <- demest:::logLikelihood_Binomial
    initialModel <- demest:::initialModel
    ## tests where -inf unlikely
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        exposure <- Counts(array(20 * rpois(n = 20, lambda = 10),
                                 dim = c(5, 4),
                                 dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        spec <- Model(y ~ Binomial(mean ~ age + region))
        model <- initialModel(spec, y = y, exposure = exposure)
        dataset <- Counts(array(as.integer(rpois(n = 20, lambda = 20)),
                                dim = c(2, 10),
                                dimnames = list(sex = c("f", "m"), age = 0:9)))
        i <- sample.int(20, size = 1)
        count <- as.integer(rpois(n = 1, lambda = dataset[i] * 1.5))
        ans.R <- logLikelihood_Binomial(model = model,
                                        count = count,
                                        dataset = dataset,
                                        i = i,
                                        useC = FALSE)
        ans.C <- logLikelihood_Binomial(model = model,
                                        count = count,
                                        dataset = dataset,
                                        i = i,
                                        useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
    ## tests where -inf likely
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        exposure <- Counts(array(20 * rpois(n = 20, lambda = 10),
                                 dim = c(5, 4),
                                 dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        spec <- Model(y ~ Binomial(mean ~ age + region))
        model <- initialModel(spec, y = y, exposure = exposure)
        dataset <- Counts(array(as.integer(rpois(n = 20, lambda = 20)),
                                dim = c(2, 10),
                                dimnames = list(sex = c("f", "m"), age = 0:9)))
        i <- sample.int(20, size = 1)
        count <- as.integer(rpois(n = 1, lambda = dataset[i] * 0.5))
        ans.R <- logLikelihood_Binomial(model = model,
                                        count = count,
                                        dataset = dataset,
                                        i = i,
                                        useC = FALSE)
        ans.C <- logLikelihood_Binomial(model = model,
                                        count = count,
                                        dataset = dataset,
                                        i = i,
                                        useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})


test_that("logLikelihood_CMP gives valid answer", {
    initialModel <- demest:::initialModel
    logLikelihood_CMP <- demest:::logLikelihood_CMP
    logDensCMPUnnormalised1 <- demest:::logDensCMPUnnormalised1
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        exposure <- Counts(array(20 * rpois(n = 20, lambda = 10),
                                 dim = c(5, 4),
                                 dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        spec <- Model(y ~ CMP(mean ~ age + region))
        model <- initialModel(spec, y = y, exposure = exposure)
        dataset <- Counts(array(as.integer(rpois(n = 20, lambda = 20)),
                                dim = c(2, 10),
                                dimnames = list(sex = c("f", "m"), age = 0:9)))
        i <- sample.int(20, size = 1)
        count <- as.integer(rpois(n = 1, lambda = dataset[i]))
        ans.obtained <- logLikelihood_CMP(model = model,
                                          count = count,
                                          dataset = dataset,
                                          i = i)
        ans.expected <- logDensCMPUnnormalised1(x = dataset[i],
                                                gamma = count * model@theta[i],
                                                nu = model@nuCMP[i])
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of logLikelihood_CMP give same answer", {
    initialModel <- demest:::initialModel
    logLikelihood_CMP <- demest:::logLikelihood_CMP
    logDensCMPUnnormalised1 <- demest:::logDensCMPUnnormalised1
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        exposure <- Counts(array(20 * rpois(n = 20, lambda = 10),
                                 dim = c(5, 4),
                                 dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        spec <- Model(y ~ CMP(mean ~ age + region))
        model <- initialModel(spec, y = y, exposure = exposure)
        dataset <- Counts(array(as.integer(rpois(n = 20, lambda = 20)),
                                dim = c(2, 10),
                                dimnames = list(sex = c("f", "m"), age = 0:9)))
        i <- sample.int(20, size = 1)
        count <- as.integer(rpois(n = 1, lambda = dataset[i]))
        ans.R <- logLikelihood_CMP(model = model,
                                   count = count,
                                   dataset = dataset,
                                   i = i,
                                   useC = FALSE)
        ans.C <- logLikelihood_CMP(model = model,
                                   count = count,
                                   dataset = dataset,
                                   i = i,
                                   useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("logLikelihood_Poisson gives valid answer", {
    initialModel <- demest:::initialModel
    logLikelihood_Poisson <- demest:::logLikelihood_Poisson
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        exposure <- Counts(array(20 * rpois(n = 20, lambda = 10),
                                 dim = c(5, 4),
                                 dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        spec <- Model(y ~ Poisson(mean ~ age + region))
        model <- initialModel(spec, y = y, exposure = exposure)
        dataset <- Counts(array(as.integer(rpois(n = 20, lambda = 20)),
                                dim = c(2, 10),
                                dimnames = list(sex = c("f", "m"), age = 0:9)))
        i <- sample.int(20, size = 1)
        count <- as.integer(rpois(n = 1, lambda = dataset[i]))
        ans.obtained <- logLikelihood_Poisson(model = model,
                                              count = count,
                                              dataset = dataset,
                                              i = i)
        ans.expected <- dpois(x = dataset[i], lambda = count * model@theta[i], log = TRUE)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of logLikelihood_Poisson give same answer", {
    logLikelihood_Poisson <- demest:::logLikelihood_Poisson
    initialModel <- demest:::initialModel
    ## ans not -inf
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        exposure <- Counts(array(20 * rpois(n = 20, lambda = 10),
                                 dim = c(5, 4),
                                 dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        spec <- Model(y ~ Poisson(mean ~ age + region))
        model <- initialModel(spec, y = y, exposure = exposure)
        dataset <- Counts(array(as.integer(rpois(n = 20, lambda = 20)),
                                dim = c(2, 10),
                                dimnames = list(sex = c("f", "m"), age = 0:9)))
        i <- sample.int(20, size = 1)
        count <- as.integer(rpois(n = 1, lambda = dataset[i]))
        ans.R <- logLikelihood_Poisson(model = model,
                                       count = count,
                                       dataset = dataset,
                                       i = i,
                                       useC = FALSE)
        ans.C <- logLikelihood_Poisson(model = model,
                                       count = count,
                                       dataset = dataset,
                                       i = i,
                                       useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
    ## ans -inf
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        exposure <- Counts(array(20 * rpois(n = 20, lambda = 10),
                                 dim = c(5, 4),
                                 dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        spec <- Model(y ~ Poisson(mean ~ age + region))
        model <- initialModel(spec, y = y, exposure = exposure)
        dataset <- Counts(array(as.integer(rpois(n = 20, lambda = 20)),
                                dim = c(2, 10),
                                dimnames = list(sex = c("f", "m"), age = 0:9)))
        i <- sample.int(20, size = 1)
        count <- 0L
        ans.R <- logLikelihood_Poisson(model = model,
                                       count = count,
                                       dataset = dataset,
                                       i = i,
                                       useC = FALSE)
        ans.C <- logLikelihood_Poisson(model = model,
                                       count = count,
                                       dataset = dataset,
                                       i = i,
                                       useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("logLikelihood gives valid answer with PoissonBinomialMixture", {
    logLikelihood_PoissonBinomialMixture <- demest:::logLikelihood_PoissonBinomialMixture
    dpoibin1 <- demest:::dpoibin1
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        model <- new("PoissonBinomialMixture", prob = 0.9)
        dataset <- Counts(array(as.integer(rpois(n = 20, lambda = 20)),
                                dim = c(2, 10),
                                dimnames = list(sex = c("f", "m"), age = 0:9)))
        i <- sample.int(20, size = 1)
        count <- as.integer(rpois(n = 1, lambda = dataset[i]))
        ans.obtained <- logLikelihood_PoissonBinomialMixture(model = model,
                                                             count = count,
                                                             dataset = dataset,
                                                             i = i)
        ans.expected <- dpoibin1(x = dataset[i], size = count, prob = model@prob, log = TRUE)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of logLikelihood_PoissonBinomialMixture give same answer", {
    logLikelihood_PoissonBinomialMixture<- demest:::logLikelihood_PoissonBinomialMixture
    dpoibin1 <- demest:::dpoibin1
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        model <- new("PoissonBinomialMixture", prob = 0.9)
        dataset <- Counts(array(as.integer(rpois(n = 20, lambda = 20)),
                                dim = c(2, 10),
                                dimnames = list(sex = c("f", "m"), age = 0:9)))
        i <- sample.int(20, size = 1)
        count <- as.integer(rpois(n = 1, lambda = dataset[i]))
        ans.R <- logLikelihood_PoissonBinomialMixture(model = model,
                                                      count = count,
                                                      dataset = dataset,
                                                      i = i,
                                                      useC = FALSE)
        ans.C <- logLikelihood_PoissonBinomialMixture(model = model,
                                                      count = count,
                                                      dataset = dataset,
                                                      i = i,
                                                      useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("logLikelihood gives valid answer with NormalFixedUseExp", {
    logLikelihood_NormalFixedUseExp <- demest:::logLikelihood_NormalFixedUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        dataset <- Counts(array(as.integer(rpois(n = 20, lambda = 20)),
                                dim = c(2, 10),
                                dimnames = list(sex = c("f", "m"), age = 0:9)))
        mean <- Values(array(runif(20),
                             dim = c(2, 10),
                             dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ NormalFixed(mean = mean, sd = 0.1))
        model <- initialModel(spec, y = dataset, exposure = dataset)
        i <- sample.int(20, size = 1)
        count <- as.integer(rpois(n = 1, lambda = dataset[i]))
        ans.obtained <- logLikelihood_NormalFixedUseExp(model = model,
                                                        count = count,
                                                        dataset = dataset,
                                                        i = i)
        ans.expected <- dnorm(x = dataset[i], mean = count * mean@.Data[i], sd = 0.1, log = TRUE)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of logLikelihood give same answer with NormalFixedUseExp", {
    logLikelihood_NormalFixedUseExp <- demest:::logLikelihood_NormalFixedUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        dataset <- Counts(array(as.integer(rpois(n = 20, lambda = 20)),
                                dim = c(2, 10),
                                dimnames = list(sex = c("f", "m"), age = 0:9)))
        mean <- Values(array(runif(20),
                             dim = c(2, 10),
                             dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ NormalFixed(mean = mean, sd = 0.1))
        model <- initialModel(spec, y = dataset, exposure = dataset)
        i <- sample.int(20, size = 1)
        count <- as.integer(rpois(n = 1, lambda = dataset[i]))
        ans.R <- logLikelihood_NormalFixedUseExp(model = model,
                                                 count = count,
                                                 dataset = dataset,
                                                 i = i,
                                                 useC = FALSE)
        ans.C <- logLikelihood_NormalFixedUseExp(model = model,
                                                 count = count,
                                                 dataset = dataset,
                                                 i = i,
                                                 useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("logLikelihood gives valid answer with Round3", {
    logLikelihood_Round3 <- demest:::logLikelihood_Round3
    initialModel <- demest:::initialModel
    count <- CountsOne(0:6, labels = letters[1:7], name = "region")
    dataset <- CountsOne(c(0L,
                           0L, 3L, 3L,
                           3L, 6L, 9L),
                         labels = letters[1:7], name = "region")
    spec <- Model(y ~ Round3())
    model <- initialModel(spec, y = dataset, exposure = count)
    ## 0
    ans.obtained <- logLikelihood_Round3(model = model,
                                         count = count[1L],
                                         dataset = dataset,
                                         i = 1L)
    ans.expected <- 0
    expect_equal(ans.obtained, ans.expected)
    ## 1
    ans.obtained <- logLikelihood_Round3(model = model,
                                         count = count[2L],
                                         dataset = dataset,
                                         i = 2L)
    ans.expected <- log(2/3)
    expect_equal(ans.obtained, ans.expected)
    ## 2
    ans.obtained <- logLikelihood_Round3(model = model,
                                         count = count[3L],
                                         dataset = dataset,
                                         i = 3L)
    ans.expected <- log(2/3)
    expect_equal(ans.obtained, ans.expected)
    ## 3
    ans.obtained <- logLikelihood_Round3(model = model,
                                         count = count[4L],
                                         dataset = dataset,
                                         i = 4L)
    ans.expected <- 0
    expect_equal(ans.obtained, ans.expected)
    ## 4
    ans.obtained <- logLikelihood_Round3(model = model,
                                         count = count[5L],
                                         dataset = dataset,
                                         i = 5L)
    ans.expected <- log(2/3)
    expect_equal(ans.obtained, ans.expected)
    ## 5
    ans.obtained <- logLikelihood_Round3(model = model,
                                         count = count[6L],
                                         dataset = dataset,
                                         i = 6L)
    ans.expected <- log(2/3)
    expect_equal(ans.obtained, ans.expected)
    ## 6
    ans.obtained <- logLikelihood_Round3(model = model,
                                         count = count[7L],
                                         dataset = dataset,
                                         i = 7L)
    ans.expected <- -Inf
    expect_equal(ans.obtained, ans.expected)
})

test_that("R and C versions of logLikelihood give same answer with Round3", {
    logLikelihood_Round3 <- demest:::logLikelihood_Round3
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        counts <- Counts(array(as.integer(rpois(n = 20, lambda = 3)),
                               dim = c(2, 10),
                               dimnames = list(sex = c("f", "m"), age = 0:9)))
        dataset <- round3(counts)
        spec <- Model(y ~ Round3())
        model <- initialModel(spec, y = dataset, exposure = counts)
        for (i in seq_len(20)) {
            count <- counts[[i]]
            ans.R <- logLikelihood_Round3(model = model,
                                          count = count,
                                          dataset = dataset,
                                          i = i,
                                          useC = FALSE)
            ans.C <- logLikelihood_Round3(model = model,
                                          count = count,
                                          dataset = dataset,
                                          i = i,
                                          useC = TRUE)
            if (test.identity)
                expect_identical(ans.R, ans.C)
            else
                expect_equal(ans.R, ans.C)
        }
    }
})


test_that("logLikelihood gives valid answer with TFixedUseExp", {
    logLikelihood_TFixedUseExp <- demest:::logLikelihood_TFixedUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        dataset <- Counts(array(as.integer(rpois(n = 20, lambda = 20)),
                                dim = c(2, 10),
                                dimnames = list(sex = c("f", "m"), age = 0:9)))
        location <- Values(array(runif(20),
                             dim = c(2, 10),
                             dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ TFixed(location = location, scale = 0.1))
        model <- initialModel(spec, y = dataset, exposure = dataset)
        i <- sample.int(20, size = 1)
        count <- as.integer(rpois(n = 1, lambda = dataset[i]))
        ans.obtained <- logLikelihood_TFixedUseExp(model = model,
                                                   count = count,
                                                   dataset = dataset,
                                                   i = i)
        ans.expected <- dt(x = (dataset[i] - count * location@.Data[i])/0.1, df = 7, log = TRUE) - log(0.1)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of logLikelihood give same answer with TFixedUseExp", {
    logLikelihood_TFixedUseExp <- demest:::logLikelihood_TFixedUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        dataset <- Counts(array(as.integer(rpois(n = 20, lambda = 20)),
                                dim = c(2, 10),
                                dimnames = list(sex = c("f", "m"), age = 0:9)))
        location <- Values(array(runif(20),
                                 dim = c(2, 10),
                                 dimnames = list(sex = c("f", "m"), age = 0:9)))
        scale <- sqrt(location)
        spec <- Model(y ~ TFixed(location = location, scale = scale))
        model <- initialModel(spec, y = dataset, exposure = dataset)
        i <- sample.int(20, size = 1)
        count <- as.integer(rpois(n = 1, lambda = dataset[i]))
        set.seed(seed + 1)
        ans.R <- logLikelihood_TFixedUseExp(model = model,
                                            count = count,
                                            dataset = dataset,
                                            i = i,
                                            useC = FALSE)
        set.seed(seed + 1)
        ans.C <- logLikelihood_TFixedUseExp(model = model,
                                            count = count,
                                            dataset = dataset,
                                            i = i,
                                            useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})





test_that("makeIOther gives valid answers", {
    makeIOther <- demest:::makeIOther
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    getIShared <- dembase::getIShared
    ## 4x3 matrix, second dimension collapsed
    for (seed in seq_len(n.test)) {
        transform <- new("CollapseTransform",
                         indices = list(1:4, rep(1L, 3)),
                         dims = c(1L, 0L),
                         dimBefore = c(4L, 3L),
                         dimAfter = 4L)
        transform <- makeCollapseTransformExtra(transform)
        set.seed(seed)
        ans.obtained <- sapply(1:12, makeIOther, transform = transform)
        set.seed(seed)
        ans.expected <- integer(12)
        for (i in seq_along(ans.expected)) {
            shared <- getIShared(i, transform)
            if (length(shared) > 1L) {
                shared <- shared[shared != i]
                ans.expected[i] <- shared[as.integer(runif(1) * length(shared)) + 1]
            }
            else
                ans.expected[i] <- 0L
        }
        expect_identical(ans.obtained, ans.expected)
        ## 3x2 matrix, rows 1 and 3 combined
        transform <- new("CollapseTransform",
                         indices = list(c(1L, 2L, 1L), 1:2),
                         dims = 1:2,
                         dimBefore = 3:2,
                         dimAfter = c(2L, 2L))
        transform <- makeCollapseTransformExtra(transform)
        set.seed(seed)
        ans.obtained <- sapply(1:6, makeIOther, transform = transform)
        set.seed(seed)
        ans.expected <- integer(6)
        for (i in seq_along(ans.expected)) {
            shared <- getIShared(i, transform)
            if (length(shared) > 1L) {
                shared <- shared[shared != i]
                ans.expected[i] <- shared[as.integer(runif(1) * length(shared)) + 1]
            }
            else
                ans.expected[i] <- 0L
        }
        expect_identical(ans.obtained, ans.expected)
        ## 3x2x2 array, first dimension collapsed, then result transposed
        transform <- new("CollapseTransform",
                         indices = list(c(1L, 1L, 1L), 1:2, 1:2),
                         dims = c(0L, 2L, 1L),
                         dimBefore = c(3L, 2L, 2L),
                         dimAfter = c(2L, 2L))
        transform <- makeCollapseTransformExtra(transform)
        set.seed(seed)
        ans.obtained <- sapply(1:12, makeIOther, transform = transform)
        set.seed(seed)
        ans.expected <- integer(12)
        for (i in seq_along(ans.expected)) {
            shared <- getIShared(i, transform)
            if (length(shared) > 1L) {
                shared <- shared[shared != i]
                ans.expected[i] <- shared[as.integer(runif(1) * length(shared)) + 1]
            }
            else
                ans.expected[i] <- 0L
        }
        expect_identical(ans.obtained, ans.expected)
        ## 3x2 matrix, nothing changed
        transform <- new("CollapseTransform",
                         indices = list(1:3, 1:2),
                         dims = 1:2,
                         dimBefore = 3:2,
                         dimAfter = 3:2)
        transform <- makeCollapseTransformExtra(transform)
        ans.obtained <- sapply(1:6, makeIOther, transform = transform)
        ans.expected <- rep(0L, 6)
        expect_identical(ans.obtained, ans.expected)
        ## 3x2 matrix, first row dropped, rows 2 and 3 combined
        transform <- new("CollapseTransform",
                         indices = list(c(0L, 1L, 1L), 1:2),
                         dims = 0:1,
                         dimBefore = 3:2,
                         dimAfter = 2L)
        transform <- makeCollapseTransformExtra(transform)
        ans.obtained <- lapply(1:6, makeIOther, transform = transform)
        ans.expected <- list(-1L, 3L, 2L, -1L, 6L, 5L)
        expect_identical(ans.obtained, ans.expected)
        ## 4x3 matrix, last column dropped, rows 1 and 2, and rows 3 and 4 combined
        transform <- new("CollapseTransform",
                         indices = list(c(1L, 1L, 2L, 2L), c(1L, 2L, 0L)),
                         dims = 1:2,
                         dimBefore = 4:3,
                         dimAfter = c(2L, 2L))
        transform <- makeCollapseTransformExtra(transform)
        ans.obtained <- sapply(1:12, makeIOther, transform = transform)
        ans.expected <- c(2L, 1L, 4L, 3L, 6L, 5L, 8L, 7L, -1L, -1L, -1L, -1L)
        expect_identical(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of makeIOther give same answer", {
    makeIOther <- demest:::makeIOther
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    getIShared <- dembase::getIShared
    ## 4x3 matrix, second dimension collapsed
    for (seed in seq_len(n.test)) {
        transform <- new("CollapseTransform",
                         indices = list(1:4, rep(1L, 3)),
                         dims = c(1L, 0L),
                         dimBefore = c(4L, 3L),
                         dimAfter = 4L)
        transform <- makeCollapseTransformExtra(transform)
        set.seed(seed)
        ans.R <- sapply(1:12, makeIOther, transform = transform, useC = FALSE)
        set.seed(seed)
        ans.C <- sapply(1:12, makeIOther, transform = transform, useC = TRUE)
        expect_identical(ans.R, ans.C)
        ## 3x2 matrix, rows 1 and 3 combined
        transform <- new("CollapseTransform",
                         indices = list(c(1L, 2L, 1L), 1:2),
                         dims = 1:2,
                         dimBefore = 3:2,
                         dimAfter = c(2L, 2L))
        transform <- makeCollapseTransformExtra(transform)
        set.seed(seed)
        ans.R <- sapply(1:6, makeIOther, transform = transform, useC = FALSE)
        set.seed(seed)
        ans.C <- sapply(1:6, makeIOther, transform = transform, useC = TRUE)
        expect_identical(ans.R, ans.C)

        ## 3x2x2 array, first dimension collapsed, then result transposed
        transform <- new("CollapseTransform",
                         indices = list(c(1L, 1L, 1L), 1:2, 1:2),
                         dims = c(0L, 2L, 1L),
                         dimBefore = c(3L, 2L, 2L),
                         dimAfter = c(2L, 2L))
        transform <- makeCollapseTransformExtra(transform)
        set.seed(seed)
        ans.R <- sapply(1:12, makeIOther, transform = transform, useC = FALSE)
        set.seed(seed)
        ans.C <- sapply(1:12, makeIOther, transform = transform, useC = TRUE)
        expect_identical(ans.R, ans.C)
        ## 3x2 matrix, nothing changed
        transform <- new("CollapseTransform",
                         indices = list(1:3, 1:2),
                         dims = 1:2,
                         dimBefore = 3:2,
                         dimAfter = 3:2)
        transform <- makeCollapseTransformExtra(transform)
        set.seed(seed)
        ans.R <- sapply(1:6, makeIOther, transform = transform, useC = FALSE)
        set.seed(seed)
        ans.C <- sapply(1:6, makeIOther, transform = transform, useC = TRUE)
        expect_identical(ans.R, ans.C)
        ## 3x2 matrix, first row dropped, rows 2 and 3 combined
        transform <- new("CollapseTransform",
                         indices = list(c(0L, 1L, 1L), 1:2),
                         dims = 0:1,
                         dimBefore = 3:2,
                         dimAfter = 2L)
        transform <- makeCollapseTransformExtra(transform)
        set.seed(seed)
        ans.R <- lapply(1:6, makeIOther, transform = transform, useC = FALSE)
        set.seed(seed)
        ans.C <- lapply(1:6, makeIOther, transform = transform, useC = TRUE)
        expect_identical(ans.R, ans.C)
        ## 4x3 matrix, last column dropped, rows 1 and 2, and rows 3 and 4 combined
        transform <- new("CollapseTransform",
                         indices = list(c(1L, 1L, 2L, 2L), c(1L, 2L, 0L)),
                         dims = 1:2,
                         dimBefore = 4:3,
                         dimAfter = c(2L, 2L))
        transform <- makeCollapseTransformExtra(transform)
        set.seed(seed)
        ans.R <- sapply(1:12, makeIOther, transform = transform, useC = FALSE)
        set.seed(seed)
        ans.C <- sapply(1:12, makeIOther, transform = transform, useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
})

## ESTIMATION #########################################################################

test_that("joinFiles works", {
    joinFiles <- demest:::joinFiles
    filenames.first <- character(3)
    filenames.last <- character(3)
    for (i in 1:3) {
        filenames.first[i] <- tempfile()
        filenames.last[i] <- tempfile()
        con <- file(filenames.first[i], "wb")
        writeBin(1:10 + (i - 1) * 10, con)
        close(con)
        con <- file(filenames.last[i], "wb")
        writeBin(1:10 + (i - 1) * 10 + 100, con)
        close(con)
    }
    joinFiles(filenames.first, filenames.last)
    for (i in 1:3) {
        con <- file(filenames.first[i], "rb")
        res <- readBin(con, n = 20, what = "double")
        expect_identical(res, 1:10 + (i-1) * 10 + rep(c(0, 100), each = 10))
        expect_false(file.exists(filenames.last[i]))
        close(con)
    }
})

test_that("logLikelihood_Binomial gives valid answer", {
    logLikelihood_Binomial <- demest:::logLikelihood_Binomial
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        exposure <- Counts(array(20 * rpois(n = 20, lambda = 10),
                                 dim = c(5, 4),
                                 dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        spec <- Model(y ~ Binomial(mean ~ age + region))
        model <- initialModel(spec, y = y, exposure = exposure)
        dataset <- Counts(array(as.integer(rpois(n = 20, lambda = 20)),
                                dim = c(2, 10),
                                dimnames = list(sex = c("f", "m"), age = 0:9)))
        i <- sample.int(20, size = 1)
        count <- as.integer(rpois(n = 1, lambda = dataset[i] * 1.5))
        ans.obtained <- logLikelihood_Binomial(model = model,
                                               count = count,
                                               dataset = dataset,
                                               i = i)
        ans.expected <- dbinom(x = dataset[i], size = count, prob = model@theta[i], log = TRUE)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R, and C versions of logLikelihood_Binomial give same answer", {
    logLikelihood_Binomial <- demest:::logLikelihood_Binomial
    initialModel <- demest:::initialModel
    ## tests where -inf unlikely
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        exposure <- Counts(array(20 * rpois(n = 20, lambda = 10),
                                 dim = c(5, 4),
                                 dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        spec <- Model(y ~ Binomial(mean ~ age + region))
        model <- initialModel(spec, y = y, exposure = exposure)
        dataset <- Counts(array(as.integer(rpois(n = 20, lambda = 20)),
                                dim = c(2, 10),
                                dimnames = list(sex = c("f", "m"), age = 0:9)))
        i <- sample.int(20, size = 1)
        count <- as.integer(rpois(n = 1, lambda = dataset[i] * 1.5))
        ans.R <- logLikelihood_Binomial(model = model,
                                        count = count,
                                        dataset = dataset,
                                        i = i,
                                        useC = FALSE)
        ans.C <- logLikelihood_Binomial(model = model,
                                        count = count,
                                        dataset = dataset,
                                        i = i,
                                        useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
    ## tests where -inf likely
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        exposure <- Counts(array(20 * rpois(n = 20, lambda = 10),
                                 dim = c(5, 4),
                                 dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        spec <- Model(y ~ Binomial(mean ~ age + region))
        model <- initialModel(spec, y = y, exposure = exposure)
        dataset <- Counts(array(as.integer(rpois(n = 20, lambda = 20)),
                                dim = c(2, 10),
                                dimnames = list(sex = c("f", "m"), age = 0:9)))
        i <- sample.int(20, size = 1)
        count <- as.integer(rpois(n = 1, lambda = dataset[i] * 0.5))
        ans.R <- logLikelihood_Binomial(model = model,
                                        count = count,
                                        dataset = dataset,
                                        i = i,
                                        useC = FALSE)
        ans.C <- logLikelihood_Binomial(model = model,
                                        count = count,
                                        dataset = dataset,
                                        i = i,
                                        useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})


test_that("logLikelihood_CMP gives valid answer", {
    initialModel <- demest:::initialModel
    logLikelihood_CMP <- demest:::logLikelihood_CMP
    logDensCMPUnnormalised1 <- demest:::logDensCMPUnnormalised1
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        exposure <- Counts(array(20 * rpois(n = 20, lambda = 10),
                                 dim = c(5, 4),
                                 dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        spec <- Model(y ~ CMP(mean ~ age + region))
        model <- initialModel(spec, y = y, exposure = exposure)
        dataset <- Counts(array(as.integer(rpois(n = 20, lambda = 20)),
                                dim = c(2, 10),
                                dimnames = list(sex = c("f", "m"), age = 0:9)))
        i <- sample.int(20, size = 1)
        count <- as.integer(rpois(n = 1, lambda = dataset[i]))
        ans.obtained <- logLikelihood_CMP(model = model,
                                          count = count,
                                          dataset = dataset,
                                          i = i)
        ans.expected <- logDensCMPUnnormalised1(x = dataset[i],
                                                gamma = count * model@theta[i],
                                                nu = model@nuCMP[i])
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of logLikelihood_CMP give same answer", {
    initialModel <- demest:::initialModel
    logLikelihood_CMP <- demest:::logLikelihood_CMP
    logDensCMPUnnormalised1 <- demest:::logDensCMPUnnormalised1
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        exposure <- Counts(array(20 * rpois(n = 20, lambda = 10),
                                 dim = c(5, 4),
                                 dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        spec <- Model(y ~ CMP(mean ~ age + region))
        model <- initialModel(spec, y = y, exposure = exposure)
        dataset <- Counts(array(as.integer(rpois(n = 20, lambda = 20)),
                                dim = c(2, 10),
                                dimnames = list(sex = c("f", "m"), age = 0:9)))
        i <- sample.int(20, size = 1)
        count <- as.integer(rpois(n = 1, lambda = dataset[i]))
        ans.R <- logLikelihood_CMP(model = model,
                                   count = count,
                                   dataset = dataset,
                                   i = i,
                                   useC = FALSE)
        ans.C <- logLikelihood_CMP(model = model,
                                   count = count,
                                   dataset = dataset,
                                   i = i,
                                   useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("logLikelihood_Poisson gives valid answer", {
    initialModel <- demest:::initialModel
    logLikelihood_Poisson <- demest:::logLikelihood_Poisson
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        exposure <- Counts(array(20 * rpois(n = 20, lambda = 10),
                                 dim = c(5, 4),
                                 dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        spec <- Model(y ~ Poisson(mean ~ age + region))
        model <- initialModel(spec, y = y, exposure = exposure)
        dataset <- Counts(array(as.integer(rpois(n = 20, lambda = 20)),
                                dim = c(2, 10),
                                dimnames = list(sex = c("f", "m"), age = 0:9)))
        i <- sample.int(20, size = 1)
        count <- as.integer(rpois(n = 1, lambda = dataset[i]))
        ans.obtained <- logLikelihood_Poisson(model = model,
                                              count = count,
                                              dataset = dataset,
                                              i = i)
        ans.expected <- dpois(x = dataset[i], lambda = count * model@theta[i], log = TRUE)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of logLikelihood_Poisson give same answer", {
    logLikelihood_Poisson <- demest:::logLikelihood_Poisson
    initialModel <- demest:::initialModel
    ## ans not -inf
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        exposure <- Counts(array(20 * rpois(n = 20, lambda = 10),
                                 dim = c(5, 4),
                                 dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        spec <- Model(y ~ Poisson(mean ~ age + region))
        model <- initialModel(spec, y = y, exposure = exposure)
        dataset <- Counts(array(as.integer(rpois(n = 20, lambda = 20)),
                                dim = c(2, 10),
                                dimnames = list(sex = c("f", "m"), age = 0:9)))
        i <- sample.int(20, size = 1)
        count <- as.integer(rpois(n = 1, lambda = dataset[i]))
        ans.R <- logLikelihood_Poisson(model = model,
                                       count = count,
                                       dataset = dataset,
                                       i = i,
                                       useC = FALSE)
        ans.C <- logLikelihood_Poisson(model = model,
                                       count = count,
                                       dataset = dataset,
                                       i = i,
                                       useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
    ## ans -inf
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        exposure <- Counts(array(20 * rpois(n = 20, lambda = 10),
                                 dim = c(5, 4),
                                 dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        y <- Counts(array(rbinom(n = 20, size = exposure, prob = 0.5),
                          dim = c(5, 4),
                          dimnames = list(age = 0:4, region = c("a", "b", "c", "d"))))
        spec <- Model(y ~ Poisson(mean ~ age + region))
        model <- initialModel(spec, y = y, exposure = exposure)
        dataset <- Counts(array(as.integer(rpois(n = 20, lambda = 20)),
                                dim = c(2, 10),
                                dimnames = list(sex = c("f", "m"), age = 0:9)))
        i <- sample.int(20, size = 1)
        count <- 0L
        ans.R <- logLikelihood_Poisson(model = model,
                                       count = count,
                                       dataset = dataset,
                                       i = i,
                                       useC = FALSE)
        ans.C <- logLikelihood_Poisson(model = model,
                                       count = count,
                                       dataset = dataset,
                                       i = i,
                                       useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("logLikelihood gives valid answer with PoissonBinomialMixture", {
    logLikelihood_PoissonBinomialMixture <- demest:::logLikelihood_PoissonBinomialMixture
    dpoibin1 <- demest:::dpoibin1
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        model <- new("PoissonBinomialMixture", prob = 0.9)
        dataset <- Counts(array(as.integer(rpois(n = 20, lambda = 20)),
                                dim = c(2, 10),
                                dimnames = list(sex = c("f", "m"), age = 0:9)))
        i <- sample.int(20, size = 1)
        count <- as.integer(rpois(n = 1, lambda = dataset[i]))
        ans.obtained <- logLikelihood_PoissonBinomialMixture(model = model,
                                                             count = count,
                                                             dataset = dataset,
                                                             i = i)
        ans.expected <- dpoibin1(x = dataset[i], size = count, prob = model@prob, log = TRUE)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of logLikelihood_PoissonBinomialMixture give same answer", {
    logLikelihood_PoissonBinomialMixture<- demest:::logLikelihood_PoissonBinomialMixture
    dpoibin1 <- demest:::dpoibin1
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        model <- new("PoissonBinomialMixture", prob = 0.9)
        dataset <- Counts(array(as.integer(rpois(n = 20, lambda = 20)),
                                dim = c(2, 10),
                                dimnames = list(sex = c("f", "m"), age = 0:9)))
        i <- sample.int(20, size = 1)
        count <- as.integer(rpois(n = 1, lambda = dataset[i]))
        ans.R <- logLikelihood_PoissonBinomialMixture(model = model,
                                                      count = count,
                                                      dataset = dataset,
                                                      i = i,
                                                      useC = FALSE)
        ans.C <- logLikelihood_PoissonBinomialMixture(model = model,
                                                      count = count,
                                                      dataset = dataset,
                                                      i = i,
                                                      useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("logLikelihood gives valid answer with NormalFixedUseExp", {
    logLikelihood_NormalFixedUseExp <- demest:::logLikelihood_NormalFixedUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        dataset <- Counts(array(as.integer(rpois(n = 20, lambda = 20)),
                                dim = c(2, 10),
                                dimnames = list(sex = c("f", "m"), age = 0:9)))
        mean <- Values(array(runif(20),
                             dim = c(2, 10),
                             dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ NormalFixed(mean = mean, sd = 0.1))
        model <- initialModel(spec, y = dataset, exposure = dataset)
        i <- sample.int(20, size = 1)
        count <- as.integer(rpois(n = 1, lambda = dataset[i]))
        ans.obtained <- logLikelihood_NormalFixedUseExp(model = model,
                                                        count = count,
                                                        dataset = dataset,
                                                        i = i)
        ans.expected <- dnorm(x = dataset[i], mean = count * mean@.Data[i], sd = 0.1, log = TRUE)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of logLikelihood give same answer with NormalFixedUseExp", {
    logLikelihood_NormalFixedUseExp <- demest:::logLikelihood_NormalFixedUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        dataset <- Counts(array(as.integer(rpois(n = 20, lambda = 20)),
                                dim = c(2, 10),
                                dimnames = list(sex = c("f", "m"), age = 0:9)))
        mean <- Values(array(runif(20),
                             dim = c(2, 10),
                             dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ NormalFixed(mean = mean, sd = 0.1))
        model <- initialModel(spec, y = dataset, exposure = dataset)
        i <- sample.int(20, size = 1)
        count <- as.integer(rpois(n = 1, lambda = dataset[i]))
        ans.R <- logLikelihood_NormalFixedUseExp(model = model,
                                                 count = count,
                                                 dataset = dataset,
                                                 i = i,
                                                 useC = FALSE)
        ans.C <- logLikelihood_NormalFixedUseExp(model = model,
                                                 count = count,
                                                 dataset = dataset,
                                                 i = i,
                                                 useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("logLikelihood gives valid answer with Round3", {
    logLikelihood_Round3 <- demest:::logLikelihood_Round3
    initialModel <- demest:::initialModel
    count <- CountsOne(0:6, labels = letters[1:7], name = "region")
    dataset <- CountsOne(c(0L,
                           0L, 3L, 3L,
                           3L, 6L, 9L),
                         labels = letters[1:7], name = "region")
    spec <- Model(y ~ Round3())
    model <- initialModel(spec, y = dataset, exposure = count)
    ## 0
    ans.obtained <- logLikelihood_Round3(model = model,
                                         count = count[1L],
                                         dataset = dataset,
                                         i = 1L)
    ans.expected <- 0
    expect_equal(ans.obtained, ans.expected)
    ## 1
    ans.obtained <- logLikelihood_Round3(model = model,
                                         count = count[2L],
                                         dataset = dataset,
                                         i = 2L)
    ans.expected <- log(2/3)
    expect_equal(ans.obtained, ans.expected)
    ## 2
    ans.obtained <- logLikelihood_Round3(model = model,
                                         count = count[3L],
                                         dataset = dataset,
                                         i = 3L)
    ans.expected <- log(2/3)
    expect_equal(ans.obtained, ans.expected)
    ## 3
    ans.obtained <- logLikelihood_Round3(model = model,
                                         count = count[4L],
                                         dataset = dataset,
                                         i = 4L)
    ans.expected <- 0
    expect_equal(ans.obtained, ans.expected)
    ## 4
    ans.obtained <- logLikelihood_Round3(model = model,
                                         count = count[5L],
                                         dataset = dataset,
                                         i = 5L)
    ans.expected <- log(2/3)
    expect_equal(ans.obtained, ans.expected)
    ## 5
    ans.obtained <- logLikelihood_Round3(model = model,
                                         count = count[6L],
                                         dataset = dataset,
                                         i = 6L)
    ans.expected <- log(2/3)
    expect_equal(ans.obtained, ans.expected)
    ## 6
    ans.obtained <- logLikelihood_Round3(model = model,
                                         count = count[7L],
                                         dataset = dataset,
                                         i = 7L)
    ans.expected <- -Inf
    expect_equal(ans.obtained, ans.expected)
})

test_that("R and C versions of logLikelihood give same answer with Round3", {
    logLikelihood_Round3 <- demest:::logLikelihood_Round3
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        counts <- Counts(array(as.integer(rpois(n = 20, lambda = 3)),
                               dim = c(2, 10),
                               dimnames = list(sex = c("f", "m"), age = 0:9)))
        dataset <- round3(counts)
        spec <- Model(y ~ Round3())
        model <- initialModel(spec, y = dataset, exposure = counts)
        for (i in seq_len(20)) {
            count <- counts[[i]]
            ans.R <- logLikelihood_Round3(model = model,
                                          count = count,
                                          dataset = dataset,
                                          i = i,
                                          useC = FALSE)
            ans.C <- logLikelihood_Round3(model = model,
                                          count = count,
                                          dataset = dataset,
                                          i = i,
                                          useC = TRUE)
            if (test.identity)
                expect_identical(ans.R, ans.C)
            else
                expect_equal(ans.R, ans.C)
        }
    }
})


test_that("logLikelihood gives valid answer with TFixedUseExp", {
    logLikelihood_TFixedUseExp <- demest:::logLikelihood_TFixedUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        dataset <- Counts(array(as.integer(rpois(n = 20, lambda = 20)),
                                dim = c(2, 10),
                                dimnames = list(sex = c("f", "m"), age = 0:9)))
        location <- Values(array(runif(20),
                             dim = c(2, 10),
                             dimnames = list(sex = c("f", "m"), age = 0:9)))
        spec <- Model(y ~ TFixed(location = location, scale = 0.1))
        model <- initialModel(spec, y = dataset, exposure = dataset)
        i <- sample.int(20, size = 1)
        count <- as.integer(rpois(n = 1, lambda = dataset[i]))
        ans.obtained <- logLikelihood_TFixedUseExp(model = model,
                                                   count = count,
                                                   dataset = dataset,
                                                   i = i)
        ans.expected <- dt(x = (dataset[i] - count * location@.Data[i])/0.1, df = 7, log = TRUE) - log(0.1)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of logLikelihood give same answer with TFixedUseExp", {
    logLikelihood_TFixedUseExp <- demest:::logLikelihood_TFixedUseExp
    initialModel <- demest:::initialModel
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        dataset <- Counts(array(as.integer(rpois(n = 20, lambda = 20)),
                                dim = c(2, 10),
                                dimnames = list(sex = c("f", "m"), age = 0:9)))
        location <- Values(array(runif(20),
                                 dim = c(2, 10),
                                 dimnames = list(sex = c("f", "m"), age = 0:9)))
        scale <- sqrt(location)
        spec <- Model(y ~ TFixed(location = location, scale = scale))
        model <- initialModel(spec, y = dataset, exposure = dataset)
        i <- sample.int(20, size = 1)
        count <- as.integer(rpois(n = 1, lambda = dataset[i]))
        set.seed(seed + 1)
        ans.R <- logLikelihood_TFixedUseExp(model = model,
                                            count = count,
                                            dataset = dataset,
                                            i = i,
                                            useC = FALSE)
        set.seed(seed + 1)
        ans.C <- logLikelihood_TFixedUseExp(model = model,
                                            count = count,
                                            dataset = dataset,
                                            i = i,
                                            useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})





test_that("makeIOther gives valid answers", {
    makeIOther <- demest:::makeIOther
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    getIShared <- dembase::getIShared
    ## 4x3 matrix, second dimension collapsed
    for (seed in seq_len(n.test)) {
        transform <- new("CollapseTransform",
                         indices = list(1:4, rep(1L, 3)),
                         dims = c(1L, 0L),
                         dimBefore = c(4L, 3L),
                         dimAfter = 4L)
        transform <- makeCollapseTransformExtra(transform)
        set.seed(seed)
        ans.obtained <- sapply(1:12, makeIOther, transform = transform)
        set.seed(seed)
        ans.expected <- integer(12)
        for (i in seq_along(ans.expected)) {
            shared <- getIShared(i, transform)
            if (length(shared) > 1L) {
                shared <- shared[shared != i]
                ans.expected[i] <- shared[as.integer(runif(1) * length(shared)) + 1]
            }
            else
                ans.expected[i] <- 0L
        }
        expect_identical(ans.obtained, ans.expected)
        ## 3x2 matrix, rows 1 and 3 combined
        transform <- new("CollapseTransform",
                         indices = list(c(1L, 2L, 1L), 1:2),
                         dims = 1:2,
                         dimBefore = 3:2,
                         dimAfter = c(2L, 2L))
        transform <- makeCollapseTransformExtra(transform)
        set.seed(seed)
        ans.obtained <- sapply(1:6, makeIOther, transform = transform)
        set.seed(seed)
        ans.expected <- integer(6)
        for (i in seq_along(ans.expected)) {
            shared <- getIShared(i, transform)
            if (length(shared) > 1L) {
                shared <- shared[shared != i]
                ans.expected[i] <- shared[as.integer(runif(1) * length(shared)) + 1]
            }
            else
                ans.expected[i] <- 0L
        }
        expect_identical(ans.obtained, ans.expected)
        ## 3x2x2 array, first dimension collapsed, then result transposed
        transform <- new("CollapseTransform",
                         indices = list(c(1L, 1L, 1L), 1:2, 1:2),
                         dims = c(0L, 2L, 1L),
                         dimBefore = c(3L, 2L, 2L),
                         dimAfter = c(2L, 2L))
        transform <- makeCollapseTransformExtra(transform)
        set.seed(seed)
        ans.obtained <- sapply(1:12, makeIOther, transform = transform)
        set.seed(seed)
        ans.expected <- integer(12)
        for (i in seq_along(ans.expected)) {
            shared <- getIShared(i, transform)
            if (length(shared) > 1L) {
                shared <- shared[shared != i]
                ans.expected[i] <- shared[as.integer(runif(1) * length(shared)) + 1]
            }
            else
                ans.expected[i] <- 0L
        }
        expect_identical(ans.obtained, ans.expected)
        ## 3x2 matrix, nothing changed
        transform <- new("CollapseTransform",
                         indices = list(1:3, 1:2),
                         dims = 1:2,
                         dimBefore = 3:2,
                         dimAfter = 3:2)
        transform <- makeCollapseTransformExtra(transform)
        ans.obtained <- sapply(1:6, makeIOther, transform = transform)
        ans.expected <- rep(0L, 6)
        expect_identical(ans.obtained, ans.expected)
        ## 3x2 matrix, first row dropped, rows 2 and 3 combined
        transform <- new("CollapseTransform",
                         indices = list(c(0L, 1L, 1L), 1:2),
                         dims = 0:1,
                         dimBefore = 3:2,
                         dimAfter = 2L)
        transform <- makeCollapseTransformExtra(transform)
        ans.obtained <- lapply(1:6, makeIOther, transform = transform)
        ans.expected <- list(-1L, 3L, 2L, -1L, 6L, 5L)
        expect_identical(ans.obtained, ans.expected)
        ## 4x3 matrix, last column dropped, rows 1 and 2, and rows 3 and 4 combined
        transform <- new("CollapseTransform",
                         indices = list(c(1L, 1L, 2L, 2L), c(1L, 2L, 0L)),
                         dims = 1:2,
                         dimBefore = 4:3,
                         dimAfter = c(2L, 2L))
        transform <- makeCollapseTransformExtra(transform)
        ans.obtained <- sapply(1:12, makeIOther, transform = transform)
        ans.expected <- c(2L, 1L, 4L, 3L, 6L, 5L, 8L, 7L, -1L, -1L, -1L, -1L)
        expect_identical(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of makeIOther give same answer", {
    makeIOther <- demest:::makeIOther
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    getIShared <- dembase::getIShared
    ## 4x3 matrix, second dimension collapsed
    for (seed in seq_len(n.test)) {
        transform <- new("CollapseTransform",
                         indices = list(1:4, rep(1L, 3)),
                         dims = c(1L, 0L),
                         dimBefore = c(4L, 3L),
                         dimAfter = 4L)
        transform <- makeCollapseTransformExtra(transform)
        set.seed(seed)
        ans.R <- sapply(1:12, makeIOther, transform = transform, useC = FALSE)
        set.seed(seed)
        ans.C <- sapply(1:12, makeIOther, transform = transform, useC = TRUE)
        expect_identical(ans.R, ans.C)
        ## 3x2 matrix, rows 1 and 3 combined
        transform <- new("CollapseTransform",
                         indices = list(c(1L, 2L, 1L), 1:2),
                         dims = 1:2,
                         dimBefore = 3:2,
                         dimAfter = c(2L, 2L))
        transform <- makeCollapseTransformExtra(transform)
        set.seed(seed)
        ans.R <- sapply(1:6, makeIOther, transform = transform, useC = FALSE)
        set.seed(seed)
        ans.C <- sapply(1:6, makeIOther, transform = transform, useC = TRUE)
        expect_identical(ans.R, ans.C)

        ## 3x2x2 array, first dimension collapsed, then result transposed
        transform <- new("CollapseTransform",
                         indices = list(c(1L, 1L, 1L), 1:2, 1:2),
                         dims = c(0L, 2L, 1L),
                         dimBefore = c(3L, 2L, 2L),
                         dimAfter = c(2L, 2L))
        transform <- makeCollapseTransformExtra(transform)
        set.seed(seed)
        ans.R <- sapply(1:12, makeIOther, transform = transform, useC = FALSE)
        set.seed(seed)
        ans.C <- sapply(1:12, makeIOther, transform = transform, useC = TRUE)
        expect_identical(ans.R, ans.C)
        ## 3x2 matrix, nothing changed
        transform <- new("CollapseTransform",
                         indices = list(1:3, 1:2),
                         dims = 1:2,
                         dimBefore = 3:2,
                         dimAfter = 3:2)
        transform <- makeCollapseTransformExtra(transform)
        set.seed(seed)
        ans.R <- sapply(1:6, makeIOther, transform = transform, useC = FALSE)
        set.seed(seed)
        ans.C <- sapply(1:6, makeIOther, transform = transform, useC = TRUE)
        expect_identical(ans.R, ans.C)
        ## 3x2 matrix, first row dropped, rows 2 and 3 combined
        transform <- new("CollapseTransform",
                         indices = list(c(0L, 1L, 1L), 1:2),
                         dims = 0:1,
                         dimBefore = 3:2,
                         dimAfter = 2L)
        transform <- makeCollapseTransformExtra(transform)
        set.seed(seed)
        ans.R <- lapply(1:6, makeIOther, transform = transform, useC = FALSE)
        set.seed(seed)
        ans.C <- lapply(1:6, makeIOther, transform = transform, useC = TRUE)
        expect_identical(ans.R, ans.C)
        ## 4x3 matrix, last column dropped, rows 1 and 2, and rows 3 and 4 combined
        transform <- new("CollapseTransform",
                         indices = list(c(1L, 1L, 2L, 2L), c(1L, 2L, 0L)),
                         dims = 1:2,
                         dimBefore = 4:3,
                         dimAfter = c(2L, 2L))
        transform <- makeCollapseTransformExtra(transform)
        set.seed(seed)
        ans.R <- sapply(1:12, makeIOther, transform = transform, useC = FALSE)
        set.seed(seed)
        ans.C <- sapply(1:12, makeIOther, transform = transform, useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
})

## ESTIMATION #########################################################################

test_that("joinFiles works", {
    joinFiles <- demest:::joinFiles
    filenames.first <- character(3)
    filenames.last <- character(3)
    for (i in 1:3) {
        filenames.first[i] <- tempfile()
        filenames.last[i] <- tempfile()
        con <- file(filenames.first[i], "wb")
        writeBin(1:10 + (i - 1) * 10, con)
        close(con)
        con <- file(filenames.last[i], "wb")
        writeBin(1:10 + (i - 1) * 10 + 100, con)
        close(con)
    }
    joinFiles(filenames.first, filenames.last)
    for (i in 1:3) {
        con <- file(filenames.first[i], "rb")
        res <- readBin(con, n = 20, what = "double")
        expect_identical(res, 1:10 + (i-1) * 10 + rep(c(0, 100), each = 10))
        expect_false(file.exists(filenames.last[i]))
        close(con)
    }
})

## test_that("estimateOneChain works on small objects", {
##     estimateOneChain <- demest:::estimateOneChain
##     initialCombinedModel <- demest:::initialCombinedModel
##     updateCombined <- demest:::updateCombined
##     extractValues <- demest:::extractValues
##     set.seed(1)
##     y <- Counts(array(as.integer(rpois(n = 20, lambda = 10)),
##                       dim = c(5, 4),
##                       dimnames = list(age = 0:4, region = letters[1:4])))
##     spec <- Model(y ~ Poisson(mean = 10, exposure = FALSE))
##     ## not continuing, nBurnin > 0, nThin > 1
##     set.seed(2)
##     combined <- initialCombinedModel(spec, y = y, exposure = NULL, weights = NULL)
##     tempfile <- tempfile()
##     set.seed(100)
##     ans.obtained.obj <- estimateOneChain(combined, tempfile = tempfile,
##                                          seed = NULL, nBurnin = 3L,
##                                          nSim = 6L, continuing = FALSE,
##                                          nThin = 2L)
##     con <- file(tempfile, "rb")
##     ans.obtained.file <- readBin(con = con, what = "double", n = 1000)
##     close(con)
##     set.seed(100)
##     ans.expected.file <- vector(mode = "list", length = 3)
##     combined <- updateCombined(combined, nUpdate = 2L)
##     for (i in 1:3) {
##         combined <- updateCombined(combined, n = 2L)
##         ans.expected.file[[i]] <- extractValues(combined)
##     }
##     ans.expected.file <- unlist(ans.expected.file)
##     ans.expected.obj <- combined
##     if (test.identity) {
##         expect_identical(ans.obtained.obj, ans.expected.obj)
##         expect_identical(ans.obtained.file, ans.expected.file)
##     }
##     else {
##         expect_identical(ans.obtained.obj, ans.expected.obj)
##         expect_identical(ans.obtained.file, ans.expected.file)
##     }
##     ## not continuing, nBurnin > 0, nThin == 1
##     set.seed(2)
##     combined <- initialCombinedModel(spec, y = y, exposure = NULL, weights = NULL)
##     tempfile <- tempfile()
##     set.seed(100)
##     ans.obtained.obj <- estimateOneChain(combined, tempfile = tempfile,
##                                          seed = NULL,
##                                          nBurnin = 3L,
##                                          nSim = 3L, continuing = FALSE,
##                                          nThin = 1L)
##     con <- file(tempfile, "rb")
##     ans.obtained.file <- readBin(con = con, what = "double", n = 1000)
##     close(con)
##     set.seed(100)
##     ans.expected.file <- vector(mode = "list", length = 3)
##     combined <- updateCombined(combined, nUpdate = 2L)
##     for (i in 1:3) {
##         combined <- updateCombined(combined, n = 1L)
##         ans.expected.file[[i]] <- extractValues(combined)
##     }
##     ans.expected.file <- unlist(ans.expected.file)
##     ans.expected.obj <- combined
##     if (test.identity) {
##         expect_identical(ans.obtained.obj, ans.expected.obj)
##         expect_identical(ans.obtained.file, ans.expected.file)
##     }
##     else {
##         expect_identical(ans.obtained.obj, ans.expected.obj)
##         expect_identical(ans.obtained.file, ans.expected.file)
##     }
##     ## not continuing, nBurnin == 0, nThin > 1
##     set.seed(2)
##     combined <- initialCombinedModel(spec, y = y, exposure = NULL, weights = NULL)
##     tempfile <- tempfile()
##     set.seed(100)
##     ans.obtained.obj <- estimateOneChain(combined, tempfile = tempfile,
##                                          seed = NULL,
##                                          nBurnin = 0L,
##                                          nSim = 6L, continuing = FALSE,
##                                          nThin = 2L)
##     con <- file(tempfile, "rb")
##     ans.obtained.file <- readBin(con = con, what = "double", n = 1000)
##     close(con)
##     set.seed(100)
##     ans.expected.file <- vector(mode = "list", length = 3)
##     combined <- updateCombined(combined, nUpdate = 1L)
##     ans.expected.file[[1]] <- extractValues(combined)
##     for (i in 2:3) {
##         combined <- updateCombined(combined, n = 2L)
##         ans.expected.file[[i]] <- extractValues(combined)
##     }
##     ans.expected.file <- unlist(ans.expected.file)
##     ans.expected.obj <- combined
##     if (test.identity) {
##         expect_identical(ans.obtained.obj, ans.expected.obj)
##         expect_identical(ans.obtained.file, ans.expected.file)
##     }
##     else {
##         expect_identical(ans.obtained.obj, ans.expected.obj)
##         expect_identical(ans.obtained.file, ans.expected.file)
##     }
##     ## not continuing, nBurnin == 0, nThin == 1
##     set.seed(2)
##     combined <- initialCombinedModel(spec, y = y, exposure = NULL, weights = NULL)
##     tempfile <- tempfile()
##     set.seed(100)
##     ans.obtained.obj <- estimateOneChain(combined, tempfile = tempfile,
##                                          seed = NULL, nBurnin = 0L,
##                                          nSim = 3L, continuing = FALSE,
##                                          nThin = 1L)
##     con <- file(tempfile, "rb")
##     ans.obtained.file <- readBin(con = con, what = "double", n = 1000)
##     close(con)
##     set.seed(100)
##     ans.expected.file <- vector(mode = "list", length = 3)
##     ans.expected.file[[1]] <- extractValues(combined)
##     for (i in 2:3) {
##         combined <- updateCombined(combined, n = 1L)
##         ans.expected.file[[i]] <- extractValues(combined)
##     }
##     ans.expected.file <- unlist(ans.expected.file)
##     ans.expected.obj <- combined
##     if (test.identity) {
##         expect_identical(ans.obtained.obj, ans.expected.obj)
##         expect_identical(ans.obtained.file, ans.expected.file)
##     }
##     else {
##         expect_identical(ans.obtained.obj, ans.expected.obj)
##         expect_identical(ans.obtained.file, ans.expected.file)
##     }
##     ## continuing, nThin > 1
##     set.seed(2)
##     combined <- initialCombinedModel(spec, y = y, exposure = NULL, weights = NULL)
##     tempfile <- tempfile()
##     set.seed(100)
##     ans.obtained.obj <- estimateOneChain(combined, tempfile = tempfile,
##                                          seed = NULL, nBurnin = 0L,
##                                          nSim = 6L, continuing = TRUE,
##                                          nThin = 2L)
##     con <- file(tempfile, "rb")
##     ans.obtained.file <- readBin(con = con, what = "double", n = 1000)
##     close(con)
##     set.seed(100)
##     ans.expected.file <- vector(mode = "list", length = 3)
##     for (i in 1:3) {
##         combined <- updateCombined(combined, n = 2L)
##         ans.expected.file[[i]] <- extractValues(combined)
##     }
##     ans.expected.file <- unlist(ans.expected.file)
##     ans.expected.obj <- combined
##     if (test.identity) {
##         expect_identical(ans.obtained.obj, ans.expected.obj)
##         expect_identical(ans.obtained.file, ans.expected.file)
##     }
##     else {
##         expect_identical(ans.obtained.obj, ans.expected.obj)
##         expect_identical(ans.obtained.file, ans.expected.file)
##     }
##     ## continuing, nThin == 1
##     set.seed(2)
##     combined <- initialCombinedModel(spec, y = y, exposure = NULL, weights = NULL)
##     tempfile <- tempfile()
##     set.seed(100)
##     ans.obtained.obj <- estimateOneChain(combined, tempfile = tempfile,
##                                          seed = NULL, nBurnin = 0L,
##                                          nSim = 3L, continuing = TRUE,
##                                          nThin = 1L)
##     con <- file(tempfile, "rb")
##     ans.obtained.file <- readBin(con = con, what = "double", n = 1000)
##     close(con)
##     set.seed(100)
##     ans.expected.file <- vector(mode = "list", length = 3)
##     for (i in 1:3) {
##         combined <- updateCombined(combined, n = 1L)
##         ans.expected.file[[i]] <- extractValues(combined)
##     }
##     ans.expected.file <- unlist(ans.expected.file)
##     ans.expected.obj <- combined
##     if (test.identity) {
##         expect_identical(ans.obtained.obj, ans.expected.obj)
##         expect_identical(ans.obtained.file, ans.expected.file)
##     }
##     else {
##         expect_identical(ans.obtained.obj, ans.expected.obj)
##         expect_identical(ans.obtained.file, ans.expected.file)
##     }
## })

## test_that("compare R and C versions of estimateOneChain on small objects, not continuing, nBurnin > 0, nThin > 1", {
##     estimateOneChain <- demest:::estimateOneChain
##     initialCombinedModel <- demest:::initialCombinedModel
##     updateCombined <- demest:::updateCombined
##     extractValues <- demest:::extractValues
##     seed1 <- 1
##     seed2 <- 2
##     seed3 <- 100
##     set.seed(seed1)
##     y <- Counts(array(as.integer(rpois(n = 20, lambda = 10)),
##                       dim = c(5, 4),
##                       dimnames = list(age = 0:4, region = letters[1:4])))
##     spec <- Model(y ~ Poisson(mean = 10, exposure = FALSE))
##     ## not continuing, nBurnin > 0, nThin > 1
##     set.seed(seed2)
##     combined <- initialCombinedModel(spec, y = y, exposure = NULL, weights = NULL)
##     filenameR <- tempfile()
##     filenameC <- tempfile()
##     set.seed(seed3)
##     ans.obtained.R.obj <- estimateOneChain(combined, tempfile = filenameR,
##                                            seed = NULL, nBurnin = 3L,
##                                          nSim = 6L, continuing = FALSE,
##                                          nThin = 2L, nAttempt = 10L, useC = FALSE)
##     con <- file(filenameR, "rb")
##     ans.obtained.R.file <- readBin(con = con, what = "double", n = 1000)
##     close(con)
##     set.seed(seed3)
##     ans.obtained.C.obj <- estimateOneChain(combined, tempfile = filenameC,
##                                            seed = NULL, nBurnin = 3L,
##                                          nSim = 6L, continuing = FALSE,
##                                          nThin = 2L, nAttempt = 10L, useC = TRUE)
##     con <- file(filenameC, "rb")
##     ans.obtained.C.file <- readBin(con = con, what = "double", n = 1000)
##     close(con)
##     set.seed(seed3)
##     ans.expected.file <- vector(mode = "list", length = 3)
##     combined <- updateCombined(combined, nUpdate = 2L)
##     for (i in 1:3) {
##         combined <- updateCombined(combined, n = 2L)
##         ans.expected.file[[i]] <- extractValues(combined)
##     }
##     ans.expected.file <- unlist(ans.expected.file)
##     ans.expected.obj <- combined
##     if (test.identity) {
##         expect_identical(ans.obtained.R.obj, ans.obtained.C.obj)
##         expect_identical(ans.obtained.R.file, ans.obtained.C.file)
##         expect_identical(ans.obtained.R.obj, ans.expected.obj)
##         expect_identical(ans.obtained.R.file, ans.expected.file)
##     }
##     else {
##         expect_equal(ans.obtained.R.obj, ans.obtained.C.obj)
##         expect_equal(ans.obtained.R.file, ans.obtained.C.file)
##         expect_equal(ans.obtained.R.obj, ans.expected.obj)
##         expect_equal(ans.obtained.R.file, ans.expected.file)
##     }
## })

## test_that("compare R and C versions of estimateOneChain on small objects, not continuing, nBurnin > 0, nThin == 1", {
##     estimateOneChain <- demest:::estimateOneChain
##     initialCombinedModel <- demest:::initialCombinedModel
##     updateCombined <- demest:::updateCombined
##     extractValues <- demest:::extractValues
##     seed1 <- 1
##     seed2 <- 2
##     seed3 <- 100
##     set.seed(seed1)
##     y <- Counts(array(as.integer(rpois(n = 20, lambda = 10)),
##                       dim = c(5, 4),
##                       dimnames = list(age = 0:4, region = letters[1:4])))
##     spec <- Model(y ~ Poisson(mean = 10, exposure = FALSE))
##     ## not continuing, nBurnin > 0, nThin == 1
##     set.seed(seed2)
##     combined <- initialCombinedModel(spec, y = y, exposure = NULL, weights = NULL)
##     filenameR <- tempfile()
##     filenameC <- tempfile()
##     set.seed(seed3)
##     ans.obtained.R.obj <- estimateOneChain(combined, tempfile = filenameR,
##                                            seed = NULL, nBurnin = 3L,
##                                            nSim = 3L, continuing = FALSE,
##                                            nThin = 1L, nAttempt = 10L, useC = FALSE)
##     con <- file(filenameR, "rb")
##     ans.obtained.R.file <- readBin(con = con, what = "double", n = 1000)
##     close(con)
##     set.seed(seed3)
##     ans.obtained.C.obj <- estimateOneChain(combined, tempfile = filenameC,
##                                            seed = NULLnBurnin = 3L,
##                                            nSim = 3L, continuing = FALSE,
##                                            nThin = 1L, nAttempt = 10L, useC = TRUE)
##     con <- file(filenameC, "rb")
##     ans.obtained.C.file <- readBin(con = con, what = "double", n = 1000)
##     close(con)
##     set.seed(seed3)
##     ans.expected.file <- vector(mode = "list", length = 3)
##     combined <- updateCombined(combined, nUpdate = 2L)
##     for (i in 1:3) {
##         combined <- updateCombined(combined, n = 1L)
##         ans.expected.file[[i]] <- extractValues(combined)
##     }
##     ans.expected.file <- unlist(ans.expected.file)
##     ans.expected.obj <- combined
##     if (test.identity) {
##         expect_identical(ans.obtained.R.obj, ans.obtained.C.obj)
##         expect_identical(ans.obtained.R.file, ans.obtained.C.file)
##         expect_identical(ans.obtained.R.obj, ans.expected.obj)
##         expect_identical(ans.obtained.R.file, ans.expected.file)
##     }
##     else {
##         expect_equal(ans.obtained.R.obj, ans.obtained.C.obj)
##         expect_equal(ans.obtained.R.file, ans.obtained.C.file)
##         expect_equal(ans.obtained.R.obj, ans.expected.obj)
##         expect_equal(ans.obtained.R.file, ans.expected.file)
##     }
## })

## test_that("compare R and C versions of estimateOneChain on small objects, not continuing, nBurnin == 0, nThin > 1", {
##     estimateOneChain <- demest:::estimateOneChain
##     initialCombinedModel <- demest:::initialCombinedModel
##     updateCombined <- demest:::updateCombined
##     extractValues <- demest:::extractValues
##     seed1 <- 1
##     seed2 <- 2
##     seed3 <- 100
##     set.seed(seed1)
##     y <- Counts(array(as.integer(rpois(n = 20, lambda = 10)),
##                       dim = c(5, 4),
##                       dimnames = list(age = 0:4, region = letters[1:4])))
##     spec <- Model(y ~ Poisson(mean = 10, exposure = FALSE))
##     ## not continuing, nBurnin == 0, nThin > 1
##     set.seed(seed2)
##     combined <- initialCombinedModel(spec, y = y, exposure = NULL, weights = NULL)
##     filenameR <- tempfile()
##     filenameC <- tempfile()
##     set.seed(seed3)
##     ans.obtained.R.obj <- estimateOneChain(combined, tempfile = filenameR, nBurnin = 0L,
##                                          nSim = 6L, continuing = FALSE,
##                                          nThin = 2L, nAttempt = 10L, useC = FALSE)
##     con <- file(filenameR, "rb")
##     ans.obtained.R.file <- readBin(con = con, what = "double", n = 1000)
##     close(con)
##     set.seed(seed3)
##     ans.obtained.C.obj <- estimateOneChain(combined, tempfile = filenameC, nBurnin = 0L,
##                                          nSim = 6L, continuing = FALSE,
##                                          nThin = 2L, nAttempt = 10L, useC = TRUE)
##     con <- file(filenameC, "rb")
##     ans.obtained.C.file <- readBin(con = con, what = "double", n = 1000)
##     close(con)
##     set.seed(seed3)
##     ans.expected.file <- vector(mode = "list", length = 3)
##     combined <- updateCombined(combined, nUpdate = 1L)
##     ans.expected.file[[1]] <- extractValues(combined)
##     for (i in 2:3) {
##         combined <- updateCombined(combined, n = 2L)
##         ans.expected.file[[i]] <- extractValues(combined)
##     }
##     ans.expected.file <- unlist(ans.expected.file)
##     ans.expected.obj <- combined
##     if (test.identity) {
##         expect_identical(ans.obtained.R.obj, ans.obtained.C.obj)
##         expect_identical(ans.obtained.R.file, ans.obtained.C.file)
##         expect_identical(ans.obtained.R.obj, ans.expected.obj)
##         expect_identical(ans.obtained.R.file, ans.expected.file)
##     }
##     else {
##         expect_equal(ans.obtained.R.obj, ans.obtained.C.obj)
##         expect_equal(ans.obtained.R.file, ans.obtained.C.file)
##         expect_equal(ans.obtained.R.obj, ans.expected.obj)
##         expect_equal(ans.obtained.R.file, ans.expected.file)
##     }
## })


## test_that("compare R and C versions of estimateOneChain on small objects, not continuing, nBurnin == 0, nThin == 1", {
##     estimateOneChain <- demest:::estimateOneChain
##     initialCombinedModel <- demest:::initialCombinedModel
##     updateCombined <- demest:::updateCombined
##     extractValues <- demest:::extractValues
##     seed1 <- 1
##     seed2 <- 2
##     seed3 <- 100
##     set.seed(seed1)
##     y <- Counts(array(as.integer(rpois(n = 20, lambda = 10)),
##                       dim = c(5, 4),
##                       dimnames = list(age = 0:4, region = letters[1:4])))
##     spec <- Model(y ~ Poisson(mean = 10, exposure = FALSE))
##     ## not continuing, nBurnin == 0, nThin == 1
##     set.seed(seed2)
##     combined <- initialCombinedModel(spec, y = y, exposure = NULL, weights = NULL)
##     filenameR <- tempfile()
##     filenameC <- tempfile()
##     set.seed(seed3)
##     ans.obtained.R.obj <- estimateOneChain(combined, tempfile = filenameR, nBurnin = 0L,
##                                          nSim = 3L, continuing = FALSE,
##                                          nThin = 1L, nAttempt = 10L, useC = FALSE)
##     con <- file(filenameR, "rb")
##     ans.obtained.R.file <- readBin(con = con, what = "double", n = 1000)
##     close(con)
##     set.seed(seed3)
##     ans.obtained.C.obj <- estimateOneChain(combined, tempfile = filenameC, nBurnin = 0L,
##                                          nSim = 3L, continuing = FALSE,
##                                          nThin = 1L, nAttempt = 10L, useC = TRUE)
##     con <- file(filenameC, "rb")
##     ans.obtained.C.file <- readBin(con = con, what = "double", n = 1000)
##     close(con)
##     set.seed(seed3)
##     ans.expected.file <- vector(mode = "list", length = 3)
##     ans.expected.file[[1]] <- extractValues(combined)
##     for (i in 2:3) {
##         combined <- updateCombined(combined, n = 1L)
##         ans.expected.file[[i]] <- extractValues(combined)
##     }
##     ans.expected.file <- unlist(ans.expected.file)
##     ans.expected.obj <- combined
##     if (test.identity) {
##         expect_identical(ans.obtained.R.obj, ans.obtained.C.obj)
##         expect_identical(ans.obtained.R.file, ans.obtained.C.file)
##         expect_identical(ans.obtained.R.obj, ans.expected.obj)
##         expect_identical(ans.obtained.R.file, ans.expected.file)
##     }
##     else {
##         expect_equal(ans.obtained.R.obj, ans.obtained.C.obj)
##         expect_equal(ans.obtained.R.file, ans.obtained.C.file)
##         expect_equal(ans.obtained.R.obj, ans.expected.obj)
##         expect_equal(ans.obtained.R.file, ans.expected.file)
##     }
## })

## test_that("compare R and C versions of estimateOneChain on small objects, continuing, nThin > 1", {
##     estimateOneChain <- demest:::estimateOneChain
##     initialCombinedModel <- demest:::initialCombinedModel
##     updateCombined <- demest:::updateCombined
##     extractValues <- demest:::extractValues
##     seed1 <- 1
##     seed2 <- 2
##     seed3 <- 100
##     set.seed(seed1)
##     y <- Counts(array(as.integer(rpois(n = 20, lambda = 10)),
##                       dim = c(5, 4),
##                       dimnames = list(age = 0:4, region = letters[1:4])))
##     spec <- Model(y ~ Poisson(mean = 10, exposure = FALSE))
##     ## continuing, nThin > 1
##     set.seed(seed2)
##     combined <- initialCombinedModel(spec, y = y, exposure = NULL, weights = NULL)
##     filenameR <- tempfile()
##     filenameC <- tempfile()
##     set.seed(seed3)
##     ans.obtained.R.obj <- estimateOneChain(combined, tempfile = filenameR, nBurnin = 0L,
##                                          nSim = 6L, continuing = TRUE,
##                                          nThin = 2L, nAttempt = 10L, useC = FALSE)
##     con <- file(filenameR, "rb")
##     ans.obtained.R.file <- readBin(con = con, what = "double", n = 1000)
##     close(con)
##     set.seed(seed3)
##     ans.obtained.C.obj <- estimateOneChain(combined, tempfile = filenameC, nBurnin = 0L,
##                                          nSim = 6L, continuing = TRUE,
##                                          nThin = 2L, nAttempt = 10L, useC = TRUE)
##     con <- file(filenameC, "rb")
##     ans.obtained.C.file <- readBin(con = con, what = "double", n = 1000)
##     close(con)
##     set.seed(seed3)
##     ans.expected.file <- vector(mode = "list", length = 3)
##     for (i in 1:3) {
##         combined <- updateCombined(combined, n = 2L)
##         ans.expected.file[[i]] <- extractValues(combined)
##     }
##     ans.expected.file <- unlist(ans.expected.file)
##     ans.expected.obj <- combined
##     if (test.identity) {
##         expect_identical(ans.obtained.R.obj, ans.obtained.C.obj)
##         expect_identical(ans.obtained.R.file, ans.obtained.C.file)
##         expect_identical(ans.obtained.R.obj, ans.expected.obj)
##         expect_identical(ans.obtained.R.file, ans.expected.file)
##     }
##     else {
##         expect_equal(ans.obtained.R.obj, ans.obtained.C.obj)
##         expect_equal(ans.obtained.R.file, ans.obtained.C.file)
##         expect_equal(ans.obtained.R.obj, ans.expected.obj)
##         expect_equal(ans.obtained.R.file, ans.expected.file)
##     }
## })

## test_that("compare RC and C versions of estimateOneChain on small objects, continuing, nThin == 1", {
##     estimateOneChain <- demest:::estimateOneChain
##     initialCombinedModel <- demest:::initialCombinedModel
##     updateCombined <- demest:::updateCombined
##     extractValues <- demest:::extractValues
##     seed1 <- 1
##     seed2 <- 2
##     seed3 <- 100
##     set.seed(seed1)
##     y <- Counts(array(as.integer(rpois(n = 20, lambda = 10)),
##                       dim = c(5, 4),
##                       dimnames = list(age = 0:4, region = letters[1:4])))
##     spec <- Model(y ~ Poisson(mean = 10, exposure = FALSE))
##     ## continuing, nThin == 1
##     set.seed(seed2)
##     combined <- initialCombinedModel(spec, y = y, exposure = NULL, weights = NULL)
##     filenameR <- tempfile()
##     filenameC <- tempfile()
##     set.seed(seed3)
##     ans.obtained.R.obj <- estimateOneChain(combined, tempfile = filenameR, nBurnin = 0L,
##                                          nSim = 3L, continuing = TRUE,
##                                          nThin = 1L, nAttempt = 10L, useC = FALSE)
##     con <- file(filenameR, "rb")
##     ans.obtained.R.file <- readBin(con = con, what = "double", n = 1000)
##     close(con)
##     set.seed(seed3)
##     ans.obtained.C.obj <- estimateOneChain(combined, tempfile = filenameC, nBurnin = 0L,
##                                          nSim = 3L, continuing = TRUE,
##                                          nThin = 1L, nAttempt = 10L, useC = TRUE)
##     con <- file(filenameC, "rb")
##     ans.obtained.C.file <- readBin(con = con, what = "double", n = 1000)
##     close(con)
##     set.seed(seed3)
##     ans.expected.file <- vector(mode = "list", length = 3)
##     for (i in 1:3) {
##         combined <- updateCombined(combined, n = 1L)
##         ans.expected.file[[i]] <- extractValues(combined)
##     }
##     ans.expected.file <- unlist(ans.expected.file)
##     ans.expected.obj <- combined
##     if (test.identity) {
##         expect_identical(ans.obtained.R.obj, ans.obtained.C.obj)
##         expect_identical(ans.obtained.R.file, ans.obtained.C.file)
##         expect_identical(ans.obtained.R.obj, ans.expected.obj)
##         expect_identical(ans.obtained.R.file, ans.expected.file)
##     }
##     else {
##         expect_equal(ans.obtained.R.obj, ans.obtained.C.obj)
##         expect_equal(ans.obtained.R.file, ans.obtained.C.file)
##         expect_equal(ans.obtained.R.obj, ans.expected.obj)
##         expect_equal(ans.obtained.R.file, ans.expected.file)
##     }
## })


## if (test.extended) {
##     ##compare R and C versions of estimateOneChain with a large object
##     estimateOneChain <- demest:::estimateOneChain
##     estimateOneChainInC <- demest:::estimateOneChainInC
##     estimateOneChainAllInR <- demest:::estimateOneChainAllInR
##     initialCombinedModel <- demest:::initialCombinedModel
##     updateCombined <- demest:::updateCombined
##     extractValues <- demest:::extractValues
##     ##make fake data
##     set.seed(2)
##     intercept <- 2
##     age.effect <- as.numeric(scale(rnorm(n = 101, mean = seq(from = 0, to = 2, length.out = 101), sd = 0.1)))
##     sex.effect <- as.numeric(scale(rnorm(n = 2, sd = 0.1)))
##     region.effect <- as.numeric(scale(rnorm(n = 100, sd = 0.1)))
##     time.effect <- as.numeric(scale(rnorm(n = 10, mean = seq(from = 0, to = 0.5), sd = 0.1)))
##     mu <- intercept + Reduce(function(x, y) outer(x, y, "+"),
##                              list(age.effect, sex.effect, region.effect, time.effect))
##     theta <- exp(mu)
##     dim <- c(101, 2, 100, 10)
##     dimnames <- list(age = 0:100, sex = c("f", "m"), region = 1:100, time = 2000:2009)
##     exposure <- Counts(array(as.numeric(rpois(n = prod(dim), lambda = 100)), dim = dim, dimnames = dimnames))
##     y <- Counts(array(as.numeric(rpois(n = prod(dim), lambda = exposure * theta)), dim = dim, dimnames = dimnames))
##     spec <- Model(y ~ Poisson(mean ~ age * sex * region + age * sex * time),
##                   age:sex ~ Exch(),
##                   age:region ~ Exch(),
##                   age:sex:region ~ Exch(),
##                   age:sex ~ Exch(),
##                   age:time ~ Exch(),
##                   sex:time ~ Exch(),
##                   age:sex:time ~ Exch())
##     combined <- initialCombinedModel(spec, y = y, exposure = exposure, weights = NULL)
##     ## compare times, and check final results same
##     filenameAllInR = tempfile()
##     filenameInC = tempfile()
##     filenameInRAndC = tempfile()
##     nBurnin = 10L
##     nSim = 10L
##     nThin = 5L
##     nAttempt = 10L
##     for (i in seq_len(5)) {
##         set.seed(5)  # all tests absolutely identical so only difference in timing machine conditions
##         print(system.time(ans.C <- estimateOneChainInC(combined, tempfile = filenameInC,
##                 nBurnin = nBurnin, nSim = nSim, nThin = nThin)))
##         set.seed(5)
##         print(system.time(ans.RC <- estimateOneChain(combined, tempfile = filenameInRAndC,
##                  nBurnin = nBurnin, nSim = nSim, nThin = nThin, nAttempt = nAttempt, useC = FALSE)))
##         expect_identical(ans.RC, ans.C)
##         rm(ans.C)
##         rm(ans.RC)
##     }
## }

## if (test.extended) {
##     ##compare R and C versions of estimateOneChain with a large object
##     estimateOneChain <- demest:::estimateOneChain
##     estimateOneChainInC <- demest:::estimateOneChainInC
##     estimateOneChainAllInR <- demest:::estimateOneChainAllInR
##     initialCombinedModel <- demest:::initialCombinedModel
##     updateCombined <- demest:::updateCombined
##     extractValues <- demest:::extractValues
##     ##make fake data
##     set.seed(2)
##     intercept <- 2
##     age.effect <- as.numeric(scale(rnorm(n = 101, mean = seq(from = 0, to = 2, length.out = 101), sd = 0.1)))
##     sex.effect <- as.numeric(scale(rnorm(n = 2, sd = 0.1)))
##     region.effect <- as.numeric(scale(rnorm(n = 100, sd = 0.1)))
##     time.effect <- as.numeric(scale(rnorm(n = 10, mean = seq(from = 0, to = 0.5), sd = 0.1)))
##     mu <- intercept + Reduce(function(x, y) outer(x, y, "+"),
##                              list(age.effect, sex.effect, region.effect, time.effect))
##     theta <- exp(mu)
##     dim <- c(101, 2, 100, 10)
##     dimnames <- list(age = 0:100, sex = c("f", "m"), region = 1:100, time = 2000:2009)
##     exposure <- Counts(array(as.numeric(rpois(n = prod(dim), lambda = 100)), dim = dim, dimnames = dimnames))
##     y <- Counts(array(as.numeric(rpois(n = prod(dim), lambda = exposure * theta)), dim = dim, dimnames = dimnames))
##     spec <- Model(y ~ Poisson(mean ~ age * sex * region + age * sex * time),
##                   age:sex ~ Exch(),
##                   age:region ~ Exch(),
##                   age:sex:region ~ Exch(),
##                   age:sex ~ Exch(),
##                   age:time ~ Exch(),
##                   sex:time ~ Exch(),
##                   age:sex:time ~ Exch())
##     combined <- initialCombinedModel(spec, y = y, exposure = exposure, weights = NULL)
##     ## compare times, and check final results same
##     filenameAllInR = tempfile()
##     filenameInC = tempfile()
##     filenameInRAndC = tempfile()
##     nBurnin = 10L
##     nSim = 10L
##     nThin = 5L
##     nAttempt = 10L
##     set.seed(5)  # all tests absolutely identical so only difference in timing machine conditions
##     print(system.time(ans.C <- estimateOneChainInC(combined, tempfile = filenameInC,
##                                                    nBurnin = nBurnin, nSim = nSim, nThin = nThin)))
## }


## ## comparing a pure C version with C for updateCombined, R for file writing
## ## for small object but more sims
## if (test.extended) {
##     estimateOneChain <- demest:::estimateOneChain
##     estimateOneChainInC <- demest:::estimateOneChainInC
##     initialCombinedModel <- demest:::initialCombinedModel
##     updateCombined <- demest:::updateCombined
##     extractValues <- demest:::extractValues
##     seed1 <- 1
##     seed2 <- 2
##     seed3 <- 100
##     set.seed(seed1)
##     y <- Counts(array(as.numeric(rpois(n = 20, lambda = 10)),
##                       dim = c(5, 4),
##                       dimnames = list(age = 0:4, region = letters[1:4])))
##     spec <- Model(y ~ Poisson(mean = 10, exposure = FALSE))
##     ## not continuing, nBurnin > 0, nThin > 1
##     set.seed(seed2)
##     combined <- initialCombinedModel(spec, y = y, exposure = NULL, weights = NULL)
##     filenameC <- tempfile()
##     filenameRC <- tempfile()
##     set.seed(seed3)
##     ans.obtained.C.obj <- estimateOneChainInC(combined, tempfile = filenameC, nBurnin = 3L,
##                                               nSim = 600L, continuing = FALSE,
##                                               nThin = 2L)
##     con <- file(filenameC, "rb")
##     ans.obtained.C.file <- readBin(con = con, what = "double", n = 10000)
##     close(con)
##     set.seed(seed3)
##     ans.obtained.RC.obj <- estimateOneChain(combined, tempfile = filenameRC, nBurnin = 3L,
##                                             nSim = 600L, continuing = FALSE,
##                                             nThin = 2L, nAttempt = 10L)
##     con <- file(filenameRC, "rb")
##     ans.obtained.RC.file <- readBin(con = con, what = "double", n = 10000)
##     close(con)
##     set.seed(seed3)
##     ans.expected.file <- vector(mode = "list", length = 300)
##     combined <- updateCombined(combined, nUpdate = 2L)
##     for (i in 1:300) {
##         combined <- updateCombined(combined, n = 2L)
##         ans.expected.file[[i]] <- extractValues(combined)
##     }
##     ans.expected.file <- unlist(ans.expected.file)
##     ans.expected.obj <- combined
##     expect_identical(ans.obtained.C.obj, ans.obtained.RC.obj)
##     expect_identical(ans.obtained.C.file, ans.obtained.RC.file)
##     expect_identical(ans.obtained.RC.obj, ans.expected.obj)
##     expect_identical(ans.obtained.RC.file, ans.expected.file)
## }

## if (test.extended) {
##     ## compare R and C versions of estimateOneChain with a large object
##     estimateOneChain <- demest:::estimateOneChain
##     initialCombinedModel <- demest:::initialCombinedModel
##     updateCombined <- demest:::updateCombined
##     extractValues <- demest:::extractValues
##     ## make fake data
##     intercept <- 2
##     age.effect <- as.numeric(scale(rnorm(n = 101, mean = seq(from = 0, to = 2, length.out = 101), sd = 0.1)))
##     sex.effect <- as.numeric(scale(rnorm(n = 2, sd = 0.1)))
##     region.effect <- as.numeric(scale(rnorm(n = 100, sd = 0.1)))
##     time.effect <- as.numeric(scale(rnorm(n = 10, mean = seq(from = 0, to = 0.5), sd = 0.1)))
##     mu <- intercept + Reduce(function(x, y) outer(x, y, "+"),
##                              list(age.effect, sex.effect, region.effect, time.effect))
##     theta <- exp(mu)
##     dim <- c(101, 2, 100, 10)
##     dimnames <- list(age = 0:100, sex = c("f", "m"), region = 1:100, time = 2000:2009)
##     exposure <- Counts(array(as.numeric(rpois(n = prod(dim), lambda = 100)), dim = dim, dimnames = dimnames))
##     y <- Counts(array(as.numeric(rpois(n = prod(dim), lambda = exposure * theta)), dim = dim, dimnames = dimnames))
##     ## set up model
##     spec <- Model(y ~ Poisson(mean ~ age * sex * region + age * sex * time),
##                   age:time ~ RW(along = "time"),
##                   age:sex:time ~ RW(along = "time"))
##     combined <- initialCombinedModel(spec, y = y, exposure = exposure, weights = NULL)
##     ## compare times, and check final results same
##     system.time(ans.R <- estimateOneChain(combined, tempfile = tempfile(),
##                                           nBurnin = 100L, nSim = 100L, nThin = 5L, nAttempt = 10L,
##                                           useC = FALSE))
##     system.time(ans.C <- estimateOneChain(combined, tempfile = tempfile(),
##                                           nBurnin = 100L, nSim = 100L, nThin = 5L, nAttempt = 10L,
##                                           useC = TRUE))
##     expect_equal(ans.R, ans.C)
## }

test_that("finalMessage works", {
    finalMessage <- demest:::finalMessage
    expect_message(finalMessage("filename", verbose = TRUE),
                     "results contained in file \"filename\"")
    expect_identical(finalMessage("name", verbose = FALSE), NULL)
})

test_that("makeControlArgs works", {
    makeControlArgs <- demest:::makeControlArgs
    set.seed(100)
    call <- call("estimateModel", list())
    ans.obtained <- makeControlArgs(call = call,
                                    parallel = TRUE,
                                    nUpdateMax = 200)
    ans.expected <- list(call = call,
                         parallel = TRUE,
                         lengthIter = NULL,
                         nUpdateMax = 200L)
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- makeControlArgs(call = call,
                                    parallel = FALSE,
                                    nUpdateMax = 20L)
    ans.expected <- list(call = call,
                         parallel = FALSE,
                         lengthIter = NULL,
                         nUpdateMax = 20L)
    expect_identical(ans.obtained, ans.expected)
    ## call is call
    expect_error(makeControlArgs(call = "wrong",
                                 parallel = TRUE,
                                 nUpdateMax = 200),
                 "'call' does not have class \"call\"")
    ## parallel is logical
    expect_error(makeControlArgs(call = call,
                                 parallel = "TRUE",
                                 nUpdateMax = 200),
                 "'parallel' does not have type \"logical\"")
    ## parallel has length 1
    expect_error(makeControlArgs(call = call,
                                 parallel = c(TRUE, FALSE),
                                 nUpdateMax = 200),
                 "'parallel' does not have length 1")
    ## parallel is not missing
    expect_error(makeControlArgs(call = call,
                                 parallel = NA,
                                 nUpdateMax = 200),
                 "'parallel' is missing")
    ## 'nUpdateMax' has length 1
    expect_error(makeControlArgs(call = call,
                                 parallel = TRUE,
                                 nUpdateMax = c(200, 200)),
                 "'nUpdateMax' does not have length 1")
    ## 'nUpdateMax' is not missing
    expect_error(makeControlArgs(call = call,
                                 parallel = TRUE,
                                 nUpdateMax = NA),
                 "'nUpdateMax' is missing")
    ## 'nUpdateMax' is numeric
    expect_error(makeControlArgs(call = call,
                                 parallel = TRUE,
                                 nUpdateMax = "200"),
                 "'nUpdateMax' is non-numeric")
    ## 'nUpdateMax' is integer
    expect_error(makeControlArgs(call = call,
                                 parallel = TRUE,
                                 nUpdateMax = 200.1),
                 "'nUpdateMax' has non-integer value")
    expect_error(makeControlArgs(call = call,
                                 parallel = TRUE,
                                 nUpdateMax = 0L),
                 "'nUpdateMax' is less than 1")
})

test_that("makeMCMCArgs works", {
    makeMCMCArgs <- demest:::makeMCMCArgs
    ans.obtained <- makeMCMCArgs(nBurnin = 1000,
                                 nSim = 1000,
                                 nChain = 5,
                                 nThin = 10,
                                 nCore = 4)
    ans.expected <- list(nBurnin = 1000L,
                         nSim = 1000L,
                         nChain = 5L,
                         nThin = 10L,
                         nCore = 4L)
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- makeMCMCArgs(nBurnin = 0,
                                 nSim = 1,
                                 nChain = 1,
                                 nThin = 1)
    ans.expected <- list(nBurnin = 0L,
                         nSim = 1L,
                         nChain = 1L,
                         nThin = 1L,
                         nCore = 1L)
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- makeMCMCArgs(nBurnin = 0,
                                 nSim = 0,
                                 nChain = 1,
                                 nThin = 1)
    ans.expected <- list(nBurnin = 0L,
                         nSim = 0L,
                         nChain = 1L,
                         nThin = 1L,
                         nCore = 1L)
    expect_identical(ans.obtained, ans.expected)
    ## length 1
    expect_error(makeMCMCArgs(nBurnin = 1:2, nSim = 1000, nChain = 5, nThin = 2),
                 "'nBurnin' does not have length 1")
    ## is not missing
    expect_error(makeMCMCArgs(nBurnin = 1000, nSim = NA, nChain = 5, nThin = 2),
                 "'nSim' is missing")
    ## is numeric
    expect_error(makeMCMCArgs(nBurnin = 1000, nSim = 1000, nChain = "5", nThin = 2),
                 "'nChain' is non-numeric")
    ## is integer
    expect_error(makeMCMCArgs(nBurnin = 1000, nSim = 1000, nChain = 5, nThin = 2.1),
                 "'nThin' has non-integer value")
    ## 'nBurnin', 'nSim' non-negative
    expect_error(makeMCMCArgs(nBurnin = -1, nSim = 1000, nChain = 5, nThin = 2),
                 "'nBurnin' is negative")
    expect_error(makeMCMCArgs(nBurnin = 2, nSim = -1, nChain = 5, nThin = 2),
                 "'nSim' is negative")
    ## 'nChain', 'nThin' positive
    expect_error(makeMCMCArgs(nBurnin = 1000, nSim = 0, nChain = 0, nThin = 2),
                 "'nChain' is less than 1")
    ## nThin <= nSim if nSim > 0
    expect_error(makeMCMCArgs(nBurnin = 1000, nSim = 5, nChain = 5, nThin = 10),
                 "'nThin' is greater than 'nSim'")
})


## INSPECT RESULTS ###################################################################

test_that("addIterationsToTransform works", {
    addIterationsToTransform <- demest:::addIterationsToTransform
    transform <- new("CollapseTransform",
                     indices = list(1:3, c(1L, 1L)),
                     dims = c(1L, 0L),
                     dimBefore = 3:2,
                     dimAfter = 3L)
    ans.obtained <- addIterationsToTransform(transform, nIter = 10L)
    ans.expected <- new("CollapseTransform",
                        indices = list(1:3, c(1L, 1L), 1:10),
                        dims = c(1L, 0L, 2L),
                        dimBefore = c(3:2, 10L),
                        dimAfter = c(3L, 10L))
    expect_identical(ans.obtained, ans.expected)
    transform <- new("CollapseTransform",
                     indices = list(c(1L, 1L), c(1L, 1L), 1:4),
                     dims = c(0L, 0L, 1L),
                     dimBefore = c(2L, 2L, 4L),
                     dimAfter = 4L)
    ans.obtained <- addIterationsToTransform(transform, nIter = 100L)
    ans.expected <- new("CollapseTransform",
                        indices = list(c(1L, 1L), c(1L, 1L), 1:4, 1:100),
                        dims = c(0L, 0L, 1L, 2L),
                        dimBefore = c(2L, 2L, 4L, 100L),
                        dimAfter = c(4L, 100L))
    expect_identical(ans.obtained, ans.expected)
})    

test_that("calculateDF works", {
    calculateDF <- demest:::calculateDF
    ## 5
    expect_identical(calculateDF(5L), 4L)
    ## c(3, 2)
    expect_identical(calculateDF(c(3L, 2L)), 2L)
    ## c(4, 3)
    expect_identical(calculateDF(c(4L, 3L)), 6L)
    ## c(5, 4, 3)
    expect_identical(calculateDF(5:3),
                     60L - 1L - 4L - 3L - 2L - calculateDF(5:4) -
                     calculateDF(4:3) - calculateDF(c(5L, 3L)))
    expect_error(calculateDF(c(3, 4)),
                 "'dim' does not have type \"integer\"")
    expect_error(calculateDF(integer()),
                 "'dim' has length 0")
    expect_error(calculateDF(c(2L, NA)),
                 "'dim' has missing values")
    expect_error(calculateDF(c(1L, 5L)),
                 "'dim' has values less than 2")
})

test_that("centerPolyGammaTrend works", {
    centerPolyGammaTrend <- demest:::centerPolyGammaTrend
    object <- Values(array(rnorm(300),
                           dim = c(3, 10, 10),
                           dimnames = list(order = c("order1", "order2", "order3"),
                               time = 2001:2010,
                               iteration = 1:10)),
                     dimscales = c(time = "Intervals"))
    ans.obtained <- centerPolyGammaTrend(object)
    ans.expected <- object
    for (i in 1:10)
        ans.expected[1,,i] <- ans.expected[1,,i] - mean(ans.expected[1,,i])
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})

test_that("checkProbs works", {
    checkProbs <- demest:::checkProbs
    expect_identical(checkProbs(c(0.025, 0.5, 0.975)),
                     NULL)
    expect_error(checkProbs("1"),
                 "'probs' is non-numeric")
    expect_error(checkProbs(numeric()),
                 "'probs' has length 0")
    expect_error(checkProbs(c(0.2, NA)),
                 "'probs' has missing values")
    expect_error(checkProbs(c(0.2, 0.2)),
                 "'probs' has duplicates")
    expect_error(checkProbs(c(0.2, -0.2)),
                 "'probs' has negative values")
    expect_error(checkProbs(c(0.2, 1.2)),
                 "'probs' has values greater than 1")
})

test_that("centerAlong works", {
    centerAlong <- demest:::centerAlong
    ## dim = c(3, 10, 10), iAlong = 2
    object <- Values(array(rnorm(300),
                           dim = c(3, 10, 10),
                           dimnames = list(order = c("reg1", "reg2", "reg3"),
                               time = 2001:2010,
                               iteration = 1:10)),
                     dimscales = c(time = "Intervals"))
    ans.obtained <- centerAlong(object, iAlong = 2L)
    ans.expected <- object
    for (i in 1:3) {
        for (k in 1:10)
            ans.expected[i,,k] <- ans.expected[i,,k] - mean(ans.expected[i,,k])
    }
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
    ## dim = c(20, 10), iAlong = 1
    object <- Values(array(rnorm(300),
                           dim = c(20, 10),
                           dimnames = list(time = 1:20,
                               iteration = 1:10)),
                     dimscales = c(time = "Intervals"))
    ans.obtained <- centerAlong(object, iAlong = 1L)
    ans.expected <- object
    for (j in 1:10)
          ans.expected[,j] <- ans.expected[,j] - mean(ans.expected[,j])
    if (test.identity)
        expect_identical(ans.obtained, ans.expected)
    else
        expect_equal(ans.obtained, ans.expected)
})


test_that("combineEstPredHelper works with valid inputs", {
    combineEstPredHelper <- demest:::combineEstPredHelper
    ## objects compatible - Categories
    est <- Values(array(rnorm(60),
                          dim = 3:5,
                          dimnames = list(region = c("a", "b", "c"),
                              time = 1:4,
                              iteration = 1:5)),
                  dimscales = c(time = "Intervals"))
    pred <- Values(array(rnorm(60),
                           dim = 3:5,
                           dimnames = list(region = c("e", "f", "g"),
                               time = 1:4,
                               iteration = 1:5)),
                   dimscales = c(time = "Intervals"))
    ans.obtained <- combineEstPredHelper(est = est,
                                             pred = pred)
    ans.expected <- dbind(est, pred, along = "region")
    ans.expected <- list(.Data = ans.expected@.Data,
                         metadata = ans.expected@metadata)
    expect_identical(ans.obtained, ans.expected)
    ## objects compatible - Intervals
    est <- Values(array(rnorm(60),
                          dim = 3:5,
                          dimnames = list(region = c("a", "b", "c"),
                              time = 1:4,
                              iteration = 1:5)),
                  dimscales = c(time = "Intervals"))
    pred <- Values(array(rnorm(60),
                           dim = c(3, 5, 5),
                           dimnames = list(region = c("a", "b", "c"),
                               time = 5:9,
                               iteration = 1:5)),
                   dimscales = c(time = "Intervals"))
    ans.obtained <- combineEstPredHelper(est = est,
                                             pred = pred)
    ans.expected <- dbind(est, pred, along = "time")
    ans.expected <- list(.Data = ans.expected@.Data,
                         metadata = ans.expected@metadata)
    expect_identical(ans.obtained, ans.expected)
})

test_that("combineEstPredHelper throws appropriate errors", {
    combineEstPredHelper <- demest:::combineEstPredHelper
    ## different names
    est <- Values(array(rnorm(60),
                        dim = 3:5,
                        dimnames = list(region = c("a", "b", "c"),
                            time = 1:4,
                            iteration = 1:5)),
                  dimscales = c(time = "Intervals"))
    pred <- Values(array(rnorm(60),
                         dim = 3:5,
                         dimnames = list(region = c("a", "b", "c"),
                             Time = 5:8,
                             iteration = 1:5)),
                   dimscales = c(Time = "Intervals"))
    expect_error(combineEstPredHelper(est = est,
                                      pred = pred),
                 "results from 'est' and 'pred' have different 'names'")
    ## different dimtypes
    est <- Values(array(rnorm(60),
                        dim = 3:5,
                        dimnames = list(region = c("a", "b", "c"),
                            time = 1:4,
                            iteration = 1:5)),
                  dimscales = c(time = "Intervals"))
    pred <- Values(array(rnorm(60),
                         dim = 3:5,
                         dimnames = list(region = c("a", "b", "c"),
                             time = 5:8,
                             iteration = 1:5)),
                   dimtypes = c(time = "state"))
    expect_error(combineEstPredHelper(est = est,
                                      pred = pred),
                 "results from 'est' and 'pred' have different 'dimtypes'")
    ## different dimscales
    est <- Values(array(rnorm(60),
                        dim = 3:5,
                        dimnames = list(region = c("a", "b", "c"),
                            time = 1:4,
                            iteration = 1:5)),
                  dimscales = c(time = "Intervals"))
    pred <- Values(array(rnorm(40),
                         dim = c(2, 4, 5),
                         dimnames = list(region = c("a", "b"),
                             time = 5:8,
                             iteration = 1:5)),
                   dimscales = c(time = "Intervals"))
    expect_error(combineEstPredHelper(est = est,
                                      pred = pred),
                 "results from 'est' and 'pred' have incompatible dimensions or 'dimscales'")
})

test_that("finiteSDInner works with ResultsModel from BinomialVarying", {
    fetchResultsObject <- demest:::fetchResultsObject
    finiteSDInner <- demest:::finiteSDInner
    exposure <- Counts(array(as.numeric(rpois(n = 24, lambda = 10)),
                             dim = 2:4,
                             dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rbinom(n = 24, size = exposure, prob = 0.8)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(time = "Intervals"))
    filename <- tempfile()
    estimateModel(Model(y ~ Binomial(mean ~ age + sex)),
                  y = y,
                  exposure = exposure,
                  nBurnin = 0,
                  nSim = 2,
                  nThin = 2,
                  nChain = 2,
                  filename = filename)
    object <- fetchResultsObject(filename)
    ans.obtained <- finiteSDInner(filename = filename,
                                  model = object@final[[1]]@model,
                                  where = "model",
                                  probs = c(0.025, 0.5, 0.975),
                                  iterations = NULL)
    beta.age <- fetch(filename, c("model", "prior", "age"))
    beta.sex <- fetch(filename, c("model", "prior", "sex"))
    theta <- fetch(filename, c("model", "likelihood", "prob"))
    gtheta <- log(theta / (1 - theta))
    SS.age <- apply(beta.age@.Data, 2, function(x) sum((x - mean(x))^2))
    SS.sex <- apply(beta.sex@.Data, 2, function(x) sum((x - mean(x))^2))
    sd.age <- sqrt(SS.age / 2)
    sd.sex <- sqrt(SS.sex)
    ans.expected <- rbind(age = quantile(sd.age, probs = c(0.025, 0.5, 0.975)),
                          sex = quantile(sd.sex, probs = c(0.025, 0.5, 0.975)))
    colnames(ans.expected) <- c("2.5%", "50%", "97.5%")
    names(dimnames(ans.expected)) <- c("term", "quantile")
    ans.expected <- Values(ans.expected)
    ans.expected <- new("FiniteSD",
                        ans.expected,
                        df = c(2L, 1L))
    expect_equal(ans.obtained, ans.expected)
})

test_that("foldMCMCList works", {
    foldMCMCList <- demest:::foldMCMCList
    mcmc.list <- coda::mcmc.list
    mcmc <- coda::mcmc
    l <- mcmc.list(list(mcmc(matrix(c(1L, 3L, 5L, 2L, 4L, 6L),
                                    nrow = 3,
                                    dimnames = list(NULL, c("f", "m"))),
                             thin = 4),
                        mcmc(matrix(c(7L, 9L, 11L, 8L, 10L, 12L),
                                    nrow = 3,
                                    dimnames = list(NULL, c("f", "m"))),
                             thin = 4)))
    ans.obtained <- foldMCMCList(l)
    ans.expected <- mcmc.list(list(mcmc(matrix(c(1L, 2L),
                                               nrow = 1,
                                               dimnames = list(NULL, c("f", "m"))),
                                        thin = 4),
                                   mcmc(matrix(c(5L, 6L),
                                               nrow = 1,
                                               dimnames = list(NULL, c("f", "m"))),
                                        thin = 4),
                                   mcmc(matrix(c(7L, 8L),
                                               nrow = 1,
                                               dimnames = list(NULL, c("f", "m"))),
                                        thin = 4),
                                   mcmc(matrix(c(11L, 12L),
                                               nrow = 1,
                                               dimnames = list(NULL, c("f", "m"))),
                                        thin = 4)))
    expect_identical(ans.obtained, ans.expected)
    expect_error(foldMCMCList(ans.obtained),
                 "'l' has fewer than 2 rows")
    expect_error(foldMCMCList("wrong"),
                 "'l' has class \"character\"")
})

test_that("fetchSkeleton and fetchSkeletonInner work with ResultsModel from BinomialVarying", {
    fetchResultsObject <- demest:::fetchResultsObject
    fetchSkeleton <- demest:::fetchSkeleton
    exposure <- Counts(array(as.numeric(rpois(n = 24, lambda = 10)),
                             dim = 2:4,
                             dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rbinom(n = 24, size = exposure, prob = 0.8)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(time = "Intervals"))
    filename <- tempfile()
    estimateModel(Model(y ~ Binomial(mean ~ age + sex)),
                         y = y,
                         exposure = exposure,
                         nBurnin = 0,
                         nSim = 2, nThin = 2,
                         nChain = 2,
                         filename = filename)
    object <- fetchResultsObject(filename)
    ans.obtained <- fetchSkeleton(object, where = c("mod", "lik", "prob"))
    ans.expected <- object@model$likelihood$prob
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- fetchSkeleton(object, where = c("model", "prior", "sd"))
    ans.expected <- object@model$prior$sd
    expect_identical(ans.obtained, ans.expected)
})

test_that("MCMCDemographic works", {
    MCMCDemographic <- demest:::MCMCDemographic
    Skeleton <- demest:::Skeleton
    mcmc.list <- coda:::mcmc.list
    mcmc <- coda:::mcmc
    x <- Counts(array(1:12, dim = c(2, 6), dimnames = list(sex = c("f", "m"), iteration = 1:6)))
    y <- mcmc.list(list(mcmc(matrix(c(1L, 3L, 5L, 2L, 4L, 6L),
                                    nrow = 3,
                                    dimnames = list(NULL, c("f", "m")))),
                        mcmc(matrix(c(7L, 9L, 11L, 8L, 10L, 12L),
                                    nrow = 3,
                                    dimnames = list(NULL, c("f", "m"))))))
    expect_identical(MCMCDemographic(x, nChain = 2), y)
    expect_identical(MCMCDemographic(x, sample = 1:2, nChain = 2), y)
    x <- Counts(array(1:12, dim = c(2, 6), dimnames = list(sex = c("f", "m"), iteration = 1:6)))
    y <- mcmc.list(list(mcmc(matrix(c(2L, 4L, 6L, 1L, 3L, 5L),
                                    nrow = 3,
                                    dimnames = list(NULL, c("m", "f")))),
                        mcmc(matrix(c(8L, 10L, 12L, 7L, 9L, 11L),
                                    nrow = 3,
                                    dimnames = list(NULL, c("m", "f"))))))
    expect_identical(MCMCDemographic(x, sample = 2:1, nChain = 2), y)
    x <- Counts(array(1:12, dim = c(2, 6), dimnames = list(sex = c("f", "m"), state = 1:6)))
    expect_error(MCMCDemographic(x, nChain = 2),
                 "no dimension with dimtype \"iteration\"")
    x <- Counts(array(1:12, dim = c(2, 6), dimnames = list(sex = c("f", "m"), iteration = 1:6)))
    expect_error(MCMCDemographic("wrong", nChain = 2),
                 "'object' has class \"character\"")
    expect_error(MCMCDemographic(x, nChain = "2"),
                 "'nChain' does not have type \"numeric\"")
    expect_error(MCMCDemographic(x, nChain = 1:2),
                 "'nChain' does not have length 1")
    expect_error(MCMCDemographic(x, nChain = 2, nThin = as.numeric(NA)),
                 "'nThin' is missing")
    expect_error(MCMCDemographic(x, nChain = 2, nThin = 0),
                 "'nThin' is less than 1")
    expect_error(MCMCDemographic(x, nChain = 5),
                 "number of iterations is not divisible by 'nChain'")
    x <- Counts(array(1:12, dim = c(2, 6), dimnames = list(sex = c("f", "m"), iteration = 1:6)))
    y <- mcmc.list(list(mcmc(matrix(c(1L, 3L, 5L, 2L, 4L, 6L),
                                    nrow = 3,
                                    dimnames = list(NULL, c("f", "m"))),
                             thin = 3),
                        mcmc(matrix(c(7L, 9L, 11L, 8L, 10L, 12L),
                                    nrow = 3,
                                    dimnames = list(NULL, c("f", "m"))),
                             thin = 3)))
    expect_identical(MCMCDemographic(x, nChain = 2, nThin = 3), y)
    x <- Counts(array(1:6, dim = c(2, 3), dimnames = list(iteration = 1:2, age = c("0-4", "5-9", "10+"))))
    y <- mcmc.list(list(mcmc(matrix(1:6,
                                    nrow = 2,
                                    dimnames = list(NULL, c("0-4", "5-9", "10+"))))))
    expect_identical(MCMCDemographic(x, nChain = 1), y)
    ## has structural zeros
    x <- Values(array(1:12, dim = c(2, 6), dimnames = list(sex = c("f", "m"), iteration = 1:6)))
    strucZeroArray <- CountsOne(c(1L, 0L), labels = c("f", "m"), name = "sex")
    object <- ValuesOne(rep(0, 2), labels = c("f", "m"), name = "sex")
    skeleton <- Skeleton(object, first = 10L, strucZeroArray = strucZeroArray, margin = 1L)
    ans.obtained <- MCMCDemographic(x, nChain = 2, nThin = 3, skeleton = skeleton)
    ans.expected <- mcmc.list(list(mcmc(matrix(c(1L, 3L, 5L),
                                               nrow = 3,
                                               dimnames = list(NULL, "f")),
                                        thin = 3),
                                   mcmc(matrix(c(7L, 9L, 11L),
                                               nrow = 3,
                                               dimnames = list(NULL, "f")),
                                        thin = 3)))
    expect_identical(ans.obtained, ans.expected)
})

test_that("checkAndTidyTotalOrSampled works", {
    checkAndTidyTotalOrSampled <- demest:::checkAndTidyTotalOrSampled
    x <- Counts(array(1:2, dim = 2, dimnames = list(sex = c("f", "m"))))
    model <- new("BinomialVarying")
    ySampled <- Counts(array(1:2, dim = 2, dimnames = list(sex = c("f", "m"))))
    ## valid args
    expect_identical(checkAndTidyTotalOrSampled(x = x, model = model, ySampled = ySampled,
                                                name = "total"),
                     x)
    ## not Counts
    x.wrong <- as(x, "Values")
    expect_error(checkAndTidyTotalOrSampled(x.wrong, model = model, ySampled = ySampled,
                                            name = "total"),
                 "'total' has class \"Values\"")
    ## missing values
    x.wrong <- x
    x.wrong[1] <- NA
    expect_error(checkAndTidyTotalOrSampled(x.wrong, model = model, ySampled = ySampled,
                                            name = "total"),
                 "'total' has missing values")
    ## negative values
    x.wrong <- x
    x.wrong[1] <- -1
    expect_error(checkAndTidyTotalOrSampled(x.wrong, model = model, ySampled = ySampled,
                                            name = "total"),
                 "'total' has negative values")
    ## incompatible
    x.wrong <- Counts(array(1:2, dim = 2, dimnames = list(sex = c("female", "male"))))
    expect_error(checkAndTidyTotalOrSampled(x.wrong, model = model, ySampled = ySampled,
                                            name = "total"),
                 "'total' and 'y' incompatible : ")
})

test_that("checkMax works", {
    checkMax <- demest:::checkMax
    expect_identical(checkMax(NULL), NULL)
    expect_identical(checkMax(1), NULL)
    expect_error(checkMax(1:2),
                 "'max' does not have length 1")
    expect_error(checkMax("1"),
                 "'max' does not have type \"numeric\"")
    expect_error(checkMax(as.integer(NA)),
                 "'max' is missing")
    expect_error(checkMax(1.2),
                 "'max' is not an integer")
    expect_error(checkMax(0),
                 "'max' is less than 1")
})

test_that("excludeFromList works", {
    excludeFromList <- demest:::excludeFromList
    l <- list(a = 1, b = list(c = 1, a = 2))
    expect_identical(excludeFromList(l, exclude = "a"),
                     list(a = NULL, b = list(c = 1, a = NULL)))
    expect_identical(excludeFromList(l, exclude = "b"),
                     l)
    expect_identical(excludeFromList(l, exclude = "c"),
                     list(a = 1, b = list(c = NULL, a = 2)))
    expect_identical(excludeFromList(l, exclude = c("a", "b", "c")),
                     list(a = NULL, b = list(c = NULL, a = NULL)))
    expect_identical(excludeFromList(l),
                     l)
    expect_identical(excludeFromList(list()),
                     list())
    expect_error(excludeFromList("wrong"),
                 "'object' has class \"character\"")
})

test_that("fetchResultsObject works", {
    fetchResultsObject <- demest:::fetchResultsObject
    makeResultsFile <- demest:::makeResultsFile
    ## files are 500 char long
    filename <- tempfile()
    results <- new("ResultsModelEst")
    tempfiles <- c(tempfile(), tempfile(), tempfile())
    con <- file(filename, "wb")
    res.vec <- serialize(results, connection = NULL)
    size.res <- length(res.vec)
    writeBin(size.res, con = con)
    writeBin(0L, con = con)
    writeBin(res.vec, con = con)
    close(con)
    for (i in 1:3) {
        con <- file(tempfiles[i], "wb")
        writeBin((1:500) + (i - 1) * 500, con)
        close(con)
    }
    makeResultsFile(filename = filename, results = results, tempfiles = tempfiles)
    ans.obtained <- fetchResultsObject(filename)
    ans.expected <- results
    expect_identical(ans.obtained, ans.expected)
})


test_that("fetchInner works", {
    fetchInner <- demest:::fetchInner
    ans.obtained <- fetchInner(object = list(a = 1),
                               nameObject = "x",
                               where = character(),
                               iterations = NULL,
                               filename = "f",
                               lengthIter = 5L,
                               nIteration = 10L,
                               listsAsSingleItems = "x")
    ans.expected <- list(a = 1)
    expect_identical(ans.obtained, ans.expected)
    expect_error(fetchInner(object = list(a = 1),
                               nameObject = "x",
                               where = character(),
                               iterations = NULL,
                               filename = "f",
                               lengthIter = 5L,
                               nIteration = 10L,
                               listsAsSingleItems = "xxx"),
                 sprintf("'where' stops before end of hierarchy : remaining choice is %s",
                         sQuote("a")))
    expect_error(fetchInner(object = list(a = 1, b = 2),
                               nameObject = "x",
                               where = character(),
                               iterations = NULL,
                               filename = "f",
                               lengthIter = 5L,
                               nIteration = 10L,
                               listsAsSingleItems = "xxx"),
                 sprintf("'where' stops before end of hierarchy : remaining choices are %s",
                         paste(sQuote(c("a", "b")), collapse = ", ")))
    ans.obtained <- fetchInner(object = 1,
                               nameObject = "obj",
                               where = character(),
                               iterations = NULL,
                               filename = "f",
                               lengthIter = 5L,
                               nIteration = 10L,
                               listsAsSingleItems = "x")
    ans.expected <- 1
    expect_identical(ans.obtained, ans.expected)
    l <- list(a = list(e = 1), b = list(c = 2, d = 3))
    ans.obtained <- fetchInner(object = l,
                               nameObject = "l",
                               where = c("b", "c"),
                               iterations = NULL,
                               filename = "f",
                               lengthIter = 5L,
                               nIteration = 10L,
                               listsAsSingleItems = "x")
    ans.expected <- 2
    expect_identical(ans.obtained, ans.expected)
    l <- list(aa = 1, ab = list(c = 2, d = 3))
    expect_error(fetchInner(object = l,
                            nameObject = "l",
                            where = "a",
                            iterations = NULL,
                            filename = "f",
                            lengthIter = 5L,
                            nIteration = 10L,
                            listsAsSingleItems = "x"),
                 sprintf("'a' partially matches two or more of %s",
                         paste(sQuote(c("aa", "ab")), collapse = ", ")))
    l <- list(a = 1, b = list(c = 2, d = 3))
    expect_error(fetchInner(object = l,
                            nameObject = "l",
                            where = c("b", "wrong"),
                            iterations = NULL,
                            filename = "f",
                            lengthIter = 5L,
                            nIteration = 10L,
                            listsAsSingleItems = "x"),
                 sprintf("'wrong' not found : choices are %s, %s",
                         sQuote("c"), sQuote("d")))
    l <- list(a = 1, b = list(c = 2, d = 3))
    expect_error(fetchInner(object = l,
                            nameObject = "l",
                            where = c("b", "c", "wrong"),
                            iterations = NULL,
                            filename = "f",
                            lengthIter = 5L,
                            nIteration = 10L,
                            listsAsSingleItems = "x"),
                 sprintf("hierarchy only extends to 'c' : 'where' has additional term %s",
                         dQuote("wrong")))
})

test_that("getDataFromFile gives valid answer", {
    getDataFromFile <- demest:::getDataFromFile
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    size.results <- length(results)
    data <- as.double(rep((1:20) * 100, each = 10) + 1:10)
    filename <- tempfile()
    con <- file(filename, open = "wb")
    writeBin(size.results, con = con)
    writeBin(10L, con = con)
    writeBin(results, con = con)
    writeBin(data, con = con)
    close(con)
    ans.obtained <- getDataFromFile(filename = filename,
                                    first = 1L,
                                    last = 1L,
                                    lengthIter = 10L,
                                    iterations = 1:20,
                                    useC = FALSE)
    ans.expected <- as.double((1:20) * 100 + 1L)
    expect_identical(ans.obtained, ans.expected)
    ## elements 2-4 from each row
    ans.obtained <- getDataFromFile(filename = filename,
                                    first = 2L,
                                    last = 4L,
                                    lengthIter = 10L,
                                    iterations = 1:20,
                                    useC = FALSE)
    ans.expected <- as.double(rep((1:20) * 100, each = 3) + 2:4)
    expect_identical(ans.obtained, ans.expected)
    ## elements 6-10 from rows 2, 5 8, 10
    ans.obtained <- getDataFromFile(filename = filename,
                                    first = 6L,
                                    last = 10,
                                    lengthIter = 10L,
                                    iterations = c(2L, 5L, 8L, 10L),
                                    useC = FALSE)
    ans.expected <- as.double(rep(c(2, 5, 8, 10) * 100, each = 5) + 6:10)
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of getDataFromFile give same answer", {
    getDataFromFile <- demest:::getDataFromFile
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    adjustments <- new.env(hash = TRUE)
    adjustments[["nm"]] <- 1
    adjustments <- serialize(adjustments, connect = NULL)
    size.results <- length(results)
    size.adjustments <- length(adjustments)
    data <- rnorm(n = 1000)
    filename <- tempfile()
    con <- file(filename, open = "wb")
    writeBin(size.results, con = con)
    writeBin(size.adjustments, con)
    writeBin(results, con = con)
    writeBin(data, con = con)
    writeBin(adjustments, con = con)
    close(con)
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        firstlast <- sort(sample.int(n = 20, size = 2, replace = FALSE))
        n.iter <- sample.int(n = 50, size = 1)
        iterations <- sort(sample.int(n = 50, size = n.iter, replace = FALSE))
        ans.R <- getDataFromFile(filename = filename,
                                 first = firstlast[1],
                                 last = firstlast[2],
                                 lengthIter = 20L,
                                 iterations = iterations,
                                 useC = FALSE)
        ans.C <- getDataFromFile(filename = filename,
                                 first = firstlast[1],
                                 last = firstlast[2],
                                 lengthIter = 20L,
                                 iterations = iterations,
                                 useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
})

## larger file, with timing test
if (test.extended) {
    getDataFromFile <- demest:::getDataFromFile
    results <- new("ResultsModelEst")
    results <- serialize(results, connection = NULL)
    size.results <- length(results)
    data <- rnorm(n = 1000000)
    filename <- tempfile()
    con <- file(filename, open = "wb")
    writeBin(size.results, con = con)
    writeBin(10L, con = con)
    writeBin(results, con = con)
    writeBin(data, con = con)
    close(con)
    for (seed in seq_len(5)) {
        set.seed(seed*10)
        firstlast <- sort(sample.int(n = 2000, size = 2, replace = FALSE))
        n.iter <- sample.int(n = 500, size = 1)
        iterations <- sort(sample.int(n = 500, size = n.iter, replace = FALSE))
        print(system.time(ans.R <- getDataFromFile(filename = filename,
                                 first = firstlast[1],
                                 last = firstlast[2],
                                 lengthIter = 2000L,
                                 iterations = iterations,
                                 useC = FALSE)))
        print(system.time(ans.C <- getDataFromFile(filename = filename,
                                 first = firstlast[1],
                                 last = firstlast[2],
                                 lengthIter = 2000L,
                                 iterations = iterations,
                                 useC = TRUE)))
        expect_identical(ans.R, ans.C)
    }
}

test_that("R and C versions of getDataFromFile give same answer with gaps in iterations", {
    set.seed(1L)
    getDataFromFile <- demest:::getDataFromFile
    filename <- tempfile()
    accept <- rep(c(0, 1), times = 5)
    data <- matrix(rnorm(90), ncol = 10)
    data <- as.double(rbind(data, accept))
    filename <- tempfile()
    con <- file(filename, "wb")
    results <- new("ResultsModelEst")
    adjustments <- new.env(hash = TRUE)
    results <- serialize(results, connection = NULL)
    adjustments <- serialize(adjustments, connection = NULL)
    size.results <- length(results)
    size.adjustments <- length(adjustments)
    writeBin(size.results, con)
    writeBin(size.adjustments, con)
    writeBin(results, con)
    writeBin(data, con)
    writeBin(adjustments, con)
    close(con)
    ans.R <- getDataFromFile(filename = filename,
                             first = 10L,
                             last = 10L,
                             lengthIter = 10L,
                             iterations = c(2:5, 7:10),
                             useC = FALSE)
    ans.C <- getDataFromFile(filename = filename,
                             first = 10L,
                             last = 10L,
                             lengthIter = 10L,
                             iterations = c(2:5, 7:10),
                             useC = TRUE)
    expect_identical(ans.R, ans.C)
})

test_that("getOneIterFromFile gives valid answer", {
    getOneIterFromFile <- demest:::getOneIterFromFile
    data <- as.double(rep((1:20) * 100, each = 10) + 1:10)
    filename <- tempfile()
    con <- file(filename, open = "wb")
    writeBin(data, con = con)
    close(con)
    ans.obtained <- getOneIterFromFile(filename = filename,
                                       first = 1L,
                                       last = 5L,
                                       lengthIter = 10L,
                                       iteration = 4L,
                                       useC = FALSE)
    ans.expected <- as.double(401:405)
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getOneIterFromFile(filename = filename,
                                       first = 10L,
                                       last = 10L,
                                       lengthIter = 10L,
                                       iteration = 20L,
                                       useC = FALSE)
    ans.expected <- as.double(2010)
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of getOneIterFromFile give same answer", {
    getOneIterFromFile <- demest:::getOneIterFromFile
    data <- as.double(rep((1:20) * 100, each = 10) + 1:10)
    filename <- tempfile()
    con <- file(filename, open = "wb")
    writeBin(data, con = con)
    close(con)
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        first <- sample(5, 1)
        last <- first + sample(5, 1)
        iteration <- sample(20, 1)
        ans.R <- getOneIterFromFile(filename = filename,
                                    first = first,
                                    last = last,
                                    lengthIter = 10L,
                                    iteration = iteration,
                                    useC = FALSE)
        ans.C <- getOneIterFromFile(filename = filename,
                                    first = first,
                                    last = last,
                                    lengthIter = 10L,
                                    iteration = iteration,
                                    useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
})

test_that("giveListElementsLongNames works", {
  giveListElementsLongNames <- demest:::giveListElementsLongNames
  l <- list(a = 1, b = list(c = 1, d = 3, e = list(f = 2)))
  expect_identical(giveListElementsLongNames(l),
                   list(a = 1, b = list(b.c = 1, b.d = 3, e = list(b.e.f = 2))))
  expect_identical(giveListElementsLongNames(l, names = "x"),
                   list(x.a = 1, b = list(x.b.c = 1, x.b.d = 3, e = list(x.b.e.f = 2))))
  expect_identical(giveListElementsLongNames(list()),
                   list())
  expect_error(giveListElementsLongNames("wrong"),
               "'object' has class \"character\"")
})

test_that("isSaturated works", {
    isSaturated <- demest:::isSaturated
    initialModel <- demest:::initialModel
    exposure <- Counts(array(as.integer(rpois(n = 20, lambda = 10)),
                             dim = 5:4,
                             dimnames = list(age = 0:4, region = letters[1:4])))
    y <- Counts(array(as.integer(rbinom(n = 20, size = exposure, prob = 0.1)),
                      dim = 5:4,
                      dimnames = list(age = 0:4, region = letters[1:4])))
    spec <- Model(y ~ Binomial(mean ~ region))
    x <- initialModel(spec, y = y, exposure = exposure)
    ans.obtained <- isSaturated(x)
    expect_false(ans.obtained)
    spec <- Model(y ~ Binomial(mean ~ age * region))
    x <- initialModel(spec, y = y, exposure = exposure)
    ans.obtained <- isSaturated(x)
    expect_true(ans.obtained)
})


test_that("isTimeVarying works", {
    isTimeVarying <- demest:::isTimeVarying
    expose <- Counts(array(as.numeric(rpois(n = 24, lambda = 10)),
                           dim = 2:4,
                           dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                     dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rbinom(n = 24, size = expose, prob = 0.8)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(time = "Intervals"))
    filename.est <- tempfile()
    filename.pred <- tempfile()
    estimateModel(Model(y ~ Binomial(mean ~ age + time)),
                  y = y,
                  exposure = expose,
                  nBurnin = 0,
                  nSim = 2,
                  nChain = 1,
                  filename = filename.est)
    predictModel(filenameEst = filename.est,
                 filenamePred = filename.pred,
                 n = 3)
    ans.obtained <- isTimeVarying(filenameEst = filename.est,
                                  filenamePred = filename.pred,
                                  where = c("model", "prior", "time"))
    ans.expected <- TRUE
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- isTimeVarying(filenameEst = filename.est,
                                  filenamePred = filename.pred,
                                  where = c("model", "prior", "age"))
    ans.expected <- FALSE
    expect_identical(ans.obtained, ans.expected)
})

test_that("listsAsSingleItems works", {
    listsAsSingleItems <- demest:::listsAsSingleItems
    expect_identical(listsAsSingleItems(), c("control", "final", "seed"))
})

test_that("makeAutocorr works", {
    makeAutocorr <- demest:::makeAutocorr
    mcmc.list <- coda::mcmc.list
    mcmc <- coda::mcmc
    autocorr <- coda::autocorr
    ## more than one variable
    m1 <- cbind(a = rnorm(20), b = rnorm(20))
    m2 <- cbind(a = rnorm(20), b = rnorm(20))
    l <- mcmc.list(mcmc(m1), mcmc(m2))
    ans.obtained <- makeAutocorr(l)
    corr <- autocorr(l, lags = 1, relative = FALSE)
    corr <- lapply(corr, drop)
    corr <- lapply(corr, diag)
    corr <- unlist(corr)
    ans.expected <- mean(abs(corr))
    expect_identical(ans.obtained, ans.expected)
    ## single variable
    m1 <- cbind(a = rnorm(20))
    m2 <- cbind(a = rnorm(20))
    l <- mcmc.list(mcmc(m1), mcmc(m2))
    ans.obtained <- makeAutocorr(l)
    corr <- autocorr(l, lags = 1, relative = FALSE)
    corr <- unlist(corr)
    ans.expected <- mean(abs(corr))
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeGelmanDiag works with BinomialVarying", {
    makeGelmanDiag <- demest:::makeGelmanDiag
    fetchResultsObject <- demest:::fetchResultsObject
    foldMCMCList <- demest:::foldMCMCList
    exposure <- Counts(array(as.integer(rpois(n = 24, lambda = 10)),
                             dim = 2:4,
                             dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rbinom(n = 24, size = exposure, prob = 0.8)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(time = "Intervals"))
    filename <- tempfile()
    estimateModel(Model(y ~ Binomial(mean ~ age + sex)),
                  y = y,
                  exposure = exposure,
                  nBurnin = 0,
                  nSim = 4,
                  nThin = 1,
                  nChain = 2,
                  filename = filename)
    object <- fetchResultsObject(filename)
    set.seed(1)
    ans.obtained <- makeGelmanDiag(object, filename = filename, nSample = 10)
    set.seed(1)
    mcmc.all <- fetchMCMC(filename, nSample = 10)
    ans.expected <- data.frame(med = numeric(2), max = numeric(2), n = integer(2))
    for (i in seq_along(mcmc.all)) {
        tmp <- foldMCMCList(mcmc.all[[i]])
        tmp <- coda::gelman.diag(tmp, autoburnin = FALSE, multivariate = FALSE)
        ans.expected[i,] <- c(median(tmp$psrf[, "Point est."], na.rm = TRUE),
                              max(tmp$psrf[, "Point est."]),
                              length(tmp$psrf[, "Point est."]))
    }
    ans.expected$max[ans.expected$n == 1] <- NA
    ans.expected$max[is.na(ans.expected$max) & ans.expected$n > 1] <- Inf
    rownames(ans.expected) <- names(mcmc.all)
    colnames(ans.expected) <- c("med", "max", "n")
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeMetropolis works with BinomialVarying", {
    makeMetropolis <- demest:::makeMetropolis
    fetchResultsObject <- demest:::fetchResultsObject
    makeAutocorr <- demest:::makeAutocorr
    exposure <- Counts(array(as.integer(rpois(n = 24, lambda = 10)),
                             dim = 2:4,
                             dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rbinom(n = 24, size = exposure, prob = 0.8)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(time = "Intervals"))
    filename <- tempfile()
    estimateModel(Model(y ~ Binomial(mean ~ age + sex)),
                  y = y,
                  exposure = exposure,
                  nBurnin = 0,
                  nSim = 2,
                  nThin = 1,
                  nChain = 2,
                  filename = filename)
    object <- fetchResultsObject(filename)
    set.seed(1)
    ans.obtained <- makeMetropolis(object, filename = filename, nSample = 25)
    set.seed(1)
    ans.expected <- data.frame(jump = fetch(filename, c("model", "likelihood", "jumpProb")),
                               acceptance = mean(fetch(filename, c("model", "likelihood", "acceptProb"))),
                               autocorr = makeAutocorr(fetchMCMC(filename, c("model", "likelihood", "prob"),
                                                                 nSample = 25)))
    rownames(ans.expected) <- "model.likelihood.prob"
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeMetropolis works with ResultsCounts", {
    makeMetropolis <- demest:::makeMetropolis
    MCMCDemographic <- demest:::MCMCDemographic
    fetchResultsObject <- demest:::fetchResultsObject
    makeAutocorr <- demest:::makeAutocorr
    lambda <- exp(outer(outer(rnorm(n = 10, mean = seq(from = -1, to = 3, length = 10)),
                              rnorm(2), "+"), rnorm(5), "+"))
    y <- Counts(array(as.integer(rpois(n = length(lambda), lambda = lambda)),
                      dim = c(10, 2, 5),
                      dimnames = list(age = 0:9, sex = c("f", "m"), region = 1:5)))
    d1 <- Counts(array(as.integer(rbinom(n = length(y), size = y, prob = 0.7)),
                       dim = dim(y),
                       dimnames = dimnames(y)))
    d2 <- Counts(array(as.integer(rpois(n = length(y)/ 2, lambda = collapseDimension(y, dim = "sex"))),
                       dim = c(10, 5),
                       dimnames = list(age = 0:9, region = 1:5)))
    d3 <- collapseDimension(y, dim = "region")
    filename <- tempfile()
    estimateCounts(model = Model(y ~ Poisson(mean ~ age + sex + region, useExpose = FALSE),
                       jump = 0.3,
                       age ~ Exch()),
                   y = y,
                   dataModels = list(Model(d1 ~ Binomial(mean ~ 1), jump = 0.03),
                       Model(d2 ~ Poisson(mean ~ region), jump = 0.2, lower = 0.3),
                       Model(d3 ~ PoissonBinomial(prob = 0.95))),
                   datasets = list(d1 = d1, d2 = d2, d3 = d3),
                   filename = filename,
                   nBurnin = 5,
                   nSim = 5,
                   nChain = 2)
    object <- fetchResultsObject(filename)
    ## metropolis
    set.seed(1)
    ans.obtained <- makeMetropolis(object, filename = filename, nSample = 25)
    set.seed(1)
    ans.expected <- data.frame(jump = c(fetch(filename, c("model", "likelihood", "jumpCount")),
                                   fetch(filename, c("dataModels", "d1", "likelihood", "jumpProb")),
                                   fetch(filename, c("dataModels", "d2", "likelihood", "jumpRate"))),
                               acceptance = c(mean(fetch(filename, c("model", "likelihood", "acceptCount"))),
                                   mean(fetch(filename, c("dataModels", "d1", "likelihood", "acceptProb"))),
                                   mean(fetch(filename, c("dataModels", "d2", "likelihood", "acceptRate")))),
                               autocorr = c(makeAutocorr(fetchMCMC(filename, c("model", "likelihood", "count"),
                                                                   nSample = 25)),
                                            makeAutocorr(fetchMCMC(filename, c("dataModels", "d1", "likelihood", "prob"),
                                                                   nSample = 25)),
                                            makeAutocorr(fetchMCMC(filename, c("dataModels", "d2", "likelihood", "rate"),
                                                                   nSample = 25))))
    rownames(ans.expected) <- c("model.likelihood.count", "dataModels.d1.likelihood.prob",
                                "dataModels.d2.likelihood.rate")
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeParameters works with BinomialVarying", {
    makeParameters <- demest:::makeParameters
    whereMetropStat <- demest:::whereMetropStat
    whereEstimated <- demest:::whereEstimated
    fetchResultsObject <- demest:::fetchResultsObject
    exposure <- Counts(array(as.integer(rpois(n = 24, lambda = 10)),
                             dim = 2:4,
                             dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                       dimscales = c(time = "Intervals"))
    y <- Counts(array(as.integer(rbinom(n = 24, size = exposure, prob = 0.8)),
                      dim = 2:4,
                      dimnames = list(sex = c("f", "m"), age = 0:2, time = 2000:2003)),
                dimscales = c(time = "Intervals"))
    filename <- tempfile()
    estimateModel(Model(y ~ Binomial(mean ~ age + sex)),
                  y = y,
                  exposure = exposure,
                  nBurnin = 0,
                  nSim = 10,
                  nThin = 1,
                  nChain = 2,
                  filename = filename)
    object <- fetchResultsObject(filename)
    ans.obtained <- makeParameters(object, filename = filename)
    where <- whereMetropStat(object, whereEstimated)
    ans <- vector(mode = "list", length = length(where))
    for (i in seq_along(ans))
        ans[[i]] <- c(quantile(collapseIterations(fetch(filename, where = where[[i]]), FUN = median),
                               c(0, 0.5, 1)),
                      length(fetch(filename, where = where[[i]])) %/% 20L)
    ans.expected <- data.frame(do.call(rbind, ans))
    colnames(ans.expected) <- c("min", "med", "max", "N")
    rownames(ans.expected) <- sapply(where, paste, collapse = ".")
    ans.expected$min[ans.expected$N == 1] <- NA
    ans.expected$max[ans.expected$N == 1] <- NA
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeMCMCBetas works", {
    makeMCMCBetas <- demest:::makeMCMCBetas
    priors <- list(new("ExchFixed"), new("Zero"), 
                   new("ExchNormCov"))
    names <- c("(Intercept)", "a", "b")
    ans.obtained <- makeMCMCBetas(priors = priors, names = names)
    ans.expected <- list()
    expect_identical(ans.obtained, ans.expected)
    priors <- list(new("ExchFixed"))
    names <- "(Intercept)"
    ans.obtained <- makeMCMCBetas(priors = priors, names = names)
    ans.expected <- list()
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeMCMCPriorsBetas works", {
    makeMCMCPriorsBetas <- demest:::makeMCMCPriorsBetas
    priors <- list(new("ExchFixed"), new("ExchNormZero"), 
                   new("ExchNormCov"))
    names <- c("(Intercept)", "a", "b")
    ans.obtained <- makeMCMCPriorsBetas(priors = priors, names = names)
    ans.expected <- list(c("hyper", "a", "scaleError"),
                         c("hyper", "b", "coef"),
                         c("hyper", "b", "scaleError"))
    expect_identical(ans.obtained, ans.expected)
    priors <- list(new("ExchFixed"))
    names <- "(Intercept)"
    ans.obtained <- makeMCMCPriorsBetas(priors = priors, names = names)
    ans.expected <- NULL
    expect_identical(ans.obtained, ans.expected)
})

test_that("raiseMultipleChoicesError works", {
    raiseMultipleChoicesError <- demest:::raiseMultipleChoicesError
    expect_error(raiseMultipleChoicesError(choices = "x1"),
                 sprintf("'where' stops before end of hierarchy : remaining choice is %s",
                         sQuote("x1")))
    expect_error(raiseMultipleChoicesError(choices = c("x1", "x2")),
                 sprintf("'where' stops before end of hierarchy : remaining choices are %s",
                         paste(sQuote(c("x1", "x2")), collapse = ", ")))
})

test_that("raiseMultipleMatchesError works", {
    raiseMultipleMatchesError <- demest:::raiseMultipleMatchesError
    expect_error(raiseMultipleMatchesError(target = "x", choices = c("x1", "x2")),
                 sprintf("'x' partially matches two or more of %s",
                         paste(sQuote(c("x1", "x2")), collapse = ", ")))
})

test_that("raiseNotFoundError works", {
    raiseNotFoundError <- demest:::raiseNotFoundError
    expect_error(raiseNotFoundError(target = "x", choices = "a"),
                 sprintf("'x' not found : only choice is %s",
                         sQuote("a")))
    expect_error(raiseNotFoundError(target = "x", choices = c("a", "b", "c")),
                 sprintf("'x' not found : choices are %s, %s, %s",
                         sQuote("a"), sQuote("b"), sQuote("c")))
})

test_that("raiseOvershotError works", {
    raiseOvershotError <- demest:::raiseOvershotError
    expect_error(raiseOvershotError(nameObject = "a", where = "x"),
                 sprintf("hierarchy only extends to 'a' : 'where' has additional term %s",
                         dQuote("x")))
    expect_error(raiseOvershotError(nameObject = "a", where = c("x", "y")),
                 sprintf("hierarchy only extends to 'a' : 'where' has additional terms %s, %s",
                         dQuote("x"), dQuote("y")))
})

test_that("seasonalNormalizingFactor works", {
    seasonalNormalizingFactor <- demest:::seasonalNormalizingFactor
    ## main effect
    nAlong <- 10L
    metadata <- new("MetaData",
                    nms = "time",
                    dimtypes = "time",
                    DimScales = list(new("Points", dimvalues = seq_len(nAlong))))
    nSeason <- 4L
    iAlong <- 1L
    nIteration <- 20L
    season <- array(dim = c(nSeason, nAlong + 1L, nIteration))
    for (i in seq_len(nIteration)) {
        initial <- rnorm(nSeason)
        initial <- initial - mean(initial)
        season[ , 1, i] <- initial
        for (j in seq_len(nAlong)) {
            season[1, j+1, i] <- season[nSeason, j, i] + rnorm(1, sd = 0.1)
            season[-1, j+1, i] <- season[-nSeason, j, i]
        }
    }
    ans.obtained <- seasonalNormalizingFactor(season = season,
                                             nSeason = nSeason,
                                             iAlong = iAlong,
                                             nIteration = nIteration,
                                             metadata = metadata)
    ans.expected <- colMeans(season)
    ans.expected <- ans.expected[-1,]
    ans.expected <- as.numeric(ans.expected)
    expect_equal(ans.obtained, ans.expected)
    ## interaction
    nAlong <- 5L
    metadata <- new("MetaData",
                    nms = c("region", "time"),
                    dimtypes = c("state", "time"),
                    DimScales = list(new("Categories", dimvalues = c("a", "b", "c", "d", "e")),
                        new("Points", dimvalues = seq_len(nAlong))))
    nSeason <- 2L
    iAlong <- 2L
    nIteration <- 10L
    season <- array(dim = c(nSeason, 5L, nAlong + 1L, nIteration))
    for (i in seq_len(nIteration)) {
        season[ , , 1, i] <- rnorm(nSeason * 5)
        for (j in seq_len(nAlong)) {
            season[1, , j+1, i] <- season[nSeason, , j, i] + rnorm(5, sd = 0.1)
            season[-1, , j+1, i] <- season[-nSeason,  ,j, i]
        }
    }
    ans.obtained <- seasonalNormalizingFactor(season = season,
                                              nSeason = nSeason,
                                              iAlong = iAlong,
                                              nIteration = nIteration,
                                              metadata = metadata)
    ans.expected <- colMeans(season)
    ans.expected <- ans.expected[,-1,]
    ans.expected <- as.numeric(ans.expected)
    expect_identical(ans.obtained, ans.expected)
})

test_that("flattenList works", {
    flattenList <- demest:::flattenList
    l <- list(a = 1, b = list(c = 1, d = 3, e = list(f = 2)))
    expect_identical(flattenList(l),
                     list(a = 1, c = 1, d = 3, f = 2))
    l <- list(a = 1, b = list(c = 1, a = 2))
    expect_identical(flattenList(l),
                     list(a = 1, c = 1, a = 2))
    l <- list(a = 1, b = 2)
    expect_identical(flattenList(l),
                     l)
    expect_identical(flattenList(list()),
                     list())
    expect_error(flattenList("wrong"),
                 "'object' has class \"character\"")
})

test_that("trimNULLsFromList works", {
    trimNULLsFromList <- demest:::trimNULLsFromList
    l <- list(1, NULL, list(1, 2, NULL), list(list(1, NULL)))
    expect_identical(trimNULLsFromList(l),
                     list(1, list(1, 2), list(list(1))))
    expect_identical(trimNULLsFromList(list(NULL)),
                     list())
    l <- list(1, NULL, list(1, 2, NULL), list(list(NULL)))
    expect_identical(trimNULLsFromList(l),
                     list(1, list(1, 2)))
    l <- list(1, NULL, list(1, 2, NULL), list(list(NULL), 1))
    expect_identical(trimNULLsFromList(l),
                     list(1, list(1, 2), list(1)))
    expect_error(trimNULLsFromList("wrong"),
                 "'object' has class \"character\"")
})






## test_that("fetchResults gives valid answer when object has class Skeleton", {
##     ## fetchResults <- demest:::fetchResults
##     data <- as.double(rep((1:20) * 100, each = 10) + 1:10)
##     filename <- tempfile()
##     con <- file(filename, open = "wb")
##     writeBin(data, con = con)
##     close(con)
##     ## element 1 from each row, no metadata
##     obj <- new("Skeleton",
##                first = 2L,
##                last = 2L,
##                metadata = NULL,
##                objClass = "Values")
##     ans.obtained <- fetchResults(obj,
##                                  filename = filename,
##                                  iterations = 1:20,
##                                  lengthIter = 10L)
##     ans.expected <- Values(array(as.double((1:20) * 100 + 2L),
##                           dim = 20,
##                           dimnames = list(iteration = 1:20)))
##     expect_identical(ans.obtained, ans.expected)
##     ## elements 3-4 from first 10 rows, with metadata
##     obj <- new("Skeleton",
##                first = 3L,
##                last = 4L,
##                metadata = new("MetaData",
##                nms = "sex",
##                dimtypes = "state",
##                DimScales = list(new("Sexes", dimvalues = c("f", "m")))),
##                objClass = "Values")
##     ans.obtained <- fetchResults(obj,
##                                  filename = filename,
##                                  iterations = 1:10,
##                                  lengthIter = 10L)
##     ans.expected <- Values(array(as.double(rep((1:10) * 100, each = 2) + 3:4),
##                                  dim = c(2, 10),
##                                  dimnames = list(sex = c("f", "m"), iteration = 1:10)))
##     expect_identical(ans.obtained, ans.expected)
##     ## elements 3-4 from all rows (via iterations = NULL), with metadata
##     obj <- new("Skeleton",
##                first = 3L,
##                last = 4L,
##                metadata = new("MetaData",
##                nms = "sex",
##                dimtypes = "state",
##                DimScales = list(new("Sexes", dimvalues = c("f", "m")))),
##                objClass = "Values")
##     ans.obtained <- fetchResults(obj,
##                                  filename = filename,
##                                  iterations = NULL,
##                                  lengthIter = 10L)
##     ans.expected <- Values(array(as.double(rep((1:20) * 100, each = 2) + 3:4),
##                                  dim = c(2, 20),
##                                  dimnames = list(sex = c("f", "m"), iteration = 1:10)))
##     expect_identical(ans.obtained, ans.expected)
##     ## elements 5:10 from rows 2, 20, with metadata
##     obj <- new("Skeleton",
##                first = 5L,
##                last = 10,
##                metadata = new("MetaData",
##                nms = c("region", "age"),
##                dimtypes = c("state", "age"),
##                DimScales = list(new("Categories", dimvalues = c("a", "b")),
##                new("Intervals", dimvalues = c(0, 5, 10, Inf)))),
##                objClass = "Counts")
##     ans.obtained <- fetchResults(obj,
##                                  filename = filename,
##                                  iterations = c(2L, 20L),
##                                  lengthIter = 10L)
##     ans.expected <- Values(array(as.double(rep(c(2, 20) * 100, each = 6) + 5:10),
##                                  dim = c(2, 3, 2),
##                                  dimnames = list(region = c("a", "b"),
##                                  age = c("0-4", "5-9", "10+"),
##                                  iteration = c(2, 20))))
##     expect_identical(ans.obtained, ans.expected)
## })

    

    
## DEMOGRAPHIC ACCOUNTS ###################################################


test_that("chooseICellComp works", {
    chooseICellComp <- demest:::chooseICellComp
    Description <- demest:::Description
    object <- Counts(array(1:12,
                           dim = c(3, 2, 2),
                           dimnames = list(time = c("2001-2010", "2011-2020", "2021-2030"),
                               age = c("0-9", "10+"),
                               triangle = c("Lower", "Upper"))))
    object <- new("EntriesMovements",
                  .Data = object@.Data,
                  metadata = object@metadata)
    description <- Description(object)
    x <- replicate(n = 10, chooseICellComp(description))
    expect_true(all(x %in% seq_along(object)))
})

test_that("R and C versions of chooseICellComp give same answer", {
    chooseICellComp <- demest:::chooseICellComp
    Description <- demest:::Description
    object <- Counts(array(1:12,
                           dim = c(3, 2, 2),
                           dimnames = list(time = c("2001-2010", "2011-2020", "2021-2030"),
                               age = c("0-9", "10+"),
                               triangle = c("Lower", "Upper"))))
    object <- new("EntriesMovements",
                  .Data = object@.Data,
                  metadata = object@metadata)
    description <- Description(object)
    for (seed in 1:10) {
        set.seed(seed)
        ans.R <- chooseICellComp(description, useC = FALSE)
        set.seed(seed)
        ans.C <- chooseICellComp(description, useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
})

test_that("chooseICellOutInPool works", {
    chooseICellOutInPool <- demest:::chooseICellOutInPool
    Description <- demest:::Description
    for (seed in seq_len(n.test)) {
        object <- Counts(array(1L,
                               dim = c(3, 2, 5, 2, 2),
                               dimnames = list(time = c("2001-2010", "2011-2020", "2021-2030"),
                                   age = c("0-9", "10+"),
                                   region = 1:5,
                                   direction = c("Out", "In"),
                                   triangle = c("Lower", "Upper"))))
        object <- new("InternalMovementsPool",
                      .Data = object@.Data,
                      iDirection = 4L,
                      iBetween = 3L,
                      metadata = object@metadata)
        description <- Description(object)
        ans <- chooseICellOutInPool(description)
        indices <- arrayInd(ans, .dim = dim(object))
        expect_true(all(indices[1, -(3:4)] == indices[2, -(3:4)]))
        expect_true(indices[1, 3] != indices[2, 3])
        expect_identical(indices[, 4], 1:2)
        object <- Counts(array(1L,
                               dim = c(2, 3, 3, 4, 1),
                               dimnames = list(direction = c("Out", "In"),
                                   time = c("2001-2010", "2011-2020", "2021-2030"),
                                   region = 1:3,
                                   eth = 1:4,
                                   sex = "f")))
        object <- new("InternalMovementsPool",
                      .Data = object@.Data,
                      iDirection = 1L,
                      iBetween = 3:4,
                      metadata = object@metadata)
        description <- Description(object)
        ans <- chooseICellOutInPool(description)
        indices <- arrayInd(ans, .dim = dim(object))
        expect_true(all(indices[1, -c(1, 3, 4)] == indices[2, -c(1, 3, 4)]))
        expect_true(all(indices[1, 3:4] != indices[2, 3:4]))
        expect_identical(indices[, 1], 1:2)
    }
})

test_that("R and C versions of chooseICellOutInPool give same answer", {
    chooseICellOutInPool <- demest:::chooseICellOutInPool
    Description <- demest:::Description
    for (seed in seq_len(n.test)) {
        object <- Counts(array(1L,
                               dim = c(3, 2, 5, 2, 2),
                               dimnames = list(time = c("2001-2010", "2011-2020", "2021-2030"),
                                   age = c("0-9", "10+"),
                                   region = 1:5,
                                   direction = c("Out", "In"),
                                   triangle = c("Lower", "Upper"))))
        object <- new("InternalMovementsPool",
                      .Data = object@.Data,
                      iDirection = 4L,
                      iBetween = 3L,
                      metadata = object@metadata)
        description <- Description(object)
        set.seed(seed)
        ans.R <- chooseICellOutInPool(description, useC = FALSE)
        set.seed(seed)
        ans.C <- chooseICellOutInPool(description, useC = TRUE)
        expect_identical(ans.R, ans.C)
        object <- Counts(array(1L,
                               dim = c(2, 3, 3, 4, 1),
                               dimnames = list(direction = c("Out", "In"),
                                   time = c("2001-2010", "2011-2020", "2021-2030"),
                                   region = 1:3,
                                   eth = 1:4,
                                   sex = "f")))
        object <- new("InternalMovementsPool",
                      .Data = object@.Data,
                      iDirection = 1L,
                      iBetween = 3:4,
                      metadata = object@metadata)
        description <- Description(object)
        set.seed(seed)
        ans.R <- chooseICellOutInPool(description, useC = FALSE)
        set.seed(seed)
        ans.C <- chooseICellOutInPool(description, useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
})

test_that("chooseICellPopn works", {
    chooseICellPopn <- demest:::chooseICellPopn
    Description <- demest:::Description
    Population <- dembase:::Population
    ## time is first dimension of two
    population <- Counts(array(1:6,
                               dim = c(3, 2),
                               dimnames = list(time = c(2000, 2010, 2020),
                                   age = c("0-9", "10+"))))
    population <- Population(population)
    description <- Description(population)
    x <- replicate(n = 10, chooseICellPopn(description))
    expect_true(all(x %in% c(1L, 4L)))
    ## time is second dimension of two
    population <- Counts(array(1:6,
                               dim = c(3, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                   time = c(2000, 2010))))
    population <- Population(population)
    description <- Description(population)
    x <- replicate(n = 10, chooseICellPopn(description))
    expect_true(all(x %in% 1:3))
    ## time is second dimension of three
    population <- Counts(array(1:12,
                               dim = c(3, 2, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                   time = c(2000, 2010),
                                   age = c("0-9", "10+"))))
    population <- Population(population)
    description <- Description(population)
    x <- replicate(n = 100, chooseICellPopn(description))
    expect_true(all(x %in% c(1:3, 7:9)))
    ## only has time dimension
    population <- Counts(array(1:12,
                               dim = 12,
                               dimnames = list(time = seq(from = 2000,
                                                   by = 5,
                                                   length = 12))))
    population <- Population(population)
    description <- Description(population)
    x <- replicate(n = 100, chooseICellPopn(description))
    expect_true(all(x == 1L))
})

test_that("R and C versions of chooseICellPopn give same answer", {
    chooseICellPopn <- demest:::chooseICellPopn
    Description <- demest:::Description
    Population <- dembase:::Population
    ## time is first dimension of two
    seed <- 1
    population <- Counts(array(1:6,
                               dim = c(3, 2),
                               dimnames = list(time = c(2000, 2010, 2020),
                                   age = c("0-9", "10+"))))
    population <- Population(population)
    description <- Description(population)
    set.seed(seed)
    ans.R <- chooseICellPopn(description, useC = FALSE)
    set.seed(seed)
    ans.C <- chooseICellPopn(description, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## time is second dimension of two
    seed <- 2
    population <- Counts(array(1:6,
                               dim = c(3, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                   time = c(2000, 2010))))
    population <- Population(population)
    description <- Description(population)
    set.seed(seed)
    ans.R <- chooseICellPopn(description, useC = FALSE)
    set.seed(seed)
    ans.C <- chooseICellPopn(description, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## time is second dimension of three
    seed <- 3
    population <- Counts(array(1:12,
                               dim = c(3, 2, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                   time = c(2000, 2010),
                                   age = c("0-9", "10+"))))
    population <- Population(population)
    description <- Description(population)
    set.seed(seed)
    ans.R <- chooseICellPopn(description, useC = FALSE)
    set.seed(seed)
    ans.C <- chooseICellPopn(description, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## only has time dimension
    seed <- 4
    population <- Counts(array(1:12,
                               dim = 12,
                               dimnames = list(time = seq(from = 2000,
                                                   by = 5,
                                                   length = 12))))
    population <- Population(population)
    description <- Description(population)
    set.seed(seed)
    ans.R <- chooseICellPopn(description, useC = FALSE)
    set.seed(seed)
    ans.C <- chooseICellPopn(description, useC = TRUE)
    expect_identical(ans.R, ans.C)
})

test_that("chooseICellSubAddNet works", {
    chooseICellSubAddNet <- demest:::chooseICellSubAddNet
    Description <- demest:::Description
    for (seed in seq_len(n.test)) {
        object <- Counts(array(0L,
                               dim = c(3, 2, 5, 2),
                               dimnames = list(time = c("2001-2010", "2011-2020", "2021-2030"),
                                   age = c("0-9", "10+"),
                                   region = 1:5,
                                   triangle = c("Lower", "Upper"))))
        object <- new("InternalMovementsNet",
                      .Data = object@.Data,
                      iBetween = 3L,
                      metadata = object@metadata)
        description <- Description(object)
        ans <- chooseICellSubAddNet(description)
        indices <- arrayInd(ans, .dim = dim(object))
        expect_true(all(indices[1, -3] == indices[2, -3]))
        expect_true(indices[1, 3] != indices[2, 3])
        object <- Counts(array(0L,
                               dim = c(3, 3, 4, 2),
                               dimnames = list(time = c("2001-2010", "2011-2020", "2021-2030"),
                                   region = 1:3,
                                   eth = 1:4,
                                   sex = c("f", "m"))))
        object <- new("InternalMovementsNet",
                      .Data = object@.Data,
                      iBetween = 2:3,
                      metadata = object@metadata)
        description <- Description(object)
        ans <- chooseICellSubAddNet(description)
        indices <- arrayInd(ans, .dim = dim(object))
        expect_true(all(indices[1, -c(2, 3)] == indices[2, -c(2, 3)]))
        expect_true(all(indices[1, 2:3] != indices[2, 2:3]))
    }
})

test_that("R and C versions of chooseICellSubAddNet give same answer", {
    chooseICellSubAddNet <- demest:::chooseICellSubAddNet
    Description <- demest:::Description
    for (seed in seq_len(n.test)) {
        object <- Counts(array(0L,
                               dim = c(3, 2, 5, 2),
                               dimnames = list(time = c("2001-2010", "2011-2020", "2021-2030"),
                                   age = c("0-9", "10+"),
                                   region = 1:5,
                                   triangle = c("Lower", "Upper"))))
        object <- new("InternalMovementsNet",
                      .Data = object@.Data,
                      iBetween = 3L,
                      metadata = object@metadata)
        description <- Description(object)
        set.seed(seed + 1)
        ans.R <- chooseICellSubAddNet(description, useC = FALSE)
        set.seed(seed + 1)
        ans.C <- chooseICellSubAddNet(description, useC = TRUE)
        expect_identical(ans.R, ans.C)
        object <- Counts(array(0L,
                               dim = c(3, 3, 4, 2),
                               dimnames = list(time = c("2001-2010", "2011-2020", "2021-2030"),
                                   region = 1:3,
                                   eth = 1:4,
                                   sex = c("f", "m"))))
        object <- new("InternalMovementsNet",
                      .Data = object@.Data,
                      iBetween = 2:3,
                      metadata = object@metadata)
        description <- Description(object)
        set.seed(seed + 1)
        ans.R <- chooseICellSubAddNet(description, useC = FALSE)
        set.seed(seed + 1)
        ans.C <- chooseICellSubAddNet(description, useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
})



test_that("isLowerTriangle works", {
    isLowerTriangle <- demest:::isLowerTriangle
    Description <- demest:::Description
    object <- Counts(array(1:12,
                           dim = c(3, 2, 2),
                           dimnames = list(time = c("2001-2010", "2011-2020", "2021-2030"),
                               age = c("0-9", "10+"),
                               triangle = c("Lower", "Upper"))))
    object <- new("EntriesMovements",
                  .Data = object@.Data,
                  metadata = object@metadata)
    description <- Description(object)
    for (i in 1:6)
        expect_true(isLowerTriangle(i = i, description = description))
    for (i in 7:12)
        expect_false(isLowerTriangle(i = i, description = description))
})

test_that("R and C versions of isLowerTriangle give same answer", {
    isLowerTriangle <- demest:::isLowerTriangle
    Description <- demest:::Description
    object <- Counts(array(1:12,
                           dim = c(3, 2, 2),
                           dimnames = list(time = c("2001-2010", "2011-2020", "2021-2030"),
                               age = c("0-9", "10+"),
                               triangle = c("Lower", "Upper"))))
    object <- new("EntriesMovements",
                  .Data = object@.Data,
                  metadata = object@metadata)
    description <- Description(object)
    for (i in 1:12)
        expect_identical(isLowerTriangle(i = i, description = description, useC = FALSE),
                         isLowerTriangle(i = i, description = description, useC = TRUE))
})
    
test_that("getIAccNextFromPopn works", {
    getIAccNextFromPopn <- demest:::getIAccNextFromPopn
    Description <- demest:::Description
    Population <- dembase:::Population
    ## time is first dimension of two
    population <- Counts(array(1:6,
                               dim = c(3, 2),
                               dimnames = list(time = c(2000, 2010, 2020),
                                   age = c("0-9", "10+"))))
    accession <- Counts(array(1:2,
                              dim = c(2, 1),
                               dimnames = list(time = c("2001-2010", "2011-2020"),
                                   age = "10")))
    population <- Population(population)
    description <- Description(population)
    ans.obtained <- getIAccNextFromPopn(description, i = 1L)
    ans.expected <- 1L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIAccNextFromPopn(description, i = 2L)
    ans.expected <- 2L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIAccNextFromPopn(description, i = 3L)
    ans.expected <- 0L
    ans.obtained <- getIAccNextFromPopn(description, i = 4L)
    ans.expected <- 0L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIAccNextFromPopn(description, i = 5L)
    ans.expected <- 0L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIAccNextFromPopn(description, i = 3L)
    ans.expected <- 0L
    expect_identical(ans.obtained, ans.expected)
    ## time is second dimension of two
    population <- Counts(array(1:9,
                               dim = c(3, 3),
                               dimnames = list(age = c("0-9", "10-19", "20+"),
                                   time = c(2000, 2010, 2020))))
    accession <- Counts(array(1:4,
                               dim = c(2, 2),
                               dimnames = list(reg = c("10", "20"),
                                   time = c("2001-2010", "2011-2020"))))
    population <- Population(population)
    description <- Description(population)
    ans.obtained <- getIAccNextFromPopn(description, i = 1L)
    ans.expected <- 1L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIAccNextFromPopn(description, i = 2L)
    ans.expected <- 2L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIAccNextFromPopn(description, i = 3L)
    ans.expected <- 0L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIAccNextFromPopn(description, i = 4L)
    ans.expected <- 3L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIAccNextFromPopn(description, i = 5L)
    ans.expected <- 4L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIAccNextFromPopn(description, i = 6L)
    ans.expected <- 0L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIAccNextFromPopn(description, i = 7L)
    ans.expected <- 0L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIAccNextFromPopn(description, i = 8L)
    ans.expected <- 0L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIAccNextFromPopn(description, i = 9L)
    ans.expected <- 0L
    expect_identical(ans.obtained, ans.expected)
    ## time is second dimension of three
    population <- Counts(array(1:18,
                               dim = c(3, 3, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                   time = c(2000, 2010, 2020),
                                   age = c("0-9", "10+"))))
    accession <- Counts(array(1:6,
                               dim = c(3, 2, 1),
                               dimnames = list(reg = c("a", "b", "c"),
                                   time = c("2001-2010", "2011-2020"),
                                   age = "10")))
    population <- Population(population)
    description <- Description(population)
    for (i in 1:6) {
        ans.obtained <- getIAccNextFromPopn(description, i = i)
        ans.expected <- i
        expect_identical(ans.obtained, ans.expected)
    }
    for (i in 7:18) {
        ans.obtained <- getIAccNextFromPopn(description, i = i)
        ans.expected <- 0L
        expect_identical(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of getIAccNextFromPopn give same answer", {
    getIAccNextFromPopn <- demest:::getIAccNextFromPopn
    Population <- dembase:::Population
    Description <- demest:::Description
    ## time is first dimension of two
    population <- Counts(array(1:6,
                               dim = c(3, 2),
                               dimnames = list(time = c(2000, 2010, 2020),
                                               age = c("0-9", "10+"))))
    population <- Population(population)
    description <- Description(population)
    for (i in 1:6) {
        ans.R <- getIAccNextFromPopn(description, i = i, useC = FALSE)
        ans.C <- getIAccNextFromPopn(description, i = i, useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
    ## time is second dimension of two
    population <- Counts(array(1:9,
                               dim = c(3, 3),
                               dimnames = list(age = c("0-9", "10-19", "20+"),
                                               time = c(2000, 2010, 2020))))
    population <- Population(population)
    description <- Description(population)
    for (i in 1:9) {
        ans.R <- getIAccNextFromPopn(description, i = i, useC = FALSE)
        ans.C <- getIAccNextFromPopn(description, i = i, useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
    ## time is second dimension of three
    population <- Counts(array(1:18,
                               dim = c(3, 3, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                               time = c(2000, 2010, 2020),
                                               age = c("0-9", "10+"))))
    population <- Population(population)
    description <- Description(population)
    for (i in 1:18) {
        ans.R <- getIAccNextFromPopn(description, i = i, useC = FALSE)
        ans.C <- getIAccNextFromPopn(description, i = i, useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
})

test_that("getIExpFirstFromPopn works - with age", {
    getIExpFirstFromPopn <- demest:::getIExpFirstFromPopn
    exposureWithTriangles <- dembase:::exposureWithTriangles
    Description <- demest:::Description
    Population <- dembase:::Population
    ## time is first dimension of two
    population <- Counts(array(1:6,
                               dim = c(3, 2),
                               dimnames = list(time = c(2000, 2010, 2020),
                                               age = c("0-9", "10+"))))
    exposure <- exposureWithTriangles(population)
    population <- Population(population)
    description <- Description(population)
    ans.obtained <- getIExpFirstFromPopn(description, i = 1L)
    ans.expected <- 5L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIExpFirstFromPopn(description, i = 4L)
    ans.expected <- 7L
    expect_identical(ans.obtained, ans.expected)
    ## time is second dimension of two
    population <- Counts(array(1:9,
                               dim = c(3, 3),
                               dimnames = list(age = c("0-9", "10-19", "20+"),
                                               time = c(2000, 2010, 2020))))
    exposure <- exposureWithTriangles(population)
    population <- Population(population)
    description <- Description(population)
    ans.obtained <- getIExpFirstFromPopn(description, i = 1L)
    ans.expected <- 7L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIExpFirstFromPopn(description, i = 2L)
    ans.expected <- 8L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIExpFirstFromPopn(description, i = 3L)
    ans.expected <- 9L
    expect_identical(ans.obtained, ans.expected)
    ## time is second dimension of three
    population <- Counts(array(1:18,
                               dim = c(3, 3, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                   time = c(2000, 2010, 2020),
                                   age = c("0-9", "10+"))))
    exposure <- exposureWithTriangles(population)
    population <- Population(population)
    description <- Description(population)
    ans.obtained <- getIExpFirstFromPopn(description, i = 1L)
    ans.expected <- 13L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIExpFirstFromPopn(description, i = 2L)
    ans.expected <- 14L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIExpFirstFromPopn(description, i = 3L)
    ans.expected <- 15L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIExpFirstFromPopn(description, i = 10L)
    ans.expected <- 19L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIExpFirstFromPopn(description, i = 11L)
    ans.expected <- 20L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIExpFirstFromPopn(description, i = 12L)
    ans.expected <- 21L
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of getIExpFirstFromPopn give same answer - with age", {
    getIExpFirstFromPopn <- demest:::getIExpFirstFromPopn
    exposureWithTriangles <- dembase:::exposureWithTriangles
    Description <- demest:::Description
    Population <- dembase:::Population
    ## time is first dimension of two
    population <- Counts(array(1:6,
                               dim = c(3, 2),
                               dimnames = list(time = c(2000, 2010, 2020),
                                               age = c("0-9", "10+"))))
    exposure <- exposureWithTriangles(population)
    population <- Population(population)
    description <- Description(population)
    ans.R <- getIExpFirstFromPopn(description, i = 1L, useC = FALSE)
    ans.C <- getIExpFirstFromPopn(description, i = 1L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ans.R <- getIExpFirstFromPopn(description, i = 4L, useC = FALSE)
    ans.C <- getIExpFirstFromPopn(description, i = 4L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## time is second dimension of two
    population <- Counts(array(1:9,
                               dim = c(3, 3),
                               dimnames = list(age = c("0-9", "10-19", "20+"),
                                               time = c(2000, 2010, 2020))))
    exposure <- exposureWithTriangles(population)
    population <- Population(population)
    description <- Description(population)
    ans.R <- getIExpFirstFromPopn(description, i = 1L, useC = FALSE)
    ans.C <- getIExpFirstFromPopn(description, i = 1L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ans.R <- getIExpFirstFromPopn(description, i = 2L, useC = FALSE)
    ans.C <- getIExpFirstFromPopn(description, i = 2L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ans.R <- getIExpFirstFromPopn(description, i = 3L, useC = FALSE)
    ans.C <- getIExpFirstFromPopn(description, i = 3L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## time is second dimension of three
    population <- Counts(array(1:18,
                               dim = c(3, 3, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                   time = c(2000, 2010, 2020),
                                   age = c("0-9", "10+"))))
    exposure <- exposureWithTriangles(population)
    population <- Population(population)
    description <- Description(population)
    ans.R <- getIExpFirstFromPopn(description, i = 1L, useC = FALSE)
    ans.C <- getIExpFirstFromPopn(description, i = 1L, useC = TRUE)
    expect_identical(ans.R, ans.C)    
    ans.R <- getIExpFirstFromPopn(description, i = 2L, useC = FALSE)
    ans.C <- getIExpFirstFromPopn(description, i = 2L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ans.R <- getIExpFirstFromPopn(description, i = 3L, useC = FALSE)
    ans.C <- getIExpFirstFromPopn(description, i = 3L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ans.R <- getIExpFirstFromPopn(description, i = 10L, useC = FALSE)
    ans.C <- getIExpFirstFromPopn(description, i = 10L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ans.R <- getIExpFirstFromPopn(description, i = 11L, useC = FALSE)
    ans.C <- getIExpFirstFromPopn(description, i = 11L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ans.R <- getIExpFirstFromPopn(description, i = 12L, useC = FALSE)
    ans.C <- getIExpFirstFromPopn(description, i = 12L, useC = TRUE)
    expect_identical(ans.R, ans.C)
})

test_that("getIExpFirstFromPopn works - no age", {
    getIExpFirstFromPopn <- demest:::getIExpFirstFromPopn
    exposureNoTriangles <- dembase:::exposureNoTriangles
    Description <- demest:::Description
    Population <- dembase:::Population
    ## time is first dimension of two
    population <- Counts(array(1:6,
                               dim = c(3, 2),
                               dimnames = list(time = c(2000, 2010, 2020),
                                               reg = c("A", "B"))))
    exposure <- exposureNoTriangles(population)
    population <- Population(population)
    description <- Description(population)
    ans.obtained <- getIExpFirstFromPopn(description, i = 1L)
    ans.expected <- 1L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIExpFirstFromPopn(description, i = 4L)
    ans.expected <- 3L
    expect_identical(ans.obtained, ans.expected)
    ## time is second dimension of two
    population <- Counts(array(1:9,
                               dim = c(3, 3),
                               dimnames = list(reg = c("A", "B", "C"),
                                               time = c(2000, 2010, 2020))))
    exposure <- exposureNoTriangles(population)
    population <- Population(population)
    description <- Description(population)
    ans.obtained <- getIExpFirstFromPopn(description, i = 1L)
    ans.expected <- 1L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIExpFirstFromPopn(description, i = 2L)
    ans.expected <- 2L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIExpFirstFromPopn(description, i = 3L)
    ans.expected <- 3L
    expect_identical(ans.obtained, ans.expected)
    ## time is second dimension of three
    population <- Counts(array(1:18,
                               dim = c(3, 3, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                   time = c(2000, 2010, 2020),
                                   sex = c("m", "f"))))
    exposure <- exposureNoTriangles(population)
    population <- Population(population)
    description <- Description(population)
    ans.obtained <- getIExpFirstFromPopn(description, i = 1L)
    ans.expected <- 1L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIExpFirstFromPopn(description, i = 2L)
    ans.expected <- 2L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIExpFirstFromPopn(description, i = 3L)
    ans.expected <- 3L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIExpFirstFromPopn(description, i = 10L)
    ans.expected <- 7L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIExpFirstFromPopn(description, i = 11L)
    ans.expected <- 8L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIExpFirstFromPopn(description, i = 12L)
    ans.expected <- 9L
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of getIExpFirstFromPopn give same answer - with age", {
    getIExpFirstFromPopn <- demest:::getIExpFirstFromPopn
    exposureNoTriangles <- dembase:::exposureNoTriangles
    Description <- demest:::Description
    Population <- dembase:::Population
    ## time is first dimension of two
    population <- Counts(array(1:6,
                               dim = c(3, 2),
                               dimnames = list(time = c(2000, 2010, 2020),
                                               reg = c("a", "b"))))
    exposure <- exposureNoTriangles(population)
    population <- Population(population)
    description <- Description(population)
    ans.R <- getIExpFirstFromPopn(description, i = 1L, useC = FALSE)
    ans.C <- getIExpFirstFromPopn(description, i = 1L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ans.R <- getIExpFirstFromPopn(description, i = 4L, useC = FALSE)
    ans.C <- getIExpFirstFromPopn(description, i = 4L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## time is second dimension of two
    population <- Counts(array(1:9,
                               dim = c(3, 3),
                               dimnames = list(reg = c("a", "b", "c"),
                                               time = c(2000, 2010, 2020))))
    exposure <- exposureNoTriangles(population)
    population <- Population(population)
    description <- Description(population)
    ans.R <- getIExpFirstFromPopn(description, i = 1L, useC = FALSE)
    ans.C <- getIExpFirstFromPopn(description, i = 1L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ans.R <- getIExpFirstFromPopn(description, i = 2L, useC = FALSE)
    ans.C <- getIExpFirstFromPopn(description, i = 2L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ans.R <- getIExpFirstFromPopn(description, i = 3L, useC = FALSE)
    ans.C <- getIExpFirstFromPopn(description, i = 3L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## time is second dimension of three
    population <- Counts(array(1:18,
                               dim = c(3, 3, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                   time = c(2000, 2010, 2020),
                                   sex = c("m", "f"))))
    exposure <- exposureNoTriangles(population)
    population <- Population(population)
    description <- Description(population)
    ans.R <- getIExpFirstFromPopn(description, i = 1L, useC = FALSE)
    ans.C <- getIExpFirstFromPopn(description, i = 1L, useC = TRUE)
    expect_identical(ans.R, ans.C)    
    ans.R <- getIExpFirstFromPopn(description, i = 2L, useC = FALSE)
    ans.C <- getIExpFirstFromPopn(description, i = 2L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ans.R <- getIExpFirstFromPopn(description, i = 3L, useC = FALSE)
    ans.C <- getIExpFirstFromPopn(description, i = 3L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ans.R <- getIExpFirstFromPopn(description, i = 10L, useC = FALSE)
    ans.C <- getIExpFirstFromPopn(description, i = 10L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ans.R <- getIExpFirstFromPopn(description, i = 11L, useC = FALSE)
    ans.C <- getIExpFirstFromPopn(description, i = 11L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ans.R <- getIExpFirstFromPopn(description, i = 12L, useC = FALSE)
    ans.C <- getIExpFirstFromPopn(description, i = 12L, useC = TRUE)
    expect_identical(ans.R, ans.C)
})

test_that("getIPopnNextFromPopn works", {
    getIPopnNextFromPopn <- demest:::getIPopnNextFromPopn
    Description <- demest:::Description
    Population <- dembase:::Population
    ## time is first dimension of two
    population <- Counts(array(1:6,
                               dim = c(3, 2),
                               dimnames = list(time = c(2000, 2010, 2020),
                                   age = c("0-9", "10+"))))
    population <- Population(population)
    description <- Description(population)
    ans.obtained <- getIPopnNextFromPopn(description, i = 1L)
    ans.expected <- 5L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIPopnNextFromPopn(description, i = 5L)
    ans.expected <- 6L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIPopnNextFromPopn(description, i = 3L)
    ans.expected <- 0L
    expect_identical(ans.obtained, ans.expected)
    ## time is second dimension of two
    population <- Counts(array(1:6,
                               dim = c(3, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                   time = c(2000, 2010))))
    population <- Population(population)
    description <- Description(population)
    ans.obtained <- getIPopnNextFromPopn(description, i = 1L)
    ans.expected <- 4L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIPopnNextFromPopn(description, i = 5L)
    ans.expected <- 0L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIPopnNextFromPopn(description, i = 3L)
    ans.expected <- 6L
    expect_identical(ans.obtained, ans.expected)
    ## time is second dimension of three
    population <- Counts(array(1:12,
                               dim = c(3, 2, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                   time = c(2000, 2010),
                                   age = c("0-9", "10+"))))
    population <- Population(population)
    description <- Description(population)
    ans.obtained <- getIPopnNextFromPopn(description, i = 1L)
    ans.expected <- 10L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIPopnNextFromPopn(description, i = 5L)
    ans.expected <- 0L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getIPopnNextFromPopn(description, i = 3L)
    ans.expected <- 12L
    ans.obtained <- getIPopnNextFromPopn(description, i = 8L)
    ans.expected <- 11L
    expect_identical(ans.obtained, ans.expected)
    ## only has time dimension
    population <- Counts(array(1:12,
                               dim = 12,
                               dimnames = list(time = seq(from = 2000,
                                                   by = 5,
                                                   length = 12))))
    population <- Population(population)
    description <- Description(population)
    ans.obtained <- getIPopnNextFromPopn(description, i = 3L)
    ans.expected <- 4L
    ans.obtained <- getIPopnNextFromPopn(description, i = 12L)
    ans.expected <- 0L
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of getIPopnNextFromPopn give same answer", {
    getIPopnNextFromPopn <- demest:::getIPopnNextFromPopn
    Population <- dembase:::Population
    Description <- demest:::Description
    ## time is first dimension of two
    population <- Counts(array(1:6,
                               dim = c(3, 2),
                               dimnames = list(time = c(2000, 2010, 2020),
                                   age = c("0-9", "10+"))))
    population <- Population(population)
    description <- Description(population)
    ans.R <- getIPopnNextFromPopn(description, i = 1L, useC = FALSE)
    ans.C <- getIPopnNextFromPopn(description, i = 1L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ans.R <- getIPopnNextFromPopn(description, i = 5L, useC = FALSE)
    ans.C <- getIPopnNextFromPopn(description, i = 5L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ans.R <- getIPopnNextFromPopn(description, i = 3L, useC = FALSE)
    ans.C <- getIPopnNextFromPopn(description, i = 3L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## time is second dimension of two
    population <- Counts(array(1:6,
                               dim = c(3, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                   time = c(2000, 2010))))
    population <- Population(population)
    description <- Description(population)
    ans.R <- getIPopnNextFromPopn(description, i = 1L, useC = FALSE)
    ans.C <- getIPopnNextFromPopn(description, i = 1L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ans.R <- getIPopnNextFromPopn(description, i = 5L, useC = FALSE)
    ans.C <- getIPopnNextFromPopn(description, i = 5L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ans.R <- getIPopnNextFromPopn(description, i = 3L, useC = FALSE)
    ans.C <- getIPopnNextFromPopn(description, i = 3L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## time is second dimension of three
    population <- Counts(array(1:12,
                               dim = c(3, 2, 2),
                               dimnames = list(reg = c("a", "b", "c"),
                                   time = c(2000, 2010),
                                   age = c("0-9", "10+"))))
    population <- Population(population)
    description <- Description(population)
    ans.R <- getIPopnNextFromPopn(description, i = 1L, useC = FALSE)
    ans.C <- getIPopnNextFromPopn(description, i = 1L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ans.R <- getIPopnNextFromPopn(description, i = 5L, useC = FALSE)
    ans.C <- getIPopnNextFromPopn(description, i = 5L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ans.R <- getIPopnNextFromPopn(description, i = 3L, useC = FALSE)
    ans.C <- getIPopnNextFromPopn(description, i = 3L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ans.R <- getIPopnNextFromPopn(description, i = 8L, useC = FALSE)
    ans.C <- getIPopnNextFromPopn(description, i = 8L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ## only has time dimension
    population <- Counts(array(1:12,
                               dim = 12,
                               dimnames = list(time = seq(from = 2000,
                                                   by = 5,
                                                   length = 12))))
    population <- Population(population)
    description <- Description(population)
    ans.R <- getIPopnNextFromPopn(description, i = 3L, useC = FALSE)
    ans.C <- getIPopnNextFromPopn(description, i = 3L, useC = TRUE)
    expect_identical(ans.R, ans.C)
    ans.R <- getIPopnNextFromPopn(description, i = 12L, useC = FALSE)
    ans.C <- getIPopnNextFromPopn(description, i = 12L, useC = TRUE)
    expect_identical(ans.R, ans.C)
})

test_that("getMinValCohortAccession gives valid answer", {
    getMinValCohortAccession <- demest:::getMinValCohortAccession
    CohortIterator <- demest:::CohortIterator
    Accession <- dembase:::Accession
    accession <- Counts(array(12:1,
                           dim = c(4, 3),
                               dimnames = list(age = c("5", "10", "15", "20"),
                                 time = c(2000, 2005, 2010))))
    accession <- Accession(accession)
    iter <- CohortIterator(accession)
    ans.obtained <- getMinValCohortAccession(i = 2L, series = accession, iter = iter)
    ans.expected <- 1L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getMinValCohortAccession(i = 3L, series = accession, iter = iter)
    ans.expected <- 5L
    expect_identical(ans.obtained, ans.expected)
    accession <- Counts(array(12:1,
                              dim = c(3, 4),
                               dimnames = list(time = c(2000, 2005, 2010),
                                   age = c("5", "10", "15", "20"))))
    accession <- Accession(accession)
    iter <- CohortIterator(accession)
    ans.obtained <- getMinValCohortAccession(i = 5L, series = accession, iter = iter)
    ans.expected <- 4L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getMinValCohortAccession(i = 12L, series = accession, iter = iter)
    ans.expected <- 1L
    expect_identical(ans.obtained, ans.expected)
    accession <- Counts(array(60:1,
                               dim = 5:3,
                               dimnames = list(region = 1:5,
                                   time = c(2001, 2006, 2011, 2016),
                                   age = c("5", "10", "15"))))
    accession <- Accession(accession)
    iter <- CohortIterator(accession)
    ans.obtained <- getMinValCohortAccession(i = 7L, series = accession, iter = iter)
    ans.expected <- 4L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getMinValCohortAccession(i = 2L, series = accession, iter = iter)
    ans.expected <- 9L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getMinValCohortAccession(i = 45L, series = accession, iter = iter)
    ans.expected <- 16L
    expect_identical(ans.obtained, ans.expected)
})

test_that("R and C versions of getMinValCohortAccession give same answer", {
    getMinValCohortAccession <- demest:::getMinValCohortAccession
    CohortIterator <- demest:::CohortIterator
    Accession <- dembase:::Accession
    accession <- Counts(array(12:1,
                           dim = c(4, 3),
                               dimnames = list(age = c("5", "10", "15", "20"),
                                 time = c(2000, 2005, 2010))))
    accession <- Accession(accession)
    iter <- CohortIterator(accession)
    for (i in 1:12) {
        ans.R <- getMinValCohortAccession(i = i, series = accession, iter = iter, useC = FALSE)
        ans.C <- getMinValCohortAccession(i = i, series = accession, iter = iter, useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
    accession <- Counts(array(12:1,
                              dim = c(3, 4),
                               dimnames = list(time = c(2000, 2005, 2010),
                                   age = c("5", "10", "15", "20"))))
    accession <- Accession(accession)
    iter <- CohortIterator(accession)
    for (i in 1:12) {
        ans.R <- getMinValCohortAccession(i = i, series = accession, iter = iter, useC = FALSE)
        ans.C <- getMinValCohortAccession(i = i, series = accession, iter = iter, useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
    accession <- Counts(array(60:1,
                               dim = 5:3,
                               dimnames = list(region = 1:5,
                                   time = c(2001, 2006, 2011, 2016),
                                   age = c("5", "10", "15"))))
    accession <- Accession(accession)
    iter <- CohortIterator(accession)
    for (i in 1:60) {
        ans.R <- getMinValCohortAccession(i = i, series = accession, iter = iter, useC = FALSE)
        ans.C <- getMinValCohortAccession(i = i, series = accession, iter = iter, useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
})

test_that("getMinValCohortPopulation gives valid answer", {
    getMinValCohortPopulation <- demest:::getMinValCohortPopulation
    CohortIterator <- demest:::CohortIterator
    Population <- dembase:::Population
    population <- Counts(array(1:12,
                           dim = c(4, 3),
                               dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                 time = c(2000, 2005, 2010))))
    population <- Population(population)
    iter <- CohortIterator(population)
    ans.obtained <- getMinValCohortPopulation(i = 2L, series = population, iter = iter)
    ans.expected <- 2L
    expect_identical(ans.obtained, ans.expected)
    population <- Counts(array(12:1,
                               dim = c(4, 3),
                               dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                   time = c(2000, 2005, 2010))))
    population <- Population(population)
    iter <- CohortIterator(population)
    ans.obtained <- getMinValCohortPopulation(i = 5L, series = population, iter = iter)
    ans.expected <- 3L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getMinValCohortPopulation(i = 4L, series = population, iter = iter)
    ans.expected <- 1L
    expect_identical(ans.obtained, ans.expected)
    population <- Counts(array(12:1,
                               dim = c(4, 3),
                               dimnames = list(region = 1:4, time = c(2000, 2005, 2010))))
    population <- Population(population)
    iter <- CohortIterator(population)
    ans.obtained <- getMinValCohortPopulation(i = 1L, series = population, iter = iter)
    ans.expected <- 4L
    ans.obtained <- getMinValCohortPopulation(i = 7L, series = population, iter = iter)
    ans.expected <- 2L
    population <- Counts(array(60:1,
                               dim = 5:3,
                               dimnames = list(region = 1:5,
                                   time = c(2001, 2006, 2011, 2016),
                                   age = c("0-4", "5-9", "10+"))))
    population <- Population(population)
    iter <- CohortIterator(population)
    ans.obtained <- getMinValCohortPopulation(i = 7L, series = population, iter = iter)
    ans.expected <- 4L
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- getMinValCohortPopulation(i = 45L, series = population, iter = iter)
    ans.expected <- 1L
    expect_identical(ans.obtained, ans.expected)
    population <- Counts(array(5:2,
                               dim = 4L,
                               dimnames = list(time = c(0, 10, 20, 30))))
    population <- Population(population)
    iter <- CohortIterator(population)
    ans.obtained <- getMinValCohortPopulation(i = 1L, series = population, iter = iter)
    ans.expected <- 2L
    expect_identical(ans.obtained, ans.expected)
})


test_that("R and C versions of getMinValCohortPopulation give same answer", {
    getMinValCohortPopulation <- demest:::getMinValCohortPopulation
    CohortIterator <- demest:::CohortIterator
    Population <- dembase:::Population
    population <- Counts(array(1:12,
                           dim = c(4, 3),
                               dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                 time = c(2000, 2005, 2010))))
    population <- Population(population)
    iter <- CohortIterator(population)
    for (i in 1:12) {
        ans.R <- getMinValCohortPopulation(i = i, series = population, iter = iter, useC = FALSE)
        ans.C <- getMinValCohortPopulation(i = i, series = population, iter = iter, useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
    population <- Counts(array(12:1,
                               dim = c(4, 3),
                               dimnames = list(age = c("0-4", "5-9", "10-14", "15+"),
                                   time = c(2000, 2005, 2010))))
    population <- Population(population)
    iter <- CohortIterator(population)
    for (i in 1:12) {
        ans.R <- getMinValCohortPopulation(i = i, series = population, iter = iter, useC = FALSE)
        ans.C <- getMinValCohortPopulation(i = i, series = population, iter = iter, useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
    population <- Counts(array(12:1,
                               dim = c(4, 3),
                               dimnames = list(region = 1:4, time = c(2000, 2005, 2010))))
    population <- Population(population)
    iter <- CohortIterator(population)
    for (i in 1:12) {
        ans.R <- getMinValCohortPopulation(i = i, series = population, iter = iter, useC = FALSE)
        ans.C <- getMinValCohortPopulation(i = i, series = population, iter = iter, useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
    population <- Counts(array(60:1,
                               dim = 5:3,
                               dimnames = list(region = 1:5,
                                   time = c(2001, 2006, 2011, 2016),
                                   age = c("0-4", "5-9", "10+"))))
    population <- Population(population)
    iter <- CohortIterator(population)
    for (i in 1:60) {
        ans.R <- getMinValCohortPopulation(i = i, series = population, iter = iter, useC = FALSE)
        ans.C <- getMinValCohortPopulation(i = i, series = population, iter = iter, useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
})

test_that("makeTransformExpToBirths works", {
    makeTransformExpToBirths <- demest:::makeTransformExpToBirths
    ## exposure has sex and age dimensions
    exposure <- Counts(array(1,
                             dim = c(5, 2, 2, 3, 2),
                             dimnames = list(region = 1:5,
                                             time = c("2002-2006", "2007-2011"),
                                             sex = c("f", "m"),
                                             age = c("0-4", "5-9", "10+"),
                                             triangle = c("Lower", "Upper"))))
    births <- Counts(array(1,
                           dim = c(5, 2, 2, 1, 2),
                           dimnames = list(region = 1:5,
                                           time = c("2002-2006", "2007-2011"),
                                           sex = c("f", "m"),
                                           age = "5-9",
                                           triangle = c("Lower", "Upper"))))
    ans.obtained <- makeTransformExpToBirths(exposure = exposure,
                                             births = births,
                                             dominant = "Female")
    ans.expected <- new("CollapseTransform",
                        dims = c(1L, 2L, 0L, 3L, 4L),
                        indices = list(1:5,
                                       1:2,
                                       c(1L, 0L),
                                       c(0L, 1L, 0L),
                                       1:2),
                        dimBefore = c(5L, 2L, 2L, 3L, 2L),
                        dimAfter = c(5L, 2L, 1L, 2L))
    expect_identical(ans.obtained, ans.expected)
    ## exposure has sex dimension but no age dimension; births has no sex dimension; male dominant
    exposure <- Counts(array(1,
                             dim = c(5, 2, 2),
                             dimnames = list(region = 1:5,
                                             time = c("2002-2006", "2007-2011"),
                                             sex = c("f", "m"))))
    births <- Counts(array(1,
                           dim = c(5, 2),
                           dimnames = list(region = 1:5,
                                           time = c("2002-2006", "2007-2011"))))
    ans.obtained <- makeTransformExpToBirths(exposure = exposure,
                                             births = births,
                                             dominant = "Male")
    ans.expected <- new("CollapseTransform",
                        dims = c(1L, 2L, 0L),
                        indices = list(1:5,
                                       1:2,
                                       c(0L, 1L)),
                        dimBefore = c(5L, 2L, 2L),
                        dimAfter = c(5L, 2L))
    expect_identical(ans.obtained, ans.expected)    
    ## exposure has age dimensions but not sex dimension
    exposure <- Counts(array(1,
                             dim = c(5, 2, 3, 2),
                             dimnames = list(region = 1:5,
                                             time = c("2002-2006", "2007-2011"),
                                             age = c("0-4", "5-9", "10+"),
                                             triangle = c("Lower", "Upper"))))
    births <- Counts(array(1,
                           dim = c(5, 2, 1, 2),
                           dimnames = list(region = 1:5,
                                           time = c("2002-2006", "2007-2011"),
                                           age = "5-9",
                                           triangle = c("Lower", "Upper"))))
    ans.obtained <- makeTransformExpToBirths(exposure = exposure,
                                             births = births,
                                             dominant = "Female")
    ans.expected <- new("CollapseTransform",
                        dims = c(1L, 2L, 3L, 4L),
                        indices = list(1:5,
                                       1:2,
                                       c(0L, 1L, 0L),
                                       1:2),
                        dimBefore = c(5L, 2L, 3L, 2L),
                        dimAfter = c(5L, 2L, 1L, 2L))
    expect_identical(ans.obtained, ans.expected)
    ## exposure has no age dimension, no sex dimension
    exposure <- Counts(array(1,
                             dim = c(5, 2),
                             dimnames = list(region = 1:5,
                                             time = c("2002-2006", "2007-2011"))))
    births <- Counts(array(1,
                           dim = c(5, 2),
                           dimnames = list(region = 1:5,
                                           time = c("2002-2006", "2007-2011"))))
    ans.obtained <- makeTransformExpToBirths(exposure = exposure,
                                             births = births,
                                             dominant = "Female")
    ans.expected <- new("CollapseTransform",
                        dims = c(1L, 2L),
                        indices = list(1:5,
                                       1:2),
                        dimBefore = c(5L, 2L),
                        dimAfter = c(5L, 2L))
    expect_identical(ans.obtained, ans.expected)
})


test_that("makeIteratorCAP creates objects from valid inputs - Accession", {
    makeIteratorCAP <- demest:::makeIteratorCAP
    ## dim = 3:4, iAge = 1L, iTime = 2L
    ans.obtained <- makeIteratorCAP(dim = 3:4, iTime = 1L, iAge = 2L, accession = TRUE)
    ans.expected <- new("CohortIteratorAccession",
                        i = 1L,
                        nTime = 3L,
                        stepTime = 1L,
                        iTime = 1L,
                        hasAge = TRUE,
                        nAge = 4L,
                        stepAge = 3L,
                        iAge = 1L,
                        finished = FALSE)
    expect_identical(ans.obtained, ans.expected)
    ## dim = 3:4, iAge = 2L, iTime = 1L 
    ans.obtained <- makeIteratorCAP(dim = 3:4, iTime = 1L, iAge = 2L, accession = TRUE)
    ans.expected <- new("CohortIteratorAccession",
                        i = 1L,
                        nTime = 3L,
                        stepTime = 1L,
                        iTime = 1L,
                        hasAge = TRUE,
                        nAge = 4L,
                        stepAge = 3L,
                        iAge = 1L,
                        finished = FALSE)
    expect_identical(ans.obtained, ans.expected)
    ## dim = 3:4, iAge = 0L, iTime = 2L 
    ans.obtained <- makeIteratorCAP(dim = 3:4, iTime = 2L, iAge = 0L, accession = TRUE)
    ans.expected <- new("CohortIteratorAccession",
                        i = 1L,
                        nTime = 4L,
                        stepTime = 3L,
                        iTime = 1L,
                        hasAge = FALSE,
                        nAge = as.integer(NA),
                        stepAge = as.integer(NA),
                        iAge = as.integer(NA),
                        finished = FALSE)
    expect_identical(ans.obtained, ans.expected)
    ## dim = 2:5, iTime = 4L, iAge = 3L
    ans.obtained <- makeIteratorCAP(dim = 2:5, iTime = 4L, iAge = 3L, accession = TRUE)
    ans.expected <- new("CohortIteratorAccession",
                        i = 1L,
                        nTime = 5L,
                        stepTime = 24L,
                        iTime = 1L,
                        hasAge = TRUE,
                        nAge = 4L,
                        stepAge = 6L,
                        iAge = 1L,
                        finished = FALSE)
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeIteratorCAP creates objects from valid inputs - Population", {
    makeIteratorCAP <- demest:::makeIteratorCAP
    ## dim = 3:4, iAge = 1L, iTime = 2L
    ans.obtained <- makeIteratorCAP(dim = 3:4, iTime = 1L, iAge = 2L, accession = FALSE)
    ans.expected <- new("CohortIteratorPopulation",
                        i = 1L,
                        nTime = 3L,
                        stepTime = 1L,
                        iTime = 1L,
                        hasAge = TRUE,
                        nAge = 4L,
                        stepAge = 3L,
                        iAge = 1L,
                        finished = FALSE)
    expect_identical(ans.obtained, ans.expected)
    ## dim = 3:4, iAge = 2L, iTime = 1L 
    ans.obtained <- makeIteratorCAP(dim = 3:4, iTime = 1L, iAge = 2L, accession = FALSE)
    ans.expected <- new("CohortIteratorPopulation",
                        i = 1L,
                        nTime = 3L,
                        stepTime = 1L,
                        iTime = 1L,
                        hasAge = TRUE,
                        nAge = 4L,
                        stepAge = 3L,
                        iAge = 1L,
                        finished = FALSE)
    expect_identical(ans.obtained, ans.expected)
    ## dim = 3:4, iAge = 0L, iTime = 2L 
    ans.obtained <- makeIteratorCAP(dim = 3:4, iTime = 2L, iAge = 0L, accession = FALSE)
    ans.expected <- new("CohortIteratorPopulation",
                        i = 1L,
                        nTime = 4L,
                        stepTime = 3L,
                        iTime = 1L,
                        hasAge = FALSE,
                        nAge = as.integer(NA),
                        stepAge = as.integer(NA),
                        iAge = as.integer(NA),
                        finished = FALSE)
    expect_identical(ans.obtained, ans.expected)
    ## dim = 2:5, iTime = 4L, iAge = 3L
    ans.obtained <- makeIteratorCAP(dim = 2:5, iTime = 4L, iAge = 3L, accession = FALSE)
    ans.expected <- new("CohortIteratorPopulation",
                        i = 1L,
                        nTime = 5L,
                        stepTime = 24L,
                        iTime = 1L,
                        hasAge = TRUE,
                        nAge = 4L,
                        stepAge = 6L,
                        iAge = 1L,
                        finished = FALSE)
    expect_identical(ans.obtained, ans.expected)
})


test_that("makeIteratorCC creates objects from valid inputs", {
    makeIteratorCC <- demest:::makeIteratorCC 
    ## dim = 3:4, iAge = 1L, iTime = 2L
    ans.obtained <- makeIteratorCC(dim = c(3:5, 2L), iTime = 1L, iAge = 2L, iTriangle = 4L,
                                   lastAgeGroupOpen = FALSE)
    ans.expected <- new("CohortIteratorComponent",
                        i = 1L,
                        nTime = 3L,
                        stepTime = 1L,
                        iTime = 1L,
                        hasAge = TRUE,
                        nAge = 4L,
                        stepAge = 3L,
                        iAge = 1L,
                        stepTriangle = 60L,
                        iTriangle = 1L,
                        finished = FALSE,
                        lastAgeGroupOpen = FALSE)
    expect_identical(ans.obtained, ans.expected)
    ## dim = 3:4, iAge = 2L, iTime = 1L 
    ans.obtained <- makeIteratorCC(dim = 2:4, iTime = 2L, iAge = 3L, iTriangle = 1L,
                                   lastAgeGroupOpen = TRUE)
    ans.expected <- new("CohortIteratorComponent",
                        i = 1L,
                        nTime = 3L,
                        stepTime = 2L,
                        iTime = 1L,
                        hasAge = TRUE,
                        nAge = 4L,
                        stepAge = 6L,
                        iAge = 1L,
                        stepTriangle = 1L,
                        iTriangle = 1L,
                        finished = FALSE,
                        lastAgeGroupOpen = TRUE)
    expect_identical(ans.obtained, ans.expected)
    ## dim = 3:4, iAge = 0L, iTime = 2L 
    ans.obtained <- makeIteratorCC(dim = 3:4, iTime = 2L, iAge = 0L, iTriangle = 0L,
                                   lastAgeGroupOpen = NA)
    ans.expected <- new("CohortIteratorComponent",
                        i = 1L,
                        nTime = 4L,
                        stepTime = 3L,
                        iTime = 1L,
                        hasAge = FALSE,
                        nAge = as.integer(NA),
                        stepAge = as.integer(NA),
                        iAge = as.integer(NA),
                        stepTriangle = as.integer(NA),
                        iTriangle = as.integer(NA),
                        finished = FALSE,
                        lastAgeGroupOpen = NA)
    expect_identical(ans.obtained, ans.expected)
    ## dim = 2:5, iTime = 4L, iAge = 3L, iTriangle = 1L
    ans.obtained <- makeIteratorCC(dim = 2:5, iTime = 4L, iAge = 3L, iTriangle = 1L,
                                   lastAgeGroupOpen = TRUE)
    ans.expected <- new("CohortIteratorComponent",
                        i = 1L,
                        nTime = 5L,
                        stepTime = 24L,
                        iTime = 1L,
                        hasAge = TRUE,
                        nAge = 4L,
                        stepAge = 6L,
                        iAge = 1L,
                        stepTriangle = 1L,
                        iTriangle = 1L,
                        finished = FALSE,
                        lastAgeGroupOpen = TRUE)
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeIteratorCODPCP creates objects from valid inputs", {
    makeIteratorCODPCP <- demest:::makeIteratorCODPCP 
    ans.obtained <- makeIteratorCODPCP(dim = c(3:5, 5L, 2L), iTime = 1L, iAge = 2L,
                                       iMultiple = 4L, iTriangle = 5L,
                                       lastAgeGroupOpen = TRUE)
    ans.expected <- new("CohortIteratorOrigDestParChPool",
                        i = 1L,
                        nTime = 3L,
                        stepTime = 1L,
                        iTime = 1L,
                        hasAge = TRUE,
                        nAge = 4L,
                        stepAge = 3L,
                        iAge = 1L,
                        stepTriangle = 300L,
                        iTriangle = 1L,
                        iVec = c(1L, 61L, 121L, 181L, 241L),
                        lengthVec = 5L,
                        increment = c(0L, 60L, 120L, 180L, 240L),
                        finished = FALSE,
                        lastAgeGroupOpen = TRUE)
    expect_identical(ans.obtained, ans.expected)
    ## dim = 3:4, iAge = 2L, iTime = 1L 
    ans.obtained <- makeIteratorCODPCP(dim = c(2:4, 3L, 3L), iTime = 2L, iAge = 3L, iTriangle = 1L,
                                       iMultiple = 4L,
                                       lastAgeGroupOpen = TRUE)
    ans.expected <- new("CohortIteratorOrigDestParChPool",
                        i = 1L,
                        nTime = 3L,
                        stepTime = 2L,
                        iTime = 1L,
                        hasAge = TRUE,
                        nAge = 4L,
                        stepAge = 6L,
                        iAge = 1L,
                        stepTriangle = 1L,
                        iTriangle = 1L,
                        iVec = c(1L, 25L, 49L),
                        lengthVec = 3L,
                        increment = c(0L, 24L, 48L),
                        lastAgeGroupOpen = TRUE,
                        finished = FALSE)
    expect_identical(ans.obtained, ans.expected)
    ## dim = 3:4, iAge = 0L, iTime = 2L 
    ans.obtained <- makeIteratorCODPCP(dim = c(3:4, 3L, 3L, 2L, 2L), iTime = 2L, iAge = 0L,
                                       iTriangle = 0L, iMult = c(4L, 6L),
                                       lastAgeGroupOpen = NA)
    ans.expected <- new("CohortIteratorOrigDestParChPool",
                        i = 1L,
                        nTime = 4L,
                        stepTime = 3L,
                        iTime = 1L,
                        hasAge = FALSE,
                        nAge = as.integer(NA),
                        stepAge = as.integer(NA),
                        iAge = as.integer(NA),
                        stepTriangle = as.integer(NA),
                        iTriangle = as.integer(NA),
                        iVec = c(1L, 37L, 73L, 217L, 253L, 289L),
                        lengthVec = 6L,
                        increment = c(0L, 36L, 72L, 216L, 252L, 288L),
                        lastAgeGroupOpen = NA,
                        finished = FALSE)
    expect_identical(ans.obtained, ans.expected)
    ## dim = 2:5, iTime = 4L, iAge = 3L, iTriangle = 1L, iMult = c(2L, 7L)
    ans.obtained <- makeIteratorCODPCP(dim = c(2:5, 3L, 2L, 2L), iTime = 4L, iAge = 3L,
                                       iTriangle = 1L, iMult = c(2L, 7L),
                                       lastAgeGroupOpen = FALSE)
    ans.expected <- new("CohortIteratorOrigDestParChPool",
                        i = 1L,
                        nTime = 5L,
                        stepTime = 24L,
                        iTime = 1L,
                        hasAge = TRUE,
                        nAge = 4L,
                        stepAge = 6L,
                        iAge = 1L,
                        stepTriangle = 1L,
                        iTriangle = 1L,
                        iVec = c(1L, 3L, 5L, 721L, 723L, 725L),
                        lengthVec = 6L,
                        increment = c(0L, 2L, 4L, 720L, 722L, 724L),
                        lastAgeGroupOpen = FALSE,
                        finished = FALSE)
    expect_identical(ans.obtained, ans.expected)
})



test_that("makeOutputAccount works", {
    makeOutputAccount <- demest:::makeOutputAccount
    Skeleton <- demest:::Skeleton
    initialModel <- demest:::initialModel
    population <- CountsOne(values = seq(100, 200, 10),
                            labels = seq(2000, 2100, 10),
                            name = "time")
    births <- CountsOne(values = rpois(n = 10, lambda = 15),
                        labels = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-"),
                        name = "time")
    deaths <- CountsOne(values = rpois(n = 10, lambda = 5),
                        labels = paste(seq(2001, 2091, 10), seq(2010, 2100, 10), sep = "-"),
                        name = "time")
    account <- Movements(population = population,
                         births = births,
                         exits = list(deaths = deaths))
    system.models <- list(initialModel(Model(population ~ Poisson(mean ~ 1, useExpose = FALSE)),
                                       y = toInteger(population),
                                       exposure = NULL),
                          initialModel(Model(births ~ Poisson(mean ~ 1)),
                                       y = toInteger(births),
                                       exposure = exposure(population)),
                          initialModel(Model(deaths ~ Poisson(mean ~ 1)),
                                       y = toInteger(deaths),
                                       exposure = exposure(population)))
    ans.obtained <- makeOutputAccount(account = account,
                                      systemModels = system.models,
                                      pos = 11L)
    ans.expected <- list(population = Skeleton(account@population, first = 11L),
                         births = Skeleton(account@components[[1]], first = 22L),
                         deaths = Skeleton(account@components[[2]], first = 32L))
    expect_identical(ans.obtained, ans.expected)    
})




## CMP ###############################################################

test_that("logDensCMPUnnormalised1 works", {
    logDensCMPUnnormalised1 <- demest:::logDensCMPUnnormalised1
    mydcmp<- function(y,gamma,nu){
        pdf <- nu*(y*log(gamma)-lgamma(y+1))
        return(pdf)
    }
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x <- rpois(n = 1, lambda = 10)
        gamma <- runif(n = 1, max = 10)
        nu <- runif(n = 1, max = 10)
        ans.obtained <- logDensCMPUnnormalised1(x = x, gamma = gamma, nu = nu)
        ans.expected <- mydcmp(y = x, gamma = gamma, nu = nu)
        if (test.identity)
            expect_identical(ans.obtained, ans.expected)
        else
            expect_equal(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of logDensCMPUnnormalised1 give same answer", {
    logDensCMPUnnormalised1 <- demest:::logDensCMPUnnormalised1
    mydcmp<- function(y,gamma,nu){
        pdf <- nu*(y*log(gamma)-lgamma(y+1))
        return(pdf)
    }
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x <- rpois(n = 1, lambda = 10)
        gamma <- runif(n = 1, max = 10)
        nu <- runif(n = 1, max = 10)
        ans.R <- logDensCMPUnnormalised1(x = x, gamma = gamma, nu = nu, useC = FALSE)
        ans.C <- logDensCMPUnnormalised1(x = x, gamma = gamma, nu = nu, useC = TRUE)
        ans.expected <- mydcmp(y = x, gamma = gamma, nu = nu)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("rcmpUnder works", {
    rcmpUnder <- demest:::rcmpUnder
    set.seed(0)
    mu <- runif(n = 1, max = 10000)
    nu <- runif(n = 1, min = 1, max = 10)
    max <- 100L
    y <- replicate(n = 10000, rcmpUnder(mu = mu, nu = nu, max = max))
    y_fin <- y[is.finite(y) == TRUE]
    expect_equal(mean(y_fin), mu + 1 / (2 * nu) - 0.5, tolerance = 0.02)
    expect_equal(var(y_fin), mu / nu , tolerance = 0.02)
})

## tests equal but not identical
test_that("R and C versions of rcmpUnder give same answer", {
    rcmpUnder <- demest:::rcmpUnder
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        mu <- runif(n = 1, max = 100)
        nu <- runif(n = 1, min = 1, max = 10)
        max <- 100L
        set.seed(1)
        ans.R <- replicate(n = 100, rcmpUnder(mu = mu, nu = nu, max = max, useC = FALSE))
        set.seed(1)
        ans.C <- replicate(n = 100, rcmpUnder(mu = mu, nu = nu, max = max, useC = TRUE))
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("rcmpOver works", {
    rcmpOver <- demest:::rcmpOver
    set.seed(0)
    mu <- runif(n = 1, max = 10000)
    nu <- runif(n = 1, max = (1 - 10^( -7)))
    max <- 100L
    y <- replicate(n = 10000, rcmpOver(mu = mu, nu = nu, max = max))
    y_fin <- y[is.finite(y) == TRUE]
    expect_equal(mean(y_fin), mu + 1 / (2 * nu) - 0.5, tolerance = 0.02)
    expect_equal(var(y_fin), mu / nu , tolerance = 0.02)
})

## tests equal but not identical
test_that("R and C versions of rcmpOver give same answer", {
    rcmpOver <- demest:::rcmpOver
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        mu <- runif(n = 1, max = 100)
        nu <- runif(n = 1, min = 1, max = 10)
        max <- 100L
        set.seed(1)
        ans.R <- replicate(n = 100, rcmpOver(mu = mu, nu = nu, max = max, useC = FALSE))
        set.seed(1)
        ans.C <- replicate(n = 100, rcmpOver(mu = mu, nu = nu, max = max, useC = TRUE))
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})

test_that("rcmp1 works", {
    rcmp1 <- demest:::rcmp1
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        mu <- runif(n = 1, max = 10000)
        nu <- runif(n = 1, max = 10)
        max <- 100L
        y <- replicate(n = 1000, rcmp1(mu = mu, nu = nu, max = max))
        y_fin <- y[is.finite(y) == TRUE]
        if (nu < 1){
            disp <- mean(y_fin) < var(y_fin)
        } else {
            disp <- mean(y_fin) >= var(y_fin)
        }
        expect_true(disp)
    }
})

## tests equal but not identical
test_that("R and C versions of rcmp1 give same answer", {
    rcmp1 <- demest:::rcmp1
    ## nu < 1
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        mu <- runif(n = 1, max = 10000)
        nu <- runif(n = 1, max = 1)
        max <- 100L
        set.seed(seed)
        ans.R <- replicate(n = 1000, rcmp1(mu = mu, nu = nu, max = max, useC = FALSE))
        set.seed(seed)
        ans.C <- replicate(n = 1000, rcmp1(mu = mu, nu = nu, max = max, useC = TRUE))
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
    ## nu = 1
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        mu <- runif(n = 1, max = 10000)
        nu <- 1
        max <- 100L
        set.seed(seed)
        ans.R <- replicate(n = 1000, rcmp1(mu = mu, nu = nu, max = max, useC = FALSE))
        set.seed(seed)
        ans.C <- replicate(n = 1000, rcmp1(mu = mu, nu = nu, max = max, useC = TRUE))
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
    ## nu > 1
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        mu <- runif(n = 1, max = 10000)
        nu <- runif(n = 1, min = 1, max = 10)
        max <- 100L
        set.seed(seed)
        ans.R <- replicate(n = 1000, rcmp1(mu = mu, nu = nu, max = max, useC = FALSE))
        set.seed(seed)
        ans.C <- replicate(n = 1000, rcmp1(mu = mu, nu = nu, max = max, useC = TRUE))
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
})









