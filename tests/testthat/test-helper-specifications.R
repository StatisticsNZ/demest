
context("helper-specifications")

n.test <- 5
test.identity <- FALSE
test.extended <- FALSE


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

test_that("checkAndTidyJump works", {
    checkAndTidyJump <- demest:::checkAndTidyJump
    expect_identical(checkAndTidyJump(NULL),
                     new("Scale", 0.1))
    expect_identical(checkAndTidyJump(0.5),
                     new("Scale", 0.5))
    expect_identical(checkAndTidyJump(1L),
                     new("Scale", 1.0))
    expect_error(checkAndTidyJump(c(1, 1)),
                 "'jump' does not have length 1")
    expect_error(checkAndTidyJump(as.numeric(NA)),
                 "'jump' is missing")
    expect_error(checkAndTidyJump("a"),
                 "'jump' is not numeric")
    expect_error(checkAndTidyJump(0),
                 "'jump' is non-positive")
})

test_that("checkAndTidyUseHMC works", {
    checkAndTidyUseHMC <- demest:::checkAndTidyUseHMC
    expect_identical(checkAndTidyUseHMC(NULL),
                     new("LogicalFlag", TRUE))
    expect_identical(checkAndTidyUseHMC(FALSE),
                     new("LogicalFlag", FALSE))
    expect_error(checkAndTidyUseHMC(c(FALSE, TRUE)),
                 "'useHMC' does not have length 1")
    expect_error(checkAndTidyUseHMC(NA),
                 "'useHMC' is missing")
    expect_error(checkAndTidyUseHMC("FALSE"),
                 "'useHMC' does not have type \"logical\"")
})

test_that("checkAndTidySizeStep works", {
    checkAndTidySizeStep <- demest:::checkAndTidySizeStep
    expect_identical(checkAndTidySizeStep(NULL),
                     new("Scale", 0.1))
    expect_identical(checkAndTidySizeStep(0.5),
                     new("Scale", 0.5))
    expect_identical(checkAndTidySizeStep(1L),
                     new("Scale", 1.0))
    expect_error(checkAndTidySizeStep(c(1, 1)),
                 "'sizeStep' does not have length 1")
    expect_error(checkAndTidySizeStep(as.numeric(NA)),
                 "'sizeStep' is missing")
    expect_error(checkAndTidySizeStep("a"),
                 "'sizeStep' is not numeric")
    expect_error(checkAndTidySizeStep(0),
                 "'sizeStep' is non-positive")
})

test_that("checkAndTidyNStep works", {
    checkAndTidyNStep <- demest:::checkAndTidyNStep
    expect_identical(checkAndTidyNStep(NULL),
                     new("Length", 10L))
    expect_identical(checkAndTidyNStep(5),
                     new("Length", 5L))
    expect_identical(checkAndTidyNStep(1L),
                     new("Length", 1L))
    expect_error(checkAndTidyNStep(c(1, 1)),
                 "'nStep' does not have length 1")
    expect_error(checkAndTidyNStep(as.numeric(NA)),
                 "'nStep' is missing")
    expect_error(checkAndTidyNStep("a"),
                 "'nStep' is not numeric")
    expect_error(checkAndTidyNStep(1.1),
                 "'nStep' is not an integer")
    expect_error(checkAndTidyNStep(0L),
                 "'nStep' is non-positive")
})


test_that("checkAndTidySeries works", {
    checkAndTidySeries <- demest:::checkAndTidySeries
    expect_identical(checkAndTidySeries("births"),
                     new("SpecName", "births"))
    expect_identical(checkAndTidySeries(NULL),
                     new("SpecName", as.character(NA)))
    expect_error(checkAndTidySeries(1),
                 "'series' does not have type \"character\"")
    expect_error(checkAndTidySeries(c("births", "births")),
                 "'series' does not have length 1")
    expect_error(checkAndTidySeries(""),
                 "'series' is blank")
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

test_that("checkAndTidyMultVec works", {
    checkAndTidyMultVec <- demest:::checkAndTidyMultVec
    expect_identical(checkAndTidyMultVec(mult = c(3, 2, 1),
                                         scale = c(NA, NA, NA),
                                         nameMult = "mult",
                                         nameScale = "scale"),
                     new("ScaleVec", c(3, 2, 1)))
    expect_identical(checkAndTidyMultVec(mult = 3,
                                         scale = c(NA, NA, NA),
                                         nameMult = "mult",
                                         nameScale = "scale"),
                     new("ScaleVec", c(3, 3, 3)))
    expect_identical(checkAndTidyMultVec(mult = c(3, 2, 1),
                                         scale = NA,
                                         nameMult = "mult",
                                         nameScale = "scale"),
                     new("ScaleVec", c(3, 2, 1)))
    expect_identical(checkAndTidyMultVec(mult = 1,
                                         scale = c(3, 1, 3),
                                         nameMult = "mult",
                                         nameScale = "scale"),
                     new("ScaleVec", c(1, 1, 1)))
    expect_error(checkAndTidyMultVec(mult = c(3, 2, 1),
                                     scale = c(NA, 2),
                                     nameMult = "mult",
                                     nameScale = "scale"),
                 "'scale' has length 2 and 'mult' has length 3")
    expect_warning(checkAndTidyMultVec(mult = c(3, 2, 1),
                                       scale = c(NA, 2, 1),
                                       nameMult = "mult",
                                       nameScale = "scale"),
                   "'mult' argument ignored for elements where 'scale' argument supplied")
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
