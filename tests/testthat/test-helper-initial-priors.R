
context("helper-initial-priors")

n.test <- 5
test.identity <- FALSE
test.extended <- FALSE

test_that("initialCov works", {
    initialCov <- demest:::initialCov
    set.seed(100)
    data <- data.frame(region = rep(letters[1:10], times = 2),
                       sex = rep(c("f", "m"), each = 10),
                       income = rnorm(20),
                       cat = sample(c("x" ,"y", "z"), size = 20, replace = TRUE))
    formula <- mean ~ income * cat
    contrastsArg = list(cat = diag(3))
    spec <- Exch(covariates = Covariates(formula = formula,
                                         data = data,
                                         contrastsArg = contrastsArg))
    expect_is(spec, "SpecExchNormCov")
    beta <- rnorm(20)
    metadata <- new("MetaData",
                    nms = c("region", "sex"),
                    dimtypes = c("state", "state"),
                    DimScales = list(new("Categories", dimvalues = letters[1:10]),
                                     new("Categories", dimvalues = c("f", "m"))))
    allStrucZero <- rep(FALSE, 20)
    l <- initialCov(spec,
                    beta = beta,
                    metadata = metadata,
                    sY = NULL,
                    allStrucZero = allStrucZero)
    expect_identical(l$AEtaCoef, new("ScaleVec", rep(0.5, 7)))
    expect_identical(l$AEtaIntercept, new("Scale", 10))
    expect_identical(l$contrastsArg, contrastsArg)
    expect_identical(length(l$eta), 8L)
    expect_identical(l$formula, formula)
    expect_identical(l$infant, new("LogicalFlag", FALSE))
    expect_identical(l$meanEtaCoef, new("ParameterVector", rep(0, 7)))
    expect_identical(l$nuEtaCoef, new("DegreesFreedomVector", rep(7.0, 7)))
    expect_identical(l$P, new("Length", 8L))
    expect_identical(length(l$UEtaCoef), 7L)
    expect_identical(dim(l$Z), c(20L, 8L))
    mean <- c(-1, 1, 1, 0.5, 0.5, 0.5, 0.5)
    scale <- c(1, 1, 1, 0.2, 0.3, 0.4, 0.1)
    spec <- Exch(covariates = Covariates(formula = formula,
                                         data = data,
                                         contrastsArg = contrastsArg,
                                         coef = TDist(mean = mean,
                                                      scale = scale)))
    l <- initialCov(spec,
                    beta = beta,
                    metadata = metadata,
                    sY = NULL,
                    allStrucZero = allStrucZero)
    expect_identical(l$AEtaCoef, new("ScaleVec", scale))
    expect_identical(l$meanEtaCoef, new("ParameterVector", mean))
    spec <- Exch(covariates = Covariates(formula = formula,
                                         data = data,
                                         contrastsArg = contrastsArg,
                                         coef = TDist(mult = 0.3)))
    l <- initialCov(spec,
                    beta = beta,
                    metadata = metadata,
                    sY = NULL,
                    allStrucZero = allStrucZero)
    expect_identical(l$AEtaCoef, new("ScaleVec", rep(0.5 * 0.3, 7)))
    expect_identical(l$meanEtaCoef, new("ParameterVector", rep(0, 7)))
})

test_that("initialCovPredict works", {
    initialCovPredict <- demest:::initialCovPredict
    initialPrior <- demest:::initialPrior
    set.seed(100)
    data <- data.frame(time = seq(2000, 2050, 5),
                       sex = rep(c("f", "m"), each = 11),
                       income = rnorm(22),
                       cat = sample(c("x" ,"y", "z"), size = 22, replace = TRUE))
    formula <- mean ~ income * cat
    contrastsArg = list(cat = diag(3))
    spec <- DLM(covariates = Covariates(formula = formula,
                                        data = data, contrastsArg = contrastsArg))
    beta <- rnorm(22)
    metadata <- new("MetaData",
                    nms = c("time", "sex"),
                    dimtypes = c("time", "sex"),
                    DimScales = list(new("Points", dimvalues = seq(2000, 2050, 5)),
                                     new("Sexes", dimvalues = c("f", "m"))))
    strucZeroArray <- Counts(array(1L,
                                   dim = c(11, 2),
                                   dimnames = list(time = seq(2000, 2050, 5),
                                                   sex = c("f", "m"))))
    prior <- initialPrior(spec,
                          beta = beta,
                          metadata = metadata,
                          sY = NULL,
                          isSaturated = TRUE,
                          margin = 1:2,
                          strucZeroArray = strucZeroArray)
    data.new <- data.frame(time = seq(2055, 2080, 5),
                           sex = rep(c("f", "m"), each = 6),
                           income = rnorm(12),
                           cat = sample(c("x" ,"y", "z"), size = 12, replace = TRUE))
    metadata.new <- new("MetaData",
                        nms = c("time", "sex"),
                        dimtypes = c("time", "sex"),
                        DimScales = list(new("Points", dimvalues = seq(2055, 2080, 5)),
                                         new("Sexes", dimvalues = c("f", "m"))))
    allStrucZero <- rep(FALSE, 12)
    l <- initialCovPredict(prior,
                           data = data.new,
                           metadata = metadata.new,
                           allStrucZero = allStrucZero)
    expect_identical(dim(l$Z), c(12L, 8L))
})

test_that("makeStandardizedVariables works", {
    makeStandardizedVariables <- demest:::makeStandardizedVariables
    formula <-  ~ age * sex
    inputs <- data.frame(age = rep(1:50, times = 2), sex = rep(c("f", "m"), each = 50))
    namePrior <- "age:sex"
    contrastsArg <- list()
    allStrucZero <- rep(FALSE, 100)
    ans.obtained <- makeStandardizedVariables(formula = formula,
                                              inputs = inputs,
                                              namePrior = namePrior,
                                              contrastsArg = contrastsArg,
                                              allStrucZero = allStrucZero)
    means <- unname(apply(ans.obtained, 2, mean))
    expect_equal(means, c(1, 0, 0, 0))
    sds <- unname(apply(ans.obtained, 2, sd))
    expect_equal(sds, c(0, 0.5, 0.5, 0.25), tol = 0.03)
    allStrucZero <- c(rep(TRUE, 10), rep(FALSE, 90))
    ans.obtained <- makeStandardizedVariables(formula = formula,
                                              inputs = inputs,
                                              namePrior = namePrior,
                                              contrastsArg = contrastsArg,
                                              allStrucZero = allStrucZero)
    means <- unname(apply(ans.obtained[!allStrucZero,], 2, mean))
    expect_equal(means, c(1, 0, 0, 0), tol = 0.03)
    sds <- unname(apply(ans.obtained[!allStrucZero,], 2, sd))
    expect_equal(sds, c(0, 0.5, 0.5, 0.25), tol = 0.03)
})


test_that("makeAllStrucZero works", {
    makeAllStrucZero <- demest:::makeAllStrucZero
    strucZeroArray <- Counts(array(c(1L, 0L),
                                   dim = c(2:3, 3),
                                   dimnames = list(sex = c("f", "m"),
                                                   eth_orig = c("a", "b", "c"),
                                                   eth_dest = c("a", "b", "c"))))
    metadata <- new("MetaData",
                    nms = "sex",
                    dimtypes = "sex",
                    DimScales = list(new("Sexes", dimvalues = c("f", "m"))))
    margin <- 1L
    ans.obtained <- makeAllStrucZero(strucZeroArray = strucZeroArray,
                                     margin = margin,
                                     metadata = metadata)
    ans.expected <- c(FALSE, TRUE)
    expect_identical(ans.obtained, ans.expected)
    strucZeroArray <- Counts(array(rep(1L - diag(3), each = 2),
                                   dim = c(2:3, 3),
                                   dimnames = list(sex = c("f", "m"),
                                                   eth_orig = c("a", "b", "c"),
                                                   eth_dest = c("a", "b", "c"))))
    metadata <- new("MetaData",
                    nms = c("eth_orig", "eth_dest"),
                    dimtypes = c("origin", "destination"),
                    DimScales = list(new("Categories", dimvalues = c("a", "b", "c")),
                                     new("Categories", dimvalues = c("a", "b", "c"))))
    margin <- 2:3
    ans.obtained <- makeAllStrucZero(strucZeroArray = strucZeroArray,
                                     margin = margin,
                                     metadata = metadata)
    ans.expected <- as.logical(diag(3))
    expect_identical(ans.obtained, ans.expected)
    strucZeroArray <- Counts(array(rep(1L - diag(3), each = 2),
                                   dim = c(2:3, 3),
                                   dimnames = list(sex = c("f", "m"),
                                                   eth_orig = c("a", "b", "c"),
                                                   eth_dest = c("a", "b", "c"))))
    metadata <- new("MetaData",
                    nms = c("eth_orig", "eth_dest"),
                    dimtypes = c("origin", "destination"),
                    DimScales = list(new("Categories", dimvalues = c("a", "b", "c")),
                                     new("Categories", dimvalues = c("a", "b", "wrong"))))
    margin <- 1:4
    expect_error(makeAllStrucZero(strucZeroArray = strucZeroArray,
                                  margin = margin,
                                  metadata = metadata),
                 "problem assigning structural zeros to prior")
})

test_that("makeAllStrucZeroError works", {
    makeAllStrucZeroError <- demest:::makeAllStrucZeroError
    strucZeroArray <- Counts(array(1L,
                                   dim = c(2:3, 3),
                                   dimnames = list(sex = c("f", "m"),
                                                   eth_orig = c("a", "b", "c"),
                                                   eth_dest = c("a", "b", "c"))))
    metadata <- new("MetaData",
                    nms = "sex",
                    dimtypes = "sex",
                    DimScales = list(new("Sexes", dimvalues = c("f", "m"))))
    ans.obtained <- makeAllStrucZeroError(strucZeroArray = strucZeroArray,
                                          margin = 1L,
                                          metadata = metadata,
                                          classPrior = "MixNormZero")
    ans.expected <- rep(FALSE, times = 2)
    expect_identical(ans.obtained, ans.expected)
    strucZeroArray <- Counts(array(c(1L, 0L),
                                   dim = c(2:3, 3),
                                   dimnames = list(sex = c("f", "m"),
                                                   eth_orig = c("a", "b", "c"),
                                                   eth_dest = c("a", "b", "c"))))
    metadata <- new("MetaData",
                    nms = "sex",
                    dimtypes = "sex",
                    DimScales = list(new("Sexes", dimvalues = c("f", "m"))))
    expect_error(makeAllStrucZeroError(strucZeroArray = strucZeroArray,
                                       margin = 1L,
                                       metadata = metadata,
                                       classPrior = "MixNormZero"),
                 "'sex' has elements where all contributing cells are structural zeros; priors with class \"MixNormZero\" cannot be used in such cases")
})


test_that("makeAlongAllStrucZero works", {
    makeAlongAllStrucZero <- demest:::makeAlongAllStrucZero
    strucZeroArray <- Counts(array(c(rep(1L, 15), rep(0L, 3)),
                                   dim = c(2:3, 3),
                                   dimnames = list(sex = c("f", "m"),
                                                   time = c("2001-2005", "2006-2010", "2011-2015"),
                                                   eth = c("a", "b", "c"))))
    metadata <- new("MetaData",
                    nms = c("sex", "time"),
                    dimtypes = c("sex", "time"),
                    DimScales = list(new("Sexes", dimvalues = c("f", "m")),
                                     new("Intervals", dimvalues = c(2000, 2005, 2010, 2015))))
    iAlong <- 2L
    ans.obtained <- makeAlongAllStrucZero(strucZeroArray = strucZeroArray,
                                          iAlong = iAlong,
                                          margin = 1:2,
                                          metadata = metadata)
    ans.expected <- c(FALSE, FALSE)
    expect_identical(ans.obtained, ans.expected)
    strucZeroArray <- Counts(array(c(rep(1L, 12), rep(0L, 6)),
                                   dim = c(2:3, 3),
                                   dimnames = list(sex = c("f", "m"),
                                                   time = c("2001-2005", "2006-2010", "2011-2015"),
                                                   eth = c("a", "b", "c"))))
    metadata <- new("MetaData",
                    nms = c("time", "eth"),
                    dimtypes = c("time", "state"),
                    DimScales = list(new("Intervals", dimvalues = c(2000, 2005, 2010, 2015)),
                                     new("Categories", dimvalues = c("a", "b", "c"))))
    iAlong <- 1L
    margin <- 2:3
    ans.obtained <- makeAlongAllStrucZero(strucZeroArray = strucZeroArray,
                                          iAlong = iAlong,
                                          margin = margin,
                                          metadata = metadata)
    ans.expected <- c(FALSE, FALSE, TRUE)
    expect_identical(ans.obtained, ans.expected)
    strucZeroArray.wrong <- Counts(array(rep(c(1L, 1L, 0L, 0L, 1L, 1L), times = 3),
                                   dim = c(2:3, 3),
                                   dimnames = list(sex = c("f", "m"),
                                                   time = c("2001-2005", "2006-2010", "2011-2015"),
                                                   eth = c("a", "b", "c"))))
    expect_error(makeAlongAllStrucZero(strucZeroArray = strucZeroArray.wrong,
                                       iAlong = iAlong,
                                       margin = 2:3,
                                       metadata = metadata),
                 "all cells contributing to element \"2006-2010\" of 'along' dimension \\[\"time\"\\] for prior 'time:eth' are structural zeros")
})

test_that("makeStrucZeroArray works", {
    makeStrucZeroArray <- demest:::makeStrucZeroArray
    y <- Counts(array(10L,
                      dim = c(2:3, 3),
                      dimnames = list(sex = c("f", "m"),
                                      eth_orig = c("a", "b", "c"),
                                      eth_dest = c("a", "b", "c"))))
    ## NULL
    ans.obtained <- makeStrucZeroArray(structuralZeros = NULL, y = y)
    ans.expected <- Counts(array(1L,
                                 dim = c(2:3, 3),
                                 dimnames = list(sex = c("f", "m"),
                                                 eth_orig = c("a", "b", "c"),
                                                 eth_dest = c("a", "b", "c"))))
    expect_identical(ans.obtained, ans.expected)
    ## diag
    ans.obtained <- makeStrucZeroArray(structuralZeros = new("Values"), y = y)
    ans.expected <- Counts(array(1L,
                      dim = c(2:3, 3),
                      dimnames = list(sex = c("f", "m"),
                                      eth_orig = c("a", "b", "c"),
                                      eth_dest = c("a", "b", "c"))))
    ans.expected[slice.index(ans.expected, 2) == slice.index(ans.expected, 3)] <- 0L
    expect_identical(ans.obtained, ans.expected)
    ## Values
    structuralZeros <- ValuesOne(0:1, labels = c("m", "f"), name = "sex")
    ans.obtained <- makeStrucZeroArray(structuralZeros = structuralZeros, y = y)
    ans.expected <- Counts(array(1:0,
                      dim = c(2:3, 3),
                      dimnames = list(sex = c("f", "m"),
                                      eth_orig = c("a", "b", "c"),
                                      eth_dest = c("a", "b", "c"))))
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeStrucZeroArrayNULL works", {
    makeStrucZeroArrayNULL <- demest:::makeStrucZeroArrayNULL
    y <- Counts(array(10L,
                      dim = 2:3,
                      dimnames = list(sex = c("f", "m"), age = c("0-4", "5-9", "10+"))))
    ans.obtained <- makeStrucZeroArrayNULL(y)
    ans.expected <- Counts(array(1L,
                                 dim = 2:3,
                                 dimnames = list(sex = c("f", "m"), age = c("0-4", "5-9", "10+"))))
    expect_identical(ans.obtained, ans.expected)
})

test_that("makeStrucZeroArrayDiag works", {
    makeStrucZeroArrayDiag <- demest:::makeStrucZeroArrayDiag
    y <- Counts(array(10L,
                      dim = c(2:3, 3),
                      dimnames = list(sex = c("f", "m"),
                                      eth_orig = c("a", "b", "c"),
                                      eth_dest = c("a", "b", "c"))))
    ans.obtained <- makeStrucZeroArrayDiag(y)
    ans.expected <- Counts(array(1L,
                                 dim = c(2:3, 3),
                                 dimnames = list(sex = c("f", "m"),
                                                 eth_orig = c("a", "b", "c"),
                                                 eth_dest = c("a", "b", "c"))))
    ans.expected[slice.index(ans.expected, 2) == slice.index(ans.expected, 3)] <- 0L
    expect_identical(ans.obtained, ans.expected)
    y <- Counts(array(10L,
                      dim = c(2, 2, 2:3, 3),
                      dimnames = list(reg_dest = 1:2,
                                      reg_orig = 1:2,
                                      sex = c("f", "m"),
                                      eth_orig = c("a", "b", "c"),
                                      eth_dest = c("a", "b", "c"))))
    ans.obtained <- makeStrucZeroArrayDiag(y)
    ans.expected <- Counts(array(1L,
                                 dim = c(2, 2, 2:3, 3),
                                 dimnames = list(reg_dest = 1:2,
                                                 reg_orig = 1:2,
                                                 sex = c("f", "m"),
                                                 eth_orig = c("a", "b", "c"),
                                                 eth_dest = c("a", "b", "c"))))
    ans.expected[slice.index(ans.expected, 1) == slice.index(ans.expected, 2)] <- 0L
    ans.expected[slice.index(ans.expected, 4) == slice.index(ans.expected, 5)] <- 0L
    expect_identical(ans.obtained, ans.expected)
    y <- Counts(array(10L,
                      dim = 2:3,
                      dimnames = list(sex = c("f", "m"), age = c("0-4", "5-9", "10+"))))
    expect_error(makeStrucZeroArrayDiag(y),
                 "'y' has no dimensions with dimtype \"origin\"")
    y <- Counts(array(10L,
                      dim = c(2:3, 3),
                      dimnames = list(sex = c("f", "m"),
                                      eth_orig = c("a", "b", "c"),
                                      eth_dest = c("a", "b", "wrong"))))
    expect_error(makeStrucZeroArrayDiag(y))
})

test_that("makeStrucZeroArrayGeneral works", {
    makeStrucZeroArrayGeneral <- demest:::makeStrucZeroArrayGeneral
    structuralZeros <- ValuesOne(c(1, 0), labels = c("f", "m"), name = "sex")
    y <- Counts(array(10L,
                      dim = c(2:3, 3),
                      dimnames = list(sex = c("f", "m"),
                                      eth_orig = c("a", "b", "c"),
                                      eth_dest = c("a", "b", "c"))))
    ans.obtained <- makeStrucZeroArrayGeneral(structuralZeros = structuralZeros, y = y)
    ans.expected <- Counts(array(c(1L, 0L),
                                 dim = c(2:3, 3),
                                 dimnames = list(sex = c("f", "m"),
                                                 eth_orig = c("a", "b", "c"),
                                                 eth_dest = c("a", "b", "c"))))
    expect_identical(ans.obtained, ans.expected)
    structuralZeros <- Values(matrix(c(0, 1, 1, 0), nr = 2, dimnames = list(reg_orig = 1:2, reg_dest = 1:2)))
    y <- Counts(array(10L,
                      dim = c(2, 2, 2:3, 3),
                      dimnames = list(reg_dest = 1:2,
                                      reg_orig = 1:2,
                                      sex = c("f", "m"),
                                      eth_orig = c("a", "b", "c"),
                                      eth_dest = c("a", "b", "c"))))
    ans.obtained <- makeStrucZeroArrayGeneral(structuralZeros = structuralZeros, y = y)
    ans.expected <- Counts(array(1L - (slice.index(y, 1) == slice.index(y, 2)),
                                 dim = c(2, 2, 2:3, 3),
                                 dimnames = list(reg_dest = 1:2,
                                                 reg_orig = 1:2,
                                                 sex = c("f", "m"),
                                                 eth_orig = c("a", "b", "c"),
                                                 eth_dest = c("a", "b", "c"))))
    expect_identical(ans.obtained, ans.expected)
    structuralZeros <- Values(matrix(c(0, 1, 1, 0),
                                     nr = 2,
                                     dimnames = list(reg_orig = 1:2, reg_dest = c(1, "wrong"))))
    y <- Counts(array(10L,
                      dim = c(2, 2, 2:3, 3),
                      dimnames = list(reg_dest = 1:2,
                                      reg_orig = 1:2,
                                      sex = c("f", "m"),
                                      eth_orig = c("a", "b", "c"),
                                      eth_dest = c("a", "b", "c"))))
    expect_error(makeStrucZeroArrayGeneral(structuralZeros = structuralZeros, y = y),
                 "problem expanding 'structuralZeros' to make it compatible with 'y' :")
})

test_that("makeZ works", {
    makeZ <- demest:::makeZ
    makeStandardizedVariables <- demest:::makeStandardizedVariables
    ## age main effect with infant = TRUE and no data
    formula <- new("formula")
    data <- new("data.frame")
    metadata <- new("MetaData",
                    nms = "age",
                    dimtypes = "age", 
                    DimScales = list(new("Intervals", dimvalues = c(0, 1, 5, 10, Inf))))
    contrastsArg <- list()
    infant <- TRUE
    ans.obtained <- makeZ(formula = formula,
                          data = data,
                          metadata = metadata,
                          contrastsArg = contrastsArg,
                          infant = infant,
                          allStrucZero = rep(FALSE, 4))
    ans.expected <- cbind(`(Intercept)` = rep(1, 4),
                          infant = c(0.75, -0.25, -0.25, -0.25))
    rownames(ans.expected) <- 1:4
    expect_identical(ans.obtained, ans.expected)
    ## age main effect with infant = TRUE and has data
    formula <- ~ income
    data <- data.frame(age = c("0", "1-4", "5-9", "10+"),
                       income = 1:4)
    metadata <- new("MetaData",
                    nms = "age",
                    dimtypes = "age", 
                    DimScales = list(new("Intervals", dimvalues = c(0, 1, 5, 10, Inf))))
    contrastsArg <- list()
    infant <- TRUE
    ans.obtained <- makeZ(formula = formula,
                          data = data,
                          metadata = metadata,
                          contrastsArg = contrastsArg,
                          infant = infant,
                          allStrucZero = rep(FALSE, 4))
    ans.expected <- makeStandardizedVariables(formula = ~ income + infant,
                                              inputs = data.frame(income = 1:4,
                                                                  infant = c(1, 0, 0, 0)),
                                              namePrior = "age",
                                              contrastsArg = list(),
                                              allStrucZero = rep(FALSE, 4))
    expect_identical(ans.obtained, ans.expected)
    ## infant = FALSE
        formula <- ~ income
    data <- data.frame(age = c("0", "1-4", "5-9", "10+"),
                       income = 1:4)
    metadata <- new("MetaData",
                    nms = "age",
                    dimtypes = "age", 
                    DimScales = list(new("Intervals", dimvalues = c(0, 1, 5, 10, Inf))))
    contrastsArg <- list()
    infant <- FALSE
    ans.obtained <- makeZ(formula = formula,
                          data = data,
                          metadata = metadata,
                          contrastsArg = contrastsArg,
                          infant = infant,
                          allStrucZero = rep(FALSE, 4))
    ans.expected <- makeStandardizedVariables(formula = ~ income,
                                              inputs = data.frame(income = 1:4),
                                              namePrior = "age",
                                              contrastsArg = list(),
                                              allStrucZero = rep(FALSE, 4))
    expect_identical(ans.obtained, ans.expected)
})
