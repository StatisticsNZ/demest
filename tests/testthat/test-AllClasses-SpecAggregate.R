

context("AllClasses-SpecAggregate")

n.test <- 5
test.identity <- FALSE
test.extended <- FALSE

test_that("can create object of class SpecAgCertain", {
    Concordance <- dembase::Concordance
    x <- new("SpecAgCertain",
             valueAg = new("ParameterVector", as.double(1:4)),
             weightAg = NULL,
             concordancesAg = list(),
             metadataAg = new("MetaData",
                              nms = "region",
                              dimtypes = "state",
                              DimScales = list(new("Categories", dimvalues = as.character(1:4)))))
    expect_true(validObject(x))
    x <- new("SpecAgCertain",
             valueAg = new("ParameterVector", as.double(1:4)),
             weightAg = Counts(array(1,
                                     dim = 4:3,
                                     dimnames = list(region = 1:4, time = c(0, 5, 10)))),
             concordancesAg = list(region = Concordance(data.frame(old = letters[1:8],
                                                                   new = rep(1:4, each = 2)))),
             metadataAg = new("MetaData",
                              nms = "region",
                              dimtypes = "state",
                              DimScales = list(new("Categories", dimvalues = as.character(1:4)))))
    expect_true(validObject(x))
    x <- new("SpecAgCertain",
             valueAg = new("ParameterVector", 1),
             weightAg = NULL,
             metadataAg = NULL)
    expect_true(validObject(x))
})


test_that("validityTests inherited from ConcordancesAgMixin work", {
    Concordance <- dembase::Concordance
    x <- new("SpecAgCertain",
             valueAg = new("ParameterVector", as.double(1:4)),
             weightAg = Counts(array(1,
                 dim = 4:3,
                 dimnames = list(region = 1:4, time = c(0, 5, 10)))),
             concordancesAg = list(),
             metadataAg = new("MetaData",
                 nms = "region",
                 dimtypes = "state",
                 DimScales = list(new("Categories", dimvalues = as.character(1:4)))))
    expect_true(validObject(x))
    ## all elements of 'concordancesAg' have class "ManyToOne"
    x.wrong <- x
    x.wrong@concordancesAg[[1]] <- "wrong"
    expect_error(validObject(x.wrong),
                 "'concordancesAg' has elements not of class \"ManyToOne\"")
    ## 'concordancesAg' has names
    x.wrong <- x
    x.wrong@concordancesAg[[1]] <- Concordance(data.frame(from = 1:3,
                                                          to = c(1, 1, 2)))
    expect_error(validObject(x.wrong),
                 "concordancesAg' does not have names")
    ## no duplicated names for 'concordancesAg'
    x.wrong <- x
    x.wrong@concordancesAg <- list(conc = Concordance(data.frame(from = 1:3,
                                                                 to = c(1, 1, 2))),
                                   conc = Concordance(data.frame(from = 1:3,
                                                                 to = c(1, 1, 2))))
    expect_error(validObject(x.wrong),
                 "concordancesAg' has duplicate names")
})

test_that("can create object of class SpecAgNormal", {
    x <- new("SpecAgNormal",
             valueAg = new("ParameterVector", as.double(1:4)),
             scaleAg = new("Scale", 0.1),
             sdAg = new("ScaleVec", rep(1.3, 4)),
             weightAg = NULL,
             metadataAg = new("MetaData",
                 nms = "region",
                 dimtypes = "state",
                 DimScales = list(new("Categories", dimvalues = as.character(1:4)))))
    expect_true(validObject(x))
    x <- new("SpecAgNormal",
             valueAg = new("ParameterVector", 1),
             scaleAg = new("Scale", 0.2),
             sdAg = new("ScaleVec", 0.5),
             weightAg = NULL,
             metadataAg = NULL)
    expect_true(validObject(x))
})

test_that("can create object of class SpecAgFun", {
    x <- new("SpecAgFun",
             funAg = function(x, weights) mean(x * weights),
             valueAg = new("ParameterVector", as.double(1:4)),
             sdAg = new("ScaleVec", rep(1.3, 4)),
             weightAg = NULL,
             metadataAg = new("MetaData",
                 nms = "region",
                 dimtypes = "state",
                 DimScales = list(new("Categories", dimvalues = as.character(1:4)))))
    expect_true(validObject(x))
    x <- new("SpecAgFun",
             funAg = function(x, weights) mean(x * weights),
             valueAg = new("ParameterVector", 1),
             sdAg = new("ScaleVec", 0.5),
             weightAg = NULL,
             metadataAg = NULL)
    expect_true(validObject(x))
})

test_that("validity tests inherited from FunAgMixin work", {
    x <- new("SpecAgFun",
             funAg = function(x, weights) mean(x * weights),
             valueAg = new("ParameterVector", as.double(1:4)),
             sdAg = new("ScaleVec", rep(1.3, 4)),
             weightAg = NULL,
             metadataAg = new("MetaData",
                 nms = "region",
                 dimtypes = "state",
                 DimScales = list(new("Categories", dimvalues = as.character(1:4)))))
    ## 'funAg' has arguments 'x' and 'weights'
    x.wrong <- x
    f.wrong <- function(wrong, weights) NULL
    x.wrong@funAg <- f.wrong
    expect_error(validObject(x.wrong),
                 "'funAg' does not have formal arguments 'x' and 'weights'")
})

test_that("can create object of class SpecAgLife", {
    Concordance <- dembase::Concordance
    x <- new("SpecAgLife",
             axAg = NULL,
             valueAg = new("ParameterVector", as.double(1:4)),
             sdAg = new("ScaleVec", rep(1.3, 4)),
             concordancesAg = list(),
             metadataAg = new("MetaData",
                              nms = "region",
                              dimtypes = "state",
                              DimScales = list(new("Categories", dimvalues = as.character(1:4)))))
    expect_true(validObject(x))
    x <- new("SpecAgLife",
             axAg = ValuesOne(0.1, labels = "0", name = "age", dimscale = "Intervals"),
             valueAg = new("ParameterVector", as.double(1:4)),
             sdAg = new("ScaleVec", rep(1.3, 4)),
             concordancesAg = list(region = Concordance(data.frame(from = 11:18,
                                                          to = rep(1:4, each = 2)))),
             metadataAg = new("MetaData",
                              nms = "region",
                              dimtypes = "state",
                              DimScales = list(new("Categories", dimvalues = as.character(1:4)))))
    expect_true(validObject(x))
})

test_that("validity tests for SpecAgLife inherited from SpecAxAgMixin work", {
    Concordance <- dembase::Concordance
    x <- new("SpecAgLife",
             axAg = ValuesOne(0.1, labels = "0", name = "age", dimscale = "Intervals"),
             valueAg = new("ParameterVector", as.double(1:4)),
             sdAg = new("ScaleVec", rep(1.3, 4)),
             concordancesAg = list(region = Concordance(data.frame(from = 11:18,
                                                                   to = rep(1:4, each = 2)))),
             metadataAg = new("MetaData",
                              nms = "region",
                              dimtypes = "state",
                              DimScales = list(new("Categories", dimvalues = as.character(1:4)))))
    ## 'axAg' has dimension with dimtype "age"
    x.wrong <- x
    x.wrong@axAg <- ValuesOne(0.1, labels = "0", name = "wrong") 
    expect_error(validObject(x.wrong),
                 "'axAg' does not have a dimension with dimtype \"age\"")
    ## age dimension of 'axAg' has dimscale "Intervals"
    x.wrong <- x
    x.wrong@axAg <- ValuesOne(0.1, labels = "0", name = "age", dimscale = "Points")
    expect_error(validObject(x.wrong),
                 "dimension of 'axAg' with dimtype \"age\" does not have dimscale \"Intervals\"")
    ## 'axAg' has no missing values
    x.wrong <- x
    x.wrong@axAg[1] <- NA
    expect_error(validObject(x.wrong),
                 "'axAg' has missing values")
    ## 'axAg' is non-negative
    x.wrong <- x
    x.wrong@axAg[1] <- -1
    expect_error(validObject(x.wrong),
                 "'axAg' has negative values")
})



test_that("can create object of class SpecAgPoisson", {
    x <- new("SpecAgPoisson",
             valueAg = new("ParameterVector", as.double(1:4)),
             metadataAg = new("MetaData",
                 nms = "region",
                 dimtypes = "state",
                 DimScales = list(new("Categories", dimvalues = as.character(1:4)))))
    expect_true(validObject(x))
    x <- new("SpecAgPoisson",
             scaleAg = new("Scale", 0.1),
             valueAg = new("ParameterVector", 1),
             metadataAg = NULL)
    expect_true(validObject(x))
})

test_that("can create object of class SpecAgPlaceholder", {
    x <- new("SpecAgPlaceholder")
    expect_true(validObject(x))
})
