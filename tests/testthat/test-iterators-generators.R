
context("iterators-generators")

n.test <- 5
test.identity <- FALSE
test.extended <- TRUE

test_that("AlongIterator creates objects from valid inputs", {
    AlongIterator <- demest:::AlongIterator
    ## dim = 3:4, iAlong = 1L
    expect_identical(AlongIterator(dim = 3:4, iAlong = 1L),
                     new("AlongIterator",
                         indices = 1:3,
                         nWithin = 1L,
                         nBetween = 4L,
                         initial = 1:3,
                         incrementBetween = 3L))
    ## dim = 2:4, iAlong = 2L
    expect_identical(AlongIterator(dim = 2:4, iAlong = 2L),
                     new("AlongIterator",
                         indices = c(1L, 3L, 5L),
                         nWithin = 2L,
                         nBetween = 4L,
                         initial = c(1L, 3L, 5L),
                         incrementBetween = 5L))
    ## dim = c(4L, 3L, 2L, 2L), iAlong = 4L
    expect_identical(AlongIterator(dim = c(4L, 3L, 2L, 2L), iAlong = 4L),
                     new("AlongIterator",
                         indices = c(1L, 25L),
                         nWithin = 24L,
                         nBetween = 1L,
                         initial = c(1L, 25L),
                         incrementBetween = 25L))
})

test_that("BetaIterator creates objects from valid inputs", {
    BetaIterator <- demest:::BetaIterator
    ## two dimensions; margins in order
    x.obtained <- BetaIterator(dim = 4:3, margins = list(0L, 1L, 2L, 1:2))
    x.expected <- new("BetaIterator",
                      indices = c(1L, 1L, 1L, 1L),
                      strideLengths = list(c(1L, 0L), c(0L, 1L), c(1L, 4L)),
                      dimIterators = list(new("DimIterator", nStrides = -3L, nWithin = 1L, nBetween = 4L),
                      new("DimIterator", nStrides = -2L, nWithin = 4L, nBetween = 3L)))
    expect_identical(x.obtained, x.expected)
    ## two dimensions; margins out of ourder
    x.obtained <- BetaIterator(dim = 4:3, margins = list(0L, 1:2, 2L, 1L))
    x.expected <- new("BetaIterator",
                      indices = c(1L, 1L, 1L, 1L),
                      strideLengths = list(c(1L, 4L), c(0L, 1L), c(1L, 0L)),
                      dimIterators = list(new("DimIterator", nStrides = -3L, nWithin = 1L, nBetween = 4L),
                      new("DimIterator", nStrides = -2L, nWithin = 4L, nBetween = 3L)))
    expect_identical(x.obtained, x.expected)
    ## one dimension
    x.obtained <- BetaIterator(dim = 4:3, margins = list(0L))
    x.expected <- new("BetaIterator",
                      indices = 1L,
                      strideLengths = list(),
                      dimIterators = list())
    expect_identical(x.obtained, x.expected)
})

test_that("ComponentIterator works with Accession", {
    CohortIterator <- demest:::CohortIterator
    Accession <- dembase:::Accession
    accession <- Counts(array(1L,
                            dim = c(3, 3, 2),
                            dimnames = list(age = c("0-4", "5-9", "10+"),
                                region = 1:3,
                                time = c("2001-2005", "2006-2010"))))
    accession <- Accession(accession = accession)
    ans.obtained <- CohortIterator(accession)
    ans.expected <- new("CohortIteratorAccessionPopulation",
                        i = 1L,
                        nTime = 2L,
                        stepTime = 9L,
                        iTime = 1L,
                        hasAge = TRUE,
                        nAge = 3L,
                        stepAge = 1L,
                        iAge = 1L,
                        finished = FALSE)
    expect_identical(ans.obtained, ans.expected)
})

test_that("CohortIterator works with BirthsMovementsHasParentChild", {
    CohortIterator <- demest:::CohortIterator
    BirthsMovements <- dembase:::BirthsMovements
    births <- Counts(array(1L,
                           dim = c(3, 3, 2, 3, 2),
                           dimnames = list(eth_parent = 1:3,
                               eth_child = 1:3,
                               age = c("5-9", "10+"),
                               region = 1:3,
                               time = c("2001-2005", "2006-2010"))))
    template <- Counts(array(0L,
                             dim = c(3, 3, 3, 2, 2),
                             dimnames = list(eth = 1:3,
                                 age = c("0-4", "5-9", "10+"),
                                 region = 1:3,
                                 time = c("2001-2005", "2006-2010"),
                                 triangle = c("TL", "TU"))))
    set.seed(1)
    births <- BirthsMovements(births = births, template = template)
    ans.obtained <- CohortIterator(births)
    ans.expected <- new("CohortIteratorOrigDestParChPool",
                        i = 1L,
                        nTime = 2L,
                        stepTime = 54L,
                        iTime = 1L,
                        hasAge = TRUE,
                        nAge = 2L,
                        stepAge = 9L,
                        iAge = 1L,
                        stepTriangle = 108L,
                        iTriangle = 1L,
                        iVec = c(1L, 4L, 7L),
                        lengthVec = 3L,
                        increment = c(0L, 3L, 6L),
                        finished = FALSE)
    expect_identical(ans.obtained, ans.expected)
})

test_that("ComponentIterator works with InternalMovementsOrigDest", {
    CohortIterator <- demest:::CohortIterator
    InternalMovements <- dembase:::InternalMovements
    internal <- Counts(array(1L,
                             dim = c(3, 3, 2, 3),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 reg_dest= 1:3,
                                 time = c("2001-2005", "2006-2010"),
                                 reg_orig = 1:3)))
    template <- Counts(array(0L,
                             dim = c(3, 3, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 reg = 1:3,
                                 time = c("2001-2005", "2006-2010"),
                                 triangle = c("TL", "TU"))))
    set.seed(1)
    internal <- InternalMovements(internal = internal,
                                  template = template)
    ans.obtained <- CohortIterator(internal)
    ans.expected <- new("CohortIteratorOrigDestParChPool",
                        i = 1L,
                        nTime = 2L,
                        stepTime = 27L,
                        iTime = 1L,
                        hasAge = TRUE,
                        nAge = 3L,
                        stepAge = 1L,
                        iAge = 1L,
                        stepTriangle = 54L,
                        iTriangle = 1L,
                        iVec = c(1L, 10L, 19L),
                        lengthVec = 3L,
                        increment = c(0L, 9L, 18L),
                        finished = FALSE)
    expect_identical(ans.obtained, ans.expected)
})

test_that("ComponentIterator works with InternalMovementsPool", {
    CohortIterator <- demest:::CohortIterator
    InternalMovements <- dembase:::InternalMovements
    internal <- Counts(array(1L,
                             dim = c(3, 3, 2, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 region = 1:3,
                                 time = c("2001-2005", "2006-2010"),
                                 triangle = c("TL", "TU"),
                                 direction = c("Out", "In"))))
    internal <- Pool(internal, direction = "direction", between = "region")
    template <- Counts(array(0L,
                             dim = c(3, 3, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 region = 1:3,
                                 time = c("2001-2005", "2006-2010"),
                                 triangle = c("TL", "TU"))))
    set.seed(1)
    internal <- InternalMovements(internal = internal,
                                  template = template)
    ans.obtained <- CohortIterator(internal)
    ans.expected <- new("CohortIteratorOrigDestParChPool",
                        i = 1L,
                        nTime = 2L,
                        stepTime = 9L,
                        iTime = 1L,
                        hasAge = TRUE,
                        nAge = 3L,
                        stepAge = 1L,
                        iAge = 1L,
                        stepTriangle = 18L,
                        iTriangle = 1L,
                        iVec = c(1L, 37L),
                        lengthVec = 2L,
                        increment = c(0L, 36L),
                        finished = FALSE)
    expect_identical(ans.obtained, ans.expected)
})

test_that("ComponentIterator works with ordinary component", {
    CohortIterator <- demest:::CohortIterator
    EntriesMovements <- dembase:::EntriesMovements
    entries <- Counts(array(1L,
                            dim = c(3, 3, 2),
                            dimnames = list(age = c("0-4", "5-9", "10+"),
                                region = 1:3,
                                time = c("2001-2005", "2006-2010"))))
    template <- Counts(array(0L,
                             dim = c(3, 3, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                 region = 1:3,
                                 time = c("2001-2005", "2006-2010"),
                                 triangle = c("TL", "TU"))))
    set.seed(1)
    component <- EntriesMovements(entries = entries,
                                  template = template,
                                  name = "immigration")
    ans.obtained <- CohortIterator(component)
    ans.expected <- new("CohortIteratorComponent",
                        i = 1L,
                        nTime = 2L,
                        stepTime = 9L,
                        iTime = 1L,
                        hasAge = TRUE,
                        nAge = 3L,
                        stepAge = 1L,
                        iAge = 1L,
                        stepTriangle = 18L,
                        iTriangle = 1L,
                        finished = FALSE)
    expect_identical(ans.obtained, ans.expected)
})

test_that("ComponentIterator works with population", {
    CohortIterator <- demest:::CohortIterator
    Population <- dembase:::Population
    population <- Counts(array(1L,
                            dim = c(3, 3, 3),
                            dimnames = list(age = c("0-4", "5-9", "10+"),
                                region = 1:3,
                                time = c(2000, 2005, 2010))))
    set.seed(1)
    population <- Population(population)
    ans.obtained <- CohortIterator(population)
    ans.expected <- new("CohortIteratorAccessionPopulation",
                        i = 1L,
                        nTime = 3L,
                        stepTime = 9L,
                        iTime = 1L,
                        hasAge = TRUE,
                        nAge = 3L,
                        stepAge = 1L,
                        iAge = 1L,
                        finished = FALSE)
    expect_identical(ans.obtained, ans.expected)
})

test_that("DimIterator creates objects from valid inputs", {
    DimIterator <- demest:::DimIterator
    x <- DimIterator(dim = 4:2, i = 1L)
    expect_identical(x,
                     new("DimIterator",
                         nStrides = -3L,
                         nWithin = 1L,
                         nBetween = 4L))
    x <- DimIterator(dim = 4:2, i = 2L)
    expect_identical(x,
                     new("DimIterator",
                         nStrides = -2L,
                         nWithin = 4L,
                         nBetween = 3L))
    x <- DimIterator(dim = 4:2, i = 3L)
    expect_identical(x,
                     new("DimIterator",
                         nStrides = -1L,
                         nWithin = 12L,
                         nBetween = 2L))
})

test_that("MarginIterator creates objects from valid inputs", {
    MarginIterator <- demest:::MarginIterator
    DimIterator <- demest:::DimIterator
    x <- MarginIterator(dim = 4:2)
    expect_identical(x,
                     new("MarginIterator",
                         indices = c(1L, 1L, 1L),
                         dimIterators = mapply(DimIterator,
                         dim = list(4:2), i = 1:3)))
    x <- MarginIterator(dim = 4L)
    expect_identical(x,
                     new("MarginIterator",
                         indices = 1L,
                         dimIterators = list(DimIterator(dim = 4L, i = 1L))))
})



test_that("SliceIterator creates objects from valid inputs", {
    SliceIterator <- demest:::SliceIterator
    ans.obtained <- SliceIterator(dim = c(3L, 2L, 2L),
                                 iAlong = 1L)
    ans.expected <- new("SliceIterator",
                        indices = c(1L, 4L, 7L, 10L),
                        increment = 1L,
                        posDim = 1L,
                        lengthDim = 3L)
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- SliceIterator(dim = c(3L, 2L, 2L),
                                 iAlong = 2L)
    ans.expected <- new("SliceIterator",
                        indices = c(1:3, 7:9),
                        increment = 3L,
                        posDim = 1L,
                        lengthDim = 2L)
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- SliceIterator(dim = c(3L, 2L, 2L),
                                 iAlong = 3L)
    ans.expected <- new("SliceIterator",
                        indices = 1:6,
                        increment = 6L,
                        posDim = 1L,
                        lengthDim = 2L)
    expect_identical(ans.obtained, ans.expected)
    ans.obtained <- SliceIterator(dim = 5L,
                                  iAlong = 1L)
    ans.expected <- new("SliceIterator",
                        indices = 1L,
                        increment = 1L,
                        posDim = 1L,
                        lengthDim = 5L)
    expect_identical(ans.obtained, ans.expected)
})
  


