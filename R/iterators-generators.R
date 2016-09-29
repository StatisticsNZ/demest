

## Generators for iterators

## Assume arguments correct because (i) functions only ever
## called internally, and (ii) validity tests for objects
## created should pick up any errors.

## HAS_TESTS
AlongIterator <- function(dim, iAlong) {
    n.dim <- length(dim)
    n.along <- dim[iAlong]
    if (iAlong > 1L) {
        s.before <- seq.int(from = 1L, to = iAlong - 1L)
        n.within <- prod(dim[s.before])
        n.within <- as.integer(n.within)
    }
    else
        n.within <- 1L
    if (iAlong < n.dim) {
        s.after <- seq.int(from = iAlong + 1L, to = n.dim)
        n.between <- prod(dim[s.after])
        n.between <- as.integer(n.between)
    }
    else
        n.between <- 1L
    initial <- seq.int(from = 1L, by = n.within, length.out = n.along)
    increment.between <- n.within * (n.along - 1L) + 1L
    methods::new("AlongIterator",
        indices = initial,
        nWithin = n.within,
        nBetween = n.between,
        initial = initial,
        incrementBetween = increment.between)
}

## HAS_TESTS
BetaIterator <- function(dim, margins) {
    n.beta <- length(margins)
    indices <- rep(1L, times = n.beta)
    dim.used <- sort(unique(unlist(margins)))[-1L] ## omit 0
    n.dim.used <- length(dim.used)
    dim.iterators <- vector(mode = "list", length = n.dim.used)
    for (d in seq_len(n.dim.used))
        dim.iterators[[d]] <- DimIterator(dim = dim, i = dim.used[d])
    stride.lengths <- replicate(n = n.beta - 1L,
                                rep(0L, times = n.dim.used),
                                simplify = FALSE)
    if (n.beta > 1L) {
        for (b in seq.int(from = 2L, to = n.beta)) {
            margin <- margins[[b]]
            ## allow for possibility that dimensions in 'margin'
            ## are out of order (eg margin is 3:2)
            stride.length <- 1L
            for (m in margin) {
                stride.lengths[[b - 1L]][match(m, dim.used)] <- stride.length
                stride.length <- as.integer(stride.length * dim[m])
            }
        }
    }
    methods::new("BetaIterator",
        indices = indices,
        strideLengths = stride.lengths,
        dimIterators = dim.iterators)
}

## HAS_TESTS
setMethod("CohortIterator",
          signature(object = "Accession"),
          function(object) {
              dim <- dim(object)
              dimtypes <- dembase::dimtypes(object, use.names = FALSE)
              i.time <- match("time", dimtypes)
              i.age <- match("age", dimtypes)
              makeIteratorCAP(dim = dim,
                              iTime = i.time,
                              iAge = i.age)
          })

## HAS_TESTS
setMethod("CohortIterator",
          signature(object = "BirthsMovementsHasParentChild"),
          function(object) {
              dim <- dim(object)
              dimtypes <- dembase::dimtypes(object, use.names = FALSE)
              i.time <- match("time", dimtypes)
              i.age <- match("age", dimtypes, nomatch = 0L)
              i.triangle <- match("triangle", dimtypes, nomatch = 0L)
              i.dest <- grep("child", dimtypes)
              makeIteratorCODPCP(dim = dim,
                               iTime = i.time,
                               iAge = i.age,
                               iTriangle = i.triangle,
                               iMultiple = i.dest)
          })

## HAS_TESTS
setMethod("CohortIterator",
          signature(object = "Component"),
          function(object) {
              dim <- dim(object)
              dimtypes <- dembase::dimtypes(object, use.names = FALSE)
              i.time <- match("time", dimtypes)
              i.age <- match("age", dimtypes, nomatch = 0L)
              i.triangle <- match("triangle", dimtypes, nomatch = 0L)
              makeIteratorCC(dim = dim,
                             iTime = i.time,
                             iAge = i.age,
                             iTriangle = i.triangle)
          })

## HAS_TESTS
setMethod("CohortIterator",
          signature(object = "InternalMovementsOrigDest"),
          function(object) {
              dim <- dim(object)
              dimtypes <- dembase::dimtypes(object, use.names = FALSE)
              i.time <- match("time", dimtypes)
              i.age <- match("age", dimtypes, nomatch = 0L)
              i.triangle <- match("triangle", dimtypes, nomatch = 0L)
              i.dest <- grep("destination", dimtypes)
              makeIteratorCODPCP(dim = dim,
                                 iTime = i.time,
                                 iAge = i.age,
                                 iTriangle = i.triangle,
                                 iMultiple = i.dest)
          })

## HAS_TESTS
setMethod("CohortIterator",
          signature(object = "InternalMovementsPool"),
          function(object) {
              i.direction = object@iDirection
              dim <- dim(object)
              dimtypes <- dembase::dimtypes(object, use.names = FALSE)
              i.time <- match("time", dimtypes)
              i.age <- match("age", dimtypes, nomatch = 0L)
              i.triangle <- match("triangle", dimtypes, nomatch = 0L)
              makeIteratorCODPCP(dim = dim,
                                 iTime = i.time,
                                 iAge = i.age,
                                 iTriangle = i.triangle,
                                 iMultiple = i.direction)
          })

## HAS_TESTS
setMethod("CohortIterator",
          signature(object = "Population"),
          function(object) {
              dim <- dim(object)
              dimtypes <- dembase::dimtypes(object, use.names = FALSE)
              i.time <- match("time", dimtypes)
              i.age <- match("age", dimtypes, nomatch = 0L)
              makeIteratorCAP(dim = dim, iTime = i.time, iAge = i.age)
          })

## HAS_TESTS
DimIterator <- function(dim, i) {
    n.dim <- length(dim)
    if (i > 1L) {
        s.before <- seq.int(from = 1L, to = i - 1L)
        n.within <- prod(dim[s.before])
        n.within <- as.integer(n.within)
    }
    else
        n.within <- 1L
    n.between <- dim[i]
    n.strides <- 1L - n.between
    methods::new("DimIterator",
        nStrides = n.strides,
        nWithin = n.within,
        nBetween = n.between)
}

## HAS_TESTS
MarginIterator <- function(dim) {
    n.dim <- length(dim)
    indices <- rep(1L, times = n.dim)
    dimIterators <- vector(mode = "list", length = n.dim)
    for (i in seq_along(dimIterators))
        dimIterators[[i]] <- DimIterator(dim = dim, i = i)
    methods::new("MarginIterator",
        indices = indices,
        dimIterators = dimIterators)
}

