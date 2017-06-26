

addInfantIndicator <- function(Z, metadata) {
    infant <- makeInfantIndicator(metadata)
    colnames <- make.unique(colnames(Z, "infant"))
    Z <- cbind(Z, infant)
    colnames(Z) <- colnames
    Z
}


makeInfantIndicator <- function(metadata) {
    dimtypes <- dimtypes(metadata,
                         use.names = FALSE)
    DimScales <- DimScales(metadata,
                           use.names = FALSE)
    i.age <- match("age", dimtypes, nomatch = 0L)
    has.age <- i.age > 0L
    if (!has.age)
        stop(gettextf("can't make '%s' indicator variable, because no dimension with %s \"%s\""
                      "infant", "dimtype", "age"))
    DS.age <- DimScales[[i.age]]
    if (!methods::is(DS.age, "Intervals"))
        stop(gettextf("can't make '%s' indicator variable, because dimension with %s \"%s\" has %s \"%s\""
                      "infant", "dimtype", "age", "dimscale", class(DS.age)))
    n.age <- length(DS.age)
    if (length(n.age) < 2L)
        stop(gettextf("can't make '%s' indicator variable, because dimension with %s \"%s\" has length %d"
                      "infant", "dimtype", "age", n.age))
    dv.age <- DS.age@dimvalues
    if (!isTRUE(all.equal(dv.age[1:2], 0:1)))
        stop(gettextf("can't make '%s' indicator variable, because first age group for dimension with %s \"%s\" is not \"%d\""
                      "infant", "dimtype", "age", 0L))
    ans <- c(1L, rep(0L, times = n.age - 1L))
    ans <- ans - mean(ans)
    ans
}
