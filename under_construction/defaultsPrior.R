

## need to have Cross default for orig-dest or parent-child
defaultPriorOuter <- function(beta, metadata, default = defaultStandard) {
    dim <- dim(metadata)
    names <- names(metadata)
    dimtypes <- dembase::dimtypes(metadata, use.names = FALSE)
    n.beta <- length(beta)
    name.term <- paste(names, collapse = ":")
    if (n.beta < 2L)
        stop(gettextf("'%s' for \"%s\" has length %d",
                      "beta", name.term, n.beta))
    if (n.beta != prod(dim))
        stop(gettextf("length of '%s' for \"%s\" [%d] not equal to product of dimensions [%d]",
                      "beta", name.term, n.beta, prod(dim)))
    if (!is.function(default))
        stop(gettextf("'%s' has class \"%s\"",
                      "default", class(default)))
    if (!identical(formalArgs(default), c("beta", "names", "dimtypes")))
        stop(gettextf("formal arguments for function '%s' are not \"%s\", \"%s\", and \"%s\"",
                      "default", "beta", "names", "dimtypes"))
    default(beta = beta, names = names, dimtypes = dimtypes)
}

defaultStandard <- function(beta, names, dimtypes) {
    n.beta <- length(beta)
    n.dimtypes <- length(dimtypes)
    if (n.beta <= 2L)
        ExchFixed()
    else {
        is.main.effect <- n.dimtypes == 1L
        i.time <- match("time", dimtypes, nomatch = 0L)
        has.time <- i.time > 0L
        if (is.main.effect) {
            if (has.time)
                DLM()
            else
                Exch()
        }
        else {
            if (has.time) {
                along <- names[i.time]
                DLM(along = along, trend = NULL)
            }
            else
                Exch()
        }
    }
}

defaultANOVA <- function(beta, names, dimtypes) {
    ExchFixed(multiplier = 0.5)
}

defaultFertility <- function(beta, names, dimtypes) {
    n.beta <- length(beta)
    n.dimtypes <- length(dimtypes)
    if (n.beta <= 2L)
        ExchFixed()
    else {
        is.main.effect <- n.dimtypes == 1L
        i.age <- match("age", dimtypes, nomatch = 0L)
        has.age <- i.age > 0L
        i.time <- match("time", dimtypes, nomatch = 0L)
        has.time <- i.time > 0L
        if (is.main.effect) {
            if (has.age) {
                if (n.beta < 10)
                    DLM(trend = NULL, damp = NULL)
                else
                    DLM(damp = NULL)
            }
            else if (has.time) {
                DLM()
            }
            else
                Exch()
        }
        else {
            if (has.time) {
                along <- names[i.time]
                DLM(along = along, trend = NULL)
            }
            else
                Exch()
        }
    }
}




                
