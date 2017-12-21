


             
SkeletonManyValuesStrucZero <- function(object, metadata, first, strucZeroArray) {

    


             

fetchMCMC <- function(filename, where = NULL, sample = NULL, thinned = TRUE) {
    object <- fetchResultsObject(filename)
    if (!methods::is(object, "Results"))
        stop(gettextf("results object has class \"%s\"",
                      class(object)))
    if (!identical(length(thinned), 1L))
        stop(gettextf("'%s' does not have length %d",
                      "thinned", 1L))
    if (!is.logical(thinned))
        stop(gettextf("'%s' does not have type \"%s\"",
                      "thinned", "logical"))
    if (is.na(thinned))
        stop(gettextf("'%s' is missing",
                      "thinned"))
    mcmc.args <- fetch(filename, where = "mcmc")
    n.chain <- mcmc.args[["nChain"]]
    if (thinned)
        n.thin <- 1L
    else
        n.thin <- mcmc.args[["nThin"]]
    if (is.null(where)) {
        where.mcmc <- whereMetropStat(object, whereEstimated)
        n.where <- length(where.mcmc)
        if (n.where == 0L)
            NULL
        else {
            ans <- vector(mode = "list", length = n.where)
            for (i in seq_len(n.where)) {
                where <- where.mcmc[[i]]
                obj <- fetch(filename, where = where)
                skeleton <- fetchSkeleton(filename, where = where)
                ans[[i]] <- MCMCDemographic(object = obj,
                                            sample = sample,
                                            nChain = n.chain,
                                            nThin = n.thin,
                                            skeleton = skeleton)
            }
            names(ans) <- sapply(where.mcmc, paste, collapse = ".")
            ans
        }
    }
    else {
        obj <- fetch(filename, where = where)
        if (!methods::is(obj, "DemographicArray"))
            stop(gettextf("'%s' has class \"%s\"",
                          where[length(where)], class(obj)))
        if (!("iteration" %in% dembase::dimtypes(obj)))
            stop(gettextf("'%s' does not have dimension with dimtype \"%s\"",
                          where[length(where)], "iteration"))
        MCMCDemographic(object = obj,
                        sample = sample,
                        nChain = n.chain,
                        nThin = n.thin)
    }
}


## HAS_TESTS
MCMCDemographic <- function(object, sample = NULL, nChain, nThin = 1L, skeleton = NULL) {
    if (!methods::is(object, "DemographicArray"))
        stop(gettextf("'%s' has class \"%s\"",
                      "object", class(object)))
    kDefaultSize <- 25L
    .Data <- object@.Data
    dim <- dim(object)
    n.dim <- length(dim)
    i.iter <- match("iteration", dembase::dimtypes(object), nomatch = 0L)
    if (identical(i.iter, 0L))
        stop(gettextf("no dimension with dimtype \"%s\"", "iteration"))
    n.data <- length(.Data)
    n.iter <- dim[i.iter]
    n.slice <- n.data / n.iter  ## number of values in one iteration
    s <- seq_len(n.slice)
    if (is.null(sample)) {
        indices.struc.zero <- getIndicesStrucZero(skeleton)
        s <- setdiff(s, indices.struc.zero)
        n.s <- length(s)
        if ((n.s == 1L) || (kDefaultSize >= n.s))
            sample <- s
        else
            sample <- sample(s, size = kDefaultSize, replace = FALSE)
        sample <- sort(sample)
    }
    else {
        if (!all(sample %in% s))
            stop(gettextf("'%s' has values outside the valid range", "sample"))
    }
    for (name in c("nChain", "nThin")) {
        value <- get(name)
        if (!is.numeric(value))
            stop(gettextf("'%s' does not have type \"%s\"", name, "numeric"))
        if (!identical(length(value), 1L))
            stop(gettextf("'%s' does not have length %d", name, 1L))
        if (is.na(value))
            stop(gettextf("'%s' is missing", name))
        if (value < 1L)
            stop(gettextf("'%s' is less than %d", name, 1L))
        assign(name, as.integer(value))
    }
    if (!identical(n.iter %% nChain, 0L))
        stop(gettextf("number of iterations is not divisible by '%s'", "nChain"))
    if (!identical(i.iter, n.dim)) {
        s <- seq_len(n.dim)
        .Data <- aperm(.Data, perm = c(s[-i.iter], i.iter))
    }
    .Data <- array(.Data, dim = c(n.slice, n.iter / nChain, nChain))
    .Data <- .Data[sample, , , drop = FALSE]
    makeColnames <- function(x, y) outer(x, y, FUN = paste, sep = ".")
    colnames <- Reduce(makeColnames, dimnames(object)[-i.iter])
    colnames <- colnames[sample]
    makeChainIntoMCMC <- function(i) {
        chain <- matrix(.Data[ , , i], nrow = nrow(.Data))
        chain <- t(chain)
        colnames(chain) <- colnames
        coda::mcmc(chain, thin = nThin)
    }
    l <- lapply(seq_len(nChain), makeChainIntoMCMC)
    coda::mcmc.list(l)
}

          
             
         



    





