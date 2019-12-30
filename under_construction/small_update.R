
## HAS_TESTS
## READY_TO_TRANSLATE
## like chooseICellComp, but restricted to upper Lexis triangles
chooseICellCompUpperTri <- function(description, useC = FALSE) {
    stopifnot(methods::is(description, "DescriptionComp"))
    stopifnot(description@hasAge) # implies has triangles
    if (useC) {
        .Call(chooseICellCompUpperTri_R, description)
    }
    else {
        length <- description@length
        step <- description@stepTriangle
        half_length <- description@length / 2L
        i <- as.integer(stats::runif(n = 1L) * half_length) # C-style
        if (i == half_length) # just in case
            i <- half_length - 1L
        i <- (i %/% step) * (2L * step) + (i %% step) + step
        i <- i + 1L # R-style
        i
    }
}

    
test_that("chooseICellCompUpperTri works", {
    chooseICellCompUpperTri <- demest:::chooseICellCompUpperTri
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
    x <- replicate(n = 10, chooseICellCompUpperTri(description))
    expect_true(all(x %in% 7:12))
    object <- Counts(array(1:12,
                           dim = c(3, 2, 2),
                           dimnames = list(time = c("2001-2010", "2011-2020", "2021-2030"),
                                           triangle = c("Lower", "Upper"),
                                           age = c("0-9", "10+"))))
    object <- new("EntriesMovements",
                  .Data = object@.Data,
                  metadata = object@metadata)
    description <- Description(object)
    x <- replicate(n = 10, chooseICellCompUpperTri(description))
    expect_true(all(x %in% c(4:6, 10:12)))
    object <- Counts(array(1:12,
                           dim = c(2, 3, 2),
                           dimnames = list(triangle = c("Lower", "Upper"),
                                           time = c("2001-2010", "2011-2020", "2021-2030"),
                                           age = c("0-9", "10+"))))
    object <- new("EntriesMovements",
                  .Data = object@.Data,
                  metadata = object@metadata)
    description <- Description(object)
    x <- replicate(n = 10, chooseICellCompUpperTri(description))
    expect_true(all(x %in% seq(2, 12, 2)))
})

test_that("R and C versions of chooseICellCompUpperTri give same answer", {
    chooseICellCompUpperTri <- demest:::chooseICellCompUpperTri
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
    x <- replicate(n = 10, chooseICellCompUpperTri(description))
    ans.R <- replicate(n = 10, chooseICellCompUpperTri(description, useC = FALSE))
    ans.C <- replicate(n = 10, chooseICellCompUpperTri(description, useC = TRUE))
    expect_identical(ans.R, ans.C)
    object <- Counts(array(1:12,
                           dim = c(3, 2, 2),
                           dimnames = list(time = c("2001-2010", "2011-2020", "2021-2030"),
                                           triangle = c("Lower", "Upper"),
                                           age = c("0-9", "10+"))))
    object <- new("EntriesMovements",
                  .Data = object@.Data,
                  metadata = object@metadata)
    description <- Description(object)
    ans.R <- replicate(n = 10, chooseICellCompUpperTri(description, useC = FALSE))
    ans.C <- replicate(n = 10, chooseICellCompUpperTri(description, useC = TRUE))
    expect_identical(ans.R, ans.C)
    object <- Counts(array(1:12,
                           dim = c(2, 3, 2),
                           dimnames = list(triangle = c("Lower", "Upper"),
                                           time = c("2001-2010", "2011-2020", "2021-2030"),
                                           age = c("0-9", "10+"))))
    object <- new("EntriesMovements",
                  .Data = object@.Data,
                  metadata = object@metadata)
    description <- Description(object)
    ans.R <- replicate(n = 10, chooseICellCompUpperTri(description, useC = FALSE))
    ans.C <- replicate(n = 10, chooseICellCompUpperTri(description, useC = TRUE))
    expect_identical(ans.R, ans.C)
})

## HAS_TESTS
## READY_TO_TRANSLATE
## get lower Lexis triangle cell from within same age-period square
## as triangle indexed by iCellUp
getICellLowerTriFromComp <- function(iCellUp, description, useC = FALSE) {
    stopifnot(methods::is(description, "DescriptionComp"))
    stopifnot(description@hasAge) # implies has triangles
    if (useC) {
        .Call(getICellLowerTriFromComp_R, description)
    }
    else {
        step <- description@stepTriangle
        iCellUp - step
    }
}

test_that("getICellLowerTriFromComp works", {
    getICellLowerTriFromComp <- demest:::getICellLowerTriFromComp
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
    upper <- 7:12
    lower <- 1:6
    for (i in seq_along(upper)) {
        expect_identical(getICellLowerTriFromComp(iCellUp = upper[i],
                                                  description),
                         lower[i])
    }
    object <- Counts(array(1:12,
                           dim = c(3, 2, 2),
                           dimnames = list(time = c("2001-2010", "2011-2020", "2021-2030"),
                                           triangle = c("Lower", "Upper"),
                                           age = c("0-9", "10+"))))
    object <- new("EntriesMovements",
                  .Data = object@.Data,
                  metadata = object@metadata)
    description <- Description(object)
    upper <- c(4:6, 10:12)
    lower <- c(1:3, 7:9)
    for (i in seq_along(upper)) {
        expect_identical(getICellLowerTriFromComp(iCellUp = upper[i],
                                                  description),
                         lower[i])
    }
    object <- Counts(array(1:12,
                           dim = c(2, 3, 2),
                           dimnames = list(triangle = c("Lower", "Upper"),
                                           time = c("2001-2010", "2011-2020", "2021-2030"),
                                           age = c("0-9", "10+"))))
    object <- new("EntriesMovements",
                  .Data = object@.Data,
                  metadata = object@metadata)
    description <- Description(object)
    upper <- seq.int(2L, 12L, 2L)
    lower <- seq.int(1L, 11L, 2L)
    for (i in seq_along(upper)) {
        expect_identical(getICellLowerTriFromComp(iCellUp = upper[i],
                                                  description),
                         lower[i])
    }
})


test_that("R and C versions of getICellLowerTriFromComp give same answer", {
    getICellLowerTriFromComp <- demest:::getICellLowerTriFromComp
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
    upper <- 7:12
    for (i in seq_along(upper)) {
        expect_identical(getICellLowerTriFromComp(iCellUp = upper[i],
                                                  description,
                                                  useC = FALSE),
                         getICellLowerTriFromComp(iCellUp = upper[i],
                                                  description,
                                                  useC = TRUE))
    }
    object <- Counts(array(1:12,
                           dim = c(3, 2, 2),
                           dimnames = list(time = c("2001-2010", "2011-2020", "2021-2030"),
                                           triangle = c("Lower", "Upper"),
                                           age = c("0-9", "10+"))))
    object <- new("EntriesMovements",
                  .Data = object@.Data,
                  metadata = object@metadata)
    description <- Description(object)
    upper <- c(4:6, 10:12)
    for (i in seq_along(upper)) {
        expect_identical(getICellLowerTriFromComp(iCellUp = upper[i],
                                                  description,
                                                  useC = FALSE),
                         getICellLowerTriFromComp(iCellUp = upper[i],
                                                  description,
                                                  useC = TRUE))
    }
    object <- Counts(array(1:12,
                           dim = c(2, 3, 2),
                           dimnames = list(triangle = c("Lower", "Upper"),
                                           time = c("2001-2010", "2011-2020", "2021-2030"),
                                           age = c("0-9", "10+"))))
    object <- new("EntriesMovements",
                  .Data = object@.Data,
                  metadata = object@metadata)
    description <- Description(object)
    upper <- seq.int(2L, 12L, 2L)
    for (i in seq_along(upper)) {
        expect_identical(getICellLowerTriFromComp(iCellUp = upper[i],
                                                  description,
                                                  useC = FALSE),
                         getICellLowerTriFromComp(iCellUp = upper[i],
                                                  description,
                                                  useC = TRUE))
    }
})


## HAS_TESTS
## READY_TO_TRANSLATE
## get lower Lexis triangle cell from next oldest age-period square,
## or same square if iCellUp refers to oldest age group
getICellLowerTriNextFromComp <- function(iCellUp, description, useC = FALSE) {
    stopifnot(methods::is(description, "DescriptionComp"))
    stopifnot(description@hasAge) # implies has triangles
    if (useC) {
        .Call(getICellLowerTriNextFromComp_R, description)
    }
    else {
        step.age <- description@stepAge
        n.age <- description@nAge
        step.triangle <- description@stepTriangle
        i.age <- ((iCellUp - 1L) %/% step.age) %% n.age ## C-style
        is.final.age.group <- i.age == (n.age - 1L)
        if (is.final.age.group)
            iCellUp - step.triangle
        else
            iCellUp - step.triangle + step.age
    }
}

test_that("getICellLowerTriNextFromComp works", {
    ## getICellLowerTriNextFromComp <- demest:::getICellLowerTriNextFromComp
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
    upper <- 7:12
    lower <- rep(4:6, times = 2)
    for (i in seq_along(upper)) {
        expect_identical(getICellLowerTriNextFromComp(iCellUp = upper[i],
                                                  description),
                         lower[i])
    }
    object <- Counts(array(1:12,
                           dim = c(3, 2, 2),
                           dimnames = list(time = c("2001-2010", "2011-2020", "2021-2030"),
                                           triangle = c("Lower", "Upper"),
                                           age = c("0-9", "10+"))))
    object <- new("EntriesMovements",
                  .Data = object@.Data,
                  metadata = object@metadata)
    description <- Description(object)
    upper <- c(4:6, 10:12)
    lower <- rep(7:9, times = 2)
    for (i in seq_along(upper)) {
        expect_identical(getICellLowerTriNextFromComp(iCellUp = upper[i],
                                                  description),
                         lower[i])
    }
    object <- Counts(array(1:12,
                           dim = c(2, 3, 2),
                           dimnames = list(triangle = c("Lower", "Upper"),
                                           time = c("2001-2010", "2011-2020", "2021-2030"),
                                           age = c("0-9", "10+"))))
    object <- new("EntriesMovements",
                  .Data = object@.Data,
                  metadata = object@metadata)
    description <- Description(object)
    upper <- seq.int(2L, 12L, 2L)
    lower <- rep(c(7L, 9L, 11L), times = 2)
    for (i in seq_along(upper)) {
        expect_identical(getICellLowerTriNextFromComp(iCellUp = upper[i],
                                                  description),
                         lower[i])
    }
})


test_that("R and C versions of getICellLowerTriNextFromComp give same answer", {
    getICellLowerTriNextFromComp <- demest:::getICellLowerTriNextFromComp
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
    upper <- 7:12
    for (i in seq_along(upper)) {
        expect_identical(getICellLowerTriNextFromComp(iCellUp = upper[i],
                                                      description,
                                                      useC = FALSE),
                         getICellLowerTriNextFromComp(iCellUp = upper[i],
                                                      description,
                                                      useC = TRUE))
    }
    object <- Counts(array(1:12,
                           dim = c(3, 2, 2),
                           dimnames = list(time = c("2001-2010", "2011-2020", "2021-2030"),
                                           triangle = c("Lower", "Upper"),
                                           age = c("0-9", "10+"))))
    object <- new("EntriesMovements",
                  .Data = object@.Data,
                  metadata = object@metadata)
    description <- Description(object)
    upper <- c(4:6, 10:12)
    for (i in seq_along(upper)) {
        expect_identical(getICellLowerTriNextFromComp(iCellUp = upper[i],
                                                      description,
                                                      useC = FALSE),
                         getICellLowerTriNextFromComp(iCellUp = upper[i],
                                                      description,
                                                      useC = TRUE))
    }
    object <- Counts(array(1:12,
                           dim = c(2, 3, 2),
                           dimnames = list(triangle = c("Lower", "Upper"),
                                           time = c("2001-2010", "2011-2020", "2021-2030"),
                                           age = c("0-9", "10+"))))
    object <- new("EntriesMovements",
                  .Data = object@.Data,
                  metadata = object@metadata)
    description <- Description(object)
    upper <- seq.int(2L, 12L, 2L)
    for (i in seq_along(upper)) {
        expect_identical(getICellLowerTriNextFromComp(iCellUp = upper[i],
                                                      description,
                                                      useC = FALSE),
                         getICellLowerTriNextFromComp(iCellUp = upper[i],
                                                      description,
                                                      useC = TRUE))
    }
})




## READY_TO_TRANSLATE
## HAS_TESTS
## If no lower, upper limit, 'lower', 'upper' NA_integer_
rbinomTrunc1 <- function(size, prob, lower, upper, maxAttempt, useC = FALSE) {
    ## size
    stopifnot(is.integer(size))
    stopifnot(identical(length(size), 1L))
    stopifnot(!is.na(size))
    stopifnot(size >= 0L)
    ## prob
    stopifnot(is.double(prob))
    stopifnot(identical(length(prob), 1L))
    stopifnot(!is.na(prob))
    stopifnot(prob >= 0)
    stopifnot(prob <= 1)
    ## lower
    stopifnot(is.integer(lower))
    stopifnot(identical(length(lower), 1L))
    ## upper
    stopifnot(is.integer(upper))
    stopifnot(identical(length(upper), 1L))
    ## maxAttempt
    stopifnot(is.integer(maxAttempt))
    stopifnot(identical(length(maxAttempt), 1L))
    stopifnot(!is.na(maxAttempt))
    stopifnot(maxAttempt > 0L)
    ## lower, upper
    stopifnot(is.na(lower) || is.na(upper) || (lower <= upper))
    if (useC) {
        .Call(rbinomTrunc1_R, size, prob, lower, upper, maxAttempt)
    }
    else {
        if (is.na(lower) || (lower < 0L))
            lower <- 0L
        if (is.na(upper) || (upper > size))
            upper <- size
        if (lower == upper)
            return(lower)
        found <- FALSE
        for (i in seq_len(maxAttempt)) {
            prop.value <- stats::rbinom(n = 1L,
                                        size = size,
                                        prob = prob)
            found <- (lower <= prop.value) && (prop.value <= upper)
            if (found)
                break
        }
        if (found)
            as.integer(prop.value)
        else
            NA_integer_
    }
}

test_that("rbinomTrunc1 gives valid answer", {
    rbinomTrunc1 <- demest:::rbinomTrunc1
    for (seed in seq_len(n.test)) {
        ## no limits
        set.seed(seed)
        size <- rpois(n = 1L, lambda = 10)
        prob <- runif(1)
        set.seed(seed + 1)
        ans.obtained <- rbinomTrunc1(size = size,
                                     prob = prob,
                                     lower = 0L,
                                     upper = NA_integer_,
                                     maxAttempt = 1L)
        set.seed(seed + 1)
        ans.expected <- rbinom(n = 1L,
                               size = size,
                               prob = prob)
        expect_identical(ans.obtained, ans.expected)
        ## within range
        ans <- rbinomTrunc1(size = size,
                            prob = prob,
                            lower = 2L,
                            upper = 10L,
                            maxAttempt = 100L)
        expect_true((is.na(ans)) || ((ans >= 2L) && ans <= 10L))
        ## returns 0 if upper = 0
        ans <- rbinomTrunc1(size = size,
                            prob = prob,
                            lower = -1L,
                            upper = 0L,
                            maxAttempt = 1L)
        expect_identical(ans, 0L)
        ## returns NA_integer_ if failed
        ans <- rbinomTrunc1(size = 100000L,
                            prob = 0.5,
                            lower = -1L,
                            upper = 1L,
                            maxAttempt = 1L)
        expect_identical(ans, NA_integer_)
        ## lower is NA gives same answer as lower is 0
        set.seed(seed + 1)
        ans.obtained <- rbinomTrunc1(size = size,
                                     prob = prob,
                                     lower = NA_integer_,
                                     upper = 100L, maxAttempt = 100L)
        set.seed(seed + 1)
        ans.expected <- rbinomTrunc1(size = size,
                                     prob = prob,
                                     lower = 0L,
                                     upper = 100L, maxAttempt = 100L)
        expect_identical(ans.obtained, ans.expected)
    }
})

test_that("R and C versions of rbinomTrunc1 give same answer", {
    rbinomTrunc1 <- demest:::rbinomTrunc1
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        size <- rpois(1, 10)
        prob <- runif(1)
        lower <- as.integer(rpois(n = 1, lambda = 5))
        if (runif(1) < 0.8)
            upper <- lower + as.integer(rpois(1, lambda = 10))
        else
            upper <- NA_integer_
        set.seed(seed + 1)
        ans.R <- rbinomTrunc1(size = size,
                              prob = prob,
                              lower = lower,
                              upper = upper,
                              maxAttempt = 10L,
                              useC = FALSE)
        set.seed(seed + 1)
        ans.C <- rbinomTrunc1(size = size,
                              prob = prob,
                              lower = lower,
                              upper = upper,
                              maxAttempt = 10L,
                              useC = TRUE)
        expect_identical(ans.R, ans.C)
        set.seed(seed + 1)
        ans.R <- rbinomTrunc1(size = size,
                              prob = prob,
                              lower = lower,
                              upper = upper,
                              maxAttempt = 10L,
                              useC = FALSE)
        set.seed(seed + 1)
        ans.C <- rbinomTrunc1(size = size,
                              prob = prob,
                              lower = lower,
                              upper = upper,
                              maxAttempt = 10L,
                              useC = TRUE)
        expect_identical(ans.R, ans.C)
    }
})

setMethod("updateProposalAccount",
          signature(object = "CombinedAccountMovements"),
          function(object, useC = FALSE, useSpecific = FALSE) {
              stopifnot(methods::validObject(object))
              if (useC) {
                  if (useSpecific)
                      .Call(updateProposalAccount_CombinedAccountMovements_R, object)
                  else
                      .Call(updateProposalAccount_R, object)
              }
              else {
                  account <- object@account
                  prob.popn <- object@probPopn
                  prob.small.update <- object@probSmallUpdate@.Data ## NEW
                  update.popn <- stats::runif(n = 1L) < prob.popn
                  if (update.popn) {
                      object@iComp <- 0L
                      updateProposalAccountMovePopn(object)
                  }
                  else {
                      cum.prob <- object@cumProbComp
                      i.births <- object@iBirths
                      i.orig.dest <- object@iOrigDest
                      i.pool <- object@iPool
                      i.int.net <- object@iIntNet
                      is.net.vec <- object@isNet ## NEW
                      prob.small.update <- object@probSmallUpdate ## NEW
                      i.comp <- rcateg1(cum.prob)
                      object@iComp <- i.comp
                      if (i.comp == i.births) {
                          is.small.update <- stats::runif(n = 1L) < prob.small.update ## NEW
                          if (is.small.update) ## NEW
                              updateProposalAccountMoveBirthsSmall(object) ## NEW
                          else ## NEW
                              updateProposalAccountMoveBirths(object)
                      } ## NEW
                      else if (i.comp == i.orig.dest) {
                          is.small.update <- stats::runif(n = 1L) < prob.small.update ## NEW
                          if (is.small.update) ## NEW
                              updateProposalAccountMoveOrigDestSmall(object) ## NEW
                          else ## NEW
                              updateProposalAccountMoveOrigDest(object) ## NEW
                      }
                      else if (i.comp == i.pool)
                          updateProposalAccountMovePool(object)
                      else if (i.comp == i.int.net)
                          updateProposalAccountMoveNet(object)
                      else { # comp
                          is.net <- is.net.vec[i.comp] ## NEW
                          is.small.update <- !is.net && (stats::runif(n = 1L) < prob.small.update) ## NEW
                          if (is.small.update) ## NEW
                              updateProposalAccountMoveCompSmall(object) ## NEW
                          else ## NEW
                              updateProposalAccountMoveComp(object)
                      }
                  }
              }
          })

setMethod("diffLogDensAccount",
          signature(combined = "CombinedAccountMovements"),
          function(combined, useC = FALSE, useSpecific = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(diffLogDensAccount_CombinedAccountMovements_R, combined)
                  else
                      .Call(diffLogDensAccount_R, combined)
              }
              else {
                  i.comp <- combined@iComp
                  i.orig.dest <- combined@iOrigDest
                  i.pool <- combined@iPool
                  i.int.net <- combined@iIntNet
                  is.small.update <- combined@isSmallUpdate ## NEW
                  model.uses.exposure <- combined@modelUsesExposure
                  is.popn <- i.comp == 0L
                  is.orig.dest <- i.comp == i.orig.dest
                  is.pool <- i.comp == i.pool
                  is.int.net <- i.comp == i.int.net
                  use.prior.popn <- combined@usePriorPopn@.Data
                  ans <- 0
                  if (use.prior.popn)
                      ans <- ans + diffLogDensPopn(combined)
                  if (is.popn)
                      ans <- ans + diffLogDensExpPopn(combined)
                  else if (is.orig.dest) {
                      if (is.small.update) ## NEW
                          ans <- ans + diffLogDensCompSmall(combined) ## NEW
                      else { ## NEW
                          if (model.uses.exposure[i.comp])
                              ans <- ans + diffLogDensJumpOrigDest(combined)
                          ans <- ans + diffLogDensExpOrigDestPoolNet(combined)
                      }
                  }
                  else if (is.pool) {
                      if (model.uses.exposure[i.comp])
                          ans <- ans + diffLogDensJumpPoolWithExpose(combined)
                      else
                          ans <- ans + diffLogDensJumpPoolNoExpose(combined)
                      ans <- ans + diffLogDensExpOrigDestPoolNet(combined)
                  }
                  else if (is.int.net) {
                      ans <- ans + diffLogDensJumpNet(combined)
                      ans <- ans + diffLogDensExpOrigDestPoolNet(combined)
                  }
                  else {
                      if (is.small.update) ## NEW
                          ans <- ans + diffLogDensCompSmall(combined) ## NEW
                      else { ## NEW
                          if (model.uses.exposure[i.comp])
                              ans <- ans + diffLogDensJumpComp(combined)
                          ans <- ans + diffLogDensExpComp(combined)
                      } ## NEW
                  }
                  ans
              }
          })


setMethod("diffLogLikAccount",
          signature(object = "CombinedAccountMovements"),
          function(object, useC = FALSE, useSpecific = FALSE) {
              if (useC) {
                  if (useSpecific)
                      .Call(diffLogLikAccount_CombinedAccountMovements_R, object)
                  else
                      .Call(diffLogLikAccount_R, object)
              }
              else {
                  i.comp <- object@iComp
                  i.orig.dest <- object@iOrigDest
                  i.pool <- object@iPool
                  i.int.net <- object@iIntNet
                  is.small.update <- object@isSmallUpdate ## NEW
                  if (i.comp == 0L)
                      diffLogLikAccountMovePopn(object)
                  else if (i.comp == i.orig.dest) { ## NEW
                      if (is.small.update) ## NEW
                          diffLogLikAccountMoveCompSmall(object) ## NEW
                      else ## NEW
                          diffLogLikAccountMoveOrigDest(object)
                  } ## NEW
                  else if (i.comp == i.pool) 
                      diffLogLikAccountMovePool(object)
                  else if (i.comp == i.int.net) 
                      diffLogLikAccountMoveNet(object)
                  else { ## NEW
                      if (is.small.update) ## NEW
                          diffLogLikAccountMoveCompSmall(object) ## NEW
                      else ## NEW
                          diffLogLikAccountMoveComp(object)
                  } ## NEW
              }
          })



setMethod("updateValuesAccount",
          signature(combined = "CombinedAccountMovements"),
          function(combined, useC = FALSE, useSpecific = FALSE) {
              stopifnot(methods::validObject(combined))
              if (useC) {
                  if (useSpecific)
                      .Call(updateValuesAccount_CombinedAccountMovements_R, combined)
                  else
                      .Call(updateValuesAccount_R, combined)
              }
              else {
                  has.age <- combined@hasAge
                  is.small.update <- combined@isSmallUpdate ## NEW
                  combined <- updateCellMove(combined)
                  if (is.small.update) ## NEW
                      combined <- updateAccSmall(combined)
                  else { ## NEW
                      combined <- updateSubsequentPopnMove(combined)
                      combined <- updateSubsequentExpMove(combined)
                      if (has.age)
                          combined <- updateSubsequentAccMove(combined)
                  } ## NEW
                  combined
              }
          })



## READY_TO_TRANSLATE
## HAS_TESTS
## Assume has age. Note that accession irrelevant,
## since accession applies to cohort being born,
## and total number of births does not change
updateProposalAccountMoveBirthsSmall <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(updateProposalAccountMoveBirthsSmall_R, combined)
    }
    else {
        account <- combined@account
        population <- account@population
        i.comp <- combined@iComp
        component <- account@components[[i.comp]]
        max.attempt <- combined@maxAttempt
        uses.exposure <- combined@modelUsesExposure[i.comp + 1L]
        mapping.to.exp <- combined@mappingsToExp[[i.comp]]
        description <- combined@descriptions[[i.comp + 1L]]
        sys.mod.comp <- combined@systemModels[[i.comp + 1L]]
        theta <- sys.mod.comp@theta
        struc.zero.array <- sys.mod.comp@strucZeroArray
        generated.new.proposal <- FALSE
        for (i in seq_len(max.attempt)) {
            i.cell.up <- chooseICellCompUpperTri(description)
            is.struc.zero <- struc.zero.array[i.cell.up] == 0L
            if (!is.struc.zero) {
                generated.new.proposal <- TRUE
                break
            }
        }
        if (generated.new.proposal) {
            ## i.cell.low is lower Lexis triangle within same
            ## age-period square as i.cell.up
            i.cell.low <- getICellLowerTriFromComp(iCellUp = i.cell.up,
                                                   description = description)
            val.up.curr <- component[i.cell.up]
            val.low.curr <- component[i.cell.low]
            val.up.expected <- theta[i.cell.up]
            val.low.expected <- theta[i.cell.low]
            if (uses.exposure) {
                exposure <- combined@exposure
                i.expose.up <- getIExposureFromBirths(i = i.cell.up,
                                                      mapping = mapping.to.exp)
                i.expose.low <- getIExposureFromBirths(i = i.cell.low,
                                                       mapping = mapping.to.exp)
                expose.up <- exposure[i.expose.up]
                expose.low <- exposure[i.expose.low]
                val.up.expected <- val.up.expected * expose.up
                val.low.expected <- val.low.expected * expose.low
            }
            prob <- val.up.expected / (val.up.expected + val.low.expected)
            size <- val.up.curr + val.low.curr
            val.up.prop <- rbinom(n = 1L,
                                  size = size,
                                  prob = prob)
            val.low.prop <- size - val.up.prop
            diff.prop <- unname(val.up.prop - val.up.curr)
            generated.new.proposal  <- diff.prop != 0L
        }
        if (generated.new.proposal) {
            combined@generatedNewProposal@.Data <- TRUE
            combined@isSmallUpdate@.Data <- TRUE
            combined@iCell <- i.cell.up
            combined@iCellOther <- i.cell.low
            combined@iPopnNext <- NA_integer_
            combined@iPopnNextOther <- NA_integer_
            combined@iAccNext <- 0L
            combined@iAccNextOther <- NA_integer_
            combined@isLowerTriangle@.Data <- FALSE
            if (uses.exposure) {
                combined@iExposure <- i.expose.up
                combined@iExposureOther <- i.expose.low
            }
            else {
                combined@iExposure <- 0L
                combined@iExposureOther <- NA_integer_
            }
            combined@iExpFirst <- NA_integer_
            combined@iExpFirstOther <- NA_integer_
            combined@diffProp <- diff.prop
        }
        else {
            combined@generatedNewProposal@.Data <- FALSE
            combined@isSmallUpdate@.Data <- TRUE
            combined@iCell <- NA_integer_
            combined@iCellOther <- NA_integer_
            combined@iPopnNext <- NA_integer_
            combined@iPopnNextOther <- NA_integer_
            combined@iAccNext <- NA_integer_
            combined@iAccNextOther <- NA_integer_
            combined@isLowerTriangle@.Data <- NA
            combined@iExposure <- NA_integer_
            combined@iExposureOther <- NA_integer_
            combined@iExpFirst <- NA_integer_
            combined@iExpFirstOther <- NA_integer_
            combined@diffProp <- NA_integer_
        }
        combined
    }
}


test_that("updateProposalAccountMoveBirthsSmall works with CombinedAccountMovementsHasAge", {
    ## updateProposalAccountMoveBirthsSmall <- demest:::updateProposalAccountMoveBirthsSmall
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    popn <- Counts(array(rpois(n = 90, lambda = 100),
                         dim = c(3, 2, 5, 3),
                         dimnames = list(age = c("0-4", "5-9", "10+"),
                                         sex = c("f", "m"),
                                         reg = 1:5,
                                         time = c(2000, 2005, 2010))))
    births <- Counts(array(rpois(n = 90, lambda = 5),
                           dim = c(1, 2, 5, 2, 2),
                           dimnames = list(age = "5-9",
                                           sex = c("m", "f"),
                                           reg = 1:5,
                                           time = c("2001-2005", "2006-2010"),
                                           triangle = c("Lower", "Upper"))))
    internal <- Counts(array(rpois(n = 300, lambda = 10),
                             dim = c(3, 2, 5, 5, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                             sex = c("m", "f"),
                                             reg_orig = 1:5,
                                             reg_dest = 1:5,
                                             time = c("2001-2005", "2006-2010"),
                                             triangle = c("Lower", "Upper"))))
    deaths <- Counts(array(rpois(n = 72, lambda = 10),
                           dim = c(3, 2, 5, 2, 2),
                           dimnames = list(age = c("0-4", "5-9", "10+"),
                                           sex = c("m", "f"),
                                           reg = 5:1,
                                           time = c("2001-2005", "2006-2010"),
                                           triangle = c("Lower", "Upper"))))
    account <- Movements(population = popn,
                         births = births,
                         internal = internal,
                         exits = list(deaths = deaths))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ age + sex, useExpose = FALSE)),
                         Model(births ~ Poisson(mean ~ 1)),
                         Model(internal ~ Poisson(mean ~ reg_orig + reg_dest)),
                         Model(deaths ~ Poisson(mean ~ 1)))
    systemWeights <- list(NULL, NULL, NULL, NULL)
    census <- subarray(popn, time == "2000", drop = FALSE) + 2L
    register <- Counts(array(rpois(n = 90, lambda = popn),
                             dim = dim(popn),
                             dimnames = dimnames(popn)))
    reg.births <- Counts(array(rbinom(n = 90, size = births, prob = 0.98),
                               dim = dim(births),
                               dimnames = dimnames(births)))
    address.change <- Counts(array(rpois(n = 300, lambda = internal),
                                   dim = dim(internal),
                                   dimnames = dimnames(internal)))
    reg.deaths <- Counts(array(rbinom(n = 90, size = deaths, prob = 0.98),
                               dim = dim(deaths),
                               dimnames = dimnames(deaths))) + 1L
    datasets <- list(census, register, reg.births, address.change, reg.deaths)
    namesDatasets <- c("census", "register", "reg.births", "address.change", "reg.deaths")
    data.models <- list(Model(census ~ PoissonBinomial(prob = 0.95), series = "population"),
                              Model(register ~ Poisson(mean ~ 1), series = "population"),
                              Model(reg.births ~ PoissonBinomial(prob = 0.98), series = "births"),
                              Model(address.change ~ Poisson(mean ~ 1), series = "internal"),
                              Model(reg.deaths ~ PoissonBinomial(prob = 0.98), series = "deaths"))
    seriesIndices <- c(0L, 0L, 1L, 2L, 3L)
    updateInitialPopn <- new("LogicalFlag", TRUE)
    usePriorPopn <- new("LogicalFlag", TRUE)
    transforms <- list(makeTransform(x = population(account), y = datasets[[1]], subset = TRUE),
                       makeTransform(x = population(account), y = datasets[[2]], subset = TRUE),
                       makeTransform(x = components(account, "births"), y = datasets[[3]], subset = TRUE),
                       makeTransform(x = components(account, "internal"), y = datasets[[4]], subset = TRUE),
                       makeTransform(x = components(account, "deaths"), y = datasets[[5]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x0 <- initialCombinedAccount(account = account,
                                 systemModels = systemModels,
                                 systemWeights = systemWeights,
                                 dataModels = data.models,
                                 seriesIndices = seriesIndices,
                                 updateInitialPopn = updateInitialPopn,
                                 usePriorPopn = usePriorPopn,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    expect_true(validObject(x0))
    expect_is(x0, "CombinedAccountMovementsHasAge")
    x0@iComp <- 1L
    updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x1 <- updateProposalAccountMoveBirthsSmall(x0)
        if (x1@generatedNewProposal@.Data) {
            updated <- TRUE
            expect_false(x1@diffProp == 0L)
        }
        expect_is(x1, "CombinedAccountMovementsHasAge")
        expect_true(validObject(x1))
        expect_true(x1@isSmallUpdate@.Data)
    }
    if (!updated)
        warning("not updated")
})

test_that("R and C versions of updateProposalAccountMoveBirthsSmall give same answer with CombinedAccountMovementsHasAge", {
    updateProposalAccountMoveBirthsSmall <- demest:::updateProposalAccountMoveBirthsSmall
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    popn <- Counts(array(rpois(n = 90, lambda = 100),
                         dim = c(3, 2, 5, 3),
                         dimnames = list(age = c("0-4", "5-9", "10+"),
                                         sex = c("f", "m"),
                                         reg = 1:5,
                                         time = c(2000, 2005, 2010))))
    births <- Counts(array(rpois(n = 90, lambda = 5),
                           dim = c(1, 2, 5, 2, 2),
                           dimnames = list(age = "5-9",
                                           sex = c("m", "f"),
                                           reg = 1:5,
                                           time = c("2001-2005", "2006-2010"),
                                           triangle = c("Lower", "Upper"))))
    internal <- Counts(array(rpois(n = 300, lambda = 10),
                             dim = c(3, 2, 5, 5, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                             sex = c("m", "f"),
                                             reg_orig = 1:5,
                                             reg_dest = 1:5,
                                             time = c("2001-2005", "2006-2010"),
                                             triangle = c("Lower", "Upper"))))
    deaths <- Counts(array(rpois(n = 72, lambda = 10),
                           dim = c(3, 2, 5, 2, 2),
                           dimnames = list(age = c("0-4", "5-9", "10+"),
                                           sex = c("m", "f"),
                                           reg = 5:1,
                                           time = c("2001-2005", "2006-2010"),
                                           triangle = c("Lower", "Upper"))))
    account <- Movements(population = popn,
                         births = births,
                         internal = internal,
                         exits = list(deaths = deaths))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ age + sex, useExpose = FALSE)),
                         Model(births ~ Poisson(mean ~ 1)),
                         Model(internal ~ Poisson(mean ~ reg_orig + reg_dest)),
                         Model(deaths ~ Poisson(mean ~ 1)))
    systemWeights <- list(NULL, NULL, NULL, NULL)
    census <- subarray(popn, time == "2000", drop = FALSE) + 2L
    register <- Counts(array(rpois(n = 90, lambda = popn),
                             dim = dim(popn),
                             dimnames = dimnames(popn)))
    reg.births <- Counts(array(rbinom(n = 90, size = births, prob = 0.98),
                               dim = dim(births),
                               dimnames = dimnames(births)))
    address.change <- Counts(array(rpois(n = 300, lambda = internal),
                                   dim = dim(internal),
                                   dimnames = dimnames(internal)))
    reg.deaths <- Counts(array(rbinom(n = 90, size = deaths, prob = 0.98),
                               dim = dim(deaths),
                               dimnames = dimnames(deaths))) + 1L
    datasets <- list(census, register, reg.births, address.change, reg.deaths)
    namesDatasets <- c("census", "register", "reg.births", "address.change", "reg.deaths")
    data.models <- list(Model(census ~ PoissonBinomial(prob = 0.95), series = "population"),
                              Model(register ~ Poisson(mean ~ 1), series = "population"),
                              Model(reg.births ~ PoissonBinomial(prob = 0.98), series = "births"),
                              Model(address.change ~ Poisson(mean ~ 1), series = "internal"),
                              Model(reg.deaths ~ PoissonBinomial(prob = 0.98), series = "deaths"))
    seriesIndices <- c(0L, 0L, 1L, 2L, 3L)
    updateInitialPopn <- new("LogicalFlag", TRUE)
    usePriorPopn <- new("LogicalFlag", TRUE)
    transforms <- list(makeTransform(x = population(account), y = datasets[[1]], subset = TRUE),
                       makeTransform(x = population(account), y = datasets[[2]], subset = TRUE),
                       makeTransform(x = components(account, "births"), y = datasets[[3]], subset = TRUE),
                       makeTransform(x = components(account, "internal"), y = datasets[[4]], subset = TRUE),
                       makeTransform(x = components(account, "deaths"), y = datasets[[5]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x0 <- initialCombinedAccount(account = account,
                                 systemModels = systemModels,
                                 systemWeights = systemWeights,
                                 dataModels = data.models,
                                 seriesIndices = seriesIndices,
                                 updateInitialPopn = updateInitialPopn,
                                 usePriorPopn = usePriorPopn,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    expect_true(validObject(x0))
    expect_is(x0, "CombinedAccountMovementsHasAge")
    x0@iComp <- 1L
    updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- updateProposalAccountMoveBirthsSmall(x0, useC = FALSE)
        if (ans.R@generatedNewProposal@.Data)
            updated <- TRUE
        set.seed(seed)
        ans.C <- updateProposalAccountMoveBirthsSmall(x0, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
    if (!updated)
        warning("not updated")
})


## READY_TO_TRANSLATE
## HAS_TESTS
## assume has age
updateProposalAccountMoveOrigDestSmall <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    stopifnot(combined@hasAge@.Data)
    if (useC) {
        .Call(updateProposalAccountMoveOrigDestSmall_R, combined)
    }
    else {
        account <- combined@account
        population <- account@population
        i.comp <- combined@iComp
        component <- account@components[[i.comp]]
        max.attempt <- combined@maxAttempt
        accession <- combined@accession
        mapping.to.acc <- combined@mappingsToAcc[[i.comp]]
        uses.exposure <- combined@modelUsesExposure[i.comp + 1L]
        mapping.to.exp <- combined@mappingsToExp[[i.comp]]
        description <- combined@descriptions[[i.comp + 1L]]
        sys.mod.comp <- combined@systemModels[[i.comp + 1L]]
        theta <- sys.mod.comp@theta
        struc.zero.array <- sys.mod.comp@strucZeroArray
        for (i in seq_len(max.attempt)) {
            i.cell.up <- chooseICellCompUpperTri(description)
            is.struc.zero <- struc.zero.array[i.cell.up] == 0L
            if (!is.struc.zero) {
                generated.new.proposal <- TRUE
                break
            }
        }
        if (generated.new.proposal) {
            ## i.cell.low is lower Lexis triangle within same
            ## period but next age group - except when i.cell.up
            ## is for oldest age group, in which case i.cell.low
            ## is from same age-period square
            i.cell.low <- getICellLowerTriNextFromComp(iCellUp = i.cell.up,
                                                       description = description)
            pair.acc <- getIAccNextFromOrigDest(i = i.cell.up,
                                                mapping = mapping.to.acc)
            i.acc.orig <- pair.acc[1L]
            i.acc.dest <- pair.acc[2L]
            is.final.age.group <- i.acc.orig == 0L
            ## Our existing accounting system ignores possible accession
            ## for cohort aged A+ at time t. Future version should
            ## include this.
            if (!is.final.age.group) {
                val.acc.orig <- accession[i.acc.orig]
                val.acc.dest <- accession[i.acc.dest]
            }
            val.up.curr <- component[i.cell.up]
            val.low.curr <- component[i.cell.low]
            val.up.expected <- theta[i.cell.up]
            val.low.expected <- theta[i.cell.low]
            if (uses.exposure) {
                exposure <- combined@exposure
                i.expose.up <- getIExposureFromOrigDest(i = i.cell.up,
                                                        mapping = mapping.to.exp)
                i.expose.low <- getIExposureFromOrigDest(i = i.cell.low,
                                                         mapping = mapping.to.exp)
                expose.up <- exposure[i.expose.up]
                expose.low <- exposure[i.expose.low]
                val.up.expected <- val.up.expected * expose.up
                val.low.expected <- val.low.expected * expose.low
            }
            prob <- val.up.expected / (val.up.expected + val.low.expected)
            size <- val.up.curr + val.low.curr
            if (is.final.age.group) { ## no accession constraint
                val.up.prop <- rbinom(n = 1L,
                                      size = size,
                                      prob = prob)
            }
            else {
                lower <- val.up.curr - val.acc.dest
                upper <- val.up.curr + val.acc.orig
                val.up.prop <- rbinomTrunc1(size = size,
                                            prob = prob,
                                            lower = lower,
                                            upper = upper,
                                            maxAttempt = max.attempt)
            }
            found.value <- !is.na(val.up.prop)
            if (found.value) {
                val.low.prop <- size - val.up.prop
                diff.prop <- unname(val.up.prop - val.up.curr)
                generated.new.proposal  <- diff.prop != 0L
            }
            else
                generated.new.proposal  <- FALSE
        }
    }
    if (generated.new.proposal) {
        combined@generatedNewProposal@.Data <- TRUE
        combined@isSmallUpdate@.Data <- TRUE
        combined@iCell <- i.cell.up
        combined@iCellOther <- i.cell.low
        combined@iPopnNext <- NA_integer_
        combined@iPopnNextOther <- NA_integer_
        combined@iAccNext <- i.acc.orig
        combined@iAccNextOther <- i.acc.dest
        combined@isLowerTriangle@.Data <- FALSE
        if (uses.exposure) {
            combined@iExposure <- i.expose.up
            combined@iExposureOther <- i.expose.low
        }
        else {
            combined@iExposure <- 0L
            combined@iExposureOther <- NA_integer_
        }
        combined@iExpFirst <- NA_integer_
        combined@iExpFirstOther <- NA_integer_
        combined@diffProp <- diff.prop
    }
    else {
        combined@generatedNewProposal@.Data <- FALSE
        combined@isSmallUpdate@.Data <- TRUE
        combined@iCell <- NA_integer_
        combined@iCellOther <- NA_integer_
        combined@iPopnNext <- NA_integer_
        combined@iPopnNextOther <- NA_integer_
        combined@iAccNext <- NA_integer_
        combined@iAccNextOther <- NA_integer_
        combined@isLowerTriangle@.Data <- NA
        combined@iExposure <- NA_integer_
        combined@iExposureOther <- NA_integer_
        combined@iExpFirst <- NA_integer_
        combined@iExpFirstOther <- NA_integer_
        combined@diffProp <- NA_integer_
    }
    combined
}

test_that("updateProposalAccountMoveOrigDestSmall works with CombinedAccountMovementsHasAge", {
    set.seed(1)
    ## updateProposalAccountMoveOrigDestSmall <- demest:::updateProposalAccountMoveOrigDestSmall
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    popn <- Counts(array(rpois(n = 90, lambda = 100),
                         dim = c(3, 2, 5, 3),
                         dimnames = list(age = c("0-4", "5-9", "10+"),
                                         sex = c("f", "m"),
                                         reg = 1:5,
                                         time = c(2000, 2005, 2010))))
    births <- Counts(array(rpois(n = 90, lambda = 5),
                           dim = c(1, 2, 5, 2, 2),
                           dimnames = list(age = "5-9",
                                           sex = c("m", "f"),
                                           reg = 1:5,
                                           time = c("2001-2005", "2006-2010"),
                                           triangle = c("Lower", "Upper"))))
    internal <- Counts(array(rpois(n = 300, lambda = 10),
                             dim = c(3, 2, 5, 5, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                             sex = c("m", "f"),
                                             reg_orig = 1:5,
                                             reg_dest = 1:5,
                                             time = c("2001-2005", "2006-2010"),
                                             triangle = c("Lower", "Upper"))))
    deaths <- Counts(array(rpois(n = 72, lambda = 10),
                           dim = c(3, 2, 5, 2, 2),
                           dimnames = list(age = c("0-4", "5-9", "10+"),
                                           sex = c("m", "f"),
                                           reg = 5:1,
                                           time = c("2001-2005", "2006-2010"),
                                           triangle = c("Lower", "Upper"))))
    account <- Movements(population = popn,
                         births = births,
                         internal = internal,
                         exits = list(deaths = deaths))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ age + sex, useExpose = FALSE)),
                         Model(births ~ Poisson(mean ~ 1)),
                         Model(internal ~ Poisson(mean ~ reg_orig + reg_dest)),
                         Model(deaths ~ Poisson(mean ~ 1)))
    systemWeights <- list(NULL, NULL, NULL, NULL)
    census <- subarray(popn, time == "2000", drop = FALSE) + 2L
    register <- Counts(array(rpois(n = 90, lambda = popn),
                             dim = dim(popn),
                             dimnames = dimnames(popn)))
    reg.births <- Counts(array(rbinom(n = 90, size = births, prob = 0.98),
                               dim = dim(births),
                               dimnames = dimnames(births)))
    address.change <- Counts(array(rpois(n = 300, lambda = internal),
                                   dim = dim(internal),
                                   dimnames = dimnames(internal)))
    reg.deaths <- Counts(array(rbinom(n = 90, size = deaths, prob = 0.98),
                               dim = dim(deaths),
                               dimnames = dimnames(deaths))) + 1L
    datasets <- list(census, register, reg.births, address.change, reg.deaths)
    namesDatasets <- c("census", "register", "reg.births", "address.change", "reg.deaths")
    data.models <- list(Model(census ~ PoissonBinomial(prob = 0.95), series = "population"),
                              Model(register ~ Poisson(mean ~ 1), series = "population"),
                              Model(reg.births ~ PoissonBinomial(prob = 0.98), series = "births"),
                              Model(address.change ~ Poisson(mean ~ 1), series = "internal"),
                              Model(reg.deaths ~ PoissonBinomial(prob = 0.98), series = "deaths"))
    seriesIndices <- c(0L, 0L, 1L, 2L, 3L)
    updateInitialPopn <- new("LogicalFlag", TRUE)
    usePriorPopn <- new("LogicalFlag", TRUE)
    transforms <- list(makeTransform(x = population(account), y = datasets[[1]], subset = TRUE),
                       makeTransform(x = population(account), y = datasets[[2]], subset = TRUE),
                       makeTransform(x = components(account, "births"), y = datasets[[3]], subset = TRUE),
                       makeTransform(x = components(account, "internal"), y = datasets[[4]], subset = TRUE),
                       makeTransform(x = components(account, "deaths"), y = datasets[[5]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x0 <- initialCombinedAccount(account = account,
                                 systemModels = systemModels,
                                 systemWeights = systemWeights,
                                 dataModels = data.models,
                                 seriesIndices = seriesIndices,
                                 updateInitialPopn = updateInitialPopn,
                                 usePriorPopn = usePriorPopn,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    expect_true(validObject(x0))
    expect_is(x0, "CombinedAccountMovementsHasAge")
    x0@iComp <- 2L
    updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        x1 <- updateProposalAccountMoveOrigDestSmall(x0)
        if (x1@generatedNewProposal@.Data) {
            updated <- TRUE
            expect_false(x1@diffProp == 0L)
        }
        expect_is(x1, "CombinedAccountMovementsHasAge")
        expect_true(validObject(x1))
        expect_true(x1@isSmallUpdate@.Data)
    }
    if (!updated)
        warning("not updated")
})


test_that("R and C versions of updateProposalAccountMoveOrigDestSmall give same answer with CombinedAccountMovementsHasAge", {
    updateProposalAccountMoveOrigDestSmall <- demest:::updateProposalAccountMoveOrigDestSmall
    initialCombinedAccount <- demest:::initialCombinedAccount
    makeCollapseTransformExtra <- dembase::makeCollapseTransformExtra
    popn <- Counts(array(rpois(n = 90, lambda = 100),
                         dim = c(3, 2, 5, 3),
                         dimnames = list(age = c("0-4", "5-9", "10+"),
                                         sex = c("f", "m"),
                                         reg = 1:5,
                                         time = c(2000, 2005, 2010))))
    births <- Counts(array(rpois(n = 90, lambda = 5),
                           dim = c(1, 2, 5, 2, 2),
                           dimnames = list(age = "5-9",
                                           sex = c("m", "f"),
                                           reg = 1:5,
                                           time = c("2001-2005", "2006-2010"),
                                           triangle = c("Lower", "Upper"))))
    internal <- Counts(array(rpois(n = 300, lambda = 10),
                             dim = c(3, 2, 5, 5, 2, 2),
                             dimnames = list(age = c("0-4", "5-9", "10+"),
                                             sex = c("m", "f"),
                                             reg_orig = 1:5,
                                             reg_dest = 1:5,
                                             time = c("2001-2005", "2006-2010"),
                                             triangle = c("Lower", "Upper"))))
    deaths <- Counts(array(rpois(n = 72, lambda = 10),
                           dim = c(3, 2, 5, 2, 2),
                           dimnames = list(age = c("0-4", "5-9", "10+"),
                                           sex = c("m", "f"),
                                           reg = 5:1,
                                           time = c("2001-2005", "2006-2010"),
                                           triangle = c("Lower", "Upper"))))
    account <- Movements(population = popn,
                         births = births,
                         internal = internal,
                         exits = list(deaths = deaths))
    account <- makeConsistent(account)
    systemModels <- list(Model(population ~ Poisson(mean ~ age + sex, useExpose = FALSE)),
                         Model(births ~ Poisson(mean ~ 1)),
                         Model(internal ~ Poisson(mean ~ reg_orig + reg_dest)),
                         Model(deaths ~ Poisson(mean ~ 1)))
    systemWeights <- list(NULL, NULL, NULL, NULL)
    census <- subarray(popn, time == "2000", drop = FALSE) + 2L
    register <- Counts(array(rpois(n = 90, lambda = popn),
                             dim = dim(popn),
                             dimnames = dimnames(popn)))
    reg.births <- Counts(array(rbinom(n = 90, size = births, prob = 0.98),
                               dim = dim(births),
                               dimnames = dimnames(births)))
    address.change <- Counts(array(rpois(n = 300, lambda = internal),
                                   dim = dim(internal),
                                   dimnames = dimnames(internal)))
    reg.deaths <- Counts(array(rbinom(n = 90, size = deaths, prob = 0.98),
                               dim = dim(deaths),
                               dimnames = dimnames(deaths))) + 1L
    datasets <- list(census, register, reg.births, address.change, reg.deaths)
    namesDatasets <- c("census", "register", "reg.births", "address.change", "reg.deaths")
    data.models <- list(Model(census ~ PoissonBinomial(prob = 0.95), series = "population"),
                              Model(register ~ Poisson(mean ~ 1), series = "population"),
                              Model(reg.births ~ PoissonBinomial(prob = 0.98), series = "births"),
                              Model(address.change ~ Poisson(mean ~ 1), series = "internal"),
                              Model(reg.deaths ~ PoissonBinomial(prob = 0.98), series = "deaths"))
    seriesIndices <- c(0L, 0L, 1L, 2L, 3L)
    updateInitialPopn <- new("LogicalFlag", TRUE)
    usePriorPopn <- new("LogicalFlag", TRUE)
    transforms <- list(makeTransform(x = population(account), y = datasets[[1]], subset = TRUE),
                       makeTransform(x = population(account), y = datasets[[2]], subset = TRUE),
                       makeTransform(x = components(account, "births"), y = datasets[[3]], subset = TRUE),
                       makeTransform(x = components(account, "internal"), y = datasets[[4]], subset = TRUE),
                       makeTransform(x = components(account, "deaths"), y = datasets[[5]], subset = TRUE))
    transforms <- lapply(transforms, makeCollapseTransformExtra)
    x0 <- initialCombinedAccount(account = account,
                                 systemModels = systemModels,
                                 systemWeights = systemWeights,
                                 dataModels = data.models,
                                 seriesIndices = seriesIndices,
                                 updateInitialPopn = updateInitialPopn,
                                 usePriorPopn = usePriorPopn,
                                 datasets = datasets,
                                 namesDatasets = namesDatasets,
                                 transforms = transforms)
    expect_true(validObject(x0))
    expect_is(x0, "CombinedAccountMovementsHasAge")
    x0@iComp <- 2L
    updated <- FALSE
    for (seed in seq_len(n.test)) {
        set.seed(seed)
        ans.R <- updateProposalAccountMoveOrigDestSmall(x0, useC = FALSE)
        if (ans.R@generatedNewProposal@.Data)
            updated <- TRUE
        set.seed(seed)
        ans.C <- updateProposalAccountMoveOrigDestSmall(x0, useC = TRUE)
        if (test.identity)
            expect_identical(ans.R, ans.C)
        else
            expect_equal(ans.R, ans.C)
    }
    if (!updated)
        warning("not updated")
})




## assume has age, and is not net
updateProposalAccountMoveCompSmall <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    stopifnot(combined@hasAge@.Data)
    stopifnot(!combined@isNet[combined@i.comp])
    if (useC) {
        .Call(updateProposalAccountMoveCompSmall_R, combined)
    }
    else {
        account <- combined@account
        population <- account@population
        i.comp <- combined@iComp
        component <- account@components[[i.comp]]
        is.increment <- combined@isIncrement[[i.comp]]
        max.attempt <- combined@maxAttempt
        accession <- combined@accession
        mapping.to.acc <- combined@mappingsToAcc[[i.comp]]
        uses.exposure <- combined@modelUsesExposure[i.comp + 1L]
        mapping.to.exp <- combined@mappingsToExp[[i.comp]]
        description <- combined@descriptions[[i.comp + 1L]]
        sys.mod.comp <- combined@systemModels[[i.comp + 1L]]
        theta <- sys.mod.comp@theta
        struc.zero.array <- sys.mod.comp@strucZeroArray
        for (i in seq_len(max.attempt)) {
            i.cell.up <- chooseICellCompUpperTri(description)
            is.struc.zero <- struc.zero.array[i.cell.up] == 0L
            if (!is.struc.zero) {
                generated.new.proposal <- TRUE
                break
            }
        }
        if (generated.new.proposal) {
            ## i.cell.low is lower Lexis triangle within same
            ## period but next age group - except when i.cell.up
            ## is for oldest age group, in which case i.cell.low
            ## is from same age-period square
            i.cell.low <- getICellLowerTriNextFromComp(iCellUp = i.cell.up,
                                                   description = description)
            i.acc <- getIAccNextFromComp(i = i.cell.up,
                                         mapping = mapping.to.acc)
            is.final.age.group <- i.acc == 0L
            ## Our existing accounting system ignores possible accession
            ## for cohort aged A+ at time t. Future version should
            ## include this.
            if (!is.final.age.group)
                val.acc <- accession[i.acc]
            val.up.curr <- component[i.cell.up]
            val.low.curr <- component[i.cell.low]
            val.up.expected <- theta[i.cell.up]
            val.low.expected <- theta[i.cell.low]
            if (uses.exposure) {
                exposure <- combined@exposure
                i.expose.up <- getIExposureFromComp(i = i.cell.up,
                                                    mapping = mapping.to.exp)
                i.expose.low <- getIExposureFromComp(i = i.cell.up,
                                                     mapping = mapping.to.exp)
                expose.up <- exposure[i.expose.up]
                expose.low <- exposure[i.expose.low]
                val.up.expected <- val.up.expected * expose.up
                val.low.expected <- val.low.expected * expose.low
            }
            prob <- val.up.expected / (val.up.expected + val.low.expected)
            size <- val.up.curr + val.low.curr
            if (is.final.age.group) { ## no accession constraint
                val.up.prop <- rbinom(n = 1L,
                                      size = size,
                                      prob = prob)
            }
            else {
                if (is.increment) {
                    lower <- val.up.curr - val.acc
                    upper <- NA_integer_
                }
                else {
                    lower <- NA_integer_
                    upper <- val.up.curr + val.acc
                }
                val.up.prop <- rbinomTrunc1(size = size,
                                            prob = prob,
                                            lower = lower,
                                            upper = upper)
            }
            found.value <- !is.na(val.prop)
            if (found.value) {
                val.low.prop <- size - val.up.prop
                diff.prop <- unname(val.up.prop - val.up.curr)
                generated.new.proposal  <- diff.prop != 0L
            }
            else
                generated.new.proposal  <- FALSE
        }
    }
    if (generated.new.proposal) {
        combined@generatedNewProposal@.Data <- TRUE
        combined@isSmallUpdate@.Data <- TRUE
        combined@iCell <- i.cell.up
        combined@iCellOther <- i.cell.low
        combined@iPopnNext <- NA_integer_
        combined@iPopnNextOther <- NA_integer_
        combined@iAccNext <- i.acc
        combined@iAccNextOther <- NA_integer_
        combined@isLowerTriangle@.Data <- FALSE
        if (uses.exposure) {
            combined@iExposure <- i.exposure
            combined@iExposureOther <- NA_integer_
        }
        else {
            combined@iExposure <- 0L
            combined@iExposureOther <- NA_integer_
        }
        combined@iExpFirst <- i.exp.first
        combined@iExpFirstOther <- NA_integer_
        combined@diffProp <- diff.prop
    }
    else {
        combined@generatedNewProposal@.Data <- FALSE
        combined@isSmallUpdate@.Data <- FALSE
        combined@iCell <- NA_integer_
        combined@iCellOther <- NA_integer_
        combined@iPopnNext <- NA_integer_
        combined@iPopnNextOther <- NA_integer_
        combined@iAccNext <- NA_integer_
        combined@iAccNextOther <- NA_integer_
        combined@isLowerTriangle@.Data <- NA
        combined@iExposure <- NA_integer_
        combined@iExposureOther <- NA_integer_
        combined@iExpFirst <- NA_integer_
        combined@iExpFirstOther <- NA_integer_
        combined@diffProp <- NA_integer_
    }
    combined
}


diffLogLikAccountMoveCompSmall <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    stopifnot(combined@@.Data)
    if (useC) {
        .Call(diffLogLikAccountMoveCompSmall_R, combined)
    }
    else {
        account <- combined@account
        i.comp <- combined@iComp
        component <- combined@account@components[[i.comp]]
        data.models <- combined@dataModels
        datasets <- combined@datasets
        series.indices <- combined@seriesIndices
        transforms <- combined@transforms
        i.cell.up <- combined@iCell
        i.cell.low <- combined@iCellOther
        diff <- combined@diffProp
        is.increment <- combined@isIncrement[i.comp]
        diff.log.lik.up <- diffLogLikCellComp(diff = diff,
                                              iComp = i.comp,
                                              iCell = i.cell.up,
                                              component = component,
                                              dataModels = data.models,
                                              datasets = datasets,
                                              seriesIndices = series.indices,
                                              transforms = transforms)
        if (is.infinite(diff.log.lik.up))
            return(diff.log.lik.up)
        diff.log.lik.low <- diffLogLikCellComp(diff = -diff,
                                               iComp = i.comp,
                                               iCell = i.cell.low,
                                               component = component,
                                               dataModels = data.models,
                                               datasets = datasets,
                                               seriesIndices = series.indices,
                                               transforms = transforms)
        if (is.infinite(diff.log.lik.low))
            return(diff.log.lik.low)
        diff.log.lik.up + diff.log.lik.low
    }
}

diffLogDensCompSmall <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(diffLogDensCompSmall_R, combined)
    }
    else {
        i.comp <- combined@iComp
        component <- combined@account@components[[i.comp]]
        theta <- combined@systemModels[[i.comp + 1L]]@theta
        uses.exposure <- combined@modelUsesExposure[i.comp + 1L]
        i.cell.up <- combined@iCell
        i.cell.low <- combined@iCellOther
        uses.exposure <- combined@modelUsesExposure[i.comp + 1L]
        diff <- combined@diffProp
        val.up.curr <- component[i.cell.up]
        val.low.curr <- component[i.cell.low]
        val.up.prop <- val.up.curr + diff
        val.low.prop <- val.low.cur - diff
        val.up.expected <- theta[i.cell.up]
        val.low.expected <- theta[i.cell.low]
        if (uses.exposure) {
            exposure <- combined@exposure
            i.expose.up <- combined@iExposure
            i.expose.low <- combined@iExposureOther
            expose.up <- exposure[i.expose.up]
            expose.low <- exposure[i.expose.low]
            val.up.expected <- val.up.expected * expose.up
            val.low.expected <- val.low.expected * expose.low
        }
        ans <- (stats::dpois(x = val.up.prop, lambda = val.up.expected, log = TRUE) +
                stats::dpois(x = val.low.prop, lambda = val.low.expected, log = TRUE) -
                stats::dpois(x = val.up.curr, lambda = val.up.expected, log = TRUE) -
                stats::dpois(x = val.low.curr, lambda = val.low.expected, log = TRUE))
        ans
    }
}


updateCellMove <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovements"))
    if (useC) {
        .Call(updateCellMove_R, combined)
    }
    else {
        i.comp <- combined@iComp
        i.cell <- combined@iCell
        i.cell.other <- combined@iCellOther
        i.pool <- combined@iPool
        i.int.net <- combined@iIntNet
        is.small.update <- combined@isSmallUpdate ## NEW
        diff <- combined@diffProp
        is.popn <- i.comp == 0L
        is.pool <- i.comp == i.pool
        is.int.net <- i.comp == i.int.net
        if (is.popn) {
            combined@account@population[i.cell] <- combined@account@population[i.cell] + diff
        }
        else if (is.pool) {
            combined@account@components[[i.comp]][i.cell] <-
                combined@account@components[[i.comp]][i.cell] + diff
            combined@account@components[[i.comp]][i.cell.other] <-
                combined@account@components[[i.comp]][i.cell.other] + diff
        }
        else if (is.int.net || is.small.update) { ## NEW
            combined@account@components[[i.comp]][i.cell] <-
                combined@account@components[[i.comp]][i.cell] + diff
        combined@account@components[[i.comp]][i.cell.other] <-
            combined@account@components[[i.comp]][i.cell.other] - diff
        }
        else {
            combined@account@components[[i.comp]][i.cell] <-
                combined@account@components[[i.comp]][i.cell] + diff
        }
        combined
    }
}            



updateAccSmall <- function(combined, useC = FALSE) {
    stopifnot(methods::is(combined, "CombinedAccountMovementsHasAge"))
    if (useC) {
        .Call(updateAccSmall_R, combined)
    }
    else {
        i.comp <- combined@iComp
        diff <- combined@diffProp
        is.increment <- combined@isIncrement
        i.acc <- combined@iAccNext
        has.accession <- i.acc > 0L
        if (has.accession) {
            if (is.increment)
                combined@accession[i.acc] <- combined@accession[i.acc] + diff
            else
                combined@accession[i.acc] <- combined@accession[i.acc] - diff
        }
    }
    combined
}
