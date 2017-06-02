
## MAPPINGS TO POPULATION ################################################################

## component, net

## HAS_TESTS
setMethod("Mapping",
          signature(current = "Component",
                    target = "Population"),
          function(current, target) {
              dim.current <- dim(current)
              dim.target <- dim(target)
              dimtypes.current <- dembase::dimtypes(current, use.names = FALSE)
              dimtypes.target <- dembase::dimtypes(target, use.names = FALSE)
              i.time.current <- match("time", dimtypes.current)
              i.time.target <- match("time", dimtypes.target)
              i.age.current <- match("age", dimtypes.current, nomatch = 0L)
              has.age <- i.age.current > 0L
              n.time.current <- dim.current[i.time.current]
              step.time.current <- 1L
              for (d in seq_len(i.time.current - 1L))
                  step.time.current <- step.time.current * dim.current[d]
              step.time.target <- 1L
              for (d in seq_len(i.time.target - 1L))
                  step.time.target <- step.time.target * dim.target[d]
              s.current <- seq_along(dim.current)
              s.target <- seq_along(dim.target)
              i.shared.current <- setdiff(s.current, i.time.current)
              i.shared.target <- setdiff(s.target, i.time.target)
              if (has.age) {
                  n.age <- dim.current[i.age.current]
                  i.age.target <- match("age", dimtypes.target)
                  i.triangle <- match("triangle", dimtypes.current)
                  step.age.current <- 1L
                  for (d in seq_len(i.age.current - 1L))
                      step.age.current <- step.age.current * dim.current[d]
                  step.age.target <- 1L
                  for (d in seq_len(i.age.target - 1L))
                      step.age.target <- step.age.target * dim.target[d]
                  step.triangle.current <- 1L
                  for (d in seq_len(i.triangle - 1L))
                      step.triangle.current <- step.triangle.current * dim.current[d]
                  i.shared.current <- setdiff(i.shared.current, c(i.age.current, i.triangle))
                  i.shared.target <- setdiff(i.shared.target, i.age.target)
              }
              else {
                  n.age <- NA_integer_
                  step.age.current <- NA_integer_
                  step.age.target <- NA_integer_
                  step.triangle.current <- NA_integer_
              }
              n.shared.vec <- dim.current[i.shared.current]
              length.shared <- length(i.shared.current)
              step.shared.current.vec <- integer(length = length.shared)
              step.shared.target.vec <- integer(length = length.shared)
              for (i in seq_len(length.shared)) {
                  step <- 1L
                  for (d in seq_len(i.shared.current[i] - 1L))
                      step <- step * dim.current[d]
                  step.shared.current.vec[i] <- step
                  step <- 1L
                  for (d in seq_len(i.shared.target[i] - 1L))
                      step <- step * dim.target[d]
                  step.shared.target.vec[i] <- step
              }
              methods::new("MappingCompToPopn",
                  nTimeCurrent = n.time.current,
                  stepTimeCurrent = step.time.current,
                  stepTimeTarget = step.time.target,
                  nSharedVec = n.shared.vec,
                  stepSharedCurrentVec = step.shared.current.vec,
                  stepSharedTargetVec = step.shared.target.vec,
                  hasAge = has.age,
                  nAge = n.age,
                  stepAgeCurrent = step.age.current,
                  stepAgeTarget = step.age.target,
                  stepTriangleCurrent = step.triangle.current)
          })

## births no parent, births with parent

## HAS_TESTS
## 'age' in 'current' is age of mother; 'age' in 'target' is age of child.
## Always set 'hasAge' to FALSE, and omit 'age' from shared dimensions.
## Include dimensions from 'current' with dimtype "child" in
## shared dimensions and exclude dimensions with dimtype "parent"
setMethod("Mapping",
          signature(current = "BirthsMovements",
                    target = "Population"),
          function(current, target) {
              dim.current <- dim(current)
              dim.target <- dim(target)
              dimtypes.current <- dembase::dimtypes(current, use.names = FALSE)
              dimtypes.target <- dembase::dimtypes(target, use.names = FALSE)
              i.age.mother <- match("age", dimtypes.current, nomatch = 0L)
              i.age.child <- match("age", dimtypes.target, nomatch = 0L)
              i.triangle <- match("triangle", dimtypes.current, nomatch = 0L)
              i.parent <- grep("parent", dimtypes.current)
              has.age.mother <- i.age.mother > 0L
              has.age.child <- i.age.child > 0L
              i.time.current <- match("time", dimtypes.current)
              i.time.target <- match("time", dimtypes.target)
              n.time.current <- dim.current[i.time.current]
              step.time.current <- 1L
              for (d in seq_len(i.time.current - 1L))
                  step.time.current <- step.time.current * dim.current[d]
              step.time.target <- 1L
              for (d in seq_len(i.time.target - 1L))
                  step.time.target <- step.time.target * dim.target[d]
              s.current <- seq_along(dim.current)
              s.target <- seq_along(dim.target)
              i.shared.current <- setdiff(s.current, i.time.current)
              i.shared.target <- setdiff(s.target, i.time.target)
              if (has.age.mother)
                  i.shared.current <- setdiff(i.shared.current, c(i.age.mother, i.triangle))
              if (has.age.child)
                  i.shared.target <- setdiff(i.shared.target, i.age.child)
              i.shared.current <- setdiff(i.shared.current, i.parent)
              n.shared.vec <- dim.current[i.shared.current]
              length.shared <- length(i.shared.current)
              step.shared.current.vec <- integer(length = length.shared)
              step.shared.target.vec <- integer(length = length.shared)
              for (i in seq_len(length.shared)) {
                  step <- 1L
                  for (d in seq_len(i.shared.current[i] - 1L))
                      step <- step * dim.current[d]
                  step.shared.current.vec[i] <- step
                  step <- 1L
                  for (d in seq_len(i.shared.target[i] - 1L))
                      step <- step * dim.target[d]
                  step.shared.target.vec[i] <- step
              }
              methods::new("MappingCompToPopn",
                  nTimeCurrent = n.time.current,
                  stepTimeCurrent = step.time.current,
                  stepTimeTarget = step.time.target,
                  nSharedVec = n.shared.vec,
                  stepSharedCurrentVec = step.shared.current.vec,
                  stepSharedTargetVec = step.shared.target.vec,
                  hasAge = FALSE,
                  nAge = NA_integer_,
                  stepAgeCurrent = NA_integer_,
                  stepAgeTarget = NA_integer_,
                  stepTriangleCurrent = NA_integer_)
          })

## orig-dest

## HAS_TESTS
setMethod("Mapping",
          signature(current = "InternalMovementsOrigDest",
                    target = "Population"),
          function(current, target) {
              names.current <- names(current)
              names.target <- names(target)
              dim.current <- dim(current)
              dim.target <- dim(target)
              dimtypes.current <- dembase::dimtypes(current, use.names = FALSE)
              dimtypes.target <- dembase::dimtypes(target, use.names = FALSE)
              i.time.current <- match("time", dimtypes.current)
              i.time.target <- match("time", dimtypes.target)
              i.orig.current <- grep("origin", dimtypes.current)
              base <- sub("_orig", "", names.current[i.orig.current])
              i.dest.current <- match(paste(base, "dest", sep = "_"), names.current)
              i.orig.dest.target <- match(base, names.target)
              i.age.current <- match("age", dimtypes.current, nomatch = 0L)
              has.age <- i.age.current > 0L
              n.time.current <- dim.current[i.time.current]
              step.time.current <- 1L
              for (d in seq_len(i.time.current - 1L))
                  step.time.current <- step.time.current * dim.current[d]
              step.time.target <- 1L
              for (d in seq_len(i.time.target - 1L))
                  step.time.target <- step.time.target * dim.target[d]
              n.orig.dest.vec <- dim.current[i.orig.current]
              n.orig.dest <- length(n.orig.dest.vec)
              step.orig.current.vec <- integer(length = n.orig.dest)
              step.dest.current.vec <- integer(length = n.orig.dest)
              step.orig.dest.target.vec <- integer(length = n.orig.dest)
              for (i in seq_len(n.orig.dest)) {
                  step.orig.current <- 1L
                  step.dest.current <- 1L
                  step.orig.dest.target <- 1L
                  for (d in seq_len(i.orig.current[i] - 1L))
                      step.orig.current <- step.orig.current * dim.current[d]
                  for (d in seq_len(i.dest.current[i] - 1L))
                      step.dest.current <- step.dest.current * dim.current[d]
                  for (d in seq_len(i.orig.dest.target[i] - 1L))
                      step.orig.dest.target <- step.orig.dest.target * dim.target[d]
                  step.orig.current.vec[i] <- step.orig.current
                  step.dest.current.vec[i] <- step.dest.current
                  step.orig.dest.target.vec[i] <- step.orig.dest.target
              }
              s.current <- seq_along(dim.current)
              s.target <- seq_along(dim.target)
              i.shared.current <- setdiff(s.current,
                                          c(i.time.current, i.orig.current, i.dest.current))
              i.shared.target <- setdiff(s.target,
                                         c(i.time.target, i.orig.dest.target))
              if (has.age) {
                  n.age <- dim.current[i.age.current]
                  i.age.target <- match("age", dimtypes.target)
                  i.triangle <- match("triangle", dimtypes.current)
                  step.age.current <- 1L
                  for (d in seq_len(i.age.current - 1L))
                      step.age.current <- step.age.current * dim.current[d]
                  step.age.target <- 1L
                  for (d in seq_len(i.age.target - 1L))
                      step.age.target <- step.age.target * dim.target[d]
                  step.triangle.current <- 1L
                  for (d in seq_len(i.triangle - 1L))
                      step.triangle.current <- step.triangle.current * dim.current[d]
                  i.shared.current <- setdiff(i.shared.current, c(i.age.current, i.triangle))
                  i.shared.target <- setdiff(i.shared.target, i.age.target)
              }
              else {
                  n.age <- NA_integer_
                  step.age.current <- NA_integer_
                  step.age.target <- NA_integer_
                  step.triangle.current <- NA_integer_
              }
              n.shared.vec <- dim.current[i.shared.current]
              length.shared <- length(i.shared.current)
              step.shared.current.vec <- integer(length = length.shared)
              step.shared.target.vec <- integer(length = length.shared)
              for (i in seq_len(length.shared)) {
                  step <- 1L
                  for (d in seq_len(i.shared.current[i] - 1L))
                      step <- step * dim.current[d]
                  step.shared.current.vec[i] <- step
                  step <- 1L
                  for (d in seq_len(i.shared.target[i] - 1L))
                      step <- step * dim.target[d]
                  step.shared.target.vec[i] <- step
              }
              methods::new("MappingOrigDestToPopn",
                           nTimeCurrent = n.time.current,
                           stepTimeCurrent = step.time.current,
                           stepTimeTarget = step.time.target,
                           nOrigDestVec = n.orig.dest.vec,
                           stepOrigCurrentVec = step.orig.current.vec,
                           stepDestCurrentVec = step.dest.current.vec,
                           stepOrigDestTargetVec = step.orig.dest.target.vec,
                           nSharedVec = n.shared.vec,
                           stepSharedCurrentVec = step.shared.current.vec,
                           stepSharedTargetVec = step.shared.target.vec,
                           hasAge = has.age,
                           nAge = n.age,
                           stepAgeCurrent = step.age.current,
                           stepAgeTarget = step.age.target,
                           stepTriangleCurrent = step.triangle.current)
          })

## pool

## HAS_TESTS
setMethod("Mapping",
          signature(current = "InternalMovementsPool",
                    target = "Population"),
          function(current, target) {
              i.direction <- current@iDirection # specific to Pool
              dim.current <- dim(current)
              dim.target <- dim(target)
              dimtypes.current <- dembase::dimtypes(current, use.names = FALSE)
              dimtypes.target <- dembase::dimtypes(target, use.names = FALSE)
              i.time.current <- match("time", dimtypes.current)
              i.time.target <- match("time", dimtypes.target)
              i.age.current <- match("age", dimtypes.current, nomatch = 0L)
              has.age <- i.age.current > 0L
              n.time.current <- dim.current[i.time.current]
              step.time.current <- 1L
              for (d in seq_len(i.time.current - 1L))
                  step.time.current <- step.time.current * dim.current[d]
              step.time.target <- 1L
              for (d in seq_len(i.time.target - 1L))
                  step.time.target <- step.time.target * dim.target[d]
              s.current <- seq_along(dim.current)
              s.target <- seq_along(dim.target)
              i.shared.current <- setdiff(s.current, i.time.current)
              i.shared.target <- setdiff(s.target, i.time.target)
              i.shared.current <- setdiff(i.shared.current, i.direction) # specific to Pool
              if (has.age) {
                  n.age <- dim.current[i.age.current]
                  i.age.target <- match("age", dimtypes.target)
                  i.triangle <- match("triangle", dimtypes.current)
                  step.age.current <- 1L
                  for (d in seq_len(i.age.current - 1L))
                      step.age.current <- step.age.current * dim.current[d]
                  step.age.target <- 1L
                  for (d in seq_len(i.age.target - 1L))
                      step.age.target <- step.age.target * dim.target[d]
                  step.triangle.current <- 1L
                  for (d in seq_len(i.triangle - 1L))
                      step.triangle.current <- step.triangle.current * dim.current[d]
                  i.shared.current <- setdiff(i.shared.current, c(i.age.current, i.triangle))
                  i.shared.target <- setdiff(i.shared.target, i.age.target)
              }
              else {
                  n.age <- NA_integer_
                  step.age.current <- NA_integer_
                  step.age.target <- NA_integer_
                  step.triangle.current <- NA_integer_
              }
              n.shared.vec <- dim.current[i.shared.current]
              length.shared <- length(i.shared.current)
              step.shared.current.vec <- integer(length = length.shared)
              step.shared.target.vec <- integer(length = length.shared)
              for (i in seq_len(length.shared)) {
                  step <- 1L
                  for (d in seq_len(i.shared.current[i] - 1L))
                      step <- step * dim.current[d]
                  step.shared.current.vec[i] <- step
                  step <- 1L
                  for (d in seq_len(i.shared.target[i] - 1L))
                      step <- step * dim.target[d]
                  step.shared.target.vec[i] <- step
              }
              methods::new("MappingCompToPopn",
                           nTimeCurrent = n.time.current,
                           stepTimeCurrent = step.time.current,
                           stepTimeTarget = step.time.target,
                           nSharedVec = n.shared.vec,
                           stepSharedCurrentVec = step.shared.current.vec,
                           stepSharedTargetVec = step.shared.target.vec,
                           hasAge = has.age,
                           nAge = n.age,
                           stepAgeCurrent = step.age.current,
                           stepAgeTarget = step.age.target,
                           stepTriangleCurrent = step.triangle.current)
          })


## MAPPINGS TO ACCESSION ################################################################

## component

## HAS_TESTS
setMethod("Mapping",
          signature(current = "Component",
                    target = "Accession"),
          function(current, target) {
              dim.current <- dim(current)
              dim.target <- dim(target)
              dimtypes.current <- dembase::dimtypes(current, use.names = FALSE)
              dimtypes.target <- dembase::dimtypes(target, use.names = FALSE)
              i.time.current <- match("time", dimtypes.current)
              i.time.target <- match("time", dimtypes.target)
              i.age.current <- match("age", dimtypes.current)
              i.age.target <- match("age", dimtypes.target)
              i.triangle.current <- match("triangle", dimtypes.current)
              n.time.current <- dim.current[i.time.current]
              step.time.current <- 1L
              for (d in seq_len(i.time.current - 1L))
                  step.time.current <- step.time.current * dim.current[d]
              step.time.target <- 1L
              for (d in seq_len(i.time.target - 1L))
                  step.time.target <- step.time.target * dim.target[d]
              n.age <- dim.current[i.age.current]
              step.age.current <- 1L
              for (d in seq_len(i.age.current - 1L))
                  step.age.current <- step.age.current * dim.current[d]
              step.age.target <- 1L
              for (d in seq_len(i.age.target - 1L))
                  step.age.target <- step.age.target * dim.target[d]
              step.triangle.current <- 1L
              for (d in seq_len(i.triangle.current - 1L))
                  step.triangle.current <- step.triangle.current * dim.current[d]
              s.current <- seq_along(dim.current)
              s.target <- seq_along(dim.target)
              i.shared.current <- setdiff(s.current,
                                          c(i.time.current, i.age.current, i.triangle.current))
              i.shared.target <- setdiff(s.target,
                                         c(i.time.target, i.age.target))
              n.shared.vec <- dim.current[i.shared.current]
              length.shared <- length(i.shared.current)
              step.shared.current.vec <- integer(length = length.shared)
              step.shared.target.vec <- integer(length = length.shared)
              for (i in seq_len(length.shared)) {
                  step <- 1L
                  for (d in seq_len(i.shared.current[i] - 1L))
                      step <- step * dim.current[d]
                  step.shared.current.vec[i] <- step
                  step <- 1L
                  for (d in seq_len(i.shared.target[i] - 1L))
                      step <- step * dim.target[d]
                  step.shared.target.vec[i] <- step
              }
              methods::new("MappingCompToAcc",
                           nTimeCurrent = n.time.current,
                           stepTimeCurrent = step.time.current,
                           stepTimeTarget = step.time.target,
                           nSharedVec = n.shared.vec,
                           stepSharedCurrentVec = step.shared.current.vec,
                           stepSharedTargetVec = step.shared.target.vec,
                           hasAge = TRUE,
                           nAge = n.age,
                           stepAgeCurrent = step.age.current,
                           stepAgeTarget = step.age.target,
                           stepTriangleCurrent = step.triangle.current)
          })

## births no parent, births with parent

## NO_TESTS
## 'age' in 'current' is age of mother; 'age' in 'target' is age of child.
## Always set 'hasAge' to FALSE, and omit 'age' from shared dimensions,
## since the ages refer to different things.
## Include dimensions from 'current' with dimtype "child" in
## shared dimensions and exclude dimensions with dimtype "parent"
setMethod("Mapping",
          signature(current = "BirthsMovements",
                    target = "Accession"),
          function(current, target) {
              dim.current <- dim(current)
              dim.target <- dim(target)
              dimtypes.current <- dembase::dimtypes(current, use.names = FALSE)
              dimtypes.target <- dembase::dimtypes(target, use.names = FALSE)
              i.age.mother <- match("age", dimtypes.current, nomatch = 0L)
              i.age.child <- match("age", dimtypes.target, nomatch = 0L)
              i.triangle <- match("triangle", dimtypes.current, nomatch = 0L)
              i.parent <- grep("parent", dimtypes.current)
              has.age.mother <- i.age.mother > 0L
              has.age.child <- i.age.child > 0L
              i.time.current <- match("time", dimtypes.current)
              i.time.target <- match("time", dimtypes.target)
              n.time <- dim.current[i.time.current]
              step.time.current <- 1L
              for (d in seq_len(i.time.current - 1L))
                  step.time.current <- step.time.current * dim.current[d]
              step.time.target <- 1L
              for (d in seq_len(i.time.target - 1L))
                  step.time.target <- step.time.target * dim.target[d]
              s.current <- seq_along(dim.current)
              s.target <- seq_along(dim.target)
              i.shared.current <- setdiff(s.current,
                                          c(i.time.current, i.age.mother, i.triangle, i.parent))
              i.shared.target <- setdiff(s.target,
                                         c(i.time.target, i.age.child))
              n.shared.vec <- dim.current[i.shared.current]
              length.shared <- length(i.shared.current)
              step.shared.current.vec <- integer(length = length.shared)
              step.shared.target.vec <- integer(length = length.shared)
              for (i in seq_len(length.shared)) {
                  step <- 1L
                  for (d in seq_len(i.shared.current[i] - 1L))
                      step <- step * dim.current[d]
                  step.shared.current.vec[i] <- step
                  step <- 1L
                  for (d in seq_len(i.shared.target[i] - 1L))
                      step <- step * dim.target[d]
                  step.shared.target.vec[i] <- step
              }
              methods::new("MappingCompToAcc",
                           nTimeCurrent = n.time,
                           stepTimeCurrent = step.time.current,
                           stepTimeTarget = step.time.target,
                           nSharedVec = n.shared.vec,
                           stepSharedCurrentVec = step.shared.current.vec,
                           stepSharedTargetVec = step.shared.target.vec,
                           hasAge = FALSE,
                           nAge = NA_integer_,
                           stepAgeCurrent = NA_integer_,
                           stepAgeTarget = NA_integer_,
                           stepTriangleCurrent = NA_integer_)
          })

setMethod("Mapping",
          signature(current = "InternalMovementsOrigDest",
                    target = "Accession"),
          function(current, target) {
              names.current <- names(current)
              names.target <- names(target)
              dim.current <- dim(current)
              dim.target <- dim(target)
              dimtypes.current <- dembase::dimtypes(current, use.names = FALSE)
              dimtypes.target <- dembase::dimtypes(target, use.names = FALSE)
              i.time.current <- match("time", dimtypes.current)
              i.time.target <- match("time", dimtypes.target)
              i.orig.current <- grep("origin", dimtypes.current)
              base <- sub("_orig", "", names.current[i.orig.current])
              i.dest.current <- match(paste(base, "dest", sep = "_"), names.current)
              i.orig.dest.target <- match(base, names.target)
              i.age.current <- match("age", dimtypes.current)
              n.time.current <- dim.current[i.time.current]
              step.time.current <- 1L
              for (d in seq_len(i.time.current - 1L))
                  step.time.current <- step.time.current * dim.current[d]
              step.time.target <- 1L
              for (d in seq_len(i.time.target - 1L))
                  step.time.target <- step.time.target * dim.target[d]
              n.orig.dest.vec <- dim.current[i.orig.current]
              n.orig.dest <- length(n.orig.dest.vec)
              step.orig.current.vec <- integer(length = n.orig.dest)
              step.dest.current.vec <- integer(length = n.orig.dest)
              step.orig.dest.target.vec <- integer(length = n.orig.dest)
              for (i in seq_len(n.orig.dest)) {
                  step.orig.current <- 1L
                  step.dest.current <- 1L
                  step.orig.dest.target <- 1L
                  for (d in seq_len(i.orig.current[i] - 1L))
                      step.orig.current <- step.orig.current * dim.current[d]
                  for (d in seq_len(i.dest.current[i] - 1L))
                      step.dest.current <- step.dest.current * dim.current[d]
                  for (d in seq_len(i.orig.dest.target[i] - 1L))
                      step.orig.dest.target <- step.orig.dest.target * dim.target[d]
                  step.orig.current.vec[i] <- step.orig.current
                  step.dest.current.vec[i] <- step.dest.current
                  step.orig.dest.target.vec[i] <- step.orig.dest.target
              }
              s.current <- seq_along(dim.current)
              s.target <- seq_along(dim.target)
              i.shared.current <- setdiff(s.current,
                                          c(i.time.current, i.orig.current, i.dest.current))
              i.shared.target <- setdiff(s.target,
                                         c(i.time.target, i.orig.dest.target))
              n.age <- dim.current[i.age.current]
              i.age.target <- match("age", dimtypes.target)
              i.triangle <- match("triangle", dimtypes.current)
              step.age.current <- 1L
              for (d in seq_len(i.age.current - 1L))
                  step.age.current <- step.age.current * dim.current[d]
              step.age.target <- 1L
              for (d in seq_len(i.age.target - 1L))
                  step.age.target <- step.age.target * dim.target[d]
              step.triangle.current <- 1L
              for (d in seq_len(i.triangle - 1L))
                  step.triangle.current <- step.triangle.current * dim.current[d]
              i.shared.current <- setdiff(i.shared.current, c(i.age.current, i.triangle))
              i.shared.target <- setdiff(i.shared.target, i.age.target)
              n.shared.vec <- dim.current[i.shared.current]
              length.shared <- length(i.shared.current)
              step.shared.current.vec <- integer(length = length.shared)
              step.shared.target.vec <- integer(length = length.shared)
              for (i in seq_len(length.shared)) {
                  step <- 1L
                  for (d in seq_len(i.shared.current[i] - 1L))
                      step <- step * dim.current[d]
                  step.shared.current.vec[i] <- step
                  step <- 1L
                  for (d in seq_len(i.shared.target[i] - 1L))
                      step <- step * dim.target[d]
                  step.shared.target.vec[i] <- step
              }
              methods::new("MappingOrigDestToPopn",
                           nTimeCurrent = n.time.current,
                           stepTimeCurrent = step.time.current,
                           stepTimeTarget = step.time.target,
                           nOrigDestVec = n.orig.dest.vec,
                           stepOrigCurrentVec = step.orig.current.vec,
                           stepDestCurrentVec = step.dest.current.vec,
                           stepOrigDestTargetVec = step.orig.dest.target.vec,
                           nSharedVec = n.shared.vec,
                           stepSharedCurrentVec = step.shared.current.vec,
                           stepSharedTargetVec = step.shared.target.vec,
                           hasAge = has.age,
                           nAge = n.age,
                           stepAgeCurrent = step.age.current,
                           stepAgeTarget = step.age.target,
                           stepTriangleCurrent = step.triangle.current)
          })


## MAPPINGS FROM EXPOSURE ################################################################

## NO_TESTS
setMethod("Mapping",
          signature(current = "Exposure",
                    target = "Component"),
          function(current, target) {
              dim <- dim(current)
              n.dim <- length(dim)
              step.shared.vec <- c(1L, as.integer(cumprod(dim[-n.dim])))
              methods::new("MappingExpToComp",
                  isOneToOne = TRUE,
                  nSharedVec = dim,
                  stepSharedCurrentVec = step.shared.vec,
                  stepSharedTargetVec = step.shared.vec)
          })


setMethod("Mapping",
          signature(current = "Exposure",
                    target = "InternalMovementsOrigDest"),
          function(current, target) {
              dim.current <- dim(current)
              dim.target <- dim(target)
              dimtypes.target <- dembase::dimtypes(target, use.names = FALSE)
              i.dest <- grep("parent", dimtypes.target)
              i.shared.current <- seq_along(dim.current)
              i.shared.target <- seq_along(dim.target)
              i.shared.target <- setdiff(i.shared.target, i.dest)
              n.shared.vec <- dim.current[i.shared.current]
              length.shared <- length(i.shared.current)
              step.shared.current.vec <- integer(length = length.shared)
              step.shared.target.vec <- integer(length = length.shared)
              for (i in seq_len(length.shared)) {
                  step <- 1L
                  for (d in seq_len(i.shared.current[i] - 1L))
                      step <- step * dim.current[d]
                  step.shared.current.vec[i] <- step
                  step <- 1L
                  for (d in seq_len(i.shared.target[i] - 1L))
                      step <- step * dim.target[d]
                  step.shared.target.vec[i] <- step
              }
              methods::new("MappingCompToExp",
                  isOneToOne = FALSE,
                  nSharedVec = n.shared.vec,
                  stepSharedCurrentVec = step.shared.current.vec,
                  stepSharedTargetVec = step.shared.target.vec)
          })              

setMethod("Mapping",
          signature(current = "Exposure",
                    target = "InternalMovementsPool"),
          function(current, target) {
              dim.current <- dim(current)
              dim.target <- dim(target)
              i.direction <- target@iDirection
              i.shared.current <- seq_along(dim.current)
              i.shared.target <- seq_along(dim.target)
              i.shared.target <- setdiff(i.shared.target, i.direction)
              n.shared.vec <- dim.current[i.shared.current]
              length.shared <- length(i.shared.current)
              step.shared.current.vec <- integer(length = length.shared)
              step.shared.target.vec <- integer(length = length.shared)
              for (i in seq_len(length.shared)) {
                  step <- 1L
                  for (d in seq_len(i.shared.current[i] - 1L))
                      step <- step * dim.current[d]
                  step.shared.current.vec[i] <- step
                  step <- 1L
                  for (d in seq_len(i.shared.target[i] - 1L))
                      step <- step * dim.target[d]
                  step.shared.target.vec[i] <- step
              }
              methods::new("MappingCompToExp",
                  isOneToOne = FALSE,
                  nSharedVec = n.shared.vec,
                  stepSharedCurrentVec = step.shared.current.vec,
                  stepSharedTargetVec = step.shared.target.vec)
          })


setMethod("Mapping",
          signature(current = "Exposure",
                    target = "BirthsMovements"),
          function(current, target) {
              i.min.age <- target@iMinAge
              dim.current <- dim(current)
              dim.target <- dim(target)
              dimtypes.current <- dembase::dimtypes(current, use.names = FALSE)
              dimtypes.target <- dembase::dimtypes(target, use.names = FALSE)
              i.time.current <- match("time", dimtypes.current, nomatch = 0L)
              i.time.target <- match("time", dimtypes.target, nomatch = 0L)
              i.age.current <- match("age", dimtypes.current, nomatch = 0L)
              has.age <- i.age.current > 0L
              i.parent <- grep("parent", dimtypes.target)
              n.time.current <- dim.current[i.time.current]
              step.time.current <- 1L
              for (d in seq_len(i.time.current - 1L))
                  step.time.current <- step.time.current * dim.current[d]
              step.time.target <- 1L
              for (d in seq_len(i.time.target - 1L))
                  step.time.target <- step.time.target * dim.target[d]
              s.current <- seq_along(dim.current)
              s.target <- seq_along(dim.target)
              i.shared.current <- setdiff(s.current, i.time.current)
              i.shared.target <- setdiff(s.target, i.time.target)
              if (has.age) {
                  i.age.target <- match("age", dimtypes.target, nomatch = 0L)
                  i.triangle.current <- match("triangle", dimtypes.current, nomatch = 0L)
                  i.triangle.target <- match("triangle", dimtypes.target, nomatch = 0L)
                  i.shared.current <- setdiff(i.shared.current,
                                              c(i.age.current, i.triangle.current))
                  i.shared.target <- setdiff(i.shared.target,
                                             c(i.age.target, i.triangle.target))
              }
              i.shared.target <- setdiff(i.shared.target, i.parent)
              n.shared.vec <- dim.current[i.shared.current]
              length.shared <- length(i.shared.current)
              step.shared.current.vec <- integer(length = length.shared)
              step.shared.target.vec <- integer(length = length.shared)
              for (i in seq_len(length.shared)) {
                  step <- 1L
                  for (d in seq_len(i.shared.current[i] - 1L))
                      step <- step * dim.current[d]
                  step.shared.current.vec[i] <- step
                  step <- 1L
                  for (d in seq_len(i.shared.target[i] - 1L))
                      step <- step * dim.target[d]
                  step.shared.target.vec[i] <- step
              }
              methods::new("MappingExpToComp",
                  isOneToOne = FALSE,
                  nSharedVec = n.shared.vec,
                  stepSharedCurrentVec = step.shared.current.vec,
                  stepSharedTargetVec = step.shared.target.vec,
                  nTimeCurrent = n.time.current,
                  stepTimeCurrent = step.time.current,
                  stepTimeTarget = step.time.target,
                  iMinAge = i.min.age)
          })              

