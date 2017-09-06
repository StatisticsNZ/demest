
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
                  nAgeCurrent = n.age,
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
                           nAgeCurrent = NA_integer_,
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
                           nAgeCurrent = n.age,
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
                           nAgeCurrent = n.age,
                           stepAgeCurrent = step.age.current,
                           stepAgeTarget = step.age.target,
                           stepTriangleCurrent = step.triangle.current)
          })


## MAPPINGS TO ACCESSION ################################################################

## component, net

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
                           nAgeCurrent = n.age,
                           stepAgeCurrent = step.age.current,
                           stepAgeTarget = step.age.target,
                           stepTriangleCurrent = step.triangle.current)
          })

## births no parent, births with parent

## HAS_TESTS
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
                           nAgeCurrent = NA_integer_,
                           stepAgeCurrent = NA_integer_,
                           stepAgeTarget = NA_integer_,
                           stepTriangleCurrent = NA_integer_)
          })

## orig-dest

## HAS_TESTS
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
              n.time <- dim.current[i.time.current]
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
              methods::new("MappingOrigDestToAcc",
                           nTimeCurrent = n.time,
                           stepTimeCurrent = step.time.current,
                           stepTimeTarget = step.time.target,
                           nOrigDestVec = n.orig.dest.vec,
                           stepOrigCurrentVec = step.orig.current.vec,
                           stepDestCurrentVec = step.dest.current.vec,
                           stepOrigDestTargetVec = step.orig.dest.target.vec,
                           nSharedVec = n.shared.vec,
                           stepSharedCurrentVec = step.shared.current.vec,
                           stepSharedTargetVec = step.shared.target.vec,
                           hasAge = TRUE,
                           nAgeCurrent = n.age,
                           stepAgeCurrent = step.age.current,
                           stepAgeTarget = step.age.target,
                           stepTriangleCurrent = step.triangle.current)
          })

## pool

## HAS_TESTS
setMethod("Mapping",
          signature(current = "InternalMovementsPool",
                    target = "Accession"),
          function(current, target) {
              i.direction <- current@iDirection # specific to Pool
              dim.current <- dim(current)
              dim.target <- dim(target)
              dimtypes.current <- dembase::dimtypes(current, use.names = FALSE)
              dimtypes.target <- dembase::dimtypes(target, use.names = FALSE)
              i.time.current <- match("time", dimtypes.current)
              i.time.target <- match("time", dimtypes.target)
              i.age.current <- match("age", dimtypes.current, nomatch = 0L)
              n.time <- dim.current[i.time.current]
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
              methods::new("MappingCompToAcc",
                           nTimeCurrent = n.time,
                           stepTimeCurrent = step.time.current,
                           stepTimeTarget = step.time.target,
                           nSharedVec = n.shared.vec,
                           stepSharedCurrentVec = step.shared.current.vec,
                           stepSharedTargetVec = step.shared.target.vec,
                           hasAge = TRUE,
                           nAgeCurrent = n.age,
                           stepAgeCurrent = step.age.current,
                           stepAgeTarget = step.age.target,
                           stepTriangleCurrent = step.triangle.current)
          })


## MAPPINGS TO EXPOSURE ##################################################################

## HAS_TESTS
setMethod("Mapping",
          signature(current = "Component",
                    target = "Exposure"),
          function(current, target) {
              metadata.comp <- current@metadata
              metadata.exp <- target@metadata
              dim.comp <- dim(current)
              dim.exp <- dim(target)
              dimtypes.comp <- dembase::dimtypes(current,
                                                 use.names = FALSE)
              dimtypes.exp <- dembase::dimtypes(target,
                                                use.names = FALSE)
              is.one.to.one <- identical(metadata.comp, metadata.exp)
              i.time.comp <- match("time", dimtypes.comp)
              i.time.exp <- match("time", dimtypes.exp)
              i.age.comp <- match("age", dimtypes.comp, nomatch = 0L)
              n.time <- dim.comp[i.time.comp]
              step.time.comp <- 1L
              for (d in seq_len(i.time.comp - 1L))
                  step.time.comp <- step.time.comp * dim.comp[d]
              step.time.exp <- 1L
              for (d in seq_len(i.time.exp - 1L))
                  step.time.exp <- step.time.exp * dim.exp[d]
              s.comp <- seq_along(dim.comp)
              s.exp <- seq_along(dim.exp)
              i.shared.comp <- setdiff(s.comp,
                                       c(i.time.comp))
              i.shared.exp <- setdiff(s.exp,
                                      i.time.comp)
              has.age <- i.age.comp > 0L
              if (has.age) {
                  n.age <- dim.comp[i.age.comp]
                  i.age.exp <- match("age", dimtypes.exp)
                  i.triangle.comp <- match("triangle", dimtypes.comp)
                  i.triangle.exp <- match("triangle", dimtypes.exp)
                  step.age.comp <- 1L
                  for (d in seq_len(i.age.comp - 1L))
                      step.age.comp <- step.age.comp * dim.comp[d]
                  step.age.exp <- 1L
                  for (d in seq_len(i.age.exp - 1L))
                      step.age.exp <- step.age.exp * dim.exp[d]
                  step.triangle.comp <- 1L
                  for (d in seq_len(i.triangle.comp - 1L))
                      step.triangle.comp <- step.triangle.comp * dim.comp[d]
                  step.triangle.exp <- 1L
                  for (d in seq_len(i.triangle.exp - 1L))
                      step.triangle.exp <- step.triangle.exp * dim.exp[d]
                  i.shared.comp <- setdiff(i.shared.comp,
                                           c(i.age.comp, i.triangle.comp))
                  i.shared.exp <- setdiff(i.shared.exp,
                                          c(i.age.exp, i.triangle.exp))
              }
              else {
                  n.age <- NA_integer_
                  step.age.comp <- NA_integer_
                  step.age.exp <- NA_integer_
                  step.triangle.comp <- NA_integer_
                  step.triangle.exp <- NA_integer_
              }
              n.shared.vec <- dim.comp[i.shared.comp]
              length.shared <- length(i.shared.comp)
              step.shared.comp.vec <- integer(length = length.shared)
              step.shared.exp.vec <- integer(length = length.shared)
              for (i in seq_len(length.shared)) {
                  step <- 1L
                  for (d in seq_len(i.shared.comp[i] - 1L))
                      step <- step * dim.comp[d]
                  step.shared.comp.vec[i] <- step
                  step <- 1L
                  for (d in seq_len(i.shared.exp[i] - 1L))
                      step <- step * dim.exp[d]
                  step.shared.exp.vec[i] <- step
              }
              methods::new("MappingCompToExp",
                           isOneToOne = is.one.to.one,
                           nTimeCurrent = n.time,
                           stepTimeCurrent = step.time.comp,
                           stepTimeTarget = step.time.exp,
                           nSharedVec = n.shared.vec,
                           stepSharedCurrentVec = step.shared.comp.vec,
                           stepSharedTargetVec = step.shared.exp.vec,
                           hasAge = has.age,
                           nAgeCurrent = n.age,
                           stepAgeCurrent = step.age.comp,
                           stepAgeTarget = step.age.exp,
                           stepTriangleCurrent = step.triangle.comp,
                           stepTriangleTarget = step.triangle.exp)
          })

## HAS_TESTS
setMethod("Mapping",
          signature(current = "BirthsMovements",
                    target = "Exposure"),
          function(current, target) {
              metadata.births <- current@metadata
              metadata.exp <- target@metadata
              dim.births <- dim(current)
              dim.exp <- dim(target)
              dimtypes.births <- dembase::dimtypes(current,
                                                   use.names = FALSE)
              dimtypes.exp <- dembase::dimtypes(target,
                                                use.names = FALSE)
              DimScales.births <- dembase::DimScales(current,
                                                     use.names = FALSE)
              DimScales.exp <- dembase::DimScales(target,
                                                  use.names = FALSE)
              i.time.births <- match("time", dimtypes.births)
              i.time.exp <- match("time", dimtypes.exp)
              n.time <- dim.births[i.time.births]
              step.time.births <- 1L
              for (d in seq_len(i.time.births - 1L))
                  step.time.births <- step.time.births * dim.births[d]
              step.time.exp <- 1L
              for (d in seq_len(i.time.exp - 1L))
                  step.time.exp <- step.time.exp * dim.exp[d]
              i.shared.births <- seq_along(dim.births)
              i.shared.exp <- seq_along(dim.exp)
              i.shared.births <- setdiff(i.shared.births, i.time.births)
              i.shared.exp <- setdiff(i.shared.exp, i.time.exp)
              i.age.births <- match("age", dimtypes.births, nomatch = 0L)
              has.age <- i.age.births > 0L
              if (has.age) {
                  i.age.exp <- match("age", dimtypes.exp)
                  i.triangle.births <- match("triangle", dimtypes.births)
                  i.triangle.exp <- match("triangle", dimtypes.exp)
                  DS.age.births <- DimScales.births[[i.age.births]]
                  DS.age.exp <- DimScales.exp[[i.age.exp]]
                  dv.age.births <- DS.age.births@dimvalues
                  dv.age.exp <- DS.age.exp@dimvalues
                  i.min.age <- match(dv.age.births[1L], dv.age.exp)
                  n.age <- dim.births[i.age.births]
                  step.age.births <- 1L
                  for (d in seq_len(i.age.births - 1L))
                      step.age.births <- step.age.births * dim.births[d]
                  step.age.exp <- 1L
                  for (d in seq_len(i.age.exp - 1L))
                      step.age.exp <- step.age.exp * dim.exp[d]
                  step.triangle.births <- 1L
                  for (d in seq_len(i.triangle.births - 1L))
                      step.triangle.births <- step.triangle.births * dim.births[d]
                  step.triangle.exp <- 1L
                  for (d in seq_len(i.triangle.exp - 1L))
                      step.triangle.exp <- step.triangle.exp * dim.exp[d]
                  i.shared.births <- setdiff(i.shared.births,
                                             c(i.age.births, i.triangle.births))
                  i.shared.exp <- setdiff(i.shared.exp,
                                          c(i.age.exp, i.triangle.exp))
              }
              else {
                  n.age <- NA_integer_
                  i.min.age <- NA_integer_
                  step.age.births <- NA_integer_
                  step.age.exp <- NA_integer_
                  step.triangle.births <- NA_integer_
                  step.triangle.exp <- NA_integer_
              }
              i.parent <- grep("parent", dimtypes.births)
              i.shared.births <- setdiff(i.shared.births, i.parent)
              n.shared.vec <- dim.births[i.shared.births]
              length.shared <- length(i.shared.births)
              step.shared.births.vec <- integer(length = length.shared)
              step.shared.exp.vec <- integer(length = length.shared)
              for (i in seq_len(length.shared)) {
                  step <- 1L
                  for (d in seq_len(i.shared.births[i] - 1L))
                      step <- step * dim.births[d]
                  step.shared.births.vec[i] <- step
                  step <- 1L
                  for (d in seq_len(i.shared.exp[i] - 1L))
                      step <- step * dim.exp[d]
                  step.shared.exp.vec[i] <- step
              }
              methods::new("MappingBirthsToExp",
                           isOneToOne = FALSE,
                           nTimeCurrent = n.time,
                           stepTimeCurrent = step.time.births,
                           stepTimeTarget = step.time.exp,
                           nSharedVec = n.shared.vec,
                           stepSharedCurrentVec = step.shared.births.vec,
                           stepSharedTargetVec = step.shared.exp.vec,
                           hasAge = has.age,
                           nAgeCurrent = n.age,
                           iMinAge = i.min.age,
                           stepAgeCurrent = step.age.births,
                           stepAgeTarget = step.age.exp,
                           stepTriangleCurrent = step.triangle.births,
                           stepTriangleTarget = step.triangle.exp)
          })

## HAS_TESTS
setMethod("Mapping",
          signature(current = "InternalMovementsOrigDest",
                    target = "Exposure"),
          function(current, target) {
              metadata.comp <- current@metadata
              metadata.exp <- target@metadata
              names.comp <- names(current)
              names.exp <- names(target)
              dim.comp <- dim(current)
              dim.exp <- dim(target)
              dimtypes.comp <- dembase::dimtypes(current,
                                                 use.names = FALSE)
              dimtypes.exp <- dembase::dimtypes(target,
                                                use.names = FALSE)
              i.time.comp <- match("time", dimtypes.comp)
              i.time.exp <- match("time", dimtypes.exp)
              i.age.comp <- match("age", dimtypes.comp, nomatch = 0L)
              i.orig.comp <- grep("origin", dimtypes.comp)
              base <- sub("_orig", "", names.comp[i.orig.comp])
              i.dest.comp <- match(paste(base, "dest", sep = "_"), names.comp)
              i.orig.dest.exp <- match(base, names.exp)
              n.time <- dim.comp[i.time.comp]
              step.time.comp <- 1L
              for (d in seq_len(i.time.comp - 1L))
                  step.time.comp <- step.time.comp * dim.comp[d]
              step.time.exp <- 1L
              for (d in seq_len(i.time.exp - 1L))
                  step.time.exp <- step.time.exp * dim.exp[d]
              n.orig.dest.vec <- dim.comp[i.orig.comp]
              n.orig.dest <- length(n.orig.dest.vec)
              step.orig.comp.vec <- integer(length = n.orig.dest)
              step.dest.comp.vec <- integer(length = n.orig.dest)
              step.orig.dest.exp.vec <- integer(length = n.orig.dest)
              for (i in seq_len(n.orig.dest)) {
                  step.orig.comp <- 1L
                  step.dest.comp <- 1L
                  step.orig.dest.exp <- 1L
                  for (d in seq_len(i.orig.comp[i] - 1L))
                      step.orig.comp <- step.orig.comp * dim.comp[d]
                  for (d in seq_len(i.dest.comp[i] - 1L))
                      step.dest.comp <- step.dest.comp * dim.comp[d]
                  for (d in seq_len(i.orig.dest.exp[i] - 1L))
                      step.orig.dest.exp <- step.orig.dest.exp * dim.exp[d]
                  step.orig.comp.vec[i] <- step.orig.comp
                  step.dest.comp.vec[i] <- step.dest.comp
                  step.orig.dest.exp.vec[i] <- step.orig.dest.exp
              }
              s.comp <- seq_along(dim.comp)
              s.exp <- seq_along(dim.exp)
              i.shared.comp <- setdiff(s.comp,
                                       c(i.time.comp, i.orig.comp, i.dest.comp))
              i.shared.exp <- setdiff(s.exp,
                                      c(i.time.exp, i.orig.dest.exp))
              has.age <- i.age.comp > 0L
              if (has.age) {
                  n.age <- dim.comp[i.age.comp]
                  i.age.exp <- match("age", dimtypes.exp)
                  i.triangle.comp <- match("triangle", dimtypes.comp)
                  i.triangle.exp <- match("triangle", dimtypes.exp)
                  step.age.comp <- 1L
                  for (d in seq_len(i.age.comp - 1L))
                      step.age.comp <- step.age.comp * dim.comp[d]
                  step.age.exp <- 1L
                  for (d in seq_len(i.age.exp - 1L))
                      step.age.exp <- step.age.exp * dim.exp[d]
                  step.triangle.comp <- 1L
                  for (d in seq_len(i.triangle.comp - 1L))
                      step.triangle.comp <- step.triangle.comp * dim.comp[d]
                  step.triangle.exp <- 1L
                  for (d in seq_len(i.triangle.exp - 1L))
                      step.triangle.exp <- step.triangle.exp * dim.exp[d]
                  i.shared.comp <- setdiff(i.shared.comp,
                                           c(i.age.comp, i.triangle.comp))
                  i.shared.exp <- setdiff(i.shared.exp,
                                          c(i.age.exp, i.triangle.exp))
              }
              else {
                  n.age <- NA_integer_
                  step.age.comp <- NA_integer_
                  step.age.exp <- NA_integer_
                  step.triangle.comp <- NA_integer_
                  step.triangle.exp <- NA_integer_
              }
              n.shared.vec <- dim.comp[i.shared.comp]
              length.shared <- length(i.shared.comp)
              step.shared.comp.vec <- integer(length = length.shared)
              step.shared.exp.vec <- integer(length = length.shared)
              for (i in seq_len(length.shared)) {
                  step <- 1L
                  for (d in seq_len(i.shared.comp[i] - 1L))
                      step <- step * dim.comp[d]
                  step.shared.comp.vec[i] <- step
                  step <- 1L
                  for (d in seq_len(i.shared.exp[i] - 1L))
                      step <- step * dim.exp[d]
                  step.shared.exp.vec[i] <- step
              }
              methods::new("MappingOrigDestToExp",
                           isOneToOne = FALSE,
                           nTimeCurrent = n.time,
                           stepTimeCurrent = step.time.comp,
                           stepTimeTarget = step.time.exp,
                           nOrigDestVec = n.orig.dest.vec,
                           stepOrigCurrentVec = step.orig.comp.vec,
                           stepDestCurrentVec = step.dest.comp.vec,
                           stepOrigDestTargetVec = step.orig.dest.exp.vec,
                           nSharedVec = n.shared.vec,
                           stepSharedCurrentVec = step.shared.comp.vec,
                           stepSharedTargetVec = step.shared.exp.vec,
                           hasAge = has.age,
                           nAgeCurrent = n.age,
                           stepAgeCurrent = step.age.comp,
                           stepAgeTarget = step.age.exp,
                           stepTriangleCurrent = step.triangle.comp,
                           stepTriangleTarget = step.triangle.exp)
          })

## HAS_TESTS
setMethod("Mapping",
          signature(current = "InternalMovementsPool",
                    target = "Exposure"),
          function(current, target) {
              i.direction <- current@iDirection # specific to Pool
              metadata.comp <- current@metadata
              metadata.exp <- target@metadata
              dim.comp <- dim(current)
              dim.exp <- dim(target)
              dimtypes.comp <- dembase::dimtypes(current,
                                                 use.names = FALSE)
              dimtypes.exp <- dembase::dimtypes(target,
                                                use.names = FALSE)
              i.time.comp <- match("time", dimtypes.comp)
              i.time.exp <- match("time", dimtypes.exp)
              i.age.comp <- match("age", dimtypes.comp, nomatch = 0L)
              n.time <- dim.comp[i.time.comp]
              step.time.comp <- 1L
              for (d in seq_len(i.time.comp - 1L))
                  step.time.comp <- step.time.comp * dim.comp[d]
              step.time.exp <- 1L
              for (d in seq_len(i.time.exp - 1L))
                  step.time.exp <- step.time.exp * dim.exp[d]
              s.comp <- seq_along(dim.comp)
              s.exp <- seq_along(dim.exp)
              i.shared.comp <- setdiff(s.comp,
                                       c(i.direction, i.time.comp))
              i.shared.exp <- setdiff(s.exp,
                                      i.time.comp)
              has.age <- i.age.comp > 0L
              if (has.age) {
                  n.age <- dim.comp[i.age.comp]
                  i.age.exp <- match("age", dimtypes.exp)
                  i.triangle.comp <- match("triangle", dimtypes.comp)
                  i.triangle.exp <- match("triangle", dimtypes.exp)
                  step.age.comp <- 1L
                  for (d in seq_len(i.age.comp - 1L))
                      step.age.comp <- step.age.comp * dim.comp[d]
                  step.age.exp <- 1L
                  for (d in seq_len(i.age.exp - 1L))
                      step.age.exp <- step.age.exp * dim.exp[d]
                  step.triangle.comp <- 1L
                  for (d in seq_len(i.triangle.comp - 1L))
                      step.triangle.comp <- step.triangle.comp * dim.comp[d]
                  step.triangle.exp <- 1L
                  for (d in seq_len(i.triangle.exp - 1L))
                      step.triangle.exp <- step.triangle.exp * dim.exp[d]
                  i.shared.comp <- setdiff(i.shared.comp,
                                           c(i.age.comp, i.triangle.comp))
                  i.shared.exp <- setdiff(i.shared.exp,
                                          c(i.age.exp, i.triangle.exp))
              }
              else {
                  n.age <- NA_integer_
                  step.age.comp <- NA_integer_
                  step.age.exp <- NA_integer_
                  step.triangle.comp <- NA_integer_
                  step.triangle.exp <- NA_integer_
              }
              n.shared.vec <- dim.comp[i.shared.comp]
              length.shared <- length(i.shared.comp)
              step.shared.comp.vec <- integer(length = length.shared)
              step.shared.exp.vec <- integer(length = length.shared)
              for (i in seq_len(length.shared)) {
                  step <- 1L
                  for (d in seq_len(i.shared.comp[i] - 1L))
                      step <- step * dim.comp[d]
                  step.shared.comp.vec[i] <- step
                  step <- 1L
                  for (d in seq_len(i.shared.exp[i] - 1L))
                      step <- step * dim.exp[d]
                  step.shared.exp.vec[i] <- step
              }
              methods::new("MappingCompToExp",
                           isOneToOne = FALSE,
                           nTimeCurrent = n.time,
                           stepTimeCurrent = step.time.comp,
                           stepTimeTarget = step.time.exp,
                           nSharedVec = n.shared.vec,
                           stepSharedCurrentVec = step.shared.comp.vec,
                           stepSharedTargetVec = step.shared.exp.vec,
                           hasAge = has.age,
                           nAgeCurrent = n.age,
                           stepAgeCurrent = step.age.comp,
                           stepAgeTarget = step.age.exp,
                           stepTriangleCurrent = step.triangle.comp,
                           stepTriangleTarget = step.triangle.exp)
          })


## MAPPINGS FROM EXPOSURE ################################################################

## HAS_TESTS
## should always be one-to-one, but fill in other slots to get valid object
setMethod("Mapping",
          signature(current = "Exposure",
                    target = "Component"),
          function(current, target) {
              metadata.exp <- current@metadata
              metadata.comp <- target@metadata
              is.one.to.one <- identical(metadata.exp, metadata.comp)
              if (!is.one.to.one)
                  stop(gettextf("'%s' and '%s' have different metadata",
                                "exposure", "component"))
              dim <- dim(current)
              i.shared <- seq_along(dim)
              n.shared.vec <- dim
              length.shared <- length(i.shared)
              step.shared.exp.vec <- integer(length = length.shared)
              step.shared.comp.vec <- integer(length = length.shared)
              for (i in seq_len(length.shared)) {
                  step <- 1L
                  for (d in seq_len(i.shared[i] - 1L))
                      step <- step * dim[d]
                  step.shared.exp.vec[i] <- step
                  step.shared.comp.vec[i] <- step
              }
              methods::new("MappingExpToComp",
                           isOneToOne = is.one.to.one,
                           nSharedVec = dim,
                           stepSharedCurrentVec = step.shared.exp.vec,
                           stepSharedTargetVec = step.shared.exp.vec)
          })

## HAS_TESTS
setMethod("Mapping",
          signature(current = "Exposure",
                    target = "BirthsMovements"),
          function(current, target) {
              i.min.age <- target@iMinAge
              dim.exp <- dim(current)
              dim.births <- dim(target)
              dimtypes.exp <- dembase::dimtypes(current, use.names = FALSE)
              dimtypes.births <- dembase::dimtypes(target, use.names = FALSE)
              i.time.exp <- match("time", dimtypes.exp)
              i.time.births <- match("time", dimtypes.births)
              i.age.exp <- match("age", dimtypes.exp, nomatch = 0L)
              has.age <- i.age.exp > 0L
              i.parent <- grep("parent", dimtypes.births)
              n.time <- dim.exp[i.time.exp]
              step.time.exp <- 1L
              for (d in seq_len(i.time.exp - 1L))
                  step.time.exp <- step.time.exp * dim.exp[d]
              step.time.births <- 1L
              for (d in seq_len(i.time.births - 1L))
                  step.time.births <- step.time.births * dim.births[d]
              s.exp <- seq_along(dim.exp)
              s.births <- seq_along(dim.births)
              i.shared.exp <- setdiff(s.exp, i.time.exp)
              i.shared.births <- setdiff(s.births,
                                         c(i.time.births, i.parent))
              if (has.age) {
                  i.age.births <- match("age", dimtypes.births)
                  i.triangle.exp <- match("triangle", dimtypes.exp)
                  i.triangle.births <- match("triangle", dimtypes.births)
                  n.age.exp <- dim.exp[i.age.exp]
                  n.age.births <- dim.births[i.age.births]
                  step.age.exp <- 1L
                  for (d in seq_len(i.age.exp - 1L))
                      step.age.exp <- step.age.exp * dim.exp[d]
                  step.age.births <- 1L
                  for (d in seq_len(i.age.births - 1L))
                      step.age.births <- step.age.births * dim.births[d]
                  step.triangle.exp <- 1L
                  for (d in seq_len(i.triangle.exp - 1L))
                      step.triangle.exp <- step.triangle.exp * dim.exp[d]
                  step.triangle.births <- 1L
                  for (d in seq_len(i.triangle.births - 1L))
                      step.triangle.births <- step.triangle.births * dim.births[d]
                  i.shared.exp <- setdiff(i.shared.exp,
                                          c(i.age.exp, i.triangle.exp))
                  i.shared.births <- setdiff(i.shared.births,
                                             c(i.age.births, i.triangle.births))
              }
              else {
                  n.age.exp <- NA_integer_
                  n.age.births <- NA_integer_
                  step.age.exp <- NA_integer_
                  step.age.births <- NA_integer_
                  step.triangle.exp <- NA_integer_
                  step.triangle.births <- NA_integer_
              }
              n.shared.vec <- dim.exp[i.shared.exp]
              length.shared <- length(i.shared.exp)
              step.shared.exp.vec <- integer(length = length.shared)
              step.shared.births.vec <- integer(length = length.shared)
              for (i in seq_len(length.shared)) {
                  step <- 1L
                  for (d in seq_len(i.shared.exp[i] - 1L))
                      step <- step * dim.exp[d]
                  step.shared.exp.vec[i] <- step
                  step <- 1L
                  for (d in seq_len(i.shared.births[i] - 1L))
                      step <- step * dim.births[d]
                  step.shared.births.vec[i] <- step
              }
              methods::new("MappingExpToBirths",
                           isOneToOne = FALSE,
                           nSharedVec = n.shared.vec,
                           stepSharedCurrentVec = step.shared.exp.vec,
                           stepSharedTargetVec = step.shared.births.vec,
                           nTimeCurrent = n.time,
                           stepTimeCurrent = step.time.exp,
                           stepTimeTarget = step.time.births,
                           hasAge = has.age,
                           iMinAge = i.min.age,
                           nAgeCurrent = n.age.exp,
                           nAgeTarget = n.age.births,
                           stepAgeCurrent = step.age.exp,
                           stepAgeTarget = step.age.births,
                           stepTriangleCurrent = step.triangle.exp,
                           stepTriangleTarget = step.triangle.births)
          })              

## HAS_TESTS
setMethod("Mapping",
          signature(current = "Exposure",
                    target = "InternalMovementsOrigDest"),
          function(current, target) {
              dim.exp <- dim(current)
              dim.comp <- dim(target)
              dimtypes.comp <- dembase::dimtypes(target, use.names = FALSE)
              i.dest <- grep("destination", dimtypes.comp)
              i.shared.exp <- seq_along(dim.exp)
              i.shared.comp <- seq_along(dim.comp)
              i.shared.comp <- setdiff(i.shared.comp, i.dest)
              n.shared.vec <- dim.exp[i.shared.exp]
              length.shared <- length(i.shared.exp)
              step.shared.exp.vec <- integer(length = length.shared)
              step.shared.comp.vec <- integer(length = length.shared)
              for (i in seq_len(length.shared)) {
                  step <- 1L
                  for (d in seq_len(i.shared.exp[i] - 1L))
                      step <- step * dim.exp[d]
                  step.shared.exp.vec[i] <- step
                  step <- 1L
                  for (d in seq_len(i.shared.comp[i] - 1L))
                      step <- step * dim.comp[d]
                  step.shared.comp.vec[i] <- step
              }
              methods::new("MappingExpToComp",
                           isOneToOne = FALSE,
                           nSharedVec = n.shared.vec,
                           stepSharedCurrentVec = step.shared.exp.vec,
                           stepSharedTargetVec = step.shared.comp.vec)
          })

## HAS_TESTS
setMethod("Mapping",
          signature(current = "Exposure",
                    target = "InternalMovementsPool"),
          function(current, target) {
              dim.exp <- dim(current)
              dim.comp <- dim(target)
              i.direction <- target@iDirection
              i.shared.exp <- seq_along(dim.exp)
              i.shared.comp <- seq_along(dim.comp)
              i.shared.comp <- setdiff(i.shared.comp, i.direction)
              n.shared.vec <- dim.exp[i.shared.exp]
              length.shared <- length(i.shared.exp)
              step.shared.exp.vec <- integer(length = length.shared)
              step.shared.comp.vec <- integer(length = length.shared)
              for (i in seq_len(length.shared)) {
                  step <- 1L
                  for (d in seq_len(i.shared.exp[i] - 1L))
                      step <- step * dim.exp[d]
                  step.shared.exp.vec[i] <- step
                  step <- 1L
                  for (d in seq_len(i.shared.comp[i] - 1L))
                      step <- step * dim.comp[d]
                  step.shared.comp.vec[i] <- step
              }
              methods::new("MappingExpToComp",
                           isOneToOne = FALSE,
                           nSharedVec = n.shared.vec,
                           stepSharedCurrentVec = step.shared.exp.vec,
                           stepSharedTargetVec = step.shared.comp.vec)
          })
