
## HAS_TESTS
setMethod("Description",
          signature(object = "Population"),
          function(object) {
              dim <- dim(object)
              dimtypes <- dembase::dimtypes(object, use.names = FALSE)
              i.time <- match("time", dimtypes)
              n.time <- dim[i.time]
              step.time <- 1L
              for (d in seq_len(i.time - 1L))
                  step.time <- step.time * dim[d]
              i.age <- match("age", dimtypes, nomatch = 0L)
              has.age <- i.age > 0L
              if (has.age) {
                  n.age <- dim[i.age]
                  step.age <- 1L
                  for (d in seq_len(i.age - 1L))
                      step.age <- step.age * dim[d]
              }
              else {
                  n.age <- as.integer(NA)
                  step.age <- as.integer(NA)
              }
              length <- length(object)
              methods::new("DescriptionPopn",
                  nTime = n.time,
                  stepTime = step.time,
                  hasAge = has.age,
                  nAge = n.age,
                  stepAge = step.age,
                  length = length)
          })

## HAS_TESTS
setMethod("Description",
          signature(object = "Component"),
          function(object) {
              dim <- dim(object)
              dimtypes <- dembase::dimtypes(object, use.names = FALSE)
              i.time <- match("time", dimtypes)
              n.time <- dim[i.time]
              step.time <- 1L
              for (d in seq_len(i.time - 1L))
                  step.time <- step.time * dim[d]
              i.age <- match("age", dimtypes, nomatch = 0L)
              has.age <- i.age > 0L
              if (has.age) {
                  n.age <- dim[i.age]
                  step.age <- 1L
                  for (d in seq_len(i.age - 1L))
                      step.age <- step.age * dim[d]
                  i.triangle <- match("triangle", dimtypes)
                  step.triangle <- 1L
                  for (d in seq_len(i.triangle - 1L))
                      step.triangle <- step.triangle * dim[d]
              }
              else {
                  n.age <- as.integer(NA)
                  step.age <- as.integer(NA)
                  step.triangle <- as.integer(NA)
              }
              length <- length(object)
              methods::new("DescriptionComp",
                  nTime = n.time,
                  stepTime = step.time,
                  hasAge = has.age,
                  nAge = n.age,
                  stepAge = step.age,
                  stepTriangle = step.triangle,
                  length = length)
          })

## HAS_TESTS
setMethod("Description",
          signature(object = "InternalMovementsPool"),
          function(object) {
              dim <- dim(object)
              dimtypes <- dembase::dimtypes(object, use.names = FALSE)
              i.direction <- object@iDirection
              i.between <- object@iBetween
              i.time <- match("time", dimtypes)
              n.time <- dim[i.time]
              step.time <- 1L
              for (d in seq_len(i.time - 1L))
                  step.time <- step.time * dim[d]
              i.age <- match("age", dimtypes, nomatch = 0L)
              has.age <- i.age > 0L
              if (has.age) {
                  n.age <- dim[i.age]
                  step.age <- 1L
                  for (d in seq_len(i.age - 1L))
                      step.age <- step.age * dim[d]
                  i.triangle <- match("triangle", dimtypes)
                  step.triangle <- 1L
                  for (d in seq_len(i.triangle - 1L))
                      step.triangle <- step.triangle * dim[d]
              }
              else {
                  n.age <- as.integer(NA)
                  step.age <- as.integer(NA)
                  step.triangle <- as.integer(NA)
              }
              step.direction <- 1L
              for (d in seq_len(i.direction - 1L))
                  step.direction <- step.direction * dim[d]
              n.between.vec <- dim[i.between]
              step.between.vec <- integer(length = length(i.between))
              for (i in seq_along(i.between)) {
                  step.between <- 1L
                  for (d in seq_len(i.between[i] - 1L))
                      step.between <- step.between * dim[d]
                  step.between.vec[i] <- step.between
              }
              s <- seq_along(dim)
              i.within <- setdiff(s, c(i.between, i.direction))
              n.within.vec <- dim[i.within]
              step.within.vec <- integer(length = length(i.within))
              for (i in seq_along(i.within)) {
                  step.within <- 1L
                  for (d in seq_len(i.within[i] - 1L))
                      step.within <- step.within * dim[d]
                  step.within.vec[i] <- step.within
              }
              length <- length(object)
              methods::new("DescriptionPool",
                  nTime = n.time,
                  stepTime = step.time,
                  hasAge = has.age,
                  nAge = n.age,
                  stepAge = step.age,
                  stepTriangle = step.triangle,
                  stepDirection = step.direction,
                  nBetweenVec = n.between.vec,
                  stepBetweenVec = step.between.vec,
                  nWithinVec = n.within.vec,
                  stepWithinVec = step.within.vec,
                  length = length)
          })


              

              
