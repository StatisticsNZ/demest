
## ## NO_TESTS
## setMethod("fetchInnerFake",
##           signature(object = "numeric"),
##           function(object, nameObject, where) {
##               n.where <- length(where)
##               if (n.where == 0L)
##                   object
##               else
##                   raiseOvershotError(nameObject = nameObject, where = where)
##           })

## ## NO_TESTS
## setMethod("fetchInnerFake",
##           signature(object = "DemographicArray"),
##           function(object, nameObject, where) {
##               n.where <- length(where)
##               if (n.where == 0L)
##                   object
##               else
##                   raiseOvershotError(nameObject = nameObject, where = where)
##           })

## ## NO_TESTS
## setMethod("fetchInnerFake",
##           signature(object = "list"),
##           function(object, nameObject, where) {
##               n.where <- length(where)
##               choices <- names(object)
##               if (n.where == 0L)
##                   raiseMultipleChoicesError(choices = choices)
##               else {
##                   name <- where[1L]
##                   i <- charmatch(name, choices, nomatch = -1L)
##                   if (i > 0L) {
##                       name <- choices[i]
##                       fetchInnerFake(object = object[[name]],
##                                      nameObject = name,
##                                      where = where[-1L])
##                   }
##                   else if (i == 0L)
##                       raiseMultipleMatchesError(target = name, choices = choices)
##                   else
##                       raiseNotFoundError(target = name, choices = choices)
##               }
##           })

## ## ## NO_TESTS
## ## setMethod("fetch",
## ##           signature(object = "FakeData"),
## ##           function(object, where = character()) {
## ##               where <- as.character(where)
## ##               if (any(is.na(where)))
## ##                   stop(gettextf("'%s' has missing values",
## ##                                 "where"))
## ##               n.where <- length(where)
## ##               choices <- methods::slotNames(object)
## ##               if (n.where == 0L)
## ##                   stop(gettextf("'%s' has length %d",
## ##                                 "where", 0L))
## ##               name <- where[1L]
## ##               i <- charmatch(name, choices, nomatch = -1L)
## ##               if (i > 0L) {
## ##                   name <- choices[i]
## ##                   fetchInnerFake(object = methods::slot(object, name),
## ##                                  nameObject = name,
## ##                                  where = where[-1L])
## ##               }
## ##               else if (i == 0L)
## ##                       raiseMultipleMatchesError(target = name, choices = choices)
## ##                   else
## ##                       raiseNotFoundError(target = name, choices = choices)
## ##           })

## ## NO_TESTS
## setMethod("listContents",
##           signature(object = "FakeData"),
##           function(object, where = character(), max = NULL) {
##               checkMax(max)
##               l <- makeContentsList(object = object, where = where, max = max)
##               call.here <- match("call", names(l))
##               l <- l[-call.here]
##               showContentsList(l)
##               invisible(l)
##           })


## setMethod("show",
##           signature(object = "FakeData"),
##           function(object) {
##               call <- object@call
##               cat("An object of class \"", class(object), "\"\n", sep = "")
##               cat(deparse(call), "\n")
##           })
