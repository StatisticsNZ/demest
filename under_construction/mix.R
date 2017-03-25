                                   

setClass("PriorSampleSizesMixin",
         contains = "VIRTUAL",
         slots = c(priorSuccesses = "Scale",
                   priorFailures = "Scale"))

setClass("VectorsMultinomMixin",
         contains = c("VIRTUAL",
                      "VectorsMixMixin"),
         validity = function(object) {
           vectorsMix <- object@vectorsMix
           for (i in seq_along(vectorsMix)) {
             vector <- vectorsMix[[i]]
             if (any(vector < 0))
               return(gettextf("vector %d has elements with negative values",
                               i))
           }
           TRUE
         })

setClass("ShapeComponentWeightMixMixin",
         contains = "VIRTUAL",
         slots = c(shapeComponentWeightMix = "Scale"))

setClass("PermMultinomMixin",
         contains = "VIRTUAL",
         slots(permMultinom = "integers"),
         validity = function(object) {
             permMultinom <- object@permMultinom
             metadataY <- object@metadataY
             ## 'permMultinom' consistent with 'metadataY'
             dim <- dim(metadataY)
             s <- seq_along(dim)
             if (!identical(order(permMultinom), s))
                 return(gettextf("'%s' not consistent with '%s'",
                                 "permMultinom", "metadataY"))
             TRUE
         })
                 
setClass("DimPermMultinomMixin",
         contains = "VIRTUAL",
         slots = c(dimPermMultinom = "integer"))
         validity = function(object) {
             dimPermMultinom <- object@dimPermMultinom
             permMultinom <- object@permMultinom
             metadataY <- object@metadataY
             ## 'dimPermMultinom' consistent with 'metadataY'
             dim <- dim(metadataY)
             dim.expected <- dim[permMultinom]
             if (!identical(dimPermMultinom, dim.expected)) {
                 dim.expected <- c(as.integer(prod(dim.expected[1:2])),
                                   dim.expected[-(1:2)])
                 if (!identical(dimPermMultinom, dim.expected))
                     return(gettextf("'%s' not consistent with '%s' and '%s'",
                                     "dimPermMultinom", "permMultinom", "metadataY"))
             }
             TRUE
         })


setClass("MixMixin",
         contains = c("VIRTUAL",
                      "AlphaMixMixin",
                      "ComponentFlags",
                      "ComponentWeightMixMixin",
                      "DimBetaMixin",
                      "FoundIndexClassMaxPossibleMixMixin",
                      "IndexClassMaxMixMixin",
                      "IndexClassMaxPossibleMixMixin",
                      "IndexClassMaxUsedMixMixin",
                      "IndexClassMixMixin",
                      "IndexClassProbMixMixin",
                      "IteratorProdVectorMix",
                      "IteratorsDimsMixMixin",
                      "LatentWeightMixMixin",
                      "PosProdVectorsMixMixin",
                      "ProdVectorsMixMixin",
                      "ToleranceMixin",
                      "VectorsMixMixin",
                      "WeightMixMixin"),
         validity = function(object) {
           ## at least 2 dimensions
           TRUE
         })

setClass("MixVaryingMixin",
         contains = c("VIRTUAL",
                      "AComponentWeightMixMixin",
                      "ALevelComponentWeightMixMixin",
                      "aMixMixin",
                      "AVectorsMixMixin",
                      "AlphaMixMixin",
                      "CMixMixin",
                      "IAlongMixin",
                      "LatentComponentWeightMixMixin",
                      "LevelComponentWeightMinMaxMixin",
                      "LevelComponentWeightMixMixin",
                      "mMixMixin",
                      "MeanLevelComponentWeightMixMixin",
                      "NuComponentWeightMixMixin",
                      "NuLevelComponentWeightMixMixin",
                      "NuVectorsMixMixin",
                      "OmegaComponentWeightMaxMixMixin",
                      "OmegaComponentWeightMixMixin",
                      "OmegaLevelComponentWeightMaxMixMixin",
                      "OmegaLevelComponentWeightMixMixin",
                      "OmegaVectorsMaxMixMixin",
                      "OmegaVectorsMixMixin",
                      "PhiMixMixin",
                      "PriorMeanLevelComponentWeightMixMixin",
                      "PriorSDLevelComponentWeightMixMixin",
                      "RMixMixin",
                      "SumsWeightsMixMixin",
                      "YXXXMixMixin"))

setClass("MixFixedMixin",
         contains = c("VIRTUAL",
                      "PriorSampleSizesMixin",
                      "ShapeComponentWeightMixMixin"))

setClass("Mix",
         contains = "MixVaryingMixin")

setClass("MixFixed",
         contains = "MixFixedMixin")

setClass("Multinomial",
         contains = c("VIRTUAL",
                      "Model",
                      "NotUseExposure"))

setClass("MultinomialVarying",
         contains = c("Multinomial",
                      "MixVaryingMixin"))

setClass("MultinomialFixed",
         contains = c("Multinomial",
                      "MixFixedMixin"))


betaHatAlphaMix <- function(prior, useC = FALSE) {
    stopifnot(methods::is(prior, "Prior"))
    stopifnot(methods::is(prior, "ComponentFlags"))
    stopifnot(prior@hasAlphaMix)
    if (useC) {
        .Call(betaHatAlphaMix_R, prior)
    }
    else {
        prior@alphaMix@.Data
    }
}

setClass("VectorsMultinomMixin",
         contains = c("VIRTUAL",
                      "VectorsMixMixin"),
         validity = function(object) {
             vectorsMix <- object@vectorsMix
             ## all elements of 'vectorsMix' are non-negative
             for (i in seq_along(vectorsMix)) {
                 vector <- vectorsMix[[i]]
                 if (any(vector < 0))
                     return(gettextf("element of %d of '%s' has negative values",
                                     i, "vectorsMix"))
             }
             TRUE
         })

updateVectorsMultinomAndProdVectorsMix <- function(prior, y, useC = FALSE) {
    ## 'prior'
    stopifnot(methods::is(prior, "Mix"))
    stopifnot(validObject(prior))
    ## 'betaTilde'
    stopifnot(is.double(y))
    stopifnot(!any(is.na(y)))
    stopifnot(identical(length(betaTilde), prior@J@.Data))
    if (useC) {
        .Call(updateVectorsMixAndProdVectorsMix_R, prior, betaTilde)
    }
    else {    
        vectors <- prior@vectorsMix
        omega.vectors <- prior@omegaVectorsMix@.Data
        iterators.dims <- prior@iteratorsDimsMix
        prod.vectors <- prior@prodVectorsMix@.Data
        iterator.prod <- prior@iteratorProdVectorMix
        index.class <- prior@indexClassMix
        index.class.max.used <- prior@indexClassMaxUsedMix@.Data
        iAlong <- prior@iAlong
        dim.beta <- prior@dimBeta
        pos1 <- prior@posProdVectors1Mix
        pos2 <- prior@posProdVectors2Mix
        n.beta.no.along <- prior@nBetaNoAlongMix
        yX <- prior@yXMix@.Data # length index.class.max
        XX <- prior@XXMix@.Data # length index.class.max
        prec.prior <- 1 / omega.vectors^2
        v <- getV(prior)
        ## loop through vectors, skipping position occupied by "along" dimension
        for (i in seq_along(vectors)) {
            if (i == iAlong)
                next
            vector <- vectors[[i]]@.Data
            n.element.vector <- dim.beta[i]
            iterator.beta <- iterators.dims[[i]]
            iterator.beta <- resetS(iterator.beta)
            ## update i'th vector
            for (i.element.vector in seq_len(n.element.vector)) {
                ## reset vectors holding statistics for updating
                for (i.class in seq_len(index.class.max.used)) {
                    yX[i.class] <- 0
                    XX[i.class] <- 0
                }
                ## Loop along selected elements of 'betaTilde', 'v', and 'index.class',
                ## plus associated elements from prod.vector, to calculate
                ## statistics needed for updating i.element.vector'th slice of i'th vector.
                indices.beta <- iterator.beta@indices
                for (i.beta in indices.beta) {
                    beta.tilde.i.beta <- betaTilde[i.beta]
                    v.i.beta <- v[i.beta]
                    i.class <- index.class[i.beta]
                    i.vector <- (i.class - 1L) * n.element.vector + i.element.vector
                    val.vector <- vector[i.vector]
                    i.beta.no.along <- ((i.beta - 1L) %/% pos1) * pos2 + (i.beta - 1L) %% pos2 + 1L
                    i.prod <- (i.class - 1L) * n.beta.no.along + i.beta.no.along
                    val.prod.vector <- prod.vectors[i.prod]
                    X <- val.prod.vector / val.vector
                    yX[i.class] <- yX[i.class] + beta.tilde.i.beta * X  / v.i.beta
                    XX[i.class] <- XX[i.class] + X * X / v.i.beta
                }
                iterator.beta <- advanceS(iterator.beta)
                ## Update this slice.
                for (i.class in seq_len(index.class.max.used)) {
                    prec.data <- XX[i.class]
                    i.vector <- (i.class - 1L) * n.element.vector + i.element.vector
                    var <- 1 / (prec.data + prec.prior)
                    sd <- sqrt(var)
                    mean <- var * yX[i.class]
                    vector[i.vector] <- rnorm(n = 1L,
                                              mean = mean,
                                              sd = sd)
                }
            }
            vectors[[i]]@.Data <- vector
            ## Update 'prod.vectors'. We calculate 'prod.vectors' from scratch,
            ## rather than by scaling up or down in proportion to the change
            ## in vector i, to avoid the possibility of small numerical
            ## inconsistencies creeping in.
            for (i.class in seq_len(index.class.max.used)) {
                iterator.prod <- resetM(iterator.prod)
                for (i.beta.no.along in seq_len(n.beta.no.along)) {
                    indices.vectors <- iterator.prod@indices
                    i.prod.vectors <- (i.class - 1L) * n.beta.no.along + i.beta.no.along
                    prod.values <- 1
                    for (i.pv in seq_along(vectors)) {
                        if (i.pv == iAlong)
                            next
                        vector <- vectors[[i.pv]]@.Data
                        n.element.vector.pv <- dim.beta[i.pv]
                        i.element.vector.pv <- indices.vectors[i.pv]
                        i.vector.pv <- ((i.class - 1L) * n.element.vector.pv
                            + i.element.vector.pv)
                        value <- vector[i.vector.pv]
                        prod.values <- prod.values * value
                    }
                    prod.vectors[i.prod.vectors] <- prod.values
                    iterator.prod <- advanceM(iterator.prod)
                }
            }
        }
        ## update slots in prior
        prior@vectorsMix <- vectors
        prior@prodVectorsMix@.Data <- prod.vectors
        prior
    }
}
