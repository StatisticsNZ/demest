
## HAS_TESTS
setMethod("makeOutputModel",
          signature(model = "PoissonVarying"),
          function(model, pos, mcmc) {
              theta <- model@theta
              nu.cmp <- model@nuCMP
              metadata <- model@metadataY
              scale.theta <- model@scaleTheta@.Data
              betas.obj <- model@betas
              priors.betas <- model@priorsBetas
              names.betas <- model@namesBetas
              margins <- model@margins
              dims <- model@dims
              struc.zero.array <- model@strucZeroArray
              n.beta <- length(betas.obj)
              n.attempt <- as.integer(prod(dim(metadata)))
              nChain <- mcmc["nChain"]
              nIteration <- mcmc["nIteration"]
              uses.exposure <- methods::is(model, "UseExposure")
              ## make theta
              first <- pos
              pos <- first + length(theta)
              class <- if (uses.exposure) "Values" else "Counts"
              .Data <- array(theta,
                             dim = dim(metadata),
                             dimnames = dimnames(metadata))
              theta <- methods::new(class,
                                    .Data = .Data,
                                    metadata = metadata)
              s <- seq_along(dim(metadata))
              theta <- Skeleton(object = theta,
                                first = first,
                                strucZeroArray = struc.zero.array,
                                margin = s)
              ## make nuCMP
              first <- pos
              pos <- first + length(theta)
              .Data <- array(nu.cmp,
                             dim = dim(metadata),
                             dimnames = dimnames(metadata))
              nu.cmp <- methods::new("Values",
                                     .Data = .Data,
                                     metadata = metadata)
              s <- seq_along(dim(metadata))
              nu.cmp <- Skeleton(object = nu.cmp,
                                 first = first,
                                 strucZeroArray = struc.zero.array,
                                 margin = s)
              ## make nFailedPropTheta
              first <- pos
              pos <- first + 1L
              fail.prop.theta <- SkeletonAccept(nAttempt = n.attempt,
                                                first = first,
                                                nChain = nChain,
                                                nIteration = nIteration)
              ## make nAcceptTheta
              first <- pos
              pos <- first + 1L
              accept.theta <- SkeletonAccept(nAttempt = n.attempt,
                                             first = first,
                                             nChain = nChain,
                                             nIteration = nIteration)
              ## make mu and betas
              first <- pos
              pos <- pos + 1L
              mu <- SkeletonMu(betas = betas.obj,
                               margins = margins,
                               first = first,
                               metadata = metadata,
                               strucZeroArray = struc.zero.array)
              betas <- vector(mode = "list", length = n.beta)
              betas[[1L]] <- SkeletonBetaIntercept(first = first)
              if (n.beta > 1L) {
                  for (i in seq_len(n.beta)[-1L]) {
                      first <- pos
                      pos <- first + length(betas.obj[[i]])
                      margin <- margins[[i]]
                      betas[[i]] <- SkeletonBetaTerm(first = first,
                                                     metadata = metadata[margin],
                                                     strucZeroArray = struc.zero.array,
                                                     margin = margin)
                  }
              }
              names(betas) <- names.betas
              ## make sigma
              first <- pos
              pos <- first + 1L
              sigma <- Skeleton(first = first)
              ## make hyper
              hyper <- vector(mode = "list", length = n.beta)
              for (i in seq_len(n.beta)) {
                  if (i == 1L)
                      metadata.i <- NULL
                  else {
                      margin <- margins[[i]]
                      metadata.i <- metadata[margin]
                  }
                  hyper[i] <- list(makeOutputPrior(priors.betas[[i]],
                                                   metadata = metadata.i,
                                                   pos = pos,
                                                   strucZeroArray = struc.zero.array,
                                                   margin = margin))
                  pos <- pos + changeInPos(hyper[[i]])
              }
              names(hyper) <- names.betas
              ## return value
              likelihood <- list(theta,
                                 jumpMean = scale.theta,
                                 noProposal = fail.prop.theta,
                                 acceptMean = accept.theta)
              if (uses.exposure)
                  names <- c("rate", "jumpRate", "noProposal", "acceptRate")
              else
                  names <- c("count", "jumpCount", "noProposal", "acceptCount")
              names(likelihood) <- names
              prior <- c(betas, list(mean = mu), list(sd = sigma))
              ans <- list(likelihood = likelihood, prior = prior, hyper = hyper)
              if (methods::is(model, "Aggregate")) {
                  aggregate <- makeOutputAggregate(model = model,
                                                   pos = pos,
                                                   nChain = nChain,
                                                   nIteration = nIteration)
                  ans <- c(ans, list(aggregate = aggregate))
              }
              ans
          })
