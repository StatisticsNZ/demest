
#' @rdname SummaryResults-class
#' @export
setMethod("show",
          signature(object = "SummaryModel"),
          function(object) {
              specification <- object@specification
              dimensions <- object@dimensions
              cat(specification, "\n", sep = "")
              cat("dimensions: ", paste(dimensions, collapse = ", "), "\n", sep = "")
          })

#' @rdname SummaryResults-class
#' @export
setMethod("show",
          signature(object = "SummaryDataset"),
          function(object) {
              classStr <- object@classStr
              dimensions <- object@dimensions
              nCell <- object@nCell
              nMissing <- object@nMissing
              isIntegers <- object@isIntegers
              nZero <- object@nZero
              median <- object@median
              cat("Object of class \"", classStr, "\"\n", sep = "")
              cat("dimensions: ", paste(dimensions, collapse = ", "), "\n", sep = "")
              cat("n cells: ", nCell, ",  n missing: ", nMissing, sep = "")
              all.missing <- nMissing == nCell
              if (all.missing)
                  cat("\n")
              else {
                  cat(", integers: ", isIntegers, sep = "")
                  if (isIntegers)
                      cat(", n zeros: ", nZero, sep = "")
                  cat(", median: ", median, "\n", sep = "")
              }
          })

#' @rdname SummaryResults-class
#' @export
setMethod("show",
          signature(object = "SummarySeries"),
          function(object) {
              dimensions <- object@dimensions
              nCell <- object@nCell
              cat("dimensions: ", paste(dimensions, collapse = ", "), "\n", sep = "")
              cat("n cells: ", nCell, "\n", sep = "")
          })

#' @rdname SummaryResults-class
#' @export
setMethod("show",
          signature(object = "SummaryResultsModelEst"),
          function(object) {
              kDigits <- 4L
              kDivider <- paste(paste(rep("-", times = 50), collapse = ""), "\n", collapse = "")
              mcmc <- object@mcmc
              parameters <- object@parameters
              metropolis <- object@metropolis
              gelmanDiag <- object@gelmanDiag
              model <- object@model
              y <- object@y
              cat(kDivider)
              cat("model:\n")
              print(model)
              cat(kDivider)
              cat("y:\n")
              print(y)
              cat(kDivider)
              cat("MCMC statistics:\n")
              cat(paste(paste(names(mcmc), mcmc, sep = ": "),
                        collapse = ",  "),
                  "\n")
              if (!is.null(metropolis)) {
                  cat("\nMetropolis-Hastings updates:\n")
                  metropolis <- round(metropolis, digits = kDigits)
                  print(metropolis)
              }
              if (!is.null(gelmanDiag)) {
                  cat("\nparameters:\n")
                  parameters <- round(parameters, digits = kDigits)
                  Rhat <- round(gelmanDiag, digits = kDigits - 1L)
                  dot <- ifelse(Rhat < 1.1, "", ".")
                  df <- data.frame(" " = dot,
                                   Rhat = Rhat,
                                   parameters,
                                   check.names = FALSE)
                  print(df)
              }
              cat(kDivider)
          })

#' @rdname SummaryResults-class
#' @export
setMethod("show",
          signature(object = "SummaryResultsModelPred"),
          function(object) {
              kDigits <- 4L
              kDivider <- paste(paste(rep("-", times = 50), collapse = ""), "\n", collapse = "")
              mcmc <- object@mcmc
              parameters <- object@parameters
              model <- object@model
              cat(kDivider)
              cat("model:\n")
              print(model)
              cat(kDivider)
              cat("MCMC statistics:\n")
              cat(paste(paste(names(mcmc), mcmc, sep = ": "),
                        collapse = ",  "),
                  "\n")
              cat(kDivider)
              cat("parameters:\n")
              parameters <- round(parameters, digits = kDigits)
              print(parameters)
              cat(kDivider)
          })

#' @rdname SummaryResults-class
#' @export
setMethod("show",
          signature(object = "SummaryResultsCounts"),
          function(object) {
              kDigits <- 4L
              kDivider <- paste(paste(rep("-", times = 50),
                                      collapse = ""),
                                "\n",
                                collapse = "")
              mcmc <- object@mcmc
              metropolis <- object@metropolis
              gelmanDiag <- object@gelmanDiag
              parameters <- object@parameters
              model <- object@model
              y <- object@y
              observation <- object@observation
              datasets <- object@datasets
              namesDatasets <- object@namesDatasets
              cat(kDivider)
              cat("model:\n")
              print(model)
              cat(kDivider)
              cat("y:\n")
              print(y)
              cat(kDivider)
              cat("Observation models:\n")
              for (i in seq_along(observation)) {
                  cat("*", namesDatasets[i], "*\n", sep = "")
                  print(observation[[i]])
              }
              cat(kDivider)
              cat("Datasets:\n")
              for (i in seq_along(datasets)) {
                  cat("*", namesDatasets[i], "*\n", sep = "")
                  print(datasets[[i]])
              }
              cat(kDivider)
              cat("MCMC statistics:\n")
              cat(paste(paste(names(mcmc), mcmc, sep = ": "),
                        collapse = ",  "),
                  "\n")
              if (!is.null(metropolis)) {
                  cat("\nMetropolis-Hastings updates:\n")
                  metropolis <- round(metropolis, digits = kDigits)
                  print(metropolis)
              }
              if (!is.null(gelmanDiag)) {
                  cat("\nparameters:\n")
                  parameters <- round(parameters, digits = kDigits)
                  Rhat <- round(gelmanDiag, digits = kDigits - 1L)
                  df <- data.frame(Rhat = Rhat,
                                   parameters,
                                   check.names = FALSE)
                  print(df)
              }
              cat(kDivider)
          })



