
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
              kDigits <- 3L
              kDivider <- paste(paste(rep("-", times = 65), collapse = ""), "\n", collapse = "")
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
                  parameters[] <- lapply(parameters, formatC, digits = kDigits, format = "fg")
                  parameters[] <- lapply(parameters, function(x) sub("NA", "  ", x))
                  is.num <- sapply(gelmanDiag, is.numeric)
                  gelmanDiag[is.num] <- lapply(gelmanDiag[is.num], round, digits = kDigits - 1L)
                  gap <- data.frame(gap = rep("  ", times = nrow(parameters)),
                                    stringsAsFactors = FALSE)
                  df <- as.data.frame(list(gelmanDiag, gap, parameters))
                  names(df) <- c("", "med", "max", "n/N", "", "min", "med", "max")
                  df <- rbind(names(df), df)
                  row.names(df)[1L] <- ""
                  names(df) <- c("", "", "Rhat", "", "", "", "Est.", "")
                  print(df)
              }
              cat(kDivider)
          })

#' @rdname SummaryResults-class
#' @export
setMethod("show",
          signature(object = "SummaryResultsModelPred"),
          function(object) {
              kDigits <- 3L
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
              parameters[] <- lapply(parameters, formatC, digits = kDigits, format = "fg")
              print(parameters)
              cat(kDivider)
          })

#' @rdname SummaryResults-class
#' @export
setMethod("show",
          signature(object = "SummaryResultsCounts"),
          function(object) {
              kDigits <- 3L
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
              dataModels <- object@dataModels
              datasets <- object@datasets
              namesDatasets <- object@namesDatasets
              cat(kDivider)
              cat("model:\n")
              print(model)
              cat(kDivider)
              cat("y:\n")
              print(y)
              cat(kDivider)
              cat("Data models:\n")
              for (i in seq_along(dataModels)) {
                  cat("*", namesDatasets[i], "*\n", sep = "")
                  print(dataModels[[i]])
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
                  parameters[] <- lapply(parameters, formatC, digits = kDigits, format = "fg")
                  Rhat <- round(gelmanDiag, digits = kDigits - 1L)
                  dot <- ifelse(Rhat[ , "max"] < 1.1, "", ".")
                  df <- data.frame(" " = dot,
                                   "Rhat med" = Rhat[ , "median"],
                                   "Rhat max" = Rhat[ , "max"],
                                   parameters,
                                   check.names = FALSE)
                  print(df)
              }
              cat(kDivider)
          })



#' @rdname SummaryResults-class
#' @export
setMethod("show",
          signature(object = "SummaryResultsAccount"),
          function(object) {
              kDigits <- 3L
              kDivider <- paste(paste(rep("-", times = 50),
                                      collapse = ""),
                                "\n",
                                collapse = "")
              mcmc <- object@mcmc
              metropolis <- object@metropolis
              gelmanDiag <- object@gelmanDiag
              parameters <- object@parameters
              account <- object@account
              system.models <- object@systemModels
              names.series <- object@namesSeries
              datasets <- object@datasets
              data.models <- object@dataModels
              names.datasets <- object@namesDatasets
              cat(kDivider)
              cat("Account:\n")
              for (i in seq_along(account)) {
                  cat("*", names.series[i], "*\n", sep = "")
                  print(account[[i]])
              }
              cat(kDivider)
              cat("System models:\n")
              for (i in seq_along(system.models)) {
                  cat("*", names.series[i], "*\n", sep = "")
                  print(system.models[[i]])
              }
              cat(kDivider)
              cat("Datasets:\n")
              for (i in seq_along(datasets)) {
                  cat("*", names.datasets[i], "*\n", sep = "")
                  print(datasets[[i]])
              }
              cat(kDivider)
              cat("Data models:\n")
              for (i in seq_along(data.models)) {
                  cat("*", names.datasets[i], "*\n", sep = "")
                  print(data.models[[i]])
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
                  parameters[] <- lapply(parameters, formatC, digits = kDigits, format = "fg")
                  Rhat <- round(gelmanDiag, digits = kDigits - 1L)
                  dot <- ifelse(Rhat[ , "max"] < 1.1, "", ".")
                  df <- data.frame(" " = dot,
                                   "Rhat med" = Rhat[ , "median"],
                                   "Rhat max" = Rhat[ , "max"],
                                   parameters,
                                   check.names = FALSE)
                  print(df)
              }
              cat(kDivider)
          })




