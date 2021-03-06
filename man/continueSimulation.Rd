% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate-functions.R
\name{continueSimulation}
\alias{continueSimulation}
\title{Add extra iterations to simulation}
\usage{
continueSimulation(
  filename,
  nBurnin = NULL,
  nSim = 1000,
  nThin = NULL,
  parallel = NULL,
  scaleNoise = 0,
  outfile = NULL,
  verbose = FALSE,
  useC = TRUE
)
}
\arguments{
\item{filename}{The name of a file where output is collected.}

\item{nBurnin}{Number of iteration discarded before recording begins.}

\item{nSim}{Number of iterations carried out during recording.}

\item{nThin}{Thinning interval.}

\item{parallel}{Logical.  If \code{TRUE} (the default), parallel processing
is used.}

\item{scaleNoise}{Governs noise added to Metropolis-Hastings
ratio when updating accounts. Should only be used non-zero
when generating initial values. Currently experimental,
and may change.}

\item{outfile}{Where to direct the ‘stdout’ and ‘stderr’ connection
output from the workers when parallel processing.  Passed to function
\code{[parallel]{makeCluster}}.}

\item{verbose}{Logical.  If \code{TRUE} (the default) a message is
printed at the end of the calculations.}

\item{useC}{Logical.  If \code{TRUE} (the default), the calculations
are done in C.  Setting \code{useC} to \code{FALSE} may be useful
for debugging.}
}
\description{
WARNING - THIS FUNCTION IS STILL UNDER DEVELOPMENT
}
