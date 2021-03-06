% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/query-functions.R
\name{fetchSummary}
\alias{fetchSummary}
\title{Summarise estimation output.}
\usage{
fetchSummary(filename, nSample = 25)
}
\arguments{
\item{filename}{The filename used by the estimation or prediction
function.}

\item{nSample}{The number of elements to sample from a batch of
parameters. Defaults to 25.}
}
\value{
An object of class \code{\linkS4class{SummaryResults}}.
}
\description{
Summarise the results from \code{\link{estimateModel}},
\code{\link{estimateCounts}} or \code{\link{estimateAccount}}.
\code{fetchSummary} can also be called on results from prediction
functions, though doing so is generally less enlightening.
}
\section{Metropolis-Hastings updates}{


Depending on the model, one or more batches of parameters is updated
using Metropolis-Hasting updates.  \code{jump} is the standard
deviation of the propoposal density.  \code{acceptance} is the
proportion of proposals that are accepted.  \code{autocorrelation}
is the correlation between successive iterations, after thinning.
Autocorrelations are calculated separately, parameter by parameter,
and then averaged.  If a batch contains more than \code{nSample} parameters,
then \code{nSample} parameters are chosen at random, and autocorrelations
calculated only for that sample.

The Metropolis-Hastings statistics can be extracted from the summary
object using function \code{\link{metropolis}}.
}

\section{Parameters}{


'Rhat's, or 'potential scale reduction factors' are calculated
using function \code{\link[coda]{gelman.diag}}
from package \pkg{coda}.  However, like Gelman et al (2014: 284-286),
but unlike the original \code{gelman.diag} function,
each chain is split into two,
and the first and second halves compared,
to capture variation within each chain,
in addition to variation across chains.

To save time, \code{fetchSummary} calculates Rhats for a sample of
at most \code{nSample} elements from each batch of parameters.
The number of elements sampled is shown in the column labeled \code{n}.

Rhats typically vary across different parameters within a batch.
To give an idea of the distribution, \code{fetchSummary} shows
the median and maximum Rhats. The best way to get detailed information
about convergence is to use \code{\link{fetchMCMC}} followed by
\code{plot}.

The sampling of the Rhats introduces an element
of randomness: if \code{fetchSample} is called twice on the same
\code{filename} the results for \code{Rhat} will differ.

When the number of chains (\code{nChain}), the number of iterations
(\code{nSim}), or the proportion of proposals accepted is small,
estimated Rhats can be unstable, and must be interpreted with
caution.

\code{Est.} in the output from \code{fetchSummary} is short for
"Point estimates". The \code{Est.} columns provide summaries
of the distribution of the estimates. \code{fetchSummary}
calculates point estimates (posterior medians) for every element
in a batch of parameters, and then shows the distribution of
these point estimates.  If a batch has only one parameter, then
only the point estimate for that parameter is shown.
If a batch has more than one parameter, then the minimum
point estimate, median point estimate, and maximum point estimate
for that batch are shown.

To save time, \code{fetchSummary} calculates the point estimates
from a maximum of 100 draws from the posterior sample. The values
may therefore differ slightly from the more accurate values
obtained by calling \code{fetch} and then \code{collapseIterations}
(assuming the full sample has more than 100 draws.)


If greater control over the calculation of Rhat is desired, parameter
estimates can be extracted using \code{\link{fetchMCMC}}, and
results calculated using \code{\link[coda]{gelman.diag}}.
}

\examples{
deaths <- demdata::VADeaths2
popn <- demdata::VAPopn
deaths <- round(deaths)
deaths <- Counts(deaths)
popn <- Counts(popn)
filename <- tempfile()
model <- Model(y ~ Poisson(mean ~ age + sex),
               jump = 0.5)
estimateModel(model = model,
              y = deaths,
              exposure = popn,
              filename = filename,
              nBurnin = 50,
              nSim = 50,
              nChain = 2,
              parallel = FALSE)
fetchSummary(filename)

## keep summary object and extract parts from it
summary.est <- fetchSummary(filename)
gelmanDiag(summary.est)
metropolis(summary.est)
parameters(summary.est)
}
\references{
Geyer, C. J. (2011) Introduction to Markov Chain Monte Carlo.
Brooks, S., Gelman, A., Jones, G., Meng, X-L (eds.) \emph{Handbook of Markov
Chain Monte Carlo}. CRC Press.

Gelman, A., Shirley, K. (2011) Inference from simulations and monitoring
convergence.  Brooks, S., Gelman, A., Jones, G., Meng, X-L (eds.)
\emph{Handbook of Markov Chain Monte Carlo}. CRC Press.

Gelman, A., Carlin, J.B., Stern, H.S. and Rubin, D.B., 2014.
\emph{Bayesian Data Analysis. Third Edition.} Boca Raton, FL, USA:
Chapman & Hall/CRC.
}
\seealso{
Individual components of the summary can be extracted
using functions \code{\link{metropolis}}, \code{\link{parameters}},
and \code{\link{gelmanDiag}}.  Parameter estimates from estimation
or prediction are obtained using function \code{fetch}.
}
