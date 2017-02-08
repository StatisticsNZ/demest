

setGeneric("fakeDLMErrors",
           function(spec, J)
           standardGeneric("fakeDLMErrors"))

#' Generate Fake Data
#' 
#' Generate a simulated dataset, based on a hierarchical model.
#' 
#' NOTE - AT PRESENT ONLY IMPLEMENTED FOR POISSON-VARYING AND BINOMIAL-VARYING
#' MODELS
#' 
#' The values of \code{templateY} are ignored by \code{fakeData}: only the
#' metadata is used.
#' 
#' The model specified by \code{model} must not have improper priors.  This
#' implies, for instance, that a \code{priorSD} argument must be supplied in a
#' varying-means model.
#' 
#' All prior distributions for cell means in varying-means models are centered
#' at zero before values are drawn.  See below for an example.
#' 
#' Benchmarked models are not allowed.
#' 
#' Bounds on probabilities or means can be specified via the \code{lower} and
#' \code{upper} arguments to functions \code{\link{Poisson}},
#' \code{\link{Binomial}} or \code{\link{Normal}}.  In some situations,
#' \code{fakeData} make not be able to generate a value from the target
#' distribution within the specified bounds.  In such cases, \code{fakeData}
#' will instead draw from a uniform distribution if \code{uniform} is
#' \code{TRUE}, and raise an error otherwise.
#' 
#' @aliases fakeData fakeData,SpecBinomialVarying,ANY,Counts-method
#' fakeData,SpecBinomialVaryingBench,ANY,Counts-method
#' fakeData,SpecNormalVarying,ANY,Counts-method
#' fakeData,SpecPoissonVarying,ANY,Counts-method
#' fakeData,SpecPoissonVaryingBench,ANY,Counts-method
#' @param model An object of class \code{"\linkS4class{SpecModel}"}.
#' @param intercept A real number, possibly negative.  Determines the expected
#' value of the fake data.
#' @param templateY An object of class
#' \code{\link[Demographic:Counts-class]{Counts}} with the dimensions and
#' metadata required for the fake data.
#' @param exposure An object of class
#' \code{\link[Demographic:Counts-class]{Counts}}.
#' @param weights An object of class
#' \code{\link[Demographic:Counts-class]{Counts}}.
#' @param uniform Logical.  Only used if the model includes upper or lower
#' bounds.  See below for details.
#' @return An object of class \code{"\linkS4class{FakeData}"}
#' @author John Bryant \email{demographic.packages@@gmail.com}
#' @seealso \code{\link{Model}}, \code{"\linkS4class{FakeData}"},
#' \code{\link{fetch}} \code{\link{listContents}}
#' @keywords models
#' @examples
#' 
#' sex.mean <- ValuesOne(values = c(-1, 1), labels = c("Female", "Male"), name = "sex")
#' sex.prior <- Known(mean = sex.mean, sd = 0.2)
#' model <- Model(y ~ Poisson(mean ~ sex + time,
#'                            priorSD = list(df = 10, scale = 0.2)),
#'                time ~ Exch(sd = 0.2),
#'                sex ~ sex.prior)
#' exposure <- Counts(array(100 * rbeta(n = 60, shape1 = 0.5, shape2 = 2),
#'                          dim = c(3, 2, 10),
#'                          dimnames = list(region = c("A", "B", "C"),
#'                          sex = c("Female", "Male"),
#'                          time = 2000:2009)))
#' fake <- fakeData(model = model,
#'                  intercept = 1,
#'                  templateY = exposure,
#'                  exposure = exposure)
#' 
#' ## the results would have been identical if the mean for the sex prior had been
#' sex.mean <- ValuesOne(values = c(100, 101), labels = c("Female", "Male"), name = "sex")
#' ## since priors are centered before values are drawn
#' 
#' ## to change the mean, change the intercept
#' fake <- fakeData(model = model,
#'                  intercept = -1,
#'                  templateY = exposure,
#'                  exposure = exposure)
#' 
NULL

#' Display the Contents of a Results or Fake Data Object
#' 
#' Display the contents of an object of class \code{"\linkS4class{Results}"} or
#' \code{"\linkS4class{FakeData}"}, in hierarchical form.
#' 
#' Arguments \code{where} and \code{max} restrict number of elements shown in
#' complementary ways: providing values for \code{where} yeilds narrower
#' hierarchies, and providing values for \code{depth} yields shallower ones.
#' 
#' Partial matching is used with elements of \code{where}, so that only the
#' first few letters need to be used, provided this is enough to identify the
#' place in the hierarchy.  See below for an example.
#' 
#' @aliases listContents listContents,FakeData-method
#' listContents,Results-method
#' @param object An object of class \code{"\linkS4class{Results}"} or
#' \code{"\linkS4class{FakeData}"}.
#' @param where A character vector used to select a subset of the hierarchy.
#' @param max The maximum depth into the hierarchy to display.
#' @return The function is typically called for its side effect, the display of
#' a list, but the list is returned invisibly.
#' @author John Bryant \email{demographic.packages@@gmail.com}
#' @seealso To select elements with an object of class
#' \code{"\linkS4class{Results}"}, use \code{\link{fetch}}.
#' @keywords list
#' @examples
#' 
#' ## create Results object
#' deaths <- Counts(round(VADeaths2))
#' popn <- Counts(VAPopn)
#' res <- estimateModel(Model(y ~ Poisson(mean ~ age)),
#'                      y = deaths,
#'                      exposure = popn,
#'                      filename = tempfile(),
#'                      nBurnin = 2,
#'                      nSim = 5,
#'                      nChain = 2)
#' 
#' ## look at whole object
#' listContents(res)
#' 
#' ## look only at the model
#' listContents(res, where = "model")
#' 
#' ## restrict further, to the hyper-parameters part of the model
#' listContents(res, where = c("model", "hyper"))
#' 
#' ## use only enough letters to uniquely identify names
#' listContents(res, where = c("mo", "h"))
#' 
#' ## look only at first level of hierarchy
#' listContents(res, max = 1)
#' 
#' ## look at second and third levels of model
#' listContents(res, where = "model", max = 3)
#' 
NULL



setGeneric("fakeData",
           function(model, intercept, templateY, exposure = NULL,
                    weights = NULL, uniform = TRUE)
           standardGeneric("fakeData"))


setGeneric("fetchInnerFake",
           function(object, nameObject, where)
           standardGeneric("fetchInnerFake"))

