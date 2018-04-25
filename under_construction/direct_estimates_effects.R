
library(methods)
library(demest)
library(dplyr)

first_year <- 1960
last_year <- 2011


deaths <- readRDS("out/deaths.rds") %>%
    subarray(year > first_year)
exposure <- readRDS("out/exposure.rds") %>%
    subarray(year > first_year)
rates <- (deaths + 1) / (exposure + 1)
log_rates <- log(rates)


log_rates <- subarray(log_rates, year < 2011 & !(series %in% c("Israel", "Taiwan", "Greece")))
    
makeTerms <- function(object) {
    if (length(object) == 0L)
        stop(gettextf("'%s' has length %d",
                      "object", 0L))
    if (any(is.na(object)))
        stop(gettextf("'%s' has missing values",
                      "object"))
    if (any(is.infinite(object)))
        stop(gettextf("'%s' has non-finite values",
                      "object"))
    dim <- dim(object)
    .Data <- object@.Data
    metadata <- object@metadata
    n <- length(dim)
    intercept <- mean(.Data)
    ans <- list("(Intercept)" = intercept)
    if (n > 1L) {
        margins <- listAllSubsets(n)
        makeMean <- function(margin) apply(.Data, margin, mean)
        means <- lapply(margins, makeMean)
        metadata.means <- lapply(margins, function(margin) metadata[margin])
        makeValObj <- function(.Data, metadata) {
            .Data <- array(.Data,
                           dim = dim(metadata),
                           dimnames = dimnames(metadata))
            new("Values",
                .Data = .Data,
                metadata = metadata)
        }
        means <- mapply(makeValObj,
                        .Data = means,
                        metadata = metadata.means)
        means <- lapply(means, demest:::sweepAllMargins)
        names.means <- lapply(means, names)
        names.means <- sapply(names.means, paste, collapse = ":")
        names(means) <- names.means
        ans <- c(ans, means)
    }
    predicted <- Reduce(f = "+", x = ans)
    error <- object - predicted
    name <- paste(names(object), collapse = ":")
    error <- list(error)
    names(error) <- name
    ans <- c(intercept, means, error)
}


terms <- makeTerms(log_rates)


library(latticeExtra)

sample.countries <- c("New Zealand", "Scotland", "Spain", "Italy", "USA")
sample.ages <- c("0", "20-24", "40-44", "60-64", "70-74", "80-84", "90++")

dplot(~ age, data = terms$age, midpoints = "age")
dplot(~ sex, data = terms$sex)
dplot(~ year, data = terms$year, midpoints = "year")
dplot(~ year | age, data = terms[["age:year"]], midpoints = "year")
dplot(~ age | sex, data = terms[["age:sex"]], midpoints = "age")
dplot(~ age | series, data = terms[["age:series"]], midpoints = "age")
useOuterStrips(dplot(~ year | series * age, data = terms[["age:year:series"]],
                     subarray = series %in% sample.countries & age %in% sample.ages, midpoints = "year"))
dplot(~ series, data = terms$series, horizontal = T)
dplot(~ year | series, data = terms[["year:series"]], midpoints = "year", main = "year:series")
dplot(~ age | series, data = terms[["age:sex:series"]], subarray = sex == "Female", midpoints = "age", main = "age:sex:series")
dplot(~ year | age, data = terms[["age:sex:year"]], subarray = sex == "Female", midpoints = "year", main = "age:sex:year")
dplot(~ year | series, data = terms[["sex:year:series"]], subarray = sex == "Female", midpoints = "year", main = "sex:year:series")
dplot(~ series | sex, data = terms[["sex:series"]], horizontal = T, main = "sex:series")
dplot(~ year | sex, data = terms[["sex:year"]], midpoints = "year", main = "sex:year")


