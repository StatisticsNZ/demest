
## a function to set a random number seed in a worker's global environment
## the rseed is the proper seed, ie .Random.seed
workerRNGSeed_Set <- function (rseed)
{
    assign(".Random.seed", rseed, envir = .GlobalEnv)
    invisible()
}

## a function to get the random number seed from a worker's global environment
## the return value is the proper seed, ie .Random.seed
workerRNGSeed_Get <- function (rseed)
{
    .Random.seed
}

## a function to set random number seeds for the global environment
## for each worker in a cluster cl
## assumes we have as many workers in cl as there are elements in seeds
## seeds is a list of random number seeds (.Random.seed values)
## note we don't need these functions for the continue idea
## but I wanted to have them to do and check other things
clusterSetRNGSeeds <- function(cl, seeds)
{

    clusterApply(cl, seeds, workerRNGSeed_Set)
    invisible()
    
}

## a function to get random number seeds from the global environment
## for each worker in a cluster cl
## returns a list with as many elements as there are workers in cl,
## each element of which is the result of getting .Random.seed for a worker
## note we don't need these functions for the continue idea
## but I wanted to have them to do and check other things
clusterGetRNGSeeds <- function(cl)
{

    clusterCall(cl, workerRNGSeed_Get)
    
}


# get each worker to run runif n times
# store results and the final random seed and return both in a list
workerPRNG_Gen <- function(n = 2) {
	rus = runif(n)
    # save current seed
	myseed <- .Random.seed
	# make a list to return
	result <- list(myseed = myseed, rus = rus)
    ## clumsy - but in estimate functions we will return an object
}

# get each worker to continue prng generation
# the worker takes the random number seed from result
# generates n more prngs, concatenates to the prngs in result
# replaces the final random number seed in result with the new last one
# and returns the updated result
workerPRNG_ContinueGen <- function(result, n = 2) {
    rseed <- result[[1]]
    
    ## if everything we want to do is in this single function, 
    ## we don't have to set the seed in the global environment, 
    ## just for this function. I have done it for the global
    ## environment so that I could use the get worker seeds functions
    ## as well after running this, but in many ways it would be
    ## better to just have it set for this function's environment
    ## -- it depends really on how we envisage using the workers.
    assign(".Random.seed", rseed, envir = .GlobalEnv)
	rus = runif(n)
    # make a list to return
	result[[1]] = .Random.seed
    result[[2]] = c(result[[2]], rus)
    result
}

library(parallel) 

rnseed <- 42 # use for RNG 'seed'
mycores <- detectCores() # how many cores (see parallel documentation)
mycores

## use the workerPRNG_Gen function with a cluster, after setting workers up to 
## use the "L'Ecuyer-CMRG" RNG with specified seed
cl <- makeCluster(mycores)
clusterSetRNGStream(cl, rnseed) ## set initial RNG for each worker
nprng = 10 # number of prngs to generate for testing

## get first set of results
ans <- clusterCall(cl, workerPRNG_Gen, nprng)


## get the current seeds 
## this is not necessary to demonstrate the continue idea
## but it is a check on progress
seedsCheck <- clusterGetRNGSeeds(cl)

stopCluster(cl)


seeds <- vector("list", length(ans))
results <- vector("list", length(ans))

## get the seeds and results out and store them separately
for (i in seq_along(ans)) {
    ## example gets values out of list in the ans list but we would get from objects
    seeds[[i]] = ans[[i]][[1]]
    results[[i]] = ans[[i]][[2]]
}

## check clusterGetRNGSeeds is doing same as workerPRNG_Gen
all.equal(seeds, seedsCheck)

## make cluster again 
cl <- makeCluster(mycores)
## at this point the cluster workers have no .Random.seed set
## and set our seeds
clusterSetRNGSeeds(cl, seeds)

## get the current seeds
seedsCheck <- clusterGetRNGSeeds(cl)

## check clusterSetRNGSeeds is doing its job
all.equal(seeds, seedsCheck)

stopCluster(cl)

## all that was not necessary to demonstrate the continue idea
## but it is a check on progress


## make cluster yet again
cl <- makeCluster(mycores)

## and use the continue code to get another nprng prngs from each worker
newans <- clusterApply(cl, ans, workerPRNG_ContinueGen, nprng)

## get the seeds and results out and store them separately
newseeds <- vector("list", length(newans))
newresults <- vector("list", length(newans))

for (i in seq_along(newans)) {
    ## example gets values out of list in the ans list but we would get from objects
    newseeds[[i]] = newans[[i]][[1]]
    newresults[[i]] = newans[[i]][[2]]
}
newresults
## each element in newresults should now be 2*nprng long

all.equal(seeds, newseeds) ## should not be equal, because we've generated more

stopCluster(cl)

## make cluster again
cl <- makeCluster(mycores)

# set RNGs to original seed
clusterSetRNGStream(cl, rnseed)

## get a complete set of results (2 x nprng
allans <- clusterCall(cl, workerPRNG_Gen, (2*nprng))

stopCluster(cl)

allseeds <- vector("list", length(allans))
allresults <- vector("list", length(allans))

## get the seeds and results out and store them separately
for (i in seq_along(allans)) {
    allseeds[[i]] = allans[[i]][[1]]
    allresults[[i]] = allans[[i]][[2]]
}

## check 
all.equal(newresults, allresults) ## same as doing in 2 separate lots
all.equal(newseeds, allseeds) ## same final seed values
