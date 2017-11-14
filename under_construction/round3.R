logLik3 <- function(x, y) {
    y.divisible.by.3 <- y %% 3L == 0L
    diff <- x - y
    if (y.divisible.by.3)
        if (diff == 0L) 0 else -Inf
    else {
        if (diff == 0L)
            -Inf
        else if (diff == 1L)
            log(2/3)
        else if (diff == 2L)
            log(1/3)
        else
            -Inf
    }
}


diffLogLikRound3 <- function(x, yProp, yCurr, useC = FALSE) {
    ## 'x'
    stopifnot(is.integer(x))
    stopifnot(identical(length(x), 1L))
    stopifnot(!is.na(x))
    stopifnot(x >= 0L)
    ## 'yProp'
    stopifnot(is.integer(yProp))
    stopifnot(identical(length(yProp), 1L))
    stopifnot(!is.na(yProp))
    stopifnot(yProp >= 0L)
    ## 'yCurr'
    stopifnot(is.integer(yCurr))
    stopifnot(identical(length(yCurr), 1L))
    stopifnot(!is.na(yCurr))
    stopifnot(yCurr >= 0L)
    ## 'yProp' and 'yCurr'
    stopifnot(yProp != yCurr)
    if (useC) {
        .Call(diffLogLikRound3_R, x, yProp, yCurr)
    }
    else {
        y.prop.divisible.by.3 <- (yProp %% 3L) == 0L
        y.curr.divisible.by.3 <- (yCurr %% 3L) == 0L
        diff.prop <- abs(y.prop - x)
        diff.curr <- abs(y.curr - x)
        if (y.prop.divisible.by.3 && (diff.prop == 0L))
            return(Inf)
        if (y.curr.divisible.by.3 && (diff.curr == 0L))
            return(-Inf)
        
            if (diff.prop == 0L)
                return(Inf)
            if (diff.curr 
            else {
                diff.curr <- abs(y.curr - x)
                if ((diff.prop == 1L) && diff
                    if (diff.curr == 1L) 0 else log(2)
                else
                    if (diff.curr == 2)
                    
                        0
                    elseif (diff.prop == 1L)
                
            if (x == y.prop)
                Inf
            else {
                if (abs(x - y.prop) == 1L)
                    
            -Inf
        else {
            
        
    y.prop.equals.x <- yProp == x
    y.curr.equals.x <- yCurr == x
    if (y.prop.div.by.3 && y.curr.div.by.3) {
        if (y.prop.equals.x && y.curr.equals.x)
            0
        else if (!y.prop.equals.x)
            
        }
        
        
        
    }
