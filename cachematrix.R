## Functions below are very similar to the example functions given in the assignment.
## Though this is my formal assignment submission I don't like following things regarding
## this approach.  1. This is a muzzle laoding gun type approach.  Everytime you change matrix
## You will have to fire MakeCacheMatrix so that m resets to NULL.  It doesn't automatically detect 
## change in the original matrix file.  Anyway...

## makeCacheMatrix function will take in given matrix and produce list as given below

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL      
    }
    get <- function() x
    setmean <- function(a) m <<- a
    getmean <- function() m
    list(set = set, get = get, setmean = setmean, getmean = getmean)
}


## This function will check the value of M. if it is null then it means inverse was never calculated.
## so it will calculate inverse, store it in veriable m and return m.  if the value of m is not null then it means
## it already has inverse sotred in it.  So then it will display that value along with message indicating that it's a cached data

cacheSolve <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmean(m)
    m
}