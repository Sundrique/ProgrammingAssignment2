## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInversed <- function(inversed) i <<- inversed
    getInversed <- function() i
    list(set = set, get = get,
         setInversed = setInversed,
         getInversed = getInversed)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inversed <- x$getInversed()
    if(!is.null(inversed)) {
        message("getting cached data")
        return(inversed)
    }
    mx <- x$get()
    i <- solve(mx, ...)
    x$setInversed(i)
    i
}