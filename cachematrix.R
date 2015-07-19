## Functions provided below provides ability to cache time consuming 
## matrix inverse calculation. Use of these functions can be very beneficial
## for large matrices where repeated calculations are done and data in the matrix
## is not changing. Hence, providing an efficient way to lookup the inverse rather
## than calculating it each time.

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a matrix containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## The following function calculates the inverse of the special "matrix" 
## created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the 
## inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
