## Set of functions that,
##  1. Creates a matrix which can cache inverse calcuated by solve(..)
##  2. Looksup the cache to see if inverse is already calculated for
##     a given matrix, if so returns the cached value. If not, calculates the
##     inverse using solve(..) and stores it in the cache.

## makeCacheMatrix
## Provides functions for the following,
##  1. get/set for getting and setting matrix "x"
##  2. getInverse to lookup cache and return inverse of the stored
##     matrix "x", if inverse exists. Inverse is stored in "matInverse".
##  3. setInverse to set the inverse "matInverse" of the matrix "x"

makeCacheMatrix <- function(x = matrix()) {
    matInverse <- NULL
    set <- function(y) {
        x <<- y
        matInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) matInverse <<- inverse
    getInverse <- function() matInverse
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## cacheSolve
## 1. Look up cache to see if inverse of "x" is calculated, if so
##    returns the calculated inverse.
## 2. If inverse doesnt exist in cache, calculates and stores the 
##    inverse in the cache.
##
## Function input: matrix that is created using makeCacheMatrix()

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data for inverse")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
