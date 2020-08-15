## Caching the inverse of a matrix can sometimes be better than to compute
## it repeatedly, since matrix inversion can be a costly computation.

## The following are functions that cache the inverse of a matrix, assuming 
## the matrix is always invertible.

## The 'makeCacheMatrix' function caches the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
           x <<- y
           inv <<- NULL
     }
     get <- function() {x}
     setInverse <- function(inverse) {inv <<- inverse}
     getInverse <- function() {inv}
     list(set = set,
          get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## The 'cacheSolve' function computes the inverse of the matrix submitted. 
## If the inverse has already been calculated and the matrix has not changed, 
## then it should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inv <- x$getInverse()
     if(!is.null(inv)) {
        message("getting cached matrix")
        return(inv)
     }
     mat <- x$get()
     inv <- solve(mat, ...)
     x$setInverse(inv)
     inv
}


