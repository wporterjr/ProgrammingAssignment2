## This function creates a special "matrix" object that can cache its inverse.
## Like the example function it makes a list of four functions that:
## set the values of the matrix
## get the values of the matrix
## set the values of the inverse
## get the values of the inverse

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setinv <- function(inv) im <<- inv
    getinv <- function() im
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    im <- x$getinv()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$setinv(im)
    im    
}