## Set of functions that can cache the inverse of a matrix to prevent redoing the costly inverse matrix computation.

## This function creates a special "matrix" object that can cache its inverse. 
## It extends the regular matrix with 4 functions:
## - set() and get() to set respectively get the input matrix
## - setinverse() and getinverse() to set respectively get the cached inverse matrix value (which is initialy NULL, until the cacheSolve function has 
##   been called and sets it to the correct value)
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(i) inverse <<- i
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" created by the function makeCacheMatrix. It sets and returns the inverse matrix 
## stored in this special "matrix", either by calculating it with solve() or, if there is already a value present, it returns the cached value.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
