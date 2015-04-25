## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix creates a list object -- which is a list of functions that get matrix data, set that matrix data to
## the cache, solve and set the inversion of the matrix data, and get the solved inversion 
##from the cache. It is assumed that the matrix can be inverted.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Write a short comment describing this function

## cacheSolve takes some number of instances of the makeCacheMatrix object as arguments and
## returns the inverse of each, either from the cache if it was previously calculated, or 
## calculated fresh. It remains assumed as before that the matrix can be inverted, and 
## was not changed.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data) # Solve will get the inverse automatically if presented with 1 arg
    x$setinverse(inv)
    inv
}
