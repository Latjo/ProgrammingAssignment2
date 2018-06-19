## These functions creates and manages a special matrix object through defined functions.

## This function creates an empty matrix and its empty inverse
## It also defines four functions to manage the matrix and its inverse:
## - get (gets matrix)
## - set (sets matrix)
## - getinverse (gets matrix inverse)
## - setinverse (sets matrix inverse)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <-function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function gets the cached inverse of its argument.
## If there is no cached inverse, the inverse of the argument is calculated

cacheSolve <- function(x, ...) {
    inv <-x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}