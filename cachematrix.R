## Functions to cache the inverse of a matrix.

## Constructs the functions vector for the caching of the inverse of matrix x.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returns the inverse of the function vector x (must be created with function "makeCacheMatrix").
## Calculates the inverse only on the first call and returns the cached result (inverse) on subsequent calls.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        ##message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
