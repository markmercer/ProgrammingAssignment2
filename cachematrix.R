## constructs a structure that can be used to hold
## a matrix as well as the precomputed inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    set <- function(y) {
	x <<- y
	xinv <<- NULL
    }
    get <- function() x
    setinv <- function(mean) xinv <<- mean
    getinv <- function() xinv 
    list(set = set, get = get,
     setinv = setinv,
     getinv = getinv)

}

## Returns the invers of a given matrix. Uses the precomputed inverse 
## of a matrix if it exists, otherwise calculates the inverse dynamically. 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
