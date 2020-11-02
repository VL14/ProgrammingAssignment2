## Week 3 Programming Assignment

## Function to create a special "matrix" object that can cache its inverse with solve(x)

makeCacheMatrix <- function(x = matrix()) {
    # Set matrix and inverse values
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Get matrix value
    get <- function() x
    
    # Set value of inverse matrix
    setinv <- function(z) inv <<- z
    
    # Get value of inverse matrix
    getinv <- function() inv
    
    # Name and return functions to be used in cacheSolve
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Function to calculate matrix inverse from 1st function or retrieve inverse from cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # Check for existing value
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting inverse from cache")
        return(inv)
    }
    
    # Get inverse if not in cache
    mat <- x$get()
    inv <- solve(mat)
    x$setinv(inv)
    inv
}
