
## The below functions calculate the inverse of a matrix given an
## invertible matrix (it currently does not handle
## non-invertible matrices). The inverse of a matrix is calculated
## once and cached and is not recalculated unless the matrix is reset.

## This function takes in a matrix (assumed always invertible) as
## input and returns an object which is a a list of 4
## functions - get and set(reset) the matrix and get
## and set the inverse of the matrix. This also creates a cache
## variable to store the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    cInverse <- NULL       

    set <- function(y) {
        x <<- y
        cInverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) cInverse <<- inverse
    getinverse <- function() cInverse
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function takes an object created in makeCacheMatrix() as
## input and returns its inverse. It tries to return the cached
## inverse value if it was calculated before. If not, it
## calculates the inverse, caches it for future use and returns
## the inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
