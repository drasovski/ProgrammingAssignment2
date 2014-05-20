## The pair of functions below take a matrix and create a special "matrix"
## that can be used to calculate the inverse of a matrix or return a
## cached inverse if it has been previously calculated

## This function creates a special "matrix", which is a list of functions to
## set and get the original matrix and set and get its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## This function calculates the inverse of the special "matrix" created with
## the above function using solve() if the inverse has not yet been calculated
## or if the original matrix has changed. It then caches the calculated inverse
## and returns it. Otherwise, it gets the inverse from the cache and returns it.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}