## *********************************************************************
## T. Aristanto -- Programming Assignment 2 -- 11/22/2014
## Calculating Inverse of a matrix using solve function. If the inverse
## is already calculated previously and the matrix did not change, it 
## takes the result from the cache.
## Build based on the example for CacheMean calculation by R. Peng.  
##**********************************************************************

## This the function take the matrix as an argument and set it into memory.
## The function also contains the other functions;
## get for getting the matrix
## setinvert for storing the inverse of the matrix on the cache
## getinvert for getting the inverted matrix from the cache

makeCacheMatrix <- function(x = matrix()) {
	  m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvert <- function(solve) m <<- solve
        getinvert <- function() m
        list(set = set, get = get,
             setinvert = setinvert,
             getinvert = getinvert)
}


## The function will first check if the inverse of the matrix x is already
## exist in the cache. If it is it will just return the one from the cache, 
## otherwise it will get the matrix, calculate the inverse, and store it in 
## the cache.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  m <- x$getinvert()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinvert(m)
        m
}
