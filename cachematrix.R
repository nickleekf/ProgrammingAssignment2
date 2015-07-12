## Project Title: Coursera: R Programming - Programming Assignment 2
## Purpose: Caching the inverse of a matrix to improve costly computation time   
## Description: 2 functions will be written to  cache the inverse of a matrix
## Assumptions: Matrix supplied is always invertible

## Purpose: The function stores a matrix and a cached value of the inverse of the matrix
## Return Value: A special vector that contains a list of functions
makeCacheMatrix <- function(x = matrix()) {
    
    ## Holds the cached value or NULL if nothing is cached
    myCache <- NULL
    
    ## store a matrix
    setMatrix <- function(newValue) {
        x <<- newValue
        ## Flush the cache
        myCache <<- NULL
    }
    
    ## Returns the stored matrix
    getMatrix <- function() x
    
    ## Caches the given argument by inversing the matrix using the solve function.  
    setInverse <- function(solve) {
        myCache <<- solve
    }
    
    ## Returns the cached value
    getInverse <- function() myCache
    
    ## return a list of functions
    list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


## Purpose: The function calculates the inverse of a special matrix created with makeCacheMatrix
## Return Value: Returns the inverse of a matrix
cacheSolve <- function(x, ...) {
    ## get the cached value
    inverse <- x$getInverse()
    
    ## if a cached value exists return it
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    ## if cache value does not exist, then get the matrix, caclulate the inverse and store it in cache
    data <- x$getMatrix()
    inverse <- solve(data)
    x$setInverse(inverse)
    
    ## Return a matrix that is the inverse of 'x'
    inverse
           
}
