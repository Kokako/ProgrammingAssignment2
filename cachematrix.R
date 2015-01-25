## R Programming Assignment 2
## makeCacheMatrix creates a matrix object that can cache its inverse. Its 
## argument should be an invertible square matrix.  The inverse is calculated 
## and cached in the cacheSolve function, which takes the output of 
## makeCacheMatrix as its argument.


## Makes a matrix that is able to cache its inverse
makeCacheMatrix <- function(matrix = matrix()) {
    inverse <- NULL
    
    ## A function that sets the value of the matrix
    setmatrix <- function(y) {
        matrix <<- y
        inverse <<- NULL
    }
    
    ## A function that returns the matrix
    getmatrix <- function() matrix
    
    ## A function that will set its argument to the inverse of the matrix
    setinverse <- function(solved) inverse <<- solved
    
    ## A function that returns the inverse
    getinverse <- function() inverse
    
    ## Return the list of functions
    list(setmatrix = setmatrix, 
         getmatrix = getmatrix,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Returns the inverse of the input matrix, either by calculating it or 
## by retrieving it from the cache if it was already calculated
cacheSolve <- function(matrix, ...) {
    ## Check if inverse already cached
    inverse <- matrix$getinverse()
    
    ## If inverse cached, return it
    if(!is.null(inverse)) {
        message("Retrieving cached inverse ...")
        return(inverse)
    }
    ## If not cached, calculate it and cache it
    data <- matrix$getmatrix()
    inverse <- solve(data, ...)
    matrix$setinverse(inverse)
    inverse
    
}
