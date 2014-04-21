## Author: Carl McQueen
## Date: 4.21.14
## Assignment: Peer Assessment - R Programming
## Task: Inverse of a matrix

## Set the Value of the matrix
## Get the value of the Matrix
## Set the value of the inverse of that matrix
## Get the Value of the inverse of that MAtrix

## pre-set name for Matrix-Maker
makeCacheMatrix <- function(x = matrix()) {
        ## everything begins as NULL
        inverse <- NULL
        
        ## Set the value
        setM <- function(y) {
                x <<- y
                inverse <<- NULL ## this effects the loop
        }
         
        ## Now get the value of the matrix
        getM <- function() x
        
        ## set the matrix to it's inverse
        setInverse <- function(inversed) inverse <<- inversed
        ## get the inverse of the matrix
        getInverse <- function() inverse
        
        ## return a list of all of the above functions
        list(setM = setM, getM = getM,
             setInverse = setInverse,
             getInverse - getInverse)
}

## creates the inverse of a matrix or brings in the cached value
## built around the function above

cacheSolve <- function(x,...) {
        inverse <- x$getInverse()
        if(!isnull(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        ## if not cached, get the matrix data
        data <- x$get()
        ## compute the inverse of that matrix
        inverse <- solve(data,...)
        # now cache it
        x$setInverse(inverse)
        # return the inverse
        inverse
}