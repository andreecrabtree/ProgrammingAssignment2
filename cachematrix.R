## For R Programming Assignment 2
## This pair of functions will allow you to cache the inverse of the matrix
## in order to save processing time for larger matrices.

## How to use this pair of functions:
## Create the object
##    m <- makeCacheMatrix()
## Set the matrix of the object
##    m$set(matrix(runif(25), 5, 5)) 
## this will generate a 5x5 matrix of random uniform variables
## Print out the set matrix
##   m$get()
## Calculate (and cache) the inverse
##    cacheSolve(m)
## If you run cacheSolve(m) again it will return the cached copy
## That's it! Easy, peasie, lemon squeezie.

## This function generates a list containing a function to
##  * set the value of the matrix
##  * get the value of the matrix
##  * set the inverse of the matrix
##  * get the inverse of th matrix
## The <<- assignment operator will ensure that x (the matrix) and i (the inverse)
## are stored in the scope of makeCacheMatrix instead of the children functions. 
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function will solve for the matrix contained in the "special" list 
## returned from the makeCacheMatrix() function. 
## You call it with the returned makeCacheMatrix() "object" once you've $set
## the value of the matrix.
## If there is a cached inverse this will be returned rather calculating the
## inverse again.
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
