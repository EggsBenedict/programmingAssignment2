## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix creates a special matrix, which is really a list containing a 
## function to:

## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inverted <- NULL
    
    set <- function(y) {
        x <<- y
        inverted <<- NULL
    }
    get <- function() x
    setInverted <- function(inverse) inverted <<- inverse
    getInverted <- function() inverted
    list(set = set, get = get,
         setInverted = setInverted,
         getInverted = getInverted)
}


## The following function calculates the inverse of the special "vector" created 
## with the above function. However, it first checks to see if the inverse 
## matrix has already been calculated. If so, it gets the inverse matrix from
## the cache and skips the computation. Otherwise, it calculates the inverse of
## the matrix and sets the value of the inverse in the cache via the setInverted
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverted <- x$getInverted()
    if(!is.null(inverted)) {
        message("getting cached data")
        return(inverted)
    }
    data <- x$get()
    inverted <- solve(data, ...)
    x$setInverted(inverted)
    inverted
}


## Test Case:

## x <- cbind(c(4,2),c(7,6))
## cacheM <- makeCacheMatrix(x)
## cacheSolve(cacheM)
    ##SLN
    ##      [,1] [,2]
    ##[1,]  0.6 -0.7
    ##[2,] -0.2  0.4

## x <- cbind(c(4,3),c(3,2))
## cacheM <- makeCacheMatrix(x)
## cacheSolve(cacheM)
    ##SLN
    ##      [,1] [,2]
    ##[1,]   -2    3
    ##[2,]    3   -4
    
## x <- cbind(c(5,7,9),c(3,2,1),c(6,5,9))
## cacheM <- makeCacheMatrix(x)
## cacheSolve(cacheM)
    ##SLN
    ##           [,1]       [,2]        [,3]
    ##[1,] -0.2363636  0.3818182 -0.05454545
    ##[2,]  0.3272727  0.1636364 -0.30909091
    ##[3,]  0.2000000 -0.4000000  0.20000000
