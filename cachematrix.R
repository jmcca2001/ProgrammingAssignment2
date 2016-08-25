## This file contains two functions that aid in creating a cached version of the
## inverse of a matrix in order to speed up the processing time involved with
## inverting a large matrix.

## This is the first of the two functions:
## This function is resposible for creating the the special matrix containinig
## functions to set and get the value of the matrix, as well as function to 
## set and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }

        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This is the second of the two functions:
## This function is repsonsible calculating the inverse of a matrix, but it
## first checks the cache to see if the inverse has been previously created.
## If there is a stored value of the inverse in the cache then the cached value
## is returned rather than recalculating the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("Getting cached data:")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
