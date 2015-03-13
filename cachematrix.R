## The following 2 functions, makeCacheMatrix and cacheSolve, caching the 
## Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.
## Argument: a matrix X
## Return: A list containing functions to
## - set the value of the matrix
## - get the value of the matrix
## - set the value of inverse of the matrix
## - get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) i <<- solve
        getsolve <- function() i
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)     
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix.If the inverse has already been calculated (and the matrix 
## has not changed), then cacheSolve should retrieve the inverse from the cache.
## Argument: list returned from makeCacheMatrix function
## Return: inverse of the matrix
cacheSolve <- function(x, ...) {
        i <- x$getsolve()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setsolve(i)
        ## Return a matrix that is the inverse of 'x'        
        i     
}
