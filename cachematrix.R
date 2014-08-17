## The functions cache the Inverse of a Matrix rather than compute it repeatedly
## Stores a matrix and cache's its inverse.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(X = matrix()) {
        S <- NULL
        # set the value of the matrix
        set <- function(Y) {
                X <<- Y
                S <<- NULL
        }
        # get the value of the matrix
        get <- function() X 
        # set the value of the inverse matrix
        setsolve <- function(solve) S <<- solve  
        # get the value of the inverse matrix
        getsolve <- function() S
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(X, ...) {
        # Return a matrix that is the inverse of 'x'
        S <- X$getsolve()
        # If the inverse has already been calculated 
        # then the inverse is retrieve from the cache.
        if(!is.null(S)) {
                message("getting cached data")
                return(S)
        }
        # Otherwise the cache object X is used 
        # to calculate the inverse
        data <- X$get()
        S <- solve(data, ...)
        # The result is stored in object X
        X$setsolve(S)
        # The inverse is returned
        S
}
