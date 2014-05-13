# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly.  Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    # this stores the inverse cache
    cached_inverse <- NULL
    
    # sets a new matrix and nullifies the old cached matrix
    set <- function(new_matrix) {
        x <<- new_matrix
        cached_inverse <<- NULL
    }
    
    # returns the matrix
    get <- function() {
        x
    }
    
    # sets the cached inverse
    set_inverse <- function(inverse) {
        cached_inverse <<- inverse
    }
    
    # gets the cached inverse
    get_inverse <- function() {
        cached_inverse
    }
    
    # returns the defined matrix and the functions
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$get_inverse()
    
    # if cached_inverse exists, return it
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    # gets the inverse and calculates it with solve()
    data <- x$get()
    inverse <- solve(data, ...)
    
    # cache the matrix again
    x$set_inverse(inverse)
    
    # returns the calculated matrix
    inverse
}