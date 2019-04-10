## Assignment: Caching the Inverse of a Matrixless

# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly.
# The solution consists of a pair of functions that cache the inverse of a matrix.

# This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    
    # object store the inverse matrix detail
    inverseMatrix <- NULL
    
    # set matrix of our caches object using this setter method
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    
    # get matrix from our cache object using this getter method
    get <- function() {
        x
    }
    
    # function to set inverse of matrix
    setInverse <- function(inverse) {
        inverseMatrix <<- inverse
    }
    
    # function to get inverse of matrix
    getInverse <- function() {
        inverseMatrix
    }
    
    # exposing the interfaces to use this function
    list(
        set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse
    )
}

# This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. If the inverse has already been calculated (and
# the matrix has not changed), then the cachesolve should retrieve the
# inverse from the cache.
cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    inverseMatrix <- x$getInverse()
    
    # check if inverse matrix exists
    if (!is.null(inverseMatrix)) {
        # return inverse matrix from cache
        message("getting cached inversed matrix")
        return(inverseMatrix)
    }
    
    # here we collect the matrix
    data <- x$get()
    # here we use solve() functiont to calculate inverse of matrix
    inverseMatrix <- solve(data, ...)
    # now we set the value to computed inverse of matrix
    x$setInverse(inverseMatrix)
    # return inverse of matrix, which is freshly computed
    message("getting freshly computed inversed matrix")
    inverseMatrix
}

