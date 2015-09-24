# Create a special matrix that caches the inverse of the matrix once computed
# so that it need not be computed again. 

# Creates a special matrix
#
# Args:
# x: the matrix that becomes the "special" matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


# Calculate the inverse of a matrix and cache the value
#
# Args:
# x: the "special" matrix that will have its inverse cached
cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m  
}
