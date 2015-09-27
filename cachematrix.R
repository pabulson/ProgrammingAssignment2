# Create a special matrix that caches the inverse of the matrix once computed
# so that it need not be computed again. 

# Creates a special matrix
#
# Args:
# x: the matrix that becomes the "special" matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        # set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        # set m to be the inverse of x
        setInverse <- function(solve) m <<- solve
        
        getInverse <- function() m
        
        # a list of the functions defined above
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


# Calculate and return the inverse of a matrix and cache the value
#
# Args:
# x: the "special" matrix that will have its inverse cached
cacheSolve <- function(x, ...) {
        
        # Check the cache of the special matrix for the inverse
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }
        
        # calculate the inverse because it has not been cached yet
        data <- x$get()
        message("calculate inverse matrix")
        m <- solve(data, ...)
        x$setInverse(m)
        
        # Return a matrix that is the inverse of 'x'
        m  
}

# create a matrix for test purpose
test_matrix <- matrix(1:4, 2, 2)
test_matrix

# create a special matrix without a cached inverse
special_matrix <- makeCacheMatrix(test_matrix)
special_matrix$get() #should return the original matrix
special_matrix$getInverse() #should return null

# calculate and cache the inverse matrix
cacheSolve(special_matrix) #calculate inverse matrix
special_matrix$getInverse() #should return inverse matrix

# confirm access to the cache data
cacheSolve(special_matrix) #should access cache and not recalculate

