# John Ripy
# Matrix inversion is usually a costly computation and their may be some benefit to 
# caching the inverse of a matrix rather than compute it repeatedly 

# This function, makeCacheMatrix, creates a special "matrix" that can cache its inverse
# Returns a vector list of set and get functions:
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, 
             get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


# This function, cacheSolve, computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix has
# not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
                message("  ~~~~~~~~~~~~~~~~~~~~~~~~~~~")
                message("  ~~~  Using cached data  ~~~")
                message("  ~~~~~~~~~~~~~~~~~~~~~~~~~~~")
                return(i)
        }
        m <- x$get()
        i <- solve(m)
        x$setInverse(i)
        i
}