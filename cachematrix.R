## Assignment: Caching the Inverse of a Matrix
## Matrix Inversion is usually a cost computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute
## it repeatedly.

## First we create a matrix that can "cache" the inverse
## Second we write a function to compute the inverse of the special "matrix"
## using the solve function in R.


makeCacheMatrix <- function(x = matrix()) {
        invmat <- NULL
        set <- function(y) {
                x <<- y
                invmat <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) invmat <<- inverse
        getInverse <- function() invmat
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of x
        invmat <- x$getInverse()
        if (!is.null(invmat)) {
                message("getting cached data")
                return(invmat)
        }
        mat <- x$get()
        invmat <- solve(mat, ...)
        x$setInverse(invmat)
        invmat
}
