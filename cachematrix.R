## Assignment 2- Kendrick Tso

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        seti <- function(inverse) i <<- inverse
        geti <- function() i
        list(set = set, get = get,
             seti = seti,
             geti = geti)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should return the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                i <- x$geti()
        if(!is.null(i)) {
                message("getting cached inverse matrix")
                return(i)
        }
        matrix <- x$get()
        i <- solve(matrix)
        x$seti(i)
        i
}
