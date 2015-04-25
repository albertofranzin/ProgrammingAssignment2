## Functions for the second programming assignment for the online course
## 'R programming'.
##
## The following two methods create a matrix that can return a cached version
## of its inverse.


## Create the matrix object along with its methods.

makeCacheMatrix <- function(x = matrix()) {

        inverse <- NULL

        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }

        get <- function() x

        setinverse <- function(inv) inverse <<- inv

        getinverse <- function() inverse

        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Return the inverse of the matrix. If possible, return the cached version.
## Otherwise, compute the inverse matrix and return it.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if (!is.null(inverse)) {
                message("Returning the cached version")
                return(inverse)
        }

        m <- x$get()
        inv <- solve(m, ...)
        x$setinverse(inv)
        return(inv)
}

