## The following functions able to cache potentially time-consuming computations.

# Note: The deep assignment arrow, <<-, never creates a variable in the current 
# environment, but instead modifies an existing variable found by walking up the
# parent environments.


## makeCacheMatrix creates a special "matrix", which is really a list containing
## a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInv <- function(solve) m <<- solve
        getInv <- function() m
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}

## cacheSolve first checks to see if the Inverse has already been calculated (and that
## the matrix has not changed),if so, it skips the computation and gets the
## inverse from the cache. Else, it finds the inverse of the matrix and sets the value
## of the inverse in the cache via the setInv function.

cacheSolve <- function(x, ...) {
        m <- x$Inv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m
}