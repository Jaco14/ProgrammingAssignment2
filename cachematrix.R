## The purpose of these two functions is to cache the inverse of a matrix. 
## This means that the calculated value is stored on the local disk.
## When the user wants to calculate the inverse of a matirx, the function first
## checks whether this has already been done and if so, returns the stored value.
## This saves computing time.

## First function - makeCacheMatrix
## This function creates a list vector with functions to set or get
## the value of the matrix and inverse matrix.

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Second function - cacheSolve
## This function returns the inverse of a matrix by first checking
## if it has already been calculated. If so, it returns the stored inverse
## matrix. Otherwise, it computes the inverse matrix and stores the answer
## using the first function above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 	 m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m


}
