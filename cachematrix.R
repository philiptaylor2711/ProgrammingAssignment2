## The following two functions save the inverse of a matrix to the cache
## so it does not need to recalculated if the contents don't change.
## This is useful if the inverse matrix is part of a loop.

## The first function creates a list of four functions.
## Two of the functions set a null input matrix and null inverse matrix.
## The other functions store the input matrix x and its inverse.
## The list is used as an input for the second function.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
          x <<- y
          m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse, getinverse = getinverse)
}


## This function first checks whether the inverse matrix has already been calculated.
## It does this by seeing if the value of the global variable m is NULL or a matrix.
## If the value of m already exists then the inverse has already been calculated,
## therefore there is no need to recalculate it (a message is displayed).
## If m is NULL the inverse matrix is calculated.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
              message("getting cached inverse matrix")
              return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
}
