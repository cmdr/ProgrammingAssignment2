## Functions makeCacheMatrix and cacheSolve allows computations of a matrix
## inverse that is then stored for future usage. More specifically, in the case 
## the matrix inverse is needed again, the functions will allow for a use of 
## the cached result rather than preform the time-comsuming computation again.

## Creates the list of four methods: set, get, setinverse and getinverse
## allowing for, respectively: setting and getting of a matrix, and
## setting and getting of its inverse.
##
## Arguments:
## x: An invertible matrix
##
## Returns:
## The list of four method: list(set, get, setinverse, getinverse)
## for invertible matrices

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      return(list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse))
}


## Computes the inverse of matrix x if it has not been already cached otherwise
## it returns the result that had been cached with the help of the function
## makeCacheMatrix.
##
## Arguments:
##    x: An invertible matrix
##
## Returns:
##    The inverse of matrix x

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      return(m)
}
