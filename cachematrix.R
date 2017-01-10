## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inversematrix
# get the value of the inversematrix

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
           setinverse = setinverse,
           getinverse = getinverse)
}

## The following function calculates the mean of the special "matrix" created with the above function. 
# However, it first checks to see if the matrix has already been solved. If so, 
# it gets the inverse matrix from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the matrix and sets this matrix 
# in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
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
