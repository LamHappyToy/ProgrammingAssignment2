## These functions compute the inverse of matrix and speed up the calculation
## if it is cached.

## It first initialises x and inv. This function returns a list of four functions
## set: for setting new matrix values
## get: for getting the stored matrix values
## getinverse: for getting inverse after running cacheSolve
## setinverse: for setting inverse after running cachesolve

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y = matrix) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function first checks if a cache of inverse matrix exists. If no,
## return the inverse of matrix.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
  
## Return a matrix that is the inverse of 'x'
}
