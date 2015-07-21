## Together these functions can create a matrix object that caches its
## own inverse to save timeworking it out in the future

## The makeCacheMatrix function creates a special matrix object that 
## can cache its inverse. It is a list which contains 4 functions:
## set - sets the matrix
## get - retrieves the matrix
## setinverse - stores the inverse in the cache
## getinverse - retrieves the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve funtion computes the inverse or retrieves the 
## inverse from the cache if it has been computed before using the 
## functions defined in makeCacheMatrix.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
