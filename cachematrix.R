## Matrix inversion is usually a costly computation and there may be some benefit to caching
## the inverse of a matrix rather than compute it repeatedly.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
      x <<- y     #set x in parent environment
      s <<- NULL     #set s in parent environment
  }
  get <- function() x #returns the matrix
  setinverse <- function(solve) s <<- solve #sets the matrix inverse
  getinverse <- function() s #returns the matrix inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" and returns the cache if the inverse has already been calculated.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s) #pull inverse from cache value
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}
