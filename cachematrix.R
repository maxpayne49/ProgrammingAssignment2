#Matrix inversion is usually a costly computation and there are benefits to caching
#the inverse of a matrix rather than compute it repeatedly. The following pair of
#functions cache the inverse of a matrix.

# The function makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL            #provides a default if cacheSolve has not yet been used
  y <- NULL              #provides a default if cacheSolve has not yet been used
  set <- function(y) {   #sets the value of the matrix
    x <<- y              # caches the inputted matrix so that cacheSolve can check whether it has changed
    inv <<- NULL         # sets the value of inv (the matrix inverse if used cacheSolve) to NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# The function cacheSolve returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.
# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()   # if an inverse has already been calculated this gets it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)   # run the setInverse function on the inverse to cache the inverse
  inv                 # return the inverse
}
