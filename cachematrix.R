## This function, makeCacheMatrix creates a special "matrix" which also helps to cache the inverse

#set the value of the matrix
#get the value of the matrix
#setinverse the value of the inverse
#getinverse the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  mres  <- NULL
  set  <- function(y) {
    x <<- y
    mres <<- NULL
  }
  get  <- function() x
  setinverse  <- function(solve) mres <<- solve
  getinverse  <- function() mres
  list (set = set, get = get, 
        setinverse = setinverse, getinverse = getinverse)
}

## The following function calculates the inverse of the special "matrix" created with the above function

#It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the mean in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        mres  <- x$getinverse()
        if (!is.null(mres)) {
          message("getting cached data")
          return (mres)
        }
        data  <- x$get()
        mres  <- solve(data, ...)
        x$setinverse(mres)
        mres
}
