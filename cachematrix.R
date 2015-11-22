## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.
## This pair of functions caches the inverse of a matrix.
## In the end of file I put some (commented) line to test code

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  # here assign solve(param) to m
  setmean <- function(mean) m <<- solve(mean)
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  # here assign solve(data) to m
  m <- solve(data, ...)
  x$setmean(m)
  m
}
## Verification code
## You can uncomment the follow lines to check it's right
## A new matrix m
# m <- matrix(runif(16),4,4)
## Call to makeCacheMatrix
# test <- makeCacheMatrix(m)
# test
## First call to cacheSolve()
# cacheSolve(test)
## Check if solves correctly
# solve(m)
## Second call to cacheSolve(), print "getting cached data" before
# cacheSolve(test)
