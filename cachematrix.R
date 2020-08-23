## Pairs of function to cache the inverse of a matrix
## functions do

## This function, makeCacheMatrix creates a list containing a function to
## 1. set the value of a matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of matrix
## 4. get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The following function calculates the inverse of a matrix with above function on:
## 1. check and return if inverse is found in cache
## 2. otherwise, calculate inverse, cache it and return
## note: solve(X) returns inverse of matrix x.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...) #
  x$setinv(i)
  i
}
