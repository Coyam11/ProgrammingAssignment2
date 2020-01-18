## This two functions will allow to calculate the inverse of a square matrix.
## If the matrix was already calculated the function will return the result from the cache. 
## Otherwise the function will computes the inverse matrix 

## The following function creates a list of four functions
## 1 - set(): set the value of the matrix
## 2 - get() : get the value of the matrix
## 3- setinv(): set the inverse of the matrix with the function solve
## 4- getinv(): get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function will check if the inverse of the matrix was already calculated.
## If so, will return the result from the cache and will save time because no computes.
## If the matrix is new, the following function will calculate the inverse matrix

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}