## Function “makeCacheMatrix” creates a special “matrix” object that can cache its inverse. 
## makeCacheMatrix creates a list containing four functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Function "cacheSolve" computes the inverse of the special matrix returned by 'makeCacheMatrix'. 
## It first checks if the inverse has already been calculated (and the matrix has not changed),
## then 'cacheSolve' should retrieve the inverse from the cache.
## This function assumes that the matrix is always invertible and square.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
