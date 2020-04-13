## We create a special Matrix where the value of its inverse is cached


## makeCacheMatrix is the contructor which returns a list
## of functions that act as getters and setters for the actual 
## matrix and cached inverse value

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  get <- function () x
  setinverse <- function (i) inv <<- i
  getinverse <- function () inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following calculates the inverse by checking if already a value
## exists in cache. It calculates the inverse only if no value was
## present

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  if (!is.null(inv)) {
    message("getting cached data")
    x$getinverse()
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  
  inv
}
