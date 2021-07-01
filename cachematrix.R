## makeCacheMatrix creates a list of closures that a) Save the inverse of argument matrix if there doesn't exist a cached copy
## b) can retrieve and return the cached copy from the function's environment.

## cacheSolve takes a matrix variable as an argument and checks if makeCacheMatrix has a Cached copy of the inverse to that matrix and acts accordingly.



makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}




cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
