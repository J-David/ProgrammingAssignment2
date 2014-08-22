## There are two functions in this document. Run two functions sequently will
## generate an inverted matrix of an input (invertible) matrix and put the inverted matrix in cache.
## Use two step to generate the inverted matrix (assume x is an invertiable matrix)
## First step
## y <- makeCacheMatrix(x)
## Second step
## cacheSolve(y)


## The first function, makeCacheMatrix creates a list of four functions to
## set the value of a matrix
## get the value of a matrix
## set the value of a matrix
## get the value of a matrix

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvert <- function(solve) m <<- solve
  getinvert <- function() m
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
}


## When called to invert a matrix, the second function will first check the cache,
## if the inverted matrix is available, the feteched inverted matrix will be returned;
## if the inverted matrix isn't available, the new input marix will be fetched, inverted, 
## cached, and returned.

cacheSolve <- function(x, ...) {
  m <- x$getinvert()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvert(m)
  m
}