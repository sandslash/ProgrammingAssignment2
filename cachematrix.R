## These functions are for assignment2 on Coursera's R programmign course
## comments for each function below.

## makeCacheMatrix accepts a matrix and caches it to save computing resources later on


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Cache solve will return the inverse if it has already been created, 
## If the inverse hasn't been previously created, it will create it, cache it with makeCacheMatrix, 
## and return the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
