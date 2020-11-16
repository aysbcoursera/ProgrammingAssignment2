## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This allows me to create a matrix that is invertible.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## This allows me to set the initial values of the matrix.
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  ## This allows me to basically set the inversed values of my matrix. 
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This allows me to compute the inverse of the matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  ## This IF statement allows me to retrieve the already solved-for inverse of my matrix, if available.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
