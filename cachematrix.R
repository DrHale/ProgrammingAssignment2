## Put comments here that give an overall description of what your
## functions do

## Creates a cacheMatrix object from a numeric matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computes and caches inverse of cacheMatrix matrix

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
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

# Test functions

n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2,ncol = 2)
testm = makeCacheMatrix(n2)
cacheSolve(testm)
cacheSolve(testm)

