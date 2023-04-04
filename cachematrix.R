# Demonstrate how to solve a matrix
hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, `+`) }
h8 <- hilbert(8)
sh8 <- solve(h8)

## These functions solve and return matrix inverses

## This is a function that establishes a cached matrix with its inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}




## This is a function that returns the cached solve/inverse of a matrix

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached solve")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s      ## Return a matrix that is the inverse of 'x'

  
}

# Demonstrate the function working
h8cache <- makeCacheMatrix(h8)
cacheSolve(h8cache)
