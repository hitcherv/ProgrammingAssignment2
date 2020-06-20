## The first function, makeCacheMatrix creates a special "Matrix", which is really a list containing a function to

##1) set the value of the Matrix
##2) get the value of the Matrix
##3) set the value of the inverse matrix
##4) get the value of the inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}



## The second function gets an inversion of the special "matrix" created 
##using the above function. However, it first checks whether it has already 
##received an inversion. If so, it obtains the inverse matrix from the cache
##and skips the computation. Otherwise, it calculates the data matrix 
##inversion and sets the matrix inversion in the cache using the set function.


cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}

