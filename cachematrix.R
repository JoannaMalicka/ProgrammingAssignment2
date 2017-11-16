##Coursera R Programming Course - Programming Assignment2
##The following functions compute and cache the inverse of a matrix.
##The cached value is reset everytime a new matrix is given

##makeCacheMatrix creates a list of functions: set, get, setinv and getinv.
##set: sets the value of a matrix
##get: gets the value of a matrix
##setinv: sets of the value of the inverse matrix
##getinv: gets of the inverse matrix;


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

##The cacheSolve function checks if the matrix inverse has been calculated.
##If yes, it returns the cached matrix inversion.
##If not, it calculates the inversion and caches the value.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}