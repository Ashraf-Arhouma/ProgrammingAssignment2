## Pair of functions that cache the inverse of a matrix.

## Function to Create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {      
  inv <- NULL
  # This function to set a new matrix.
  set <- function(y) {
      x <<- y
      inv <<- NULL   }
  
  # This function which returns the value of the original matrix.
  get <- function() x
  
  # This function is called by cacheSolve() during the first access to set an inverse.
  setinv <- function(inverse) inv <<- inverse
  
  # This function will return the cached value to cacheSolve() on
  # subsequent accesses.
  getinv <- function() inv
  
  ## Return list of the functions.
  list(set = set, get=get,
       setinv = setinv,
       getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  ## Return the inverse from the chace if it is already set.
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## Calculate the inverse using solve() function and set it to the object.
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  
  ## Return the inverse of the matrix
  inv
}