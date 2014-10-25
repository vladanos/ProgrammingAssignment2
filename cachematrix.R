## This package contains two functions that cache the inverse of a matrix.

## This function creates a special "matrix' object that can cache its inverse.
makeCacheMatrix <- function(mtx = matrix()) {
  
  ## Initialize the inverse property
  inv <- NULL
  
  ## Method to set the matrix
  set <- function(matrix) {
    mtx <<- matrix
    inv <<- NULL
  }
  
  ## Method to get the matrix
  get <- function() {
    mtx
  }
  
  ## Method to set the inverse
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  ## Method to get the inverse
  getInverse <- function() {
    inv
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by the "makeCacheMatrix" 
## function above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" function should retrieve the inverse from the cache.
cacheSolve <- function(mtx, ...) {
  ## Get the inverse from the object (if not cached value is null)
  inv <- mtx$getInverse()
  
  ## Return the inverse that is already cached (a matrix that is the inverse of 'mtx')
  if(!is.null(inv)) {
    message("Getting cached data.")
    return(inv)
  }
  
  ## Get the matrix from the object
  data <- mtx$get()
  
  ## Calculate the inverse (uses matrix multiplication)
  inv <- solve(data) %*% data
  
  ## Set the inverse in the object
  mtx$setInverse(inv)
  
  ## Return the calculated inversed (a matrix that is the inverse of 'mtx')
  return(inv)
}
