
## This function creates a makeCacheMatrix object that stores a matrix
## and its inverse. $get retrieves de matrix, $getInverse its inverse

makeCacheMatrix <- function(x = matrix()) {
    Inv <- NULL
    set <- function(y) {
      x <<- y
      Inv <<- NULL
    }
    get <- function() x
    setInverse <- function(Inverse) Inv <<- Inverse
    getInverse <- function() Inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  }


## This function calculates the inverse of the matrix created as a
## makeCacheMatrix object if it was not created before. If it was already
## created, they retrieve that value from the object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inv <- x$getInverse()
  if(!is.null(Inv)) {
    message("Retrieving cached data")
    return(Inv)
  }
  data <- x$get()
  Inv <- solve(data, ...)
  x$setInverse(Inv)
  Inv
}
