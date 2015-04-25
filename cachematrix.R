## Code to store a matrix and its inverse in a optimized (cached) way

## Function that store a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(m) inverseMatrix <<- m
  getinverse <- function() inverseMatrix
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function that calculates and returns the inverse of a given matrix,
## checking if is stored previously in order to avoid recalculate it
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
  
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data) %*% data
    x$setinverse(m)
    m
}
