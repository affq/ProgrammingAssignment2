makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL #variable to store the matrix inverse

  # sets a new matrix and clears any previously cached inverse
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }

  # returns the stored matrix
  get <- function() x

  # sets the inverse of the matrix
  setinverse <- function(i) inverse <<- i

  # retrieves the cached inverse
  getinverse <- function() inverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()

  # if the inverse is already cached, return it
  if(!is.null(inverse)){
    message('getting inverse from cache')
    return(inverse)
  }

  # otherwise, get the matrix
  matrix <- x$get()

  # compute the inverse
  inverse <- solve(matrix, ...)

  # cache the result
  x$setinverse(inverse)

  # return the computer inverse
  inverse
}
