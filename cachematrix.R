# These functions create an object that stores a matrix and its inverse.
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  my_inverse <- NULL
  set <- function(y) {
    x <<- y
    my_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) my_inverse <<- inverse 
  getinverse <- function() my_inverse
  list(set=set, 
       get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}


# cachesolve:This function computes the inverse of the special "matrix" returned by makeCacheMatrix
# above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve'
# should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  my_inverse <- x$getinverse()
  
  # check for the inverse in cache
  if (!is.null(my_inverse)){
    # return the cache version
    message("getting cached data")
    return(my_inverse)
  }
  
  # calculate the inverse 
  matrix_data <- x$get()
  my_inverse <- solve(matrix_data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinverse(my_inverse)
  
  my_inverse
}
