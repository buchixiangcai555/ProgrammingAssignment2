## MakeCacheMatrix() and cacheSolve() are a pair of functions that cache the inverse of a matrix

## MakeCacheMatrix() creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse property
  i <- NULL
  
  ## Set the matrix
  set <- function( matrix ) {
    ## Use `<<-` to assign a value to an object in an environment 
    x <<- matrix
    i <<- NULL
  }
  
  ## Get the matrix
  get <- function() x
  
  ## Set the inverse of the matrix
  setinverse <- function(inverse) {
    i <<- inverse
  }
  ## Get the inverse of the matrix
  getinverse <- function() i
  
  ## Return a list of the methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## CacheSolve() calculates the mean of the inverse of the matrix returned by makeCacheMatrix().
## It will determine whether the mean has been calculated to determine how to obtain the mean.

cacheSolve <- function(x, ...) {
  ## Get a matrix that is the inverse of 'x'
  i <- x$getinverse()
  
  ## Return the inverse of the matrix if its already set
  if( !is.null(i) ) {
    message("getting cached data")
    return(i)
  }
  
  ## Get the inverse of the matrix from the object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  i <- solve(data, ...)
  
  ## Set the inverse of the matrix to the object
  x$setinverse(i)
  
  ## Return the matrix
  return(i)
}
