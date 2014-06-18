## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## Manage the entry matrix to be solved and the storring the solved matrix in cache
  ## Contains four subfunctions: set, get, setinverse and getinverse
  ## x contains the matrix to be inverted
  ## m contains the inverted matrix
  
  m <- NULL
  
  ## Loads the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## Retrieves the loaded matrix
  get <- function() x
  
  ## Stores the inversed matrix in cache
  setinverse <- function(inverse) m <<- inverse
  
  ## Retrieves the inversed matrix from cache
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Check if the matrix has been solved previously and stored in cache
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## In case no revious calculation performed, load the matrix to be solved
  data <- x$get()
  
  ## Solve the matrix
  m <- solve(data, ...)
  
  ##  Store the content in cache for later reuse
  x$setinverse(m)
  m
  
}