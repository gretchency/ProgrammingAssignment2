## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {    
    ## setter function of the matrix; initializes 'i' to null
    x <<- y
    i <<- NULL
  }
  get <- function() x     ## getter function for the matrix 'x'
  setinverse <- function(solve) i <<- solve       ## sets the 'i' matrix
  getinverse <- function() i                      ## returns i, the inverse matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()   ## gets the inverse matrix of 'x' and stores it in 'i', whose parent environment is cacheSolve function
  if(!is.null(i)) {
    message("getting cached data")
    return(i)  ## if inverse matrix of 'x' is already cached, this code returns the cached value 'i'
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(i)
  i
}
