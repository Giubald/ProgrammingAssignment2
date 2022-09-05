## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  h <- NULL
  
  set <- function(y) {
    x <<- y
    h <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) h <<- inverse
  getinverse <- function() h
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  h <- x$getinverse()  #h is the inverse of the matrix, coming from makeCacheMatrix
  if (!is.null(h)) {     #if h is not zero, this means that the inverse was already calculated and to save up computational time, we get its value from the cache memory
    message("the inverse of the matrix is coming from the cache")
    return(h)
  }
  data <- x$get()   #if h is empty, then we start the inverse calcularion
  h <- solve(data, ...) #the real inverse operation for the matrix
  x$setinverse(h)
  h
}
