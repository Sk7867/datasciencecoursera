## Function to create matrix object that can cache its inverse
# Args: x: The matrix which inverse to be calculated
# Returns: The object that can cache the inverse of the matrix.


makeCacheMatrix <- function(x = matrix()) {
   inverse.x <- NULL  # hold the inverse of a matrix
  set <- function(y) {
  x <<- y
    inverse.x <<- NULL
  }
  get <- function() x  
  setinverse <- function(inv) {
   inverse.x <<- inv   
  }
  getinverse <- function() inverse.x 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}
## This function computes matrix object and produces its inverse

cacheSolve <- function(x, ...) {
  inverse.x <- x$getinverse()  
  if(!is.null(inverse.x)) {  
    message("getting cached data")   
    return(inverse.x)  
  }
    data <- x$get() 
  inverse.x <- solve(data, ...)  
  x$setinverse(inverse.x)  
  inverse.x  
  
}

