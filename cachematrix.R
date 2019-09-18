## These functions are able to calculate the inverse of a matrix and cache it. 
# Then it is possible to retrieve the cached value   

## This function is inspired from the example Caching the Mean of a Vector
#In the first part x is set to be am matrix and the inverse is stored in the inverse object which is initially NULL
#Moreover, the set function is able to reset the value to null every time you reset the value.


makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<-NULL
  }      
  get <- function() x
  setInverse <- function(solution) inverse <<- solution
  getInverse <-  function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## In this second part cacheSolve solves the inverse of the matrix and returns the value of the inverse matrix

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {message('getting cached matrix')
    return(inverse)
  }
  
  mat <- x$get()
  inverse <- solve(mat)
  x$setInverse(inverse)
  inverse ## Return a matrix that is the inverse of 'x'
  
}
