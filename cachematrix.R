## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  # set inv value null
  inv <- NULL
  # set the matrix value
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #get the matrix value
  get <- function() x
  # set inverse of matrix value
  setInverse <- function() inv <<- solve(x) #calculate the inverse
  #get inverse of matrix value
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
 }


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  #Check if the value is in Cache if yes return it from cache
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # if data is not cached then calculate
  #print x matrix
  x
  #Calculate the determinant 
  det(x)
  # Using solve function calculate the inverse of x
  inv <- solve(x)
  inv
  #Setting the inverse matrix value in cache
  x$setInverse(inv)
  ## Return a matrix that is the inverse of 'x'
  inv
}
