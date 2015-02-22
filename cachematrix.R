# The makeCacheMatrix function creates and returns four values:
#	- set: set the value of the matrix
#	- get: get the value of the matrix
#	- setinv: set the inverse of the matrix in the cache
#	- getinv: get the inverse of the matrix from the cache

# cacheSolve then checks to see if the inverse value of the matrix is stored in the cache
#	and either returns the stored value or calculates and stores the inverse value in the cache



makeCacheMatrix <- function(x = matrix()) {
#Initialise the cached value to NULL
  inv = NULL

#set the matrix
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  
#get the stored matrix
  get = function() x
  
#cache the inverse value of the matrix
  setinv = function(inverse) inv <<- inverse
  
#get the cached inverse value
  getinv = function() inv
  
#return the created functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


cacheSolve <- function(x, ...) {
#Get the inverse value from the cache (if it does not exist, this will be NULL)
  inv = x$getinv()
  
#If 'inv' has already been calculated and stored in the cache
# it's value will be retrieved from the cache and it will not be recalculated
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
#Generate a value for the inverse
  matrix_data = x$get()
  inv = solve(matrix_data, ...)
  
#Set the value of the inverse in the cache
  x$setinv(inv)
  
#Return the value of the inverse
  return(inv)
}