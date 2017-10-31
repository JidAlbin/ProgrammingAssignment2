## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL				## Initialize the inverse property
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x		## Method to get the matrix
	setInverse <- function(solveMatrix) inv <<- solveMatrix							## Way to set the inverse of the matrix
    getInverse <- function() inv													## Way to get the inverse of the matrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)		## Revert the inversed property
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        								## Return: Inverse of the original matrix input to makeCacheMatrix()
  inv <- x$getInverse()
  
  if(!is.null(inv)){					##if the inverse has already been calculated
    message("getting cached data")		## get it from cache and skips computation
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)					## otherwise, calculates the inverse
  x$setInverse(inv)						## sets the value of the inverse in the cache
  inv      
}
