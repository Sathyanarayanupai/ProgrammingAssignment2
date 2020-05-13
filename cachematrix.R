## Put comments here that give an overall description of what your functions do
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix())   #This function creates a special "matrix" object that can cache its inverse.
{
  inv <- NULL
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse)  inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x,...)
{
  #This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
  #If the inverse has already been calculated (and the matrix has not changed), 
  #then the cachesolve should retrieve the inverse from the cache.
  inv <- x$getInverse()
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  requiredmatrix <- x$get()
  inv <- solve(requiredmatrix,...)
  x$setInverse(inv)
  inv
}