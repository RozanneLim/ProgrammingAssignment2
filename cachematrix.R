## The following functions first create a special matrix object that can cache its inverse, and the following function
##computes the inverse of the special matrix object if one has not been calculated already. If it has been calculated already, 
## it is able to retrieve the cached inverse stored in the previous function 

## This function creates a special matrix object that can cache its inverse. It returns a list of methods that can be called to 
## set and get the vector, as well as calculate the mean 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL #stores cached inverse of matrix 
  set <- function(y){
    x <<- y #updates the matrix 'x'
    i <<- NULL #sets i to NULL if matrix is updated to recalculate it 
  }
  get <- function() x
    setinv <- function(solve) i <<- solve
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function calculates the inverse of the special matrix created. It first
#checks to see if the inverse has been calculated. If so, it gets the value from the 
#cache in the first function. It not, it calculates the inverse of the matrix and caches the value
#using the set function.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if (!is.null(i)){
    message ("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinv(i)
  i ## Return a matrix that is the inverse of 'x'
}
