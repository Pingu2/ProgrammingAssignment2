## 
## These functions work together to return the inverse of a matrix
## without needing to compute it repeatedly 


## This function takes a square invertible matrix
## It creates a cache storing the value of matrix and it's inverse
makeCacheMatrix <- function(x = matrix()) {
  i<- NULL
  set<- function(y){
    x<<-y
    i<<- NULL
  }
  get<- function()x
  setInverse<- function(solve) i<<- solve
  getInverse<- function()i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}






## Write a short comment describing this function
## This function takes the object created by inputting a matrix of data in 
## the makeCacheMatrix function.  It checks to see if the inverse is stored 
## in the cache. If the inverse has been processed already then the is retuned, 
## otherwise it is computed, set in cache and then returned.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i<- x$getInverse()
  if(!is.null(i)){
    message("Getting cached data")
    return(i)
  }
  data<- x$get()
  i<- solve(data,...)
  x$setInverse(i)
  i
}



