##makeCacheMatrix takes a matrix as an argument and returns
## a list with four functions: set(y=matrix()) sets 
##matrix data into a global variable to cache it, get() returns
##the data from the same global/cached matrix variable, 
##setinverse(inv=numeric()) sets a global variable to 
##act as the cache of the inverse, and getinverse() returns the 
##value of the same global cached variable

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inversem) inv <<- inversem
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse,
       getinverse=getinverse)
}


##cacheSolve calculates the inverse of the special "matrix" 
##created with the above function. However, it first checks to 
##see if the inverse has already been calculated. If so, it gets
#the inverse from the cache and skips the computation. Otherwise,
##it calculates the inverse of the data and sets the value of 
##the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}
