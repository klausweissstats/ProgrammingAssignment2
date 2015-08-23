## Put comments here that give an overall description of what your
## functions do

## an object that stores matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  # inverse
  inv <- NULL
  
  #set data method
  set <- function(y) {
    x <<- y
    inv <- NULL
  }
  
  #get data method
  get <- function() x
  
  #set inverse method
  setInv <- function(y) inv <<- y
  
  #get inverse method
  getInv <- function() inv
  
  #return statement
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  #if inverted matrix exists in object return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  
  #calculate inverse, cache it and return
  inv <- solve(data, ...)
  x$setInv(data)
  data
}
