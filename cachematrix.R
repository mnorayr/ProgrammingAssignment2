#Norayr
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special matrix object, which can set its value,
## get its value, set the value of the inverse, get the value of the inverse  
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function(){x} 
  setinv <- function(inv) {m <<- inv}
  getinv <- function() {m}
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## This function checks if the inverse has been set, 
## if yes, gets it from the stored variable, if not 
## calculates and stores it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
