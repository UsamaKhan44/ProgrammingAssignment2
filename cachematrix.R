## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function is setting the value x with inverse value of matrix. Furthermore,
##you can change the value of matrix "x" by "set" command and verify the value of 
##"x" used in the code by "get" command. In the same way, we can set the value
##of inverse of matrix by "setInverseSquare" and get the value of inverse of a 
##previous matrix by "getInverseSquare".

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  inverseSquare <- function(y){
    x <- solve(y)
  }
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverseSquare <- function(inverseSquareValue) m <<- inverseSquareValue
  getInverseSquare <- function() m
  list(set = set, get = get,
       setInverseSquare = setInverseSquare,
       getInverseSquare = getInverseSquare)
}
  



## Write a short comment describing this function
##In this command, we are checking whether the inverse value of a matrix is cached
##or not. If not, then it will compute the value otherwise it would end this 
##function


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverseSquare()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverseSquare(m)
  m
  
}

