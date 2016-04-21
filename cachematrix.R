## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set=set, get=get, setinv = setinv, getinv = getinv)
}



## The following function returns the inverse of the matrix created with the above function. 
#It first checks to see if the inverse has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation.
#Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data.")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinv(i)
  i
  
}
