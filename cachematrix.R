## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function creates a special "vector", which is
# really a list containing a function to
# 
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  calculate and set the inverse of the matrix
# 4.  get the value matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    xInv <- NULL

#set function pushes the values outside this environment
    set <- function(y) {
      x <<- y
      xInv <<- NULL
    }
    get <- function() x

#this function pushes the inverse to the higher environment
    setInv <- function(solve) xInv <<- xInv
    getInv <- function() xInv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
  }

## Write a short comment describing this function
# The following function calculates the inverse of the matrix in the special "vector"
# created with the above function. However, it first checks to see if the
# inverse has already been calculated. If so, it `get`s the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the matrix and sets the value of the inverse (xInv) in the cache via the `setInv`
# function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    xInv <- x$getInv()
    if(!is.null(xInv)) {
      message("getting cached data")
      return(xInv)
    }
    data <- x$get()
    xInv <- solve(data, ...)
    x$setInv(xInv)
    xInv 
  
}
