## There are 2 functions: makeCacheMatrix and cacheSolve. The makeCacheMatrix function creates an R object that stores a 
## matrix and its inverse. It returns a list of get and set functions. The cacheSolve function calculates the inverse of 
## the matrix stored in the R object created by makeCacheMatrix.

## The makeCacheMatrix function creates an R object that stores a matrix and its inverse. It contains 4 getter-setter 
## functions to set/ get the matrix or its inverse. The function returns a list of these get and set functions to the parent 
## environment.

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function(newMat) {
    x <<- newMat
    invMatrix <<- NULL
  }
  get <- function() x
  setinv <- function(invMat) invMatrix <<- invMat
  getinv <- function() invMatrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv) 
}


## The cacheSolve function checks if the inverse of the matrix stored in the R object created by makeCacheMatric has 
## already been calculated before. If the inverse hasn't been calculated, it calculates the inverse of the matrix 
## and stores it in the R object. If it has, then the inverse is just retreived from the R object.

cacheSolve <- function(x, ...) {
  invMatrix <- x$getinv()
  if(!is.null(invMatrix)) {
    message("getting cached data")
    return(invMatrix)
  }
  data <- x$get()
  invMatrix <- solve(data)
  x$setinv(invMatrix)
  invMatrix      
}
