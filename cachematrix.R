## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function first initiates a matrix i
## defines a set function that initiates and sets x from the function input
## defines a get function that returns the value of x (if any) 
## defines a setinverse fnuction that assigns inverse matrix to i matrix
## defines getinverse function to return i value (if any)

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inv) {
    i <<- inv
  }
  
  getinverse  <- function() i
  
  list(set = set, get = get, setinverse = setinverse, getinverse  = getinverse )
}


## The cacheSolve function Checks for inverse of x and if present uses the saved inverse else 
##calculates inverse as well as caches the value for future use

cacheSolve <- function(x, ...) {
  i <- x$getinverse ()
  
  if(!is.null(i)) {
    message("getting cached inverse matrix")
    return(i)
  }
  message("creating cached inverse matrix")
  mat <- x$get()
  i <- solve(mat)
  x$setinverse(i)
  i
}


## Sample code and matrix tested as below

## > m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
## > m1
## [,1]  [,2]
## [1,]  0.50 -1.00
## [2,] -0.25  0.75
## > mMatrix <- makeCacheMatrix(m1)
## > mMatrix$get()
## [,1]  [,2]
## [1,]  0.50 -1.00
## [2,] -0.25  0.75
## > mMatrix$getinverse()
## NULL
## > cacheSolve(mMatrix)
## creating cached inverse matrix
## ## [,1] [,2]
## [1,]    6    8
## [2,]    2    4
## > m1 %*% mMatrix$getinverse()
## [,1] [,2]
## [1,]    1    0
## [2,]    0    1