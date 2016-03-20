##
## OVERVIEW OF FUNCTIONS IN R SCRIPT
## 
## This R Script contains code for two functions:
##    1. makeCacheMatrix(); and
##    2. cacheSolve()
##
## The function makeCacheMatrix() creates a special "matrix" object
## that can cache its inverse.
## 
## The function cacheSolve() computes the inverse of the special "matrix"
## object outlined above. If the inverse has already been calculated,
## then cachesolve will retrieve the inverse from the cache.
##
##
##
## MAKECACHEMATRIX() FUNCTION
##
## The makeCacheMatrix() function creates a special "matrix" object.
##
## This "matrix" object is actually a list containing functions to:
##    1. set the value of the matrix -> set()
##    2. get the value of the matrix -> get()
##    3. set the value of the inverse matrix -> setinverse()
##    4. get the value of the inverse matrix -> getinverse()
##

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse = matrix()) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##
## CACHESOLVE() FUNCTION
##
## The function cacheSolve() computes the inverse of the special "matrix".
##
## If the inverse has already been calculated (and the matrix has not
## changed), then cachesolve() will retrieve the inverse from the cache.
##
## The following comments provide an explanation of the various passages of
## code that have been earmarked for comment within the function below.
##         
## Code 1: Retrieves the cached inverse (if it exists).
##         If the cache doesn't exist, then proceed to calculate the inverse.
##
## Code 2: Input matrix is stored in 'mat' object.
##         The 'mat' object is then tested to ensure that it is:
##         
##              1. a (strict) matrix
##              2. a square n-by-n matrix
##              3. a non-singular matrix       
##         
##         The tests above are used to ensure that an inverse exists for
##         the input matrix.
##         
##         If any of the tests are false, then an error message is returned.
##         If the input matrix is invertable, then proceed.
##         
## Code 3: Compute the inverse of the input matrix and cache the result in
##         the special "matrix" object created by the makeCacheMatrix()
##         function above. The identity matrix in 2nd argument of diag()
##         is calculated based on the number of rows in the 'mat' object.
##         


cacheSolve <- function(x, ...) {
  
  i <- x$getinverse()   ## Code 1
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }

  mat <- x$get()    ## Code 2
  
  if(is.matrix(mat) == FALSE) {
    stop("Input object is not a matrix: cannot proceed.")
  }
  else if(nrow(mat) != ncol(mat)) {
    stop("Input matrix is not invertable: it is not a n-by-n square matrix.")
  }
  else if(det(mat) == 0) {
    stop("Input matrix is not invertable: it is a singular matrix.")
  }
  else if(det(mat) != 0) {    ## Code 3
    i <- solve(mat, diag(x = 1, nrow = nrow(mat)), ...)
    x$setinverse(i)
  }
  print(i)
}

