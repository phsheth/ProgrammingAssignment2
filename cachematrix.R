# Makes a matrix cache and retrieves it after checkig if it is square or not.

#The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to:

#1. set the value of the matrix
#2. get the value of the matrix - Also checks if the matrix is square - if not - it breaks the function execution in cacheSolve
#3. set the value of the inverse of matrix
#4. get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  matinv <- NULL
  set <- function(y) {
    x <<- y
    matinv <<- NULL
  }
  get <- function() {
    if((!(nrow(x) == ncol(x))) ==TRUE) {
      message("Matrix is not square")
      break
    } else {
      x
    }
  }
  setinv <- function(solve) matinv <<- solve
    
  getinv <- function() matinv
  list(setinv = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


#The following function calculates the inverse of the special "vector" created with the above function.
#However, it first checks to see if the inverse of matrix has already been calculated.
#If so, it gets the inverse of matrix from the cache and skips the computation.
#Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinv function.
#Function does not get executed if provided matrix was not square matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matinv <- x$getinv()
  if(!is.null(matinv)) {
    message("getting cached data")
    return(matinv)
  }

  data <- x$get()
  matinv <- solve(data, ...)
  x$setinv(matinv)
  matinv
}