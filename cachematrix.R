## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##makeCacheMatrix is a function that creates a list of functions when you call it.
## With set, you set the value of the matrix; with get, you get the value of the matrix
## With setsolve, you set the value of inverse matrix, Finally, with
## getsolve, you get the value of inverse matrix.
## With list you create a list with all of this functions.

makeCacheMatrix <- function(x = matrix()) {
  
  m_inverse <- NULL
  set <- function(y) {
    x <<- y
    m_inverse <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m_inverse <<- solve
  getsolve <- function() m_inverse
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)  
  
}


## Write a short comment describing this function
## cacheSolve returns the inverse of the matrix. It checks if
## the inverse matrix has already been entered. If so, it returns the result of inverse matrix 
##and skips the function, besides it displays a message saying that "getting cached data". 
##If not, it calculates the inverse of the matrix and sets the value in the 
## setsolve function.

## Besides, This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m_inverse <- x$getsolve()
  if(!is.null(m_inverse)) {
    message("getting cached data")
    return(m_inverse)
  }
  data <- x$get()
  m_inverse <- solve(data, ...)
  x$setsolve(m_inverse)
  m_inverse
}
