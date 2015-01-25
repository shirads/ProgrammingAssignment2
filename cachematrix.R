## 'cacheMatrix.R' contains two functions, 'makeCacheMatrix' and 'cacheSolve',
## to create and cache the inverse of a given matrix for later use. When the Matrix
## changes, its inverse will be created, otherwise, the existing inverse will be returned

## 'makeCacheMatrix' contains four functions
## 1. set(): Sets a new matrix
## 2. get(): Returns the original matrix
## 3. setinverse(): Sets the inverse of the matrix
## 4. getinverse(): Gets the inverse of the matrix, or NULL if not set

# The attribute 'x' is the given matrix
makeCacheMatrix <- function(x = matrix()) {
  # 'm' is the mean initiated to NULL
  m <- NULL
  
  # 'set()' sets a new matrix
  set <- function(y) {
    # Check if the 'y' attribute is actually a Matrix
    if (!is.matrix(y)) {
      # If not, notify the user
      message(y, " is not a matrix. Instead it is a ", class(y), ". Please try again.")
    }
    else {
      # Otherwise, assign the given matrix to 'x'
      x <<- y
      
      # And set the mean 'm' in the upper environment to 'NULL'
      m <<- NULL
    }
  }
  
  # The call to 'set()' below is executed after the 'makeCacheMatrix()' function R object is created
  # It will check if 'x' is a matrix, or stop otherwise
  set(x)
  
  # 'get()' returns the original matrix
  get <- function() x
  
  # 'setinverse' sets the inverse of the original matrix
  setinverse <- function(solve) {
    # Check if the 'solve' attribute is a matrix
    if (!is.matrix(solve)) {
      # If not, notify the user
      message(solve, " is not a matrix. Instead it is a ", class(solve), ". Please try again.")
    }
    else {
      # Otherwise, assign the mean to 'm' in the upper environment using '<<-'
      m <<- solve
    }
  }
  
  # 'getinverse()' return the cached inverse of the matrix
  getinverse <- function() m
  
  # This is the list of functions in 'makeCacheMatrix'
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## 'cacheSolve()' requests the inverse of a existing matrix in given function object 'x'
## If the inverse is not NULL, the cached inverse is used
## Otherwise, the inverse is calculate and assigned in the function object 'x' environment

cacheSolve <- function(x, ...) {
  # First, request the existing inverse matrix from 'x'
  m <- x$getinverse()
  
  # Check if the inverse matrix is not NULL
  if(!is.null(m)) {
    # If it is not NULL, inform the user that we will use the cached version
    message("Getting cached matrix ...")
    
    # Return the cached version
    return (m)
  }
  
  # Otherwise, get the original matrix
  data <- x$get()
  
  # Computer its inverse
  m <- solve(data, ...)
  
  # Assign the inverse matrix to the inverse variable 'm' in 'x' environment
  x$setinverse(m)
  
  # Return the matrix inverse to the user
  m
}