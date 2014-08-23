## This file includes functions that cache the inverse of a matrix.

## makeCacheMatrix is a function that constructs a 
## cache for the inverse of the given matrix.
makeCacheMatrix <- function(mat = matrix()) {
  
  inv <- NULL
  
  set <- function(newmat) {
    mat <<- newmat  # Replace the matrix with a new one ...
    inv <<- NULL    # ... and reset the inverse, it's no longer relevant
  }
  
  # Get the original matrix
  get <- function() mat
  
  # Set the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Get the inverse (which may be NULL)
  getInverse <- function() inv
  
  # A list of functions
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve returns the inverse of a matrix,
## either from a previously calculated and cached copy,
## or by calculating it from scratch by calling the solve() function.
## The cm argument must be an object returned from makeCacheMatrix.
## This assumes the matrix is invertible, i.e. no special
## handling is implemented for this case.
cacheSolve <- function(cm, ...) {
  
  inv <- cm$getInverse()
  
  if (!is.null(inv)) {
    message("...Found inverse in cache")
    return(inv)
  } else {
    message("...Computing inverse")
    inv <- solve(cm$get(), ...)
    cm$setInverse(inv)
    return(inv)
  }
  
}

## Handy testing code.  This function runs various tests on the functions above,
## including basic logical checks, plus checking that we get the correct inverse.
testMatrixCache <- function(size = 3) {

  # Create a test matrix
  mat <- matrix( data = rnorm(size * size), nrow = size, ncol = size )
  message("Test matrix:")
  print(mat)
  
  # Start the test
  cm <- makeCacheMatrix(mat)
  
  # The cached inverse should be null at the start
  if (is.null(cm$getInverse())) {
    message("Test 1: OK")
  } else {
    message("Cached inverse is not null")
  }
  
  # Solve it once and check we get something back
  inv <- cacheSolve(cm)
  if (!is.null(inv)) {
    message("Test 2: OK")
  } else {
    message("Calculated inverse is null")
  }

  # Check that the cached value is now set
  if (!is.null(cm$getInverse())) {
    message("Test 3: OK")
  } else {
    message("Cached inverse is not null")
  }

  # Solve it again and check we get something back
  inv <- cacheSolve(cm)
  if (!is.null(inv)) {
    message("Test 4: OK")
  } else {
    message("Cached inverse is null")
  }

  # Check that this is the correct inverse (just for fun really)
  # Multiply the inverse by the original, subtract the unit matrix, and check 
  # that the result is all zero.  Rounding is used to deal with micro values.
  z<-round(inv %*% mat, 10) - diag(size)
  if (all(z == 0)) {
    message("Test 5: OK")
  }
}
