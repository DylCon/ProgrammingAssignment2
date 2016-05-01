
# Dylan Connor - Programming Assignment 2: Lexical Scoping

#Code Notes  --------------------------------------------------------------
# The following functions can be used to calculate and cache the inverse of
# matrix. Then when the inverse is to be called, the function will check if
# the inverse is cached. If the inverse has already been calculated and the
# matrix has not been altered, the function will return the cached value.
# If the matrix is altered, the cached matrix is NULLed out.
#
# Note: The function assumes the matrix is invertable and has no error
# handling beyond the standard solve function message for non invertable 
# matrices.
#--------------------------------------------------------------------------

#makeCacheMatrix Notes  ---------------------------------------------------
# The following function sets up an object that stores a matrix and cache's 
# its inverse.
#--------------------------------------------------------------------------

# Code makeCacheMatrix --------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setInverse <- function(InverseMat) m <<- InverseMat
      getInverse <- function() m
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}

#cacheSolve Notes  --------------------------------------------------------
# The following function searches to see if a variable designated to cache
# the inverse of a function is NULL or is currently storing the inverse 
# of the matrix. If it is NULL, the function returns the inverse of the input
# matrix. Else it returns the cached matrix.
#--------------------------------------------------------------------------

# Code cacheSolve --------------------------------------------------------------------

cacheSolve <- function(x, ...) {
      m <- x$getInverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      dataMat <- x$get()
      m <- solve(dataMat, ...)
      x$setInverse(m)
      m
}