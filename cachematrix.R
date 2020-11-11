## The functions makeCacheMatrix and cacheSolve work together to cache a matrix's inverse,
## so it doesn't need to be recalculated (after calculation through cacheSolve).
## The cache is cleared when a new matrix is inputted into makeCacheMatrix.
## e.g. A <- makeCacheMatrix(mmatrix(c(1/2, -1/4, -1, 3/4), nrow = 2))
##      A$getinv() returns NULL, until cacheSolve(A) is run.

## makeCacheMatrix: list of functions that allows for creating/viewing/caching the inverse of a matrix
##                  $set() for inputting new matrix
##                  $get() for checking what the current matrix is
##                  $setinv() for inputting new inverse into the cache when cacheSolve is run
##                  $getinv() for checking inverse (should be NULL if cacheSolve has not been run)

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y # replaces the previous matrix x with new matrix y
    i <<- NULL # clears any cached inverse
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve: checks the cache for the matrix's inverse - if an inverse is cached, returns it.
##             Else, gets the current matrix in makeCacheMatrix and calculates its inverse,
##             then sets that inverse in the cache. Also returns the calculated inverse.

cacheSolve <- function(x, ...) {
  i <- x$getinv() # checks cache for inverse
  if (!is.null(i)) { 
    message("Getting cached data")
    return(i) # if an inverse is cached, return it
  }
  data <- x$get() 
  i <- solve(data)
  x$setinv(i)
  i
}


