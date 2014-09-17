## This functions can solve the matrix and you have chance to
## do it faster, because of we use cache for already solved matrix.

## makeCacheMatrix creates an object, that manage your origin and
## cache data.

## Usage of the function
# mtx<-makeCacheMatrix(x) - creates a list, that store matrix "x"
# mtx$set(new)            - change origin matrix in the list to "new"
# mtx$get()               - get the matrix from the list
# mtx$setsolved(solved)   - set solved matrix to cache.
# mtx$getsolved()         - get a solved (inverted) matrix from cache,
#                           or NULL if it doesn't exist


makeCacheMatrix <- function(x = matrix()) {
  
  solvedX <- NULL
  
  # set the new origin matrix and drop the cache
  set <- function(y) {
    x <<- y
    solvedX <<- NULL
  }
  
  getorigin <- function() x
  
  setsolved <- function(solved) solvedX <<- solved
  
  getsolved <- function() solvedX
  
  list(set = set, get = get,
       setsolved = setsolved,
       getsolved = getsolved)
}


## cacheSolve computes the inversion of the matrix in "makeCacheMatrix"
## list. If the computed value already exists, cacheSolve will return
## cached value from the list.

## Usage of the function
# cacheSolve(x)           - returns the inverse of the "matrix" x
#                           x must be a list, produced by
#                           makeCacheMatrix() function

cacheSolve <- function(x, ...) {
  
  # Trying to get already computed matrix from cache
  m <- x$getsolved()
  
  # if isset cached data - return it. Because of if matrix has changed,
  # we set NULL to solvedX in makeCacheMatrix$set function.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # Compute the inversion of the matrix and put it into cache.
  data <- x$getorigin()
  m <- solve(data, ...)
  x$setsolved(m)
  m
}
