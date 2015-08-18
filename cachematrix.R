## makeCacheMatrix creates an object vid functions to get and set the matrix
## and to get and set the inversion.
makeCacheMatrix <- function(x = matrix()) {
  matrix <- x
  inversion <- NULL
  set <- function(x) {
    matrix <<- x
    inversion <<- NULL
  }

  get <- function() { matrix }
  setinversion <- function(solved) { inversion <<- solved }
  getinversion <- function() { inversion }
  list(set = set, get = get, setinversion = setinversion, getinversion = getinversion)
}


## Takes an matrix as argument and returns the inversion of it
## if the inversion is alread calculated it returns the cached value
## otherwise it sets it and clears the cache.
cacheSolve <- function(x) {
  inversion <- x$getinversion()

  if (!is.null(inversion)) {
    inversion
  }

  matrix <- x$get()
  inversion <- solve(matrix)
  x$setinversion(inversion)
}
