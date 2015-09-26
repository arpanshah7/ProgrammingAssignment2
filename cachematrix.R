## This function takes a matrix as input and caches it 
## The function will cache the matrix x

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## The fuction will return the inverse of matrix x. 
## If the inverse is already calculated, it will retrieve from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }

  data <- x$get()
  m <- solve(data)
  x$setmatrix(m)
  m
}
