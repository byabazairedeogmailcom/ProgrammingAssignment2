## Assignment on caching the Inverse of a Matrix
## functions do

## Creating a function thats creates a matrix caching it's inverse

makeCacheMatrix <- function(x = matrix()) {
    my_inverse <- NULL
    set <- function(y) {
      x <<- y
      my_inverse <<- NULL
    }
    Form <- function() x
    set_inverse <- function(inverse) my_inverse <<- inverse
    get_inverse <- function() my_inverse
    list(set = set,
         Form = Form,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## Creating the function to return the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Return a matrix that is the inverse of 'x'
  my_inverse <- x$get_inverse()
  if (!is.null(my_inverse)) {
      message("getting cached data")
      return(my_inverse)
  }
  mat <- x$Form()
  my_inverse <- solve(mat, ...)
  x$set_inverse(my_inverse)
  my_inverse
}
