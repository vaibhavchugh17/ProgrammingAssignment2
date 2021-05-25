#These functions are used in caching the inverse of a matrix
#to ensure efficient cost and resource consumption


#This function creates a list that contains special objects so as to cache the inverse of a matrix x

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(val) inv <<- val
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)

}


#This function computes the inverse of the matrix x if it's not cached otherwise
#it returns the cached value of the inverse

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  newInv <- solve(data)
  x$setInv(newInv)
  newInv
}
