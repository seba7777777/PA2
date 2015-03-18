## Peer Assessments
## Programming Assignment 2: Lexical Scoping
## Sebastian Gierz

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  #x is a matrix to invert(matrix supplied must be invertible)
  x_inv <- NULL
  set <- function(y){
    x<<-y
    z_inv<<-NULL
  }
  get <- function() x
  setinv<- function(solve) z_inv<<-solve
  getinv<- function() z_inv
  list (set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x_inv <- x$getinv()
  if(!is.null(x_inv)) {
    message("Getting cached data")
    return(x_inv)
  }
  data<-x$get()
  message("Getting real data")
  x_inv<- solve(data)
  x$setinv(x_inv)
  x_inv
}
