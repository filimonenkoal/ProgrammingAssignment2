# creating a special "matrix" object 
makeCacheMatrix <- function(mtx = matrix()) {
  inversed <- NULL
  set <- function(x) {
    mtx <<- x;
    inversed <<- NULL;
  }
  get <- function() return(mtx);
  setinv <- function(inv) inversed <<- inv;
  getinv <- function() return(inversed);
  return(list(set = set, 
              get = get, 
              setinv = setinv, 
              getinv = getinv))
}


#computing the inverse of the special "matrix"
cacheSolve <- function(mtx, ...) {
  inversed <- mtx$getinv()
# check if cached data exists  
  if(!is.null(inversed)) {
    message("getting cached data")
    return(inversed)
  }
  data <- mtx$get()
  inversed <- solve(data, ...)
  mtx$setinv(inversed)
  return(inversed)
}