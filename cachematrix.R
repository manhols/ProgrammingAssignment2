## Put comments here that give an overall description of what your
## functions do

  ##makeCacheMatrix is a list of functions: 1. set a matrix. 2. get matrix
  ## 3. set inverse. 4. get the inverse. The list is used as input for cacheSolve 
  ##It's almost the same as the example given, just changing mean for inv since
  ##we work with the invert instead of the mean
  
makeCacheMatrix <- function(x = matrix()) {    

      inv = NULL
      set = function(y) {
        x <<- y
        inv <<- NULL
      }
      get = function() x
      setinv = function(inverse) inv <<- inverse 
      getinv = function() inv
      list(set=set, get=get, setinv=setinv, getinv=getinv)
}
  ## The next function computes the inverse of the matrix returned by makeCacheMatrix above. 
  ## If the inverse has already been calculated (and the matrix has not changed), 
  ## then the cachesolve should retrieve the inverse from the cache
  ## return: inverse of the original matrix input to makeCacheMatrix()
  ## Similar to cachemean example, again just using invert instead of mean
  

cacheSolve <- function(x, ...) {

  inv = x$getinv()
  
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  x$setinv(inv)
  ## Return a matrix that is the inverse of 'x'
  return(inv)
}
