
##  The following functions Compute the inverse of a square by ;Firstly,creating a special 'matrix'object and cache's its inverse.
## Secondly,Rather than recalculting the matric inverse repeatdly, the inverse is looked up from the cache if it is already calculated.  

## This function 'makeCacheMatrix'creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL

  ##'set' sets the value of the vector
  set <- function(y) {
      x <<- y
    i <<- NULL
  
  }
  
  ##'get' gets the value of the vector
  get <- function() x

  ##'setinverse' set the value of the matrix inverse
  setinverse <- function(inverse) i <<- inverse

  ##'getinverse' gets the value of the matrix inverse
  getinverse <- function() i
  
  ##Special "matrix" object that can cache its inverse created
  list(set = set, get = get,
       setinverse = setinverse,getinverse = getinverse)


}


##  'cacheSolve' function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed),
##  then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()

  ##If inverse matrix already exsists (previously calculated) Obtain from cache ,dont recommute!
  if(!is.null(i)) {
      message("Getting cached matrix Inverse Data...")
    return(i)
  }
  data <- x$get()
  
  ##  No Inverse Data calculated earlier-> Calculate inverse of matrix 
  
  #Solve Function used to calculate Inverse
  i <- solve(data, ...)
  
  #Set calculated Inverse  in Cache
  x$setinverse(i)
  i

}
