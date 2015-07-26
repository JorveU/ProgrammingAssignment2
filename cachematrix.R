##mackCachematrix
##cache matrix saves the the matrix that is given into memory and invers
##it this will allow the matric to be compared to new matrixes
##ma will be the given matrix
##this should be called once unless there is a new matrix
makeCacheMatrix <- function(ma = matrix())
{
  ##x set to null but will cache the previous matrix
  ##and will hold the inverse
  x <- NULL
  set <- function(y)
  {
    ma <<- y
    x <<- NULL
  }
  get <- function() ma
  setinverse <- function(solve) x <<- solve
  getinverse <- function() x
  ##this will be returned and used by cache solve
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}

##cacheSolve
##uses the output from makeache to cache the matrix
##returns the inverse of the orginal matrix
cacheSolve <- function(ma, ...)
{
  x <- ma$getinverse()
  ##check to see if the matrix has been calculated
  if(!is.null(x))
  {
    ##send the chached matrix back
    message("Retrieving cached data")
    return(x)
  }
  ##else calculate the inverse
  matrix.data <- ma$get()
  x <- solve(matrix.data, ...)
  
  ma$setinverse(x)
  
  return(x)
}