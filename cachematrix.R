## The two functions provide the user with the ability to calculated the inverse
## of a matrix and store it. If the program has been run for a specific matrix 
## then it retrives the inverse from cached memory instead of running the
## calculation again

## The first function, makeVector creates a matrix
#gets the value of the matrix
#sets the value of the inverse of the matrix
#gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y)
    {
      x <<- y
      m <<- NULL
    }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Calculates the inverse of the matrix
## cacheSolve calculates the inverse of matrix created with makeCacheMatrix
## It first checks to see if the inverse has already been calculated
## If so, it retrieves the inverse from the cache and skips the computation
## Otherwise, it calculates the inverse of the data and stores it

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m))
    {
      message("getting cached data")
      return(m)
    }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}


##To execute
## First make a invertable matrix
## Then make an object and assign it the output of makeCacheMatrix
## pass the object to cacheSolve function