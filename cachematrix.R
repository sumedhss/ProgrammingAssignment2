## Programming Assignment 2: Cache the inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse
## Uses prompt and format from Coursera assignment instructions

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL            ## 'm' - special matrix
  set <- function(y) {
    
    x <<- y            ##Sets value
    inv <<- NULL       ##Clears cache
  }
  get <- function() x
  
  setInverse <- function(inverse) m <<- inverse
  ##Defines function to set the inverse if no cache
  
  getInverse <- function() m
  ##as above but gets the inverse
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse) ##Returns a list with the stated fuctions
}

## The function below computes the inverse of the special "matrix" created by
## 'makeCacheMatrix' If the inverse from an unchanged matrix has been calculated
## then it should return the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInverse()      ##Retrieves the cached value
  if (!is.null(m)) {       ##Returns cache if it is not empty
    
    message("getting cached data")
    return(m)
  }
  
  ##If cache is empty
 
  dat <- x$get()             ##Get the matrix's value
  m <- solve(dat, ...)       ##Calculate that inverse
  x$setInverse(m)            ##Cache it
  m                          ##Return it
}
