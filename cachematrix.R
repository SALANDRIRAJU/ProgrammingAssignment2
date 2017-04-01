## Week 3 Assignment of Coursera R programming course
## of Github user S Raju

##    makeCacheMatrix function takes a square matrix as an argument 
##    (will give an error if the input matrix is not a square matrix) and
##    returns a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##      and this list is used as the input to cacheSolve()


makeCacheMatrix <- function(x = matrix()) {
  
  # assign NULL value to inv, inv will hold the matrix inverse
  inv = NULL
  
  # set function will assign new value of matrix in parent environment
  set = function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    x <<- y
    
    # if a new matrix is assigned, then inv is reset to NULL
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  # gets the value of inv where called
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
}


## cacheSolve function returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  
  
  inv = x$getinv()
  
  # if the inverse was previously calculated then the 'if' condition will
  # get it from the cache and does not calculate the inverse again
  if (!is.null(inv)){
    
    message("getting cached data")
    return(inv)
  }
  
  # else, calculates the inverse 
  data = x$get()
  inv = solve(data, ...)
  
  # assigns the value of the inverse to 'inv' in the cache 
  # using the setinv function.
  x$setinv(inv)
  
  return(inv)
  
}
