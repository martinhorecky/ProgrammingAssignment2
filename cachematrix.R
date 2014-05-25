## Following functions can be used to cache results of inverse function for a matrix
## solve() can be an expensive operation if repeated frequently and these functions
## allow for caching of the result of inverse operation

## Following shows example of use of the two functions
#amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
#amatrix$get()         # Returns original matrix
#cacheSolve(amatrix)   # Computes, caches, and returns matrix inverse
#amatrix$getinverse()  # Returns matrix inverse
#cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse


## makeCacheMatrix creates a vector of functions used to get/set the initial matrix
## and get/set the calcualted inversion of the initial matrix
##
## To initialize call - makeCacheMatrix(m); where m is an inversible matrix
##
## Functions within the vector
## get - returnes orignial matric
## set - set original matrix (and cleans cached value of inverted matrix)
## getinverse - retrieves cached value of inverted matrix
## setinverse - sets value of inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL # clean value of inverted matrix on initialization
  set <- function(y) { 
    x <<- y     # sets value of orignal matrix
    i <<- NULL  # clean inverted matrix as it is no longer valid after original matrix changed
  }
  get <- function() x # get value of orignal matrix
  setinverse <- function(solve) i <<- solve # invert matrix
  getinverse <- function() i # get value of inverted matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns inverted matrix of the original matrix
## It caches value of inverted function
## If called repeatedly, it checks if cached value is avaiable
## if yes, it returns cached result
## if no, it inverts original matrix and caches result

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse() # retrieves cached inverted matrix
  if(!is.null(i)) { # if available in cache, return inverted matrix
    message("getting cached data")
    return(i)
  }
  # if not cached, calculate and return
  data <- x$get() # get orignal matrix
  i <- solve(data, ...) # invert it
  x$setinverse(i) # cache inverted matrix
  i # return inverted matrix
}
