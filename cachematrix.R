## Making use of the example "makeVetor" and "cachemean" from the assignment page and altering it to calculate the inverse of a Matrix instead of the mean of a vector.

makeCacheMatrix <- function(x = matrix()) {
  ## Same as the "makeVector" with the x = numeric() replaced with x = matrix() so that our input for the function is now a matrix and not a vector 
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInvM <- function(inverse) m <<- inverse
  ## Changed mean to inverse
  getInvM <- function() m
  list(set = set, get = get,
       ## Change sub names from mean to (Inv)erse of (M)atrix
       setInvM = setInvM,
       getInvM = getInvM)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Same as the "cachemean" function
  ## Call new naming from "makeCacheMatrix"
  m <- x$getInvM()
  if ( ! is.null(m)) {
    print("getting cached data")
    return(m)
  }
  data <- x$get()
  ## Swap the mean standard function with the solve standard function as per the instructions in assignment for computing the inverse of a square matrix
  m <- solve(data)
  x$setInvM(m)
  m
}