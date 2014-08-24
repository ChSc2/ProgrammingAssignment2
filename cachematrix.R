# Function will store a passed matrix as well as the inverse after
# it has been calculated
makeCacheMatrix <- function(x = matrix()) {
  
  # Initialize the object for the inverted matrix
  # outside for scoping
  InvMatrix <- NULL
  
  # Sets the initial value and nulls the inverted matrix
  # in case we have something stored that we want to override
  set <- function(y) {
    x <<- y
    InvMatrix <<- NULL
  }
  
  # Returns the original matrix
  get <- function() x
  
  #Sets and gets the inverse
  setinverse  <- function(inverse) InvMatrix <<- inverse 
  getinverse  <- function() InvMatrix
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# Function will check within a passed cacheMatrix if the inverse has
# been set in the object. Will print it out if so, otherwise calculate
# and set it within the cacheMatrix
cacheSolve <- function(x, ...) {

  # Get the value from the object
  InvMatrix <- x$getinverse ()

  # Has it already been calculated and set within the cacheMatrix
  if(!is.null(InvMatrix)) {
    message("Getting cached matrix")
  }
  else
  {
    # Get the original object
    data <- x$get()
    # Calculate the inverse
    InvMatrix <- solve(data, ...)
    # Store inverse in cacheMatrix
    x$setinverse(InvMatrix)    
  }
  
  # Print
  InvMatrix
}