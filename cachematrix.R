# This R script file can be used to calculate the inverse of square invertable maxtrices.
# It contains a helper function that will cache an inverted matrix. 

# makeCacheMatrix caches the matrix and its inverse intended to be used to cache large
# matrix's that are computational expensive to calculate... oh and cure cancer.

testFunction <- function() {
  # First we test that using 
  a1 <- c(3, 2, 5) 
  a2 <- c(2, 3, 2) 
  a3 <- c(5, 2, 4) 
  A <- rbind(a1, a2, a3)
  
  specialMatrix <- makeCacheMatrix(A)
  message("Use the Cache to give you the inverted matrix")
  print(cacheSolve(specialMatrix))

  message("Use the Cache to give you the inverted matrix, this time from cache")
  print(cacheSolve(specialMatrix))
  
  message("Printout the Inverted matrix, verify its been set")
  print(specialMatrix$getInverted())
  
  message("Changing Matrix, seeing if it clears the cache")
  b1 <- c(1, 4, 7) 
  b2 <- c(2, 5, 8) 
  b3 <- c(3, 6, 49) 
  B <- rbind(b1, b2, b3)
  
  specialMatrix <- makeCacheMatrix(B)
  specialMatrix$getInverted()
}




makeCacheMatrix <- function(x = matrix()) {
  this_matrix <- x
  inverted_matrix <- NULL
  
  # This function sets the new matrix. It also clears the cached inverted matrix
  # This is important because if we get a new matrix, we don't want to keep the old inverted one
  # In the future it might be nice to optionally calculate and set the the inverse when we set the 
  # matrix.
  set <- function(newMatrix) {
    this_matrix <- x
    inverted_matrix <- NULL
  }
  
  # Simply return the matrix 
  get <- function() this_matrix
  
  # Set the value of the inversion
  setInverted <- function(y) {inverted_matrix <<- y}
  
  # Get the inverted matrix
  getInverted <- function() inverted_matrix
  
  # Return a list of the functions.... sorta feels like object oriented programming
  list(
      set = set,
      get = get,
      setInverted = setInverted,
      getInverted = getInverted
    
  )
}


#cacheSolve This function will calculate the invers of a provided square matrix.
# it will make use of the makeCacheMatrix. If the inverse has already been calculated
# it will use the cached matrix (wheter what has been cached is correct or not). In 
# the event there is no cached matrix, the function will determine the inverse and then 
# cache the results. 
cacheSolve <- function(specialMatrixObject, ...) {
   # here we use x as temp var to see if the we have already cached the inverted matrix
   invertedMatrix <- specialMatrixObject$getInverted()
  
  # Do we have the inverted matrix cached, if so.. YAY return it. 
  if(!is.null(invertedMatrix)) {
    message("getting cached data")
    return(invertedMatrix)
  }
  
  # Seems like we still need to calculate the inverse, bummer
  # Get the matrix from the "special" object
  data <- specialMatrixObject$get()
  
  invertedMatrix <- solve(data, ...)
  
  specialMatrixObject$setInverted(invertedMatrix)
  
  invertedMatrix    
}