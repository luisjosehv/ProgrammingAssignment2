


makeCacheMatrix <- function (x = matrix () ) {  
  inv <- NULL                                  # Define function to set the value of the matrix                                
  set<- function (y) {                        
    x <<- y                                    # Set values
    inv <<-NULL                                # Clear Cache
  }
  
  get <- function () x                         # Define function to get  value of the matrix            
  setinv <- function (inverse) inv <<-inverse  # Define function to get the inverse
  getinv <- function() inv                     # Return a list with the above functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()                 #fetches the cached value for the inverse
  if(!is.null(inv)) {                   # when the cache is not null, just return it
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()                       # Get matrix values
  inv <- solve(data)                    # Calculate inverse
  x$setinverse(inv)                     
  inv
}
