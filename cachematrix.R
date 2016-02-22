# Assignment 2: Chaching the inverse of a matrix
# Ilane Hernandez Morales

####
# function makeCacheMatrix is a list of functions to:
# 1) set the value of a special matrix
# 2) get the value of the matrix
# 3) set the inverted matrix
# 4) get the values of the inverted matrix

makeCachematrix <- function(x = matrix()) {  #creates a function matrix
      mx <- NULL                             #creates an empty space to fill in the calculated inverted matrix
      set <- function(y) {                   #set the value of the vector
            x <<- y
            mx <<- NULL
      }
      get <- function() x                    #get the value of the vector
      setinvert <- function(inv.matrix)      #set the inverted matrix values
            mx <<- inv.matrix   
      getinvert <- function() mx             #get the values of inverted matrix
      list(set = set, get = get,             #list of the values of the function
           setinvert = setinvert,
           getinvert = getinvert)
}

## 
# Function cacheSolve calculates the inverse of the special "matrix" created with 
# the function makeCachematatrix, "x". cacheSolve function will first check if the inverse of
# the given matrix has been aready calculated, and it will retrive the inverse of 'x'
# if existing. 
# 

cacheSolve <- function(x, y,...) {
      change <- NULL
      if (!identical(x$get(), y)) {             # Test if the matrix has changed
            x$set(y)                            # If TRUE Sets the new matrix in x
            change <- TRUE
            message("Matrix has changed, replacing cache data")
      } else {
            change <- FALSE
      }
      mx <- x$getinvert()          #calls the 'getinvert' function associated with 'x'
      if(!is.null(mx)) {           #checks if the 'matrix' is the same as, if it is the same, it continious to...
            message("getting cached data") 
            return(mx)}
      data <- x$get()             #calls the original matrix
      mx <- solve(data, ...)      #computes the inverse of matrix in "x"
      x$setinvert(mx)             #set the cache of a new inverted data
      mx                          #Prints the inverted matrix
}


#####
# Example
#
# Invertible matrices
invertible <- matrix(c(3,1,2,1,8,2,4,1,2,4,5,7,34,3,5,8), nrow = 4, ncol = 4)
invertible2 <- matrix(c(24, 5, -4, -12, 3, 2, -2, -5, 4), nrow = 3, ncol = 3)

x = makeCachematrix(invertible)
cacheSolve(x, invertible)           # Scenario 1, cache empty
cacheSolve(x, invertible)           # Scenario 2, inverted matrix in cache AND matrix has not changed
cacheSolve(x, invertible2)          # Scenario 3, inverted matrix in cache BUT matrix has changed
cacheSolve(x, invertible2)         # Again scenario 2
