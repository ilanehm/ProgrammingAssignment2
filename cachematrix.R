## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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

cacheSolve <- function(x, ...) {
      change <- NULL
      if (!identical(x$get(), y)) {             # Test if the matrix has changed
            x$set(y)                            # If TRUE Sets the new matrix in x
            change <- TRUE
            message("Matrix has changed, replacing cache data")
      } else {
            change <- FALSE
      
      
      mx <- x$getinvert()          #calls the 'getinvert' function associated with 'x'
      if(!is.null(mx)) {           #checks if the 'matrix' is the same as, if it is the same, it continious to...

            message("getting cached data") 
            return(mx)
      }
      data <- x$get()             #calls the original matrix
      mx <- solve(data, ...)      #computes the inverse of matrix in "x"
      x$setinvert(mx)             #set the cache of a new inverted data
      mx                          #Prints the inverted matrix
}
