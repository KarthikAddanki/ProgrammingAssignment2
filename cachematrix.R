## The objective of this code is to 
## (a) create a custom 'cache' of matrices mapped to their inverse matrices
## (b) refer to the cache whenever possible to fetch already calculated values
## of inverse matrices, thereby, reducing the processing time for complex
## inverse matrix calculations

## The makCacheMatrix function is used to create a complex object containing 
## variables and functions to instantiate the object, initialize the variables 
## and fetch them when the object is referred.
## The variables contained in the object are 
## (a) a matrix 'x' (hereby dubbed as inputmatrix)
## (b) and its inverse contained in the variable 'inv_matrix'

makeCacheMatrix <- function(x = matrix()) {
      ## initialize a variable to be contained in all objects
      ## inv_matrix will contain the value of the inverse of matrix x
      inv_matrix <- NULL
      
      ## a "set" function to instantiate an object and provide default values
      ## to variables
      set <- function(inputmatrix)
      {
            x <<- inputmatrix
            inv_matrix <<- NULL
      }
      
      ## a "get" function to fetch or display the value of the inputmatrix 
      ## retained in this object
      get <- function()
      {
            x
      }
      
      ## a "set" function to provide a value to the inv_matrix variable in the 
      ## referred object
      setinverse <- function(inversematrix)
      {
            inv_matrix <<- inversematrix
      }
      
      ## a "get" function to fetch or display the inv_matrix variable in the 
      ## referred object
      getinverse <- function()
      {
            inv_matrix
      }
      list(set = set, get = get, 
           setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function checks if the inverse of a given complext object
## (created by makeCacheMatrix) already exists. If yes, it fetched the inverse
## matrix from a cache of objects. Otherwise, it calculates the inverse of the
## matrix provided and initializes the inv_matrix parameter of the object
## with the calcualted inverse matrix.

cacheSolve <- function(x, ...) {
      ## check if the argument provided is a list
      if(class(x) != "list")
      {
            message("Invalid argument. This function can only take objects created from the makeCacheMatrix function.")
            return()
      }
      ## if the argument is a list check if the values "setinverse" and "getinverse"
      ## are stored in it. This is to make sure that we have the right object
      ## to work with.
      if(!("setinverse"%in%names(x) && "getinverse"%in%names(x)))
      {
            message("Invalid argument. The list provided does not appear to be generated from the makeCacheMatrix function")
            return()
      } 
      
      ## if the argument appears to be fine
      ## check if inverse of the inputmatrix contained in 'x' exists in cache
        inv_matrix <- x$getinverse()
        if(!is.null(inv_matrix))
        {## if yes, print the matrix with a message
              message("Fetching data from Cache...")
              
              ## end the function call by returning the value of the variable
              return(inv_matrix)
        }
        ## if not, 
        ## fetch inputmatrix contained in x
        y <- x$get()
        
        ##calculate the inverse of inputmatrix
        inv_matrix <- solve(y)
        
        ## set value of inv_matrix contained in x with the newly calculated
        ## inverse matrix
        x$setinverse(inv_matrix)
        
        ## print inverse matrix
        inv_matrix
}
