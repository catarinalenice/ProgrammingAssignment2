## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inverse_matrix <- NULL  #initializes the "inverse_matrix" (within the makeCacheMatrix environment)
  
  set <- function(y) {  # the set function inizialize "x" and "inverse_matrix" in the parent environment
    x <<- y
    inverse_matrix <<- NULL
  }
  
  get <- function() x     #the get function stores the value of x (the matrix)
  
  setinverse <- function(solve) inverse_matrix <<- solve  #the setinverse function 
  
  getinverse <- function() inverse_matrix     #the getinverse functoin returns the value of the inverse_matrix
  
  list(set = set, get = get,      ##here each function is assigned as an element of a list that is returned
       setinverse = setinverse,   ##to the parent environment 
       getinverse = getinverse)   ##Naming the list elements is what allows us to use the $ form of the extract operator to access the functions by name
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then it retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  inverse_matrix <- x$getinverse() #the value returned by the getinverse function 
                                   #(stated in the makeCacheMatrix) is stored in the "inerse_matrix" variable
  
  if(!is.null(inverse_matrix)) {     #checks if the inverse matrix was already calculated
    
    message("getting cached data")   #if so, it print this message and returns the value of it
    
    return(inverse_matrix) 
  }
  
  data <- x$get()   #if the if statement isn't true, the matrix x is stored in the variable called "data"
  
  inverse_matrix <- solve(data, ...) # the inverse matrix of "data" is calculated with the function 
                                     # "solve" and stored in the variable "inverse_matrix"
  
  x$setinverse(inverse_matrix)   #
} 
