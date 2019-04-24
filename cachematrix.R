
## functions found in this
##      1.[makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.]
##      2.[ cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##        If the inverse has already been calculated (and the matrix has not changed), 
##         then the cachesolve should retrieve the inverse from the cache.]


##---------Creating the makeCacheMatrix function--------------------
##first , the makeCacheMatrix function will be defined so that it's possible to create objects of type makeCacheMatrix, 
##objects of type makeCacheMatrix are basically matrices with added properties (added for our purpose)


## function declaration with the default value of z defined as an empty matrix.

makeCacheMatrix <- function(z = matrix()) {
  invs <- NULL                             
  
  set <- function(y) {                                    ## define the set function to assign new 
    z <<- y                                               ## value of matrix from parent environment
    invs <<- NULL                                         ## if there is a new matrix of type makeCacheMatrix then reset invs to NULL
  }
  get <- function() z                                     ## define the get function - returns value of the matrix argument
  
  setinverse <- function(inverse) invs <<- inverse        ## defining the function to assigns value of invs in parent environment
  getinverse <- function()invs                            ## defining the function to get the value of invs where called
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  ##a list containing the the functions is created and are given their respective names
  #this is for the purpose of convenience so that the functions may be called using the $ operator.
  

}


## This function will create an object of type makeCacheMatrix which is a matrix with special properties

cacheSolve <- function(z, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  invs <- z$getinverse()
  if(!is.null(invs)) {
    message("getting cached data")
    return(invs)
  }
  ## depending on the output of of !is.null(invs) , invs is returned.
  ## if is.null(invs) == FALSE (there is a value assigned to invs) then !is.null == TRUE. 
  ## Thus the cached value of invs is returned
  
  ## if is.null(invs) == TRUE (there is no value assigned to invs) then, !is.null == FALSE.
  ## Thus the conditional statement is avoided and a new value is assigned to invs
  data <- z$get()
  invs <- solve(data, ...)
  z$setinverse(invs)
  invs
        ## Return a matrix that is the inverse of 'x'
        ## this function will retain the value of the inverse in the cache to be used when necessary
}
