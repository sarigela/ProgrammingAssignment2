
#####################################################################################
# This file contains 2 functions makeCacheMatrix and cacheSolve.  
#
# These functions are # used to perform a inverse of a matrix and cache the computed 
# inverse value. 
# Any further request of Inverse will fetch the results from cache rather than 
# re-computing unless the values of the matrix are changed.
#
#####################################################################################



#####################################################################################
# Name: makeCacheMatrix
# Description: This function creates a special matrix object that can cahce its inverse.
#               It contains multiple functions in it.
#               1) setMatrix:   Setting the values of matrix
#               2) getMatrix:   Gets the values from the matrix.
#               3) setInverse:  Sets the inverse of the current matrix.
#               4) getInverse:  Gets the cahced inverse value if exits.
# Usage:
# makeCacheMatrix(x)
#
# Arguments:
# x     Matrix object of R and expected to be a square matrix that can be inverted.
#       Sample: matrix(c(0, 3, -1, 5), nrow = 2, ncol = 2)
####################################################################################

makeCacheMatrix <- function (myMatrix = matrix()) 
{
  ## Local variable to store the inverse
  inverse <- NULL
  
  ## setMatrix:  To the set the value of matrix 
  setMatrix <- function(inputMatrix)
  {
    myMatrix <<- inputMatrix
  }
  
  ## getMatrix: To get the value of Matix
  getMatrix <- function() myMatrix
  
  ## setInverse:  Sets the Inverse of the current myMatrix object
  setInverse <- function(solvedMatrix) inverse <<- solvedMatrix
  
  ## GetInverse:  Gets the inverse of the matrix if already cached.
  getInverse <- function() inverse
  
  ## lists the list of memeber funtion with 
  list(setMatrix=setMatrix, 
       getMatrix = getMatrix, 
       setInverse = setInverse, 
       getInverse = getInverse)
}



####################################################################################
# Name: cacheSolve
# Description: This function recieves makeCacheMatrix object, computes the inverse of 
#               the matrix and caches it. Also returns the inverse.
# Usage:
# cacheSolve(a, ...)
#
# Arguments:
# a     a is a object of type makeCacheMatrix.
# ...   further arguments passed to or from other methods.
####################################################################################

cacheSolve <- function(x, ...)
{
  ## Fetch the inverse of the input object x of type makeCacheMatrix.
  inverse <- x$getInverse()
  
  ## Verify inverse value to determine to compute Invers or use the cache.
  if (!is.null(inverse))
  {
    ## Inverse already exists within object X.  Hence, no need to compute again.
    
    ### Print the message that we are fetching the value from cache. 
    message("Getting the Inverse of Matrix from within cache.")
    
    ### return the cached inverse value.
    return (inverse)
  }
  
  ## Inverse is not already computed and cached. Hence, perform that action now.
  message("Computing the Inverse of the matrix.")
  
  ## Fetch the matrix.
  myMatrix <- x$getMatrix()
  
  ## Compute inverse of the matrix
  inverse <- solve(myMatrix, ...)
  
  ## Cache the computed inverse of the matrix
  x$setInverse(inverse)
  
  ## return the newly computed inverse
  inverse
}
