## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##
## makeCacheMatrix - Cache for a matrix and its inverse matrix. 
##   The cache is a list of 4 functions:
##      1. set - Set the matrix to cache 
##      2. get - Returns the cached matrix or NULL if is not initialized
##      3. setInverseMatrix - Sets the inverse matrix of the stored matrix.
##      4. getInverseMatrix - Returns the inverse matrix of the matrix or NULL
##                            if not initialized
##
## Notes:
## 1. No input correctness are performed (assuming matrix is reversible and that
##      the given reverse matrix is indeed its reversed matrix)
## 2. The Cache basically caches 2 matrices, one is called matrix and the other 
##      is called reverseMatrix
## 3. The cachd matricess are used in a separate and different environment using 
##      the operator <<-
##
makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(y) {
      inverseMatrix <<- NULL
      x <<- y
    }
    get <- function() x
    setInverseMatrix <- function(invMatrix) {
        inverseMatrix <<- invMatrix
    }
    getInverseMatrix <- function() inverseMatrix
    list(set = set, get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function

##
## cacheSolve (aMatrix, ...) - Returns the iverse matrix of aMatrix. 
##    Parameters: aMatrix - a cachedMatrix list.
##   
##   The iverse will be calculated on the first call to this 
##   function and will be pulled and returned from the cache for any subsequent 
##   call of the function.
##   The function assumes that the input matrix is reversible and does 
##   not perform any correctness verifications.
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invM <- x$getInverseMatrix()
    if(!is.null(invM)) {
        message("INFO: Using the cached value")
    } else {
        mat <- x$get()
        invM <- solve(mat, ...)
        x$setInverseMatrix(invM)
    }
    
    invM
}
