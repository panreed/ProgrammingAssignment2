## This file defines two functions that take the input of a square invertible 
## matrix, and output its inverse matrix by either computing it directly or 
## retrieve the value stored in the cache, if the inverse has previously been
## calculated already.

## makeCacheMatrix function takes the input of a numeric matrix, and stores its 
## value, as well as a cached value for its inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
## This function is a list of four individual functions, that set/get the value of
## the matrix/inverse matrix.
    qq <- NULL
    ## First initialize the variable that stores the inverse matrix
    setMatrix <- function(y) {
        x <<- y
        qq <<- NULL
    }
    ## set the value of the matrix, and initialize the value of the inverse matrix
    getMatrix <- function() x
    ## retrieve the value of the matrix
    setInverse <- function(inverseM) qq <<- inverseM
    ## set the value of the Inverse matrix
    getInverse <- function() qq
    ## retrieve the value of the inverse matrix
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)

}


## cacheSolve checks whether the matrix stored in the cache is indeed the inverse
## of the input matrix. If it is, then the cached value will be returned as the 
## final output; otherwise a new inverse matrix will be calculated, stored in the 
## cache and returedn as the output. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    qq <- x$getInverse()
    if(!is.null(qq)) {
        message("getting cached inverse matrix")
        return(qq)
    }
    ## If the inverse matrix has already been calculated and stored, then no 
    ## calculation will be performed, cached value will be returned as output.
    
    data <- x$getMatrix()
    matrixD <- dim(data)[1]
    ## If the inverse matrix has not been calculated, then the matrix will be
    ## retrieved, and its dimension obtained here.
    
    qq <- solve(data, diag(matrixD),...) 
    ## The matrix dimension is used to build an Identity matrix with the right
    ## dimension, to solve for the inverse matrix with the equation
    ## matrix %*% inverseMatrix == Identity Matrix
    
    x$setInverse(qq)
    qq
    ## After the inverse matrix has been solved, its value is stored in the cache,
    ## as well as printed out as the output.
}

## One could check the final result by evaluating this quantity:
## sum(x$getMatrix() %*% x$getInverse() - diag(MatrixD)) == 0
## If the inverse matrix was calculated correctly, then the product calculated 
## above would return an identity matrix, as a result, the sum function would 
## return zero. So the whole expression would return TRUE if the calculation was
## correct, and FAULSE otherwise.
