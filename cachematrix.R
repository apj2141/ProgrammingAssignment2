# Put comments here that give an overall description of what your
## functions do

# The functions manage the matrix and its inverse and the functions
# required for handling it within a listdefined by the makeCacheMatrix
# function. This environment can then be called by other functions to
# access the matric and its inverse without recalculation. To be called
# myMatrix<-makeCacheMatrix()

## Write a short comment describing this function

# The first function defines the required objects and function objects
# to store and manipulate the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) { # matrix object defined 
    i <- NULL # defines inverse oject
    set <- function(y) { #creates a function that can be called myMatrix$set()
        x <<- y # resets x in parent function
        i <<- NULL #resets inverse in parent funtion
        }
    get <- function() x #returns matrix from parent when called
    setinverse <- function(inv) i <<- inv #sets inverse in parent
    getinverse <- function() i #returns inverse from paretn when called
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

## Write a short comment describing this function

# Calculates inverse when it does not exists and sets it to myMatrix
# makeCacheMatrix enivronment
# Helpful to use det(matrix) to check if a matrix is singular first 
# ie returns 0 it is not invertable
# This matrix inverts: matrix(c(2,2,3,2),2,2)
# So does myMatrix$set(matrix(c(1,-1,1,2),2,2))

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
        }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}