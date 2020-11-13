############################################################################### # 
## Peer-graded Assignment: Programming Assignment 2: Lexical Scoping ----
############################################################################### #

## the second assignmet of the R Programming course will teach us more about
## Lexical Scoping.

## "Scoping 
    ## is the mechanism within R that determines how R finds symbols 
    ## (i.e. programming language elements) to retrieve their values during the execution of an R script."
## Lexical scoping:
    ## "is used to retrieve values from objects based on the way functions are nested when they were written."
## Source: https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md


## This exercise is also a good introduction to S3 objects. 

## The first function, makeCacheMatrix, will store a list of 4 different functions:
## set, get, setmean, and getmean

makeCacheMatrix <- function(x = matrix()) {  ## makeCacheMatrix environment
    inv <- NULL ## Object inv set to null
    set <- function(y) {
        x <<- y 
        inv <<- NULL ## Object inv set to null
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse 
    getinv <- function() inv
    list(set = set, get = get,  ## "each element in the list is created with a elementName = value syntax"
         setinv = setinv,
         getinv = getinv)
}


## The cacheSolve function will cache the inverse of a matrix. 
## This function returns a matrix that is the inverse of another given matrix "x"..

## cacheSolve actually gives a value to the objects in the makeCacheMatrix.
## Then, if inv is null, it returns the inverse of the matrix.

cacheSolve <- function(x, ...) {  ## cacheSolve environment
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}


## Tests ----

## Source: https://www.coursera.org/learn/r-programming/discussions/weeks/3/threads/ePlO1eMdEeahzg7_4P4Vvg

m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
I2 <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
m1 %*% n1
n1 %*% m1
solve(m1)
solve(n1)
myMatrix_object <- makeCacheMatrix(m1)
cacheSolve(myMatrix_object)
cacheSolve(myMatrix_object)
n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
myMatrix_object$set(n2)
cacheSolve(myMatrix_object)
cacheSolve(myMatrix_object)

## Resurces ----
## https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md
## https://github.com/DanieleP/PA2-clarifying_instructions
