## PROGRAMMING ASSIGNMENT 2
## Lexical Scoping // Caching the Inverse of a Matrix

## USAGE EXAMPLE:
## > source("cachematrix.R")
## > amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
## > cacheSolve(amatrix)
## ... will calculate, store (cache), and then return the matrix inverse

## UNIT TESTS:
## successfully solves for unit tests listed on:
## https://class.coursera.org/rprog-007/forum/thread?thread_id=83

## FUNCTIONS OVERVIEW:

## makeCacheMatrix():
## modified from the example `makeVector` code. this function
## returns a list of functions to set/get an input matrix,
## and set/get the computed inverse (caching by assigning with the 
## <<- operator, which assigns the value (here, the cached inverse 
## of the matrix) to a variable in a different environment.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x 
    setinverse <- function(inv) m <<- inv
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve():
## Also quite close to the example `cachemean` code, this function
## "calculates" the inverse of the special matrix we create, initially,
## by calling makeCacheMatrix() above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
