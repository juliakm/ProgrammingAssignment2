## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    matrix <- NULL
    set <- function(y) {
        x <<- y ## search through parent environment first for a value for y and assign to x
        inverse <<- NULL ## set the inverse to null, unless it already exists
    }
    get <- function() x
    setinverse <- function(inverse) x <<- inverse #set the inverse value
    getinverse <- function() x #get the inverse value
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    }
}

## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. 
## Computing the inverse of a square matrix can be done with the `solve`
## function in R.
## n is the inverted matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    n <- x$getinverse()
    if(!is.null(n)) {
        message("getting cached data")
        return(n) #return the cached inverted matrix if it already exists
    }
    data <- x$get() #if we don't have a in existing inverted matrix, create one
    n <- solve(data, ...)
    x$setinverse(n)
    n
}