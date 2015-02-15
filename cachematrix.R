## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    matrix <- NULL
    set <- function(y) { #set is a function to set the value of inverse
        x <<- y ## search through parent environment first for a value for y and assign to x
        inverse <<- NULL ## set the inverse to null, unless it already exists
    }
    get <- function() x #get is a function to get the value of the matrix
    setinverse <- function(inverse) x <<- inverse #set the inverse value
    getinverse <- function() x #get the inverse value
    list(set = set, get = get, #create a list with the values of each function
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve calculates the inverse of the “matrix” returned by makeCacheMatrix()
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    n <- x$getinverse()
    if(!is.null(n)) {
        message("getting cached data")
        return(n) #return the cached inverted matrix if it already exists
    }
    data <- x$get() #if we don't have a in existing inverted matrix, create one
    n <- solve(data, ...) #use solve() to find the inverse
    x$setinverse(n)
    n #resturn the inverse
}