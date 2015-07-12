## The first function, makeCacheMatrix, creates a special list 
## containing four functions for seting/getting the original/inversed 
## matrixes.  

makeCacheMatrix <- function(x = matrix()) {
        #set the inverse to NULL as a placeholder for a future value.
        s <- NULL
        #define a function to set the matrix, x, to a new matrix, y 
        #and resets the inverse, s, to NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        #define a function to get the original matrix
        get <- function() x
        #define a function to set the value of 
        #the inverse of the matrix
        setinverse <- function(solve)s <<- solve
        #define a function to get the value of 
        #the inverse of the matrix
        getinverse <- function() s
        list(set = set, get = get, 
             setinverse = setinverse, getinverse = getinverse)
}

## The second function actually calculate the inverse of 
## the original matrix as you make use of the first function.

cacheSolve <- function(x, ...) {
        #define s as the inverse of the matrix in getinverse.
        s <- x$getinverse()
        #if s is not NULL, get the inverse from the cache and skip the computation
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        #define data as the original matrix
        data <- x$get()
        #calculate the inverse of the original matrix
        s <- solve(data, ...)
        #set the value of the inversed matrix
        x$setinverse(s)
        #return the inversed matrix
        s
}

## For example, you can obtain the inverse of 
## the matrix(c(1,0,0,2),2,2 by inserting the following.
## cacheSolve(makeCacheMatrix(matrix(c(1,0,0,2),2,2)))