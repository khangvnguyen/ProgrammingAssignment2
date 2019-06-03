## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix initializes a matrix and allows us to get and set
## the values and the inverse of the matrix. The function cacheSolve allows us to
## solve for the inverse of the matrix, and print the cached value when it has
## already been solved. These two functions need to work together.

## Write a short comment describing this function

# Initialize the function with an empty matrix and assign NULL to inverse
makeCacheMatrix <- function(x = matrix()) {
        i = NULL
        # Reset the matrix when needed
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        
        #Get the value of the matrix
        get <- function() x
        
        #Set and get the value of the inverse of the matrix (but not compute it)
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        
        #Arrange everything into a list with names, to allow for usage of the $ operator
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Get the inverse from makeCacheMatrix.
        i <- x$getinverse()
        
        ## If the inverse is empty, it means nothing is cached. If it is not,
        ## use this cached value and don't have to re-compute the inverse
        if (!is.null(i)){
                message("Getting cached data")
                return(i)
        }
        ## If no cached data is found, get the value of the matrix.
        data <- x$get()
        ## Return a matrix that is the inverse of 'x', then set the inverse
        ## in makeCacheMatrix to be the value. Return the inverse.
        i <- solve(data,...)
        x$setinverse(i)
        i
}
