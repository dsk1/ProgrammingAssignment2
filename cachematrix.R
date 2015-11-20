## Create a vector containing functions to 
## set, get matrix and set and get the inverse of a matrix
## cache or store the input matrix and its inverse in the function's memory for each run
## Should be able to cache "makeCacheMatrix"ised matrix on each run

## a function for making a vector cacheable (in this case a Matrix vector)
## It will have two vector matrices in the function's memory. 
##      One for input matrix and second for the inverted matrix
## It will have the following 4 functions:
##      set function to assign the matrix everytime a new matrix is set
##      get function to just get the matrix
##      setinverse function is also to assign. But we use this to store the inverse of matrix
##      getinverse is to get the inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function is to 
## 1. return inverse of a "makeCacheMatrix"ised matrix
## 2. check if whether a value already exists in the inv variable of the function memory
## if exists: print a message and return the stored value
## if doesn't exist: computer inverse of the input matrix and return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
        
}

# Commit and Push