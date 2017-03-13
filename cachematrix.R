## Put comments here that give an overall description of what your
## functions do

## Function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { ## define the argument with default mode of "matrix"
              
        inv <- NULL                         ## initialize inv as NULL
        set <- function(y) {                ## define the set functuion to assign new value of matrix
                         
                x <<- y
                inv <<- NULL                ## if there is a new matrix, reset inv to null
        }
        get <- function() x                 ## define the get function 
        setinv <- function(inverse) inv <<- inverse ## assigns value of inv in parent environment
        getinv <- function() inv                    ## gets value of inv where called
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inv)
        }
        
        # otherwise, calculates the inverse 
        mat.data <- x$get()
        inv <- solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inv)
        
        return(inv)
}
