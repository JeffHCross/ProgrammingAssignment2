## 2/5/2017
## cacheMatrix.R
## -- makeCacheMatrix: Function to create a special type of matrix that can have
##                     a cached inverse
## -- cacheSolve:      Function to access the cached inverse, if available

## Function to create a special type of matrix that can have a cached inverse
makeCacheMatrix <- function(x = matrix()) {
    ## Verify a matrix was passed
    if(!is.matrix(x)){
        message("The passed object is of type: ",typeof(x))
        stop('The passed object was not a matrix. Try again.')
    }
    
    ## Initialize variables
    inv <- NULL
    
    ## Sub-function creation
    set <- function (y) {
        ## Verify a matrix was passed
        if(!is.matrix(y)){
            message("The passed object is of type: ",typeof(y))
            stop('The passed object was not a matrix. Try again.')
        }
        
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function (solve) inv <<- solve
    getinverse <- function() inv
    
    ## The list that will be returned on creation. Effectively the constructor.
    list (set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## Function to access the cached inverse, if available, otherwise create it.
cacheSolve <- function(x, ...) {
    ## Check for any current inverse available
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("Returning cached matrix")
        return(inv)
    }
    
    ## Get the current data to generate a cached matrix
    data <- x$get()
    
    ## Non-square matrices cannot be inverted
    if(nrow(data) == ncol(data)){
        inv <- solve(data,...)    
    }
    else{
        inv<-NULL
        stop("Matrix is non-square, cannot be inveretd.")
    }
    x$setinverse(inv)
    
    ## Return a matrix that is the inverse of 'x'
    inv
}
