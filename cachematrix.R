## makeCacheMatrix creates matrix object
## cacheSolve returns a matrix that is the inverse of matrix sent

## Make a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## initialize the inverse matrix 
        i <- NULL
        
        ## set the matrix object to y
        set <- function(y) {
          x <<- y
          i <<- NULL
        }
        
        ## return the matrix object x
        get <- function() x
        
        ## set the inverse matrix
        setsolve <- function(inverse) i<<- inverse
        
        ## return the inverse matrix object
        getsolve <- function() i
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
             
}


## Return the inverse matrix of the matrix sent

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
        ## set 'i' to getinverse on 'x' from parent environment
        i <- x$getsolve()
        
        ## if 'i' is not null, return cached inverse matrix
        if(!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        
        ## Otherwise need to get inverse and put into cache 
        data <- x$get()
        i <- solve(data, ...)
        x$setsolve(i)
        i
  
}
