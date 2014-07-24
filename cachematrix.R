## Write a cacheable Matrix Inverse
makeCacheMatrix <- function(x = matrix()) 
{
    ## Variable to store the inverse 
    inverse <- NULL
    
    ## Set the Original Matrix. 
    ##Default Inverse to NULL
    set <- function(y)
    {
        x <<- y
        inverse <<- NULL
    }
    
    ##Return Original Matrix
    get <- function()
    {
        x
    }
    
    ##Cache the Inverse of the Matrix
    setinverse <- function(inv)
    {
        inverse <<- inv
    }
    
    ##Return cached inverse of the Matrix
    getinverse <- function()
    {
        inverse
    }
    
    ##Return a list of getters and setters
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##Function to return inverse of the Matrix
cacheSolve <- function(x, ...) {
        ##Check if cached inverse exists
        i <- x$getinverse()
        if(!is.null(i))
        {
            ##If yes, return cached copy
            message("Getting cached Inverse")
            return(i)
        }
        
        ##If no, create inverse and cache it
        message("Cached inverse not found")
        m <- x$get()
        message("Calculating inverse")
        i <- solve(m, ...)
        message("Setting inverse into Cache")
        x$setinverse(i)
        i
}
