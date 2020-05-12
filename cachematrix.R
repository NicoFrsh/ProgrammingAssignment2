## In the following, we'll create two functions, one to 
## create a special matrix object and another to 
## calculate its inverse and puts it in the cache.



## The first function 'makeCacheMatrix' creates a
## special matrix object, that consist of a list
## that contains functions to get and set the
## matrix itself as well as its inverse, respectively.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        
        list(set = set, get = get, setInv = setInv,
             getInv = getInv)
}


## The second function 'cacheSolve' takes a matrix
## object, that is created by the 'makeCacheMatrix'
## function, checks if there is already an inverse
## available in the cache and if not computes the
## inverse using the 'solve' function.

cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        
        if(!is.null(inv)){
                message("Getting cached data")
                return(inv)
        }
        
        else {
                data <- x$get()
                inv <- solve(data, ...) 
                x$setInv(inv)
                inv
        }
}