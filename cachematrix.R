## This is a pair of functions to calculate
##  and cache the inverse of a matrix
##
## Lexical scoping is used to retain (cache)
##  the inverse of the matrix once it is 
##  initially calculated. If the matrix does
##  not change then the inverse can be retrieved
##  without additional calculations

##
## This function creates a special matrix object
##  that can cache its inverse
##
makeCacheMatrix <- function(x = matrix()) {
  
    ## inverse is not yet known
    inverse <- NULL
  
    ## set the value of the matrix
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
   
    ## return the value of the matrix
    get <- function(){
        x  
    }
   
    
    ## set the value of the inverse
    setInverse <- function(inv){
        inverse <<- inv   
    }
   
    ## return the value of the inverse
    getInverse <- function(){
        inverse
    }
   
    ## return a list of functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


##
## This function computes/retrieves the inverse of 
##  the matrix returned by makeCacheMaterix()
##
cacheSolve <- function(x, ...) {
    
    ## get the inverse
    inverse <- x$getInverse()
    
    ## if the inverse is not NULL,
    ##  retrieve the cached info
    ##  and return
    if( !is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
    
    ## if you reach here, inverse is NULL
    ## calculate the inverse
    ## set the inverse, inverse is returned
    data <- x$get()
    inverse <- solve(data)
    x$setInverse(inverse)
    inverse
}








