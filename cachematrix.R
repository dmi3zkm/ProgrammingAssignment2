# Create cached inverted matrix
makeCacheMatrix <- function(x = matrix()) {
    
    ## x: a square invertible matrix
    ##
    ## return: list of functions
    ##  1. set the matrix
    ##  2. get the matrix
    ##  3. set the inverse matrix
    ##  4. get the inverse matrix
    ##  this list is used as input to cacheSolve()
    
    inverse <- NULL;
    
    set <- function(y){
        x <<- y;
        inverse <<- NULL;
    }
    
    get <- function() x;
    
    setinverse <- function(inv) inverse <<- inv;
    
    getinverse <- function() inverse;
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse);
    
}

## Return the inverted matrix
cacheSolve <- function(x = matrix(), ...) {
    
    ## x: output of makeCacheMatrix()
    ## return: inverted matrix input to makeCacheMatrix()
    
    inverse <- x$getinverse();
    
    # if inverted matrix already exists
    if(!is.null(inverse)){
        message("getting cached inverse");
        return(inverse);
    }
    
    # otherwise, calculate matrix inverse
    mt <- x$get();
    
    inverse <- solve(mt, ...);
    
    x$setinverse(inverse);
    
    inverse;
}
