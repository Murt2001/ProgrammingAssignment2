## These functions take a square matrix and return its inverse (if there is one).
## If the input matrix is not changed, the inverse is returned from cached memory.

## This function takes an input matrix and assigns it to the variable x (in cache) in the set function.  
## It then assigns a series of functions to the get, set_inv, get_inv variables and sets
## them in a list to be called in the cacheSolve function.  The set_inv function assigns
## the matrix inverse to the cache for reference in the case that the input matrix 
## is not changed.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    set_inv <- function(solve) m <<- solve
    get_inv <- function() m
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
}
 

## cacheSolve function takes the input matrix and attempts to return its inverse.
## If the input matrix (x) is the same, the inverse is returned from cache.  If the
## input matrix is new, then the function first checks to see if the matrix is invertible.
## If the determinant is NE to 0, then the inverse is calculated and returned (and
## stored to cache for future reference).  If the determinane equals 0, a message is returne
## saying the matrix is not invertible.  

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'   
    m <- x$get_inv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    #print(data)
    
    #first checks to see if determinant NE 0 before attempting to calculate inverse
    if(det(data) != 0){
        m <- solve(data, ...)
        x$set_inv(m)
        m
    }
    
    else {
        print("Matrix not invertible.")
    }
}




