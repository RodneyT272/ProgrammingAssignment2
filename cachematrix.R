## Creates objects that stores a matrix and its inverse.
## If cacheSolve is called once it will calculate the inverse and store it.
## Afterwards if cacheSolve is called on the same object the stored
## inverse will be retrieved rather than re-calculating the inverse.

## Creates a special matrix that bundles together the original matrix data,
## the matrix inverse, and functions to change and retrieve that data.

makeCacheMatrix <- function(x = matrix()) {
    # Args:
    #     x: A square numeric matrix
    # Returns:
    #     An object that includes matrix data, "x",
    #     a placeholder for inverse data, "inv",
    #     and four functions to manipulate those variables inside
    #     this scope.  This is similar to the
    #     prinicple of data encapsulation from OOP.
    
    inv <- NULL
    set <- function(y) {
        # If called, sets the value of x and resets inv.
        x <<- y         # "Superassignment" operator, operates on variables in
        inv <<- NULL    # makeCacheMatrix scope, not set's scope.
    }
    get <- function() x            # If called, returns the stored matrix data
    setinverse <- function(inverse) inv <<- inverse    
    getinverse <- function() inv
    list(set = set, get = get,     # Returns the functions available for object
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returns the inverse of the matrix stored in the object given in the
## argument calculated by solve function.

cacheSolve <- function(x, ...) {
    # Args:
    #     x: Object created by the function makeCacheMatrix
    #     ...: Arguments to pass to the solve function
    #
    # Returns:
    #     The inverse of the matrix.
    #     If object's matrix inverse has already been calculated,
    #     the cached inverse.
    
    inv <- x$getinverse()                 # Retrieve inverse if possible
    if(!is.null(inv)) {
        message("getting cached data")    # Stored inverse variable not empty
        return(inv)                       # exits function
    }
    data <- x$get()                       # Inverse was null, must calculate
    inv <- solve(data, ...)
    x$setinverse(inv)                     # Save solution to cache
    inv
}

