## makeCacheMatrix() is a function which 
#       1. Sets a matrix
#       2. Gets a matrix
#       3. Sets(caches) the inverse of the matrix
#       4. Gets (the cached) inverse of the matrix
# The function returns a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        # Initialize the inverse to NULL
        i <- NULL
        
        # Initialize a new matrix, Initialize the inverse of the matrix to NULL
        # The use of <<- means that the x and i returned in the get_matrix and 
        # get_matrixinverse reflects the values set in set_matrix and
        # set_matrixinverse
        set_matrix <- function(y) 
        {
                x <<- y
                i <<- NULL
        }
        
        # Returns the matrix that has been cached
        get_matrix <- function()
        {
                x
        }
        
        # Set the inverse matrix
        set_matrixinverse <- function(inverse) 
        {
                i <<- inverse
        }
        
        # Returns the inverse of the cached matrix.
        get_matrixinverse <- function() 
        {
                i
        }
        
        # Return list of functions that can be accessed by the object of this function
        list(set_matrix = set_matrix, get_matrix = get_matrix,
             set_matrixinverse = set_matrixinverse,
             get_matrixinverse = get_matrixinverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix(). 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve() retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        i <- x$get_matrixinverse()
        
        # Check whether the inverse has been cached and return the cached
        # inverse matrix
        if(!is.null(i)) 
        {
                message("getting cached inverse")                
                return(i)
        }
        
        # The inverse of the matrix has not been cached
        
        # Get the matrix
        data <- x$get_matrix()
        
        # Calculate the inverse of the matrix 
        i <- solve(data, ...)
        
        # Cache the inverse
        x$set_matrixinverse(i)
        
        # Return the cached matrix that is the inverse of 'x'
        i
        
}
