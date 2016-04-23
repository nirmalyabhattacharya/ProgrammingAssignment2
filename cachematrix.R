# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.


# makeCacheMatrix accepts a matrix and contains the following functionalities:
# setvalue: sets new values for the matrix
# getvalue: gets the value of the matrix
# getinverse: reads the cached value if present, else returns NULL


makeCacheMatrix <- function(x = matrix()) {
    matrix_cache <- NULL
    setvalue <- function(new_matrix) {
        x <<- new_matrix
        matrix_cache <<- NULL
    }
    getvalue <- function() x
    setinverse <- function(inverse) {
        matrix_cache <<- inverse
    }
    getinverse <- function() {
        matrix_cache
    }
    list(setvalue = setvalue, getvalue = getvalue, setinverse = setinverse, getinverse = getinverse)
}

# cacheSolve accepts a matrix and then,
# Checks for a cached value,
# If a cached value is NOT found, then it computes that value and stores in the cache,
# However, if the value is found, then it simply reads the cache value

cacheSolve <- function(x, ...) {
    # Read the cached value
    matrix_cache <- x$getinverse()
    # If a cached value is not found, then compute the inverse, and store it in the cache
    
    if(is.null(matrix_cache)) {
        data <- x$getvalue()
        tryCatch(matrix_cache <- solve(data), error = function(e) print("Computationally singular error! Please try a simpler matrix."))
        x$setinverse(matrix_cache)
        
        if (!is.null(matrix_cache) ){
            return(matrix_cache)
        }
    }
    
    else{
        
        message("Reading cached data...")
       
        return(matrix_cache)
    }
}
