#makeCacheMatrix: This function creates a special "matrix" object 
#that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        matrix_inv <- NULL
        set <- function(y) 
        {
                x <<- y
                matrix_inv <<- NULL
        }
        get <- function() x
        
        setinverse <- function(inverse) 
        {
                matrix_inv <<- inverse
        }
        getinverse <- function() matrix_inv
 
        list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


# cacheSolve: This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(funclist, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrix_inv <- funclist$getinverse()
        if (!is.null(matrix_inv)) 
        {
                print ("getting cached inverse matrix")
        } 
        else 
        {
                print ("Computing inverse")
                matrix_inv <- solve(funclist$get(), ...)
                funclist$setinverse(matrix_inv)
                
        }
        matrix_inv
}
