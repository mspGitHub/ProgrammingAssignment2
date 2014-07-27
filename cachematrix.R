## This script computes the inverse of the matrix and caches it. Thus eliminating costly computation of matrix inversion.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()){
        #assign default value of NULL to the inverse of the matrix
        i <- NULL
        
        #function to set new matrix and this reset the value of the cache inverse to NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ##function to return the input matrix
        get <- function() x
        
        ##function to cache the inverse of the input matrix
        setinverse <- function(inverse) i <<- inverse
        
        ##function to return the inverse of the input matrix
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## gets the inverse of the special matrix created and stores in inverse(local variable) for check
        inverse <- x$getinverse()
        
        ## checks if the inverse value is NULL or not, if not then return the 
        ## inverse of the matrix
        if(!is.null(inverse))
                message("Getting Cached Inverse Matrix")
        return(inverse)
}

#gets the input matrix from the makeCacheMatrix function and stores it in local variable
data <- x$get()

#computes the inverse of the matrix locally and stores it in local variable
inverse <- solve(data, ...)

#caches the inverse of the matrix using setinverse function of makeCacheMatrix
x$setinverse(inverse)

#returns the inverse of the matrix. Returns a error if the matrix is non-invertible.
inverse
}