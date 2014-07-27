# Matrix inversion is usually a costly computation 
# and their may be some benefit to caching the inverse 
# of a matrix rather than compute it repeatedly


# EXAMPLE usage
# m = rbind(c(1,-2), c(-2,1))
# mcm =makeCacheMatrix(m)
# mcm$get()
#
# When executing the below row, it should compute the inverse of the matrix 
# cacheSolve(mcm)
#
# When executing the below row, it should use the cached version of the inverse of the matrix m
# cacheSolve(mcm)

# This function creates a special "matrix" object that can cache its inverse
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# This function computes/retrieves the inverse of the special 
# "matrix" returned by makeCacheMatrix above. 
# Assume this matrix is always inversibe.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("retrieving cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
