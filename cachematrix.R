# Caching the Inverse of a Matrix (Programming assignment 2)
# Johns Hopkins University / Coursera: "R Programming"
#
# Solution proposed by Jean-Marc Gulliet, February 2015
#
# Remarks:
#   1) I have followed the Google's Style Guide for R, 
#   available at https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml
#
#   2) Some interresting insights about how the non-local assignment operator
#   '<<-' works can be found at 
#   http://www.inside-r.org/r-doc/methods/ReferenceClasses

makeCacheMatrix <- function(mat = matrix()) {
    # This function creates a special "matrix" object that can cache its inverse,
    # thus speeding up the repeated usage of this inverse matrix.
    #
    # Args: 
    #   mat: a square invertible R matrix.
    #
    # Returns:
    #   A special "matrix" object that is a list of four elements indeed.
    
    mat.inverse <- NULL
    set <- function(m) {
        mat <<- m
        mat.inverse <<- NULL
    }
    get <- function() mat
    setinverse <- function(inv) mat.inverse <<- inv
    getinverse <- function() mat.inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

cacheSolve <- function(mat, ...) {
    # Computes the inverse of the special "matrix" returned by makeCacheMatrix. 
    # If the inverse has already been calculated and the matrix has not changed, 
    # then the cachesolve function retrieves the inverse from the cache,
    # otherwise, it computes it and caches it.
    #
    # Args:
    #   mat: a special "matrix" object made by makeCacheMatrix.
    #
    # Returns:
    #   The inverse matrix of mat.
    
    mat.inverse <- mat$getinverse()
    if(!is.null(mat.inverse)) {
        message("getting cached inverse matrix")
        return(mat.inverse)
    }
    m <- mat$get()
    mat.inverse <- solve(m, ...)
    mat$setinverse(mat.inverse)
    mat.inverse
}

# Below are some unit tests.
# First, we create a cached 2 by 2 matrix and check its value.
a <- makeCacheMatrix(matrix(1:4, 2, 2))
# We expect to see the matrix
#   [1  3]
#   [2  4]
a.val <- a$get()
a.val
# Second, we compute the inverse of 'a' for the first time: we do 
# not expect to see any message about caching here.
a.inv <- cacheSolve(a)
# The inverse should be the matrix
#   [-2  1/2]
#   [1  -1/2]
a.inv
# Now we shold see a message about the cached value
a.inv <- cacheSolve(a)
a.inv
# Finaly, we check that this is the correct inverse matrix indeed by
# using the matrix multiplication operator %*% and we expect to get
# the 2 by 2 idendity matrix.
a.val %*% a.inv
# The above line should be equivalant to a direct call to the functions:
a$get() %*% cacheSolve(a)
