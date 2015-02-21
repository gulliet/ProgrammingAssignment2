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
