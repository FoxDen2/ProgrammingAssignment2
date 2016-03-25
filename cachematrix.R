#
#   R Programming - Week 3 Making and Using an Inverted Cached Matrix
#
#   Pass in to the function a matrix capable of being inverted.  The idea
#   is to get the inverse and cache the result for future use.
#
#   The return of the function will be a list containing the four functions 
#   defined within.  These will do the following:
#       - Set the matrix
#       - Get the matrix
#       - Set the inverted matrix
#       - Get the inverted matrix
#

makeCacheMatrix <- function(x = matrix()) {

    #
    #   Initialize the inverted matrix cache and set it to NULL
    #
    
    invertedmatrix <- NULL
    
    setmatrix <- function(m) {
        x <<- m
        invertedmatrix <<- NULL
    }
    
    #
    #   Get the matrix
    #
    
    getmatrix <- function() x
    
    #
    #   Set the inverse of the matrix
    #
    
    setinverted <- function(inverse) invertedmatrix <<- inverse
    
    #
    #   Get the inverted matrix
    #
    
    getinverted <- function() invertedmatrix
    
    #
    #   Create the return list
    #
    
    list(setmatrix=setmatrix, getmatrix=getmatrix, setinverted=setinverted, getinverted=getinverted)
}



#
#   R Programming - Week 3 Making and Using an Inverted Cached Matrix
#
#   Use the functions created in makeCacheMatrix to get the inverted matrix
#
#   First this program will check to see if the value has been stored in cache.
#   If it has, the cache value will be used; otherwise we will calculate and 
#   store in cache.  I have added a message indicating if the Cached Data is
#   is being used
#

cacheSolve <- function(x, ...) {

    #
    #   Call the get inverted function to see if the cache has been stored
    #
    
    invertedmatrix <- x$getinverted()
    
    #
    #   If it is not NULL (i.e. exists), use the value -- note the message
    #
    
    if(!is.null(invertedmatrix)) {
        message("Using the Cached Data")
        return(invertedmatrix)
    }
    
    #
    #   Cache does not exist, so we will get it, set it, and return it
    #
    getm <- x$getmatrix()
    
    invertedmatrix <- solve(getm)
    
    #
    #   Set the cache and retrun the inverted matrix
    #
    
    x$setinverted(invertedmatrix)
    
    invertedmatrix
}
