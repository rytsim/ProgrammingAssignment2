## makeCacheMatrix - creates a list of function to get, set, get_inverse and set_inverse matrixes
## cacheSolve - checks if there is a inverse matrix cached if not calculates the inverse

## makeCacheMatrix - creates a list of function to get, set, get_inverse and set_inverse matrixes

makeCacheMatrix <- function(x = matrix()) {
    inverse_x <- NULL
    #get set data methods
    set <- function(y) {
        x <<- y
        inverse_x <- NULL
    }
    get <- function() x
    
    #get set inverse matrix methods
    set_inverse <- function(inverse) inverse_x <<- inverse
    get_inverse <- function() inverse_x
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}

## cacheSolve - checks if there is a inverse matrix cached if not calculates the inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse_x <- x$get_inverse()
    if(!is.null(inverse_x)) {
        message("getting cached data")
        return(inverse_x)
    }
    data <- x$get()
    inverse_x <- solve(data, ...)
    x$set_inverse(inverse_x)
    inverse_x
}