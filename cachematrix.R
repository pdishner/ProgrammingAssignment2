##   Below are two functions that cache (store) the inverse of a matrix
##
## This function creates a matrix object that can cache (store) it's inverse
makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
          ##   Function that sets the matrix
     set <- function(y) {
          ##   The <<- is used to assign a value to the objects "x" and "inv" in an environment
          ##   different from the current environment  ##
          x <<- y
          inv <<- NULL
     }
          ## Function that gets the matrix   ##
     get <- function() x
          ## Functions that sets the inverse ##
     set_invers <- function(inverse) inv <<- inverse
          ## Function that gets the inverse  ##
     get_inverse <- function() inv
          ##   return a list containing functions above that 'get' and 'set the matrix and inverses ##
     list(set = set, get = get,
          set_invers = set_invers,
          get_inverse = get_inverse)
}
##   The function below calculates the inverse of the matrix created above (makeCacheMatrix)   ##
##   If the inverse has been calculated, then the function retrieves the inverse from the cache     ##
##   If the inverse has not yet been calculated, the inverse is calculated, set, and returned  ##

cacheSolve <- function(x, ...) {
          ## Return a matrix that is the inverse of x    ##
     inv <- x$get_inverse()
          ##     Fetching cached data if the inverse has already been calculated     ##     
     if (!is.null(inv)) {
          return(inv)
     }
          ##     Calculates the inverse if the inverse has not already been calculated ##
     mat <- x$get()
     inv <- solve(mat, ...)
          ##      Sets the inverse which was just calculated   ##
     x$set_invers(inv)
          ##      Returns the inverse which was just calculated     ##
     return(inv)
}

