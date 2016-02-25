## These functions make it so an entered matrix can have its inverse calculated cached in another environment 
## separate from the current one.

## This function creates a special matrix that is actually a list that can set and get the value of the matrix and can
## set and get the value of the inverse of that matrix.

makeCacheMatrix <- function(x = matrix()) {     ## this line creates the function "makeCacheMatrix" which takes a matrix as its input
        i <- NULL               ## this line creates the dummy variable "i" and sets its value to NULL in the current environment
        set <- function(y) {    ## this line creates the function "set" which takes an input "y"
                x <<- y         ## this line assigns the input "y" to a value "x" in a separate environment
                i <<- NULL      ## this line assigns a NULL value to "i" in a separate environment
        }
        get <- function() x                     ## this line creates the function "get" which prints the value of the variable "x"
        setmatrix <- function(matrix) i <<- matrix  ##creates the function "setmatrix" which sets external variable "i" to the input
        getmatrix <- function() i               ## 
        list(set = set, get = get,              ## this creates a list of the functions and sets their tags to be the same as
             setmatrix = setmatrix,             ## the function name
             getmatrix = getmatrix)

}

## This function checks if the "special matrix" created by the first function has already had its inverse calculated and set. If it
## has, it prints that inverse matrix. If it has not, it calculates the inverse matrix, sets it in the cache, then prints it.

cacheSolve <- function(x, ...) {
        i <- x$getmatrix()
        if(!is.null(i)) {                 ## this line checks if an inverse is stored (i.e. "inverse" is not null)
                message("getting cached data")  ## if i is not null, the message "getting cached data" is printed
                return(i)                 ## this line returns the cached value of i (if it exists) and the function ends
        }
        data <- x$get()         ## if an inverse "i" was not stored (i.e. the inverse is null then data is assigned the value of x)
        i <- solve(data, ...)   ## the inverse of the matrix "data" is calculated with the solve function and assigned to "inverse" 
        x$setmatrix(i)          ## this sets the value of the inverse matrix in the cache
        i                       ## this prints the value of the inverse matrix
}
