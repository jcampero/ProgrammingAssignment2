## Programming Assignment 2 - Scoping Rules & Lexical Scoping
##      Develop 2 functions which together are capable invert a matrix,
##      using the caching mechanism of R Languaje to aviod repeating the
##      comptutation if the matrix hasn't changed

## makeCacheMatrix function - Manipulate (get/set) matrix content's with a 4 function list:
##      set - sets the values of the matrix x (checking first if the values passed 
##              are the same to the actual ones, and in that case function returns
##              without modifying the original matrix)
##      get - returns matrix's contents
##      setInverse - sets the values of the inverse matrix
##      getInverse - returns inverse matrix's contents

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        if (!identical(x,y))
            x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(Inverse) m <<- Inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve function - Main function to solve the inverse of matrix 'x'
##      1 - get contents of inverse matrix 'x'
##      2 - if contents aren't NULL (alreay computed) display message and return them
##      3 - get contents of matrix 'x'
##      4 - compute inverse ot matrix 'x' with the solve function
##      5 - set contents with inverse matrix results
##      6 - return inverse matrix results

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInverse(m)
    m
}