##In the following I have made two functions, one which creates a "special" matrix object and one function that calculates the inverse of the matrix   

## This function creates a matrix object (x). This function is capable of handling 4 main actions: It can set and get the value of the matrix, and also set and get the inverse of the matrix
## After handling the above attributes, the function then returns the results as a list object

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
            x <<- y
            m <<- NULL

    }

    get <- function() x

    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function is capable of calculating the inverse of "special" matrix createed in the above function. The function first checks whether the object m (which is the special matrix), as already calulcate nor. If it already calculated, the function
## returns the value from the cache
## In case the inverse of the matrix is not already calculated, then the function calculates the inverse of the matrix using the solve(m) function, and returns the inverse matrix into the setInverse property of the first function
cacheSolve <- function(x, ...) {

    m <- x$getInverse()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInverse(m)
    m
   
}


