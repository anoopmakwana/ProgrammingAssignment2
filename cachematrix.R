## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##	1.	set the value of the matrix
##	2.	get the value of the matrix
##	3.	set the inverse of the matrix
##	4.	get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
                x <<- y
                inverse <<- NULL
	}
	get <- function() x
	setInverse <- function(i) inverse <<- i
	getInverse <- function() inverse
	list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation
cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setInverse(inverse)
        inverse
}
