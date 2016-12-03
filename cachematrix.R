## The first function creates a special "matrix" object that can cache its inverse. The second function computes the inverse of a special "matrix" but will only calculate this if the inverse of that matrix has not been cached already. 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This function returns the inverse of a matrix but returns the cached inverse if it already has been computed. 

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if (!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve (data, ...)
	x$setinverse(m)
	m
}
