## The first function creates a special "matrix" object that can cache its inverse. The second function computes the inverse of a special "matrix" but will only calculate this if the inverse of that matrix has not been cached already. 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	n <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setmatrix <- function(inverse) m <<- inverse
	getmatrix <- function() m
	list (set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)

}


## This function returns the inverse of a matrix but returns the cached inverse if it already has been computed. 

cacheSolve <- function(x, ...) {
	m <- x$getmatrix()
	if (x == m){
		message("getting cached data")
		return(m)
	}
	data <- x$matrix()
	m <- solve (x)
	x$setmatrix(m)
	m
	
}

