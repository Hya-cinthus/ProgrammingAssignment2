## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	set_inverse <- function(solveMatrix) inv <<- solveMatrix
	get_inverse <- function() inv
	list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$get_inverse()
	if(!is.null(inv)){
		message("getting cached data...")
		return(inv)
	}
	data <- x$get()
	data_inv <- solve(data)
	x$set_inverse(data_inv)
	inv
}
