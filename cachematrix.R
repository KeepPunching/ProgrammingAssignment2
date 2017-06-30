

## makeCacheMatrix function returns a list of functions as described in comments below.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL 									# initialize inverse to NULL
	set <- function(y){								# function to reset the value of matrix with argument y and initialize invere to NULL  
		x<<-y
		inv <<- NULL
	}
	get <- function() x  								# returns matrix
	setinv <- function(inverse)inv <<- inverse				# sets inv to argument inverse
	getinv <- function() inv							# returns matrix inverse stored in cache
	list(set = set, get = get, setinv = setinv, getinv = getinv)	# list of functions returned
}


## cacheSolve function checks for inverse value stored in cache. If found, returns the stored value. Otherwise calculates inverse and returns calculated value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if (!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinv(inv)
	inv
}
