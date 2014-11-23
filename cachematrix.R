
makeCacheMatrix <- function(x = matrix()) {
	mat_inv <- NULL
	set <- function(y) {
                x <<- y
                mat_inv <<- NULL
	}	
	get <- function() x
	setinv <- function(cal_inv) mat_inv <<- cal_inv  
	getinv <- function() mat_inv
	list(set=set, get=get,
		setinv = setinv,
		getinv = getinv)       
}


cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	mat_inv <- x$getinv()
	if(!is.null(mat_inv)) {
		message("getting cached inverse")
		return(mat_inv)
	}
	data <- x$get()
	mat_inv <- solve(data,...)
	x$setinv(mat_inv)
	mat_inv
}







