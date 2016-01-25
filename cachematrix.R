## makeCacheMatrix creates a special matrix object, and then cacheSolve 
## calculates the inverse of the matrix.
## If the matrix inverse has already been calculated, it will instead 
## find it in the cache and return it, and not calculate it again.
makeCacheMatrix <- function(x = matrix()) {
	my_inverse = NULL
	set = function(y) {
		x <<- y
		my_inverse <<- NULL
	}

	get = function() x
	setinv = function(inverse)  my_inverse <<- inverse
	getinv = function() my_inverse
		list(set=set, get=get, 
			 setinv=setinv, 
			 getinv=getinv)
}


## The function cacheSolve returns the inverse of a matrix created with the makeCacheMatrix function.
## If the cached inverse is available, cacheSolve retrieves it, if not, it computes, caches, and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        my_inverse = x$getinv()

        if (!is.null(my_inverse)) {
        	message("cache available")
        	return (my_inverse)
        } else {
        	mat.data = x$get()
        	my_inverse = solve(mat.data, ...)
        	x$setinv(my_inverse)
        	return(my_inverse)	
        }
}
