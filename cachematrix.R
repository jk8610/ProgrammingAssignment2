## this function creates a compound object which is based on its input,
## a user-nominated matrix. the returned result is a list of four(4) 
## functions: set(), get(), setreverse() and getreverse(). the first two
## functions creates/returns the internal copy of the matrix received as input,
## the last two creates/returns the reverse of the same matrix, respectively
makeCacheMatrix<-function(x=matrix()) {
    my_reverse <- NULL
	set <- function(y) {
		x <<- y
		my_reverse <<- NULL
	}
	get <- function() x
	setreverse <- function(reverse) my_reverse <<- reverse
	getreverse <- function() my_reverse
	list(set=set, get=get, setreverse=setreverse, getreverse=getreverse)
}	
	
## this function accepts a 'makeCacheMatrix' object, and returns
## its inverse, by first examining the object's cache, return it ifotherwise
## its inverse has already present, otherwise compute it on the fly
cacheSolve <- function(x,...) {
	rev <- x$getreverse()
	if (!is.null(rev)) {
		message("getting cached reverse matrix")
		return(rev)
	}
	rev <- solve(x$get())
	x$setreverse(rev)
	return(rev)
}
