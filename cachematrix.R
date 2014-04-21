# This file contains two function definitions : makeCacheMatrix and cacheSolve
# It is related to the R programming Assignement 2 (Coursera - R Programming course)

# makeCacheMatrix creates a special object (a "CacheMatrix")
# containing a matrix and its (cached) inverse (when already calculated - or NULL)

# This special object has a setter (set) and a getter (get)
# It has also a setinverse and getinverse methods


makeCacheMatrix <- function(x=matrix()){
	inverse <- NULL
	set 	  <- function(y){
			x       <<-  y    # Use the <<- operator
			inverse <<-  NULL # When the matrix is set, the (cached) inverse is set to NULL
	}	
	get <- function() x
	
	setinverse <- function(inv) inverse <<- inv
	getinverse <- function() inverse 
	
	list(set=set, get=get, setinverse =setinverse , getinverse =getinverse )
}


# Cachesolve takes a "CacheMatrix" object created with the makeCacheMatrix function
# This function first checks if the CacheMatrix has a cached inverse and returns it.
# If there is no cached inverse available, the function gets the original matrix, inverts it, stores the inverse
# in the (cached) inverse and finally returns this inverse.
# Some comments have been added directly in the code.


cacheSolve <- function(x,...){
	inv <- x$getinverse()    	# get the cached inverse (NULL if not already calculated)

	if (!is.null(inv)){ 
		# cached inverse is available
		message("Getting cached inverse ...")
		return(inv) 		# return the cached inverse and exit the function

	}

	# cached inverse is not available
	mat <- x$get()          	# retrive the original matrix 
	mat.inv = solve(mat,...)	# invert the original matrix (assuming it is always invertible)
	x$setinverse(mat.inv )  	# put the inverse matrix calculated in the cache 
	mat.inv                         # return the inverse
}
