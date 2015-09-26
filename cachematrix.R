## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#Takes in a matrix and returns a list of functions to
## 1. set - sets a new matrix & clears the cached inverse value
## 2. get - get the matrix itself
## 3. setsolve - sets the cached inverse value
## 4. getsolve - returns the cached inverse value
makeCacheMatrix <- function(x = matrix()) {
	#a new matrix would have a NULL inverse matrix as cache	
	  m <- NULL
	  
	# 1. set - sets a new matrix & clears the cached inverse matrix
	  set <- function(y) {
		x <<- y
		m <<- NULL
	  }
	
	# 2. get - get the matrix itself
	  get <- function() x
	  
	# 3. setsolve - sets the cached inverse matrix
	  setsolve <- function(solve) m <<- solve
	  
	# 4. getsolve - returns the cached inverse matrix
	  getsolve <- function() m
	  
	# A list containing the addresses to the 4 functions
	  list(set = set, get = get,
		   setsolve = setsolve,
		   getsolve = getsolve)
}


## Write a short comment describing this function
## Function that takes in the list of functions to :
## 1. checks if the matrix has had its inverse previously caclculated
## 2. if not compute the inverse and cache it
## 3. return the invserse matrix
cacheSolve <- function(x, ...) {
        ## Tries to get the cached inverse value
		  m <- x$getsolve()
		  
		## if value is cached - return it
		  if(!is.null(m)) {
			message("getting cached data")
			return(m)
		  }
		  
		## else proceed to compute it by firstly getting the matrix itself
		  data <- x$get()
		  
		## solve it
		  m <- solve(data, ...)
		  
		## cache the matrix and return it
		  x$setsolve(m)
		  m
}
