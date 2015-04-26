# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# Function 1: makeCacheMatrix(x=matrix())
#
# make accepts a matrix as a formal argument and pack it into an object w/ the following
#
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
#
# it returns an object containing the matrix and the special info
#

makeCacheMatrix <- function(x = matrix()) {

		# reset the inverse value
		#
	    inv <- NULL

        # `<<-` assigns a value to an object in an environment 
        # different from the current one. 
        #
       
        set <- function(y) {

                x <<- y
                inv <<- NULL
        }

        get <- function() x

        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv

        list(set = set,
        	 get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#
# Function 2: cacheSolve(x, ...)
# A replacement function that instead of straight up computation of the inverse of a matrix
# 
# x is an output of the makeCacheMatrix, not a straight up matrix
# cacheSolve still returns the inverse of x, by calculating it if needed or retrieve a cached version 
# of inv(x) from parent environment if it exits
#
# This exercise utilizes the side effects operations of R, specifically the '<<-' operator, which assigns 
# a value to a different environment, notably the parent environment
# The end result, to me, is akin to having a global variable
# So you can calculate the inverse of a matrix, store it into an object's internal variable in an upper 
# environment, and it will work as a global variable that you can reference as needed
# 

# Usage
#
# the standard operating procedure is to use solve(matrix) to calculate inverse of matrix, 
#
# > matrix_o <- makeCacheMatrix(matrix)
# > cacheSolve (matrix_o)
#

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        # retrieve the stored inverse value by invoking the object's setinverse() method 
        #
        inv<-x$getinverse()

        if (!is.null(inv)){

                # if there is a non-null inverse available, then use it and get out 
                #
                message("using cached inverse value")
                return(inv)
        }

        # Or we'll have to calculate the inverse if no cached version avails
       	# get the matrix by invoking the object's get() method

        matrix_x <- x$get()

        # Use standard solve() function to calculate the inverse of a matrix
        #
        inv <- solve(matrix_x, ...)
 
 		# Store the inverse of the matrix into the object by invoking the setinverse() method
 		#
        x$setinverse(inv)        	

        # return the inverse of matrix_x
        #
        return (inv)

}
