#The code contains the following two functions
#
#1. makeCacheMatrix: This function creates a special "matrix" object
#                    that can cache its inverse.
#2. cacheSolve:      This function computes the inverse of the special
#                    "matrix" returned by `makeCacheMatrix` above.
#                    If the inverse has already been calculated 
#                    (and the matrix has not changed), then the `cachesolve` 
#                    should retrieve the inverse from the cache.



# Function - makeCacheMatrix 
#            This function returns a list of functions.
#            Its puspose is to store a martix and a cached value of the 
#            inverse of the matrix. 
#            
# This functions Contains the following functions:
# setMatrix      sets a new matrix for calculating inverse
# getMatrix      gets the stored matrix
# setInverse     sets the inverse of the matrix
# getInverse     get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        #Set the inverse to NULL
        inverse <- NULL
        
        #This function takes a new matrix as an argument and
        #stores for calcualting the inverse
        setMatrix <- function (newMatrix) {
                #Set the new matrix for calculating inverse
                x <<- newMatrix
                #As we have got a new matrix, set the inverse to NULL
                inverse <<- NULL
        }
        
        #This function returns the stored matrix
        getMatrix <- function () {                
                x
        }
        
        #This function takes inverse of matrix as argument and
        #caches it in another variable. 
        setInverse <- function (inverseOfMatrix){
                #Cache the inverse of a matrix
                inverse <<- inverseOfMatrix
        }
        
        #This function returns the cached inverse of the matrix
        getInverse <- function () {
                #Return the chaced inverse on a matrix
                inverse        
        }
        
        #Now return a named list containg above functions.
        list(setMatrix  = setMatrix, 
             getMatrix  = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}


# Function - cacheSolve
#            This function calculates the inverse of a "special" matrix 
#            created using makeCacheMatrix. If first looks for the 
#            cashed value for an inverse of a matrix, if found, uses it,
#            else, calculates the inverse of a matrix.
cacheSolve <- function(x, ...) {
        #First, try to get the cached value of inverse of a matrix
        inverse <- x$getInverse()
        
        #Check if the inverse of the matrix was cached.
        if (!is.null(inverse))
        {
                message("getting cached data")
                return(inverse)
        }
        
        #If not then we need to calculate the inverse and store it for future use
        
        #Get the matrix
        data <- x$getMatrix()
        #Calculate the inverse using 'solve'
        inverse <- solve(data)
        #Now cached the inverse of the given matrix
        x$setInverse(inverse)
        #Return the inverse
        inverse
}
