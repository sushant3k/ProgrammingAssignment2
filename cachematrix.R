## makeCacheMatrix function accepts a matrix object 
## and creates a special "matrix" object that can cache its inverse.
## For the function to work correctly, it is necessary that the matrix is a square matrix.
## e.g. l = makeCacheMatrix(matrix(1:4, 2, 2))
## l$get()
##       [,1] [,2]
## [1,]    1    3
## [2,]    2    4


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache. 
## e.g. cacheSolve(l)
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## Using l$getInverse() also produces the same output as above.


makeCacheMatrix <- function(x= matrix())
{
        # This is the special matrix object that can cache its inverse
        inv <- NULL
        
        #Supply another matrix object to override the previous x
        set <- function (y) {
                x <<- y
                inv <<- null
        }
        
        # Return the matrix object
        get <- function() x
        
        #Return the Inverse
        getInverse <- function() inv
        
        #Set Inverse
        setInverse <- function(inverse) inv <<- inverse
        
        #Supporting operation on the object. 
        # Note that we can don't really need get() and set() functions unless otherwise specified.
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}




## This method takes the object returned by the makeCacheMatrix object as input
## and retuns a matrix that is the inverse of 'obj'

cacheSolve <- function(obj, ...)
{
        # Get the Inverse of the function
        inv <- obj$getInverse()
        if (!is.null(inv))
        {
                message("Getting cached inverse")
                return (inv)
        }
        data <- obj$get()
        
        # In ideal scenario we don't need null check here. 
        # This scenario can occur if somebody invokes
        # makeCacheMatrix(NULL)
        if (is.null(data))
        {
                message ("No data found.")
                return (NULL)
        }
        
        # Use solve function to get the inverse
        inv <- solve(data)
        
        #Set inverse
        obj$setInverse(inv)
        inv
}

