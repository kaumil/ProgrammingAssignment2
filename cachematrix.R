## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        #this is a function to get the inverse of the matrix
        inverse <- NULL #initializing the inverse to a null
        
        #method to set the matrix
        set <- function(m){
                x <<- m
                inverse <<- NULL
        }
        
        #method to get the value of the matrix
        get <- function(){
                ##returning the matrix
                x
        }
        
        #setting the inverse of the matix
        setinverse <- function(inv){
                inverse <<- inv
        }
        
        #fetching the cached value of the inverse
        getinverse <- function(){
                inverse
        }
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        
        #return the value if already present
        if (!is.null(inverse)){
                message("getting cached inverse")
                return(inverse)
        }
        
        #Fetching the value of our object
        data <- x$get()
        
        #calculating the inverse using matrix multiplication
        inverse <- solve(data) %*% data
        
        #setting the inverse of the object
        x$setinverse(inverse)
        
        #returning the matrix
        x
}
