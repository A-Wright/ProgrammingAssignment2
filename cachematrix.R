# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
     # initialising inverse
     inv <- NULL
     
     # set the matrix x
     set <- function(y) {
             x <<- y
             inv <<- NULL
    }
            
         # get the matrix x
         get <- function() x
         
         # set the inverse of the matrix and store as inv
         setInverse <- function(inverse) inv <<- inverse
         
         # get the inverse of the matrix inv
         getInverse <- function() inv
         
         # return the list
         list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
          
}





## The following function returns the inverse of the matrix
    # 1. checks if the inverse has already been computed
    # 2. if so, gets this result and skips the computation
    # 3. if not, computes the inverse  and sets the value in the cache via the setInverse function


cacheSolve <- function(x, ...) {
    # gets a matrix that is the inverse of 'x'
    inv<-x$getInverse()
    
    # returns if the inverse if already calculated
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
        ## If the inverse wasn't calculated run this;

        # get the matrix from object
        data<-x$get()
       
        # calcualte the inverse and store
        inv<-solve(data) 
        x$setInverse(inv)
        
        # return a matrix which is the inverse of 'x'
        inv
}
