## This function creates an empty matrix and cache its inverse 
## to avoid the need to recompute it    



makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(y){
      x <<- y
      i <<- NULL
    }
    
    get <- function(){
      x
    }
    
    setInverse <- function(inv){
      i <<- inv
    }
    
    getInverse <- function(){
      i
    }
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse matrix of a special matrix object
## This function assign the inverse to a special matrix, If its possible to compute it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)){
        message("reading inverse matrix")
        return(i)
    }
    
    mat <- x$get()
    if(det(mat) == 0){
        message("This matrix is not invertible")
    }else {
        i <- solve(mat)
        x$setInverse(i)
    }
    i
}
