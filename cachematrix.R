## Cache the inverse of a matrix


## makeCacheMatrix creates a special matrix named "x" whose
## structure is a list

makeCacheMatrix <- function(x = matrix()) {
    
    inverseMatrix <- NULL 
    
    set <- function( jj ){
        x <<- jj
        inverseMatrix <<- NULL
    }
    get <- function(){
        x
    }
    setInverse <- function(kk){
        inverseMatrix <<- kk
    }
    getInverse <- function(){
        inverseMatrix
    }
    list(set = set, 
         get = get, 
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve commit the inverse of the matrix "x" if 
## it hasn't been already done

cacheSolve <- function(x, ...) {
        
    ## check if inverse has been commited
    inverseMatrix <- x$getInverse()
    if(!is.null(inverseMatrix)){
        message("inverse has already been calculated. 
                Getting cached data.")
        return(inverseMatrix)
    }
    
    ## solve matrix if inverseMatrix is NULL
    matr <- x$get()
    inverseMatrix <- solve(matr) 
    x$setInverse(inverseMatrix) 
    inverseMatrix 
}
