## Cache matrix and solve for inverse, if necessary.


## While this function will take in the argument and cache it. 
## it provides functions for set and retrieval of input matrix,
## and set and retrival of its inverse.


makeCacheMatrix <- function(mtx = matrix()) {
    inverse <- NULL
    set <- function(x) {
        mtx <<- x;
        inverse <<- NULL;
    }
    get <- function() return(mtx);
    setinv <- function(inv) inverse <<- inv;
    getinv <- function() return(inverse);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


##The first argument, x, cacheSolve takes in is one that is assigned 
##previously with makeCacheMatrix(mtx = matrix()).
##It then first checks using getinv() defined in makeCacheMatrix
## to decide whether there is a pre-baked inv which represents
##inverse of matrix(). If there is one, it will return the inverse, 
## else while it will go through else block, get calculated, and returned 
##in inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <-x$getinv()
    if(!is.null(inverse)){
        print("getting cached data")
        return(inverse)   
    }
    else{
        inverse <- solve(x$get())
        inverse <- x$setinv(inverse)
        return(inverse)
    }
}
