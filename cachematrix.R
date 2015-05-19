## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function takes an invertable matrix, inverts that matrix and
## stores the results in the function scope for later retrieval without the need to recompute.
## This function returns a list of methods associated with that inverted matrix: set, get,
## setinverse and getinverse

## set - stores the original matrix
## get - returns the original matrix
## setinverse - computes the inverted matrix and stores the result in a variable called inverse
## getinverse - retrieves the value of the variable inverse

## when initialized, the inverse is not computed, only the original matrix is stored and the
## object is created with the 4 methods described above.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL;
    set <- function(y) {
        x <<- y;
        inverse <<- NULL;
    }
    get <- function() x;
    setinverse <- function(solve) inverse <<- solve;
    getinverse <- function() inverse;
    
    #explicit use of return for clarity
    return(list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse));   
}


## cacheSolve function takes the list output/object from makeCacheMatrix function, 
## and attempts to retireve the inverse from the resultant object of makeCacheMatrix function. 
## If the inverse has been already computed, returns it from the cache (from the scope of the 
## makeCacheMatrix function) and notifies the user that a cached copy is being used.

## If the inverse has not already been computed, it retrieves the original matrix and invokes the
## setinverse method from the makeCacheMatrix object.


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse();
    if(!is.null(inverse)) {
        message("getting cached data");
        return(inverse);
    }
    data <- x$get();
    inverse <- solve(data, ...);
    x$setinverse(inverse);
    
    #explicit use of return for clarity
    return(inverse);  
}

