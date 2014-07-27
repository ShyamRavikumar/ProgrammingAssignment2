## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


#Returns -> A list containing 4 functions set,get,setinv,getinv
#Variables ->   1)  'inv' stores the inverse, is NULL initially
#               2)  'x' stores the matrix, is initialized when
#                    function is first called
#Params -> A matrix , which has atleast a single element
makeCacheMatrix <- function(x = matrix()) {
    
    inv<- NULL
    set <- function(y){
        if(is.matrix(y)){
            x<<- y
            inv<<- NULL
        }
    }
    get <- function() x
    setinv <- function(l){
        
        inv <<- l
    }
    
    getinv <- function() inv
    
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function



#Returns the inverse of the matrix.
#Params -> 'x' is a list of type makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse<- x$getinv()
        if(!is.null(inverse) ){
            message("The inverse was cached earlier.. Returning cached data")
            return(inverse)
        }
        x$setinv(solve(x$get()))
        return(x$getinv())
}
