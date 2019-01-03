## R Programming Week 3 Peer Review Quiz
## function to cache inverse of a matrix

## function makeCacheMatrix creates special Matix

makeCacheMatrix <- function(x = matrix()) {
        s<-NULL
        set<-function(y){
                x <<- y
                s <<- NULL
        }
        get <-function() x
        setsolve <- function(solve) s<<- solve
        getsolve <- function() s
        list(set=set, get = get,
             setsolve=setsolve,
             getsolve=getsolve)
 
}


## function calculates the inverse of matrix using solve function but first
## it checks if data is cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s<-x$getsolve()
        if(!is.null(s)){
                message("Getting cached data...")
                return(s)
        }
        data<-x$get()
        s<-solve(data,...)
        x$setsolve(s)
        s
}
