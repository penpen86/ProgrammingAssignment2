## The two functions will set the value of a Matrix, then solve for the inverse,
## and then cached the inverse matrix so we could save computational time
## on the future

## The first function will do 4 things: set the value of the matrix, get the 
## value of the matrix, set the inverse matrix, and get the inverse matrix. All
## expressed on a list of functions

makeCacheMatrix <- function(x = matrix()) {
        m<- NULL
        set<-function(y){ ## I will use as a model the makeVector function
                x<<-y
                m<<-NULL
        }
        get <- function() x
        setinverse <- function(solve) m<<-solve
        getinverse <-function() m
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function will get the special "matrix" of the previous function and
## will obtain the inverse matrix and store it on a cache file

cacheSolve <- function(x, ...) {
        m<-x$getinverse()
        if(!is.null(m)){
                message("Getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setinverse(m)
        m
}
