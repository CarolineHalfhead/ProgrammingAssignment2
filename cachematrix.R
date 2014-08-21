## These two functions are used to create a special object that stores 
## a matrix and caches its inverse.

## This function creates a list of functions to set a matrix and its
## inverse.

makeCacheMatrix<-function(x=matrix()){
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setsolve<-function(solve) m<<-solve
        getsolve<-function() m
        list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
}


## This function takes the output of the above function for a given 
## matrix, checks if the inverse of the matrix has already been
## cached. If so, it returns the value and exits. Otherwise, it
## calculates the inverse and caches it.

cacheSolve<-function(x,...){
        m<-x$getsolve()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setsolve(m)
        m
}