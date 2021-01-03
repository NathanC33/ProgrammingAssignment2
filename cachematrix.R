#"Caching the inverse of a matrix"

#These two functions below creates an object to cache the inverse of a given (invertible) matrix
#Since getting the inversion of a matrix can be a costly computation, this caching can be beneficial

#This function creates an object of matrix that can cache its inverse

makeCacheMatrix<-function(x=matrix()){#input a matrix that is invertible
    i<-NULL
    set<-function(y){
        x<<-y #assign to an object in the parent environment
        i<<-NULL
    }
    get<-function()x
    setinverse<-function(inverse) i<<-inverse #again, in the parent environment
    getinverse<-function() i
    #above are the getter of x, the setter of inverse matrix i and the getter of i
    list(set=set, get=get, 
         setinverse=setinverse, 
         getinverse=getinverse)
    #as the elements of a list
}

#This function computes the inverse of the matrix returned by the makeCacheMatrix function given above

cacheSolve<-function(x,...){
    i<-x$getinverse()
    if(!is.null(i)){ #checks whether i is NULL
        message("getting cached data")
        return(i)
    }
    data<-x$get()
    i<-solve(data,...)
    x$setinverse(i)
    i
}