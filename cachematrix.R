## First of all, we should install and call the package I need 
## to write this function
install.packages("matlib")
library(matlib)

## Then, we write a function to define a matrix and check 
## the matrix invertibility

makeCacheMatrix <- function(x = matrix()) {
    if (ncol(x)==nrow(x) && det(x)!=0) {
        m<-NULL
        set<-function(y){
            x<<-y
            m<<-NULL
        }
        get<-function() x
        setinverse <- function() m <<- inv(x)
        getinverse<-function() m
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
        
    }else{
        return(message("This matrix is not invertible"))
    }
}


## Finally, we will know about the matrix inverse, 
## either with getting it from cached data or with calculating it.

cacheSolve <- function(x, ...) {
    m<-x$getinverse()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data<-x$get()
    m <- inv(data, ...)
    x$setinverse(m)
    m
}

## Now, its time to check our functions with 3 different example

### Example 1
x <-makeCacheMatrix(matrix(c(1,2,3,4),ncol=2,nrow=2))
x$get()
x$setinverse()
x$getinverse()
cacheSolve(x)


### Example 2
x <-makeCacheMatrix(matrix(c(1,2,3,4,5,4,3,2,1),ncol=3,nrow=3))
x$get()
x$setinverse()
x$getinverse()
cacheSolve(x)

### Example 3
x <-makeCacheMatrix(matrix(c(1,2,3,4,5,6,7,8,9),ncol=3,nrow=3))
x$get()
x$setinverse()
x$getinverse()
cacheSolve(x)
