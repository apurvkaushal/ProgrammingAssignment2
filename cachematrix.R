## Functions builds a matrix that has its inverse cached and return an inverse
## matrix by either solving for it if not present in cache else returning
## directly from the cache

## Makes a special matrix object having its inverse cached.
makeCacheMatrix <- function(x = matrix()) {
## Initializing inverse of the matrix object as null
invrs<-NULL
## Setting the matrix attribute of the special object to a given matrix y
set<-function(y){
	x<<-y
	invrs<<-NULL
	}
## Setting the inverse attribute to an already calculated inverse matrix k
setinv<-function(k){
	invrs<<-k
	}
## Getting the value of the matrix attribute of the object.
get<-function() x
## Getting the value of the invere attribute of the object.
getinv<-function() invrs
list(set=set, get=get, setinverse=setinv, getinverse=getinv)
}


## Returns the inverse either through cache directly or via computation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
## getting the inverse attribute of the matrix object x
invt<-x$getinv()
## Checking if the cached inverse attribute is null or not.
if(!is.null(invt)){## If not null then directly returning the cached matrix
	message("mgetting cached data")
      return(invt)
	}
## If null, then getting the matrix attribute of the object x
mtrx<-x$get()
## Calculation of inverse
invrs<-solve(mtrx)
## Setting the cached inverse attribute of the object x to the computated inverse.
x$setinv(invrs)
## Returning the inverse matrix
invrs
}
