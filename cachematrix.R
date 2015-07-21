#Make cached matrix and store "set, get, setINV, getINV".

makeCacheMatrix <- function(x = matrix()) {
  
    m <- NULL #Discard the previous cached matrix to ensure recalculation
  
    set <- function(y) {
      x <<- y #Store matrix in higher level function
      m <<- NULL #Discard the previous cached matrix
    }
    
    get <- function() x #returns the vector x stored in the main function
    
    setINV <- function(INV) m <<- INV #function to store INV
    
    getINV <- function() m #function to get INV
    
    list(set = set, get = get, setINV = setINV, getINV = getINV) #Store the lower level functions 
}


#This function outputs an inversed matrix. If the matrix was already stored through makeCacheMatrix,  
CacheSolve <- function(x, ...) {
 
    m <- x$getINV() #call getINV from CacheMatrix
    
    if(!is.null(m)) { #if there's a cached value, print "getting cached data".
        message("getting cached data")
        return(m)
    }
    
    data <- x$get() #if there is no cached value...
    
    m <- solve(data, ...) #...do the calculation to inverse the matrix. This should always be the case when running in a fresh environment.
    
    x$setINV(m) #Update the cached value
    
    m #Return the inversed matrix
  }



#Test case

matr <- matrix(c(1,0,5,2,1,6,3,4,0),nrow = 3,ncol = 3) #test matrix, found on Coursera forum. If the matrix gets inversed correctly, the output shall be:
#   [,1] [,2] [,3]
#   [1,]  -24   18    5
#   [2,]   20  -15   -4
#   [3,]   -5    4    1

z <- makeCacheMatrix(matr) #store output of Cachematrix function in z

CacheSolve(z) #If Cachesolve works correctly, first time the matrix result is outputted, and second time the function prints "getting cached data".
