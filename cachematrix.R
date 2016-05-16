## Matrix inversion is a costly computation and we would like to avoid 
##the computation multiple times. 
# The following functions are used to cache the inverse of the matrix.



# This fn. Sets the matrix, gets the matrix, sets the inverse of 
#the matrix, gets the inverse of the matrix



makeCacheMatrix <- function(x = matrix()) 
  {
  inv<-NULL #Intially set inv to NULL
  set<-function(y){
    x<<-y #sets the value of the matrix
    inv<<-NULL}
  #Get value
  get<-function() x;#gets the value of the matrix
  #Set value of Inverse
  setinverse<-function(inverse) inv <<-inverse
  getinverse<-function() inv
  #Encapsulate into a list
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)	
}




## This function computes the inverse and caches the result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  
  
  if(!is.null(inv)) {# if cached value exists, return inv
    return(inv)
  }
  
  data <- x$get() # If cached value not available, get the matrix
  
 
  inv <- solve(data, ...)# Find the inverse
  
  
  x$setinverse(inv)# Cache the result
  
  ##Return inverse
  inv  
  
}
