## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##there are to functions makeCaCheMatrix,MakeCaChematrix
##makeCaCheMatrix consists of set,get,setinv,getinv
##library(MASS) is used to calculate inverse for nonsqured as well as square matrices
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL #Initializing inverse as NULl
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x       #Function to get matrix x
  setinv<-function(inverse)inv<<-inverse
  getinv<-function(){
    inver<-ginv(x)
    inver%*%x        #Function to obtain inverse of the matrix
  }
  list(set =set,get=get, setinv=setinv, getinv=getinv)
}
## Write a short comment describing this function

cacheSolve <- function(x, ...) { #Get cached data
  inv<-x$getinv()
  if(!is.null(inv)){   #Checking whether inverse is null
    massage("getting cached data")
    return(inv) #Return inverse vallues
  }
  data<-x$get()
  inv<-solve(data....)     #Calculate inverse value
  x$setinv(inv)
  inv #Return a matrix List is the inverse of'x'
  
  ## Return a matrix that is the inverse of 'x'
}


