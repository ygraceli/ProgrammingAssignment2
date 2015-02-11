library(MASS)

#matrix
makeCacheMatrix <- function(x=matrix()) 
    {
	  m<-NULL
  	  set<-function(y)
        {
	       x<<-y
	       m<<-NULL
        }
	  get<-function() x
	  setmatrix<-function(ginv) m<<- ginv
	  getmatrix<-function() m
	  list(set=set, get=get,
   		    setmatrix=setmatrix,
			   getmatrix=getmatrix)
     }

cacheSolve <- function(x, ...)
   {
     m<-x$getmatrix()
     if(!is.null(m))
      {
          message("getting cached Inverse Matrix")
          return(m)
      }
      matrix<-x$get()
      m<-ginv(matrix, ...)
      x$setmatrix(m)
      m
   }

graceMatrix<-makeCacheMatrix(x=matrix(1:12,3,4))
cacheSolve(graceMatrix)
