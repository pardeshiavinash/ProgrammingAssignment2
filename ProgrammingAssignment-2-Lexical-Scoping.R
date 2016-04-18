# Avinash Pardeshi
# Assignment: Caching the Inverse of a Matrix
# Assignment: Programming Assignment 2: Lexical Scoping

makeCacheMatrix <- function(x = matrix()){
  # print ("SETTING M MATRIX TO NULL")
  m<-NULL
  # print ("SETTING UP FUNCTION SET TO SET NEW MATRIX X FROM THE OUTSIDE ENVIRONMENT TO Y")
  set<-function(y){
    x<<-y
    # print ("SET INVERSED MATRIX CACHE VARIABLE/OUTSIDE THE ENVIRONMENT m TO NULL AS NEW MATRIX IS BETING SET TO GET INVERSE")
    m<<-NULL
  }
  # print ("SETTING UP GET FUNCTION TO DISPLAY VALUE OF X")
  get<-function() x
  # print ("SETTING UP MATRIX m TO INVERSE OF THE MATRIX PASSED TO THE FUNCTION")
  setInverseMatrix<-function(solve) 
  {
    m<<- solve
  }
  
  # print ("SETTING UP GETMATRIX FUNCTION TO DISPLAY VALUE OF INVERSE MATRIX from outside ENVIRONMENT VARIABLE m")
  getInverseMatrix<-function() m
  # print ("SETTING UP RETURN LIST AS LIST OF FUNCTIONS")
  list(set=set, get=get,
       setInverseMatrix=setInverseMatrix,
       getInverseMatrix=getInverseMatrix)
}


cacheSolve <- function(x=matrix(), ...) {
  # print ("ASSIGN m TO GET ALREADY INVERSED MATRIX- IF ANY")
  m<-x$getInverseMatrix()
  # print ("CHECK IF m HAS GOT ALREADY INVERSED MATRIX IN CACHE, IF YES USE IT")
  if(!is.null(m)){
    message("getting cached data")
    # print ("USE ALREADY CACHED INVERSE MATRIX")
    return(m)
  }
  
  # print ("THERE NO CACHED INVERSE MATRIX AVAILABLE")
  matrix<-x$get()
  # print ("GET FRESH INVERSED MATRIX , ASSIGN TO m")
  m<-solve(matrix)
  # print ("CALL SETMATRIX FUNCTION TO SET MATRIX TO OUTSIDE THE ENVRIONMENT VARIABLE m for CACHEING")
  x$setInverseMatrix(m)
  # print ("RETURN m")
  m
}

