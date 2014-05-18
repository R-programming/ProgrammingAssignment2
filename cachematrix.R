#The assignment is to write a pair of functions that cachethe inverse of 
#a matrix.

#makeCacheMatrix: This function creates a special "matrix" object that can cache
#its inverse.
makeCacheMatrix<-function(Matrix=diag(3)){
    
    CashInverse<-NULL         # Set the original value of "CashInverse" as NULL
    
    # "setMatrix" function initializes everything: getting a new matrix and 
    # clearing the cache by setting "CacheInverse" value to NULL
    setMatrix<-function(y){
        Matrix<-y             #New matrix y assigned to "Matrix"
        CacheInverse<<-NULL   #Clear CacheInverse in global environment to NULL
    }
    
    #"setCacheMatrix" function stores a matrix in the cache
    setCacheMatrix<-function(z) CacheMatrix<<-z   #Store matrix z to CacheMatrix
                                                  #in globalenvironment
    
    #"getMatrix" function returns the input matrix "Matrix"
    getMatrix<-function() Matrix
    
    #"setCacheInverse" function stores the inverse of matrix into cache by 
    #assigning inverse to "CacheInverse" 
    setCacheInverse<-function(inverse) CacheInverse<<-inverse
    
    #"getCacheInverse" function retrieves the inverse of matrix from the cache
    getCacheInverse<-function() CacheInverse
    
    #Create a list of functions defined in "makeCacheMatrix"
    list(setMatrix=setMatrix,
         setCacheMatrix=setCacheMatrix,
         getMatrix=getMatrix,
         setCacheInverse=setCacheInverse,
         getCacheInverse=getCacheInverse
        )
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
#cacheSolve: This function computes the inverse of the special "matrix" returned
#by makeCacheMatrix above. If the inverse has already been calculated (and the 
#matrix has not changed), then the cachesolve should retrieve the inverse from 
#the cache.
cacheSolve<-function(input,...){
    #Get the inverse from cache 
    CacheInverse<-input$getCacheInverse()
    
    # Get a new matrix and assign it to inputMatrix
    inputMatrix<-input$getMatrix()
    
    # if there is value in the cache (!is.null(cacheInverse)=TRUE) AND the
    # "inputMatrix" and "CacheMatrix" are identical, then the function retrieves
    # inverse of matrix from the cache (in global environment)
    if(!is.null(CacheInverse) & identical(inputMatrix,CacheMatrix)){
        message("Getting cached inverse")
        return(CacheInverse)
    }

    # if there is no value in the cache (is.null(cacheInverse)=TRUE) OR the
    # "inputMatrix" and "CacheMatrix" are different, then the function solves 
    # the inverse of matrix, return the "NewInverse", set the inverse in cache,
    # and save the new matrix in cache
    if(is.null(CacheInverse) | !identical(inputMatrix,CacheMatrix)){
        message("Computing the inverse of matrix")
        NewInverse<-solve(inputMatrix)
        input$setCacheInverse(NewInverse)
        input$setCacheMatrix(inputMatrix)
        return(NewInverse)
    }
}