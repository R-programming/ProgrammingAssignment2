#The assignment is to write a pair of functions that cachethe inverse of 
#a matrix.

#makeCacheMatrix: This function creates a special "matrix" object that can cache
#its inverse.
makeCacheMatrix<-function(Matrix=diag(3)){
    # if an object is called without a method
    CashInverse<-NULL         # Set the original value of "CashInverse" as NULL
    
    # "setCacheMatrix" function initializes everything: gets a new matrix and  
    # stores the matrix in the cache, and clears the inverse in the cache 
    # by setting "CacheInverse" value to NULL
    setCacheMatrix<-function(y){
        CacheMatrix<<-y      #Assign new matrix y to "CacheMatrix" in cache and
                             #in global environment
        CacheInverse<<-NULL  #Clear CacheInverse in global environment to NULL
    }
    
    #"getCacheMatrix" function returns the matrix "CacheMatrix" from cache
    getCacheMatrix<-function() CacheMatrix
    
    #"setCacheInverse" function assigns the inverse of matrix to "CacheInverse"
    #in cache and in global environment
    setCacheInverse<-function(inverse) CacheInverse<<-inverse
    
    #"getCacheInverse" function retrieves the inverse of matrix from the cache
    getCacheInverse<-function() CacheInverse
    
    #Create a list of functions defined in "makeCacheMatrix"
    list(setCacheMatrix=setCacheMatrix,
         getCacheMatrix=getCacheMatrix,
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
    #Get the inverse from cache and assign it to CacheInverse
    CacheInverse<-input$getCacheInverse()
    
    # Get the matrix from cache and assign it to inputMatrix
    inputMatrix<-input$getCacheMatrix()
    
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
        input$setCacheMatrix(inputMatrix)
        input$setCacheInverse(NewInverse)
        return(NewInverse)
    }
}