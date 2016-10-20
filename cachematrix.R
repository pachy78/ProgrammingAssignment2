## These functions have been created for Programming Assignment 2.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function (original_matrix = matrix())  {
    inverted_matrix <- NULL
    
    set_matrix <- function(matrix_to_set) {
        original_matrix <<- matrix_to_set
        inverted_matrix <<- NULL
    }     
    
    get_matrix <- function(){ 
        original_matrix
    }
    
    set_inv_matrix <- function(inv_matrix_to_set) {
        inverted_matrix <<- inv_matrix_to_set
    } 
    
    get_inv_matrix <- function() {
        inverted_matrix
    }
    
    list(set_matrix = set_matrix, get_matrix = get_matrix,
         set_inv_matrix = set_inv_matrix, get_inv_matrix = get_inv_matrix)
}



##  This function computes the inverse of the special "matrix" returned by 
##  makeCacheMatrix above. If the inverse has already been calculated (and the 
##  matrix has not changed), then cacheSolve will retrieve the inverse from the 
##  cache.

cacheSolve <- function(result_makeCacheMatrix, ...) {
    
    inverted_matrix <- result_makeCacheMatrix$get_inv_matrix()
    
    if(!is.null(inverted_matrix)) {
        message("This is the cached inverted matrix")
        return(inverted_matrix)
    }
    
    original_matrix <- result_makeCacheMatrix$get_matrix()
    
    inverted_matrix <- solve(original_matrix)
    
    result_makeCacheMatrix$set_inv_matrix(inverted_matrix)
    
    inverted_matrix
}


