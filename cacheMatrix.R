## Johns Hopkins University Data Science Specialization on Coursera
## Course: R Programming
## Title: Programming Assignment 2: Lexical Scoping
## Author: Kenny Ong

##
## Assignment: Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly 
## My assignment is to write a pair of functions that cache the inverse of a matrix.

##
## In this fucntion I used the <<- operator which can be used to assign a value to an object 
## in an environment that is different from the current environment. Below are two functions 
## that are used to create a special object that stores a matrix object and cache's its inverse.
## Function number one (1) is makeCacheMatrix and function number two (2) is cacheSolve

##
## Function: makeCacheMatrix
## Descriotion: The function creates a special "vector", which is really a list containing a function to
##              set the value of the vector
##              get the value of the vector
##              set the value of the inverse
##              get the value of the inverse
## Argument: 1. Matrix
## Example: A<-matrix(
##            c(2,4,3,1),
##            nrow=2,
##            ncol=2
##          )
##        
##          myMatrix <- makeCacheMatrix(A)
##          myMatrix$get()
##          
makeCacheMatrix <- function(x = matrix()) {
  
    #initialize the placeholder local variable to hold the matrix inverse
    inv <- NULL
    
    #set function to store the argument matrix into a 
    set <- function(y) {
        x <<- y ##x will be storing the argument matrix in local environment
        inv <<- NULL ##inv does not store any value and set to null
    }
    
    #get function to return the value of local variable x
    get <- function() x
    
    #set function to set the inverse directly through makeCacheMatrix$setInverse
    setInverse <- function(inverse) inv <<- inverse ## inv will be storing the argument matrix in local environment
    
    #get function to return the value of local variable inv
    getInverse <- function() inv
    
    #create a speacial list of all functions avaialbe in makeCacheMatrix
    list(set=set, 
         get=get, 
         setInverse=setInverse, 
         getInverse=getInverse)
}

##
## FUnction: cacheSolve
## Description: The following function calculates the inverse of the special "vector" created with the above function makeCacheMatrix. 
##              However, it first checks to see if the mean has already been calculated. 
##              If so, it gets the inverse from the cache and skips the computation. 
##              Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the solve function.
##              The function assumes that the matrix supplied is always invertible.
## Argument: 1. makeCacheMatrix object
## Example: cacheSolve(myMatrix)
##
cacheSolve <- function(x, ...) {
  
    ##Apply the getInverse function of the argument myMatrix object inv
    inv <- x$getInverse()
    if(!is.null(inv)) { ##if the inverse is already set (not null) return the inverse
        message("getting inversed data.") #print the message
        return(inv) #return the inversed matrix
    }
    
    ##If the inverse of the matrix within the argument myMatrix object Inv
    ##use solve to calculate the inverse and store within the Inv
    data <- x$get()
    inv <- solve(data)
    
    ##then atore the calculated inverse matrix and store within the Inv
    x$setInverse(inv)
    inv #return the calculated inveser matrix

}
