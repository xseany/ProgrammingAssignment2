## -----Overview of makeCacheMatrix and cacheSolve -----
##
## makeCacheMatrix and cacheSolve work together to initialize, 
## get, and set values of makeCacheMatrix objects, namely a
## matrix and its inverse.
## 
## makeCacheMatrix initializes a makeCacheMatrix object of type 
## 'list' which holds functions for setting and getting 2 objects 
## that are initialized in its environment: a matrix and matrix 
## inverse object. cachSolve takes a makeCacheMatrix object x and 
## using the list of functions of x either returns the matrix 
## inverse value in x if it's not NULL or else computes the inverse 
## of the matrix in x, stores the inverse in x, and then returns 
## the value.

## -----makeCacheMatrix explained-----

## makeCacheMatrix takes a matrix x as an argument and stores it. 
## It also stores the value of the inverse of matrix x in cache_inv, 
## which is initialized to NULL and manipulated with the cacheSolve 
## function, as explained later.

## makeCacheMatrix also creates 4 functions: 
## set, which sets a new matrix to be stored in x,
## get, which returns x, 
## setMatInv, which sets the inverse of the matrix stored in cache_inv
## getMatInv, which returns cache_inv

## makeCacheMatrix returns a list containing these 4 functions, which are
## named and thus accessible with the $ symbol

makeCacheMatrix <- function(x = matrix()) { #takes a matrix x (default value is an empty matrix)
  cache_inv <- NULL #cache_inv stores inverse of x (initialized to NULL)
  set <- function(y){ #set function sets value of x to y
    x <<- y #stores y in x, stored in the parent environment
    cache_inv <<- NULL
  }
  get <- function() x
  setMatInv <- function(inv) cache_inv <<- inv #sets cache_inv in the parent environment to inv
  getMatInv <- function() cache_inv
  list(set = set, get = get, setMatInv = #returns named list of functions usable on a makeCacheMatrix object 
         setMatInv, getMatInv = getMatInv)
}

## -----makeCacheMatrix explained-----

## cacheSolve takes a makeCacheMatrix object x and either returns the value 
## of its inverse or, if there is no inverse stored in x, computes it, stores
## it in x, and then returns the value of the computed inverse. 

## cacheSolve works by accessing the matrix and inverse matrix stored x using
## the set and get functions in the list returned by x. This implies that
## cacheSolve only works on makeCacheMatrix objects; if given another object
## type, it will throw an error.

cacheSolve <- function(x , ...) {
  MatInv <- x$getMatInv() #gets the current value of cache_inv
  if (!is.null(MatInv)){ #if cache_inv already has a value non-NULL value, returns the value
    message("getting cached data")
    return(MatInv)
  }
  #If cache_inv is NULL
  mat <- x$get() #gets the matrix stored in x
  MatInv <- solve(mat) #computes its inverse matrix
  x$setMatInv(MatInv) #sets cache_inv to the computed inverse of x
  MatInv #returns the computed inverse of x
}
