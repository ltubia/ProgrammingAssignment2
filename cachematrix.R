## cachmatrix.R
## ============
##      Description:    containts makeCacheMatrix and cachSolve functions code for applying ProgrammingAssignment2 of the
##                      R Programming John Hopkins specialization
##
##

# Steps for testing it:
# ---------------------
#       source("<YourPathHere>cachematrix.R")           # load code
#       m <- matrix(rnorm(100,5,1),10,10)               # create a rnorm matrix
#       x <- makeCacheMatrix(m)                         # build caché
#       x$get()                                         # return data from cache
#       cacheSolve(x)                                   # calculate inverse matrix from scratch
#       cacheSolve(x)                                   # recover inverse matrix from cache
#       m <- matrix(c(-1, -2, 1, 1), 2,2)               # prepare other matriz to change first
#       x$set(m)                                        # set new matrix
#       x$getinverse()                                  # check for inverse matrix: it's an empty matrix empty because it was restarted in previous step
#       cacheSolve(x)                                   # calculate inverse matrix again
#       cacheSolve(x)                                   # recover inverse matrix from cache


# makeCacheMatrix:
# ---------------
#       Description:    function creates a special "vector", which is really a list containing a function to
#                               set the value of the matrix
#                               get the value of the matrix
#                               set the value of the inverse matrix
#                               get the value of the inverse matrix
#
#       Parameters: a square non singular matrix
# 

makeCacheMatrix <- function(Mx = matrix()) 
{

        ## initialize inverseMx variable (set it to NA value)
        inverseMx <- matrix()
                
        ## function to set the value of a new source matrix (through MxSet parameter)
        set <- function(MxSet) 
                        {
                        Mx <<- MxSet            ## set value at the parent environment level
                        inverseMx <<- matrix()  ## initialize inverse matrix because a new matrix is set
                        }
        
        ## function to recover source matrix from Mx variable
        get <- function() 
                        { Mx                          
                        }
        
        ## function to set de inversed matrix throug inverseMxSet parameter
        setinverse <- function(inverseMxSet)  
                        { inverseMx <<-inverseMxSet 
                        }
        
        ## function to recover inversed matrix from inverseMx variable
        getinverse <- function() 
                        { inverseMx 
                        }
        
        ## return list of elements of type functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


# cacheSolve:
# -----------
#       Description:    function that calculates the inverse of an input non singular matrix. 
#                       It calculates it by using the solve() function if it's not found in cache
#
#       Parameters: a makeCacheMatrix type variable pointing to a cached matrix
# 


cacheSolve <- function(Mx, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        
        ## Recover inverse matrix from global variable and store it in the CalcinverseMx variable
        CalcInverseMx <- Mx$getinverse()
        
        ## if not null (evaluating first element only) then returns the cached inversed matrix and exit function
        if(!is.na(CalcInverseMx[1,1])) {
                message("getting cached data")
                return(CalcInverseMx)
        }
        
        ## if not, recover matrix original data
        data <- Mx$get()
        
        ## Calculate inverse matrix and overwrite CalcinverseMx variable
        CalcInverseMx <- solve(data)
        
        ## store inversed matrix in the global variable
        Mx$setinverse(CalcInverseMx)
        
        ## Show inversed matrix
        CalcInverseMx
}
