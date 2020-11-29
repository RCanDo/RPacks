## ---------------------------------------------------------------------------------------------------------------------•°
## FUNCTIONS HERE
##  lotr()
##  uptr()
##
## DEPENDENCIES
##
## TODO
##
## COMMENT
## Similar to R functions lower.tri() and upper.tri(), but more flexible and convienient.
## ---------------------------------------------------------------------------------------------------------------------•°

## ---------------------------------------------------------------------------------------------------------------------•°
lotr = function(m,diag=FALSE){
## ---------------------------------------------------------------------------------------------------------------------•°
## Lower traingular of a matrix:
## If m is vector with no dimensions then it returns binary matrix of size given in m (first 2 elements of m or
## the only element of m replicated - then a square matrix is returned of size m x m).
## If m is a matrix then lower triangular of it is returned (0's above diagonal).
## The diagonal is excluded or included depending on 'diag' is FALSE (default) or TRUE respectively.
## ---------------------------------------------------------------------------------------------------------------------•°
if(is.null(dim(m))){
   d = rep(m,length=2)
      if(!diag){mm = outer(1:d[1],1:d[2],">")}
      else{mm = outer(1:d[1],1:d[2],">=")}
}else{
   d = dim(m)
      if(!diag){mm = outer(1:d[1],1:d[2],">")}
      else{mm = outer(1:d[1],1:d[2],">=")}
   mm = m*as.numeric(mm)
}
mm
}
## ---------------------------------------------------------------------------------------------------------------------•°

## ---------------------------------------------------------------------------------------------------------------------•°
uptr = function(m,diag=FALSE){
## ---------------------------------------------------------------------------------------------------------------------•°
## Upper traingular of a matrix:
## If m is vector with no dimensions then it returns binary matrix of size given in m (first 2 elements of m or
## the only element of m replicated - then a square matrix is returned of size m x m).
## If m is a matrix then upper triangular of it is returned (0's below diagonal).
## The diagonal is excluded or included depending on 'diag' is FALSE (default) or TRUE respectively.
## ---------------------------------------------------------------------------------------------------------------------•°
if(is.null(dim(m))){
   d = rep(m,length=2)
      if(!diag){mm = outer(1:d[1],1:d[2],"<")}
      else{mm = outer(1:d[1],1:d[2],"<=")}
}else{ d = dim(m)
      if(!diag){mm = outer(1:d[1],1:d[2],"<")}
      else{mm = outer(1:d[1],1:d[2],"<=")}
   mm = m*as.numeric(mm)
}
mm
}  ##----END----##
## ---------------------------------------------------------------------------------------------------------------------•°

## ---------------------------------------------------------------------------------------------------------------------•°
dummy = function(){
## This is dummy function - it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## - in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
## ------------------------------------------------------------------------------------------------•°

## ------------------------------------------------------------------------------------------------•°
## RELOADER (see loadFunctionsAK.R) ## files & PATHfunctions  are defined in  loadFunctionsAK.R
pack = "DM"
source_to_env( files = files[[pack]] , path = PATHfunctions[[pack]] , env = paste(pack,"AK",sep="") )
## ------------------------------------------------------------------------------------------------•°

lotr
uptr
## Similar to R functions lower.tri() and upper.tri(), but more flexible and convienient.

lotr(3)        ## lower trinagle of 3x3 matrix
lotr(3,TRUE)  ## lower trinagle    -"-        with diagonal
uptr(3)        ## upper trinagle of 3x3 matrix
uptr(3,TRUE)  ## upper trinagle    -"-        with diagonal

lotr(c(3,5))
lotr(c(3,5),TRUE)
#! but
lotr(3:5)        ## Warning! Only first two dimensions taken.

(mm = matrix(sample(9),ncol=3))
mm[lotr(3)]
lotr(mm)
lotr(mm,T)

lotr(dim(mm))

## compare with lower.tri() and upper.tri() - R functions
lower.tri(mm)      ## so it is little different, (but maybe more efficient?)
   lower.tri(3)
mm[lower.tri(mm)]
   lower.tri       ## different
      row(mm)
      col(mm)
##

(mm = matrix(sample(12), ncol=3))
lotr(mm)
   lower.tri(mm)          #! not
   mm[lower.tri(mm)]      #! not
   mm*lower.tri(mm)       #! OK
lotr(dim(mm))
   lower.tri(dim(mm))     #! not
   lower.tri(mm)          #! OK
lotr(c(4,6))
   lower.tri(c(4,6))      #! not
   lower.tri(matrix(rep(0,4*6),ncol=6)) #! OK

}
## ---------------------------------------------------------------------------------------------------------------------•°
rm(dummy)
