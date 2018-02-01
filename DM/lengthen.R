########################################################################################################################•°
## FUNCTIONS HERE
##  lengthen()
##  trim()
##  adjust()
##
## DEPENDENCIES
##  none
########################################################################################################################•°

########################################################################################################################•°
lengthen = function(x,n,fill=rev(x)[1],adj=FALSE){   ## symmetric to trim()
   if(length(x)<n){
    m = length(x)
#    if(!is.null(fill)){
#      fill = fill[1]
#    }else{
#      fill = x[m]
#    }
    x = c(x,rep(fill,length=n-m))
   }else if(adj){
    x=x[1:n]
   }
   x
} ##----END----##
########################################################################################################################•°

########################################################################################################################•°
trim = function(x,n,fill=rev(x)[1],adj=FALSE){   ## symmetric to lengthen()
   if(length(x)>=n){
      x = x[1:n]
   }else if(adj){
      m = length(x)
#    if(!is.null(fill)){
#      fill = fill[1]
#    }else{
#      fill = x[m]
#    }
    x = c(x,rep(fill,length=n-m))
   }
   x
} ##----END----##
########################################################################################################################•°

########################################################################################################################•°
adjust = function(x,n,fill=rev(x)[1]){
   lengthen(x,n,fill,adj=TRUE)
} ##----END----##
########################################################################################################################•°

########################################################################################################################•°
## EXAMPLES ############################################################################################################•°
########################################################################################################################•°

########################################################################################################################•°
dummy = function(){
## This is dummy function - it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## - in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
###################################################################################################•°
( x = sample(4) )

lengthen(x,10)
lengthen(x,10,fill=rev(x)[1])  ## the same; this is default for 'fill'
lengthen(x,10,fill=x)    ## as
   rep(x,length=10)
lengthen(x,10,x)         ## 'fill' is the third argument so you may write it v.short !
#
lengthen(x,10,NA)
lengthen(x,10,0)


##
adjust(x,10)
adjust(x,10,x)
## ...

trim(x,10)               ## default for 'adj' is FALSE -- then x is not lengthened when n > length(x)
trim(x,10,adj=TRUE)
trim(x,10,NA,adj=TRUE)
trim(x,10,0,adj=TRUE)
trim(x,10,0)

(x = sample(10))
lengthen(x,4)            ## default for 'adj' is FALSE -- then x is not trimmed when n < length(x)
lengthen(x,4,adj=TRUE)
adjust(x,4)
trim(x,4)
rep(x,length=4)          ##

###########################################################•°
## standard functions


}
########################################################################################################################•°
rm(dummy)