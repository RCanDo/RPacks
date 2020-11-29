
regroup <- function(x,...){UseMethod("regroup")}

regroup.default <- function(x,...){

warning("This is default for generic regroup(). It returns object passed.
There is no 'regroup' method for class \""              #"
,paste(class(x),collapse="\", \"")
,"\".
Currently the only exiting methods are \"", paste(methods("regroup"),collapse="\", \""),"\".")     #"

return(x)
}

regroup.curves_and_prediction <- function(cap,...){
   rwt <- edit(as.data.frame(unclass(t(cap$groups))))
   rwt <- t(rwt)
   names(attr(rwt,"dimnames")) = c("portfolio","group")
   rwt
}

#regroup.curves_and_prediction <- function(cap,...){
#   rwt <- edit(unclass(cap$groups))
#   names(attr(rwt,"dimnames")) = c("portfolio","group")
#   rwt[,apply(rwt,2,function(x){!any(is.na(x))})]
#}
