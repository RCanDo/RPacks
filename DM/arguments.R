########################################################################################################################—•°
## FUNCTIONS HERE    {DM}
##  print.arguments()
##
## DEPENDENCIES
##  coalesce()       {.} infix.R
##
## TODO
##
########################################################################################################################—•°

print.arguments <- function(arg,...){
###################################################################################################—•°
## Printing method for class "arguments".
##    Arguments
## arg      object inheriting from "arguments".
##
##    Remarks
## Object of class "arguments" may be used within function body to record arguments with which the function was called.
## They are created by hand within function body in the following way:
##    arguments <- structure( list("arg_1" = f(arg_1) , "arg_2" = f(arg_2) , ... , "arg_n" = f(arg_n))
##                            , class="arguments" )
## where f() is any function and  arg_1, arg_2, ..., arg_n  are arguments passed to function.
## The idea behind the class "arguments" is to give easy access to
##    • objects itself or
##    • names of objects passed to function arguments.
## Thus f() is most commonly identity function (i.e. we pass "arg_k" = arg_k ) or
## f(arg_k) = deparse(substitute(arg_k))  to get only the name of arg_k.
## The simple rule is that we record an object itself if it is small (short vector which is probably passed unnamed)
## or function.
## If object is large (too long to write it by hand so it is also passed via name) then we remember only its name.
##
## "arguments" differs from  match.call()  as the "call" does not give access neither to names
## of objects passed to function nor to these objects itself.
###################################################################################################—•°

   for(nam in names(arg)){
      if(is.function(arg[[nam]])){
         cat(nam,"= ");print(arg[[nam]])##;cat("\n")
      }else{
         cat(nam,"=",coalesce(arg[[nam]],"NULL"),"\n")
      }
   }

}  ##----END----##
########################################################################################################################—•°

########################################################################################################################—•°
## EXAMPLES ############################################################################################################—•°
########################################################################################################################—•°

########################################################################################################################—•°
dummy = function(){
## This is dummy function - it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## - in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
##############################################################################################—•°

 ## RELOADER —— before it works you need to source("PacksAK.R"); it's best to use EfficiencyCurves.R within pack's dir.
 loadPacksAK("DM")

##############################################################################################—•°

ff <- function(a,b,FUN){
   arguments <- structure( list( a = a , b = deparse(substitute(b)) , FUN = FUN #paste(deparse(substitute(FUN)),collapse="")
                               )
                         , class = "arguments"
                         )
   result <- list( arguments = arguments
                 , b = b
                 , ab = FUN(a,b)
                 )
   result
}

a=2
b=matrix(sample(36),6)
(y = ff(a,b,FUN=function(a,b){a+b}))
y$arguments
y$arguments$a
y$arguments$b
get(y$arguments$b)
y$arguments$FUN
y$arguments$FUN(a,b)

fun <- function(a,b){a*b}
(y = ff(a,b,FUN=fun))
y$arguments
y$arguments$a
y$arguments$b
get(y$arguments$b)
y$arguments$FUN
y$arguments$FUN(a,b)


########################################################################################################################—•°
};rm(dummy)
