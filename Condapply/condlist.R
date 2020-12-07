## ---------------------------------------------------------------------------------------------------------------------
## FUNCTIONS HERE
##  condlist()  constructor
##  conditem()  constructor
##  condlist.make()
##
## DEPENDENCIES
##  lengthen.r
##

## ---------------------------------------------------------------------------------------------------------------------—•°

condlist <- function(...){
## ------------------------------------------------------------------------------------------------—•°
## Constructor of "condlist" class.
## ------------------------------------------------------------------------------------------------—•°
ll <- list(...)
stopifnot(any(sapply(ll, function(l){"conditem" %in% class(l)})))
structure(ll, class="condlist")
}

conditem <- function(variables=NULL, conditions=NULL, inputs=NULL, substitute=NULL, subindex=NULL){
## ------------------------------------------------------------------------------------------------—•°
## Constructor of "conditem" class.
## ------------------------------------------------------------------------------------------------—•°
ll <- list(
        variables=variables,
        conditions=conditions,
        inputs=inputs,
        substitute=substitute,
        subindex=subindex
    )
ll <- ll[!sapply(ll, is.null)]
structure(
    ll,
    class="conditem")
}
## ---------------------------------------------------------------------------------------------------------------------—•°


## ---------------------------------------------------------------------------------------------------------------------
condlist.make = function(conddf, condlist.list, eval=TRUE, parse=TRUE, purify=TRUE){
## ---------------------------------------------------------------------------------------------------------------------
## (Read the help for condapply() before.)
## Creating 'condlist' on the basis of 'conddf' and 'condlist.list'.
## 'conddf' is considered to be a conscise form of 'condlist' to which it needs to be transformed
## whith an aid of 'condlist.list', before 'condlist' may serve as input for condapply().
## Each row of 'conddf' is transformed to a 'conditem' for a variable with the same name as this row.
## Thus for each row each variable of 'conddf' is turned to list element (element of a 'conditem') and this elements
## are used to construct additional elements of 'conditem' according to rules encoded in 'condlist.list'.
##
##  Arguments
## conddf         data frame to be transformed to condlist whith each row having the name of a variable in some data frame
##                having data to be transformed according to instrucrions in condlist which will be created from conddf.
##                May be also a vector of names of variables in some datfram (we wish to transform according to 'condlist').
## condlist.list  list of additional entries to each conditem which will be created on the basis of variables from conddf.
## eval = TRUE    should elements of condlist.list be evaluated (within environment constructed from each row of 'conddf')
##                before further processing? Evaluation may not be needed in some cases.
## parse =TRUE    each element of 'condlist.list' is evaluated (within environment constructed from each row of 'conddf')
##                and as such returns character or numeric;
##                then it is parsed (if parse = TRUE) or simply copied (if parse = FALSE) to an element of 'condlist'
##                (with the same name as the name of a current element of 'condlist.list') which is body(function(){...}).
##                Notice that e.g.
##                   condlist.list$conditions = "all"  with  parse = FALSE
##                gives the same result as
##                   condlist.list$conditions = "\"all\""  with  parse = TRUE (default)
##                In general 'parse' has the same length as 'condlist.list' and each element of 'parse' refers to
##                respective element of 'condlist.list'. If 'parse' is shorter then its last element is repeated to match
##                the length of 'condlist.list'.
## purify = TRUE  do clear the result from unnecesery entries? During processing all variables of 'conddf' turns to be
##                elements of each conditem (each row of 'conddf' is turned to list which is a seed of conditem).
##                But we usually need only elements used by condapply() which are:
##                  "variables", "conditions", "input", "substitute", "subindex", "logs", "logs.action", "isolata", "plot".
##                Any other element of conditem is ignored by condapply() and may be safely discarded
##                thus TRUE is default for 'purify'.
##                However we may need to retain ...
##
##  Result
## A list of class 'condlist' which elements are colled 'conditem's.
## Their structure is described in the help for condapply().
##
##  Remarks
## Very difficult to explain... Try examples below in this file and wait until some tutorial appear :)
## ---------------------------------------------------------------------------------------------------------------------

if(is.vector(conddf)){   ## vector of names of variables
   conddf = data.frame(conddf);
   rownames(conddf) = conddf[, 1] ;
   if(nrow(conddf)>0){
      conddf[, 1] = NA
   }
}

## ----------------

clist = list() ##; clist

if(nrow(conddf)>0){

   ## data.frame to list
   for(k in 1:nrow(conddf)){
      clist[[k]] = as.list(conddf[k,, drop=FALSE])
   }
   names(clist) = rownames(conddf)

   ###################

   parse = lengthen(parse, length(condlist.list))
   eval = lengthen(eval, length(condlist.list))

   ###################

   ## constructing conditions
   for(k in 1:length(clist)){
      item_k = clist[[k]]
      attach(item_k, pos=which(search()=="Condapply_AK")+1)

      for(j in 1:length(condlist.list)){
      name_j = names(condlist.list)[j]

      fun = function(){}

      if(eval[j]){
         fun.body = base::eval(condlist.list[[j]])  #paste0("x < ", item_k$min )
      }else{
         fun.body = condlist.list[[j]]  #
      }

      ##if(is.list(fun.body)){fun.body = fun.body[[1]]}  #????

      if(parse[j]){
         body(fun) <- base::parse(text = fun.body)
      }else{
         body(fun) <- fun.body
      }

      clist[[k]][[name_j]] = body(fun)
      }

      if(purify){
      clist[[k]] = clist[[k]][ names(clist[[k]]) %in% c("variables", "conditions", "inputs", "substitute", "subindex"
                                                       , "logs", "logs.action", "isolate", "plot") ]
      }

      detach(item_k)
   }
}

class(clist) <- c("condlist", "list")
clist

} ##----END----##
## ---------------------------------------------------------------------------------------------------------------------


## ---------------------------------------------------------------------------------------------------------------------
## EXAMPLES ############################################################################################################
## ---------------------------------------------------------------------------------------------------------------------

## -------------------------------------------------------------------------------------------
dummy = function(){
## This is dummy function - it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## - in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
## -------------------------------------------------------------------------------------------

## ------------------------------------
## Example 0

cl0 <- condlist(
    "ee" = conditem(  ## x
        variables = c("aa", "bb") , ## a, b, c,...
        conditions = quote(a==b) ,
        inputs = c("aa", "bb") ,    ## a, b, c,...
        substitute = quote(a==b)
        )
    , "ee" = conditem(  ## x
        variables = c("aa", "bb") , ## a, b, c,...
        conditions = quote(a!=b) ,
        inputs = c("aa", "bb") ,    ## a, b, c,...
        substitute = quote(a==b)
        )
    )
cl0




## ------------------------------------
## Example 1

conddf=data.frame( min = c(1, 1, 1, 1, 0, 0, 0, 0) , max = c(2, 3, 4, 3, 2, 1, 2, 1) , exc.min = rep(.2, 8) , exc.max = rep(.2, 8) , logs = rep(".errs", 8) )
rownames(conddf) = paste0("var_", 1:8)
conddf

condlist.list = list(
   conditions = quote( paste0("x < ", min*(1-exc.min)) )
 #, conditions2 = paste0("x < ", min*(1-exc.min))  ## to see the difference; if not working comment it!
 , subindex = "x<0"
)
condlist.list

( condlist = condlist.make(conddf, condlist.list) )


class(condlist)

## ------------------------------------
## Example 2

## ----------------
## suppose it is a main data.frame comprising some reference data (used to build some model):
( datfram.ref0=data.frame( aa = sample(0:4, 10, replace=TRUE) , bb = sample(1:5, 10, replace=TRUE)
                    , cc = sample(-4:-1, 10, replace=TRUE)
                    , dd = 2*(runif(10)-.5)
                    , ee = rnorm(10, mean=2, sd=2)
                    , ff = rpois(10, lambda=3)
                    , gg = rlnorm(10) )  )     ## the log normal distribution

 ## make a copy of original data
 datfram.ref = datfram.ref0
 ## and performing some transformations, eg.
 datfram.ref$gg = log(datfram.ref$gg)
    windows(); par(mfrow = c(2, 1)) ; plot(datfram.ref$gg) ; plot(datfram.ref$gg, datfram.ref0$gg)

 ## remembering the transformations
 trans.list = list( aa = quote(x) , bb = quote(x) , cc = quote(x) , dd = quote(x) , ee = quote(x) , ff = quote(x)
                  , gg = body(function(x){log(x)}) )
    ## notice, that
    x = 1 ; lapply(trans.list, eval)

## We expect a new data (on which prediction will be made with a model build on datfram.ref)
## will have the same ranges of values as reference data on which model was build.
## Values out of range of reference data should be treated as errors or outliers
## and substituted with NAs or some carefully chosen values.
## We sholud also check other relations between variables like 'experience' should not exceed 'age'
## or 'dpd' cannot exceed 'dpo'-30, etc.
## Moreover, all variables from datfram.new should be transformed in the same way as the same variables from reference data.

conddf = as.data.frame(t(apply(datfram.ref, 2, range))) ; names(conddf) = c("min", "max")
conddf = cbind(conddf, range = apply(conddf, 1, diff) )
conddf$exc.min = c(0, 0, 0, 1.2,.2,.5, 1.5)
conddf$exc.max = c(.5,.5, 0,.2,.2,.4, 1.5)
conddf$transform = I(trans.list)         ## this is a list !!!
conddf
## it is convienient to use  fix(conddf)

    x = 1 ; lapply(conddf$transform, eval)

## ----------------
## data new
( datfram.new0=data.frame( aa = sample(-1:5, 10, replace=TRUE) , bb = sample(1:5, 10, replace=TRUE)
                    , cc = sample(-5:0, 10, replace=TRUE)
                    , dd = 4*(runif(10)-.5)
                    , ee = 2*rnorm(10, mean=2, sd=2)
                    , ff = 2*rpois(10, lambda=3)
                    , gg = rlnorm(10) )  )

## ----------------
## 1. transformation
condlist.list.trans = list(conditions=quote("\"all\"") , substitute = quote(transform))
( condlist.trans = condlist.make(conddf, condlist.list.trans) )
   ## or
condlist.list.trans = list(conditions="all" , substitute = quote(transform))
( condlist.trans = condlist.make(conddf, condlist.list.trans, parse = c(FALSE, TRUE)) )

condapply(datfram.new0, condlist.trans[-7])                                                            #!#! ERROR !!!!!!!
   ## compare
   datfram.ref



condlist.list.min = list(
   conditions = body(function(){ paste0("x < ", min*exc.min) })   ## this will be evaluated after attaching a row of a conddf
                       ## (after transforming it to a list with names inherited from names of variables of conddf);
                       ## this will happen for each row.
                       ## The names of formal arguments used here must match the names of variables of conddf
                       ## to be evaluated to their values in a given row of conddf after attaching this row.
                       ## The whole expression is evaluated to the string which then is parsed to the functions body
                       ## ascribed to a resulting list's element (conditem) with the same name as this list element.
                       ## Remember that conditem is itself an elelment of a bigger list called condlist which is a result
                       ## of condlit.make() and serves as input for condapply().
 , substitute = body(function(){ min*exc.min })                  ## "
)
condlist.list.min

## what may be written in a shorter form
condlist.list.min = list(
   conditions = quote( paste0("x < ", min*exc.min) )
 , substitute = quote( min*exc.min )
)
condlist.list.min

( condlist.min = condlist.make(conddf, condlist.list.min) )



}

rm(dummy)
