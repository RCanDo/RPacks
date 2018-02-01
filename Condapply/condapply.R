########################################################################################################################—•°
## FUNCTIONS HERE
##  condapply()
##
## DEPENDENCIES
##  update.df.R
##    update.df() , ...
##  plot.variable.R
##  lengthen.R
##
########################################################################################################################—•°

########################################################################################################################•°
condapply = function( datfram , condlist ,  isolate = TRUE , na.as = FALSE , logs = NULL , logs.action = "append", logs.join = "left", return.all = TRUE  , plot = FALSE , ... ){
########################################################################################################################—•°
## Returns all the 'datfram' with variables mentioned in names(condlist) recalculated or substituted
## according to conditions and functions defined in 'condlist' which is a list of lists with proper structure.
## Each element of 'condlist', called 'conditem' is a list having a name of a variable in 'datfram' to be transformed
## according to directives encoded in this list's elements.
##
##    Arguments
## datfram              data frame on wich conditions will be checked and calculations will be performed according to
##                      directives encoded in 'condlist'.
##
## condlist             list with the following structure:
##                        list(
##                            vname1 = list(                    ## x  i.e. name of the variable in datfram to be checked and/or transformed;
##                                                              ##  it will be referred to by 'x' in 'conditions';
##                                      , variables = NULL      ## a,b,c,... i.e. variables (their names) of datfram to be used in calculations of 'conditions';
##                                                              ##  they will be referred to by 'a', 'b', ... in 'conditions';
##                                      , conditions = body(function(){}) ## body of a function which must return logical vector or numeric within bound 1:nrow(datfram);
##                                                              ##  it defines conditions we need to check on x and/or a, b, c,... from 'variables';
##                                                              ##  You may also use word "all" - then conditions returns only TRUEs
##                                                              ##  and the whole x will be simply replaced by a result of 'substitute';
##                                                              ##  (If x is factor "all" gives different result from 'conditions' returning
##                                                              ##  only TRUEs because the latter does not delete old factor levels,
##                                                              ##  while the "all" option replaces old factor with completely new variable.
##                                                              ##  A simple way of getting only TRUEs without "all" is e.g.
##                                                              ##  quote(rep(TRUE,length(x)))  or  quote(NA) while na.as = TRUE  in condapply() options)
##                                      , inputs = NULL         ## a,b,c,... i.e. variables (their names) of datfram to be used in calculations of values
##                                                              ##  to be substituted into x at records for which conditions returned TRUE;
##                                      , substitute = body(function(){}) ## body of a function which calculates values
##                                                              ##  to be substituted into x at records for which conditions returned TRUE;
##                                                              ##  in its definition you refer to vname1 via 'x' and variables from 'inputs' via 'a', 'b', 'c', ...
##                                      , subindex = body(function(){})   ## body of a function which must return logical vector or numeric within bound 1:nrow(datfram);
##                                                              ##  it defines records which will be used during calculating 'substitute'
##                                                              ##  with use of 'x' and variables from 'inputs' referred to via 'a', 'b', 'c', ...;
##                                                              ##  if left NULL (the most common) then returnes the same vector as conditions;
##                                                              ##  you may also use word "all" - then subindex returns only TRUEs;
##                                      , logs = NULL           ## do make logs on errors, ouliers, etc.? if so, to what data frames? details below;
##                                      , logs.action = NULL    ## in what manner logs will be done? "append", "update", "replace" (the latter two do the same);
##                                      , isolate = NULL        ## do isolate calculations from results of calculations of previous condlist elements?
##                                                              ## logical of length 1 to 3; if shorter then 3 then the last element is repeated to obtain
##                                                              ## vector of length 3;
##                                                              ## consecutive elements refer to 'conditions', 'substitute', 'subindex';
##                                      , plot = NULL           ## do make plot before and after transformation? logical of length one or two;
##                                      , ...  )        ## more options may be added later; now any element other then one of the above will be ignored;
##                            vname2 = list(
##                                      ...
##                                      ) ,
##                            ...
##                        )
##                      where vname1, vname2, ..., vnameX , ... are names of the variables in 'datfram'
##                      for which:
##                          conditions  in vnameX$conditions are checked with use of
##                          variables  stipulated in vnameX$variables.
##                      Then elements of datfram[,vnameX] satisfying these conditions are
##                          substituted  with values calculated by vnameX$substitue
##                          using the variables indicated in vnameX$inputs
##                          but only with records indicated by vnameX$subindex (by default the same records as result from
##                               vnameX$conditions what may be not sufficient when you need e.g. an aggregate overall variable,
##                               like mean(x), max(x), etc. or you need indicated records like the first, etc.).
##
##                      'logs' entry of each conditem indicates if the indices obtained from 'conditions' evaluation
##                      should be logged to some data frame called "logging data frame".
##                      Notice that 'conditions' evaluation brings logical vector of length = nrow(datfram) and this vector
##                      may be written to indicated logging data frame.
##                      Thus logging data frame is in general logical data frame which is a collection of 'conditions'
##                      evaluation results. Each column of logging data frame has a name of some variable from 'datfram'
##                      possibly with numerical suffix if some variable was processed many times.
##                      There also should be additional numeric column with a name "sum.." which summarises a number of
##                      TRUEs in each row. Having in mind that 'conditions' indicates mainly improper values of a variable
##                      (errors like e.g. negatives for age or outliers like age 110)
##                      thus the higher value of a "sum.." for a given record the worse is a quality of data for this record.
##
##                      Possible values of 'logs' entry of conditem are
##                        logs = NULL then no logs are made;
##                        logs   character vector of full names of logging data frames in .GlobalEnv;
##                                if given data frames do not exist then will be created;
##                                names biginning with a period i.e. like ".suffix" are treated as suffixes
##                                added to a name of 'datfram' hence program searches for logging data frames
##                                with names "'datfram'.suffix";
##                        logs   numeric vector indicating to which logging data frames logs should be made
##                                from the internal logging data frames list, called 'logs.names', which is constructed from
##                                names passed to 'logs' parameter of condapply() and all character positions
##                                passed to 'logs' entries of all conditems. Order of names on logs.names is natural,
##                                i.e. the first are names from 'logs' parameter of condapply() (with order preserved),
##                                followed by names from conditems in the same order. Elements of 'logs.names' are unique.
##
##                      If a variable is processed many times then consecutive logs may be written to logging data frame
##                      in two ways: you may append the new column giving it a name of a variable followed with numerical
##                      suffix (big enough) or you may overwrite the old column with the same name.
##                      'logs.action' is a parameter to set this behaviour.
##                      Possible values are described at 'logs.action' parameter of condapply().
##                      For each conditem 'logs.action' is a vector of the same length as 'logs' and if it is shorter
##                      then the last value is repeated frequently enough.
##                      If left NULL then it takes values from 'logs.action' parameter of condapply().
##
##                      See also description of 'logs' and 'logs.action' parameters of condapply();
##
## isolate = TRUE       should evaluation of 'conditions' and/or 'substitutions' and/or 'subindex' be isolated from
##                      the previous changes performed according to the 'condlist'? Logical vector of length 1 to 3.
##                      If shorter then 3 then the last value is repeated to obtain a vector of length 3.
##                      The first position is for conditions checking, the second for substitutions making
##                      and the third for 'subindex' evaluation.
##                      If the first is TRUE then any changes made to 'datfram' according to the previuos elements of 'condlist'
##                      do not influence the conditions checking according to the following elements of 'condlist'.
##                      This means that before any calculations are performed the copy of 'datfram' is made, say 'datfram0',
##                      which is not a subject to (is isolated from) any calculations defined in 'condlist'.
##								      Conditions from the consequtive elements of 'condlist' are evaluated on the values of 'datfram0'
##                      while changes are made on 'datfram'.
##                      If the first element of isolate is FALSE then there is no isolation for conditions checking
##                      thus they are evaluated on the 'datfram' and changes according to the previous elements of 'condlist'
##                      influence the result of conditions evaluation from the following elements of 'condlist'.
##                      The second element of 'isolate' concerns substitutions, the third 'subindex' evaluation
##                      (which records should be used to calculate values to substitute)
##                      and their meaning is exactly the same as for the first element.
##                      !!!
##                      Notice that isolation does not work for new variables, i.e. those which are created
##                      for names of elements of 'condlist' (of the type vnameX as described at 'condlist' above)
##                      while there are no variables with this names in 'datfram'.
##                      On the course of calculations such variables are updated in 'datfram0' at the end of processing
##                      each 'condlist' entry.
##                      This enables creatig new variables in few steps regardless of 'isolate' setup.
##                      This setup however, may influence the final result of such few-steps-construction as it obviously
##                      influence all variables present in 'datfram'.
##                      !!!
##                      'isolate' of conditem (if not null) overwrites global 'isolate' ;
##
## na.as = FALSE        in what logical values should be converted NAs which appears during evaluation of 'conditions'
##                      and/or 'subindex'; logical vector of length 1 or 2; if of length 1 then a value is repeated.
##                      Default is FALSE, hence variable will not be substituted at any record where conditions checking
##                      returns NA and records for which 'subindex' returnes NA will not take part in its evaluation.
##
## logs = NULL          If character vector then indicates names of data frames in .GlobalEnv to which
##                      logs on errors and outliers will be made.
##                      Names biginning with a period i.e. like ".suffix" are treated as suffixes added to
##                      a name of 'datfram' hence program searches for logging data frames with names "'datfram'.suffix".
##                      The internal logging data frames list, called 'logs.names', is constructed from names
##                      passed to 'logs' parameter of condapply() and all character positions passed to 'logs' entries
##                      of all conditems.
##                      Order of names on logs.names is natural, i.e. the first are names from 'logs' parameter of condapply()
##                      (with order preserved), followed by names from conditems in the same order.
##                      Elements of 'logs.names' are unique.
##                      Elements of this list may be referred to in 'logs' entries of conditems via their numeric positions
##                      which are easy to infer.
##
##                      'logs' may be also NULL or logical.
##                      If NULL then is replaced with chracter(0) thus 'logs.names' mentioned above is constructed only
##                      on the basis of character 'logs' of conditems. Using only numeric 'logs' of conditems
##                      in this case will throw an error.
##
##                      If 'logs' is FALSE then no logs ara made regardless of 'logs' entries of conditems.
##                      If TRUE then logs are made by default to the data frames with names 'datfram'.errs and 'datfram'.outs,
##                      which are created if do not exist in .GlobalEnv. It is the same as logs = c(".outs",".errs").
##
## logs.action = "append"  In what mode should the logs be made. Possible values are "append" (default), "update" or "replace"
##                      which are the options of 'action' parameter of the function update.df() by which the new columns
##                      are added to the logging data frames.
##                           "append"     default; consecutive logs are appended to the logging data frame
##                                        with a numerical suffix added to the name of a new column if needed;
##                           "update", "replace"  if a new log is on the variable already being logged to logging data frame
##                                        then it updates or replaces the old one; because of lack of NAs in 'conditions' result
##                                        and assumed processing the same data frame ('datfram')
##                                        these two actions will give here exactly the same result;
##                                        see help on 'action' of update.df();
## logs.join = "outer"  type of join in update.df() used to add new column to logging.data.frame; see help for update.df()
## return.all = TRUE    return all 'datfram' with variables transformed?
##                      If FALSE then returns only variables transformed (those listed in 'condlist' as names of its elements).
##                      IF NULL then condapply() returns only TRUE (no data frame) -- this somtimes may be useful
##                      if we only want to check conditions from 'condlist' and result write to some logging data frame
##                      -- then we spare memory.
##
## plot = FALSE         if TRUE then for each item of 'condlist' plot is drawn using function plot.variable() with options:..
## ...                  arguments passed to plot.variable()
##
##
##
##  Result
## Returns all the 'datfram' with variables from names(condlist) recalculated or substituted
## according to conditions and functions defined in 'condlist'.
## Structure of 'condlist' is described in 'Arguments' section at 'condlist' entry.
##
## Variable indicated by names(condlist), say 'x', may be a matrix. However the following condition must be satisfied:
##    • dim(x)[2] = dim(result)[2]  where the 'result' is a result of 'conditions' calculations;
##    OR
##    • is.null(dim(x)) | is.null(dim(result))  i.e. one of 'x' and 'result' should by a vector (no dimensions).
##
##
##  Advice
## Always do the copy of 'datfram' before you apply 'condlist' and return results to the same name as 'datfram'
## (i.e. if you overwrite 'datfram' with condapply() results)!
## Even better - do not overwrite 'datfram' with condapply() results. i.e. assign its results to a different name!
##
##  ToDo
## Servicing of factors -- currently you may only add or replace level, but you cannot remove any. Levels are persistent
## in the levels(variable).
##
########################################################################################################################—•°

if( length(na.as)<2 ){ na.as[2] <- na.as[1] }
if( sum(is.na(na.as <- as.logical(na.as) ))>0 ){ stop("'na.as' must have logical values or any other which may be coerced to logical.")  }

###################################################################################################—•°
## conditions.method

conditions.method = function( conditem , datfram , varname ){
## Returns logical vector indicating which elements of datfram[,varname] satisfy conditions
## given by conditem$conditions and variables indicated by conditem$variables

	x = datfram[,varname]
   if(is.null(dim(x))){ length.x = length(x)
   }else{ length.x = dim(x)[1] }
		#print(x)

  if(!is.null(conditem$variables)){

  	datfram = datfram[,conditem$variables,drop=FALSE]
  		#print(datfram)

  	p = ncol(datfram)
  		#print(p)

  	for(k in 1:p){
  		assign(letters[k],datfram[,k])
  			#print(get(letters[k]))
	  }
  }

  idx = eval(conditem$conditions)
  if(!is.null(idx)){ idx[is.na(idx)] = na.as[1] }

  if(is.null(idx)){
     idx = rep(FALSE,length = length.x)   ## empty conditions -- no records selected
  }else if(idx[1]=="all"){
     idx = rep(TRUE ,length = length.x)
  }else if(is.numeric(idx)){
     idx = 1:length.x %in% idx
     if(sum(idx)==0){warning("conditions.method returned indices out of bound 1:length(x).")}
  }else if( !(is.logical(idx) && length(idx) == length.x) ){
     stop("Condition specified for variable \"" ,varname, "\" is improper.
  Specify it again correctly, such that it returns logical vector of length nrows(datfram)
  or numeric vector of indices of x or NULL or a string \"all\".
  NULL and \"all\" will be resolved to logical vector of length nrows(datfram) with all FALSEs or TRUEs respectively.")     #"
  }
            #print(idx)
  idx     #!#!#! it is always logical!!!!
}

###################################################################################################—•°
## subindex.method

subindex.method = function( conditem , datfram , varname ){
## Returns logical vector indicating which elements/rows of datfram[,'inputs'] will be used to calculate values
## to substitute to 'varname' in place of 'conditions' index.
##

	x = datfram[,varname]
   if(is.null(dim(x))){ length.x = length(x)
   }else{ length.x = dim(x)[1] }
		#print(x)

  if(!is.null(conditem$inputs)){

  	datfram = datfram[,conditem$inputs,drop=FALSE]
  		#print(datfram)

  	p = ncol(datfram)
  		#print(p)

  	for(k in 1:p){
  		assign(letters[k],datfram[,k])
  			#print(get(letters[k]))
	  }
  }

  idx.sub = eval(conditem$subindex)
  if(!is.null(idx.sub)){ idx.sub[is.na(idx.sub)] = na.as[2] }

  if( is.null(idx.sub) ){
     idx.sub = idx               ## idx is taken from the parent environment = the same set of indices as results from conditem$conditions
  }else if( idx.sub[1]=="all" ){
     idx.sub = rep(TRUE ,length = length.x)
  }else if( is.numeric(idx.sub) ){
     idx.sub = 1:length.x %in% idx.sub
     if( sum(idx.sub)==0 ){warning("subindex.method returned indices out of bound 1:length(x).")}
  }else if( !(is.logical(idx.sub) && length(idx.sub) == length.x) ){
     stop("Condition specified for variable \"" ,varname, "\" is improper.
  Specify it again correctly, such that it returns logical vector of length nrows(datfram)
  or numeric vector of indices of x or NULL or a string \"all\".
  NULL and \"all\" will be resolved to logical vector of length nrows(datfram) with all FALSEs or TRUEs respectively.")     #"
  }
             # print(idx.sub)
  idx.sub
}


###################################################################################################—•°
## substitute.method

substitute.method = function( conditem , datfram , varname  ){

length.y = sum(idx)       ## idx  is taken from the parent environment and is logical !!!
y = datfram[,varname]     ## MUST be here
if(length.y>0 && sum(idx.sub)>0){  ## is anything to do?

   x = datfram[idx.sub,varname]     ## idx.sub  is taken from the parent environment i.e. encloser of substitute.method() which is evaluation envir of condapply()
   y_is_vector = is.null(dim(y))


   ## result ####################################•°
   if(!is.null(conditem$inputs) ){
      datfram = datfram[idx.sub,conditem$inputs,drop=FALSE]  ## idx.sub  is taken from the parent environment
   	p = ncol(datfram)
   	for(k in 1:p){ assign(letters[k],datfram[,k]) }
   }
   result = eval(conditem$substitute)  ## of  length(x) == sum(idx.sub)
   ##############################################•°

   if(is.null(result)){
      length.result = 0
   }else{
      result_is_vector = is.null(dim(result)) #; print(result_is_vector)
         #
      if( result_is_vector ){
         length.result = length(result)
         if(!y_is_vector & length.result>0){
            result <- matrix(rep(result,dim(y)[2]),ncol=dim(y)[2])
            result_is_vector = FALSE  #! important!
         }
      }else{   ## when result is matrix or df
         length.result = dim(result)[1]
         if(y_is_vector & length.result>0){
            y <- matrix(rep(y,dim(result)[2]),ncol=dim(result)[2])
            y_is_vector = FALSE  ## not in later use
         }else if( dim(y)[2]!=dim(result)[2] ){
            stop("The 2nd dimension of result differs from tha 2nd dimension of variable into which it has to be substituted.
  Adjust dimensions before calculations.")
         }
      }
   }  ## y and result are adjusted wrt to dimension (both are vectors or matrices/df with equal dim(.)[2])

   if( length.result==0 ){
     # xx = xx      ## empty substitution
   }else{
      if( length.result == 1 ){

         if(result_is_vector){
            if( !is.na(result) && result == "idx" ){   #!#!#!#!#!
               y = idx                           ## idx is taken from the parent environment
            }else if( is.factor(y) ){  
               if(!is.na(result)){  levs = union(levels(y),result) }else{ levs = levels(y) }
               z <- as.character(y)
               z[idx] = rep(result,sum(idx))     ## idx is taken from the parent environment
               y <- factor(z,ordered = is.ordered(y),levels=levs)
            }else{
               y[idx] = rep(result,sum(idx))       ## idx is taken from the parent environment
            }
         }else{ y[idx,] = result[rep(1,sum(idx)),] }

      }else if( length.result != length.y  ){
       stop("Function specified to calculate new values for variable \"" ,varname, "\" is improper.
  Specify it again correctly, such that it returns vector of length sum(idx) or of length 1 or NULL or a string \"idx\".
  NULL will result in no action, \"idx\" returns logical vector 'idx' obtained via conditem$conditions,
  results of length 1 will be replicated to match the length of sum(idx).")                										#"
      }else{
         if(result_is_vector){
            if(is.factor(y)){ #print("OK")
               if(conditem$conditions=="all"){
                  y = result           ## total replacement of y by result
               }else{
                  levs = union(levels(y),unique(result[!is.na(result)]))
                  z <- as.character(y)
                  z[idx] = result      ## idx is taken from the parent environment
                  y <- factor(z,ordered = is.ordered(y),levels=levs)
               }
            }else{
               if(conditem$conditions=="all" & is.factor(result)){
                  y = result
               }else{
                  y[idx] =  result     ## idx is taken from the parent environment
               }
            }
         }else{ y[idx,] = result }
      }
   }
}
y   #!#!#!  of length = nrows(datfram)  !!!!
}

###################################################################################################—•°
## data frames for logging on errors and outliers

logs.names.dots.method = function(logs.names){  ## logs.names must be character
## If some elements of logs.names (like logs or conditem$logs) are of the form ".suffix" then this function
## turns it into "'datfram.name'.suffix".
   ii = grep('^\\.',logs.names)
   if(length(ii)>0){
     logs.names[ii] = paste0(datfram.name,logs.names[ii])  ## datfram.name is taken from the parent.frame
   }
   logs.names
}

###################

logs.names.method = function( condlist , logs ){ ## constructing list of all needed logging data frames
## merging names of logging data frames from 'logs' parameter of condapply() and 'logs' entries of condlist

  if(is.null(logs)){logs = character(0)}

  if(is.logical(logs)){
    if(logs){
      logs = c(".errs",".outs")
    }else{
      logs.names = NULL
      logs <<- FALSE
    }
  }

  if(is.character(logs)){
    logs.names = logs
    for(conditem in condlist){   #  print(conditem)
      if(is.character(conditem$logs)){ logs.names = union(logs.names,conditem$logs) }
    }
    logs <<- TRUE
  }

  logs.names.dots.method(logs.names)

}

###################

logs.method = function( conditem , logs , varnam ){

  if(logs && !is.null(conditem$logs)){

        names(idx) = rownames(datfram)    ## idx taken from parent.frame

        if(is.null(conditem$logs.action)){
          conditem$logs.action = logs.action   ## parameter of the parent.frame
        }
        if(length(conditem$logs.action)<length(conditem$logs)){
           m = length(conditem$logs.action) ; n = length(conditem$logs)
           conditem$logs.action[(m+1):n] = conditem$logs.action[m]
        }

        conditem$logs.action = lengthen(conditem$logs.action,length(conditem$logs))

        if(is.numeric(conditem$logs)){    ## numeric to character
           if(!is.null(logs.names)){      ## from parent.frame
              conditem$logs = logs.names[conditem$logs]
              conditem$logs = conditem$logs[!is.na(conditem$logs)]
           }
           if(is.null(conditem$logs) || length(conditem$logs)==0){
              stop("If conditem$logs is numeric then its values refer to the position in 'logs' of the name of a logging data frame.
  Now it is NULL or not so long thus no log can be made.
  If 'logs' is NULL then it is automatically constructed from all conditem$logs of 'condlist' which are character
  thus if none of them is character then 'logs' remains NULL.
  Pass the list of logging data frames to 'logs' (in the form of a vector of their names)
   or adjust a demanded numeric position in conditem$logs.
  It is also strongly advised to pass the names of logging data frames to conditem$logs
   rather then their positions in a 'logs'. You may pass a few names in a vector.")
           }
        }

        if(is.character(conditem$logs)){
           conditem$logs = logs.names.dots.method(conditem$logs)
           for(l in 1:length(conditem$logs)){
              if(!exists(conditem$logs[l],where = ".GlobalEnv")){
                 assign( conditem$logs[l]
                       , {  logdf <- data.frame("sum.." = checksum) ;
                            attr(logdf,"condlist") = list()  
                            class(logdf) <- c("logging.data.frame","data.frame") ;
                            logdf
                         }
                       , pos = ".GlobalEnv"
                       )   ## checksum from parent.frame
              }else  if( !(is.data.frame(get(conditem$logs[l] , pos = ".GlobalEnv")) ) ){                  ##&& nrow(get(conditem$logs[l] , pos = ".GlobalEnv"))>=nrow(datfram)) ){  ##
                   stop( "Logs cannot be processed as \"", conditem$logs[l] ,"\" is not a data frame." )   ##" or a number of its rows differ from nrow(datfram)." ) #"
              }else{}
              logdf <- get(conditem$logs[l] , pos = ".GlobalEnv")
              if(!"sum.." %in% names(logdf)){
                   stop( "Logs cannot be made to \"",conditem$logs[l],"\" because of lack of summary variable \"sum..\"
  which properly designed logging data frame is expected to posses.
  If you really want to use this data frame as logging data frame then add this variable of class numeric prior to making logs
  or let condapply() to create logging data frame with this name automatically." )  #"
              }
              assign( conditem$logs[l]
                    , {logdf<-update.df( datfram = logdf
                                , data = idx , id = 0 , variable = varnam            ## idx taken from parent.frame
                                , action = conditem$logs.action[l]
                                , join = logs.join                                   ## parameter of the main function
                               ) ;
                               #print(logdf)
                       logdf$`sum..` <- apply(logdf[,-1,drop=FALSE],1,function(x){sum(x,na.rm=TRUE)}) ;  #logdf$`sum..` +  logdf[,varnam];
                       attr(logdf,"condlist") <- c(attr(logdf,"condlist") , condlist[k]) ;      ## k from parent.frame()
                       logdf
                      }
                    , pos = ".GlobalEnv"
                    )
           }
        }else{ stop("'logs' entry of each conditem (sublist) of 'condlist' must be character or numeric.
  Character should be a name of logging data frame, while numeric should be a position of this logging data frame
  on a list of logging data frames passed to 'logs' parameter of a function (as character vector NOT a formal list).
  Notice that it is created automatically if 'logs' is TRUE and then constists only of two data frames 'datfram'.errs and 'datfram'.outs
  which may be referenced in the condlist numerically by 1 or 2.")
        }
  }
}

########################################################################################################################—•°
## MAIN

###################################################################################################—•°
if(length(condlist)==0){  ## if(1) ################################################################—•°
   message( "'condlist' is empty. 'datfram' not changed.\n" )
}else if(nrow(datfram)==0){
   message( "'datfram' is empty. 'condlist' useless.\n" )
}else{

datfram.name = deparse(substitute(datfram))

###############################################################################—•°
## data frames for logging on errors and outliers

logs.names = logs.names.method( condlist , logs ) ## logs may be NULL or boolean or character
  ## returns character vector of logging data frames  and  changes logs to TRUE if it is passed as NULL or a character; logs remains FALSE if is passed as FALSE


if(logs){ checksum = rep(0,nrow(datfram)); names(checksum) = rownames(datfram) } ## first column for non-existing logging data frames from 'logs' list;


###############################################################################—•°

new.names = setdiff(names(condlist),names(datfram))
datfram[,new.names] = NA   ## for new variables (not present in 'datfram')

if(sum(isolate)>0){ datfram0 = datfram }   ## datfram0 will not be changed during calculations, results of calc's will be written to datfram

###############################################################################—•°
## THE LOOP

report.df=data.frame(id=numeric(0),sum=numeric(0))
for(k in 1:length(condlist)){

  #############################################################################—•°
  ## preparations

	varnam = names(condlist)[k]
	conditem = condlist[[k]]
   if(is.null(conditem$conditions)){ conditem$conditions <- "all" }

  #################
  ## plots

  if( !is.null(conditem$plot) ){ plot_k = conditem$plot
  }else if( !is.null(plot) ){ plot_k = plot
  }else{ plot_k = FALSE }
    ##
  if( !is.logical(plot_k) ){
      plot_k = FALSE
      warning("Option 'plot' was not logical and was set to FALSE.")
  }
    ##
  if(length(plot_k)==1){
    plot_k[2] <- plot_k[1]
  }


  #################
  ## isolations

  if( !is.null(conditem$isolate) ){ isolate_k = conditem$isolate
  }else if( !is.null(isolate) ){ isolate_k = isolate
  }else{ isolate_k = TRUE }
    ##
  if( !is.logical(isolate_k) ){
      isolate_k = TRUE
      warning("Option 'isolate' was not logical and was set to TRUE.")
  }
    ##
  if(length(isolate_k)==1){
    isolate_k[3] <- isolate_k[2] <- isolate_k[1]
  }else if(length(isolate_k)==2){
    isolate_k[3] <- isolate_k[2]
  }

  #############################################################################—•°
  ## conditions

	if(isolate_k[1]){ ## should conditions be isolated from the results of the former calculations (made only on original 'datfram')
		idx = conditions.method( conditem , datfram0 ,  varnam )
	}else{
		idx = conditions.method( conditem , datfram ,  varnam )
	}

  #############################################################################—•°
  ## plot 1

  if(plot_k[1]){ ## 1
                #print( sum(!is.na(datfram[,varnam])) >0 )
    if( sum(!is.na(datfram[,varnam]))>0 ){ ## varnam  may not exist still (if a conditem is intended to create new variable)
       plot.variable(datfram[,varnam],main=paste0(varnam,"  [0]"),col = (!idx)+6 , ... )
    }else{ message(" \"",varnam,"\" does not exist yet and cannot be plotted. This variable is just under construction." ) }  #"
  }
           ###print(sum(idx))

  #############################################################################—•°
  ## substitution

  N = sum(idx)
  rnam = length( grep(paste0('^',varnam,'(\\.|$)'),rownames(report.df)) )
  vnam <- if(rnam>0){paste(varnam,rnam+1,sep=".")}else{varnam}
  report.df[vnam,] = data.frame(k,N)

  if(N>0){

      cat(k," : ",N," elements of \"",varnam, "\" satisfying ",deparse(conditem$conditions),".\n\n",sep="")  #"

      #############################################################################—•°
      ## subindex

    	if(isolate_k[3]){ ## should subindex be isolated from the results of the former calculations (made only on original 'datfram')
    		idx.sub = subindex.method( conditem , datfram0 ,  varnam )
    	}else{
    		idx.sub = subindex.method( conditem , datfram ,  varnam )
    	}

      #############################################################################—•°
      ## substitution

      if(isolate_k[2]){  ## should substitution be isolated from the results of the former calculations (made only on original 'datfram')
          datfram[,varnam] = substitute.method( conditem , datfram0 , varnam  )
    	}else{
          datfram[,varnam] = substitute.method( conditem , datfram  , varnam  )
    	}

      #############################################################################
      ## if we create new variable (condlist's element name has no relevant variable in 'datfram') then isolation makes no sense
      ## thus we add consecutive results of calculations to the datfram0

      if( varnam %in% new.names && sum(isolate)>0 ){ datfram0[,varnam] = datfram[,varnam] }

      #############################################################################—•°
      ## logging on errors, outliers, etc. in specified logging data frames

      logs.method(conditem,logs,varnam)

      #############################################################################—•°
      ## plot 2

      if(plot_k[2]){ ## 2
        plot.variable(datfram[,varnam],main=paste0(varnam,"  [1]") , ... )
      }
      #############################################################################—•°
  }else{
      cat(k," : 0 elements of \"",varnam, "\" satisfying ",deparse(conditem$conditions),".\n\n",sep="")  #"
  }

} ##-- END OF THE LOOP --##

###############################################################################—•°
## finally all the datfram is returned with variables from names(condlist) changed.

print(report.df)

} ## END of if(1) #################################################################################—•°
###################################################################################################—•°

if( is.null(return.all) ){
   datfram = TRUE
}else{
   if(!return.all && length(condlist)>0){
      datfram = datfram[,names(condlist),drop=FALSE]
   }else{
      datfram
   }
}

datfram

} ##----END----##
########################################################################################################################—•°


########################################################################################################################—•°
## EXAMPLES ############################################################################################################—•°
########################################################################################################################—•°

##############################################################################################—•°
dummy = function(){
## This is dummy function - it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## — in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
##############################################################################################—•°

datfram1=data.frame( aa = c(1,1,1,1,0,0,0,0) , bb = c(1,1,0,0,1,1,0,0) , cc = rep(c(1,0),4) , dd = 2:9 )
datfram1

condlist1 = list(
"aa" = list(  ## x
variables = c("bb") , ## a,b,c,...
conditions = body(function(x,a){(x<=a)}) ,
inputs = c("dd") ,    ## a,b,c,...
substitute = body(function(a){a})
),
###
"bb" = list(  ## x
variables = c("aa") , ## a,b,c,...
conditions = body(function(x){x<=a})  ,
inputs = c("aa")        , ## a,b,c,...
substitute = body(function(a){-a-1})
)
)
class(condlist1)<-"condlist"
condlist1

datfram1
condapply(datfram1,condlist1)
## the same as
condapply(datfram1,condlist1,isolate = c(TRUE,TRUE))     ## both cond's checking and susbstitution ARE isolated — default
## but
condapply(datfram1,condlist1,isolate = c(FALSE,TRUE))    ## cond's checking NOT isolated , but susbstitution IS isolated
condapply(datfram1,condlist1,isolate = c(TRUE,FALSE))    ## cond's checking IS isolated , but susbstitution NOT isolated
condapply(datfram1,condlist1,isolate = c(FALSE,FALSE))   ## both cond's checking and susbstitution are NOT isolated

#######################################
## notice that formal arguments of functions in 'conditions' and 'substitute' are irrelevent and may be ommited;

condlist1 = list(
"aa" = list(  ## x
variables = c("bb") , ## a,b,c,...
conditions = body(function(){(x<=a)}) ,
inputs = c("dd") ,    ## a,b,c,...
substitute = body(function(){a})
),
###
"bb" = list(  ## x
variables = c("aa") , ## a,b,c,...
conditions = body(function(){x<=a})  ,
inputs = c("aa")        , ## a,b,c,...
substitute = body(function(){-a-1})
)
)
condlist1

datfram1
condapply(datfram1,condlist1)

#######################################
## you don't even need to use body(function(){..}) syntax -- it's enough to use quote(..)
condlist1 = list(
"aa" = list(  ## x
variables = c("bb") , ## a,b,c,...
conditions = quote(x<=a) ,
inputs = c("dd") ,    ## a,b,c,...
substitute = quote(a)
),
###
"bb" = list(  ## x
variables = c("aa") , ## a,b,c,...
conditions = quote(x<=a)  ,
inputs = c("aa")        , ## a,b,c,...
substitute = quote(-a-1)
)
)
condlist1

datfram1
condapply(datfram1,condlist1)

######################################
## you may also create a new variable
condlist2 = list(
"ee" = list(  ## x
variables = c("aa","bb") , ## a,b,c,...
conditions = body(function(){(a==b)}) ,
inputs = c("dd") ,    ## a,b,c,...
substitute = body(function(){a})
)
)
condlist2

condapply(datfram1,condlist2)  ## it's OK, only those values were substituted for which condition returned TRUE

## hence it may be better to write the second condition
condlist3 = list(
"ee" = list(  ## x
variables = c("aa","bb") , ## a,b,c,...
conditions = body(function(){(a==b)}) ,
inputs = c("dd") ,    ## a,b,c,...
substitute = body(function(){a})
)
,"ee" = list(  ## x
variables = c("aa","bb") , ## a,b,c,...
conditions = body(function(){(a!=b)}) ,
inputs = c("aa","bb") ,    ## a,b,c,...
substitute = body(function(){b})
)
)
condlist3

condapply(datfram1,condlist3)

## returning condition check
condlist4 = list(
"ee" = list(  ## x
variables = c("aa","bb") , ## a,b,c,...
conditions = body(function(){(a==b)}) ,
inputs = c("aa","bb") ,    ## a,b,c,...
substitute = body(function(){a==b})
)
,"ee" = list(  ## x
variables = c("aa","bb") , ## a,b,c,...
conditions = body(function(){(a!=b)}) ,
inputs = c("aa","bb") ,    ## a,b,c,...
substitute = body(function(){a==b})
)
)
condlist4

condapply(datfram1,condlist4)  ##


## the same using special word "idx" in substitute function
condlist5 = list(
"ee" = list(  ## x
variables = c("aa","bb") , ## a,b,c,...
conditions = body(function(){(a==b)}) ,
inputs = NULL,    ## a,b,c,...
substitute = body(function(){"idx"})
)
)
condlist5

condapply(datfram1,condlist5)  ##


condlist6 = list(
"ee" = list(  ## x
variables = c("aa","cc") , ## a,b,c,...
conditions = body(function(){(a==b)}) ,
inputs = c("dd"),    ## a,b,c,...
substitute = body(function(){a})
)
)
condlist6

condapply(datfram1,condlist6)  ##


condlist7 = list(
"ee" = list(  ## x
variables = c("aa","cc") , ## a,b,c,...
conditions = body(function(){(a==b)}) ,
inputs = c("dd"),    ## a,b,c,...
substitute = body(function(){a})
)
,"ee" = list(  ## x
variables = c("aa","cc") , ## a,b,c,...
conditions = body(function(){(a!=b)}) ,
inputs = NULL,    ## a,b,c,...
substitute = body(function(){-10})
)
)
condlist7

condapply(datfram1,condlist7)  ##

## or little simpler
condlist7b = list(
"ee" = list(  ## x
variables = c("aa","cc") , ## a,b,c,...
conditions = body(function(){"all"}) ,  ## "all"  means all elements of a variable
inputs = c("aa","cc","dd"),    ## a,b,c,...
substitute = body(function(){ifelse(a==b,c,-10)})
)
)
condlist7b

condapply(datfram1,condlist7b)  ##

#############################
## using condlist$subindex -- which elements of condlist$inputs will be used to calculate result
datfram2 = datfram1
datfram2$dd[c(1,2)] = NA ; datfram2

condlist8 = list(
"dd" = list(  ## x
conditions = body(function(){is.na(x)}) ,
substitute = body(function(){max(x)})   ## condlist$subindex is not specified thus all x will be used to calculate result
                                        ## but there are NA thus we get NA what is not what we need
)
)
condlist8

condapply(datfram2,condlist8)  ## doesn't work!

## you must use condlist$subindex
condlist9 = list(
"dd" = list(  ## x
conditions = body(function(){is.na(x)}) ,
substitute = body(function(){max(x)})  ,
subindex = body(function(){!is.na(x)})    ## we need only non NA values
)
)
condlist9

condapply(datfram2,condlist9)  ## OK!

## or little simpler

condlist9 = list(
"dd" = list(  ## x
conditions = body(function(){is.na(x)}) ,
substitute = body(function(){max(x)})  ,
subindex = body(function(){!idx})
)
)
condlist9

condapply(datfram2,condlist9)  ## OK!

## or, e.g.

condlist9 = list(
"dd" = list(  ## x
conditions = body(function(){is.na(x)}) ,
substitute = body(function(){x}) ,
subindex = body(function(){3})   ## in places of NA substitute the 3d element of x
)
)
condlist9

condapply(datfram2,condlist9)  ## OK!

## or, e.g.

condlist9 = list(
"dd" = list(  ## x
conditions = body(function(){is.na(x)}) ,
inputs = "cc" ,
substitute = body(function(){a})  ,
subindex = body(function(){c(rep(FALSE,6),TRUE,TRUE)})
)
)
condlist9

condapply(datfram2,condlist9)  ## OK!

## or
condlist10 = list(
"dd" = list(  ## x
conditions = body(function(){is.na(x)}) ,
inputs = "cc" ,
substitute = body(function(){a})  ,
subindex = body(function(){7:8})
)
)
condlist10

condapply(datfram2,condlist10)  ## OK!

## or
condlist11 = list(
"dd" = list(  ## x
conditions = body(function(){1:2}) ,
inputs = "cc" ,
substitute = body(function(){a})  ,
subindex = body(function(){7:8})
)
)
condlist11

condapply(datfram2,condlist11)

## or
condlist12 = list(
"dd" = list(  ## x
conditions = body(function(){0}) ,
inputs = "cc" ,
substitute = body(function(){a})  ,
subindex = body(function(){11})
)
)
condlist12

condapply(datfram2,condlist12)

#!#!#! MANY MORE POSSIBILITIES... -- very difficult to test them all!

###############################################################################—•°
#! HOW DOES IT WORK WITH  factors ????

###############################################################################—•°
## 'variable' and/or 'substitute'  are matrices
datfram3 = datfram1
mm = matrix(c(1:8,sample(11:18)),ncol=2)
datfram3[,"mm"] = mm
datfram3

############
## replacing

condlist31 = list(  "mm" = list( variables = c()
                             , conditions = "all"  ## "all" means the whole variable
                             , inputs = c("aa","bb")
                             , substitute = quote(cbind(a,b))
)
)

condapply(datfram3,condlist31)


condlist32 = list(  "mm" = list( variables = c()
                             , conditions = quote(x[,1] == x[,2]-10)  ##
                             , inputs = c()
                             , substitute = quote(cbind(x[,2],x[,1]))
)
)

condapply(datfram3,condlist32)

## if you try to replace each row of 'mm' with one value ...
condlist33 = list(  "mm" = list( variables = c()
                             , conditions = "all"  ## "all" means the whole variable
                             , inputs = c("aa")
                             , substitute = quote(a)
)
)

condapply(datfram3,condlist33)
## then this value is replicated

## you may simply put NA where some conditions hold (but it will affect the whole records!!!)
condlist34 = list(  "mm" = list( variables = c()
                             , conditions = quote(x[,1] == x[,2]-10)  ##
                             , inputs = c("aa")
                             , substitute = quote(NA)
)
)

condapply(datfram3,condlist34)


## however, NOTICE that you cannot exceed ncols(x) in the 'substitute' output
condlist34 = list(  "mm" = list( variables = c()
                             , conditions = quote(x[,1] == x[,2]-10)  ##
                             , inputs = c("aa")
                             , substitute = quote(cbind(a,a,a))
)
)

condapply(datfram3,condlist34)

###################
## new variable

condlist41 = list(  "nnn" = list( variables = c()
                             , conditions = "all"
                             , inputs = c("aa","bb","mm")
                             , substitute = quote(cbind(a,b,c[,2]))
)
)

condapply(datfram3,condlist41)

##
condlist42 = list(  "nnn" = list( variables = c("mm")
                             , conditions = quote(a[,1] == a[,2]-10)
                             , inputs = c("aa","bb","mm")
                             , substitute = quote(cbind(a,b,c[,2]))
)
)

condapply(datfram3,condlist42)

## letters
condlist43 = list(  "nn" = list( variables = c("mm")
                             , conditions = quote(a[,1] == a[,2]-10)
                             , inputs = c()
                             , substitute = quote(cbind("a","b"))
)
)

condapply(datfram3,condlist43)


###############################################################################—•°
###############################################################################—•°
## USING 'logs'

( datfram=data.frame( aa = sample(0:4,8,replace=TRUE) , bb = sample(1:5,8,replace=TRUE) , cc = sample(-4:-1,8,replace=TRUE) , dd = 2*(runif(8)-.5) ) )

## Using 'logs' in condlist of the type ".suffix", eg.
condlist1 = list(
"aa" = list(  ## x
variables = c("bb") , ## a,b,c,...
conditions = body(function(x,a){(x>=a)}) ,
inputs = c("bb") ,    ## a,b,c,...
substitute = body(function(a){NA}) ,
logs = ".errs"           ## logging to datfram.errs - will be created if not exists
),
###
"dd" = list(  ## x
variables = NULL , ## a,b,c,...
conditions = body(function(x){x<=mean(x)})  ,
inputs = NULL        , ## a,b,c,...
substitute = body(function(){0})  ,
logs = ".outs"          ## logging to datfram.outs - will be created if not exists
)
)
condlist1

ls()
datfram
condapply(datfram , condlist1 )  ## 'logs' parameter of condapply() is NULL
datfram.errs                    ## but proper data frames are created
datfram.outs

## If you want to use  logging data frames from the 'logs' list of condapply() they must be referenced in condlist
## by their full name or their position on the 'logs' list of condapply()
ls()
condapply( datfram ,condlist1, logs = c("df.errs","df.outs") )
  df.errs       ## do not exists as they were not indicated in 'logs' entry of condlist1, thus were not created
  df.outs
datfram.errs    ## new columns added (the same for the second time)
datfram.outs

## THUS condlist must be modified, eg.
condlist2 = list(
"aa" = list(  ## x
variables = c("bb") , ## a,b,c,...
conditions = body(function(x,a){(x>=a)}) ,
inputs = c("bb") ,    ## a,b,c,...
substitute = body(function(a){NA}) ,
logs = 1           ## logging to the first data frame on the 'logs' list of condapply()
),
###
"dd" = list(  ## x
variables = NULL , ## a,b,c,...
conditions = body(function(x){x<=mean(x)})  ,
inputs = NULL        , ## a,b,c,...
substitute = body(function(){0})  ,
logs = 2          ## logging to the second data frame on the 'logs' list of condapply()
)
)
condlist2

ls()
condapply( datfram ,condlist2, logs = c("df.errs","df.outs") )
  df.errs       ## created on the fly
  df.outs
datfram.errs    ## no new columns added
datfram.outs

## or by the full name
condlist3 = list(
"aa" = list(  ## x
variables = c("bb") , ## a,b,c,...
conditions = body(function(x,a){(x>=a)}) ,
inputs = c("bb") ,    ## a,b,c,...
substitute = body(function(a){NA}) ,
logs = "df.errs"           ## logging to datfram.errs - will be created if not exists
),
###
"dd" = list(  ## x
variables = NULL , ## a,b,c,...
conditions = body(function(x){x<=mean(x)})  ,
inputs = NULL        , ## a,b,c,...
substitute = body(function(){0})  ,
logs = "df.outs"          ## logging to datfram.outs - will be created if not exists
)
)
condlist3

ls()
condapply( datfram , condlist3 ) ## 'logs' here may stay NULL if 'logs' entries of condlist are all full names
  df.errs       ## new columns added (the same for the second time)
  df.outs
datfram.errs    ## no new columns added
datfram.outs

rm(  df.errs , df.outs , datfram.errs , datfram.outs )

## you may also switch off logging regardless of values of 'logs' entries of condlist
condapply( datfram , condlist1, logs = FALSE )
datfram.errs    ## not exists or no new columns added
datfram.outs

condapply( datfram ,condlist2, logs = FALSE )
datfram.errs    ## not exists or no new columns added
datfram.outs

condapply( datfram ,condlist3, logs = FALSE )
  df.errs       ## not exists or no new columns added
  df.outs

rm(  df.errs , df.outs , datfram.errs , datfram.outs )
ls()

###############################################################################—•°
## Summarising:
## There is 'logs' parameter of the condapply() function which is by default NULL
## and there is 'logs' entry in each conditem of condlist.
## A. 'logs' of condapply() may be NULL (defalut) , TRUE, FALSE or character (vector with names of logging data frames);
##    It generally indicates if the logs should be made AT ALL and if so then to which logging data frames.
##   1. If FALSE then no logs will be made regardless of values of 'logs' of conditems of condlist.
##   2. TRUE is effectively the same as c(".errs",".outs") i.e. special case of character vector
##      -- the meaning is: do the logs to the standard logging data frames 'datfram'.errs and 'datfram'.outs
##         which will be created if do not exist yet.
##   3. If 'logs' is character it means it is a vector with names of logging data frames.
##      These may be full names of data frames (which do not have to exist) or names of the type ".suffix"
##       which then are internally changed to names of the form 'datfram'.suffix (see e.g. p.2. above).
#!      The vector of names of logging data frames is internally called  "logs.names".
##      It is extended with names of logging data frames read from 'logs' parameters of all conditems
##       of condlist which are characters;
##       this implies that charachter(0) or even c() may be passed (but then at least one 'logs' entry of condlist must
##       be character - a name of logging data frame - or all of them must be NULL).
##      Elements of logs.names may be referred to (in 'logs' entry of each conditem) via its numeric position in it
##       and this is the only purpose of creating logs.names (through characater 'logs').
##   4. If NULL (default) then it is internally changed to character(0).
##       Then logs.names will be created only of those names which will appear in the 'logs' of conditems of condlist,
##       thus if all of them are numeric (see B.2) then logs.names will remain character(0)
##       and an error occur during processing.
##
## B. 'logs' of conditem may be NULL (defalut) , numeric or character (vector with names of logging data frames);
##    It indicates if the logs for the given conditem will be made and to which data frame.
##   1. If NULL (default) no logs will be made for the given conditem.
##   2. If numeric then it refers to this element of logs.names (see A.3) which is a name of some loggging data frame.
##      Thus if all 'logs' in condlist are numeric and 'logs' of condapply() is NULL or character(0) then
##       an error will be thrown as in this case logs.names will remain empty (see A.4).
##   3. If character then it is the name of logging data frame to which log will be made on the given conditem.
##      If such a data frame does not exist in .GlobalEnv then it will be created.
##      If there is an object within .GlobalEnv with the same name but which is not a data frame with the proper number of rows
##       then an error will be thrown.
##      Logs based on this instruction will not be made only if 'logs' of condapply() is set to FALSE.
##
##
##  logs =                  | condapply()                   |
##                          | NULL  | names | TRUE  | FALSE |
##  ---------------------------------------------------------
##  conditem    NULL        |   X   |   X   |   X   |   X   |
##  of        -----------------------------------------------
##  condlist    numeric     |  (1)  |  (2)  |  (3)  |   X   |
##            -----------------------------------------------
##              names       |  (4)  |  (5)  |  (6)  |   X   |
##  ---------------------------------------------------------
##

rm(  df.errs , df.outs , datfram.errs , datfram.outs )
ls()


condlist4 = list(
"aa" = list(  ## x
variables = c("bb") , ## a,b,c,...
conditions = body(function(x,a){(x>=a)}) ,
substitute = body(function(){NA}) ,
logs = ".outs"           ## logging to datfram.errs - will be created if not exists
),
###
"dd" = list(  ## x
conditions = body(function(x){x<=mean(x)})  ,
substitute = body(function(){0})  ,
logs = ".outs"          ## logging to datfram.outs - will be created if not exists
)
)
condlist4

condapply( datfram ,condlist4)
datfram.outs


( datfram2=data.frame( aa = sample(0:4,8,replace=TRUE) , bb = sample(1:5,8,replace=TRUE) , cc = sample(-4:-1,8,replace=TRUE) , dd = 2*(runif(8)-.5) ) )
## the same condlist but different datfram, however with variables having the same names
condapply( datfram2 ,condlist4 )
datfram2.outs


rm( datfram.errs , datfram.outs )

###############################################################################—•°
## You may make logs on the one conditem to many logging data frames

ls()

condlist5 = list(
"aa" = list(  ## x
variables = c("bb") , ## a,b,c,...
conditions = body(function(x,a){(x>=a)}) ,
substitute = body(function(){NA}) ,
logs = c(".outs" , "df.outs")          ## logging to datfram.errs - will be created if not exists
),
###
"dd" = list(  ## x
conditions = body(function(x){x<=mean(x)})  ,
substitute = body(function(){0})  ,
logs = ".errs"         ## logging to datfram.outs - will be created if not exists
)
)
condlist5

condapply( datfram ,condlist5 )
datfram.outs
df.outs
datfram.errs

## and again

condlist6 = list(
"aa" = list(  ## x
variables = c("bb") , ## a,b,c,...
conditions = body(function(x,a){(x>=a)}) ,
substitute = body(function(){NA}) ,
logs = c(1,2)          ## logging to datfram.errs - will be created if not exists
),
###
"dd" = list(  ## x
conditions = body(function(x){x<=mean(x)})  ,
substitute = body(function(){0})  ,
logs = c(2,3)        ## logging to datfram.outs - will be created if not exists
)
)
condlist6

## in thhis case names of logging data frames must be passed directly to the condapply()
condapply( datfram , condlist6 , logs = c(".outs","df.outs",".errs"))
datfram.outs
df.outs
datfram.errs

###############################################################################—•°
##  Notice that logs are made with the use of the function update.df() which has parameter 'action' with possible values
##  "append" (default), "update", "replace".
##  This parameter may be set globally from condapply() via 'logs.action' parameter of the function
##  or via 'logs.action' entry of the conditem of the condlist when it applies only to the given conditem.
##
##  'logs.action' parameter of the function is by default "append" thus new conditem on the same variable
##  will be logged to with numeric prefix to not overwrite already existing logs on this variable.
##  Setting 'logs.action' to "update" or "replace" will result in replacing the old logs on the same variables.
##
##  'logs.action' entry of the conditem of the condlist may stay NULL and then its value is taken from
##  the 'logs.action' parameter of the function.
##  It must be a character vector (with possible values "append", "update" or "replace")
##  which is lengthend if necessery to the length of 'logs' by replicating the last element (not by recyclig like in R in general).
##  k-th element of 'logs.action' is applied to k-th element of 'logs'.
##
##  You may also set a 'logs.action' parameter of the function as a character vector but notice that its sequence applies
##  locally i.e. to each conditem separately (not to any global list of logging data frames like logs.names),
##  thus using it that way may be difficult to manage.

( datfram=data.frame( aa = sample(0:4,8,replace=TRUE) , bb = sample(1:5,8,replace=TRUE) , cc = sample(-4:-1,8,replace=TRUE) , dd = 2*(runif(8)-.5) ) )

condlist7 = list(
"aa" = list(  ## x
variables = c("bb") , ## a,b,c,...
conditions = body(function(x,a){(x>=a)}) ,
substitute = body(function(){NA}) ,
logs = c(1,2) ,         ## logging to datfram.errs - will be created if not exists
logs.action = c("replace","append")
),
###
"dd" = list(  ## x
conditions = body(function(x){x<=mean(x)})  ,
substitute = body(function(){0})  ,
logs = c(2,3) ,       ## logging to datfram.outs - will be created if not exists
logs.action = "replace"
)
)
condlist7

condapply( datfram ,condlist7, logs = c(".outs","df.outs",".errs"))
datfram.outs
df.outs
datfram.errs

###############################################################################—•°
## testing on 'sum..'
datfram1 = data.frame(aa = 10*rnorm(100),bb = sample(1:100,replace=TRUE),cc = 10*rnorm(100),dd = sample(1:100,replace=TRUE))
datfram1

condlist1 = list(
  aa = list(
      conditions = body(function(){x < -19})
    , substitute = body(function(){-19})
    , logs = 1
  )
, bb = list(
      conditions = body(function(){x<10})
    , substitute = body(function(){10})
    , logs = 1
  )
, cc = list(
      conditions = body(function(){x < -19})
    , substitute = body(function(){-19})
    , logs = 1
  )
, dd = list(
      conditions = body(function(){x<11})
    , substitute = body(function(){11})
    , logs = 1
  )
)

rm(datfram1.outs)
condapply(datfram1,condlist1,logs=".outs")

datfram1.outs

condlist2 = list(
  aa = list(
      conditions = body(function(){x > 19})
    , substitute = body(function(){19})
    , logs = 1
  )
, bb = list(
      conditions = body(function(){x>90})
    , substitute = body(function(){90})
    , logs = 1
  )
, cc = list(
      conditions = body(function(){x > 19})
    , substitute = body(function(){19})
    , logs = 1
  )
, dd = list(
      conditions = body(function(){x>89})
    , substitute = body(function(){89})
    , logs = 1
  )
)

condapply(datfram1,condlist2,logs=".outs")

datfram1.outs

}
########################################################################################################################—•°
rm(dummy)
