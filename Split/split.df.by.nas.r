## ---------------------------------------------------------------------------------------------------------------------•°
## FUNCTIONS HERE
##  split.df.by.nas()
##  split.df.by.nas.first()
##  stats.df.list()
##
## DEPENDENCIES
##
## ---------------------------------------------------------------------------------------------------------------------•°
split.df.by.nas = function(datfram , variables = NULL){
## ---------------------------------------------------------------------------------------------------------------------•°
## Splitting given data frame into a number of data frames such that for each variable from 'variables'
## each data frame has only NAs or no NAs at all.
## Variables with only NAs are removed from given data frame, hence we obtain a list of data frames
## with no NAs but with different set of variables.
## Result is returned as a list of data frames.
## Elements of the list have long names in which the process of splitting and removing variables is encoded.
##
## Arguments
##  datfram     data frame
##  variables   vector of variables (as character or as numeric position in a data frame)
##              according to which the splitting process will be performed;
##              negative numbers may be used (indicating the numeric position of the variable
##              in a data frame) to exclude the variable from splitting process;
##              if NULL (default) then all variables in a data frame will be used.
##
## Values
##  List of data frames as described above.
##
## Notes
##  1. Function is intended to be the 1st step in automation of the process of prediction 
##     from the models like lm, glm, gam, etc. when new data for prediction are not complete.
##     The 2nd step is predict.df.list().
##      When new data (for which we make prediction based on the given model) has many NAs
##     then we may apply two different approaches:
##        - impute missing values for variables which have not to much of them,
##          and/or 
##          remove the whole variable(s), say X and use  
##          (*)  mod.bis = update(model,~.-X);  predict(mod.bis, new_data)
##        - split new data frame in a number of data frames such that each of them has all variables
##          only full (no NAs) or empty (only NAs); 
##          then for each data frame we properly update the model and then make prediction
##          as in (*).
##     The first solution somehow distort data (with unpredictible consequences) hence are not recommended.
##     The latter is most accurate -- do not impute artificial data and do not remove any;
##     although it seems to be technically difficult it becomes easy to carry out  
##     with the aim of just two functions:
##        split.df.by.nas()  (the present one) which performes the proper split of new data,
##        predict.df.list()  which for each data frame from the list (result of the former function)
##                           performes prediction from a given model updating it properly.
##     In fact it's much faster then the first method and is perfectly accurate theoretically:
##     utilizes the whole information we possses and do not imposes any assumptions or introduces artificial data,
##     i.e.  prediction is based on  true data,  only true data  and  the whole true data! 
##       
##  2. This function uses  split.df.by.nas.first().
##
##  3. Could be made in a different way to work (perhaps!) faster: use apply() - see a draft in a dummy() below.
##  Some day later... 
##
## ---------------------------------------------------------------------------------------------------------------------•°


df.name = deparse(substitute(datfram))

list.df = split.df.by.nas.first(datfram, variables, no.split.name = df.name)
if(length(list.df)==1){
  cat("There's nothing to do with given data frame: \n
  there's simply no NA values in it according to which split could be made.")
}else{
    
    control = TRUE
    
    while( control ){
      list.0  = list()
      control = FALSE
      for( k in 1:length(list.df) ){
          #  cat("\n"); print(k)
          if( length(grep('END$', names(list.df[k])))==0 ){
      
              list.df_k = split.df.by.nas.first( list.df[[k]] , variables , no.na.list = FALSE)
      
              control = control | !is.null(list.df_k)
      
              if( !is.null(list.df_k) ){
                  names(list.df_k) = paste( names(list.df[k]) , names(list.df_k) , sep = "|" )
              }else{
                  list.df_k = list.df[k]
                  names(list.df_k) = paste( names(list.df[k]) , "END" , sep = "|" )
              }
          }else{
              list.df_k = list.df[k]
          }
          list.0 = c(list.0, list.df_k)
      }
      
      list.df = list.0
      # cat(length(list.df))
    }
}

list.df
} ##----END----##

## ---------------------------------------------------------------------------------------------------------------------•°
stats.df.list = function( df.list , use.df.names = FALSE , message = TRUE ){
## ---------------------------------------------------------------------------------------------------------------------•°
## Basic statistics about data frames which are elements of the list 'df.list'.
## Two tables are created: one giving the number of NAs for each variable in each data frame,
## the second table is incidence table, where 1 indicates that a variable is in a data frame,
## while 0 appears when it is not a case.
## Arguments
##  df.list         list of data frames
##  use.df.names    if FALSE (default) then in both tables each data frame is referred to
##                  by its numeric position on the list instead of using its name
##                  (which may be very long as in the output of split.df.by.nas() );
##                  if TRUE then names of data frames are used;
##  message         if TRUE (default) a table of NAs for each variable in each data frame
##                  is printed on the screen;
##                  for variables which are not in the given data frame dot is printed;
##                  if FALSE then no message appear;
##
## Result
##  Result is retuened to the list containg two tables:
##    NAs.table         table of number of NAs in each variable in each data frame from the list;
##                      if the function is applied to the df.list which is the result of split.df.by.nas()
##                      then there should be only zeros.
##                      obviously NAs appear for these variables which are not present in relevant data frame.
##    var.names.table   incidence of variables for each data frame and variable.
##
## ---------------------------------------------------------------------------------------------------------------------•°

var.names.df = data.frame( var_name = character(0) , data_frame = numeric(0) , NAs = numeric(0) )

ndf = length(df.list)

nobs = numeric(0)

for( k in 1:length(df.list) ){
    var.names.df  =
    rbind(  var.names.df
          , data.frame(   var_name = var_name <- names( df.list[[k]] )
                        , data_frame = rep( k , length(var_name) )
                        , NAs = apply( df.list[[k]] , 2 , function(x)sum(is.na(x)) )    ## nr of NAs for each variable within each data frame
                      )
         )

    nobs = c(nobs, nrow(df.list[[k]]))
}

## ----------------

if(use.df.names){
  df.names = names(df.list)
  var.names.df$data_frame = df.names[var.names.df$data_frame]
  names(nobs) = df.names
}

## ----------------

var.names.table =
    with(   var.names.df
          , table(var_name, data_frame)
    )
var.names.table = rbind( "(nr of observations)" = nobs ,  var.names.table )
  row.sums = rowSums(var.names.table)    ## nr of data frames in which the variable is present ; the first value is the number of all observations
  # nr = length(row.sums)
var.names.table = cbind( "sum" = row.sums , var.names.table )
  row.sums.ord = c( 1 , order(row.sums[-1]) + 1 )
var.names.table =  var.names.table[row.sums.ord,]


## ----------------

NAs.table =
    with(   var.names.df
          , tapply( NAs , list(var_name, data_frame) , function(x)sum(as.numeric(as.character(x)), na.rm=TRUE) )
                                                      ## nr of NAs for each variable within each data frame;
                                                      ## if the variable is not in a data frame then NA appears
    )
  nr_of_variables = c(nrow(NAs.table), NA, colSums(apply(NAs.table, 2, function(x)as.numeric(!is.na(x)))))
NAs.table = rbind( "(nr of observations)" = nobs ,  NAs.table  )
NAs.table = NAs.table[row.sums.ord,]
NAs.table = cbind( "NAs_sum" = c(NA, rowSums(NAs.table, na.rm=TRUE)[-1]) , NAs.table )

NAs.table = cbind( "nr_of_df" = var.names.table[, 1] , NAs.table )
NAs.table = rbind( "(nr of variables)" = nr_of_variables ,  NAs.table  )

colnames(NAs.table)[1:2] = c("nr_of_df", "NAs_sum")

## ----------------

if(message){
    cat("NAs statistics\n")
      print( NAs.table , na.print = ".")
       cat("\n")
#    cat("Variables incidence\n")
#      print( as.table(var.names.table) , zero.print = ".")
#       cat("\n")
}

## ----------------

stat.list = list( NAs.table = NAs.table , var.names.table = var.names.table )
#stat.list
} ##----END----##



## ---------------------------------------------------------------------------------------------------------------------•°
split.df.by.nas.first = function( datfram , variables = NULL  , no.na.list = TRUE, no.split.name = NULL ){
## ---------------------------------------------------------------------------------------------------------------------•°
## Splitting given data frame into two data frames in the following manner:
##  1. searching for a variable (from 'variables') having any NA value;
##     searching finnishes when any such variable is found;
##  2. splitting data frame into two parts: one having no NAs in this variable and second having only NAs;
##     for the latter the variable is removed;
## Result is returned as a list of two data frames if any split was made, 
## or as a list of one data frame if no split was made (in case there are no NA values in data frame). 
##
## Arguments
##  datfram       data frame to be split
##  variables     vector of variables (as character or as numeric position in a data frame)
##                according to which the splitting process will be performed;
##                negative numbers may be used (indicating the numeric position of the variable
##                in a data frame) to exclude the variable from splitting process;
##                if NULL (default) then all the variables will be used.
##  no.na.list    if TRUE (default) then in case of no split (no NA values in data frame)
##                the result will be the list with one element containing a data frame 'datfram'
##                if FALSE then the result will be NULL.               
##  no.split.name   names to  be given to the only one element of the resulting list of data frames
##                in case no split was made (no NA values in data frame);
##                if NULL (dafault) then the name will be set automatically.
##
## Values
##  List of data frames as described above. 
##  In case when no split was made and no.na.list = FALSE the result is NULL.
##  
## Notes
##  This function is used in a loop in split.df.by.nas().
##
## ---------------------------------------------------------------------------------------------------------------------•°

if(is.null(variables)) variables = names(datfram)

if(is.numeric(variables)){
  variables = names(datfram)[variables]
  variables = variables[!is.na(variables)]
}else{
  variables = variables[variables%in%names(datfram)]
}


if( length(variables)>0 ){

  k = 1; nam = variables[k]
  while( !is.na(nam) && sum( is.na(datfram[, nam]) )==0 ){
    # print(k)
    k = k+1
    nam = variables[k]
  }

  if(!is.na(nam)){
    df.list = split( datfram , is.na(datfram[, nam]) )
      kk = which( names(datfram)==nam )
    df.list$'TRUE' = df.list$'TRUE'[, -kk, drop=FALSE]
    
    if( length(df.list)>1 ){
       names(df.list) = paste(nam, c("OK", "NA"), sep="_")
    }else{
       names(df.list) = paste(nam, "NA", sep="_")
    }

  }else{
    if(no.na.list){  ## create list of length 1 in case of no split was performed? (i.e. there was no NA in the data frame)
      df.list   = list(datfram)
      names(df.list) = ifelse( is.null(no.split.name) , deparse(substitute(datfram)) , no.split.name[1] )
    }else{
      df.list = NULL
    }
  }
  
}else{ ## variables passed do not fit variables from data frame

    if(no.na.list){  ## create list of length 1 in case of no slpit was performed? (ie. there was no NA in the data frame)
      df.list   = list(datfram)
      names(df.list) = ifelse( is.null(no.split.name) , deparse(substitute(datfram)) , no.split.name[1] )
    }else{
      df.list = NULL
    }
    warning("Variables names (or numbers) given in 'variables' are not present in given data frame.
  No split made.")
}

df.list

} ##----END----##
## ---------------------------------------------------------------------------------------------------------------------•°


## ---------------------------------------------------------------------------------------------------------------------•°
## EXAMPLES ############################################################################################################•°
## ---------------------------------------------------------------------------------------------------------------------•°

## -------------------------------------------------------------------------------------------•°
dummy = function(){
## This is dummy function - it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## - in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
## -------------------------------------------------------------------------------------------•°

nulls.table(data.new)
lista1 = split.df.by.nas.first(data.new[, 1:4], no.split.name="barabara")
str(lista1)
lista2 = split.df.by.nas.first(data.new, 1:4, no.split.name="barabara")
str(lista2)
lista22 = split.df.by.nas.first(data.new, 1:4, no.split.name="barabara", no.na.list = FALSE)
str(lista22)
lista3 = split.df.by.nas.first(data.new, no.split.name="barabara")
str(lista3)
lista4 = split.df.by.nas.first(data.new,-5, no.split.name="barabara")
str(lista4)


lista0 = split.df.by.nas(data.new)
str(lista0)
for(k in 1:length(lista0)){    print(nulls.table(lista0[[k]])$NAs)  }

grep('END$', names(lista0))
length(grep('END$', names(data.new)))

## ------------
## a draft of using apply()  for the problem

dim(apply( data.new, 2,
    function(x){
        z <- ifelse(
            is.na(x),
            paste0(deparse(substitute(x)) , "_NA" ),
            paste0(deparse(substitute(x)) , "_OK" )
            )
    }
)   )

}
## ---------------------------------------------------------------------------------------------------------------------•°
