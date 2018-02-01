########################################################################################################################
## FUNCTTIONS HERE
##  update.binary.df()
##
## DEPENDENCIES
## update.df
##  update.df()
## addid.r
##  names2id() with alias   addid2df()
##  var2vec()  with alias   addid2vec()
########################################################################################################################

########################################################################################################################
########################################################################################################################

update.binary.df = function( datfram
                        , data
                        , id = NULL
                        , which = 1  ## ONLY 1 variable !!!
                        , action = "append" ##, "replace" , "update"
                        , op = NULL  ## "and" , "or"
                        , join = "left"     ##
                        ){
############################################
##  This is update.df() function adjusted and modifiad to update binary values.
## CAUTION:
##  Take care of two basic differences:
##  1. 'which' must be always specified and indicates the only one variable in 'datfram' to be updated.
##    If 'data' is data frame then variable with name 'which' must also exist in 'datfram' and will be used
##    to update its counterpart in 'datfram'.
##    If 'data' is vector then this vector will be used to update 'which' in 'datfram'.
##    Notice that 'which' in update.df() indicates variables to be choosed from 'data' and may be many of them or left NULL.
##  2. 'op' indicates binary logical operator to be used while updating 'datfram'$'which'
##    using 'data'$'which' (if 'data' is a data frame) or simply 'data' (if this is a vector).
##
##
############################################
## variable MUST be specified for this function and only one!
##

names.datfram = names(datfram)



if(!is.null(which)){

  if( is.numeric(which)){
    which = names.datfram[which[1]]
  }else if( is.character(which) ){
    which = which[1]
  }else{
    stop("The value of 'variable' must be numeric position of variable to update or its name passed as a string.")
  }

}else{

  stop("Specify variable to update: by name or by numeric position in 'datfram'.")

}

if(is.data.frame(data)){
  if(!which%in%names(data)){
    stop("There is no variable with name \"",which,"\" in data frame 'data'.")   #"
  }else{
    data = data[,which]
  }
}

var.in = FALSE

k = which( names.datfram == which )
if( length(k)>0 ){
  var.in = TRUE
}
                     print(which)
############################################
##

datfram = update.df( datfram = datfram
                     , data = data
                     , id = id
                     , variable = which
                     , action   = "append"
                     , join = join
                     )
                      print(datfram)

names.datfram = names(datfram)
p = length(datfram)
  ##variable.new = names.datfram[p] ## not used


if(var.in){
  if(is.null(op)){
     var.vec = datfram[,p]
  }else if( op == "and" ){
     var.vec = datfram[,k] & datfram[,p]
  }else if( op == "or" ){
     var.vec = datfram[,k] | datfram[,p]
  }else{
     stop("The argument 'op' must be equal to 'or', 'and' or left NULL.")
  }
}

names(var.vec) = 1:length(var.vec)
var.vec = var.vec[!is.na(var.vec)]       ## a named vector
                      print(var.vec)
datfram = datfram[,-p]
                      print(datfram)
datfram = update.df( datfram = datfram
                   , data = var.vec      ## a named vector
                   , id = 0
                   , variable = which
                   , action = action
                   , join = join
                   )

datfram

} ##----END----##
########################################################################################################################


########################################################################################################################
## EXAMPLES ############################################################################################################
########################################################################################################################

##############################################################################################
dummy = function(){
## This is dummy function - it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## - in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
##############################################################################################

df1 = data.frame( aa = sample(c(TRUE,FALSE),5,replace=TRUE)
                , bb = sample(c(TRUE,FALSE),5,replace=TRUE)
                , cc = sample(letters[1:5])
                , dd = sample(1:5)
                )
v1 = sample( c(TRUE,FALSE),5,replace=TRUE )
df1 ; v1

update.binary.df(df1,v1,which = 1)
update.binary.df(df1,v1,which = 1,action="append")
update.binary.df(df1,v1,which = 1,action="replace")
update.binary.df(df1,v1,which = "bb",action="replace")

##
update.binary.df(df1,v1,which = 1,action="append", op = "and")
update.binary.df(df1,v1,which = 1,action="append", op = "or")
update.binary.df(df1,v1,which = 1,action="replace", op = "or")
##
update.binary.df(df1,df1[,"bb"],which = 1,action="replace")
update.binary.df(df1,df1[,"bb"],which = "aa",op = "and")
update.binary.df(df1,df1[,"bb"],which = "aa",op = "or")

##
v2 = v1 ; names(v2) = 3:7 ; v2
update.binary.df(df1,v2,which = 1)
update.binary.df(df1,v2,which = 1,join="outer")
update.binary.df(df1,v2,which = 1,join="outer",op = "and")     #!#!#!
update.binary.df(df1,v2,which = 1,join="outer",op = "or")      #!#!#!


}





########################################################################################################################

update.errs = function(   var_name #= var_name
#                        , ID_variable = ID_variable
#                        , ERRS_dataframe = ERRS_dataframe
#                        , ERRS_variable = ERRS_variable
                        , envir = 1
                        , action = "append" ## "or" , "and" , "replace"
                       ){
############################################
## CAUTION:
##  This function takes some arguments from the .GlobalEnv:
##   DFerrs, VARerrs, ID_variable
##


errs = get(VARerrs)
  print(sum(errs))

datfram = get(DFerrs)

j = which( names(datfram) == var_name )

if( length(j)>0 ){    ## if var_name is already present in DF... then it is replaced with new values

  if(action == "append"){

		  	k = 1 ; var.name_k = paste(variable , 1 , sep="_")
		  	while( var.name_k %in% names.df ){ k = k+1 ; var.name_k = paste(variable , k , sep="_")}

    assign( DFerrs
      , merge(  datfram
              , vec2df( errs , ID_variable , var_name_k )
              , by = ID_variable
              , all.x = TRUE
             )
      , pos = envir
      )

  }else if(action == "replace"){

    assign( DFerrs
      , merge(  datfram[,-j]
              , vec2df( errs , ID_variable , var_name )
              , by = ID_variable
              , all.x = TRUE
             )
      , pos = envir
      )

  }else if(action %in% c("and","or")){

        errs1 = df2vec( datfram , column = var_name , names = ID_variable )

        errs2 = df2vec(   merge(  datfram[,ID_variable]
                                , vec2df( errs , ID_variable , var_name )
                                , by = ID_variable
                                , all.x = TRUE
                               )
                        , column = var_name
                        , names = ID_variable
                      )

        if(action == "and"){
          datfram[,j] = errs1 & errs2
        }else if(action == "or"){
          datfram[,j] = errs1 | errs2
        }else{ warning("If you see this message your machine went mad...") }

       assign( DFerrs, datfram , pos = envir)

  }else{
    stop("Parameter 'action' must take one of the following values: \"append\", \"replace\", \"and\", \"or\".")
  }

}else{                ## if var_name is NOT present in DF... then it's appended to it

assign( DFerrs
  , merge(  datfram
          , vec2df( errs , ID_variable , var_name )
          , by = ID_variable
          , all.x = TRUE
         )
  , pos = envir
  )

}

}

########################################################################################################################

update.outs = function(   var_name #= var_name
#                        , ID_variable = ID_variable
#                        , OUTS_dataframe = OUTS_dataframe
#                        , OUTS_variable = OUTS_variable
                        , envir = 1
                       ){
############################################
## CAUTION:
##  This function takes some arguments from the .GlobalEnv:
##   DFouts, VARouts, ID_variable
##

outs = get(VARouts)
  print(sum(errs))

datfram = get(DFouts)

j = which( names(datfram) == var_name )

if( length(j)>0 ){    ## if var_name is already present in DF... then it is replaced with new values

  if(action == "append"){

		  	k = 1 ; var.name_k = paste(variable , 1 , sep="_")
		  	while( var.name_k %in% names.df ){ k = k+1 ; var.name_k = paste(variable , k , sep="_")}

    assign( DFouts
      , merge(  datfram
              , vec2df( outs , ID_variable , var_name_k )
              , by = ID_variable
              , all.x = TRUE
             )
      , pos = envir
      )

  }else if(action == "replace"){

    assign( DFouts
      , merge(  datfram[,-j]
              , vec2df( outs , ID_variable , var_name )
              , by = ID_variable
              , all.x = TRUE
             )
      , pos = envir
      )

  }else if(action %in% c("and","or")){

        outs1 = df2vec( datfram , column = var_name , names = ID_variable )

        outs2 = df2vec(   merge(  datfram[,ID_variable]
                                , vec2df( outs , ID_variable , var_name )
                                , by = ID_variable
                                , all.x = TRUE
                               )
                        , column = var_name
                        , names = ID_variable
                      )

        if(action == "and"){
          datfram[,j] = outs1 & outs2
        }else if(action == "or"){
          datfram[,j] = outs1 | outs2
        }else{ warning("If you see this message your machine went mad...") }

       assign( DFouts, datfram , pos = envir)

  }else{
    stop("Parameter 'action' must take one of the following values: \"append\", \"replace\", \"and\", \"or\".")
  }

}else{                ## if var_name is NOT present in DF... then it's appended to it

assign( DFouts
  , merge(  datfram
          , vec2df( outs , ID_variable , var_name )
          , by = ID_variable
          , all.x = TRUE
         )
  , pos = envir
  )

}

}

########################################################################################################################
## EXAMPLES
##

dummy = function(){  ## it's not a real function, only an envelope for examples

}
