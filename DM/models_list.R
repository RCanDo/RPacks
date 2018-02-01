########################################################################################################################—•°
## FUNCTIONS HERE    {DM}    BUT should be in {SumMod} !!!
##  models_table()
##  models_table.default()
##  models_table.models_list_gam()
##  print.models_list()
##  print.models_list_gam()
##  print.models_table()
##  print.models_table_gam()
##
## DEPENDENCIES
##
## TODO
## •
##
########################################################################################################################—•°

########################################################################################################################—•°
## "models_list" is a class for list of models — all models of the same class, i.e. made by the same function.
## "models_list_gam" is list of models created by gam().
##
## models_table() generates table with most important informations extracted from each model of the models_list.
########################################################################################################################—•°

########################################################################################################################—•°
models_table <- function(x,...){UseMethod("models_table")}
########################################################################################################################—•°
models_table.default <- function(x,...){   ## NOT CHECKED !!! DO TESTS for  class(x) == "lm"
structure(
   cbind(  coeff = t(sapply(mlg,'[[',"coefficients"))
      , rank = sapply(mlg,'[[',"rank")
      , df.residul = sapply(mlg,'[[',"df.residual")
      , df.null = sapply(mlg,'[[',"df.null")
      , deviance = sapply(mlg,'[[',"deviance")
      , null.deviance = sapply(mlg,'[[',"null.deviance")
      , aic = sapply(mlg,'[[',"aic")
      )
, class = "models_table")
}
########################################################################################################################—•°
models_table.models_list_gam <- function(mlg,...){
structure(
   cbind(  coeff = t(sapply(mlg,'[[',"coefficients"))
      , rank = sapply(mlg,'[[',"rank")
      , nl.df = sapply(mlg,'[[',"nl.df")
      , df.residul = sapply(mlg,'[[',"df.residual")
      , df.null = sapply(mlg,'[[',"df.null")
      , deviance = sapply(mlg,'[[',"deviance")
      , null.deviance = sapply(mlg,'[[',"null.deviance")
      , aic = sapply(mlg,'[[',"aic")
      , nl.chisq = sapply(mlg,'[[',"nl.chisq")
      , iter = sapply(mlg,'[[',"iter")
      )
, class = "models_table_gam")
}
########################################################################################################################—•°
models_table.smooth_gam <- function(sg){
   models_table(sg$models_list)                ## "models_list_gam","models_list"
}
########################################################################################################################—•°

########################################################################################################################—•°
print.models_list <- function(ml,...){
   print(unclass(models_table(ml)))
}
########################################################################################################################—•°
print.models_list_gam <- function(mlg,...){
   print(mlg[[1]]$call)
   print(unclass(models_table(mlg)))
}
########################################################################################################################—•°

########################################################################################################################—•°
print.models_table <- function(mt,...){
   print(unclass(mt))
}
########################################################################################################################—•°
print.models_table_gam <- function(mt,...){
   print(unclass(mt))
}
########################################################################################################################—•°