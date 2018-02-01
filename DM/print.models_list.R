########################################################################################################################—•°
## FUNCTIONS HERE    {DM}
##  print.models_list()
##  print.models_list_gam()
##  print.models_table()
##  print.models_table_gam()
##
## DEPENDENCIES
##  models_table()   {DM}   models_list.R
##
## TODO
## •
##
########################################################################################################################—•°

########################################################################################################################—•°
## "models_list" is a class for list of models — all models of the same class, i.e. made by the same function.
## "models_list_gam" is list of models created by gam().
##
##!! These classes are defined  models_list.R although this file should be part of FUmMod package !!!
##
## models_table() generates table with most important informations extracted from each model of the models_list.
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