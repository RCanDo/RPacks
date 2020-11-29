## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE    {DM}
##  No functions here.
##  It is setup of options:
##    class.groups
##    class.attributes
##
## DEPENDENCIES
##
## TODO
## •
##
## ---------------------------------------------------------------------------------------------------------------------—•°

options(class.groups = list( language = c( "function","call","formula","promise","language","{")
                            , implicite = c("integer","character","numeric","double","complex","logical","matrix","array")
                            , common = c("table","data.frame","factor")
                            )
       )

options(class.attributes = list( basic = c("names","dim","dimnames","colnames","rownames","row.names","class","levels")
                               , tm =  c("transpose","margins","head","round")
                               , compact = "compact"
                               )
       )

options( messages = TRUE )    ## this options works only for "tm" class and some methods defined in packages from PacksAK
## ---------------------------------------------------------------------------------------------------------------------—•°

#print.list <- function(x,...){ indentr(x,...) }

## ---------------------------------------------------------------------------------------------------------------------—•°
dummy <- function(){
   getOption("class.attributes")
   options("class.attributes")
   getOption("class.attributes")$tm
   options("class.attributes")[[1]]$tm
   getOption("class.attributes")$basic

   getOption("class.groups")
   getOption("class.groups")$language

   getOption("messages")
}
rm(dummy)
## ---------------------------------------------------------------------------------------------------------------------—•°
