## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE    {DM}
##  head.tm()
##
## DEPENDENCIES
##  whileif()     {DM}
##  head.attr()     {DM}
##
## TODO
##
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
head.tm <- function( x , head = NULL , ... ){
## ------------------------------------------------------------------------------------------------—•°
##
## ------------------------------------------------------------------------------------------------—•°

   head = whileif(head,iftrue=NULL,ifnull=head.attr(x),ifnull=6,iffalse=NULL)
   #margins = whileif(margins(x),ifnull=0)

   if(!is.null(head)){

      nr <- nrow(x)
      maxrow <- min(nr,head)
      rows <- 1:maxrow
      #x <- tm(x,attributes=TRUE,...)   ## no tm-attributes

      attrs <- attributes(x)
      attrs$dim <- NULL
      attrs$dimnames <- NULL

      #if(margins[1]==1){
      #   rows <- c(rows,nr+1)
      #}
      x <- x[rows,]

      attributes(x)[names(attrs)] <- attrs
   }

   #head.attr(x) <- head
   x

}  ##----END----##
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
## EXAMPLES ############################################################################################################—•°
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
dummy = function(){
## This is dummy function — it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## — in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
## ------------------------------------------------------------------------------------------------—•°

 ## RELOADER — before it works you need to source("PacksAK.R"); it's best to use DM.R within pack's dir.
 loadPacksAK("DM")

## ------------------------------------------------------------------------------------------------—•°

hm <- matrix(sample(36),9)
dimnames(hm) <- list(X=letters[1:9],Y=letters[21:24])
hm
head(hm)

margins(hm) <- 1:2
hm
head(hm)
head(hm,4)
head(hm,10)

transpose(hm) <- T
hm
head(hm)
head(hm,2)

t(hm)
hm[]

head.attr(hm) <- 4
hm


## ---------------------------------------------------------------------------------------------------------------------—•°
};rm(dummy)

