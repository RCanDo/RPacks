########################################################################################################################•
## FUNCTIONS HERE
##
## create.logging.df()
##
## DEPENDENCIES
##  none
########################################################################################################################•
create.logging.df = function(datfram,logs.names=NULL,pos=".GlobalEnv"){
########################################################################################################################•
##
########################################################################################################################•

datfram.name = deparse(substitute(datfram))

if(is.null(logs.names)){
   logs.names = ".logging"
}

ii = grep('^\\.',logs.names)

if(length(ii)>0){
  logs.names[ii] = paste0(datfram.name,logs.names[ii])
}

logs.exists = unlist(lapply(logs.names,exists))

if(sum(logs.exists)>0){
   warning("Objects with names \"", paste(logs.names[logs.exists],collapse="\", \""), "\" already exist.")      #"
}

logs.names = logs.names[!logs.exists]

if(length(logs.exists)>0){

   logdf <- datfram[,1,drop=FALSE]
   logdf[,1] <- 0
   names(logdf) = "sum.."
   attr(logdf,"condlist") = list()
   class(logdf) <- c("logging.data.frame","data.frame") ;

   for(nam_k in logs.names){
     assign( nam_k , logdf , pos = pos )
   }

}
########################################################################################################################

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
datfram = data.frame("aa" = sample(letters[1:5]), "nn" = rnorm(5)) ; rownames(datfram) = sample(LETTERS[1:5])
datfram

create.logging.df(datfram)
datfram.logging
attributes(datfram.logging)

create.logging.df(datfram,".qq")
datfram.qq

create.logging.df(datfram,c(".qq",".a","qqq"))
datfram.qq
datfram.a
datfram.qqq ## ERROR!
qqq

rm(datfram.logging, datfram.qq, datfram.a, qqq)

}

rm(dummy)