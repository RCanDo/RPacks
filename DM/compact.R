########################################################################################################################—•°
## Class "compact" - definition, attributes and print method.
########################################################################################################################—•°
## FUNCTIONS HERE    {DM}
##  compact()
##  compact<-
##  uncompact()
##  compact.class()
##  print.compact()
##
## DEPENDENCIES
##  indent()
##
## TODO
##  • character vectors are displayed without "". Is that OK???  No choice...
##
########################################################################################################################—•°

########################################################################################################################—•°
compact <- function(x,n=NULL){ if(is.null(n)){attr(x,"compact")}else{ if(is.primitive(x)) x else compact(x)<-n ; x } }
###################################################################################################—•°
## Returns 'compact' attribute of x if n is null.
## If n is not null then  x<-compact(x,n)  is equivalent to  compact(x)<-n
########################################################################################################################—•°

########################################################################################################################—•°
`compact<-` <- function(x,value){
###################################################################################################—•°
## Assigning 'compact' attribute and class to the object.
## 'compact' class indicates that the object will be printed in the compact form according to parameters written in
## 'compact' attribute.
## In general 'compact' printing means printing only first few values of any atomic vector,
## in a linear form neglecting formatting proper to dimensionality.
## Internaly cat() is used for giving output to the screen thus quotations of character values are not printed
## while all the special characters like e.g. \n or \t are applied
## (what may be sometimes very useful and sometimes quite annoying).
##    Arguments
##  x       object to be assigned a 'compact' attribute.
##  value   details on what 'compact' printing should mean.
##          • c(show, side)   'show' — how many elements of an atomic vector are printed:
##                               if 0 then all elements are printed;
##                               if less then vector has elements then ... appears at the end;
##                               if more then vector has elements then then all elements are printed;
##                            'side' — where to display info about length/dim of the vector:
##                               0  do not display at all;
##                               1  at the end of the line of printed values (default);
##                               2  below the line of printed values.
##          • show   if value has only one element then it is the same as the 'show' above and 'side' is set to 1.
##          • NULL   deleting class and attribute 'compact';
##                   you may also use  x <- uncompact(x)  instead of  compact(x) <- NULL.
##    Remarks
## If x is a list() (recursive vector) then its 'compact' attribute is applied to every sublist down to the leafs
## (i.e. innermost atomic vectors).
## Every sublist (or a leaf) is indented by 3 spaces as indent() is used (see help on it).
## Additionaly names of all lists' (and sublists') elements are printed,
## replaced with their numeric position if name doesn't exist.
###################################################################################################—•°
   if(is.null(value)){
      class(x) <- setdiff(class(x),"compact")
      attr(x,"compact")<-NULL
   }else{
      class(x) <- union("compact",class(x))
      value <- if( length(value)==1 ){ c(value,1) }else{ value[1:2] }
      names(value) <- c("show","side")
      if( !is.numeric(value) ){ stop("Attribute 'compact' must be numeric.") }
      if( !value[2]%in%c(0,1,2) ){ stop("Attribute compact[2] must be 0, 1 or 2.") }
      attr(x,"compact")<-value
   }
   x
}
########################################################################################################################—•°
uncompact <- function(x){compact(x)<-NULL;x}
########################################################################################################################—•°

########################################################################################################################—•°
compact.class <- function(x){
###################################################################################################—•°
## Checks if object has "compact" attribute which allow it to inherit from "compact" class.
## If so then adds "compact" to its class attribute.
## If not then removes "compact" from its class attribute.
###################################################################################################—•°
   if( any( getOption("class.attributes")$compact %in% names(attributes(x)) ) ){
      class(x) <- union("compact",class(x))
   }else{
      class(x) <- setdiff(class(x),"compact")
   }
   if(all(class(x)%in%unlist(getOption("class.groups")[c("language","implicite")]))){class(x)<-NULL}
   x
}
########################################################################################################################—•°

########################################################################################################################—•°
print.compact <- function(x,ind="   ",digits=getOption("digits"),...){
###################################################################################################—•°
## Indenting output of print() with given string.
## See help on  compact<- .
###################################################################################################—•°

if(is.atomic(x)){

   n <- ifelse(is.null(compact(x)),0,compact(x)[1])
   side <- ifelse(!is.null(compact(x)[2]),compact(x)[2],1)

   levs <- ifelse(is.factor(x),paste0(" levels: ",paste(levels(x),collapse=" ")),"")

   size <- if(is.null(dim(x))){
              paste("length:",length(x))
           }else{
              paste("dim:",paste(dim(x),collapse=" "))
           }

            #as.character(signif(pi^6,d))
            #as.character(pi^6)

   x <- if(is.numeric(x)){ signif(x,digits) }else{ x }


   ## printing
   x <- as.character(x)
   if(n>0 & n<length(x)){ cat(x[1:n],"...") }else{ cat(x) }
   ## info
   info =  switch(as.character(side),
              "0" = if(levs==""){ NULL }else{ paste("\n",levs,sep="") }
            , "1" = if(levs==""){ paste("  ",size,sep="") }else{ paste("  ",size,"\n",levs,sep="") }
            , "2" = if(levs==""){ paste("\n",size,sep="") }else{ paste("\n",size,", ",levs,sep="") }
            )

   cat(info,"\n")

}else if(is.language(x)){
   cat( deparse(x), "\n", sep="")
}else if(is.function(x)){
   if(is.primitive(x)){
      print(uncompact(x))    ## not needed now; there is a safety catch in compact<- wher you cannot give "compact" to .Primitive
   }else{
      cat( deparse(body(x)), "\n", sep="")
   }
}else{

   if(length(x)>0){

      ## missing names
      if(is.null(names(x))){
         names(x) <- paste0("[[",1:length(x),"]]")
      }else{
         empties <- which(names(x)=="")
         names(x)[empties] <- paste0("[[",empties,"]]")
      }


      for(k in 1:length(x)){ ## safer if names repeat
         nam <- names(x)[k]
         y <- compact(x[[k]],compact(x))
         cat(nam,"\n",sep="");indent(y,ind=ind)   ## info about length in the same line as data
      }

   }else{
      cat("empty list","\n",sep="")
   }
}


}  ##----END----##
########################################################################################################################—•°


########################################################################################################################—•°
## EXAMPLES ############################################################################################################—•°
########################################################################################################################—•°

########################################################################################################################—•°
dummy = function(){
## This is dummy function — it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## — in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
##############################################################################################—•°

 ## RELOADER — before it works you need to source("PacksAK.R"); it's best to use DM.R within pack's dir.
 loadPacksAK("DM")

##############################################################################################—•°

(aa <- sample(100))
compact(aa,10)
compact(aa,0)
compact(aa,200)
compact(aa,c(10,0))
compact(aa,c(10,1))    ## 1 is default
compact(aa,c(10,2))
## but it creates a copy so that object passed do not change
aa
## setting "compact" attribute and class
ab<-compact(aa,c(10))      ## 2 args
ab
class(ab)
## checking "compact" attribute
compact(ab)    ## 1 args;  notice that by default side==1
               ## (i.e. output will take 1 line, the info about length or dim will be "appended" to the data)
ab<-compact(ab,c(10,1))
ab
compact(ab)
uncompact(ab)
   uncompact(aa)  ## works on non-compacts too
ab
compact(ab)<-NULL     ## un-compacting
ab
compact(ab)
class(ab)

   compact(aa,c(10,3))    ## ERROR !!!
   compact(aa,c(10,-1))   ## ERROR !!!
   compact(aa,c(10,pi))   ## ERROR !!!

compact(ab,TRUE)      ## logicals are coerced to numeric
compact(ab,FALSE)


ff <- factor(sample(letters[1:3],12,replace=TRUE))
ff
compact(ff,6)
compact(ff,c(6,0))     ## levels are always displayed below
compact(ff,c(6,1))     ## side=1 is default
compact(ff,c(6,2))

dim(ff)<-c(3,4)
ff
class(ff)
dim(ff)
compact(ff,4)
fg <- compact(ff,4)
fg
class(fg)
compact(fg)
dim(fg)
uncompact(fg)
fg
fg <- uncompact(fg)
fg


mm = matrix(1:12,3)
compact(mm) <- 0
mm
compact(mm)

compact(mm) <- 4
mm
compact(mm) <- 20
mm
compact(mm)
class(mm)
uncompact(mm)
compact(mm) <- NULL
mm
compact(mm)
class(mm)


(df1 <- data.frame(a=letters[1:7],b=1:7,c=sample(LETTERS[1:3],7,replace=TRUE)))
compact(df1,3)  ## ! data frame is not atomic, it is a list

## empty lists
l0 = list()
compact(l0,0)
compact(l0,2)
compact(l0) <- c(1,1)
l0
compact(l0)

## non trivial lists
ll <- list("df"=df1,"empty_list"=l0,aa,"mm"=mm,ff,"txt"=c("qq","mala kala","a\tb","up\ndown"))
ll
compact(ll,5)  ## notice that \n and \t are applied

ll2 <- list("aa"=sample(20),"long list" = ll)
ll2
compact(ll2,5)
compact(ll2,c(5,0))  ## no info on length/dim
compact(ll2,c(5,1))
compact(ll2,c(5,2))

## Displaying notes, comments, etc.
infos <- list("info 1" = "First info about sth.", "info 2" = "Second info about sth."
             , "other infos" = list( "sub-info 3" = "Third info about sth.", "sub-info 4" = "Fourth info about sth." ) )
compact(infos,c(0,0))

## without names
infos <- list( "First info about sth.", "Second info about sth."
             ,  list( "Third info about sth.",  "Fourth info about sth." ) )
compact(infos,c(0,0))

## mixture
infos <- list("info 1" = "First info about sth.", "Second info about sth."
             , "other infos" = list( "Third info about sth.", "sub-info 4" = "Fourth info about sth." ) )
compact(infos,c(0,0))

## when names repeat
infos <- list("info" = "First info about sth.", "info" = "Second info about sth."
             , "info" = list( "Third info about sth.", "sub-info 4" = "Fourth info about sth." ) )
compact(infos,c(0,0))

## you may additionaly use indent()
indent(compact(df1,3))
indent(compact(df1,5))
indent(compact(df1,5),2)
indent(compact(df1,5),1,"## ")  ## you may copy and paste it into a script as a note or quick reference
indent(compact(df1,5),2,"  .")

indent(compact(ll2,5))
indent(compact(ll2,5),1,"## ")
indent(compact(ll2,5),2,"  .")

indent(compact(infos,c(0,0)),1,"## ")

};rm(dummy)
########################################################################################################################—•°

