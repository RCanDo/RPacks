## ---------------------------------------------------------------------------------------------------------------------???
## FUNCTIONS HERE    {DM}
##  indent()
##
## DEPENDENCIES
##
## TODO
##
## ---------------------------------------------------------------------------------------------------------------------???

## ---------------------------------------------------------------------------------------------------------------------???
indent = function(x, times=1, ind="   ", as.text=FALSE, ...){
## ------------------------------------------------------------------------------------------------???
## Indenting output of print(x) with string given to 'ind'.
##    Arguments
##  x          any object possible to print;
##  times      how many times repeat ind;
##  ind        string to use to indent; default are 3 spaces "   ";
##             any sequence of characters is possible with special signs like \n, \t and other.
##             you may use "\t" for using tab or "\n" (new line) which produce an extra line between lines
##             instead of indentation;
##  as.text    deafault FALSE; then character vectors are printed normally;
##             if TRUE then character vectors are printed via cat() thus there are no quotes and all special characters
##             are applied, e.g. "\n" results with new line; however, cat() is used with its deafult separator " ",
##             hence consequtive elements of vector are separated by only one space, unless "\n" is at the end of
##             the previous what results with the next element printed in new line but indented additionaly with " ".
##             This functionality facilitates the use of tree like structured messages, see indentr().
##  ...        further arguments passed to print() if as.text=FALSE.
## ------------------------------------------------------------------------------------------------???

if(as.text && is.character(x)){
#   y <- gsub("\n$", "", x)
#   y <- gsub("\n", paste0("\n", ind, " "), x)
   y <- capture.output(cat(x))
}else{
   y <- capture.output(print(x, ...))    ## y finnishes with "\n"   !!!
}

   ## if x is print(...) itself then NULL appears at the end (print() returns NULL); get rid of it:
   n <- length(y)
   if(n>1 && y[n]=="NULL"){ y <- y[-n] }


ind <- paste(rep(ind, times), collapse="")
cat(ind)
cat(y, sep=paste0("\n", ind))

}  ##----END----##
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
## EXAMPLES ############################################################################################################—•°
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
dummy = function(){
## This is dummy function - it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## - in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
## -------------------------------------------------------------------------------------------------—•°
 ## RELOADER - before it works you need to source("RCanDo.R"); it's best to use DM.R within pack's dir.
 loadPacksAK("DM")
## -------------------------------------------------------------------------------------------------—•°

mm <- matrix(1:9, 3)
indent(mm)
indent(mm, 0) ## no indentation
indent(mm, 2)
indent(mm, 1, "   ")   ## default values: 1 times 3 spaces
indent(mm, ind="\t")  ## 1 tab which is 8 spaces..
indent(mm, ind="## ") ## you may copy it to your script as a log or comment
indent(mm, ind="#")   ## maybe useful sometimes
indent(mm, ind="-- ") ## an SQL comment

indent(mm, ind="\n") ## entering extra line :)
indent(mm, ind="\n   ") ## with indenting

b = c("message0\n", "message1\nqq", "message2", "m3")
indent(b)
indent(b, as.text=TRUE)  ## this option is useful when printing messages; see indentr() for recursive indenting of lists.

xx<-1:10; yy<-sample(10); mod<-lm(yy~xx)
indent(mod)
indent(mod, ind="## ") ## now you may copy and paste it into the script as a notice ...
indent(summary(mod), ind="## ") ## ... or just to have quick reference values

ll <- list(
   list("a", b = c("message1\nqq", "message2", "m3"), c = c("message1\n", "message2", "m3"), d = c("message1", "message2"))
   , two = 1:3
   , letters[1:3]
##, four = list( ab = list( 3:6, model = mod , zz=LETTERS[3:4]) , mm = mm , "nothing" , "3"= 33 )
)
indent(ll)   ## the list is printed normally and the whole output indented;
## BUT there is indentr() function  which indents lists resursively what gives pretty, easily readable output.

## ------------------------------------???
## indentr()
indentr(ll)  ## this function uses indent() itself but you may indent it too
indent(indentr(ll), 3)
indent(indentr(ll), ind="## ")

## in this case use as.text inside indentr()
indent(indentr(ll, as.text=TRUE))   ## OK
indent(indentr(ll), as.text=TRUE)   ## nothing

indent(print(mod))   ## double...
indent(indentr(mod))
## mod is a list so you may wish to see all its entries but recursively indented;
## this is possible with indentr()
indentr(mod, rm="lm")
indent(indentr(mod, rm="lm"))
indent(indentr(mod, rm="lm"), ind="## ")

## ------------------------------------
indent(cat("abc\n", "efg", "hij"))
indent({cat("abc\n", "efg", "hij")})
indent({cat("abc\n", "efg", "hij");})
indent({cat("abc\n", "efg", "hij"); print("")})
indent({print("")})

ff <- function(ss){
    indent({print(ss)})
}
ff("")

## ???

## ------------------------------------???
## more or indentr() in help on it



## ------------------------------------???
## other ways of capturing output
pm <- vector("character")
con <- textConnection("pm", 'wr', local=TRUE)
sink(con)
print(mm)
sink()
close(con)
pm


## ---------------------------------------------------------------------------------------------------------------------—•°
}; rm(dummy)

