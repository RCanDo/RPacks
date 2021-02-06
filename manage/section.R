## ---------------------------------------------------------------------------------------------------------------------—•°
## Printing out name of script's section and time info.
## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE    {manage}
##  section(txt, time=TRUE, t0=NULL, txt0="elapsed: ", ...)
##
## DEPENDENCIES
##
## REMARKS/WARNINGS
##
## TODO
##
## DESCRIPTION
##
## -------------------------------------------------------------------------------------------------—•°
## AUTHOR: Arkadiusz Kasprzyk; rcando@int.pl
## ---------------------------------------------------------------------------------------------------------------------—•°

section <- function(title, time=TRUE, t0=NULL, txt0="elapsed: ", ...){
## -------------------------------------------------------------------------------------------------—•°
##  txt           section title
##  time=TRUE     logical; if TRUE prints out current system time i.e. the beginning of the section
##                if FALSE not time info will be printed (and following args are irrelevant)
##  t0=NULL       other time object; if not NULL then a difference between current time and t0
##                will be printed out
##  txt0          text to be printed out before info about time difference (see t0).
##
##   Result
##  Current system time what allows to log it to some registry.
## -------------------------------------------------------------------------------------------------—•°
cat(title, "\n")
t1 <- Sys.time()

if(time){
    cat(" ", format(t1, "%Y-%m-%d %H:%M:%S %Z"), "\n")
    if(!is.null(t0)){
        dif_t <- t1 - t0
        if(!is.null(txt0)){
            cat(" ", txt0)
        }
        cat(" ", format(dif_t), "\n")
    }
}

return(t1)
}  ##----END----##
## ---------------------------------------------------------------------------------------------------------------------—•°
