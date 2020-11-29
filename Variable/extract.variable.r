## ---------------------------------------------------------------------------------------------------------------------??
## FUNCTIONS HERE
##  extract.variable()
##
## DEPENDENCIES
##  ....r
##    ...() , ...
##
## ---------------------------------------------------------------------------------------------------------------------??
extract.variable = function(
         variable.name      	## variable name
        , datfram            	## data frame
		  , as.factor = FALSE    ## if TRUE then numeric variable will be coerced to factor before any other action on its values;
										## thus only 'min.counts' is relevant ('lft', 'rgt', FUN, 'lft.fun', 'rgt.fun' and 'transform' are ignored);
										## default is FALSE;
                              ## may be also numeric - then variable is coerced to facator if  length(unique(variable)) <= as.factor
        , lft = NULL         	## left limit of the numeric variable BEFORE being transforemd by FUN
        , rgt = NULL         	## right limit of the numeric variable BEFORE being transforemd by FUN
		  , FUN = NULL	  		 	## function to transform data (only numeric); should not be an aggregate (like mean(), var(), etc.)!
        , lft.fun = NULL       ## left limit of the numeric variable AFTER being transforemd by FUN
        , rgt.fun = NULL       ## right limit of the numeric variable AFTER being transforemd by FUN
		  , transform = TRUE		## should a variable be returned transformed by FUN, or left untransformed (with original values from datfram);
		  								## irrelevant if FUN is NULL.
        , min.counts = NULL  	## minimum nr of observatins in the factor level
        , envir = NULL       	## environment to which the variable should be extracted -- see assign() 
		  , message = TRUE     	## print the basic statistics about extracted variable ?
        , log = NULL         	## name of the data frame used as a log; if NULL then log is made to data frame with name "'datfram'.trans.log"
        , full.return	= FALSE	##	should the variable be returned full (with all outliers and NAs)? 
		  								## default is FALSE and then only these values are returned which are non-NA and satisfy rules described below;
      )
## ---------------------------------------------------------------------------------------------------------------------??
{
## Extracting variable with 'variable.name' from 'datfram' data frame in the following manner:
#! 	? only non-NA values and only these which:
## 	? are within [lft, rgt] interval BEFORE being transformed by the function passed to FUN (by name or call)  AND
## 	? are also within [lft.fun, rgt.fun] interval AFTER being transformed by the function passed to FUN (by name or call).
## If FUN is null then the second condition is ignored.
## By default lft = rgt = lft.fun = rgt.fun = NULL what means that min and max of the variable are taken.
## For numeric variable 'min.counts' is ignored.
##
## If variable is a factor, then 'lft', 'rgt', FUN, 'lft.fun', 'rgt.fun' and 'transform' are ignored
## and these observations are excluded which are representatives of the levels having counts LESS then 'min.counts'.
## By default min.counts = NULL what is equivalent to min.counts = 1.
## Thus the levels with no occurances will be excluded, hence, for the variable returned, we have
##         levels(factor) == unique(factor)
## (remind that in general levels(factor) >= unique(factor) -- some levels may have no occurances).
## If mean.counts <= 0 then levels with no occurances will NOT be excluded
## from a list of levels of the factor.
#! If full.return = TRUE then generally a factor is returned intact with the exception of
## min.counts = 1 where it is cleared off of nonexisting levels.
##
## Variable will be assigned to the  variable.name  by the command  assign()
## into the environment specified through 'envir'  which is passed to option 'pos' in assign().
## For more details see help for assign().
##
#!# If 'transform' is TRUE (default) then variable will be returned transformed by the FUN.
#!# If 'transform' is FALSE then variable will be returned NOT transformed by the FUN.
#!# In this case, NAs appeared due to transformation are also ignored,
#!# however limits  'lft.fun', 'rgt.fun' are taken into account.
##
##
## If log = NULL (default) then log is made to data frame with name "'datfram'.trans.log";
## If a string is passed to 'log' then a log data frame will be created in the .GlobalEnv with name given by this string;
## If a data frame with this name already exists then the information about present extraction will be appended to the existing d.f.
##	under condition that it has a proper format; if the format is improper then an error occur.
## Each row of log data frame has unique name of the extracted variable, 
##	thus new extraction of the same variable will overwrite information about previous extraction.
## Log data frame consists of the following variables (fields):
##    all              length(variable)
##    NAs              number of NAs
##    selected         number of selected values
##    rejected         number of rejected non-NAs (outliers)                 
##    lft               left limit of the numeric variable BEFORE being transforemd by FUN
##    rgt               right limit of the numeric variable BEFORE being transforemd by FUN
##    FUN              function to transform data (only numeric); should not be an aggregate (like mean(), var(), etc.)!
##    lft.fun           left limit of the numeric variable AFTER being transforemd by FUN
##    rgt.fun           right limit of the numeric variable AFTER being transforemd by FUN
##    transformed      was a variable returned transformed by FUN, or left untransformed (with original values from datfram);
##	  						       irrelevant if FUN is NULL.
##    range.lft        minimum value of extracted values
##    range.rgt        maximum value of extracted values            
##    min.counts        minimum nr of observatins of the factor level to leave the factor out
## Entries marked with * are values of function parameters and have the same meaning as described above.
## 
## Result of this function is returned in the list, called (internally) 'extract',
## with the following elements:
##    variable    	values of the returned variable (selected according to rules described above);
##    NAs         	= is.na( FUN( datfram[, variable.name] ) )
##								indicates position of NAs of the variable in the data frame (after transformation by FUN if FUN is not NULL and 'transform' is TRUE);
##    selected    	indicates position of selected values of the variable in the data frame;
#!                   	NA values are NOT within selected, thus if also NA values are needed use
#!                   	datfram[ extract$selected | extract$NAs , variable.name ].
##    name        	variable.name -- name of the variable in 'datfram';
##    cutoff      	= c(lft, rgt) in case of numeric variable; if factor then c(NA, NA);
##    FUN		     	body of the FUN; it is usually a simple function like log, tan, 1/x, Box-Cox tranformation, etc...
##		cutoff.fun  	= c(lft.fun, rgt.fun) in case of numeric variable; if factor then c(NA, NA);
## 
##    range       	range of selected variable's values
##    statistics -- a vector defined as
##       statistics = c(  all = length(NAs) = length(selected)
##                      , NAs = sum(NAs)
##                      , selected = sum(selected)
##                      , rejected = sum(!(selected|NAs)) )   ## neither selected nor NA
##
## ---------------------------------------------------------------------------------------------------------------------??

if(!variable.name%in%names(datfram)){
   stop("Data frame does not contain variable with the given name.")
}

variable = datfram[, variable.name]
names(variable) = rownames(datfram)

## --------------------------------------------------------
if( is.numeric(as.factor) ){
  as.factor = as.factor >= length(unique(variable))
}

if( is.logical(as.factor) ){
  if( as.factor & !is.factor(variable) ){ variable = as.factor(variable) }
}

if(! (is.factor(variable)|is.numeric(variable)) ){
  variable = as.factor(variable)
  warning("Variable '", var.name, "' is neither numeric nor factor and it is coerced to factor.
  It is sensible for class 'character' but in case of 'date' or 'time' it may be not what you have expected.
  Nonetheless date/time classes are not supported yet. To be repaired soon!")
}

## --------------------------------------------------------
## needed later to statistics
cutoff.char = ifelse( is.factor(variable) , ifelse(is.null(min.counts), "NULL", min.counts) , paste( ifelse(is.null(lft), "NULL", lft) , ifelse(is.null(rgt), "NULL", rgt) , sep = " - ")  )
cutoff.num  = if( is.factor(variable) ){
      				c(NA, NA)
					}else{
						c( ifelse(is.null(lft), NA, lft)     , ifelse(is.null(rgt), NA, rgt) )
					}
		  #print(cutoff.num)

cutoff.fun.char = ifelse( is.factor(variable) , ifelse(is.null(min.counts), "NULL", min.counts) , paste( ifelse(is.null(lft.fun), "NULL", lft.fun) , ifelse(is.null(rgt.fun), "NULL", rgt.fun) , sep = " - ")  )
cutoff.fun.num  = if( is.factor(variable) ){
      				c(NA, NA)
					}else{
						c( ifelse(is.null(lft.fun), NA, lft.fun)     , ifelse(is.null(rgt.fun), NA, rgt.fun) )
					}
			#print(cutoff.fun.num)

if(is.factor(variable)){

      isordered = is.ordered(variable)

     	var.tab = table(variable)

      if(is.null(min.counts)){ min.counts = 1 }

      selected = ( variable %in% names(which(var.tab>=min.counts)) )
		NAs = is.na(variable)

      range.v = unique(variable[!NAs]) ; lrv = min(length(range.v), 7)     	## why 7 ???
     	dots = ifelse( length(range.v) > lrv , ", ..." , ""  )   	## will be needed below to print message

		if(!full.return){
         if(min.counts<=0){
            variable = variable[ selected ]              ## all levels remembered
         }else{
			   variable = factor(variable[ selected ], ordered = isordered )      ## only present levels

         }
		}else{
         if(min.counts==1){ variable = factor( variable , ordered = isordered ) }         ## only present levels
      }

}else{

   if(is.null(lft)){lft = min(variable, na.rm=TRUE)}
   if(is.null(rgt)){rgt = max(variable, na.rm=TRUE)}

   selected.0 = (variable>=lft)&(variable<=rgt)
   selected.0[is.na(selected.0)] = FALSE

	NAs = is.na(variable)

	#################################################################

	if(is.null(FUN)){

      selected = selected.0
		if(!full.return){
			variable = variable[selected]
		}
		
		FUN = function(x){x}          ## to be sensibly reported

	}else{

		variable.fun = FUN(variable)     ## NA's and FUN(x) = NaN  is NOT a problem!!!

	   if(is.null(lft.fun)){lft.fun = min(variable.fun, na.rm=TRUE)}
	   if(is.null(rgt.fun)){rgt.fun = max(variable.fun, na.rm=TRUE)}

	   selected.fun = ( variable.fun >= lft.fun ) & ( variable.fun <= rgt.fun )

	   selected.fun[is.na(selected.fun)] = FALSE

		NAs.fun = is.na(variable.fun)   ## containes NAs

		#################################################################

	   selected = selected.0 & selected.fun

		#################################################################

		if(transform){
			NAs = NAs.fun
			variable = variable.fun
			if(!full.return){
				variable = variable[ selected ]
			}
		}else{
			NAs.fun.selected.0 = NAs.fun & selected.0
			if(!full.return){
				variable = variable[ selected | NAs.fun.selected.0 ]
			}
		}

	}

   range.v = range(variable)   ##; lrv = 2

}

# statisctics.df = data.frame(  "values" = c( "all" , "NAs:", "selected:", "non-selected non-NAs:") , "number of" = c( length(NAs) , sum(NAs) , sum(selected) , sum(!(NAs|selected)) ) )

names(selected) <- names(NAs) <- rownames(datfram)

nas2all = round(sum(NAs)/length(NAs) , 4)

#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#
FUNbody = if(is.factor(variable)){ body(function(){as.factor(x)})   ##  as.factor()  doesn't destroy actual factor levels (contrary to factor())
          }else{ body(FUN) }   # as.list(body(FUN))[2]
#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#

extract = list(
   variable = variable
  , NAs = NAs
  , selected = selected   #!!!
  , name = variable.name
  , cutoff = cutoff.num               			##
  , FUN = FUNbody
  , cutoff.fun = cutoff.fun.num               ##
  , min.counts = min.counts
  , range = range.v
  , statistics = c(all = length(NAs), NAs = sum(NAs), "NAs.all" = nas2all, selected = sum(selected), rejected = sum(!(selected|NAs)) )
)

if(is.null(envir)){envir = 1} #".GlobalEnv"}
assign(variable.name, variable, pos = envir)

## ----------------------------------------------------------------------------------

if(message){
	if(!is.factor(variable)){
		cat(paste0(variable.name, "\n\n"))
		cat(paste0("Transformation: ", gsub(" ", "", gsub( "\\}", "" , gsub(";\\{", " ; ", gsub("\\{;", "", gsub("\n", "", paste(FUNbody, collapse=";")))))) , "\n"))     ## as.list(body(FUN))[2] #!#!#!#! POPRAWI? WY?WIETLANIE !!! napisa? do tego funkcj?
		cat(paste0("Cut-off values: ", cutoff.char, "\n"))
		cat(paste0("Cut-off values for trans.: ", cutoff.fun.char, "\n"))
		#if(is.factor(variable)){ clps = ", " }else{clps = " - "}
		clps = " - "
			cat(paste0("Range of variable: ", paste(range.v[1:2], collapse = clps), "\n"))
		cat("\n")
		cat(paste0("Number of all: ", length(NAs), "\n"))
		cat(paste0("Number of NAs: ", sum(NAs), "\n"))
      cat(paste0("NAs/all = ", nas2all , "\n"))
		cat(paste0("Number of selected: ", sum(selected), "\n"))
		cat(paste0("Number of rejected non-NAs: ", sum(!(NAs|selected)), "\n"))
		cat("\n")
	}else{
		cat(paste0(variable.name, "\n\n"))
		cat(paste0("Minimum number of occurances: ", min.counts, "\n"))
		#if(is.factor(variable)){ clps = ", " }else{clps = " - "}
		##
		clps =  ", "
		cat(paste0("Range of variable: ", paste0(paste(range.v[1:lrv], collapse = clps), dots) , "\n") )
		cat("\n")
		cat(paste0("Number of all: ", length(NAs), "\n"))
		cat(paste0("Number of NAs: ", sum(NAs), "\n"))
      cat(paste0("NAs/all = ", nas2all , "\n"))
		cat(paste0("Number of selected: ", sum(selected), "\n"))
		cat(paste0("Number of rejected non-NAs: ", sum(!(NAs|selected)), "\n"))
		cat("\n")
	}
}

## ----------------------------------------------------------------------------------
## log

## log is always made


if(is.null(log)){ log = paste0( deparse(substitute(datfram)) , ".trans.log" ) }

log.datfram = mget(  log
                   , ifnotfound=list( data.frame( all = numeric(0)
                                                , NAs = numeric(0)
                                                , "NAs/all" = numeric(0)
                                                , selected = numeric(0)
                                                , rejected = numeric(0)
																##
                                                , lft = numeric(0)
																, rgt = numeric(0)
																, FUNbody = list()
																, lft.fun = numeric(0)
																, rgt.fun  = numeric(0)
																, transformed = logical(0)
																, range.lft = numeric(0)
																, range.rgt = numeric(0)
																##
																, min.counts  = numeric(0)
																)
                                    )
                   , envir = .GlobalEnv )[[1]]

#FUNbody = as.character(as.list(body(FUN))[2]) ; 
#FUNbody = as.list(body(FUN))[2] ;

r.nams = row.names(log.datfram)#;  print(r.nams)
nr = nrow(log.datfram)         #;  print(nr)



if(variable.name %in% r.nams){ #print("qq")

      log.datfram[variable.name, c("all", "NAs", "NAs.all", "selected", "rejected", "lft", "rgt", "lft.fun", "rgt.fun", "range.lft", "range.rgt", "min.counts")] =
						c( extract$statistics , cutoff.num , cutoff.fun.num 
							, ifelse( is.factor(variable) , NA , range.v[1] ) , ifelse( is.factor(variable) , NA , range.v[2] )
							, ifelse( is.factor(variable) , ifelse(is.null(min.counts), 1, min.counts) , NA) 
						)
         #print(FUNbody); print(length(FUNbody))
         
      	names(log.datfram$FUNbody) = rownames(log.datfram)   
   	log.datfram$FUNbody[[variable.name]] = if( is.factor(variable)){ NA }else{ FUNbody }
		
		log.datfram[variable.name, "transformed"] = transform
   #print("aaaaa")

}else{
	   log.datfram = rbind( 	log.datfram
									,  data.frame(
          								  all = length(NAs)
											, NAs = sum(NAs)
                                 , "NAs.all" = nas2all
											, selected = sum(selected)
											, rejected = sum(!(selected|NAs))
											##
											, lft = cutoff.num[1] , rgt = cutoff.num[2]
											##
											, FUNbody = NA #if(is.factor(variable)){ NA }else{ FUNbody }
											##
											, lft.fun = cutoff.fun.num[1] , rgt.fun = cutoff.fun.num[2]
											, transformed = transform
											, range.lft = ifelse( is.factor(variable) , NA , range.v[1] )
											, range.rgt = ifelse( is.factor(variable) , NA , range.v[2] )
											##
											, min.counts = ifelse( is.factor(variable) , ifelse(is.null(min.counts), 1, min.counts) , NA)
										)
								 )
      	#	print(log.datfram)

	   row.names(log.datfram) = c(r.nams, variable.name)

		if(!is.list(log.datfram$FUNbody)){ log.datfram$FUNbody = as.list(log.datfram$FUNbody) }

     	names(log.datfram$FUNbody) = row.names(log.datfram)

     	log.datfram$FUNbody[[variable.name]] = FUNbody
}

assign(log , log.datfram , envir = .GlobalEnv)

extract$log = log.datfram[nrow(log.datfram),]
   


## ----------------------------------------------------------------------------------??

extract
} ##----END----##
## ---------------------------------------------------------------------------------------------------------------------??


## ---------------------------------------------------------------------------------------------------------------------??
## EXAMPLES ############################################################################################################??
## ---------------------------------------------------------------------------------------------------------------------??

## -------------------------------------------------------------------------------------------??
dummy = function(){
## This is dummy function - it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## - in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
## -------------------------------------------------------------------------------------------??

log.variables.df.old = log.variables.df

log.variables.df$FUNbody = list(NA, NA, NA)

var.trans = function(x){log(x+1)}  # {x} # 
FUN = var.trans
as.list(body(var.trans))[2][[1]]


log.variables.df$FUNbody
names(log.variables.df$FUNbody) = rownames(log.variables.df)

log.variables.df$FUNbody["dtu"] = as.list(body(var.trans))[2]
log.variables.df

class(log.variables.df$FUNbody[1][[1]])

}

rm(dummy)
