########################################################################################################################—•°
select_levels <- function(datfram,variable,print=TRUE,refactor=TRUE){
###################################################################################################—•°
##
###################################################################################################—•°

result <- structure( list() , class="select_levels" )


cnams <- colnames(datfram)

if( is.numeric(variable) ){
   if(!variable%in%1:length(cnams)){
      stop("If 'variable' is numeric then it must be numeric position of variable within 'datfram'." )
   }
   variable <- cnams[variable]
}else if( is.character(variable) ){
   if(!variable%in%cnams){
      stop("If 'variable' is character then it must name of a variable from 'datfram'.")
   }
}else{
   stop("'variable' must be the name of one of 'datfram' variable or its numeric position.")
}


level <- sort(unique( datfram[,variable] ))
selection <- rep(0,length(level))

t0 <- data.frame( level = level , selection = selection )

t0 <- edit(t0)


selected <- t0[t0[,"selection"]==1,"level"]

condition <- paste( variable," %in% c('",paste( selected , collapse="','" ),"')",sep="")

if(print){
   print(t0)
   print(selected)
}

result$variable <- variable
result$table <- t0
result$selected <- selected
result$condition <- parse(text=condition)
result$idx <- datfram[,variable] %in% selected
datfram <- subset( datfram , result$idx )

if(refactor){
datfram <- capply( datfram
			 		  , class = "factor"
				 	  , FUN = factor
					  , within = TRUE
					  )
}
result$datfram  <- datfram

result

}

########################################################################################################################—•°
print.select_levels <- function( selev , ... ){
###################################################################################################—•°
##
###################################################################################################—•°
indentr( selev , rm="datfram" , compact=7 )

}