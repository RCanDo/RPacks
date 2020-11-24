########################################################################################################################??
## FUNCTIONS HERE
##  coldef()
##  coldefBlack()
##  marginsMy()
##  parMy()
##
## DEPENDENCIES
##
## TODO
##
########################################################################################################################??

coldef = function(coldef = "white"){
## Defines colorstyle of plots.

#colset=c("black", "white", "grey", "")
#if(!coldef %in% colset){
#warning("Option '", coldef, "' not defined! \n
#   Set to default 'white' which is also R's  default.")
#coldef = "white"  }

if(coldef == "black"){
  par(fg="yellow", bg="black", col.axis="white", col.lab="grey", col.main="white")
}else if(coldef == "grey"){
  par(fg="black", bg="grey", col.axis="black", col.lab="black", col.main="black")
}else if(coldef == "white"){
  par(fg="black", bg="white", col.axis="black", col.lab="black", col.main="black")
}else{
  par(fg="black", bg=coldef, col.axis="black", col.lab="black", col.main="black")
}

}


########################################################################################################################??
marginsMy = function(){
 par(	 mar=c(2.2, 2, 3, 1)    ## width of the bottom, left, top, right margin
			, tck=.01
			, mgp=c(1,.1, 0)       ## position of axis components ##
	   )
}
coldefBlack = function(){ coldef("black") }
parMy = function(){ coldefBlack() ; marginsMy() }


##  par.my()
##    par(mfrow=c(1, 3), oma = c(0, 0, 3, 0),  cex.main=1, cex.axis=.8, cex.lab=.8)
##           ...      , outer margins (down, left, top, right), ...
##          mtext("Zawarto?? we krwi [?g/l]", outer=TRUE)   -- outer title
##    par(mfg = c(2, 1, 3, 4))  -- multi-figure position - (row, column, rows, columns)
##                                                       position  | size of table of figures/plots
##    pdf(file="file.pdf", height=9, width=7)  -- perhaps maximal size on A4
##
## options(contrasts = c("contr.treatment", "contr.treatment"))
## options(show.signif.stars=FALSE)
##
## sink("file.txt", split=TRUE, append=FALSE)
##    ...
## sink()
##
##    l = c(0,.4,.75, 1)
##    for(i in 1:3){  par( fig = c(  l[i], l[i+1], 0,    1 ), new=(i>1))
##                    plot(...) }    left  right   down  up
##    mtext("...", outer=TRUE )
##

# read = function(file){read.table(paste(PATHdata, file, header=TRUE)}