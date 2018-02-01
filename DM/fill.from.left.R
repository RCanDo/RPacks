########################################################################################################################•°
## FUNCTIONS HERE
##  fill.from.left()
##  fillFromLeft()  ## alias
##
## DEPENDENCIES
##
########################################################################################################################•°
fill.from.left = function(cups,pot){
########################################################################################################################•°
##
##  cups      vector of cups giving theirs capacities; may be negative;
##  pot       a capacity we'd like to put into the cups from left to right; may be negative;
## Result is a vector comprising as many cups as is needed to fill in all the pot content.
## Each cup is given as its whole capacity except the last one which gives the capacity used.
## If there is not enough cups for the whole pot then the rest of the pot (i.e. leftovers for which there was no room in the cups)
## is adjoined to the cups under the name names(pot)
## This mean that result will always sum to 'pot'.
########################################################################################################################•°
   lencups = length(cups)
   cups.csum = cumsum(cups)
   pot = pot[1]
   if(is.null(names(pot))){names(pot) <- "pot"}


   if(last(cups.csum)<pot){
      cups.tofill = c( cups , pot - rev(cups.csum)[1] )
   }else if(last(cups.csum)==pot){
      cups.tofill = cups
   }else{
      cups.csum.less.pot = cups.csum - pot
      cups.tofill.bin = cups.csum.less.pot < 0

      cups.tofill.last = last(-cups.csum.less.pot[cups.tofill.bin])   ## last defined in  last.r - returns the last element of an object
      if(is.na(cups.tofill.last)){  ## iff the whole pot fits in the first cup
         cups.tofill.last = pot
      }

      cups.tofill.bin = c(TRUE,cups.tofill.bin[-length(cups.tofill.bin)])

      cups.tofill = cups[cups.tofill.bin]
      names(cups.tofill.last) = names(last(cups.tofill))
      cups.tofill[length(cups.tofill)] = cups.tofill.last
   }

   cups.tofill

} ##----END----##
###############################################################################

fillFromLeft = function(...)  ## alias
{
   fill.from.left(...)
} ##----END----##
########################################################################################################################•°


########################################################################################################################•°
## EXAMPLES ############################################################################################################•°
########################################################################################################################•°

##############################################################################################•°
dummy = function(){
## This is dummy function - it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## - in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
##############################################################################################•°

cups = sample(1:5) ; names(cups) = letters[1:5] ; cups
pot = 10
fill.from.left(cups,pot)
fillFromLeft(cups,pot)

pot = 15
fillFromLeft(cups,pot)

pot = 20
fillFromLeft(cups,pot)

pp = 20 ; names(pp) = "potek" ; pp
fillFromLeft(cups,pp)

pp = -1 ; names(pp) = "potek" ; pp
fillFromLeft(cups,pp)  ## little nonsense


xx = sample(-5:-1) ; names(xx) = letters[1:5] ; xx
pp = 10 ; names(pp) = "potek" ; pp
fillFromLeft(xx,pp)

}

rm(dummy)

########################################################################################################################•°
