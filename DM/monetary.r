monetary = function( maximum , min.power = NULL , power.range = NULL , base = c(1,2,5)
                     , merge.last = 0 , merge.first = 0){
## Creates sequence with 'monetary' values from 0 to maximum.
## Arguments
##  maximum       maximum value of the sequence;
##  min.power     minimum power of 10 to begin with next to 0;
##                if left NULL then is set to  min(floor(log(base,10)));
##                ignored when 'power.range' is not NULL;
##  power.range   how much the minimum power of 10 used to create first sequence values
##						should be less then the power of 'maximum' ( floor(log(maximum,10)) );
##                helps to control length of the sequence independently of maximum;
##                if not left NULL then 'min.power' is ignored;
##                should be integer sign of which will be ignored;
##  base          base of the sequence, default is c(1,2,5).
##  merge.first   how many first (from the left) intervals merge into one;
##                default is 0; if 1 then the first interval will be merged with the second,
##                if 2 then the first 2 intervals will be merged with the third, and so on;
##                thus if give number k then k+1 intervals will be merged together.
##                if exceeds n-2 (n is length of the final result of monetary() without any merging)
##                then is lessend to this value and we obtain just one interval (with a warning),
##                i.e. sequence of length 2 with only first and last value of the original result.
##  merge.last    like  merge.first but concernes last k intervals.
## Default minimum power of 10 is  min(floor(log(base,10)))
## what for deafault base is 0 and gives sequence of the type (0,1,2,5,10,20,50,...,maximum).
## You may also define custom base of division, e.g. base = c(1,3,6), 
## then the function will generate sequence of the type (0,1,3,6,10,30,60,...,maximum).
## If you put base = c(12,1,3,16), then only c(1,3) will be taken.
## Similarily, base = c(12,.1,.03,16) will be cut to c(.03), i.e. 
## only values having the lowest integer part of logarithm to base 10 are considered.
## In both cases, minimum power of 10 is  min(floor(log(base,10)))
## as long as min.power = NULL (default).
## After choosing the proper subset of base
## the base is normalized to have the integer part of logarithm to base 10 equal to 0.

base = sort(base)  
log.base = floor(log(base,10))
min.log.base = min(unique(log.base))
if(length(unique(log.base))>1){
  warning("Vector 'base' contains values having different integer part of logarithm to base 10.
  Only values having the lowest integer part of logarithm were taken.")
  base = base[which(log.base == min.log.base)]
} 

if(min.log.base!=0){ base = base*10^(-min.log.base) }   ## normalization to interval (1,10)

if(is.null(min.power)){ min.power = min.log.base}

max.power = floor(log(maximum,10))

if(is.null(power.range)){min.power = round(min.power)}else{min.power = max.power - abs(round(power.range))}

min.power = min(min.power,max.power)

powers = seq(min.power,max.power,1)
seq1 = as.vector(outer(base,10^powers))
seq1 = c(0, seq1)
seq1 = c(seq1[seq1<maximum],maximum)

n = length(seq1)
idx=rep(TRUE,n)
if(merge.first>0){
   if(merge.first>n-2){
      merge.first = n-2
      warning("'merge.first' exceeded the length of the sequence minus 2 and was set to this value.")
   }
   idx = 1:n
   exclude = (1:(1+merge.first))[-1]
   idx.f = ! idx%in%exclude
   idx = idx&idx.f
}
if(merge.last>0){
   if(merge.last>n-2){
      merge.last = n-2
      warning("'merge.last' exceeded the length of the sequence minus 2 and was set to this value.")
   }
   idx = 1:n
   exclude = (1:(1+merge.last))[-1]
   idx.l = rev(! idx%in%exclude)
   idx = idx&idx.l
}
seq1 = seq1[idx]

seq1
}

##############################################
## examples
dummy=function(){

monetary(64736)
monetary(64736,0)  ## default minimum power is 
  min(floor(log(c(1,2,5),10)))
monetary(64736,1)  ## min.power may be changed
monetary(64736,base=c(1,2,5)*10)  ##
monetary(64736,-1)
monetary(64736,base=c(1,2,5)/10)
monetary(64736,-2)
monetary(64736,2)
monetary(64736,3)
monetary(64736,4)
monetary(64736,5)   ## the minimum power of 10 cannot exceed the power of maximum
monetary(64736,6)   ##  "


monetary(1)
monetary(1,0)   ## default minimum power is   min(floor(base,10)))
monetary(1,1)   ## the minimum power of 10 cannot exceed the power of maximum
monetary(1,2)
monetary(4)
monetary(4,0)   ## default minimum power is   min(floor(base,10)))
monetary(4,1)   ## the minimum power of 10 cannot exceed the power of maximum
monetary(4,2)

monetary(10,0)
monetary(10,1)
monetary(10,2)  ## the minimum power of 10 cannot exceed the power of maximum
monetary(35,0)
monetary(35,1)
monetary(35,2)

monetary(1,-1)
monetary(1,-2)

monetary(.1,1)   ## the minimum power of 10 cannot exceed the power of maximum
monetary(.1,0)   ##  "
monetary(.1,-1)
monetary(.1,-2)

monetary(64736,base=c(1,3,6))
monetary(64736,1,base=c(1,3,6))

monetary(64736,0,base=c(1,3,13))   ## This is improper but function will work after modifying base and giving warning.
monetary(64736,-1,base=c(13,3,1))  ## 

monetary(64736,1,1)   ## power.range prevails
monetary(64736,0,1)
monetary(64736,1)                 

monetary(64736,base=1)
monetary(64736,base=.1)     ## now the min.power is taken from base which is -1 here
monetary(64736,base=.01)
monetary(64736,base=10)
monetary(64736,base=100)

monetary(64736,base=c(1,2.5,5))         ## not advised -- default base is much better!
monetary(64736,base=round(runif(3),3))  ## a little nonsense, just possibility

###################
## power.range not null
monetary(64736 , power.range = 2)
monetary(64736 , power.range = -2) ## the same because sign is ignored
monetary(64736 , power.range = 0)
monetary(64736 , power.range = 1)
monetary(64736 , power.range = 10)  ## quite a long sequence
##
monetary(64736 , min.power = 1 ) ##
monetary(64736 , min.power = 1 , power.range = 2) ## min.power is ignored when  power.range is not null

###################
## sometimes we need to merge last or first intervals
monetary(64736 , power.range = 2)
monetary(64736 , power.range = 2, merge.last = 1)
monetary(64736 , power.range = 2, merge.first = 1)
monetary(64736 , power.range = 2, merge.first = 2)
monetary(64736 , power.range = 2, merge.first = 10)

}