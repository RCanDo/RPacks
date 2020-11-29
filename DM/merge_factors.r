## ---------------------------------------------------------------------------------------------------------------------—•°
## FUNCTIONS HERE
##  merge_factors()
##
## DEPENDENCIES
##
##
## ---------------------------------------------------------------------------------------------------------------------—•°
merge_factors = function( fac.a , fac.b , mode.bit = TRUE , a.first = TRUE , levels = NULL , ordered = FALSE){
## ---------------------------------------------------------------------------------------------------------------------—•°
## Merging two factors into one factor with levels' order common to both factors.
## Ordering from the first factor prevails in case of conflicting orderings,
## or in case a common ordering cannot be inferred unambiguously.
## This function works also for non-factors -- then they are coerced to factors
## with alphabetical order of levels.
##
## Examples of ambiguity in factor levels' order:
##  1.
##  fac.a levels order: a < b < d ,
##  fac.b levels order: a < c < d .
##  We can infer common ordering in two ways:
##   a < c < b < d    or
##   a < b < c < d
##  because there is no information which level, b or c, should come first.
##  2.
##  fac.a levels order: a < b < d < e ,
##  fac.b levels order: a <   c   < e .
##  We can infer common ordering in three ways:
##   a < c < b < d < e    or
##   a < b < c < d < e    or
##   a < b < d < c < e
##  because there is no information about the position of c within group of {b,c,d}.
##
## Arguments
##  fac.a       vector
##  fac.b       vector
##  a.first     if TRUE (default) then values of fac.b will be appended to the vector fac.a,
##              i.e. c(fac.a,fac.b) will be returned;
##              if FALSE then c(fac.b, fac.a) will be returned.
##              In both cases ordering from fac.a prevails.
##  mode.bit    a bit indicating mode in which common ordering is made in case of ambiguities:
##              if TRUE (default) then in above examples the last solution is applied;
##              if FALSE then the first option is taken.
##  levels      you may also specify the final set and order of levels explicitly.
##  ordered     as in factor() -- does a result will be ordered factor? FALSE is default.
##
## Values
##  factor      factor being concatenation of fac.a and fac.b in the order set up in a.first.
##  order_table   table displayng an order of levels in both fac.a and fac.b;
##              empty entries indicates an absence of a given level in a set of levels defined for a given vector.
##              Notice that some levels may not appear in a vector while being present on levels list
##              defined for this factor.
##              In case of option 'levels' is not NULL but is also not covering a set of all levels of both factors,
##              levels which are not covered appear at the end of the list.
##
##
## ---------------------------------------------------------------------------------------------------------------------—•°
  
  name.a = deparse(substitute(fac.a))
  name.b = deparse(substitute(fac.b))
  
  if(!is.factor(fac.a)){fac.a = factor(fac.a)}
  if(!is.factor(fac.b)){fac.b = factor(fac.b)}
  

   ## number of levels in each factor
   la = length(levels(fac.a))
   lb = length(levels(fac.b))
   ## d.f. of levels and their order number in each factor
   lev.df = data.frame(               #! levels will appear in proper order, i.e. taken from the factors definition
                        gr  = factor( c( levels(fac.a) , levels(fac.b) ) )
                      , as.num = c( 1:la,1:lb )
                      , gr.name  = factor(
                                             c(   rep( name.a, length(levels(fac.a)))
                                                , rep( name.b, length(levels(fac.b))) )
                                           , levels = c( name.a , name.b )
                                         )
                      )

   lev.df = as.data.frame( with( lev.df, tapply(as.num,list(gr,gr.name),max) ) )


  if(!is.null(levels)){
     if(any(!rownames(lev.df)%in%levels)){
      warning("A set of levels passed to the function do not cover a set of all levels of both factors.
NAs appeared in a resulting variable.")
     }
     lev.df.ok = lev.df[ levels,]
     lev.df.na = lev.df[!rownames(lev.df)%in%levels,]
     lev.df = rbind(lev.df.ok,lev.df.na)

  }else{
  

     common =  (levels(fac.a) %in% levels(fac.b)) && (levels(fac.b) %in% levels(fac.a))  ## all must be TRUE
     ## common       if TRUE this means that all levels are common to both data frames at the levels' definition,
     ##              while still some levels may be absent in data.
     if(common){
         levels = levels(fac.a)
         lev.df = as.data.frame( lev.df[ order(lev.df[,1]) , ] )
         # a = length(fac.a); b = length(fac.b)
         # if(a.first){
         #    fac = fac.a
         #    fac[a + 1:b] = fac.b
         #      #!  If factors have different order of levels then the ordering from fac.a is inherited.
         # }else{
         #    fac = fac.b
         #    fac[b + 1:a] = fac.a
         #      #!  If factors have different order of levels then the ordering from fac.a is inherited.
         # }
         ##
     }else{

         lev.df.ok = as.data.frame( lev.df[!is.na(lev.df[,1]) , ] )
         lev.df.na = as.data.frame( lev.df[ is.na(lev.df[,1]) , ] )

         lev.df.ok = as.data.frame( lev.df.ok[ order(lev.df.ok[,1]) , ] )
         lev.df.na = as.data.frame( lev.df.na[ order(lev.df.na[,2]) , ] )

         if(nrow(lev.df.na)>0){
             ##
             if(mode.bit){
                 for( k in 1:nrow(lev.df.na) ){
                    whichgreater = which(lev.df.ok[,2] > lev.df.na[k,2])
                    if( length(whichgreater)>0 ){
                        mingreater =  min( whichgreater )
                        #
                        if(mingreater==1){
                            lev.df.ok = rbind( 
                                               as.data.frame( lev.df.na[ k ,] )
                                             , lev.df.ok
                                         )                        
                        }else{
                            lev.df.ok = rbind( lev.df.ok[ 1:(mingreater-1) ,]
                                             , as.data.frame( lev.df.na[ k ,] )
                                             , lev.df.ok[ mingreater:nrow(lev.df.ok) , ]
                                         )
                        }
                    }else{
                      lev.df.ok = rbind(
                                          lev.df.ok[ 1:nrow(lev.df.ok) , ]
                                        , as.data.frame( lev.df.na[ k ,] )
                                       )
                    }
                 }
             ##
             }else{
                 for( k in 1:nrow(lev.df.na) ){
                    whichless = which(lev.df.ok[,2] < lev.df.na[k,2])
                    if( length( whichless )>0 ){
                        maxless =  max( whichless )
                        #
                        if(maxless == nrow(lev.df.ok)){
                            lev.df.ok = rbind( 
                                               lev.df.ok
                                             , as.data.frame( lev.df.na[ k ,] )
                                         )
                        }else{
                            lev.df.ok = rbind( lev.df.ok[ 1:maxless ,]
                                              , as.data.frame(lev.df.na[ k ,])
                                              , lev.df.ok[ (maxless+1):nrow(lev.df.ok) , ]
                                            )
                        }
                    }else{
                      lev.df.ok = rbind(
                                          as.data.frame(lev.df.na[ k ,])
                                        , lev.df.ok[ 1:nrow(lev.df.ok) , ]
                                      )
                    }
                 }
             }
             ##
         }
         levels = rownames(lev.df.ok)
         lev.df = lev.df.ok
      }
   }
   

   if(a.first){
      fac =  factor(c(as.character(fac.a),as.character(fac.b)),levels = levels, ordered = ordered)
   }else{
      fac =  factor(c(as.character(fac.b),as.character(fac.a)),levels = levels, ordered = ordered)
   }
  
   result = list(  factor = fac
                 , order_table = as.table(t(as.matrix(lev.df)))  #! unfortunately - this is data.frame  and as.table() alone doesn't work...
                )
                
   result
} ##----END----##
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
## EXAMPLES ############################################################################################################—•°
## ---------------------------------------------------------------------------------------------------------------------—•°

## ---------------------------------------------------------------------------------------------------------------------—•°
dummy = function(){
## This is dummy function —— it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## —— in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
## ------------------------------------------------------------------------------------------------—•°

 ## RELOADER —— before it works you need to source("PacksAK.R"); it's best to use DM.R within pack's dir.
 loadPacksAK("DM")

## ------------------------------------------------------------------------------------------------—•°

fac.a = factor(  sample(c("10","20" ,"50" ,"200","1000","5000") ,30 ,replace=TRUE )
               , levels=c("10","20" ,"50" ,"200","1000","5000") ,ordered=TRUE )
fac.b = factor(  sample(c("20","100","200","500","2000","5000") ,30 ,replace=TRUE )
               , levels=c("20","100","200","500","2000","5000") ,ordered=TRUE )

## common levels ( e.g. when cut with the same breaks is applied to two different vectors 
##                 - then empty loevels may appear in both cases but levels definition and order is common )
fac.a = factor( sample(letters[c(1,3,4,7)],30,replace=TRUE), levels=letters[c(7,4,3,2,1)], ordered=TRUE )
fac.b = factor( sample(letters[c(2,3,6)],30,replace=TRUE), levels=letters[c(6,4,1,3,2)], ordered=TRUE )

##
fac.a =  sample(letters[c(1,3,4,7)],10,replace=TRUE)
fac.b =  sample(letters[c(2,3,6)], 10,replace=TRUE)

fac.a
fac.b

names(fac.a) = as.numeric(fac.a)
names(fac.b) = as.numeric(fac.b)

c(fac.a,fac.b)

( u.lev = union(levels(fac.a),levels(fac.b)) )


merge_factors(fac.a,fac.b)
merge_factors(fac.a,fac.b, mode.bit = FALSE)
merge_factors(fac.a,fac.b, levels = union(levels(fac.a),levels(fac.b)))

fac.a = g.ref; fac.b = g.val

g.ref = fac.a; g.val = fac.b
merge_factors(g.val,g.ref)

}
## ---------------------------------------------------------------------------------------------------------------------—•°
rm(dummy)
