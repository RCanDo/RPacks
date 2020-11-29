merge.ordered = function(fac.a,fac.b){
  ## Merging two ordered factors into one ordered factor with levels' order common to both factors.
  ## Ordering from the first factor prevails in case of conflicting orderings in given factors,
  ## or in case the common ordering cannot be inferred unambiguously.
  ## This function works also for unordered factors and even for non-factors -- then the ordering is mainly alphabetical.
  
  if(!is.factor(fac.a)){fac.a = factor(fac.a)}
  if(!is.factor(fac.b)){fac.b = factor(fac.b)}
  
  common =  (levels(fac.a) %in% levels(fac.b)) && (levels(fac.b) %in% levels(fac.a))  ## all must be TRUE
  ## common       if TRUE this means that all levels are common to both data frames at the level definition,
  ##              while some levels may be absent in data.
  #!  If factors have different order of levels then the ordering from fac.a is inherited. 
  
   la = length(levels(fac.a))
   lb = length(levels(fac.b))
   lev.df = data.frame(
                              gr  = factor( c(levels(fac.a) , levels(fac.b)) )
                            , as.num = c( 1:la,1:lb )
                            , gr.name  = c( rep("reference",length(levels(fac.a))) , rep("valuated",length(levels(fac.b))) ) 
                            )

   lev.df = with( lev.df, tapply(as.num,list(gr,gr.name),max) )
   
  if(common){
    a = length(fac.a); b = length(fac.b)
    fac = fac.a
    fac[a + 1:b] = fac.b
      #!  If factors have different order of levels then the ordering from fac.a is inherited. 
   lev.df.ok = as.data.frame( lev.df[order(lev.df[,1]),] )
   ##
  }else{
   
   lev.df    = as.data.frame( lev.df[order(lev.df[,1]),] )
   lev.df.na = lev.df[ is.na(lev.df[,1]),]
   lev.df.ok = lev.df[!is.na(lev.df[,1]),]
   if(nrow(lev.df.na)>0){
     for( k in 1:nrow(lev.df.na) ){
        if(length(which(lev.df.ok[,2] < lev.df.na[k,2]))>0 ){
          maxless =  max(which(lev.df.ok[,2] < lev.df.na[k,2]))
          lev.df.ok = rbind( lev.df.ok[ 1:maxless ,]
                            , as.data.frame(lev.df.na[ k ,])
                            , lev.df.ok[ (maxless+1):nrow(lev.df.ok) , ]
                          )
        }else{
          lev.df.ok = rbind( 
                              as.data.frame(lev.df.na[ k ,])
                            , lev.df.ok[ 1:nrow(lev.df.ok) , ]
                          )
        } 
     }
   }
   fac =  factor(c(as.character(fac.a),as.character(fac.b)),levels = rownames(lev.df.ok), ordered = TRUE)
 }
  
   result = list(  new_ordered = fac
                 , order_table = lev.df.ok
                )
                
   result
} ##----END----###

## -------------------------------------------------------------------------------------------------

dummy = function(){

fac.a = factor(sample(c("10","20","50","200","1000","5000"),30,replace=TRUE),levels=c("10","20","50","200","1000","5000"),ordered=TRUE)
fac.b = factor(sample(c("20","100","200","500","2000","5000"),30,replace=TRUE),levels=c("20","100","200","500","2000","5000"),ordered=TRUE)

## common levels ( e.g. when cut with the same breaks is applied to two different vectors 
##                 - then empty loevels may appear in both cases but levels definition and order is common )
fac.a = factor( sample(letters[c(1,3,4,7)],30,replace=TRUE), levels=letters[c(7,6,4,3,2,1)], ordered=TRUE )
fac.b = factor( sample(letters[c(2,3,6)],30,replace=TRUE), levels=letters[c(6,4,1,3,2,7)], ordered=TRUE )

##
fac.a =  sample(letters[c(1,3,4,7)],30,replace=TRUE)
fac.b =  sample(letters[c(2,3,6)], 30,replace=TRUE)

fac.a
fac.b

names(fac.a) = as.numeric(fac.a)
names(fac.b) = as.numeric(fac.b)

c(fac.a,fac.b)

( u.lev = union(levels(fac.a),levels(fac.b)) )


fac.a = g.ref
fac.b = g.val

   
merge.ordered(fac.a,fac.b)

   names(attr(levels.as.numeric,"dimnames")) = c("group","portfolio")
}
