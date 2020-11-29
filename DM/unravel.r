unravel=function(fac,code,sep="",new.names = NULL,ordered = is.ordered(fac)){
## Unravelling factor fac to N new factors for which the old one is their full interaction / product.
## i.e. is obtained by multiplication of all new factors:
##    fac = fac1*...*facN,   with each factor having many levels.
##
## fac   full-length factor (the one we want to unravel)
## code  character vector with each entry of the same length.
##       A k-th letter in each entry will be used as a label for the level of k-th factor
##       we want to obtain. Thus k-th factor will have as many levels as there are
##       distinct letters used in k-th position of each entry of 'code' vector.
##       The levels of the 'fac' should be used as names of 'code' entries, i.e.
##          names(code) <- levels(fac).
##       If is.null(names(code)) == TRUE, then the above will be done automatically (1).
##       To avoid problems with ordering it's strongly recommended to pass the 'code'
##       vector with named entries, in which case the user may choose the order of levels.
##       It must be also recognised that  levels(fac)  may return more names than  unique(fac)
##       and it is up to user what is to be unravelled:
##          instances of levels of 'fac',  i.e. unique(fac), or
##          all possible levels of 'fac',  i.e. levels(fac).
##       This function is universal but if we pass 'code' with codes for new factors
##       matching only instances (unique(fac)) and without names (is.null(names(code)) == TRUE),
##       then an ERROR occure if there are more possible levels then their instances:
##          length(levels(fac)) > length(unique(fac)).
## sep   a character used to separate names of new factors within each entry of 'code'.
##       Default is empty string "", i.e. every single letter will be treated as a name of
##       level of consecutive factor, k-th letter for k-th factor.
## new.names   names to be given for new factors.
## ordered     binary, default is is.ordered(fac);
##             New factors will be ordered if ordered is TRUE with ordering inherited from 'fac'
##             It means that ...

if( is.null(names(code)) ){names(code) <- levels(fac)}           ##  (1)
if( ordered ){code = code[ levels(fac)[levels(fac) %in% names(code)] ]}
ll = strsplit(code,split=sep,fixed=TRUE)
nf = unique(unlist(lapply(ll, length)))     ## do all elements in 'code' have the same length
      ## which should be a number of new factors
if(length(nf)>1) stop("Different number of new factors in consecutive 'code' entries.")
mm = t(as.data.frame(ll))    ## notice the row names! later in use
newfac = data.frame(fac)
for(k in 1:nf){
   f_k = factor( mm[,k][as.character(fac)], ordered=ordered, levels=unique(mm[,k]) )
   newfac = cbind(newfac,f_k)
}
newfac=newfac[,-1];
if (is.null(new.names)) {
   colnames(newfac) = paste("fac",1:nf,sep="")
      } else {colnames(newfac) = new.names}
   return(newfac)
}


dummy = function(){
## Example
## 1°
   (fA = LETTERS[sample(1:2,12,replace=TRUE)])
   (fa = letters[sample(1:2,12,replace=TRUE)])
   (fac = factor(paste(fa,fA,sep="")))   #! repeat until all 4 combinations appear!

   code = c("aa","ab","ba","bb")
      names(code) = unique(fac)   ## may be not what we need
         code
      names(code) = sort(unique(fac))     ## may depend on locale
         code
   ## It is better to do it directly
   names(code) =  c("aA","aB","bA","bB")
   code
 unfac = unravel(fac,code);    data.frame(unfac,fac)

##  this is repeated from the function body:
      sep=""
   (ll = strsplit(code,split=sep,fixed=TRUE))
   (mm = t(as.data.frame(ll)))
      class(mm)
      (nf = unique(unlist(lapply(ll, length))))
      (f1 = factor(mm[,1][fac]))
      (f2 = factor(mm[,2][fac]))
       newfac = data.frame(f1,f2)
       rownames(newfac) = fac     #! row names in data.frame must be unique
   newfac = data.frame(fac)
   for(k in 1:nf){   newfac = cbind(newfac,factor(mm[,k][as.character(fac)])) }
    newfac=newfac[,-1]; colnames(newfac) = paste("fac",1:nf,sep="")

   (newf = cbind(fac,newfac))        # just for check
   summary(newf)
#
}
