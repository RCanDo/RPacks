## ---------------------------------------------------------------------------------------------------------------------•°
## FUNCTIONS HERE
##  refine.assignment()
##
## DEPENDENCIES
##  update.df.r
##    update.df() , ...
##  addid.r
##  fill.from.left.r
##  last.r
##
## ---------------------------------------------------------------------------------------------------------------------•°
refine.assignment = function( datfram , operator.var , group.var , weight.var , tolerance ){
## ---------------------------------------------------------------------------------------------------------------------•°
##  datfram    data.frame
##  operator.var   name of a column with operator's labels
##  group.var      name of a column with group's labels
##  weight.var     name of a column with weights labels
##  tolerance      nonnegative integer
##
## The aim is to optimize assignments of cases to operators according to following objectives:
##   1° each operator should have roughly the same amount of cases (with accuracy of 'tolerance');
##   2° number of different groups for each operators should be minimal, i.e.
##   3° try to join small gr.ops  (gr.op := group by operator) within one group
##   4° try not to divide big gr.ups
##   5° fat cases are difficult to tackle -- we move them only when other means failed;
##   6° we'd like to change as little assignments as possible
## ---------------------------------------------------------------------------------------------------------------------•°

require(adagio)

## ---------------------------------------------------------------------------------------------------------------------•°
## subroutines
## ---------------------------------------------------------------------------------------------------------------------•°
## group.to.method()
group.to.spread = function(  mode          ## "big" , "small"
                           , spread        ## "all" (all amount of op.up), "any"  -- relevant only in mode = "big" (?)
                           , no_lumps      ## TRUE, FALSE
                          ){
## ---------------------------------------------------------------------------------------------------------------------•°
## choosing the group to move gr.ops within (move to other operators)
## ---------------------------------------------------------------------------------------------------------------------•°

( lumps = tab.lumps.bin[names(op.up),] & no_lumps )    ##  if no_lumps == FALSE then all is FALSE as if there were no lumps
( op.up.row = op.up.row[ op.up.row>0  & !lumps] )   ## we EXCLUDE lumps !!

result = length(op.up.row)>0

if(result){
   if( mode == "small" ){
      ( op.up.row = sort(op.up.row) )   ##
      ( op.up.row.ok = op.up.row[ op.up.row <= op.up + tolerance ] )
   }else if(mode=="big"){
      ( op.up.row = rev(sort(op.up.row)) )   ##
      ( op.up.row.ok = op.up.row[ op.up.row > op.up + tolerance ] )
   }else{ stop("'mode' must be \"big\" or \"small\".") }

   result = length(op.up.row.ok)>0

   if(result){

      ( op.up.tab.bin.down.ok  = tab.bin[ names(op.weights.down) , names(op.up.row.ok) , drop = FALSE] )    ## never empty!   because of while() conditions
         #

      if( spread == "all" ){
          ## groups of which the whole amount may be moved from op.up (however, only one of them at time)
         ( op.up.row.ok.bin = pmin(op.up.row.ok,op.up) < apply( op.up.tab.bin.down.ok , 2 , FUN = function(x){ sum(op.weights.down[x])} ) + tolerance )
          #!#!#!
      }else if( spread == "any" ){
         ( op.up.row.ok.bin = apply( op.up.tab.bin.down.ok , 2 , any )  )  ## groups present in any of down operators
      }else{
         stop("'spread' must be \"all\" or \"any\".")
      }

      result = sum(op.up.row.ok.bin)>0

         print(op.up.row.ok.bin)

      if(result){
         gr.up = op.up.row.ok[op.up.row.ok.bin][1]
         ## which operators whith what capacities are available
         ( capacities = op.weights.down[op.up.tab.bin.down.ok[,names(gr.up)]] )
         ( capacities = c( gr.up - min(op.up,sum(capacities))  ## > 0   always!  if mode=="big" (by definition)
                                                               ## may be <= 0    otherwise
                         , capacities
                         ) )
           names(capacities)[1] = names(op.up)       ## we adjoin the source operator (from which we'd like to throw cases off)
               capacities = capacities[capacities>0]           ## important if  mode=="small"
         ( capacities = rev(sort(fill.from.left( capacities + round(2*(1+tolerance/length(capacities)))/2, gr.up ))) )   ## what weight should be dropped to which operators <= theirs capacities
      }
   }
}

## returns to parent.env
if(result){
  gr.up <<- gr.up
  capacities <<- capacities
}

## returns TRUE, FALSE
result

} ##----END----##  of  group.to.spread()
## ---------------------------------------------------------------------------------------------------------------------•°


## ---------------------------------------------------------------------------------------------------------------------•°
## spread.method()
spread.method = function( force_lumps = FALSE , rollback = !force_lumps ){
## ---------------------------------------------------------------------------------------------------------------------•°
## Moving gr.ops within given group.
## This subroutine needs the following objects from environment
##    op.up
##    gr.up
##    capacities   ...
## and
##    datfram    the main data frame -- an input of the whole function
##    tab
##
## ---------------------------------------------------------------------------------------------------------------------•°

   #############################################################################•°

   #print(any(is.na(datfram[,operator.var] == names(op.up))))
   #print( unique(datfram[,group.var]))

   ( tochange.df = datfram[ datfram[,operator.var] == names(op.up) & datfram[,group.var] == names(gr.up) , ] )
            #  print(tochange.df)
   ( tochange.df = tochange.df[do.call(order,c(tochange.df[,weight.var,drop=FALSE],"decreasing"=TRUE)),] )  #???
   #  ( tochange.cases = var2vec( tochange.df , "weight" , names = 0  ) )

   #############################################################################•°
   idx.out_k = character(0)  ## OK!!!
   tab.lumps.now = tab*0

   do_break <- FALSE

   if(!force_lumps){

      for( k  in 1:length(capacities) ){  ##  3. LOOP_2  ##  k = 1
         ( cap_k = capacities[k] )
         tochange.df_k = tochange.df[setdiff(rownames(tochange.df),idx.out_k),]  ## for k=1 it's the same df


         if( any(tochange.df_k[,weight.var] > cap_k) ){  ## case too fat and won't fit in any next cap_k because op.tofill is ordered!!!
            tab.lumps.bin[names(op.up),names(gr.up)] <- do_break <- TRUE
            tab.lumps.bin <<- tab.lumps.bin
            break
         }else if( k == length(capacities) ){  ## if we have the last item then there's nothin' to think about
            (idx.names  = rownames(tochange.df_k))
         }else{
            chosen.cases = knapsack(  w = tochange.df_k[,weight.var]    ## weight
                                    , p = tochange.df_k[,weight.var]^2  ##pmax(tochange.df_k[,"weight"]-1,1/dim(tochange.df_k)[1])
                                                                        ## profit ; if p = w-1 then 1 big case is always preferred over several smaller (what is our objective)
                                                                        ## however, we also need cases having weight 1 and 0 so we must put some profit to them;
                                                                        ##  the profit  1/(nr.of.cases)  for 1's and 0's  implies that both cases will be taken as soon as possible but not instead of heavier cases
                                    , cap = cap_k                       ## capacity
                                    #!#!#!#!#!#!   cap >= max(w)   ## otherwise an ERROR occur !!!
                                   )
               chosen.cases
            (idx.names  = rownames(tochange.df_k)[chosen.cases$indices])
         }

         ## update is made on  tochange.df
         if(is.numeric(tochange.df[ ,operator.var])){
            tochange.df[idx.names,operator.var] <- as.numeric(names(cap_k))
         }else{
            tochange.df[idx.names,operator.var] <- names(cap_k)
         }

         idx.out_k = c(idx.out_k,idx.names)

      }  ##  END OF  3. LOOP_2  ##
      ##########################################################################•°
      if( !do_break | (do_break & !rollback) ){ ##
         datfram <<- update.df( datfram , tochange.df , variable = operator.var , id=0 , action="update" )
      }
      # rm(tochange.df,tochange.df_k)
      ##########################################################################•°

   }else{  ## force_lumps == TRUE

      do_break = FALSE
      for( k  in 1:length(capacities) ){  ##  3. LOOP_2  ##  k = 2
         ( cap_k = capacities[k] )
         tochange.df_k = tochange.df[setdiff(rownames(tochange.df),idx.out_k),]  ## for k=1 it's the same df

         if( k == length(capacities) | sum(tochange.df_k[,weight.var]) <= cap_k ){  ## if we have the last item or the whole amount is less then cap_k then there's nothin' to think about
            (idx.names = rownames(tochange.df_k))
            do_break <- TRUE
         }else{
            if( tochange.df_k[,weight.var][1] > cap_k ){  ## case too fat and won't fit in any next cap_k because op.tofill is ordered!!!
               idx.names = rownames(tochange.df_k)[1]  ## then we choose just one case
               tab.lumps[names(op.up),names(gr.up)] = tab.lumps[names(op.up),names(gr.up)] + 1  ## history of lumps for this group
               tab.lumps.now[names(cap_k),names(gr.up)] = 1                                      ## new lumps position
               tab.lumps.bin[,names(gr.up)] = FALSE    ## for this group lump was just moved
            }else{
               chosen.cases = knapsack(  w = tochange.df_k[,weight.var]                      ## weight
                                       , p = tochange.df_k[,weight.var]^2 #5.1 - 1/pmax(tochange.df_k[,"weight"],.2)   #pmax(tochange.df_k[,"weight"]-1,1/dim(tochange.df_k)[1])  ## profit ; if p = w-1 then 1 big case is always preferred over several smaller (what is our objective)
                                                                                                                        ## however, we also need cases having weight 1 and 0 so we must put some profit to them;
                                                                                                                        ##  the profit  1/(nr.of.cases)  for 1's and 0's  implies that both cases will be taken as soon as possible but not instead of heavier cases
                                       , cap = cap_k                              ## capacity
                                       #!#!#!#!#!#!   cap >= max(w)   ## otherwise an ERROR occur !!!
                                      )
                  chosen.cases
               (idx.names  = rownames(tochange.df_k)[chosen.cases$indices])
            }
         }

         ## update is made on  tochange.df
         if(is.numeric(tochange.df[ ,operator.var])){
            tochange.df[idx.names,operator.var] <- as.numeric(names(cap_k))
         }else{
            tochange.df[idx.names,operator.var] <- names(cap_k)
         }

         idx.out_k = c(idx.out_k,idx.names)
         if(do_break){break}

      }  ##  END OF  3. LOOP_2  ##
      ##########################################################################•°
      #if(not_break){ ##
        datfram <<- update.df( datfram , tochange.df , variable = operator.var , id=0 , action="update" )
      #}
      # rm(tochange.df,tochange.df_k)
      ##########################################################################•°
   }
   tab.lumps <<- tab.lumps
   tab.lumps.now <<- tab.lumps.now
} ##----END----##  of  spread.method()
## ---------------------------------------------------------------------------------------------------------------------•°

## ---------------------------------------------------------------------------------------------------------------------•°
## PRE-CALCULATIONS
   #
#tolerance = 2
   #
   #   with(datfram,addmargins(table(id_operator,feature_a)))
tab <-  tapply( datfram[,weight.var] , list(datfram[,operator.var],datfram[,group.var]) , sum )
tab <- ifelse(is.na(tab),0,tab)
            ##tab = with( datfram , { tab <- tapply(weight,list(id_operator,group),sum) ; tab <- ifelse(is.na(tab),0,tab) })
   tab = tab[ , names(gr.presence <- sort(apply(tab,2,function(x){sum(x>0)}),"decreasing"=TRUE))]     #!X!
   print(tab)
( tab = tab[ names(op.weights <- sort(apply(tab,1,function(x){sum(x)}),"decreasing"=TRUE)) , ] )
      #
   ( tab.lumps.bin = (tab<0) ) ## shortcut to get only FALSEs #!#! this table will be UPDATED BELOW
   ( tab.lumps = tab*0 )    ## shortcut to get only 0s #!#! this table will be UPDATED BELOW
      #
( op.weights = op.weights - round(2*mean(op.weights))/2 )   ## OK!!!
   # (weight.mean = round(sum(datfram$weight)/length(unique(datfram$id_operator)),1))                   #!X!
   gr.presence                                                                                          #!X!
( op.weights.up = op.weights[op.weights>tolerance] )
( op.weights.down = rev(-op.weights[op.weights < -tolerance]) )

print(op.weights.up)
print(op.weights.down)

   tab.lumps.now = tab*0

## ---------------------------------------------------------------------------------------------------------------------•°
## THE MAIN LOOP
   k = 1
stop_loop = FALSE
while( length(op.weights.up)>0 & length(op.weights.down)>0 & !stop_loop & k<50 ){  ##  1. LOOP_1  ##

   cat(k," : ")

   ( tab.bin = tab > 0 )    ## which gr.op (group by operator -- groupo-operator) is present;
                            ## will be actualised with FALSEs in order to exclude further gr.ops
                            ## e.g. when gr.op contains some fat case for which there is no room anywhere
                            ## see (1) below
                             #
   ## upper operator i.e. with  largest amount of cases over the mean
   ( op.up = op.weights.up[1] )          ## overload of the most overloaded operator
    ( op.up.row = tab[names(op.up),] )



   if( group.to.spread( mode = "small" , spread = "all" , no_lumps = TRUE ) ){        ## mode.small.all.no_lumps
      spread.method( force_lumps = FALSE , rollback = TRUE)
      #
   }else if( group.to.spread( mode = "big" , spread = "all" , no_lumps = TRUE ) ){    ## mode.big.all.no_lumps
      spread.method( force_lumps = FALSE , rollback = TRUE)
      #
   }else if( group.to.spread( mode = "small" , spread = "all" , no_lumps = FALSE ) ){ ## mode.small.force_lumps
      spread.method( force_lumps = TRUE )
      #
   }else if( group.to.spread( mode = "big" , spread = "all" , no_lumps = FALSE ) ){   ## mode.big.all.force_lumps
      spread.method( force_lumps = TRUE )
      #
      #
   }else if( group.to.spread( mode = "big" , spread = "any" , no_lumps = TRUE ) ){    ## mode.big.any.force_lumps
      spread.method( force_lumps = FALSE )
      #
   }else if( group.to.spread( mode = "big" , spread = "any" , no_lumps = FALSE ) ){   ## mode.big.any.force_lumps
      spread.method( force_lumps = TRUE )
      #
   }else{ stop_loop = TRUE ; cat("KONIEC","\n")}




   ## recalculating  tab
   tab <-  tapply( datfram[,weight.var] , list(datfram[,operator.var],datfram[,group.var]) , sum )
   tab <- ifelse(is.na(tab),0,tab)
            ###( tab = with( datfram , { tab <- tapply(weight,list(id_operator,group),sum) ; tab <- ifelse(is.na(tab),0,tab) }) )
      print(tab)
      ( tab = tab[ , names(gr.presence <- sort(apply(tab,2,function(x){sum(x>0)}),"decreasing"=TRUE))] )   #!X!
   ( tab = tab[ names(op.weights <- sort(apply(tab,1,function(x){sum(x)}),"decreasing"=TRUE)) , ] )
      #  tab
   ( op.weights = op.weights - round(2*mean(op.weights))/2 )   ## OK!!!
      # (weight.mean = round(sum(datfram$weight)/length(unique(datfram$id_operator)),1))                   #!X!
      gr.presence                                                                                          #!X!
   ( op.weights.up = op.weights[op.weights>tolerance] )
   ( op.weights.down = rev(-op.weights[op.weights < -tolerance]) )

   (tab.lumps_k = tab.lumps + tab.lumps.now[rownames(tab.lumps),colnames(tab.lumps)])
   print(tab.lumps_k)
   print(stop_loop)
   print( ( sum(tab.lumps_k>1) == sum(tab.lumps_k>0) ) & sum(tab.lumps_k>0)>0 )
   stop_loop = ( ( sum(tab.lumps_k>1) == sum(tab.lumps_k>0) ) & sum(tab.lumps_k>0)>0 ) | stop_loop

   #cat( op.weights , "\n" )
   print(op.weights)
   print(op.weights.up)
   print(op.weights.down)
   print(stop_loop)
   k = k+1

}  ##  END OF  1. LOOP_1  ##
## ---------------------------------------------------------------------------------------------------------------------•°

datfram

} ##----END----##
## ---------------------------------------------------------------------------------------------------------------------•°




## ---------------------------------------------------------------------------------------------------------------------•°
## EXAMPLES ############################################################################################################•°
## ---------------------------------------------------------------------------------------------------------------------•°

## -------------------------------------------------------------------------------------------•°
dummy = function(){
## This is dummy function - it is not considered to be run.
## It contains a series of commands to test functions defined above in this file
## - in such a form you do not need to (un)comment it every session.
## They should be run line by line directly by the user.
## -------------------------------------------------------------------------------------------•°

## ---------------------------------------------------------------------------------------------------------------------•°
## SIMULATION
doo2pdf = function(N=30,ops=9,groups_per_op=5){
   datfram = data.frame(id_operator=numeric(0),group=character(0))
   for(k in 1:ops){
      datfram = rbind(  datfram ,
        data.frame(
             id_operator = rep(k,N)
          ,  group = sample( sample(letters[1:ops],groups_per_op) , N , replace=TRUE )
        )
      )
   }
   datfram
}

datfram = doo2pdf(N=50,ops=9,groups_per_op=5)
datfram = cbind(   id_case = sample(100:999,dim(datfram)[1]) , datfram , weight = sample(seq(0,5,.5),dim(datfram)[1],replace=TRUE) )
   tab = with( datfram , { tab <- tapply(weight,list(id_operator,group),sum) ; tab <- ifelse(is.na(tab),0,tab) })
       tab = tab[ , names(gr.presence <- sort(apply(tab,2,function(x){sum(x>0)}),"decreasing"=TRUE))]     #!X!
   ( tab = tab[ names(op.weights <- sort(apply(tab,1,function(x){sum(x)}),"decreasing"=TRUE)) , ] )
         #
      ( tab.lumps.bin = (tab<0) ) ## shortcut to get only FALSEs #!#! this table will be UPDATED BELOW
      ( tab.lumps = tab*0 )    ## shortcut to get only 0s #!#! this table will be UPDATED BELOW
         #
   ( op.weights = op.weights - round(2*mean(op.weights))/2 )   ## OK!!!
      # (weight.mean = round(sum(datfram$weight)/length(unique(datfram$id_operator)),1))                   #!X!
      gr.presence                                                                                          #!X!
   ( op.weights.up = op.weights[op.weights>tolerance] )
   ( op.weights.down = rev(-op.weights[op.weights < -tolerance]) )
   #

datfram2.df = refine.assignment( datfram , operator.var = "id_operator" , group.var = "group" , weight.var = "weight" , tolerance = 2 )
   tab = with( datfram2 , { tab <- tapply(weight,list(id_operator,group),sum) ; tab <- ifelse(is.na(tab),0,tab) })
       tab = tab[ , names(gr.presence <- sort(apply(tab,2,function(x){sum(x>0)}),"decreasing"=TRUE))]     #!X!
   ( tab = tab[ names(op.weights <- sort(apply(tab,1,function(x){sum(x)}),"decreasing"=TRUE)) , ] )
         #
      ( tab.lumps.bin = (tab<0) ) ## shortcut to get only FALSEs #!#! this table will be UPDATED BELOW
      ( tab.lumps = tab*0 )    ## shortcut to get only 0s #!#! this table will be UPDATED BELOW
         #
   ( op.weights = op.weights - round(2*mean(op.weights))/2 )   ## OK!!!
      # (weight.mean = round(sum(datfram$weight)/length(unique(datfram$id_operator)),1))                   #!X!
      gr.presence                                                                                          #!X!
   ( op.weights.up = op.weights[op.weights>tolerance] )
   ( op.weights.down = rev(-op.weights[op.weights < -tolerance]) )





## ---------------------------------------------------------------------------------------------------------------------•°

# Example 1
p <- c(15, 100, 90, 60, 40, 15, 10,  1)
w <- c( 2,  20, 20, 30, 40, 30, 60, 10)
cap <- 102
(is <- knapsack(w, p, cap))
# [1] 1 2 3 4 6 , capacity 102 and total profit 280
## Example 2
p <- c(70, 20, 39, 37, 7, 5, 10)
w <- c(31, 10, 20, 19, 4, 3,  6)
cap <- 50
(is <- knapsack(w, p, cap))
# [1] 1 4 , capacity 50 and total profit 107

p = rep(1,10)
w = sample(seq(1,10,.5),10,replace=TRUE)
cap = 13.5
(is = knapsack(w,p,cap))

p = sample(1:10)
w = seq(1,10)
cap = 9
(is = knapsack(w,p,cap))


is[]
names(is)
attributes(is)
class(is)

## ---------------------------------------------------------------------------------------------------------------------•°
a = 4
ff = function(x){
  a[2] <- 1
}
ff(1)
a

ff = function(x){
  a <<- a+x
}


a
ff(1)
a

gg = function(y){
   ff = function(x){
     a <<- a+x      ## to parent env. !!!
   }
a = 1
ff(y)
a
}

a
gg(1)
a

hh = function(x){
   xx = list()
   xx$arg = x
   xx$bool = as.logical(floor(2*x))
   xx<<-xx
   xx$bool
}

hh(1)
xx

if(hh(runif(1))){
   print(xx)
}
xx

## ---------------------------------------------------------------------------------------------------------------------•°
}

rm(dummy)
