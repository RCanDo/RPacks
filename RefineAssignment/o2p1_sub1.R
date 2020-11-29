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
( op.up.row = op.up.row[ op.up.row>0  & !lumps ] )   ## we EXCLUDE lumps !!

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

      if(result){
         ( gr.up = op.up.row.ok[op.up.row.ok.bin][1] )
         ## which operators whith what capacities are available
         ( capacities = op.weights.down[op.up.tab.bin.down.ok[,names(gr.up)]] )
         ( capacities = c( gr.up - min(op.up,sum(capacities))  ## > 0   always!  if mode=="big" (by definition)
                                                               ## may be <= 0    otherwise
                         , capacities
                         ) )
           names(capacities)[1] = names(op.up)       ## we adjoin the source operator (from which we'd like to throw cases off)
               capacities = capacities[capacities>0]           ## important if  mode=="small"
         tolerancies = rep( round(2*(1+tolerance/length(capacities)))/2 , length(capacities) )  #; tolerancies[1] = 1   #!#!#!#!#!#!  WA¯NE -- kiedy poprawimy  knapsack()  nale¿y zast¹piæ pierwszy element 1
         ( capacities = rev(sort(fill.from.left( capacities + tolerancies , gr.up ))) )   ## what weight should be dropped to which operators <= theirs capacities
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
