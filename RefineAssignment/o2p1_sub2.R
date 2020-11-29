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

   ( tochange.df = datfram[ datfram[,operator.var] == names(op.up) & datfram[,group.var] == names(gr.up) , ] )
   ( tochange.df = tochange.df[do.call(order,c(tochange.df[,weight.var,drop=FALSE],"decreasing"=TRUE)),] )  #???
   #  ( tochange.cases = var2vec( tochange.df , "weight" , names = 0  ) )

   #############################################################################•°
   idx.out_k = character(0)  ## OK!!!
   tab.lumps.now = tab*0

   do_break <- FALSE

   if(!force_lumps){  ## force_lumps == FALSE

      for( k  in 1:length(capacities) ){  ##  3. LOOP_2  ##  k = 2
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
                                    , p = tochange.df_k[,weight.var]^3  ##pmax(tochange.df_k[,"weight"]-1,1/dim(tochange.df_k)[1])
                                                                        ## profit ; if p = w-1 then 1 big case is always preferred over several smaller (what is our objective)
                                                                        ## however, we also need cases having weight 1 and 0 so we must put some profit to them;
                                                                        ##  the profit  1/(nr.of.cases)  for 1's and 0's  implies that both cases will be taken as soon as possible but not instead of heavier cases
                                    , cap = cap_k                       ## capacity
                                    #!#!#!#!#!#!   cap >= max(w)        ## otherwise an ERROR occur !!!
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
               tab.lumps[names(op.up),names(gr.up)] = tab.lumps[names(op.up),names(gr.up)] + 1   ## history of lumps for this group
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
