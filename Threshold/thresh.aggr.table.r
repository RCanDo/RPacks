thresh.aggr.table = function( variable = Data.ref$skutecznosc
                            , covariate = Data.ref$saldo_data_cutoff
                            , goodsallratio = seq(.1,.9,.1)
                            , FUN = sum
)
{

    thresh.aggr.tab  = data.frame( bads = numeric(0) , goods = numeric(0))
    for(k in  goodsallratio){
      GoodsBads = set.threshold(variable, goodsallratio = k , message = FALSE )$GoodsBads
      thresh.aggr.tab  =  rbind(thresh.aggr.tab , tapply( covariate , GoodsBads , FUN ) )
    }
    colnames(thresh.aggr.tab)  =  c("bads","goods")
    rownames(thresh.aggr.tab)  =  goodsallratio

    thresh.aggr.tab

}