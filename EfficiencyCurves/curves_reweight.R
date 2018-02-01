curves_reweight = function( curves_by_groups , regroup , file = NULL ){
## reweighting curves from 'curves.by.groups' result
##
## Arguments
##  curves_by_groups     object (list) of class "curves_by_groups" (result of the function with this name)
##  regrouping    table of the same structure as curves.by.groups$levels.as.numeric
##                where some of the NAs in the reference column (possibly all)
##                are replaced with numeric id of levels which we intend to 'cover' groups from
##                valuated portfolio.
## Writing results in a concise form to the text file:
##  file = NULL          name of the file (with expansion, e.g. .csv); if left NULL no result will be written.
##
## Result
## List of the following items:
##  reweighted.curve  =  reweighted.curve2[-1]
##  regroup           =  regroup
##  levels.as.numeric =  src$levels.as.numeric
##  summary.df        =  summary.df

src = curves_by_groups

(curves      = src$curves)
(levs        = src$levels.as.numeric)
(amounts     = src$amounts)

  # regroup = levs
  # fix(regroup)

 (remount = tapply(amounts[,2],regroup[,1],sum)[regroup[,1]])
     cbind(amounts,remount,curves)
 
 rem.ok = (remount > 0); rem.ok[is.na(rem.ok)] = FALSE
 ref.ok = (amounts[,1]>0)
 ok = rem.ok & ref.ok
 reweighted.curve = rbind(remount[ok])%*%curves[ok,] / sum(remount[ok])
 
 curew.curves  = rbind( curves , reweighted.curve = reweighted.curve )
 curew.curves2 = cbind( net_efficeiency = rowSums(curew.curves) , curew.curves )
 summary.df = cbind(src$summary.df[,1:2],curew.curves2)
 rownames( summary.df )[nrow(summary.df)] = "reweighted_curve"
 
 #regroup = as.table(regroup)
  names(attr(regroup,"dimnames")) = c("group","portfolio")
    
 result = list(
        reweighted.curve  =  reweighted.curve
      , regroup           =  regroup
      , levels.as.numeric =  src$levels.as.numeric
      , summary.df        =  summary.df
 )
 
   if(!is.null(file)){
    summary.df = cbind(row_names = rownames(summary.df),summary.df)
    write.table(summary.df, file = file, dec = ",", sep = ";", na = "",row.names=FALSE)          
   }
   
 result
 
}