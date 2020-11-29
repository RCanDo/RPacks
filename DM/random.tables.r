addmargs = function(tab,names = TRUE,attribute=TRUE){
## Adds margins sums to the 2-dimensional table, ie.
##    adds column with row sums and
##    adds row with column sums.
## IF 'names=TRUE' (default) then function automatically creates names
## (unless col- row- names are already present)
## for rows and columns of the form 'r_i' and 'c_j' respectively
## (where i and j are col's and rows's numbers).
#! Margins are not given names.
## If 'attribute=TRUE' (default) then attribute 'margins' is created of the form
##    margins$cols = colSums(tab);  margins$rows = rowSums(tab).
dimnams = dimnames(tab)
rows = rowSums(tab)
cols = colSums(tab)
tt = rbind(tab,cols)
tt = cbind(tt,c(rows,sum(rows)))
tt = as.table(tt)
if(names){
   if(!is.null(dimnams)) {
      dimnams[[1]] = c(dimnams[[1]]," ")
         lastcol = paste0(rep(" ",round(max(nchar(dimnams[[2]]))/2)+2),collapse="")
      dimnams[[2]] = c(dimnams[[2]],lastcol)
      dimnames(tt) =  dimnams
   }else{
      rownams = c(paste0("r_",1:length(rows))," ")
      colnams = c(paste0("c_",1:length(cols))," ")
      dimnames(tt) = list(rownams,colnams)
   }
}else{dimnames(tt) =  NULL}
if(attribute){ attr(tt,"margins") = list(rows=rows,cols=cols)  }
tt
}
#! see help(addmargins), '.../help-addmargins.r'

demargs = function(tab,force=FALSE){
## Deletes margins from table if there is attribute 'margins'.
## If force=TRUE then margins (last column and last row)
## are deleted from given table without checking if they are really columns or rows sums.
## Then attribute 'margins' will be deleted too.
## Function do not checks if margins are really columns or rows sums.
d=dim(tab)-1
if(force)
{tabd = as.table(tab[1:d[1],1:d[2]])
   attr(tabd,"margins") = NULL
}else{
   if(is.null(attr(tab,"margins")))
   {stop("There are presumabely no given marginals in this table (no 'margins' attribute).
          Use 'force=TRUE' if you are sure that last row and last column are proper table marginals.")
   }else{
      if(length(attr(tab,"margins")$rows)>d[1])
      {stop("This table do nat have marginals as part of the table.
   Use 'force=TRUE' if you are sure that last row and last column are proper table marginals.
   Then attribute 'margins' will be deleted too.")
      }else{
         tabd = as.table(tab[1:d[1],1:d[2]])
         ## if(colSums(tab[1:d[1]])!=tab[d[1]+1,]|    ){stop()}
            dimnams = dimnames(tab)
         dimnames(tabd)[[1]] = dimnams[[1]][1:d[1]]
         dimnames(tabd)[[2]] = dimnams[[2]][1:d[2]]
         attr(tabd,"margins") = attr(tab,"margins")
      }
   }
}
tabd
}

rtabtwo = function(rows,cols,margins=FALSE,names=TRUE){
## Random generator of 2x2 tables with given marginal sums.
## margins     if TRUE than table will be returned together with given marginals and total sum
##             as 3x3 table. Default is FALSE what gives 2x2 table with no marginals.
   rownams = names(rows)
   colnams = names(cols)
#! ???  if (!is.integer(rows) | !is.integer(cols)) stop("rows and cols must be integer.")
#!    class(c(10,20))  is not integer but numeric...
if (length(rows)==2 & length(cols)==1){
   cols = c(cols,sum(rows)-cols)
   if (!is.null(colnams)) {colnams = c(colnams,"column_2"); names(cols) = colnams}
}
if (length(rows)==1 & length(cols)==2){
   rows = c(rows,sum(cols)-rows)
   if (!is.null(rownams)) {rownams = c(rownams,"row_2"); names(rows) = rownams}
}
if (length(rows)!=2 | length(cols)!=2)
   stop("'rows' and 'cals' parameters must have length 1 or 2 and only one may have length 1.")
if (sum(rows)!=sum(cols)) stop("'rows' and 'cols' must sum to the same value.")
W = rows[1]; B = rows[2]; D = cols[1]; L = cols[2]
if (names) {
   if (is.null(rownams)){rownams = c("White","Black")}
   if (is.null(colnams)){colnams = c("Drawn","Left") }
}
if(margins){
   tt = as.table(rbind(c(X <- rhyper(1, W, B, D),W-X,W),c(D-X,B-(D-X),B),c(D,L,W+B)))
   if (names) {
      rownams = c(rownams,"")
      colnams = c(colnams,"     ")
   }
}else{
   tt = as.table(rbind(c(X <- rhyper(1, W, B, D),W-X),c(D-X,B-(D-X))))
}
if(names){dimnames(tt) = list(rownams,colnams)}else{dimnames(tt)=NULL}
attr(tt,"margins") = list(rows=rows,cols=cols)
tt
}

rtable = function(rows=2,cols=2,tot=rpois(1,sum(rows*cols)*10),margins=FALSE,names=TRUE){
## Random generator of IxJ tables with given marginal sums.
## rows     marginal distribution for rows, i.e. sums of elements in each row.
## cols     marginal distribution for columns, i.e. sums of elements in each column.
## tot      total sum of table. Ignored if rows or cols have length > 1.
##
## See /ROBOCZY/R/myHelp/help-tables.r  for examples.
##
#! ???  if (!is.integer(rows) | !is.integer(cols)) stop("rows and cols must be integer.")
#!    class(c(10,20))  is not integer but numeric...
   rownams = names(rows)
   colnams = names(cols)
if( length(rows)==1 & length(cols)>1 ){
   rows = as.vector(rmultinom(1,size=sum(cols),rep(1,rows)))      ## randomly chosen rows
}else if( length(rows)>1 & length(cols)==1 ){
   cols = as.vector(rmultinom(1,size=sum(rows),rep(1,cols)))
}else if( length(rows)==1 & length(cols)==1 ){
   tt = matrix(rmultinom(1,size = tot,prob = rep(1,(rows*cols))),ncol=cols)
}else{}
if( length(rows)>1 & length(cols)>1 ){
   if( sum(rows)<sum(cols) ){
      warning("Extra row added.")
      rows = c(rows,sum(cols) - sum(rows))
      if(!is.null(rownams)){rownams = c(rownams,"(extra)")}
   }
   if( sum(rows)>sum(cols) ){
      warning("Extra column added.")
      cols = c(cols,sum(rows) - sum(cols))
      if(!is.null(colnams)){colnams = c(colnams,"(extra)")}
   }
   ## MAIN
   I = length(rows)
   J = length(cols)
   tt = matrix(,nrow=I,ncol=J)
   for( i in 1:(I-1) ) {
      rows_i = c(sum(rows[1:(I-i)]),rows[I-i+1])
      for( k in 1:(J-1) ) {                        ## calculating row  I-i+1
         cols_k = c(sum(cols[1:(J-k)]),cols[J-k+1])
         tt_k = rtabtwo(rows_i,cols_k,names=FALSE)
         tt[I-i+1,J-k+1] = tt_k[2,2]
         rows_i = rows_i - tt_k[,2]
      }
      tt[I-i+1,1] = tt_k[2,1]
      cols = cols - tt[I-i+1,]
   }
   tt[1,] = cols
   ## END MAIN
}
rows = rowSums(tt)
cols = colSums(tt)
##
if(names){
   if(is.null(rownams)){rownams = paste0("r_",1:length(rows))}
   if(is.null(colnams)){colnams = paste0("c_",1:length(cols))}
}
if(margins){tt = rbind(tt,cols); tt = cbind(tt,c(rows,sum(rows)))
      if(names){
         rownams = c(rownams,"")
            lastcol = paste0( rep( " ", round( max(nchar(colnams))/2 + 1 )) , collapse="" )
         colnams = c(colnams,lastcol)
      }
}
tt = as.table(tt)
if(names){dimnames(tt) = list(rownams,colnams)}else{dimnames(tt)=NULL}
##
attr(tt,"rows") = rows
attr(tt,"cols") = cols
tt
}
