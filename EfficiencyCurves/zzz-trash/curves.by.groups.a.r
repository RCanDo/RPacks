curves.by.groups = function(
    valuated.df
   ,reference.df
   ,amount.var = NULL
   ,group.var = NULL
   ,time.var = NULL
   ,yield.var = NULL
   ,id.var = NULL
   ,file = NULL
){
## We are given two investment portfolios divided in groups (sub-portfolios) of the same types (e.g. balance groups):
##   - reference for which historical yields/payments are given,
##   - valuated for which prediction is needed on the basis of reference data.
## Calculating this prediction is the aim af this function.
##
## Arguments
##  valuated.df          valuated data frame having at least two columns: 
##                        amount variable, 
##                        group variable;
##  reference.df         reference data frame having at least five columns: 
##                        amount variable  -- balance on investment member/item 
##                                            (may be price of financial instrument, debt outstanding amount, etc.)
##                        group variable   -- variable grouping members/items into sub-portfolios
##                        time variable    -- periods of investment: 1, 2,...
##                        yield variable   -- yield (e.g. payments from investment, repayments of a debt)
##                        id variable      -- member or item id (e.g. debtor id, financial instrument);
## The default sequence of the variables (in terms of its meanings not names which may be arbitrary)
## in each data frame is as stated above.
## It is also possible to indicate directly which variable in the data frame has given meaning
## by passing their names or numeric position in data frame to the following function variables:
##  amount.var = NULL
##  group.var = NULL
##  time.var = NULL
##  yield.var = NULL
##  id.var = NULL
## Writing results in conscise form to the text file: 
##  file = NULL          name of the file (with expansion, e.g. .csv); if left NULL no result will be written.
##
## Notice that yields/payments do not need to be grouped within id by period cells.
## Thus the reference data frame may contain simply a history of yields without any earlier aggregation.
## Nontheless for each yield a period in which it was made must be determined by time variable. 
##
## Result
## Function returns list with the following entries:
##  curves            table of groups (from group variable) by periods (from time variable)
##                    where for reference data sum of yield is given within each group and period    
##  weighted.curve
##  groups.presence
##  groups.sums
##  group.time.yields  
##  summary.df
## 
##
##



## STAGE 1. checking data integrity
   if(is.null(amount.var)){
      a.val = valuated.df[,1]
      a.ref = reference.df[,1]       ## index must be integer
   }else if(is.character(amount.var)|(is.numeric(amount.var)&(floor(amount.var)==amount.var))){
      a.val = valuated.df[,amount.var]
      a.ref = reference.df[,amount.var]
   }else{
      stop("Incorrect 'amount.var' identifier: must be character or integer or left NULL.
   If left NULL then the 1st variable of valuated and reference data frame will be taken as amount variable")
   }
   
   if(is.null(group.var)){
      g.val = valuated.df[,2]
      g.ref = reference.df[,2]          ## index must be integer
   }else if(is.character(group.var)|(is.numeric(group.var)&(floor(group.var)==group.var))){
      g.val = valuated.df[,group.var]
      g.ref = reference.df[,group.var]
   }else{
      stop("Incorrect 'group.var' identifier: must be character or integer or left NULL.
   If left NULL then the 2nd variable of valuated and reference data frames will be taken as groping variable")
   }

   if(is.null(time.var)){
      #t.val = valuated.df[,3]
      t.ref = reference.df[,3]      ## index must be integer
   }else if(is.character(time.var)|(is.numeric(time.var)&(floor(time.var)==time.var))){
      #t.val = valuated.df[,time.var]
      t.ref = reference.df[,time.var]
   }else{
      stop("Incorrect 'time.var' identifier: must be character or integer or left NULL.
   If left NULL then the 3d variable of reference data frame will be taken as time variable")
   }

   if(is.null(yield.var)){
      #y.val = valuated.df[,4]
      y.ref = reference.df[,4]       ## index must be integer
   }else if(is.character(yield.var)|(is.numeric(yield.var)&(floor(yield.var)==yield.var))){
      #y.val = valuated.df[,yield.var]
      y.ref = reference.df[,yield.var]
   }else{
      stop("Incorrect 'yield.var' identifier: must be character or integer or left NULL.
   If left NULL then the 4th variable of reference data frame will be taken as yield variable")
   }
   
   if(is.null(id.var)){
      id.ref = reference.df[,5]     ## index must be integer
   }else if(is.character(id.var)|(is.numeric(id.var)&(floor(id.var)==id.var))){
      id.ref = reference.df[,id.var]
   }else{
      stop("Incorrect 'id.var' identifier: must be character or integer or left NULL.
   If left NULL then the 5th variable of reference data frame will be taken as id variable")
   }

## STAGE 2. calculations
## a. - amount,   g. - group,    y. - yield,    t. - time,  id. - id
## .val - valuated,   .ref - reference


   ## We shall need aggregating within each time value for each member:
   # ref.df = ref.df[order(id.ref),]  ## to see better

#   y.idt.tab = tapply(y.ref,list(id.ref,t.ref),sum)
#   y.idt.tab[is.na(y.idt.tab)] = 0
#   y.idt.tab

   ## better - aggregating within each time value for each group:
   y.gt.tab = tapply(y.ref,list(g.ref,t.ref),sum)     ## yields for each group across time
   y.gt.tab[is.na(y.gt.tab)] = 0    ## rarely needed
    y.gt.tab

   ## aggregating amounts within each group - must be made in 2 steps!!!
   (a.idg.ref = tapply(a.ref,list(id.ref,g.ref),max))
   (a.g.ref = colSums(a.idg.ref,na.rm=TRUE))

   ## efficiency curves within groups on reference
   groups.curves = apply(y.gt.tab,2,function(x){x/a.g.ref})
    groups.curves
    
    
   gval.in.gref = unique(g.val) %in% unique(g.ref)
   if( any( !gval.in.gref ) ){
      warning(" There are groups in reference data frame having no counterparts in valuated data frame.
   It results with lack of curve fitted to this group.
   The weighted curve is weighted only with amounts given for groups present also on reference.
   Curves my be reweighted using function 'reweight.curves'.")
   }
   
   ##
   gty.val = expand.grid(group = union(unique(g.val),unique(g.ref)),time = unique(t.ref),yield = 0)
   groups.curves.val = tapply( gty.val$yield , list(gty.val$group,gty.val$time) , sum )
   groups.curves.val[unique(g.ref),] = groups.curves[unique(g.ref),]
   
   ## weighted efficiency curve for valuated
   a.g.val = numeric(nrow(groups.curves.val)); names(a.g.val) = rownames(groups.curves.val)
   a.g.val[unique(g.val)] = tapply( a.val , g.val , sum )[unique(g.val)]
   wvr = a.g.val   
   wvr[unique(g.val)[!gval.in.gref]] = 0
      # weights.val[is.na(weights.val)] = 0
   weighted.curve = as.vector(t(wvr)%*%groups.curves.val) / sum(wvr)
      names(weighted.curve) = sort(unique(t.ref))
      
    ## groups.presence 
    a.g.val = a.g.val[a.g.val!=0]
    groups.presence = rbind(  data.frame( groups = names(a.g.ref) , data = "reference" , amount = a.g.ref )
                            , data.frame( groups = names(a.g.val) , data = "valuated"  , amount = a.g.val ) )
    groups.presence.tab = table(groups.presence$data,groups.presence$groups)
    groups.amounts.tab = tapply(groups.presence$amount,list(groups.presence$groups,groups.presence$data),max)
    groups.amounts.tab[is.na(groups.amounts.tab)] = 0
    
    ## summary  
    df1 = as.data.frame(groups.amounts.tab); colnames(df1) = c("ref_balance","val_balance")
    df2 = rbind( df1 , weighted_curve = c( sum(a.g.ref) , sum(a.g.val)) )
    ##
    df3 = rbind( groups.curves.val, weighted.curve )
    df4 = cbind( curve_sum = rowSums(df3) , df3) 
    summary.df = cbind(df2,df4)    


   result = list(   curves = groups.curves.val
                  , weighted.curve = weighted.curve
                  , groups.presence = groups.presence.tab
                  , groups.sums = groups.amounts.tab
                  , group.time.yields = y.gt.tab 
                  , summary.df = summary.df 
                )
   
          
   if(!is.null(file)){
    summary.df = cbind(row_names = rownames(summary.df),summary.df)
    write.table(summary.df, file = file, dec = ",", sep = ";", na = "",row.names=FALSE)          
   }
                      
   result

} ##----END----##


################################################################################
#### testing data


## valuated portfolio
sim.val.df = function(members = 100, groups = 3){
## Simulating valuated data frame
   val.df = data.frame(   amount = sample( (1:10)*100        , members , replace=TRUE )
                        , group  = sample( LETTERS[1:groups] , members , replace=TRUE )  )
   val.df
} ##----END----##

val.df = sim.val.df(members = 30, groups = 3)
a.val = val.df$amount
g.val = val.df$group


## reference portfolio
sim.ref.df = function(  members = 100
                       ,groups = 3
                       ,horizon = 4
                       ,yield.par = .1
                       ){
## members = 100 ; groups = 3 ; horizon = 4 ; yield.par = .1
##
## Simulating reference data frame

M = members
G = groups
H = horizon

amount.var = sample(c(1:10)*100,M,replace=TRUE)
group.var = sample(LETTERS[1:G],M,replace=TRUE)
id.var = sample(100000:999999,M)
time.mat = as.data.frame(matrix(rep(1:H,each=M),ncol=H)); colnames(time.mat) = 1:H
ref.df.0 = cbind(amount = amount.var, group = group.var, id = id.var, time.mat)

ref.df.long = reshape(ref.df.0, varying = as.character(c(1:H)), v.name = "time", direction = "long")

Y = M*H+rpois(1,10) ## random (but moderate) deviation from  M*H
choice.id = sample(M*H,Y,replace=TRUE)  ## random yield moments for each member
ref.df = ref.df.long[choice.id,]
## non-zero random yields roughly proportional to the amount for given member:
ref.df = cbind( ref.df , yield = rpois( Y , ref.df$amount*yield.par ) + 1 )  ## +1 to exclude 0 yields

ref.df[,c("amount","group","time","yield","id")]
} ##----END----##


ref.df = sim.ref.df( members = 20 , groups = 4 , horizon = 8 , yield.par = .1 )
head(ref.df)

# variable names
a.ref = ref.df$amount
g.ref = ref.df$group
t.ref = ref.df$time
y.ref = ref.df$yield
id.ref = ref.df$id
#
# now you can go to the main function code    STAGE 2


(src = curves.by.groups( val.df , ref.df ))






