## ---------------------------------------------------------------------------------------------------------------------•°
## SIMULATION

N=50;ops=9;groups_per_op=5
# doo2pdf = function(N,ops,groups_per_op){ }
groups = replicate(ops,sample( sample(letters[1:ops],groups_per_op) , N , replace=TRUE ))
o2p = data.frame(
   id_case        = sample(100:999, N*ops) # numeric(0)
,  id_operator    = rep(NA_integer_, N*ops)    # numeric(0)
,  group          = as.vector(groups)  # character(0)
,  weight         = sample(seq(0,5,.5), N*ops, replace=TRUE)
)

   with( o2p , tapply(weight,c(group),sum,na.rm=T) )


subset( o2p ,!is.na(get("id_operator") ))


## ---------------------------------------------------------------------------------------------------------------------•°

function(datfram , operator_var , group_var , weight_var , tolerance , distribution = NULL ){

datfram_1 = subset(datfram,!is.na(get(operator_var)))
datfram_0 = subset(datfram,is.na(get(operator_var)))

}

