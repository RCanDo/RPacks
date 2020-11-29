## ---------------------------------------------------------------------------------------------------------------------•°
## ---------------------------------------------------------------------------------------------------------------------•°
predict.df.list = function( model
                          , df.list 
                          , id.var = NULL 
                          , as.df = FALSE 
                          , add.df.nr =FALSE 
                          , prediction.name = NULL 
                          , data.name = c(call="Data.ref",envir="Data.ref")
                          , ... ){
## ---------------------------------------------------------------------------------------------------------------------•°
## For each data frame from a list 'df.list' this function performes prediction based on a 'model'.
## As each data frame may have different set of variables the model must be properly updated each time.
## This is the main task of this function - it makes it automatically.
## This function is primarily designed to process the output of the function split.df.by.nas()
## but it's not limited to it.
## For motivation, a broader view of the problem and other datails see help in split.df.by.nas().
##
## Arguments
##   model      model we use to obtain prodiction from data frames in df.list;
##   df.list    list of data frames from which we want to make prediction based on a 'model';
##   id.var     identifier, i.e. name (or position number in a data frames) of variable 
##              which identifies distinct records (observations);
##              if left NULL (default) the first variable in each data frame is assumed to be an identifier;
##   as.df      if FALSE (default) then the result is a list of vectors of predictions 
##              with names taken from 'id.var'; 
##              if TRUE then the result is a list of data frames each having two columns:
##                  1. id.variable - identifier as stated in 'id.var' 
##                                   (with the same name as in data frame from the df.list);
##                  2. prediction  - predicted values from given data frame from df.list based on 'model';
##                                   with the name given in 'prediction.name' (if not null); 
##   prediction.name   a name to be given to a prediction variable 
##              in resulting list of data frames (if as.df = TRUE);
##              e.g. 'score' is better suited for model = gam( ,family=binomial, );  
##              if NULL (default) then this name is simply 'prediction';  
##   add.df.nr  should data frame nr (in the df.list) be added to each resulting data frame of predictions?
##              (relevant only if as.df = TRUE); default is FALSE;   
##   data.name = c(call="Data.ref",envir="Data.ref")   what is the name of data in call to model (model$call) and what
##              is the name of data in .GlobalEnv to be used as data from the call. Default for both is "Data.ref".
##              Vector of length 1 is replicated to length 2.
##   ...        further arguments passed to predict(). 
##    
##
## Result
##   Already described at as.df.
##
## ---------------------------------------------------------------------------------------------------------------------•°

data.name = rep(data.name,length=2)
assign(data.name[1],.GlobalEnv[[data.name[2]]])

terms.model = strsplit( gsub(" ","",gsub("\n","",as.character(model$formula)[3])) , split = "+", fixed = TRUE )[[1]]

prediction.list = list()

for(k in 1:length(df.list)){  # k=62
  cat(k,"\n")

  df_k = df.list[[k]]
  df_name = names(df.list)[k]

  if(is.null(id.var)){
    id_name = names(df_k)[1]
    id.variable = df_k[,1]
  }else{
    if( is.numeric(id.var) & id.var %in% 1:ncol(df_k) ){
        id_name = names(df_k)[id.var]      
    }else if(is.character(id.var) & id.var %in% names(df_k)){
        id_name = id.var
    }else{
      stop("Identifier variable given in 'id.var' does not exist or is out of range.")
    }      
    id.variable = df_k[,id_name]
  }

    
  #idx = numeric(0)
#  for(nam in names(df_k)){
#    idx = c( idx , grep( nam , terms.model ) )   ## which variable from model is present in df
#  }

  names.df_k  = names(df_k)

  #!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!
  terms.model.uns = gsub('\\)','',gsub('^s\\(','',terms.model))
  names.df_k.az = paste0("^",paste0(names.df_k,"$"))
  #!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!
  ## all MODEL terms which could be calculated from columns of  df_k
  terms.common.idx = unlist(sapply(names.df_k.az,function(x)grep(x,terms.model.uns)))
  terms.common = terms.model[terms.common.idx] ; names(terms.common) = names(terms.common.idx)
  ## unlist(sapply(c("a","b","c"),function(x)grep(x,c("q","s(q)","a","s(a)","b","s(c)"),value=TRUE))) ## how it works
  #!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!#!
  #!#! interactions are currently not supported, only single terms, eg. s(var_name) in gam but not s(var_1,var_2)
  #!#! this is enough for now

  if(length(terms.common)==0){
      terms.excl = terms.model
      warning("Model for the ",k,"-th data frame from the 'df.list'",
  ifelse(is.null(df_name)|df_name=="","\n",paste0(", with name '",df_name,"',\n")),
  "  is NULL - none of the variables used in the model can be find in the data frame.
  Thus only mean is fitted to the data.")
      cat("\n")
  }else{
      terms.excl = setdiff(terms.model,terms.common)
  }

  if( length(terms.excl)==0 ){
    model_k = model # ;print(model_k)
  }else{

    formula_k = paste(c("~.",terms.excl),collapse="-")
    model_k = update(model,as.formula(formula_k))
   #  print(model_k)
  }


#	print(terms.excl)
#	print(terms.common)
#	print(formula_k)
#	print(model_k)

  prediction_k = predict(model_k,df_k, ... )
   # prediction_k = predict(model_k,df_k, type="response" )


  if(as.df){
    prediction_k = data.frame(id.variable , prediction = prediction_k)
      names(prediction_k)[1] = id_name
    if(!is.null(prediction.name)){ 
      names(prediction_k)[2] = prediction.name   
    }
    if(add.df.nr){
      prediction_k = cbind(prediction_k, df_nr = rep(k,length=length(id.variable)))
    }
  }else{
    names(prediction_k) = id.variable
  }
  prediction.list = c(prediction.list,list(prediction_k))
  

}
names(prediction.list) = names(df.list)
prediction.list

}
## ---------------------------------------------------------------------------------------------------------------------•°
