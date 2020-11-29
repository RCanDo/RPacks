ranges = function( datfram, model ){
  range.tab = data.frame(left = numeric(0),right = numeric(0))
  for (k in names(model$coefficients)[-1]) {
    range.tab_k = range(datfram[,k]);
    rnams = rownames(range.tab)
    range.tab = rbind(range.tab,range.tab_k)
    rownames(range.tab) = c(rnams, k)
  }
  colnames(range.tab) = c("left","right")
  range.tab
}
