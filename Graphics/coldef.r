coldef = function(coldef = "white"){
## Defines colorstyle of plots.

#colset=c("black", "white", "grey", "")
#if(!coldef %in% colset){
#warning("Option '", coldef, "' not defined! \n
#   Set to default 'white' which is also R's  default.")
#coldef = "white"  }

if(coldef == "black"){
  par(fg="yellow", bg="black", col.axis="white", col.lab="grey", col.main="white")
}else if(coldef == "grey"){
  par(fg="black", bg="grey", col.axis="black", col.lab="black", col.main="black")
}else if(coldef == "white"){
  par(fg="black", bg="white", col.axis="black", col.lab="black", col.main="black")
}else{
  par(fg="black", bg=coldef, col.axis="black", col.lab="black", col.main="black")
}

}
